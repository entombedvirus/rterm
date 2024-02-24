use std::{
    collections::VecDeque,
    ops::Range,
    os::fd::{AsFd, AsRawFd, OwnedFd},
    sync::{mpsc, Arc},
};

use crate::{
    ansi::{self, SgrControl},
    pty, terminal_input,
};
use ansi::AsciiControl;
use anyhow::Context;
use egui::{text::LayoutJob, CentralPanel, Color32, FontId, Key, Rect};
use log::info;
use nix::errno::Errno;

#[derive(Debug)]
pub struct TerminalEmulator {
    _io_thread: std::thread::JoinHandle<()>,
    pty_fd: Arc<OwnedFd>,
    token_stream: mpsc::Receiver<VecDeque<ansi::AnsiToken>>,
    window_rect: Option<Rect>,
    pub char_dimensions: Option<egui::Vec2>,
    buffered_input: String,
    grid: AnsiGrid,
    pub regular_font: egui::FontId,
}

impl eframe::App for TerminalEmulator {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| -> anyhow::Result<()> {
            let rect = ui.available_rect_before_wrap();
            if self.window_rect != Some(rect) {
                self.window_rect = Some(rect);
                let char_dims = self
                    .char_dimensions
                    .get_or_insert_with(|| get_char_size(ctx, &self.regular_font).into());
                let winsz =
                    pty::compute_winsize(rect.width(), rect.height(), char_dims.x, char_dims.y);
                if self
                    .grid
                    .resize(winsz.ws_row as usize, winsz.ws_col as usize)
                {
                    pty::update_pty_window_size(self.pty_fd.as_fd(), &winsz)
                        .context("update_pty_window_size")?;
                }
            }

            loop {
                match self.token_stream.try_recv() {
                    Ok(tokens) => {
                        for token in tokens {
                            self.grid.update(&token);
                        }
                        log::debug!(
                            "move_cursor: ({}, {}), visible_line: {}",
                            self.grid.cursor_position.0,
                            self.grid.cursor_position.1,
                            self.grid.first_visible_line_no
                        );
                    }
                    Err(mpsc::TryRecvError::Disconnected) => {
                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                        anyhow::bail!("shell exit");
                    }
                    Err(mpsc::TryRecvError::Empty) => break,
                }
            }

            ui.input(|input_state| -> anyhow::Result<()> {
                for event in &input_state.events {
                    self.handle_event(input_state, event)
                        .context("handling event failed")?;
                }
                Ok(())
            })
            .context("input handling failed")?;

            self.write_to_pty().context("writing to pty failed")?;

            ui.painter().rect_filled(
                egui::Rect::from_min_size([0.0, 0.0].into(), ui.available_size()),
                0.0,
                Color32::BLACK,
            );
            self.paint_cursor(ui).context("paint_cursor failed")?;
            self.render(ctx, ui, 0..0);
            Ok(())
            // let char_height = ctx.fonts(|fonts| fonts.row_height(&self.regular_font));
            // egui::ScrollArea::vertical().stick_to_bottom(true).show(
            //     ui,
            //     // char_height,
            //     // self.grid.num_current_rows(),
            //     // |ui, visible_rows| -> anyhow::Result<()> {
            //     |ui| -> anyhow::Result<()> {
            //     },
            // );
            // Ok(())
        });
    }
}

impl TerminalEmulator {
    pub fn new(cc: &eframe::CreationContext<'_>, pty_fd: Arc<OwnedFd>) -> anyhow::Result<Self> {
        let (tx, rx) = mpsc::channel();

        let io_fd = Arc::clone(&pty_fd);
        let ctx = cc.egui_ctx.clone();
        let _io_thread = std::thread::Builder::new()
            .name("rterm i/o thread".to_string())
            .spawn(move || {
                terminal_input::input_loop(ctx, io_fd, tx);
            })
            .expect("Failed to spawn input_loop thread");

        let regular_font = FontId::monospace(22.0);
        cc.egui_ctx.style_mut(|style| {
            // style.override_text_style = Some(egui::TextStyle::Monospace);
            style.override_font_id = Some(regular_font.clone());
        });
        // cc.egui_ctx.set_pixels_per_point(2.0);

        Ok(Self {
            _io_thread,
            buffered_input: String::new(),
            grid: AnsiGrid::new(24, 80, 5000),
            pty_fd,
            token_stream: rx,
            window_rect: None,
            char_dimensions: None,
            regular_font,
        })
    }

    pub fn render(&self, _ctx: &egui::Context, ui: &mut egui::Ui, visible_rows: Range<usize>) {
        for (line_chars, formats) in self.grid.lines(visible_rows) {
            let mut layout = LayoutJob::default();
            layout.wrap = egui::text::TextWrapping::no_max_width();
            for (&ch, format) in line_chars.into_iter().zip(formats) {
                let byte_range = layout.text.len()..layout.text.len() + ch.len_utf8();
                layout.text.push(ch);
                let format = egui::text::TextFormat {
                    font_id: self.regular_font.clone(),
                    color: if format.bold {
                        ansi::Color::brighter(format.fg_color).into()
                    } else {
                        format.fg_color.into()
                    },
                    ..Default::default()
                };
                let section = egui::text::LayoutSection {
                    leading_space: 0.0,
                    byte_range,
                    format,
                };
                layout.sections.push(section);
            }
            let galley = ui.fonts(|fonts| fonts.layout_job(layout));
            ui.label(galley);
        }
    }

    pub fn write_to_pty(&mut self) -> anyhow::Result<()> {
        if self.buffered_input.is_empty() {
            return Ok(());
        }

        let mut buf = self.buffered_input.as_bytes();
        while !buf.is_empty() {
            match nix::unistd::write(self.pty_fd.as_raw_fd(), buf) {
                Ok(num_written) => {
                    buf = &buf[num_written..];
                }
                Err(Errno::EAGAIN) => {
                    // can't write right now, try again later
                    let total_written = self.buffered_input.len() - buf.len();
                    self.buffered_input.drain(0..total_written);
                    return Ok(());
                }
                Err(unexpected) => return Err(anyhow::format_err!("read failed: {}", unexpected)),
            }
        }

        self.buffered_input.clear();
        Ok(())
    }

    pub fn handle_event(
        &mut self,
        input_state: &egui::InputState,
        event: &egui::Event,
    ) -> anyhow::Result<()> {
        match event {
            egui::Event::Text(txt) => {
                if input_state.modifiers.alt {
                    terminal_input::alt(txt, &mut self.buffered_input);
                } else {
                    self.buffered_input.push_str(&txt);
                };
            }
            egui::Event::Key {
                key,
                pressed: true,
                modifiers: egui::Modifiers::CTRL,
                ..
            } => terminal_input::ctrl(key.name(), &mut self.buffered_input),
            egui::Event::Key {
                key: Key::Escape,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::Escape.into()),
            egui::Event::Key {
                key: Key::Tab,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::Tab.into()),
            egui::Event::Key {
                key: Key::Backspace,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::Backspace.into()),
            egui::Event::Key {
                key: Key::Enter,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::LineFeed.into()),
            egui::Event::Key {
                key: Key::ArrowUp,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[A"),
            egui::Event::Key {
                key: Key::ArrowDown,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[B"),
            egui::Event::Key {
                key: Key::ArrowRight,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[C"),
            egui::Event::Key {
                key: Key::ArrowLeft,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[D"),
            egui::Event::Key { pressed: false, .. } => (),
            egui::Event::PointerMoved { .. } => (),
            egui::Event::PointerGone { .. } => (),
            egui::Event::WindowFocused { .. } => (),
            egui::Event::Scroll { .. } => (),
            egui::Event::MouseWheel { .. } => (),
            _ => log::debug!("unhandled event: {event:?}"),
        };
        Ok(())
    }

    pub fn paint_cursor(&self, ui: &mut egui::Ui) -> anyhow::Result<()> {
        let rect = ui.available_rect_before_wrap();
        let char_dims = self.char_dimensions.unwrap_or(egui::vec2(12.0, 12.0));
        let (cursor_row, cursor_col) = self.grid.cursor_position;
        let cursor_rect = Rect::from_min_size(
            egui::pos2(
                cursor_col as f32 * char_dims.x,
                cursor_row as f32 * char_dims.y,
            ) + rect.min.to_vec2(),
            char_dims,
        );
        ui.painter().rect_filled(cursor_rect, 0.0, Color32::GOLD);
        Ok(())
    }
}

#[derive(Debug)]
struct AnsiGrid {
    num_rows: usize,
    num_cols: usize,
    max_rows_scrollback: usize,
    first_visible_line_no: usize,
    cells: Vec<char>,
    text_format: Vec<TextFormat>,
    cursor_position: (usize, usize),
    current_text_format: TextFormat,
}

impl AnsiGrid {
    const FILL_CHAR: char = '-';

    fn new(num_rows: usize, num_cols: usize, max_rows_scrollback: usize) -> Self {
        // scrollback has to be at least num_rows
        let max_rows_scrollback = max_rows_scrollback.max(num_rows);
        let cursor_position = (0, 0);
        let first_visible_line_no = 0;
        Self {
            num_rows,
            num_cols,
            max_rows_scrollback,
            first_visible_line_no,
            cells: vec![Self::FILL_CHAR; num_rows * num_cols],
            text_format: vec![TextFormat::default(); num_rows * num_cols],
            cursor_position,
            current_text_format: TextFormat::default(),
        }
    }

    fn resize(&mut self, new_num_rows: usize, new_num_cols: usize) -> bool {
        if self.num_rows == new_num_rows && self.num_cols == new_num_cols {
            return false;
        }
        log::info!(
            "resize: {} x {} -> {new_num_rows} x {new_num_cols}",
            self.num_rows,
            self.num_cols
        );
        let new_len = (self.first_visible_line_no + new_num_rows) * new_num_cols;
        let mut new_cells = vec![Self::FILL_CHAR; new_len];
        let mut new_format = vec![TextFormat::default(); new_len];
        let line_len = self.num_cols.min(new_num_cols);
        for (line_no, (old_cells, old_formats)) in self
            .cells
            .chunks_exact(self.num_cols)
            .zip(self.text_format.chunks_exact(self.num_cols))
            .enumerate()
        {
            let new_line_start = line_no * new_num_cols;
            let copy_range = new_line_start..new_line_start + line_len;
            if let Some(dst) = new_cells.get_mut(copy_range.clone()) {
                dst.copy_from_slice(&old_cells[..line_len]);
            }
            if let Some(dst) = new_format.get_mut(copy_range) {
                dst.copy_from_slice(&old_formats[..line_len]);
            }
        }
        self.cells = new_cells;
        self.text_format = new_format;
        self.num_rows = new_num_rows;
        self.num_cols = new_num_cols;
        true
    }

    fn update(&mut self, token: &ansi::AnsiToken) {
        use ansi::AnsiToken::*;
        use ansi::AsciiControl;
        use ansi::CursorControl;
        use ansi::EraseControl;

        match token {
            Text(txt) => {
                for ch in txt.chars() {
                    self.update_cursor_cell(ch);
                    self.move_cursor_relative(0, 1);
                }
            }
            AsciiControl(AsciiControl::Backspace) => {
                self.move_cursor_relative(0, -1);
            }
            AsciiControl(AsciiControl::Tab) => {
                self.move_cursor_relative(0, 4);
            }
            AsciiControl(AsciiControl::LineFeed) => {
                self.move_cursor_relative(1, 0);
            }
            AsciiControl(AsciiControl::CarriageReturn) => {
                self.move_cursor(self.cursor_position.0, 0);
            }
            CursorControl(CursorControl::MoveRight { cols }) => {
                self.move_cursor_relative(0, *cols as isize);
            }
            CursorControl(CursorControl::MoveTo { line, col }) => {
                self.move_cursor(line.saturating_sub(1), col.saturating_sub(1));
            }
            CursorControl(CursorControl::MoveLineUp) => {
                let (cursor_row, _) = self.cursor_position;
                if cursor_row > 0 {
                    // no need to scroll
                    self.move_cursor_relative(-1, 0);
                } else {
                    self.first_visible_line_no = self.first_visible_line_no.saturating_sub(1);
                    // // shift last num_rows lines down by one line
                    // let last_row_start = self.cells.len() - self.num_cols;
                    // let visible_start = self.cursor_position_to_buf_pos(&(0, 0));
                    // self.cells
                    //     .copy_within(visible_start..last_row_start, self.num_cols);
                    // self.text_format
                    //     .copy_within(visible_start..last_row_start, self.num_cols);
                    // self.cells[visible_start..self.num_cols].fill(Self::FILL_CHAR);
                    // self.text_format[visible_start..self.num_cols].fill(TextFormat::default());
                }
            }
            EraseControl(EraseControl::Screen) => {
                let visible_start = self.cursor_position_to_buf_pos(&(0, 0));
                let visible_end =
                    self.cursor_position_to_buf_pos(&(self.num_rows - 1, self.num_cols - 1));
                self.cells[visible_start..visible_end].fill(Self::FILL_CHAR);
                self.text_format[visible_start..visible_end].fill(TextFormat::default());
            }
            EraseControl(EraseControl::FromCursorToEndOfScreen) => {
                let cur_idx = self.cursor_position_to_buf_pos(&self.cursor_position);
                let visible_end =
                    self.cursor_position_to_buf_pos(&(self.num_rows - 1, self.num_cols - 1));
                self.cells[cur_idx..visible_end].fill(Self::FILL_CHAR);
                self.text_format[cur_idx..visible_end].fill(TextFormat::default());
            }
            EraseControl(EraseControl::FromCursorToEndOfLine) => {
                let cur_idx = self.cursor_position_to_buf_pos(&self.cursor_position);
                let line_end_idx =
                    self.cursor_position_to_buf_pos(&(self.cursor_position.0 + 1, 0));
                self.cells[cur_idx..line_end_idx].fill(Self::FILL_CHAR);
                self.text_format[cur_idx..line_end_idx].fill(TextFormat::default());
            }
            SGR(params) => {
                for sgr in params {
                    match sgr {
                        SgrControl::Bold => {
                            self.current_text_format.bold = true;
                        }
                        SgrControl::ForgroundColor(color) => {
                            self.current_text_format.fg_color = *color;
                        }
                        SgrControl::Reset => {
                            self.current_text_format = TextFormat::default();
                        }
                        SgrControl::Unimplemented(_) => {
                            // noop
                        }
                    }
                }
            }
            ignored => info!("ignoring ansii token: {ignored:?}"),
        }
    }

    fn cursor_position_to_buf_pos(&self, (r, c): &(usize, usize)) -> usize {
        let start = self.first_visible_line_no * self.num_cols;
        let offset = r * self.num_cols + c;
        start + offset
    }

    fn move_cursor(&mut self, new_row: usize, new_col: usize) {
        assert!(new_col < self.num_cols);
        // new position is within bounds
        if new_row < self.num_rows {
            self.cursor_position = (new_row, new_col);
            return;
        }

        let num_new_lines = new_row - self.num_rows + 1;
        if self.cells.len() >= self.max_rows_scrollback * self.num_cols {
            // if we can't grow the buffer, create room at the end by shifting everything up
            let _ = self.cells.drain(0..num_new_lines * self.num_cols);
            let _ = self.text_format.drain(0..num_new_lines * self.num_cols);
        } else {
            // otherwise, grow the buffer and move the visible area forward
            self.first_visible_line_no += new_row - self.num_rows + 1;
        }

        // we can grow to accomodate
        let new_len = self.cells.len() + num_new_lines * self.num_cols;
        self.cells.resize(new_len, Self::FILL_CHAR);
        self.text_format.resize(new_len, TextFormat::default());
        self.cursor_position = (self.num_rows - 1, new_col);
    }

    fn move_cursor_relative(&mut self, dr: isize, dc: isize) {
        let (r, c) = self.cursor_position;
        let mut new_row = ((r as isize) + dr) as usize;
        let mut new_col = ((c as isize) + dc) as usize;
        new_row += new_col / self.num_cols;
        new_col %= self.num_cols;
        self.move_cursor(new_row, new_col)
    }

    fn update_cursor_cell(&mut self, new_ch: char) {
        let cur_idx = self.cursor_position_to_buf_pos(&self.cursor_position);
        self.cells[cur_idx] = new_ch;
        self.text_format[cur_idx] = self.current_text_format;
    }

    fn lines<'a>(
        &'a self,
        visible_rows: Range<usize>,
    ) -> impl Iterator<Item = (&'a [char], &'a [TextFormat])> + 'a {
        let start = self.cursor_position_to_buf_pos(&(0, 0));
        let end = start + (self.num_rows * self.num_cols);
        self.cells[start..end]
            .chunks_exact(self.num_cols)
            .zip(self.text_format[start..end].chunks_exact(self.num_cols))
        // .skip(visible_rows.start)
        // .take(visible_rows.len())
    }

    fn num_current_rows(&self) -> usize {
        self.cells.len() / self.num_cols
        // self.num_rows
    }
}

// See: https://github.com/sphaerophoria/termie/blob/934f23bfe9ff9ff5a8168ba2bc7ac46e8b64bcfa/src/gui.rs#L26C1-L44C2
fn get_char_size(ctx: &egui::Context, font_id: &egui::FontId) -> (f32, f32) {
    ctx.fonts(move |fonts| {
        // NOTE: Glyph width seems to be a little too wide
        let width = fonts
            .layout(
                "M".to_string(),
                font_id.clone(),
                Color32::WHITE,
                f32::INFINITY,
            )
            .mesh_bounds
            .width();

        let height = fonts.row_height(font_id);

        (width, height + 3.3)
    })
}

#[derive(Debug, Clone, Copy)]
struct TextFormat {
    fg_color: ansi::Color,
    bold: bool,
}

impl Default for TextFormat {
    fn default() -> Self {
        Self {
            fg_color: ansi::Color::BrightBlack,
            bold: false,
        }
    }
}
