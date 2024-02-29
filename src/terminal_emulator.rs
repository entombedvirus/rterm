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
use egui::{text::LayoutJob, CentralPanel, Color32, DragValue, FontId, Key, Rect};
use log::info;
use nix::errno::Errno;

#[derive(Debug)]
pub struct TerminalEmulator {
    _io_thread: std::thread::JoinHandle<()>,
    pty_fd: Arc<OwnedFd>,
    token_stream: mpsc::Receiver<VecDeque<ansi::AnsiToken>>,
    pub char_dimensions: Option<egui::Vec2>,
    buffered_input: String,
    grid: AnsiGrid,
    pub regular_font: egui::FontId,
    enable_debug_render: bool,
}

impl eframe::App for TerminalEmulator {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let panel_response = CentralPanel::default().show(ctx, |ui| -> anyhow::Result<()> {
            if self.enable_debug_render {
                ctx.debug_painter()
                    .debug_rect(ui.max_rect(), Color32::YELLOW, "panel");
            }

            let spacing = ui.spacing_mut();
            spacing.item_spacing = egui::Vec2::ZERO;
            spacing.window_margin = egui::Margin::ZERO;

            let char_dims = self
                .char_dimensions
                .insert({
                    let dims = ctx.fonts(|fonts| {
                        let width = fonts.glyph_width(&self.regular_font, 'M');
                        let height = fonts.row_height(&self.regular_font);
                        (width, height).into()
                    });
                    // needed to avoid floating point errors when multiplying
                    ui.painter().round_vec_to_pixels(dims)
                })
                .clone();

            let winsz = pty::compute_winsize(
                ui.painter().round_to_pixel(ui.available_width()),
                ui.painter().round_to_pixel(ui.available_height()),
                char_dims.x,
                char_dims.y,
            );
            if self
                .grid
                .resize(winsz.ws_row as usize, winsz.ws_col as usize)
            {
                pty::update_pty_window_size(self.pty_fd.as_fd(), &winsz)
                    .context("update_pty_window_size")?;
            }

            ui.input(|input_state| -> anyhow::Result<()> {
                for event in &input_state.events {
                    self.handle_event(input_state, event)
                        .context("handling event failed")?;
                }
                Ok(())
            })
            .context("input handling failed")?;

            let read_input = self.read_from_pty(ctx).context("reading from pty failed")?;
            self.write_to_pty().context("writing to pty failed")?;

            ui.with_layout(egui::Layout::top_down_justified(egui::Align::TOP), |ui| {
                ui.set_height(self.grid.num_rows as f32 * char_dims.y);
                ui.set_width(self.grid.num_cols as f32 * char_dims.x);
                if self.enable_debug_render {
                    ctx.debug_painter()
                        .debug_rect(ui.max_rect(), Color32::GREEN, "layout");
                }

                egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .stick_to_bottom(true)
                    .show_rows(
                        ui,
                        char_dims.y,
                        self.grid.num_current_rows(),
                        |ui, visible_rows| -> anyhow::Result<()> {
                            if self.enable_debug_render {
                                ctx.debug_painter().debug_rect(
                                    ui.max_rect(),
                                    Color32::WHITE,
                                    "scroll_area",
                                );
                                for r in 0..visible_rows.len() {
                                    for c in 0..self.grid.num_cols {
                                        let min = egui::Vec2::new(
                                            c as f32 * char_dims.x,
                                            r as f32 * char_dims.y,
                                        );
                                        let rect = egui::Rect::from_min_size(
                                            ui.max_rect().min + min,
                                            char_dims,
                                        );
                                        ui.painter().rect_stroke(
                                            rect,
                                            0.0,
                                            (1.0, Color32::DARK_GRAY),
                                        );
                                    }
                                }
                            }
                            let cursor_rect = self
                                .paint_cursor(ui, &visible_rows)
                                .context("paint_cursor failed")?;
                            self.render(ctx, ui, visible_rows);
                            if read_input && !ui.clip_rect().contains_rect(cursor_rect) {
                                ui.scroll_to_rect(cursor_rect, None);
                            }
                            Ok(())
                        },
                    );
            });
            Ok(())
        });

        panel_response.response.context_menu(|ui| {
            ui.horizontal(|ui| {
                ui.label("Font Size:");
                ui.add(DragValue::new(&mut self.regular_font.size).clamp_range(2.0..=88.0));
            });
            ui.checkbox(&mut self.enable_debug_render, "Enable Debug Renderer");
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

        let regular_font = FontId::monospace(24.0);
        // cc.egui_ctx.set_pixels_per_point(2.0);

        Ok(Self {
            _io_thread,
            buffered_input: String::new(),
            grid: AnsiGrid::new(24, 80, 5000),
            pty_fd,
            token_stream: rx,
            char_dimensions: None,
            regular_font,
            enable_debug_render: false,
        })
    }

    pub fn render(
        &self,
        _ctx: &egui::Context,
        ui: &mut egui::Ui,
        visible_rows: Range<usize>,
    ) -> Vec<egui::Response> {
        ui.style_mut().override_font_id = Some(self.regular_font.clone());
        let mut label_responses = Vec::new();
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
            label_responses.push(ui.label(galley));
        }
        label_responses
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
            _ => log::trace!("unhandled event: {event:?}"),
        };
        Ok(())
    }

    pub fn paint_cursor(
        &self,
        ui: &mut egui::Ui,
        visible_rows: &Range<usize>,
    ) -> anyhow::Result<egui::Rect> {
        let (cursor_row_in_screen_space, cursor_col) = self.grid.cursor_position;
        let cursor_row_in_scrollback_space =
            self.grid.first_visible_line_no() + cursor_row_in_screen_space;
        let num_rows_from_top = cursor_row_in_scrollback_space.saturating_sub(visible_rows.start);
        let char_dims = self.char_dimensions.unwrap_or(egui::vec2(12.0, 12.0));
        let cursor_rect = Rect::from_min_size(
            egui::pos2(
                cursor_col as f32 * char_dims.x,
                num_rows_from_top as f32 * char_dims.y,
            ) + ui.max_rect().min.to_vec2(),
            char_dims,
        );
        ui.painter().rect_filled(cursor_rect, 0.0, Color32::GOLD);
        Ok(cursor_rect)
    }

    fn read_from_pty(&mut self, ctx: &egui::Context) -> anyhow::Result<bool> {
        match self.token_stream.try_recv() {
            Ok(tokens) => {
                for token in tokens {
                    self.grid.update(&token);
                }
                // keep requesting painting a new frame as long as we keep getting data. Only
                // processing a subset of data each frame keeps the UI responsive to the user
                // sending Ctrl-C etc
                ctx.request_repaint();
                Ok(true)
            }
            Err(mpsc::TryRecvError::Disconnected) => {
                ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                Ok(false)
            }
            Err(mpsc::TryRecvError::Empty) => Ok(false),
        }
    }
}

#[derive(Debug)]
struct AnsiGrid {
    num_rows: usize,
    num_cols: usize,
    max_rows_scrollback: usize,
    cells: Vec<char>,
    text_format: Vec<TextFormat>,
    cursor_position: (usize, usize), // in the range (0..num_rows, 0..num_cols)
    current_text_format: TextFormat,
}

impl AnsiGrid {
    const FILL_CHAR: char = '-';

    fn new(num_rows: usize, num_cols: usize, max_rows_scrollback: usize) -> Self {
        // scrollback has to be at least num_rows
        let max_rows_scrollback = max_rows_scrollback.max(num_rows);
        let cursor_position = (0, 0);
        Self {
            num_rows,
            num_cols,
            max_rows_scrollback,
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
        let new_len = (self.first_visible_line_no() + new_num_rows) * new_num_cols;
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
            CursorControl(CursorControl::ScrollUpFromHome) => {
                let (cursor_row, _) = self.cursor_position;
                if cursor_row > 0 {
                    // no need to scroll
                    self.move_cursor_relative(-1, 0);
                } else {
                    let home_row_start = self.cursor_position_to_buf_pos(&(0, 0));
                    let next_row_start = home_row_start + self.num_cols;
                    let last_row_start = self.cells.len() - self.num_cols;
                    self.cells
                        .copy_within(home_row_start..last_row_start, next_row_start);
                    self.text_format
                        .copy_within(home_row_start..last_row_start, next_row_start);
                    // clear the new row created on top
                    self.cells[home_row_start..next_row_start].fill(Self::FILL_CHAR);
                    self.text_format[home_row_start..next_row_start].fill(TextFormat::default());
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
        };
    }

    fn cursor_position_to_buf_pos(&self, (r, c): &(usize, usize)) -> usize {
        let start = self
            .cells
            .len()
            .saturating_sub(self.num_rows * self.num_cols);
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

        // if we can't grow the buffer, create room at the end by shifting everything up
        let num_new_lines = new_row - self.num_rows + 1;
        if self.cells.len() >= self.max_rows_scrollback * self.num_cols {
            let _ = self.cells.drain(0..num_new_lines * self.num_cols);
            let _ = self.text_format.drain(0..num_new_lines * self.num_cols);
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
        // let start = self.cursor_position_to_buf_pos(&(0, 0));
        // let end = start + (self.num_rows * self.num_cols);
        self.cells
            .chunks_exact(self.num_cols)
            .zip(self.text_format.chunks_exact(self.num_cols))
            .skip(visible_rows.start)
            .take(visible_rows.len())
    }

    fn num_current_rows(&self) -> usize {
        self.cells.len() / self.num_cols
        // self.num_rows
    }

    fn first_visible_line_no(&self) -> usize {
        self.cursor_position_to_buf_pos(&(0, 0)) / self.num_cols
    }
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
