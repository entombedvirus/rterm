use std::{
    collections::VecDeque,
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

            self.paint_cursor(ui).context("paint_cursor failed")?;
            self.render(ctx, ui);

            Ok(())
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
            grid: AnsiGrid::new(24, 80),
            pty_fd,
            token_stream: rx,
            window_rect: None,
            char_dimensions: None,
            regular_font,
        })
    }

    pub fn render(&self, _ctx: &egui::Context, ui: &mut egui::Ui) {
        for (line_chars, formats) in self.grid.lines() {
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
                repeat: false,
                ..
            } => self.buffered_input.push(AsciiControl::Escape.into()),
            egui::Event::Key {
                key: Key::Tab,
                pressed: true,
                repeat: false,
                ..
            } => self.buffered_input.push(AsciiControl::Tab.into()),
            egui::Event::Key {
                key: Key::Backspace,
                pressed: true,
                repeat: false,
                ..
            } => self.buffered_input.push(AsciiControl::Backspace.into()),
            egui::Event::Key {
                key: Key::Enter,
                pressed: true,
                repeat: false,
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
            _ => log::info!("unhandled event: {event:?}"),
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
    cells: Vec<char>,
    text_format: Vec<TextFormat>,
    cursor_position: (usize, usize),
    current_text_format: TextFormat,
}

impl AnsiGrid {
    const FILL_CHAR: char = '-';

    fn new(num_rows: usize, num_cols: usize) -> Self {
        let cursor_position = (0, 0);
        Self {
            num_rows,
            num_cols,
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
        eprintln!(
            "resize: {} x {} -> {new_num_rows} x {new_num_cols}",
            self.num_rows, self.num_cols
        );
        let mut new_cells = vec![Self::FILL_CHAR; new_num_rows * new_num_cols];
        let mut new_format = vec![TextFormat::default(); new_num_rows * new_num_cols];
        let line_len = self.num_cols.min(new_num_cols);
        for (line_no, (old_cells, old_formats)) in self
            .cells
            .chunks_exact(self.num_cols)
            .zip(self.text_format.chunks_exact(self.num_cols))
            .enumerate()
        {
            if line_no >= new_num_rows {
                break;
            }
            let new_line_start = line_no * new_num_cols;
            let copy_range = new_line_start..new_line_start + line_len;
            new_cells[copy_range.clone()].copy_from_slice(&old_cells[..line_len]);
            new_format[copy_range.clone()].copy_from_slice(&old_formats[..line_len]);
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
                    // shift all lines down by one line
                    let last_row_start = self.cells.len() - self.num_cols;
                    self.cells.copy_within(..last_row_start, self.num_cols);
                    self.text_format
                        .copy_within(..last_row_start, self.num_cols);
                    self.cells[..self.num_cols].fill(Self::FILL_CHAR);
                    self.text_format[..self.num_cols].fill(TextFormat::default());
                }
            }
            EraseControl(EraseControl::Screen) => {
                self.cells.fill(Self::FILL_CHAR);
                self.text_format.fill(TextFormat::default());
            }
            EraseControl(EraseControl::FromCursorToEndOfScreen) => {
                let cur_idx = self.cursor_position_to_buf_pos();
                self.cells[cur_idx..].fill(Self::FILL_CHAR);
                self.text_format[cur_idx..].fill(TextFormat::default());
            }
            EraseControl(EraseControl::FromCursorToEndOfLine) => {
                let cur_idx = self.cursor_position_to_buf_pos();
                let line_end_idx = (self.cursor_position.0 + 1) * self.num_cols;
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

    fn cursor_position_to_buf_pos(&self) -> usize {
        let (r, c) = self.cursor_position;
        r * self.num_cols + c
    }

    fn move_cursor(&mut self, new_row: usize, new_col: usize) {
        let cur_idx = new_row * self.num_cols + new_col;
        let max_idx = self.cells.len().saturating_sub(1);
        self.cursor_position = if cur_idx > max_idx {
            let num_lines_to_scroll = (cur_idx / self.num_cols) - self.num_rows + 1;
            self.cells
                .copy_within(num_lines_to_scroll * self.num_cols.., 0);
            self.text_format
                .copy_within(num_lines_to_scroll * self.num_cols.., 0);
            let last_n_lines = self.cells.len() - (self.num_cols * num_lines_to_scroll)..;
            self.cells[last_n_lines.clone()].fill(Self::FILL_CHAR);
            self.text_format[last_n_lines].fill(TextFormat::default());
            (self.num_rows - 1, cur_idx % self.num_cols)
        } else {
            (cur_idx / self.num_cols, cur_idx % self.num_cols)
        }
    }

    fn move_cursor_relative(&mut self, dr: isize, dc: isize) {
        let (r, c) = self.cursor_position;
        let new_row = (r as isize) + dr;
        let new_col = (c as isize) + dc;
        self.move_cursor(new_row as usize, new_col as usize)
    }

    fn update_cursor_cell(&mut self, new_ch: char) {
        let cur_idx = self.cursor_position_to_buf_pos();
        self.cells[cur_idx] = new_ch;
        self.text_format[cur_idx] = self.current_text_format;
    }

    fn lines<'a>(&'a self) -> impl Iterator<Item = (&'a [char], &'a [TextFormat])> + 'a {
        self.cells
            .chunks_exact(self.num_cols)
            .zip(self.text_format.chunks_exact(self.num_cols))
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
