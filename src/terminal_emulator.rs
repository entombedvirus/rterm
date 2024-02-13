#![allow(unused)]

use std::{
    borrow::Cow,
    os::fd::{AsFd, AsRawFd, OwnedFd, RawFd},
};

use crate::{
    ansi::{self, CursorControl},
    pty,
};
use ansi::AsciiControl;
use anyhow::Context;
use egui::{CentralPanel, Color32, FontId, Key, Rect};
use log::{debug, info};
use nix::errno::Errno;

#[derive(Debug)]
pub struct TerminalEmulator {
    scrollback: LineBuffer,
    parser: ansi::Parser,
    pty_fd: OwnedFd,
    window_rect: Option<Rect>,
    pub char_dimensions: Option<egui::Vec2>,
    buffered_input: String,
    grid: AnsiGrid,
    pub font: egui::FontId,
}

impl eframe::App for TerminalEmulator {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| -> anyhow::Result<()> {
            if self.char_dimensions.is_none() {
                self.char_dimensions = Some(get_char_size(ctx, &self.font).into());
            }

            let rect = ctx
                .input(|i| i.viewport().inner_rect)
                .context("viewport.inner_rect failed")?;
            if self.window_rect != Some(rect) {
                let winsz = Self::get_pty_winsize(ctx, self.char_dimensions.unwrap())
                    .context("updating window size failed")?;
                self.grid.resize(winsz.ws_row, winsz.ws_col);
                self.window_rect = Some(rect);
                pty::update_pty_window_size(self.pty_fd.as_fd(), &winsz)
                    .context("update_pty_window_size")?;
            }

            self.read_from_pty().context("reading from pty failed")?;

            ui.input(|input_state| -> anyhow::Result<()> {
                for event in &input_state.events {
                    self.handle_event(event).context("handling event failed")?;
                }
                Ok(())
            })
            .context("input handling failed")?;

            self.write_to_pty().context("writing to pty failed")?;
            self.render(ctx, ui);

            self.paint_cursor(ui).context("paint_cursor failed")?;
            Ok(())
        });
    }
}

impl TerminalEmulator {
    pub fn new(cc: &eframe::CreationContext<'_>, pty_fd: OwnedFd) -> anyhow::Result<Self> {
        let font = FontId::monospace(22.0);
        cc.egui_ctx.style_mut(|style| {
            // style.override_text_style = Some(egui::TextStyle::Monospace);
            style.override_font_id = Some(font.clone());
        });
        // cc.egui_ctx.set_pixels_per_point(2.0);

        Ok(Self {
            scrollback: LineBuffer::with_capacity(10),
            buffered_input: String::new(),
            grid: AnsiGrid::new(0, 0),
            pty_fd,
            window_rect: None,
            char_dimensions: None,
            font,
            parser: ansi::Parser::new(),
        })
    }

    pub fn render(&self, _ctx: &egui::Context, ui: &mut egui::Ui) {
        for line in self.grid.lines() {
            ui.label(line);
        }
    }

    pub fn read_from_pty(&mut self) -> anyhow::Result<()> {
        let mut buf = [0u8; 1024];
        match nix::unistd::read(self.pty_fd.as_raw_fd(), &mut buf) {
            Ok(num_read) => {
                debug!(
                    "read {} bytes from pty: {as_str:?}",
                    num_read,
                    as_str = std::str::from_utf8(&buf[..num_read])
                );
                self.parser.push_bytes(&buf[..num_read]);

                let mut dst = [0u8; 10];
                for token in self.parser.tokens() {
                    self.grid.update(&token);
                    match token {
                        ansi::AnsiToken::AsciiControl(
                            ctrl @ (AsciiControl::CarriageReturn | AsciiControl::LineFeed),
                        ) => {
                            dst[0] = ctrl as u8;
                            self.scrollback.push_bytes(&dst[0..1])
                        }
                        ansi::AnsiToken::Char(ch) => self
                            .scrollback
                            .push_bytes(ch.encode_utf8(&mut dst).as_bytes()),
                        _ => (),
                    }
                }
                Ok(())
            }
            Err(Errno::EAGAIN) => {
                // No data available
                Ok(())
            }
            Err(unexpected) => Err(anyhow::format_err!("read failed: {}", unexpected)),
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

    pub fn handle_event(&mut self, event: &egui::Event) -> anyhow::Result<()> {
        match event {
            egui::Event::Text(txt) => {
                self.buffered_input += txt;
            }
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
            _ => (),
        };
        Ok(())
    }

    pub fn get_pty_winsize(
        ctx: &egui::Context,
        char_dimensions: egui::Vec2,
    ) -> anyhow::Result<nix::pty::Winsize> {
        let rect = ctx
            .input(|i| i.viewport().inner_rect)
            .context("viewport.inner_rect failed")?;
        let (char_width, char_height) = char_dimensions.into();
        let num_chars_x = rect.width() / char_width;
        let num_chars_y = rect.height() / char_height;
        Ok(nix::pty::Winsize {
            ws_row: num_chars_y.floor() as u16,
            ws_col: (num_chars_x.floor() as u16).saturating_sub(1),
            ws_xpixel: rect.width().ceil() as u16,
            ws_ypixel: rect.height().ceil() as u16,
        })
    }

    pub fn paint_cursor(&self, ui: &mut egui::Ui) -> anyhow::Result<()> {
        let char_dims = self.char_dimensions.unwrap_or(egui::vec2(12.0, 12.0));
        let (cursor_row, cursor_col) = self.grid.cursor_position;
        let rect = Rect::from_min_size(
            egui::pos2(
                (cursor_col as f32 * char_dims.x) + 14.0,
                (cursor_row as f32 * char_dims.y) + 8.0,
            ),
            char_dims,
        );
        ui.painter().rect_filled(rect, 5.0, Color32::GOLD);
        Ok(())
    }
}

#[derive(Debug)]
struct AnsiGrid {
    num_rows: usize,
    num_cols: usize,
    cells: Vec<char>,
    cursor_position: (usize, usize),
}

// impl Default for AnsiCell {
//     fn default() -> Self {
//         Self::Char(' ')
//     }
// }

impl AnsiGrid {
    fn new(num_rows: usize, num_cols: usize) -> Self {
        let cursor_position = (0, 0);
        Self {
            num_rows,
            num_cols,
            cells: Vec::new(),
            cursor_position,
        }
    }

    fn resize(&mut self, ws_row: u16, ws_col: u16) {
        self.num_rows = ws_row as usize;
        self.num_cols = ws_col as usize;
        self.cells.resize(self.num_rows * self.num_cols, ' ');
    }

    fn update(&mut self, token: &ansi::AnsiToken) {
        use ansi::AnsiToken::*;
        use ansi::AsciiControl;
        use ansi::CursorControl;
        use ansi::EraseControl;
        let mut cur_idx = {
            let (cur_row, cur_col) = self.cursor_position;
            cur_row * self.num_cols + cur_col
        };

        match token {
            Char(ch) => {
                self.cells[cur_idx] = *ch;
                cur_idx += 1;
            }
            AsciiControl(AsciiControl::Backspace) => {
                cur_idx -= 1;
            }
            AsciiControl(AsciiControl::Tab) => {
                cur_idx += 4;
            }
            AsciiControl(AsciiControl::LineFeed) => {
                cur_idx += self.num_cols;
            }
            AsciiControl(AsciiControl::CarriageReturn) => {
                cur_idx -= self.cursor_position.1;
            }
            CursorControl(CursorControl::MoveTo { line, col }) => {
                let line = line.saturating_sub(1);
                let col = col.saturating_sub(1);
                cur_idx = line * self.num_cols + col;
            }
            CursorControl(CursorControl::MoveLineUp) => {
                let last_row_start = self.cells.len() - self.num_cols;
                self.cells.copy_within(..last_row_start, self.num_cols);
                self.cells[..self.num_cols].fill(' ');
            }
            EraseControl(EraseControl::Screen) => self.cells.fill(' '),
            EraseControl(EraseControl::FromCursorToEndOfScreen) => {
                self.cells[cur_idx..].fill(' ');
            }
            EraseControl(EraseControl::FromCursorToEndOfLine) => {
                let line_end_idx = (self.cursor_position.0 + 1) * self.num_cols;
                self.cells[cur_idx..line_end_idx].fill(' ');
            }
            ignored => info!("ignoring ansii token: {ignored:?}"),
        }

        let max_idx = self.cells.len().saturating_sub(1);
        self.cursor_position = if cur_idx > max_idx {
            let num_lines_to_scroll = (cur_idx / self.num_cols) - self.num_rows + 1;
            self.cells
                .copy_within(num_lines_to_scroll * self.num_cols.., 0);
            let last_n_lines = self.cells.len() - (self.num_cols * num_lines_to_scroll)..;
            self.cells[last_n_lines].fill(' ');
            (self.num_rows - 1, cur_idx % self.num_cols)
        } else {
            (cur_idx / self.num_cols, cur_idx % self.num_cols)
        }
    }

    fn lines<'a>(&'a self) -> impl Iterator<Item = String> + 'a {
        self.cells
            .chunks_exact(self.num_cols)
            .map(|line_chars| String::from_iter(line_chars))
    }
}

#[derive(Debug)]
struct LineBuffer {
    buf: Vec<u8>,
}

impl LineBuffer {
    fn with_capacity(num_bytes: usize) -> Self {
        Self {
            buf: Vec::with_capacity(num_bytes),
        }
    }

    fn push_bytes(&mut self, buf: &[u8]) {
        if buf.is_empty() {
            return;
        }
        self.buf.extend(buf);
    }

    fn as_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.buf.as_slice())
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
