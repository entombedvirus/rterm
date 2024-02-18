#![allow(unused)]

use std::{
    borrow::Cow,
    collections::BTreeMap,
    os::fd::{AsFd, AsRawFd, OwnedFd, RawFd},
};

use crate::{
    ansi::{self, CursorControl, SgrControl},
    pty,
};
use ansi::AsciiControl;
use anyhow::Context;
use egui::{text::LayoutJob, CentralPanel, Color32, FontId, Key, Rect};
use log::{debug, info};
use nix::errno::Errno;

#[derive(Debug)]
pub struct TerminalEmulator {
    parser: ansi::Parser,
    pty_fd: OwnedFd,
    window_rect: Option<Rect>,
    pub char_dimensions: Option<egui::Vec2>,
    buffered_input: String,
    grid: AnsiGrid,
    pub regular_font: egui::FontId,
}

impl eframe::App for TerminalEmulator {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| -> anyhow::Result<()> {
            if self.char_dimensions.is_none() {
                self.char_dimensions = Some(get_char_size(ctx, &self.regular_font).into());
            }

            let rect = ui.available_rect_before_wrap();
            if self.window_rect != Some(rect) {
                let winsz = Self::get_pty_winsize(rect.size(), self.char_dimensions.unwrap());
                self.grid.resize(winsz.ws_row, winsz.ws_col);
                self.window_rect = Some(rect);
                pty::update_pty_window_size(self.pty_fd.as_fd(), &winsz)
                    .context("update_pty_window_size")?;
            }

            self.read_from_pty().context("reading from pty failed")?;

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
    pub fn new(cc: &eframe::CreationContext<'_>, pty_fd: OwnedFd) -> anyhow::Result<Self> {
        let regular_font = FontId::monospace(22.0);
        cc.egui_ctx.style_mut(|style| {
            // style.override_text_style = Some(egui::TextStyle::Monospace);
            style.override_font_id = Some(regular_font.clone());
        });
        // cc.egui_ctx.set_pixels_per_point(2.0);

        Ok(Self {
            buffered_input: String::new(),
            grid: AnsiGrid::new(0, 0),
            pty_fd,
            window_rect: None,
            char_dimensions: None,
            regular_font,
            parser: ansi::Parser::new(),
        })
    }

    pub fn render(&self, _ctx: &egui::Context, ui: &mut egui::Ui) {
        for (line_chars, formats) in self.grid.lines() {
            let mut layout = LayoutJob::default();
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
            ui.label(layout);
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
                debug!("parsed_tokens: {:?}", &self.parser.parsed_tokens);

                for token in self.parser.tokens() {
                    self.grid.update(&token);
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

    pub fn handle_event(
        &mut self,
        input_state: &egui::InputState,
        event: &egui::Event,
    ) -> anyhow::Result<()> {
        match event {
            egui::Event::Text(txt) => {
                self.buffered_input += if input_state.modifiers.alt {
                    match txt.as_str() {
                        "å" => "\u{1b}a",
                        "∫" => "\u{1b}b",
                        "ç" => "\u{1b}c",
                        "∂" => "\u{1b}d",
                        "e" => "\u{1b}e",
                        "ƒ" => "\u{1b}f",
                        "©" => "\u{1b}g",
                        "˙" => "\u{1b}h",
                        "i" => "\u{1b}i",
                        "∆" => "\u{1b}j",
                        "˚" => "\u{1b}k",
                        "¬" => "\u{1b}l",
                        "µ" => "\u{1b}m",
                        "n" => "\u{1b}n",
                        "ø" => "\u{1b}o",
                        "π" => "\u{1b}p",
                        "œ" => "\u{1b}q",
                        "®" => "\u{1b}r",
                        "ß" => "\u{1b}s",
                        "†" => "\u{1b}t",
                        "u" => "\u{1b}u",
                        "√" => "\u{1b}v",
                        "∑" => "\u{1b}w",
                        "≈" => "\u{1b}x",
                        "¥" => "\u{1b}y",
                        "Ω" => "\u{1b}z",
                        _ => txt,
                    }
                } else {
                    txt
                };
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
            egui::Event::Key {
                key,
                pressed: true,
                modifiers: egui::Modifiers::CTRL,
                ..
            } => {
                match key {
                    Key::A => self.buffered_input.push_str("\u{01}"),
                    Key::B => self.buffered_input.push_str("\u{02}"),
                    // mapts to ascii end of text
                    Key::C => self.buffered_input.push_str("\u{03}"),
                    // mapts to ascii end of transmission
                    Key::D => self.buffered_input.push_str("\u{04}"),
                    Key::E => self.buffered_input.push_str("\u{05}"),
                    // mapts to ascii form feed 0xc
                    Key::L => self.buffered_input.push_str("\u{0c}"),
                    // mapts to ascii data link escape
                    Key::P => self.buffered_input.push_str("\u{10}"),
                    // mapts to ascii device control 2
                    Key::R => self.buffered_input.push_str("\u{12}"),
                    // mapts to ascii end of transmission block
                    Key::W => self.buffered_input.push_str("\u{17}"),
                    Key::X => self.buffered_input.push_str("\u{18}"),
                    _ => (),
                }
            }
            _ => (),
        };
        Ok(())
    }

    pub fn get_pty_winsize(
        egui::Vec2 {
            x: rect_width,
            y: rect_height,
        }: egui::Vec2,
        char_dimensions: egui::Vec2,
    ) -> nix::pty::Winsize {
        let (char_width, char_height) = char_dimensions.into();
        let num_chars_x = rect_width / char_width;
        let num_chars_y = rect_height / char_height;
        nix::pty::Winsize {
            ws_row: num_chars_y.floor() as u16,
            ws_col: (num_chars_x.floor() as u16),
            ws_xpixel: rect_width.floor() as u16,
            ws_ypixel: rect_height.floor() as u16,
        }
    }

    pub fn paint_cursor(&self, ui: &mut egui::Ui) -> anyhow::Result<()> {
        let rect = ui.available_rect_before_wrap();
        let spacing = ui.spacing();
        let char_dims = self.char_dimensions.unwrap_or(egui::vec2(12.0, 12.0));
        let (cursor_row, cursor_col) = self.grid.cursor_position;
        let rect = Rect::from_min_size(
            egui::pos2(
                (cursor_col as f32 * char_dims.x),
                (cursor_row as f32 * char_dims.y),
            ) + rect.min.to_vec2(),
            char_dims,
        );
        ui.painter().rect_filled(rect, 0.0, Color32::GOLD);
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

// impl Default for AnsiCell {
//     fn default() -> Self {
//         Self::Char(' ')
//     }
// }

impl AnsiGrid {
    const FILL_CHAR: char = '-';

    fn new(num_rows: usize, num_cols: usize) -> Self {
        let cursor_position = (0, 0);
        Self {
            num_rows,
            num_cols,
            cells: Vec::new(),
            text_format: vec![TextFormat::default(); num_rows * num_cols],
            cursor_position,
            current_text_format: TextFormat::default(),
        }
    }

    fn resize(&mut self, ws_row: u16, ws_col: u16) {
        self.num_rows = ws_row as usize;
        self.num_cols = ws_col as usize;
        self.cells
            .resize(self.num_rows * self.num_cols, Self::FILL_CHAR);
        self.text_format
            .resize(self.num_rows * self.num_cols, TextFormat::default());
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
                self.update_cursor_cell(Self::FILL_CHAR);
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
            SGR(SgrControl::Bold) => {
                self.current_text_format.bold = true;
            }
            SGR(SgrControl::ForgroundColor(color)) => {
                self.current_text_format.fg_color = *color;
            }
            SGR(SgrControl::Reset) => {
                self.current_text_format = TextFormat::default();
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
