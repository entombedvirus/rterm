#![allow(unused)]

use std::{
    borrow::Cow,
    os::fd::{AsFd, AsRawFd, OwnedFd},
};

use crate::ansi;
use ansi::AsciiControl;
use anyhow::Context;
use egui::{CentralPanel, Color32, FontId, Key, Rect};
use log::info;
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
            self.update_win_size(ui)
                .context("updating window size failed")?;

            self.read_from_pty().context("reading from pty failed")?;

            ui.input(|input_state| -> anyhow::Result<()> {
                for event in &input_state.events {
                    self.handle_event(event).context("handling event failed")?;
                }
                Ok(())
            })
            .context("input handling failed")?;

            self.paint_cursor(ui).context("paint_cursor failed")?;
            self.write_to_pty().context("writing to pty failed")?;

            self.render(ctx, ui);
            Ok(())
        });
    }
}

impl TerminalEmulator {
    pub fn new(cc: &eframe::CreationContext<'_>) -> anyhow::Result<Self> {
        cc.egui_ctx.style_mut(|style| {
            style.override_font_id = Some(FontId::monospace(22.0));
        });
        let pty_fd = pty::create_pty().context("create_pty failed")?;
        let pty_winsz =
            pty::get_pty_window_size(pty_fd.as_fd()).context("get_pty_window_size failed")?;
        pty::set_nonblocking(pty_fd.as_fd()).context("pty: set_nonblocking mode failed")?;
        Ok(Self {
            scrollback: LineBuffer::with_capacity(10),
            buffered_input: String::new(),
            grid: AnsiGrid::new(pty_winsz.ws_row as usize, pty_winsz.ws_col as usize),
            pty_fd,
            window_rect: None,
            char_dimensions: None,
            font: FontId::monospace(22.0),
            parser: ansi::Parser::new(),
        })
    }

    pub fn render(&self, _ctx: &egui::Context, ui: &mut egui::Ui) {
        ui.label(self.scrollback.as_str());
    }

    pub fn read_from_pty(&mut self) -> anyhow::Result<()> {
        let mut buf = [0u8; 1024];
        match nix::unistd::read(self.pty_fd.as_raw_fd(), &mut buf) {
            Ok(num_read) => {
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
                key: Key::Enter,
                pressed: true,
                repeat: false,
                ..
            } => {
                self.buffered_input += "\n";
            }
            _ => (),
        };
        Ok(())
    }

    pub fn update_win_size(&mut self, ui: &mut egui::Ui) -> anyhow::Result<()> {
        let rect = ui
            .ctx()
            .input(|i| i.viewport().inner_rect)
            .context("viewport.inner_rect failed")?;
        if self.window_rect != Some(rect) {
            let (char_width, char_height) = get_char_size(ui.ctx(), &self.font);
            let num_chars_x = rect.width() / char_width;
            let num_chars_y = rect.height() / char_height;
            let winsz = nix::pty::Winsize {
                ws_row: num_chars_x.ceil() as u16,
                ws_col: num_chars_y.ceil() as u16,
                ws_xpixel: rect.width().ceil() as u16,
                ws_ypixel: rect.height().ceil() as u16,
            };
            pty::update_pty_window_size(self.pty_fd.as_fd(), &winsz)
                .context("update_pty_window_size")?;
            self.grid.resize(winsz.ws_row, winsz.ws_col);
            self.window_rect = Some(rect);
            self.char_dimensions = Some(egui::vec2(char_width, char_height));
        }

        Ok(())
    }

    pub fn paint_cursor(&self, ui: &mut egui::Ui) -> anyhow::Result<()> {
        let char_dims = self.char_dimensions.unwrap_or(egui::vec2(12.0, 12.0));
        let (cursor_row, cursor_col) = self.grid.cursor_position;
        let rect = Rect::from_min_size(
            egui::pos2(
                cursor_col as f32 * char_dims.x,
                cursor_row as f32 * char_dims.y,
            ),
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
        use ansi::EraseControl;
        let mut cur_idx = {
            let (cur_row, cur_col) = self.cursor_position;
            cur_row * self.num_cols + cur_col
        };

        let max_idx = self.cells.len().saturating_sub(1) as isize;
        let mut move_horiz = |steps: isize| {
            let prev = cur_idx;
            cur_idx = (cur_idx as isize + steps).clamp(0, max_idx) as usize;
            prev
        };

        match token {
            Char(ch) => {
                self.cells[move_horiz(1)] = *ch;
            }
            AsciiControl(AsciiControl::Backspace) => {
                move_horiz(-1);
            }
            AsciiControl(AsciiControl::Tab) => {
                move_horiz(8);
            }
            AsciiControl(AsciiControl::LineFeed) => {
                move_horiz(self.num_cols as isize);
            }
            AsciiControl(AsciiControl::CarriageReturn) => {
                move_horiz(self.cursor_position.1 as isize * -1);
            }
            EraseControl(EraseControl::Screen) => self.cells.fill(' '),
            ignored => info!("ignoring ansii token: {ignored:?}"),
        }

        self.cursor_position = (cur_idx / self.num_cols, cur_idx % self.num_cols);
    }
}

macro_rules! cstr {
    ($lit:literal) => {
        std::ffi::CStr::from_bytes_with_nul($lit.as_bytes()).expect("invalid CStr literal")
    };
    ( $( $lit:literal ), + ) => {
        &[
            $(
                std::ffi::CStr::from_bytes_with_nul($lit.as_bytes()).expect("invalid CStr literal"),
             )+
        ]
    };
}

mod pty {
    use std::os::fd::{AsRawFd, OwnedFd};

    use anyhow::Context;
    use nix::unistd::ForkResult;

    pub fn create_pty() -> anyhow::Result<OwnedFd> {
        let winsize = None;
        let termios = None;
        let res = unsafe { nix::pty::forkpty(winsize, termios).context("forkpty failed")? };
        match res.fork_result {
            ForkResult::Parent { .. } => Ok(res.master),
            ForkResult::Child => {
                let _ = nix::unistd::execve(
                    cstr!["/bin/zsh\x00"],
                    cstr!["--login\x00", "--no-rcs\x00"],
                    cstr!["TERM=xterm\x00", "PS1=$ \x00"],
                )
                .expect("execve failed");
                unreachable!();
            }
        }
    }

    nix::ioctl_read_bad!(tiocgwinsz, libc::TIOCGWINSZ, nix::pty::Winsize);

    pub fn get_pty_window_size(fd: impl AsRawFd) -> anyhow::Result<nix::pty::Winsize> {
        let mut winsz = nix::pty::Winsize {
            ws_row: 0,
            ws_col: 0,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        let res = unsafe { tiocgwinsz(fd.as_raw_fd(), &mut winsz as *mut _) };
        match res {
            Ok(_) => Ok(winsz),
            Err(err) => Err(anyhow::format_err!("{err}")),
        }
    }

    pub fn set_nonblocking(fd: impl AsRawFd) -> anyhow::Result<()> {
        use nix::fcntl::{fcntl, FcntlArg, OFlag};

        let bits = fcntl(fd.as_raw_fd(), FcntlArg::F_GETFL).context("fcntl F_GETFL failed")?;
        let mut flags = OFlag::from_bits(bits).unwrap_or(OFlag::empty());
        flags.set(OFlag::O_NONBLOCK, true);
        fcntl(fd.as_raw_fd(), FcntlArg::F_SETFL(flags)).context("fcntl F_SETFL failed")?;
        Ok(())
    }

    nix::ioctl_write_ptr_bad!(tiocswinsz, libc::TIOCSWINSZ, nix::pty::Winsize);

    pub fn update_pty_window_size(
        fd: impl AsRawFd,
        winsz: &nix::pty::Winsize,
    ) -> anyhow::Result<()> {
        match unsafe { tiocswinsz(fd.as_raw_fd(), winsz as *const _) } {
            Ok(_) => Ok(()),
            Err(err) => Err(anyhow::format_err!("tiocswinsz failed: {err}")),
        }
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
pub fn get_char_size(ctx: &egui::Context, font_id: &egui::FontId) -> (f32, f32) {
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

        (width, height + 5.0)
    })
}
