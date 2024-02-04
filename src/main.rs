use std::{
    collections::VecDeque,
    os::fd::{AsFd, AsRawFd, OwnedFd},
};

use anyhow::Context;
use eframe::NativeOptions;
use egui::{CentralPanel, FontId, ViewportBuilder};
use log::{debug, error, info};
use nix::{errno::Errno, unistd::ForkResult};

fn main() -> anyhow::Result<()> {
    env_logger::init();

    // Our application state:
    let mut app = TerminalEmulator::new().context("TerminalEmulator initialization failed")?;

    let mut options = NativeOptions::default();

    const SCALE: f32 = 2.0;
    options.viewport = ViewportBuilder::default()
        .with_title("rterm - a toy terminal emulator")
        .with_inner_size(egui::vec2(800.0 * SCALE, 600.0 * SCALE));

    eframe::run_simple_native("rterm", options, move |ctx, _frame| {
        ctx.style_mut(|style| {
            style.override_font_id = Some(FontId::monospace(app.font_size));
        });
        CentralPanel::default().show(ctx, |ui| {
            if let Err(err) = app.update(ui) {
                error!("app.update() failed: {}", err);
            };
            app.render(ctx, ui);
        });
    })
    .map_err(|err| anyhow::format_err!("{}", err))
}

#[derive(Debug)]
struct TerminalEmulator {
    font_size: f32,
    scrollback: LineBuffer,
    pty_fd: OwnedFd,
    buffered_input: String,
}

impl TerminalEmulator {
    fn new() -> anyhow::Result<Self> {
        let pty_fd = create_pty().context("create_pty failed")?;
        set_nonblocking(pty_fd.as_fd()).context("pty: set_nonblocking mode failed")?;
        Ok(Self {
            font_size: 22.0,
            scrollback: LineBuffer::with_capacity(10),
            buffered_input: String::new(),
            pty_fd,
        })
    }

    fn render(&self, _ctx: &egui::Context, ui: &mut egui::Ui) {
        for line in &self.scrollback.lines {
            ui.label(line);
        }
    }

    fn update(&mut self, ui: &mut egui::Ui) -> anyhow::Result<()> {
        self.read_from_pty().context("reading from pty failed")?;

        ui.input(|input_state| -> anyhow::Result<()> {
            for event in &input_state.events {
                self.handle_event(event).context("handling event failed")?;
            }
            Ok(())
        })
        .context("input handling failed")?;

        self.write_to_pty().context("writing to pty failed")?;
        Ok(())
    }

    fn read_from_pty(&mut self) -> anyhow::Result<()> {
        let mut buf = [0u8; 1024];
        match nix::unistd::read(self.pty_fd.as_raw_fd(), &mut buf) {
            Ok(num_read) => {
                self.scrollback.push_bytes(&buf[..num_read]);
                Ok(())
            }
            Err(Errno::EAGAIN) => {
                // No data available
                Ok(())
            }
            Err(unexpected) => Err(anyhow::format_err!("read failed: {}", unexpected)),
        }
    }

    fn write_to_pty(&mut self) -> anyhow::Result<()> {
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

    fn handle_event(&mut self, event: &egui::Event) -> anyhow::Result<()> {
        match event {
            egui::Event::Text(txt) => {
                self.buffered_input += txt;
            }
            _ => (),
        };
        Ok(())
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
             )*
        ]
    };
}
fn create_pty() -> anyhow::Result<OwnedFd> {
    let winsize = None;
    let termios = None;
    let res = unsafe { nix::pty::forkpty(winsize, termios).context("forkpty failed")? };
    match res.fork_result {
        ForkResult::Parent { .. } => Ok(res.master),
        ForkResult::Child => {
            let _ = nix::unistd::execv(cstr!["/bin/zsh\x00"], cstr!["--login\x00", "--no-rcs\x00"])
                .expect("execv failed");
            unreachable!();
        }
    }
}

fn set_nonblocking(fd: impl AsRawFd) -> anyhow::Result<()> {
    use nix::fcntl::{fcntl, FcntlArg, OFlag};

    let bits = fcntl(fd.as_raw_fd(), FcntlArg::F_GETFL).context("fcntl F_GETFL failed")?;
    let mut flags = OFlag::from_bits(bits).unwrap_or(OFlag::empty());
    flags.set(OFlag::O_NONBLOCK, true);
    fcntl(fd.as_raw_fd(), FcntlArg::F_SETFL(flags)).context("fcntl F_SETFL failed")?;
    Ok(())
}

#[derive(Debug)]
struct LineBuffer {
    lines: VecDeque<String>,
    partial_line: VecDeque<u8>,
}

impl LineBuffer {
    fn with_capacity(num_lines: usize) -> Self {
        Self {
            lines: VecDeque::with_capacity(num_lines),
            partial_line: VecDeque::new(),
        }
    }

    fn push_bytes(&mut self, buf: &[u8]) {
        if buf.is_empty() {
            return;
        }

        self.partial_line.extend(buf);
        let mut buf = self.partial_line.make_contiguous();
        while let Some(newline_pos) = buf.iter().position(|&ch| ch == b'\n') {
            let new_line = String::from_utf8_lossy(&buf[..newline_pos]);
            self.lines.push_back(new_line.to_string());
            buf = &mut buf[newline_pos + 1..];
        }
        let remaining = buf.len();
        let n = self.partial_line.len();
        self.partial_line.drain(..n - remaining);
    }
}
