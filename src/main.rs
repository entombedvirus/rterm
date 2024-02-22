use std::os::fd::AsFd;

use crate::terminal_emulator::TerminalEmulator;

mod ansi;
mod pty;
mod terminal_emulator;
mod terminal_input;

fn main() {
    env_logger::init();

    let pty_fd = pty::create_pty().expect("create_pty failed");
    pty::set_nonblocking(pty_fd.as_fd()).expect("pty: set_nonblocking mode failed");

    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "rterm - a toy terminal emulator",
        native_options,
        Box::new(|cc| {
            Box::new(
                TerminalEmulator::new(cc, pty_fd).expect("TerminalEmulator initialization failed"),
            )
        }),
    )
    .unwrap();
}
