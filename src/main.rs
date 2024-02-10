use crate::terminal_emulator::TerminalEmulator;
use anyhow::Context;

mod ansi;
mod terminal_emulator;

fn main() {
    env_logger::init();

    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "rterm - a toy terminal emulator",
        native_options,
        Box::new(|cc| {
            Box::new(
                TerminalEmulator::new(cc)
                    .context("TerminalEmulator initialization failed")
                    .unwrap(),
            )
        }),
    )
    .unwrap();
}
