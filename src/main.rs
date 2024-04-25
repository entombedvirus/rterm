pub use puffin_egui::puffin;
use terminal_emulator::TerminalEmulator;

mod ansi;
mod config;
mod fonts;
mod grid;
mod grid_string;
mod pty;
mod sum_tree;
mod terminal_emulator;
mod terminal_input;
mod tree;

fn main() {
    env_logger::init();

    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "rterm - a toy terminal emulator",
        native_options,
        Box::new(|cc| {
            Box::new(TerminalEmulator::new(cc).expect("TerminalEmulator initialization failed"))
        }),
    )
    .unwrap();
}
