use terminal_emulator::TerminalEmulator;

mod ansi;
mod config;
mod fonts;
mod keyboard_handler;
mod pty;
mod terminal_emulator;
mod terminal_input;

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
