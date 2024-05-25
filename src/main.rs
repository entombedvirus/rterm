use std::sync::Arc;

use buffer::Buffer;
use eframe::{App, CreationContext};
use grid::GridStack;
pub use puffin_egui::puffin;
use terminal_emulator::TerminalEmulator;

mod ansi;
mod buffer;
mod config;
mod fonts;
mod grid;
mod grid_string;
mod pty;
mod terminal_emulator;
mod terminal_input;
mod tree;

fn main() {
    env_logger::init();

    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "rterm - a toy terminal emulator",
        native_options,
        Box::new(create_app),
    )
    .unwrap();
}

fn create_app(cc: &CreationContext) -> Box<dyn App> {
    let grids = Arc::new(Buffer::new(GridStack::new(24, 80)));
    let child_process = terminal_input::ChildProcess::spawn(cc.egui_ctx.clone(), grids.clone());
    let emulator = TerminalEmulator::new(cc, child_process, grids)
        .expect("TerminalEmulator initialization failed");
    Box::new(emulator)
}
