use std::sync::Arc;

use eframe::{App, CreationContext};
use egui::mutex::RwLock;
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

// - pull out the ChildProcess::spawn outside of gui
// - then, pull out primary and alternate grid from gui
// - then give both the input loop and gui handles to the pulled out grid
//   - the grid handle should expose apis needed for gui and input loop to do their thing
//   - pull out token_stream coming from ChildProcess out of gui
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
    let grids = Arc::new(RwLock::new(GridStack::new(24, 80)));
    let child_process = terminal_input::ChildProcess::spawn(cc.egui_ctx.clone(), grids.clone());
    Box::new(
        TerminalEmulator::new(cc, child_process, grids)
            .expect("TerminalEmulator initialization failed"),
    )
}
