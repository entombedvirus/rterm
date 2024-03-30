use anyhow::Context;
use bootstrap::Bootstrap;
use egui::Rgba;
use glutin_winit;
use std::num::NonZeroU32;
use terminal_emulator::TerminalEmulator;
use winit::event_loop::{EventLoop, EventLoopWindowTarget};

mod ansi;
mod bootstrap;
mod config;
mod fonts;
mod pty;
mod terminal_emulator;
mod terminal_input;

// fn main() {
//     env_logger::init();

//     let native_options = eframe::NativeOptions::default();
//     eframe::run_native(
//         "rterm - a toy terminal emulator",
//         native_options,
//         Box::new(|cc| {
//             Box::new(TerminalEmulator::new(cc).expect("TerminalEmulator initialization failed"))
//         }),
//     )
//     .unwrap();
// }

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let winit_window_builder = winit::window::WindowBuilder::new()
        .with_resizable(true)
        .with_inner_size(winit::dpi::LogicalSize {
            width: 800.0,
            height: 600.0,
        })
        .with_title("rterm - a toy terminal emulator")
        .with_transparent(true)
        .with_visible(false); // Keep hidden until we've painted something. See https://github.com/emilk/egui/pull/2279

    let mut bootstrap = unsafe { Bootstrap::new(winit_window_builder)? };
    let app = TerminalEmulator::new(bootstrap.egui_ctx()).context("app creation failed")?;
    bootstrap.init_app(app);
    bootstrap.run_event_loop();

    Ok(())
}
