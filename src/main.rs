use anyhow::Context;
use bootstrap::Bootstrap;
use terminal_emulator::TerminalEmulator;

mod ansi;
mod bootstrap;
mod config;
mod fonts;
mod keyboard_handler;
mod pty;
mod terminal_emulator;
mod terminal_input;

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

    let project_dirs = directories_next::ProjectDirs::from("com", "example", "rterm")
        .context("app directory init failed")?;

    let mut bootstrap = unsafe { Bootstrap::new(project_dirs.clone(), winit_window_builder)? };
    let app = TerminalEmulator::new(bootstrap.egui_ctx(), &project_dirs)
        .context("app creation failed")?;

    bootstrap.run_event_loop(app);

    Ok(())
}
