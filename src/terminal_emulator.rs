use std::{
    collections::VecDeque,
    fmt::Write,
    ops::{Neg, Range},
    os::fd::{AsFd, AsRawFd, OwnedFd},
    sync::{mpsc, Arc},
};

use crate::{
    ansi::{self, AnsiToken, SgrControl},
    config::{self, Config},
    fonts::{FontDesc, FontManager},
    grid, pty, puffin, terminal_input,
};
use ansi::AsciiControl;
use anyhow::Context;
use egui::{text::LayoutJob, CentralPanel, Color32, DragValue, Key, Rect};
use log::info;
use nix::errno::Errno;

#[derive(Debug)]
struct ChildProcess {
    _io_thread: std::thread::JoinHandle<()>,
    pty_fd: Arc<OwnedFd>,
    token_stream: mpsc::Receiver<VecDeque<ansi::AnsiToken>>,
}

impl ChildProcess {
    fn spawn(ctx: egui::Context) -> Self {
        let pty_fd = pty::create_pty().expect("create_pty failed");
        let pty_fd = Arc::new(pty_fd);
        let (tx, rx) = mpsc::channel();

        let io_fd = Arc::clone(&pty_fd);
        let _io_thread = std::thread::Builder::new()
            .name("rterm i/o thread".to_string())
            .spawn(move || {
                terminal_input::input_loop(ctx, io_fd, tx);
            })
            .expect("Failed to spawn input_loop thread");

        Self {
            _io_thread,
            pty_fd,
            token_stream: rx,
        }
    }
}
#[derive(Debug)]
pub struct TerminalEmulator {
    config: Config,
    font_manager: FontManager,
    child_process: Option<ChildProcess>,
    pub char_dimensions: Option<egui::Vec2>,
    buffered_input: String,

    primary_grid: grid::Grid,
    // used for fullscreen apps. does not have scrollback
    alternate_grid: Option<grid::Grid>,

    show_settings: bool,
    settings_state: Option<SettingsState>,

    show_profiler: bool,

    enable_bracketed_paste: bool,
    enable_focus_tracking: bool,
    enable_application_escape: bool,
    enable_debug_render: bool,
}

impl eframe::App for TerminalEmulator {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        puffin::set_scopes_on(self.show_profiler);
        puffin::profile_function!();

        if self.show_profiler {
            ctx.show_viewport_immediate(
                egui::ViewportId::from_hash_of("profiler_ui"),
                egui::ViewportBuilder::default()
                    .with_min_inner_size((1024.0, 768.0))
                    .with_resizable(true)
                    .with_active(true)
                    .with_title("rterm - profiler"),
                |ctx, _window_class| {
                    egui::CentralPanel::default().show(ctx, |ui| {
                        puffin_egui::profiler_ui(ui);
                    });
                    self.show_profiler = !ctx.input_mut(|input| {
                        input.viewport().close_requested()
                            || input.consume_key(egui::Modifiers::COMMAND, Key::W)
                            || input.consume_key(egui::Modifiers::NONE, Key::Escape)
                    });
                },
            );
        }

        if self.show_settings {
            self.settings_state
                .get_or_insert_with(|| SettingsState::new(&self.font_manager))
                .show(
                    ctx,
                    &mut self.config,
                    &mut self.enable_debug_render,
                    &mut self.show_settings,
                    &mut self.show_profiler,
                );
        }

        Self::init_fonts(&self.config, &mut self.font_manager);
        CentralPanel::default()
            .frame(egui::Frame::default().fill(ansi::Color::DefaultBg.into()))
            .show(ctx, |ui| -> anyhow::Result<()> {
                if self.enable_debug_render {
                    ctx.debug_painter()
                        .debug_rect(ui.max_rect(), Color32::YELLOW, "panel");
                }

                let spacing = ui.spacing_mut();
                spacing.item_spacing = egui::Vec2::ZERO;
                spacing.window_margin = egui::Margin::ZERO;

                let char_dims = self
                    .char_dimensions
                    .insert({
                        let regular_font = egui::FontId::new(
                            self.config.font_size,
                            self.font_manager
                                .get_or_init("regular", &self.config.regular_font),
                        );
                        let dims = ctx.fonts(|fonts| {
                            let width = fonts.glyph_width(&regular_font, 'M');
                            let height = fonts.row_height(&regular_font);
                            (width, height).into()
                        });
                        // needed to avoid floating point errors when multiplying
                        ui.painter().round_vec_to_pixels(dims)
                    })
                    .clone();

                let winsz = pty::compute_winsize(
                    ui.painter().round_to_pixel(ui.available_width()),
                    ui.painter().round_to_pixel(ui.available_height()),
                    char_dims.x,
                    char_dims.y,
                );

                let pty_fd = self
                    .child_process
                    .get_or_insert_with(|| ChildProcess::spawn(ctx.clone()))
                    .pty_fd
                    .as_fd();
                let grids = self
                    .alternate_grid
                    .iter_mut()
                    .chain(std::iter::once(&mut self.primary_grid));
                let mut needs_signal = false;
                for grid in grids {
                    needs_signal |= grid.resize(winsz.ws_row as usize, winsz.ws_col as usize);
                }
                if needs_signal {
                    pty::update_pty_window_size(pty_fd, &winsz)
                        .context("update_pty_window_size")?;
                }

                ui.input(|input_state| -> anyhow::Result<()> {
                    for event in &input_state.events {
                        self.handle_event(input_state, event)
                            .context("handling event failed")?;
                    }
                    Ok(())
                })
                .context("input handling failed")?;

                let read_input = self.read_from_pty(ctx).context("reading from pty failed")?;
                self.write_to_pty().context("writing to pty failed")?;

                ui.with_layout(egui::Layout::top_down_justified(egui::Align::TOP), |ui| {
                    let grid = self.alternate_grid.as_ref().unwrap_or(&self.primary_grid);
                    ui.set_height(grid.num_rows() as f32 * char_dims.y);
                    ui.set_width(grid.num_cols() as f32 * char_dims.x);
                    if self.enable_debug_render {
                        ctx.debug_painter()
                            .debug_rect(ui.max_rect(), Color32::GREEN, "layout");
                    }

                    let grid_cols = grid.num_cols();
                    egui::ScrollArea::vertical()
                        .auto_shrink([false, false])
                        .stick_to_bottom(true)
                        .show_rows(
                            ui,
                            char_dims.y,
                            grid.total_rows(),
                            |ui, visible_rows| -> anyhow::Result<()> {
                                if self.enable_debug_render {
                                    ctx.debug_painter().debug_rect(
                                        ui.max_rect(),
                                        Color32::WHITE,
                                        "scroll_area",
                                    );
                                    for r in 0..visible_rows.len() {
                                        for c in 0..grid_cols {
                                            let min = egui::Vec2::new(
                                                c as f32 * char_dims.x,
                                                r as f32 * char_dims.y,
                                            );
                                            let rect = egui::Rect::from_min_size(
                                                ui.max_rect().min + min,
                                                char_dims,
                                            );
                                            ui.painter().rect_stroke(
                                                rect,
                                                0.0,
                                                (1.0, Color32::DARK_GRAY),
                                            );
                                        }
                                    }
                                }
                                self.render(ctx, ui, visible_rows.clone());
                                let cursor_rect = self
                                    .paint_cursor(ui, &visible_rows)
                                    .context("paint_cursor failed")?;
                                if read_input && !ui.clip_rect().contains_rect(cursor_rect) {
                                    ui.scroll_to_rect(cursor_rect, None);
                                }
                                Ok(())
                            },
                        );
                });
                Ok(())
            });
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        config::set(storage, self.config.clone());
    }
}

impl TerminalEmulator {
    pub fn new(cc: &eframe::CreationContext<'_>) -> anyhow::Result<Self> {
        let config = dbg!(config::get(cc.storage));

        // initialize fonts before first frame render
        let mut font_manager = FontManager::new(cc.egui_ctx.clone());
        Self::init_fonts(&config, &mut font_manager);

        let mut primary_grid = grid::Grid::new(24, 80);
        primary_grid.max_scrollback_lines(5000);
        Ok(Self {
            config,
            font_manager,
            primary_grid,
            child_process: None,
            buffered_input: String::new(),
            alternate_grid: None,
            char_dimensions: None,
            enable_debug_render: false,
            show_profiler: false,
            show_settings: false,
            settings_state: None,
            enable_bracketed_paste: false,
            enable_focus_tracking: false,
            enable_application_escape: false,
        })
    }

    fn init_fonts(config: &Config, font_manager: &mut FontManager) {
        puffin::profile_function!();
        for (family_name, font_postscript_name) in [
            ("regular", &config.regular_font),
            ("bold", &config.bold_font),
            ("bold_italic", &config.bold_italic_font),
            ("italic", &config.italic_font),
        ] {
            font_manager.get_or_init(family_name, font_postscript_name);
        }
    }

    pub fn render(
        &mut self,
        _ctx: &egui::Context,
        ui: &mut egui::Ui,
        visible_rows: Range<usize>,
    ) -> Vec<egui::Response> {
        puffin::profile_function!();
        let mut label_responses = Vec::new();
        let grid = self.alternate_grid.as_ref().unwrap_or(&self.primary_grid);

        let font_bold_italic = self
            .font_manager
            .get_or_init("bold_italic", &self.config.bold_italic_font);
        let font_bold = self
            .font_manager
            .get_or_init("bold", &self.config.bold_font);
        let font_italic = self
            .font_manager
            .get_or_init("italic", &self.config.italic_font);
        let font_regular = self
            .font_manager
            .get_or_init("regular", &self.config.regular_font);

        let build_text_format = |format: &SgrState| -> egui::text::TextFormat {
            let font_id = egui::FontId::new(
                self.config.font_size,
                if format.bold && format.italic {
                    font_bold_italic.clone()
                } else if format.bold {
                    font_bold.clone()
                } else if format.italic {
                    font_italic.clone()
                } else {
                    font_regular.clone()
                },
            );
            egui::text::TextFormat {
                font_id,
                color: format.fg_color.into(),
                background: format.bg_color.into(),
                ..Default::default()
            }
        };

        let build_line_layout = |line: grid::DisplayLine| -> LayoutJob {
            puffin::profile_function!("build_line_layout");
            let mut layout = LayoutJob::default();
            layout.break_on_newline = false;
            layout.wrap = egui::text::TextWrapping::no_max_width();
            layout.text = line.padded_text;
            layout.sections = line
                .format_attributes
                .into_iter()
                .map(|attrs| egui::text::LayoutSection {
                    leading_space: 0.0,
                    byte_range: attrs.byte_range,
                    format: build_text_format(attrs.sgr_state),
                })
                .collect();

            //             for format in formats {
            //                 let format = build_text_format(format);
            //                 let byte_range = layout.text.len()..layout.text.len() + ch.len_utf8();
            //                 let section = egui::text::LayoutSection {
            //                     leading_space: 0.0,
            //                     byte_range,
            //                     format,
            //                 };
            //                 layout.sections.push(section);
            //             }

            layout
        };

        {
            puffin::profile_scope!("render_lines");
            for line in grid.display_lines(visible_rows) {
                let layout = build_line_layout(line);
                let galley = ui.fonts(|fonts| {
                    puffin::profile_scope!("galley_construction");
                    fonts.layout_job(layout)
                });
                label_responses.push(ui.label(galley));
            }
        }
        label_responses
    }

    pub fn write_to_pty(&mut self) -> anyhow::Result<()> {
        puffin::profile_function!();
        if self.buffered_input.is_empty() {
            return Ok(());
        }

        let pty_fd = self
            .child_process
            .as_ref()
            .map(|child| child.pty_fd.as_raw_fd())
            .context("child process not spawned yet")?;

        let mut buf = self.buffered_input.as_bytes();
        while !buf.is_empty() {
            match nix::unistd::write(pty_fd, buf) {
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

    pub fn handle_event(
        &mut self,
        input_state: &egui::InputState,
        event: &egui::Event,
    ) -> anyhow::Result<()> {
        puffin::profile_function!();
        match event {
            egui::Event::WindowFocused(is_focused) => {
                if self.enable_focus_tracking {
                    self.buffered_input.push_str(match is_focused {
                        true => "\x1b[I",
                        false => "\x1b[O",
                    })
                }
            }
            egui::Event::Paste(txt) => {
                if self.enable_bracketed_paste {
                    self.buffered_input.push_str("\x1b[200~");
                }
                self.buffered_input.push_str(&txt);
                if self.enable_bracketed_paste {
                    self.buffered_input.push_str("\x1b[201~");
                }
            }
            egui::Event::Text(txt) => {
                if input_state.modifiers.alt {
                    terminal_input::alt(txt, &mut self.buffered_input);
                } else {
                    self.buffered_input.push_str(&txt);
                };
            }
            egui::Event::Key {
                key: Key::Comma,
                pressed: true,
                repeat: false,
                modifiers,
                ..
            } if modifiers.mac_cmd => {
                self.show_settings = true;
            }
            egui::Event::Key {
                key,
                pressed: true,
                modifiers: egui::Modifiers::CTRL,
                ..
            } => terminal_input::ctrl(key.name(), &mut self.buffered_input),
            egui::Event::Key {
                key: Key::Escape,
                pressed: true,
                ..
            } => {
                if self.enable_application_escape {
                    self.buffered_input.push_str("\x1bO[");
                } else {
                    self.buffered_input.push(AsciiControl::Escape.into());
                }
            }
            egui::Event::Key {
                key: Key::Tab,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::Tab.into()),
            egui::Event::Key {
                key: Key::Backspace,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::Backspace.into()),
            egui::Event::Key {
                key: Key::Enter,
                pressed: true,
                ..
            } => self.buffered_input.push(AsciiControl::LineFeed.into()),
            egui::Event::Key {
                key: Key::ArrowUp,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[A"),
            egui::Event::Key {
                key: Key::ArrowDown,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[B"),
            egui::Event::Key {
                key: Key::ArrowRight,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[C"),
            egui::Event::Key {
                key: Key::ArrowLeft,
                pressed: true,
                ..
            } => self.buffered_input.push_str("\u{1b}[D"),
            egui::Event::Key { pressed: false, .. } => (),
            egui::Event::PointerMoved { .. } => (),
            egui::Event::PointerGone { .. } => (),
            egui::Event::Scroll { .. } => (),
            egui::Event::MouseWheel { .. } => (),
            _ => log::trace!("unhandled event: {event:?}"),
        };
        Ok(())
    }

    pub fn paint_cursor(
        &self,
        ui: &mut egui::Ui,
        visible_rows: &Range<usize>,
    ) -> anyhow::Result<egui::Rect> {
        let grid = self.alternate_grid.as_ref().unwrap_or(&self.primary_grid);
        let (cursor_row_in_screen_space, cursor_col) = grid.cursor_position();
        let cursor_row_in_scrollback_space =
            grid.first_visible_line_no() + cursor_row_in_screen_space;
        let num_rows_from_top = cursor_row_in_scrollback_space.saturating_sub(visible_rows.start);
        let char_dims = self.char_dimensions.unwrap_or(egui::vec2(12.0, 12.0));
        let cursor_rect = Rect::from_min_size(
            egui::pos2(
                cursor_col as f32 * char_dims.x,
                num_rows_from_top as f32 * char_dims.y,
            ) + ui.max_rect().min.to_vec2(),
            char_dims,
        );
        ui.painter().rect_filled(cursor_rect, 0.0, Color32::GOLD);
        Ok(cursor_rect)
    }

    fn handle_ansi_token(&mut self, ctx: &egui::Context, token: AnsiToken) {
        match token {
            AnsiToken::OSC(osc_ctrl) => self.handle_osc_token(ctx, osc_ctrl),
            AnsiToken::ModeControl(ansi::ModeControl::BracketedPasteEnter) => {
                self.enable_bracketed_paste = true
            }
            AnsiToken::ModeControl(ansi::ModeControl::BracketedPasteExit) => {
                self.enable_bracketed_paste = false
            }
            AnsiToken::ModeControl(ansi::ModeControl::AlternateScreenEnter) => {
                self.enter_alternate_screen();
            }
            AnsiToken::ModeControl(ansi::ModeControl::AlternateScreenExit) => {
                self.exit_alternate_screen();
            }
            AnsiToken::ModeControl(ansi::ModeControl::FocusTrackEnter) => {
                self.enable_focus_tracking = true;
            }
            AnsiToken::ModeControl(ansi::ModeControl::FocusTrackExit) => {
                self.enable_focus_tracking = false;
            }
            AnsiToken::ModeControl(ansi::ModeControl::ApplicationEscEnter) => {
                self.enable_application_escape = true;
            }
            AnsiToken::ModeControl(ansi::ModeControl::ApplicationEscExit) => {
                self.enable_application_escape = false;
            }
            AnsiToken::DA(ansi::DeviceAttributes::XtVersion) => {
                const XT_VERSION: &str = "0.0.1";
                let _ = write!(&mut self.buffered_input, "\x1bP>|rterm({XT_VERSION})\x1b\\");
            }
            AnsiToken::DA(ansi::DeviceAttributes::Primary) => {
                // See: https://github.com/kovidgoyal/kitty/blob/5b4ea0052c3db89063a4b4af9f4b78ef90b7332c/kitty/screen.c#L2084
                let _ = write!(&mut self.buffered_input, "\x1b[?62;c");
            }
            AnsiToken::DA(ansi::DeviceAttributes::Secondary) => {
                // See: https://github.com/kovidgoyal/kitty/blob/5b4ea0052c3db89063a4b4af9f4b78ef90b7332c/kitty/screen.c#L2087
                //  We add 4000 to the primary version because vim turns on SGR mouse mode
                //   automatically if this version is high enough
                const PRIMARY_VERSION: &str = "4000";
                const SECONDARY_VERSION: &str = "0";
                let _ = write!(
                    &mut self.buffered_input,
                    "\x1b[>1;{PRIMARY_VERSION};{SECONDARY_VERSION}c"
                );
            }
            _ => {
                puffin::profile_scope!("grid token handling");
                use ansi::CursorControl;
                use ansi::EraseControl;
                let grid = self
                    .alternate_grid
                    .as_mut()
                    .unwrap_or(&mut self.primary_grid);
                match token {
                    AnsiToken::ResetToInitialState => {
                        grid.clear_including_scrollback();
                    }
                    AnsiToken::Text(txt) => {
                        grid.write_text_at_cursor(&txt);
                    }
                    AnsiToken::AsciiControl(AsciiControl::Backspace) => {
                        grid.move_cursor_relative(0, -1);
                    }
                    AnsiToken::AsciiControl(AsciiControl::Tab) => {
                        grid.move_cursor_relative(0, 4);
                    }
                    AnsiToken::AsciiControl(AsciiControl::LineFeed) => {
                        grid.move_cursor_relative(1, 0);
                    }
                    AnsiToken::AsciiControl(AsciiControl::CarriageReturn) => {
                        let (current_row, _) = grid.cursor_position();
                        grid.move_cursor(current_row, 0);
                    }
                    AnsiToken::CursorControl(CursorControl::MoveUp { lines }) => {
                        grid.move_cursor_relative(lines.try_into().unwrap_or(0_isize).neg(), 0);
                    }
                    AnsiToken::CursorControl(CursorControl::MoveDown { lines }) => {
                        grid.move_cursor_relative(lines.try_into().unwrap_or(0_isize), 0);
                    }
                    AnsiToken::CursorControl(CursorControl::MoveLeft { cols }) => {
                        grid.move_cursor_relative(0, cols.try_into().unwrap_or(0_isize).neg());
                    }
                    AnsiToken::CursorControl(CursorControl::MoveRight { cols }) => {
                        grid.move_cursor_relative(0, cols.try_into().unwrap_or(0_isize));
                    }
                    AnsiToken::CursorControl(CursorControl::MoveTo { line, col }) => {
                        grid.move_cursor(line.saturating_sub(1), col.saturating_sub(1));
                    }
                    AnsiToken::CursorControl(CursorControl::ScrollUpFromHome) => {
                        let (cursor_row, _) = grid.cursor_position();
                        if cursor_row > 0 {
                            // no need to scroll
                            grid.move_cursor_relative(-1, 0);
                        } else {
                            grid.move_lines_down(0, 1);
                        }
                    }
                    AnsiToken::CursorControl(CursorControl::SavePositionDEC) => {
                        grid.save_cursor_state();
                    }
                    AnsiToken::CursorControl(CursorControl::RestorePositionDEC) => {
                        grid.restore_cursor_state();
                    }
                    AnsiToken::EraseControl(EraseControl::Screen) => {
                        grid.clear_screen();
                    }
                    AnsiToken::EraseControl(EraseControl::ScreenAndScrollback) => {
                        grid.clear_including_scrollback();
                    }
                    AnsiToken::EraseControl(EraseControl::FromCursorToEndOfScreen) => {
                        grid.erase_from_cursor_to_screen();
                    }
                    AnsiToken::EraseControl(EraseControl::FromCursorToEndOfLine) => {
                        grid.erase_from_cursor_to_eol();
                    }
                    AnsiToken::SGR(params) => {
                        let sgr_state = grid.cursor_format_mut();
                        for sgr in params {
                            match sgr {
                                SgrControl::Bold => {
                                    sgr_state.bold = true;
                                }
                                SgrControl::EnterItalicsMode => {
                                    sgr_state.italic = true;
                                }
                                SgrControl::ExitItalicsMode => {
                                    sgr_state.italic = false;
                                }
                                SgrControl::ForgroundColor(color) => {
                                    sgr_state.fg_color = color;
                                }
                                SgrControl::BackgroundColor(color) => {
                                    sgr_state.bg_color = color;
                                }
                                SgrControl::Reset => {
                                    *sgr_state = SgrState::default();
                                }
                                SgrControl::ResetFgColor => {
                                    sgr_state.fg_color = ansi::Color::DefaultFg;
                                }
                                SgrControl::ResetBgColor => {
                                    sgr_state.bg_color = ansi::Color::DefaultBg;
                                }
                                SgrControl::Unimplemented(_) => {
                                    // noop
                                }
                            }
                        }
                    }
                    ignored => info!("ignoring ansii token: {ignored:?}"),
                }
            }
        }
    }

    fn read_from_pty(&mut self, ctx: &egui::Context) -> anyhow::Result<bool> {
        puffin::profile_function!();

        let mut read_input = false;
        loop {
            let token_stream = self
                .child_process
                .as_ref()
                .map(|child| &child.token_stream)
                .context("child process not spawned yet")?;
            match token_stream.try_recv() {
                Ok(tokens) => {
                    for token in tokens {
                        self.handle_ansi_token(ctx, token);
                    }
                    read_input = true;
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    break;
                }
                Err(mpsc::TryRecvError::Empty) => break,
            }
        }

        Ok(read_input)
    }

    fn handle_osc_token(&mut self, ctx: &egui::Context, osc_ctrl: ansi::OscControl) {
        puffin::profile_function!();
        use ansi::OscControl::*;
        match osc_ctrl {
            GetDefaultFgColor => {
                let fg: egui::Color32 = ansi::Color::DefaultFg.into();
                let _ = write!(
                    &mut self.buffered_input,
                    "\x1b]10;{c}\x1b\\",
                    c = ansi::encode_color32(fg)
                );
            }
            GetDefaultBgColor => {
                let bg: egui::Color32 = ansi::Color::DefaultBg.into();
                let _ = write!(
                    &mut self.buffered_input,
                    "\x1b]11;{c}\x1b\\",
                    c = ansi::encode_color32(bg)
                );
            }
            Reset => (),
            SetWindowTitle(title) => ctx.send_viewport_cmd(egui::ViewportCommand::Title(title)),
            Unknown(seq) => log::warn!("unknown osc sequence: {seq:?}"),
        }
    }

    fn enter_alternate_screen(&mut self) {
        self.alternate_grid.get_or_insert_with(|| {
            self.primary_grid.save_cursor_state();
            let num_rows = self.primary_grid.num_rows();
            let num_cols = self.primary_grid.num_cols();
            grid::Grid::new(num_rows, num_cols)
        });
    }

    fn exit_alternate_screen(&mut self) {
        if let Some(_) = self.alternate_grid.take() {
            self.primary_grid.restore_cursor_state();
        }
    }
}

#[derive(Debug)]
struct SettingsState {
    async_font_search_result: Option<anyhow::Result<Vec<FontDesc>>>,
    async_receiver: mpsc::Receiver<anyhow::Result<Vec<FontDesc>>>,
    should_scroll: bool,
}

impl SettingsState {
    fn new(font_manager: &FontManager) -> Self {
        let async_receiver = font_manager.async_search_for_monospace_fonts();
        let async_font_search_result = None;
        Self {
            async_font_search_result,
            async_receiver,
            should_scroll: true,
        }
    }

    fn get_fonts<'a>(&'a mut self) -> anyhow::Result<Option<&'a [FontDesc]>> {
        if self.async_font_search_result.is_none() {
            self.async_font_search_result = match self.async_receiver.try_recv() {
                Ok(res) => Some(res),
                Err(mpsc::TryRecvError::Empty) => None,
                Err(mpsc::TryRecvError::Disconnected) => Some(Err(anyhow::format_err!(
                    "background thread disconnected without result"
                ))),
            }
        }
        match &self.async_font_search_result {
            Some(Ok(fonts)) => Ok(Some(fonts.as_slice())),
            Some(Err(err)) => Err(anyhow::format_err!("async font search failed: {err}")),
            None => Ok(None),
        }
    }

    fn show(
        &mut self,
        ctx: &egui::Context,
        config: &mut Config,
        enable_debug_render: &mut bool,
        show_settings: &mut bool,
        show_profiler: &mut bool,
    ) {
        ctx.show_viewport_immediate(
            egui::ViewportId::from_hash_of("settings_ui"),
            egui::ViewportBuilder::default()
                .with_min_inner_size((100.0, 200.0))
                .with_resizable(false)
                .with_active(true)
                .with_title("rterm - settings"),
            |ctx, _window_class| {
                egui::CentralPanel::default().show(ctx, |ui| {
                    ui.horizontal(|ui| {
                        ui.label("Font Size:");
                        ui.add(DragValue::new(&mut config.font_size).clamp_range(2.0..=88.0));
                    });
                    ui.checkbox(enable_debug_render, "Enable Debug Renderer");
                    ui.checkbox(show_profiler, "Show Profiler Window");

                    ui.separator();
                    ui.collapsing("Fonts", |ui| {
                        let should_scroll = self.should_scroll;
                        match self.get_fonts() {
                            Ok(Some(fonts)) => {
                                ui.horizontal(|ui| {
                                    Self::font_ui(
                                        ui,
                                        "Regular",
                                        &mut config.regular_font,
                                        fonts,
                                        should_scroll,
                                    )
                                });
                                ui.horizontal(|ui| {
                                    Self::font_ui(
                                        ui,
                                        "Italic",
                                        &mut config.italic_font,
                                        fonts,
                                        should_scroll,
                                    )
                                });
                                ui.horizontal(|ui| {
                                    Self::font_ui(
                                        ui,
                                        "Bold",
                                        &mut config.bold_font,
                                        fonts,
                                        should_scroll,
                                    )
                                });
                                ui.horizontal(|ui| {
                                    Self::font_ui(
                                        ui,
                                        "Bold Italic",
                                        &mut config.bold_italic_font,
                                        fonts,
                                        should_scroll,
                                    )
                                });
                                self.should_scroll = false;
                            }
                            Ok(None) => {
                                ui.spinner();
                            }
                            Err(err) => {
                                ui.label(format!("font search error: {err}"));
                            }
                        }
                    });

                    *show_settings = !ctx.input_mut(|input| {
                        input.viewport().close_requested()
                            || input.consume_key(egui::Modifiers::COMMAND, Key::W)
                            || input.consume_key(egui::Modifiers::NONE, Key::Escape)
                    });
                })
            },
        );
    }

    fn font_ui(
        ui: &mut egui::Ui,
        label_text: &str,
        dest: &mut String,
        fonts: &[FontDesc],
        should_scroll: bool,
    ) {
        ui.columns(2, |cols| {
            cols[0].label(label_text);

            egui::ScrollArea::new([false, true])
                .id_source(label_text)
                .max_height(60.0)
                .show(&mut cols[1], |ui| {
                    ui.vertical(|ui| {
                        for font in fonts {
                            let resp = ui.selectable_value(
                                dest,
                                font.postscript_name.clone(),
                                font.display_name.clone(),
                            );
                            if should_scroll && *dest == font.postscript_name {
                                resp.scroll_to_me(Some(egui::Align::Center));
                            }
                        }
                    });
                });
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SgrState {
    pub fg_color: ansi::Color,
    pub bg_color: ansi::Color,
    pub bold: bool,
    pub italic: bool,
}

impl Default for SgrState {
    fn default() -> Self {
        Self {
            fg_color: ansi::Color::DefaultFg,
            bg_color: ansi::Color::DefaultBg,
            bold: false,
            italic: false,
        }
    }
}
