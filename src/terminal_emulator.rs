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

    primary_grid: grid::Grid2,
    // used for fullscreen apps. does not have scrollback
    alternate_grid: Option<grid::Grid2>,

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
                            grid.num_current_display_rows(),
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

        let mut primary_grid = grid::Grid2::new(24, 80);
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

        let build_text_format = |format: &grid::SgrState| -> egui::text::TextFormat {
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

    fn read_from_pty(&mut self, ctx: &egui::Context) -> anyhow::Result<bool> {
        puffin::profile_function!();
        let token_stream = self
            .child_process
            .as_ref()
            .map(|child| &child.token_stream)
            .context("child process not spawned yet")?;
        match token_stream.try_recv() {
            Ok(tokens) => {
                for token in tokens {
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
                            let _ = write!(
                                &mut self.buffered_input,
                                "\x1bP>|rterm({XT_VERSION})\x1b\\"
                            );
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
                                    grid.move_cursor_relative(
                                        lines.try_into().unwrap_or(0_isize).neg(),
                                        0,
                                    );
                                }
                                AnsiToken::CursorControl(CursorControl::MoveDown { lines }) => {
                                    grid.move_cursor_relative(
                                        lines.try_into().unwrap_or(0_isize),
                                        0,
                                    );
                                }
                                AnsiToken::CursorControl(CursorControl::MoveLeft { cols }) => {
                                    grid.move_cursor_relative(
                                        0,
                                        cols.try_into().unwrap_or(0_isize).neg(),
                                    );
                                }
                                AnsiToken::CursorControl(CursorControl::MoveRight { cols }) => {
                                    grid.move_cursor_relative(
                                        0,
                                        cols.try_into().unwrap_or(0_isize),
                                    );
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
                                                *sgr_state = grid::SgrState::default();
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
                // keep requesting painting a new frame as long as we keep getting data. Only
                // processing a subset of data each frame keeps the UI responsive to the user
                // sending Ctrl-C etc
                ctx.request_repaint();
                Ok(true)
            }
            Err(mpsc::TryRecvError::Disconnected) => {
                ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                Ok(false)
            }
            Err(mpsc::TryRecvError::Empty) => Ok(false),
        }
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
            grid::Grid2::new(num_rows, num_cols)
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

#[derive(Debug)]
struct AnsiGrid {
    num_rows: usize,
    num_cols: usize,
    max_rows_scrollback: usize,
    cells: Vec<char>,
    text_format: Vec<SgrState>,

    cursor_state: CursorState,
    saved_cursor_state: Option<CursorState>,
}

#[derive(Debug, Default, Copy, Clone)]
struct CursorState {
    position: (usize, usize), // in the range (0..num_rows, 0..num_cols)
    sgr_state: SgrState,
    pending_wrap: bool,
}

impl AnsiGrid {
    const FILL_CHAR: char = '-';

    fn new(num_rows: usize, num_cols: usize, max_rows_scrollback: usize) -> Self {
        // scrollback has to be at least num_rows
        let max_rows_scrollback = max_rows_scrollback.max(num_rows);
        let cursor_state = CursorState::default();
        Self {
            num_rows,
            num_cols,
            max_rows_scrollback,
            cells: vec![Self::FILL_CHAR; num_rows * num_cols],
            text_format: vec![SgrState::default(); num_rows * num_cols],
            cursor_state,
            saved_cursor_state: None,
        }
    }

    fn resize(&mut self, new_num_rows: usize, new_num_cols: usize) -> bool {
        if self.num_rows == new_num_rows && self.num_cols == new_num_cols {
            return false;
        }
        log::info!(
            "resize: {} x {} -> {new_num_rows} x {new_num_cols}",
            self.num_rows,
            self.num_cols
        );
        let new_len = (self.first_visible_line_no() + new_num_rows) * new_num_cols;
        let mut new_cells = vec![Self::FILL_CHAR; new_len];
        let mut new_format = vec![SgrState::default(); new_len];
        let line_len = self.num_cols.min(new_num_cols);
        for (line_no, (old_cells, old_formats)) in self
            .cells
            .chunks_exact(self.num_cols)
            .zip(self.text_format.chunks_exact(self.num_cols))
            .enumerate()
        {
            let new_line_start = line_no * new_num_cols;
            let copy_range = new_line_start..new_line_start + line_len;
            if let Some(dst) = new_cells.get_mut(copy_range.clone()) {
                dst.copy_from_slice(&old_cells[..line_len]);
            }
            if let Some(dst) = new_format.get_mut(copy_range) {
                dst.copy_from_slice(&old_formats[..line_len]);
            }
        }
        self.cells = new_cells;
        self.text_format = new_format;
        self.num_rows = new_num_rows;
        self.num_cols = new_num_cols;
        true
    }

    fn update(&mut self, token: &ansi::AnsiToken) {
        puffin::profile_function!();
        use ansi::AnsiToken::*;
        use ansi::AsciiControl;
        use ansi::CursorControl;
        use ansi::EraseControl;

        match token {
            ResetToInitialState => {
                self.clear_including_scrollback();
            }
            Text(txt) => {
                puffin::profile_scope!("grid: Text");
                for ch in txt.chars() {
                    // before inserting txt, check if a wrap is pending
                    if self.cursor_state.pending_wrap {
                        // moving will clear the pending wrap flag
                        self.move_cursor_relative(0, 1);
                    }
                    self.update_cursor_cell(ch);
                    // if the current position is the last column of any row,  flip the
                    // pending_wrap flag to true so that that wrap can be delayed. This allows
                    // printing on the very last line of the buffer to the last cell w/o triggering
                    // scrolling.
                    let (_, col) = self.cursor_state.position;
                    if col + 1 == self.num_cols {
                        self.cursor_state.pending_wrap = true;
                    } else {
                        self.move_cursor_relative(0, 1);
                    }
                }
            }
            AsciiControl(AsciiControl::Backspace) => {
                self.move_cursor_relative(0, -1);
            }
            AsciiControl(AsciiControl::Tab) => {
                self.move_cursor_relative(0, 4);
            }
            AsciiControl(AsciiControl::LineFeed) => {
                self.move_cursor_relative(1, 0);
            }
            AsciiControl(AsciiControl::CarriageReturn) => {
                self.move_cursor(self.cursor_state.position.0, 0);
            }
            CursorControl(CursorControl::MoveUp { lines }) => {
                self.move_cursor_relative((*lines).try_into().unwrap_or(0_isize).neg(), 0);
            }
            CursorControl(CursorControl::MoveDown { lines }) => {
                self.move_cursor_relative((*lines).try_into().unwrap_or(0_isize), 0);
            }
            CursorControl(CursorControl::MoveLeft { cols }) => {
                self.move_cursor_relative(0, (*cols).try_into().unwrap_or(0_isize).neg());
            }
            CursorControl(CursorControl::MoveRight { cols }) => {
                self.move_cursor_relative(0, (*cols).try_into().unwrap_or(0_isize));
            }
            CursorControl(CursorControl::MoveTo { line, col }) => {
                self.move_cursor(line.saturating_sub(1), col.saturating_sub(1));
            }
            CursorControl(CursorControl::ScrollUpFromHome) => {
                let (cursor_row, _) = self.cursor_state.position;
                if cursor_row > 0 {
                    // no need to scroll
                    self.move_cursor_relative(-1, 0);
                } else {
                    let home_row_start = self.cursor_position_to_buf_pos(&(0, 0));
                    let next_row_start = home_row_start + self.num_cols;
                    let last_row_start = self.cells.len() - self.num_cols;
                    self.cells
                        .copy_within(home_row_start..last_row_start, next_row_start);
                    self.text_format
                        .copy_within(home_row_start..last_row_start, next_row_start);
                    // clear the new row created on top
                    self.cells[home_row_start..next_row_start].fill(Self::FILL_CHAR);
                    self.text_format[home_row_start..next_row_start].fill(SgrState::default());
                }
            }
            CursorControl(CursorControl::SavePositionDEC) => {
                self.save_cursor_state();
            }
            CursorControl(CursorControl::RestorePositionDEC) => {
                self.restore_cursor_state();
            }
            EraseControl(EraseControl::Screen) => {
                self.clear_screen();
            }
            EraseControl(EraseControl::ScreenAndScrollback) => {
                self.clear_including_scrollback();
            }
            EraseControl(EraseControl::FromCursorToEndOfScreen) => {
                let cur_idx = self.cursor_position_to_buf_pos(&self.cursor_state.position);
                let visible_end =
                    self.cursor_position_to_buf_pos(&(self.num_rows - 1, self.num_cols - 1));
                self.cells[cur_idx..visible_end].fill(Self::FILL_CHAR);
                self.text_format[cur_idx..visible_end].fill(SgrState::default());
            }
            EraseControl(EraseControl::FromCursorToEndOfLine) => {
                let cur_idx = self.cursor_position_to_buf_pos(&self.cursor_state.position);
                let line_end_idx =
                    self.cursor_position_to_buf_pos(&(self.cursor_state.position.0 + 1, 0));
                self.cells[cur_idx..line_end_idx].fill(Self::FILL_CHAR);
                self.text_format[cur_idx..line_end_idx].fill(SgrState::default());
            }
            SGR(params) => {
                for sgr in params {
                    match sgr {
                        SgrControl::Bold => {
                            self.cursor_state.sgr_state.bold = true;
                        }
                        SgrControl::EnterItalicsMode => {
                            self.cursor_state.sgr_state.italic = true;
                        }
                        SgrControl::ExitItalicsMode => {
                            self.cursor_state.sgr_state.italic = false;
                        }
                        SgrControl::ForgroundColor(color) => {
                            self.cursor_state.sgr_state.fg_color = *color;
                        }
                        SgrControl::BackgroundColor(color) => {
                            self.cursor_state.sgr_state.bg_color = *color;
                        }
                        SgrControl::Reset => {
                            self.cursor_state.sgr_state = SgrState::default();
                        }
                        SgrControl::ResetFgColor => {
                            self.cursor_state.sgr_state.fg_color = ansi::Color::DefaultFg;
                        }
                        SgrControl::ResetBgColor => {
                            self.cursor_state.sgr_state.bg_color = ansi::Color::DefaultBg;
                        }
                        SgrControl::Unimplemented(_) => {
                            // noop
                        }
                    }
                }
            }
            ignored => info!("ignoring ansii token: {ignored:?}"),
        };
    }

    fn cursor_position_to_buf_pos(&self, (r, c): &(usize, usize)) -> usize {
        let start = self
            .cells
            .len()
            .saturating_sub(self.num_rows * self.num_cols);
        let offset = r * self.num_cols + c;
        start + offset
    }

    fn move_cursor(&mut self, new_row: usize, new_col: usize) {
        puffin::profile_function!();
        assert!(new_col < self.num_cols);
        self.cursor_state.pending_wrap = false;

        // new position is within bounds
        if new_row < self.num_rows {
            self.cursor_state.position = (new_row, new_col);
            return;
        }

        // if we can't grow the buffer, create room at the end by shifting everything up
        let num_new_lines = new_row - self.num_rows + 1;
        if self.cells.len() >= self.max_rows_scrollback * self.num_cols {
            puffin::profile_scope!("shifting buffer");
            let _ = self.cells.drain(0..num_new_lines * self.num_cols);
            let _ = self.text_format.drain(0..num_new_lines * self.num_cols);
        }

        // we can grow to accomodate
        puffin::profile_scope!("growing buffer");
        let new_len = self.cells.len() + num_new_lines * self.num_cols;
        self.cells.resize(new_len, Self::FILL_CHAR);
        self.text_format.resize(new_len, SgrState::default());
        self.cursor_state.position = (self.num_rows - 1, new_col);
    }

    fn move_cursor_relative(&mut self, dr: isize, dc: isize) {
        let (r, c) = self.cursor_state.position;
        let mut new_row = ((r as isize) + dr) as usize;
        let mut new_col = ((c as isize) + dc) as usize;
        new_row += new_col / self.num_cols;
        new_col %= self.num_cols;
        self.move_cursor(new_row, new_col)
    }

    fn update_cursor_cell(&mut self, new_ch: char) {
        let cur_idx = self.cursor_position_to_buf_pos(&self.cursor_state.position);
        self.cells[cur_idx] = new_ch;
        self.text_format[cur_idx] = self.cursor_state.sgr_state;
    }

    fn lines<'a>(
        &'a self,
        visible_rows: Range<usize>,
    ) -> impl Iterator<Item = (&'a [char], &'a [SgrState])> + 'a {
        puffin::profile_function!();
        // let start = self.cursor_position_to_buf_pos(&(0, 0));
        // let end = start + (self.num_rows * self.num_cols);
        self.cells
            .chunks_exact(self.num_cols)
            .zip(self.text_format.chunks_exact(self.num_cols))
            .skip(visible_rows.start)
            .take(visible_rows.len())
    }

    fn num_current_rows(&self) -> usize {
        self.cells.len() / self.num_cols
        // self.num_rows
    }

    fn first_visible_line_no(&self) -> usize {
        self.cursor_position_to_buf_pos(&(0, 0)) / self.num_cols
    }

    fn clear_screen(&mut self) {
        let visible_start = self.cursor_position_to_buf_pos(&(0, 0));
        let visible_end = self.cursor_position_to_buf_pos(&(self.num_rows - 1, self.num_cols - 1));
        self.cells[visible_start..visible_end].fill(Self::FILL_CHAR);
        self.text_format[visible_start..visible_end].fill(SgrState::default());
    }

    fn clear_including_scrollback(&mut self) {
        *self = Self::new(self.num_rows, self.num_cols, self.max_rows_scrollback);
    }

    fn save_cursor_state(&mut self) {
        self.saved_cursor_state = Some(self.cursor_state);
    }

    fn restore_cursor_state(&mut self) {
        if let Some(saved_state) = self.saved_cursor_state.take() {
            self.cursor_state = saved_state;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SgrState {
    fg_color: ansi::Color,
    bg_color: ansi::Color,
    bold: bool,
    italic: bool,
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
