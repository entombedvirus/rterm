use std::{
    fmt::Write as _,
    ops::Range,
    os::fd::{AsFd, AsRawFd},
    sync::{mpsc, Arc},
    time::{self, Duration},
};

use crate::{
    ansi::{self, AnsiToken, DeviceStatusReport},
    buffer::Buffer,
    config::{self, Config},
    fonts::{FontDesc, FontManager},
    grid::{self, Grid, GridStack},
    pty, puffin,
    terminal_input::{self, ChildProcess},
};
use ansi::AsciiControl;
use anyhow::Context;
use egui::{text::LayoutJob, CentralPanel, Color32, DragValue, Key, Rect};
use nix::errno::Errno;

#[derive(Debug, PartialEq, Eq)]
enum ScrollTarget {
    Cursor,
    Line(u32),
}

pub struct TerminalEmulator {
    config: Config,
    font_manager: FontManager,
    child_process: ChildProcess,
    buffered_input: String,

    previous_winsz: Option<libc::winsize>,
    grids: Arc<Buffer<GridStack>>,

    show_settings: bool,
    settings_state: Option<SettingsState>,
    show_profiler: bool,

    enable_bracketed_paste: bool,
    enable_focus_tracking: bool,
    enable_application_escape: bool,
    enable_debug_render: bool,

    debounce_resize_signal: Option<time::Instant>,
    scroll_to: Option<ScrollTarget>,
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

        CentralPanel::default()
            .frame(egui::Frame::default().fill(ansi::Color::DefaultBg.into()))
            .show(ctx, |ui| -> anyhow::Result<()> {
                let spacing = ui.spacing_mut();
                spacing.item_spacing = egui::Vec2::ZERO;
                spacing.window_margin = egui::Margin::ZERO;

                if self.enable_debug_render {
                    ctx.debug_painter()
                        .debug_rect(ui.max_rect(), Color32::YELLOW, "panel");
                }

                let fonts = FontSpec::new(&self.config, &mut self.font_manager);
                let char_dims = self.compute_char_dims(&ui, &fonts);

                let winsz = pty::compute_winsize(
                    ui.painter().round_to_pixel(ui.available_width()),
                    ui.painter().round_to_pixel(ui.available_height()),
                    char_dims.x,
                    char_dims.y,
                );
                if self.previous_winsz != Some(winsz) {
                    self.previous_winsz = Some(winsz);
                    self.grids
                        .write(|grids| grids.resize(winsz.ws_row as u32, winsz.ws_col as u32));
                    let delay = Duration::from_millis(100);
                    self.debounce_resize_signal = time::Instant::now().checked_add(delay);
                    ctx.request_repaint_after(delay);
                } else if let Some(t) = self.debounce_resize_signal {
                    let now = time::Instant::now();
                    if now >= t {
                        log::info!("sending resize signal to pty");
                        let pty_fd = self.child_process.pty_fd.as_fd();
                        pty::update_pty_window_size(pty_fd, &winsz)
                            .context("update_pty_window_size")?;
                        self.debounce_resize_signal = None;
                    } else {
                        let delay = t.duration_since(now);
                        ctx.request_repaint_after(delay)
                    }
                }

                ui.input(|input_state| -> anyhow::Result<()> {
                    for event in &input_state.events {
                        self.handle_event(input_state, event)
                            .context("handling event failed")?;
                    }
                    Ok(())
                })
                .context("input handling failed")?;

                self.read_from_pty(ctx).context("reading from pty failed")?;
                self.write_to_pty().context("writing to pty failed")?;
                self.render_grid(ctx, ui, &fonts, char_dims);
                Ok(())
            });
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        config::set(storage, self.config.clone());
    }
}

fn render_debug_cell_outlines(
    ctx: &egui::Context,
    ui: &mut egui::Ui,
    num_cols: u32,
    char_dims: egui::Vec2,
    visible_rows: Range<usize>,
) {
    ctx.debug_painter()
        .debug_rect(ui.max_rect(), Color32::WHITE, "scroll_area");
    for r in 0..visible_rows.len() {
        for c in 0..num_cols {
            let min = egui::Vec2::new(c as f32 * char_dims.x, r as f32 * char_dims.y);
            let rect = egui::Rect::from_min_size(ui.max_rect().min + min, char_dims);
            ui.painter()
                .rect_stroke(rect, 0.0, (1.0, Color32::DARK_GRAY));
        }
    }
}

#[derive(Debug)]
pub struct FontSpec {
    regular: egui::FontFamily,
    italic: egui::FontFamily,
    bold: egui::FontFamily,
    bold_italic: egui::FontFamily,
}

impl FontSpec {
    fn new(config: &Config, font_manager: &mut FontManager) -> Self {
        puffin::profile_function!();
        let regular = font_manager.get_or_init("regular", &config.regular_font);
        let bold = font_manager.get_or_init("bold", &config.bold_font);
        let bold_italic = font_manager.get_or_init("bold_italic", &config.bold_italic_font);
        let italic = font_manager.get_or_init("italic", &config.italic_font);
        Self {
            regular,
            italic,
            bold,
            bold_italic,
        }
    }
}

impl TerminalEmulator {
    pub fn new(
        cc: &eframe::CreationContext<'_>,
        child_process: ChildProcess,
        grids: Arc<Buffer<GridStack>>,
    ) -> anyhow::Result<Self> {
        let config = dbg!(config::get(cc.storage));

        // initialize fonts before first frame render
        let mut font_manager = FontManager::new(cc.egui_ctx.clone());
        let _ = FontSpec::new(&config, &mut font_manager);

        Ok(Self {
            config,
            font_manager,
            child_process,
            grids,
            buffered_input: String::new(),
            enable_debug_render: false,
            show_profiler: false,
            show_settings: false,
            settings_state: None,
            enable_bracketed_paste: false,
            enable_focus_tracking: false,
            enable_application_escape: false,
            debounce_resize_signal: None,
            scroll_to: None,
            previous_winsz: None,
        })
    }

    pub fn render_grid(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        fonts: &FontSpec,
        char_dims: egui::Vec2,
    ) {
        puffin::profile_function!();

        let scroll_to = self.scroll_to.take();
        let grids = self.grids.read();
        let grid = grids.current();

        ui.with_layout(egui::Layout::top_down_justified(egui::Align::TOP), |ui| {
            ui.set_height(grid.num_rows() as f32 * char_dims.y);
            ui.set_width(grid.num_cols() as f32 * char_dims.x);
            if self.enable_debug_render {
                ctx.debug_painter()
                    .debug_rect(ui.max_rect(), Color32::GREEN, "layout");
            }

            let add_contents =
                |ui: &mut egui::Ui, visible_rows: Range<usize>| -> anyhow::Result<()> {
                    if self.enable_debug_render {
                        render_debug_cell_outlines(
                            ctx,
                            ui,
                            grid.num_cols(),
                            char_dims,
                            visible_rows.clone(),
                        );
                    }

                    {
                        puffin::profile_scope!("render_lines");
                        let start_row = visible_rows.start as u32;
                        let end_row = (visible_rows.end as u32).min(grid.total_rows());
                        let visible_rows = start_row..end_row;

                        for line in grid.display_lines(visible_rows.clone()) {
                            let layout = self.build_line_layout(&fonts, line);
                            let galley = ui.fonts(|fonts| {
                                puffin::profile_scope!("galley_construction");
                                fonts.layout_job(layout)
                            });
                            ui.label(galley);
                        }
                    }

                    let cursor_rect = self
                        .paint_cursor(grid, char_dims, ui, &visible_rows)
                        .context("paint_cursor failed")?;
                    match scroll_to {
                        Some(ScrollTarget::Cursor) => {
                            if !ui.clip_rect().contains_rect(cursor_rect) {
                                ui.scroll_to_rect(cursor_rect, None);
                            }
                        }
                        Some(ScrollTarget::Line(_)) => {
                            // already handled in scroll area's vertical_scroll_offset
                        }
                        None => (),
                    }
                    Ok(())
                };

            let mut scroll_area = egui::ScrollArea::vertical()
                .auto_shrink([false, false])
                .stick_to_bottom(true);

            if let Some(ScrollTarget::Line(target_line_idx)) = scroll_to {
                let row_height = char_dims.y + ui.spacing().item_spacing.y;
                let offset = row_height * target_line_idx as f32;
                scroll_area = scroll_area.vertical_scroll_offset(offset);
            }

            // adjust the total number of rows so that the ScrollArea will place the
            // home row at the top of the screen.
            let total_rows = grid
                .total_rows()
                .max(grid.first_visible_line_no() + grid.num_rows());
            scroll_area.show_rows(ui, char_dims.y, total_rows as usize, add_contents);
        });
    }

    pub fn write_to_pty(&mut self) -> anyhow::Result<()> {
        puffin::profile_function!();
        if self.buffered_input.is_empty() {
            return Ok(());
        }

        let pty_fd = self.child_process.pty_fd.as_raw_fd();
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
            egui::Event::Key {
                key: Key::F1,
                pressed: false,
                modifiers,
                ..
            } => {
                let grid_contents = self.grids.read().current().text_contents();
                if modifiers.alt {
                    log::info!(
                        "\n=====================\n{grid_contents:?}\n=====================\n"
                    );
                } else {
                    log::info!("\n=====================\n{grid_contents}\n=====================\n");
                }
            }
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
        grid: &Grid,
        char_dims: egui::Vec2,
        ui: &mut egui::Ui,
        visible_rows: &Range<usize>,
    ) -> anyhow::Result<egui::Rect> {
        let (cursor_row_in_screen_space, cursor_col) = grid.cursor_position_for_display();
        let cursor_row_in_scrollback_space =
            grid.first_visible_line_no() + cursor_row_in_screen_space;
        let num_rows_from_top =
            cursor_row_in_scrollback_space.saturating_sub(visible_rows.start as u32);
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

    fn build_line_layout(&self, fonts: &FontSpec, line: grid::DisplayLine) -> LayoutJob {
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
                format: self.build_text_format(fonts, &attrs.sgr_state),
            })
            .collect();

        layout
    }

    fn build_text_format(&self, fonts: &FontSpec, format: &SgrState) -> egui::text::TextFormat {
        let font_id = egui::FontId::new(
            self.config.font_size,
            if format.bold && format.italic {
                fonts.bold_italic.clone()
            } else if format.bold {
                fonts.bold.clone()
            } else if format.italic {
                fonts.italic.clone()
            } else {
                fonts.regular.clone()
            },
        );
        egui::text::TextFormat {
            font_id,
            color: format.fg_color.into(),
            background: format.bg_color.into(),
            ..Default::default()
        }
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
            AnsiToken::DSR(DeviceStatusReport::StatusReport) => {
                let _ = write!(&mut self.buffered_input, "\x1b[0n");
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
            AnsiToken::EraseControl(ansi::EraseControl::Screen) => {
                let home_row_line_idx = self.grids.read().current().first_visible_line_no();
                self.scroll_to = Some(ScrollTarget::Line(home_row_line_idx));
            }
            unknown => {
                log::warn!("gui: ignoring unknown ansi token: {unknown:?}");
            }
        }
    }

    fn read_from_pty(&mut self, ctx: &egui::Context) -> anyhow::Result<()> {
        puffin::profile_function!();

        loop {
            let token_stream = &self.child_process.token_stream;
            match token_stream.try_recv() {
                Ok(tokens) => {
                    for token in tokens {
                        self.handle_ansi_token(ctx, token);
                    }
                    // if none of the token handling has specified a specific scroll target,
                    // default to scrolling the cursor into view.
                    self.scroll_to.get_or_insert(ScrollTarget::Cursor);
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    break;
                }
                Err(mpsc::TryRecvError::Empty) => break,
            }
        }
        Ok(())
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

    fn compute_char_dims(&self, ui: &egui::Ui, fonts: &FontSpec) -> egui::Vec2 {
        let regular_font = egui::FontId::new(self.config.font_size, fonts.regular.clone());
        let dims = ui.ctx().fonts(|fonts| {
            let width = fonts.glyph_width(&regular_font, 'M');
            let height = fonts.row_height(&regular_font);
            (width, height).into()
        });
        // needed to avoid floating point errors when multiplying
        ui.painter().round_vec_to_pixels(dims)
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
