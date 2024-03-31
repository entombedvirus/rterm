use anyhow::Context;
use directories_next::ProjectDirs;
use glutin::context::NotCurrentGlContext;
use glutin::display::GetGlDisplay;
use glutin::display::GlDisplay;
use glutin::prelude::GlSurface;
use glutin_winit;
use raw_window_handle::HasRawWindowHandle;
use std::ops::Deref;
use std::{num::NonZeroU32, sync::Arc};
use winit::event_loop::EventLoop;

#[derive(Debug)]
pub enum UserEvent {
    Redraw(std::time::Duration),
}

pub struct Bootstrap {
    project_dirs: ProjectDirs,

    event_loop: Option<EventLoop<UserEvent>>,
    window: winit::window::Window,
    glow_ctx: Arc<glow::Context>,
    gl_context: glutin::context::PossiblyCurrentContext,
    gl_display: glutin::display::Display,
    gl_surface: glutin::surface::Surface<glutin::surface::WindowSurface>,

    egui_glow: egui_glow::EguiGlow,
}

impl Bootstrap {
    pub unsafe fn new(
        project_dirs: ProjectDirs,
        winit_window_builder: winit::window::WindowBuilder,
    ) -> anyhow::Result<Self> {
        let event_loop: EventLoop<UserEvent> =
            winit::event_loop::EventLoopBuilder::with_user_event()
                .build()
                .context("winit event_loop creation failed")?;
        let window_target = event_loop.deref();

        let config_template_builder = glutin::config::ConfigTemplateBuilder::new()
            .prefer_hardware_accelerated(None)
            .with_depth_size(0)
            .with_stencil_size(0)
            .with_transparency(false);

        log::debug!("trying to get gl_config");
        let (mut window, gl_config) =
            glutin_winit::DisplayBuilder::new() // let glutin-winit helper crate handle the complex parts of opengl context creation
                .with_preference(glutin_winit::ApiPreference::FallbackEgl) // https://github.com/emilk/egui/issues/2520#issuecomment-1367841150
                .with_window_builder(Some(winit_window_builder.clone()))
                .build(
                    window_target,
                    config_template_builder,
                    |mut config_iterator| {
                        config_iterator.next().expect(
                            "failed to find a matching configuration for creating glutin config",
                        )
                    },
                )
                .expect("failed to create gl_config");
        let gl_display = gl_config.display();
        log::debug!("found gl_config: {:?}", &gl_config);

        let raw_window_handle = window.as_ref().map(|w| w.raw_window_handle());
        log::debug!("raw window handle: {:?}", raw_window_handle);
        let context_attributes =
            glutin::context::ContextAttributesBuilder::new().build(raw_window_handle);
        // by default, glutin will try to create a core opengl context. but, if it is not available, try to create a gl-es context using this fallback attributes
        let fallback_context_attributes = glutin::context::ContextAttributesBuilder::new()
            .with_context_api(glutin::context::ContextApi::Gles(None))
            .build(raw_window_handle);
        let not_current_gl_context = unsafe {
            gl_display
                    .create_context(&gl_config, &context_attributes)
                    .unwrap_or_else(|_| {
                        log::debug!("failed to create gl_context with attributes: {:?}. retrying with fallback context attributes: {:?}",
                            &context_attributes,
                            &fallback_context_attributes);
                        gl_config
                            .display()
                            .create_context(&gl_config, &fallback_context_attributes)
                            .expect("failed to create context even with fallback attributes")
                    })
        };

        // this is where the window is created, if it has not been created while searching for suitable gl_config
        let window = window.take().unwrap_or_else(|| {
            log::debug!("window doesn't exist yet. creating one now with finalize_window");
            glutin_winit::finalize_window(window_target, winit_window_builder.clone(), &gl_config)
                .expect("failed to finalize glutin window")
        });
        let (width, height): (u32, u32) = window.inner_size().into();
        let width = NonZeroU32::new(width).unwrap_or(NonZeroU32::MIN);
        let height = NonZeroU32::new(height).unwrap_or(NonZeroU32::MIN);
        let surface_attributes =
            glutin::surface::SurfaceAttributesBuilder::<glutin::surface::WindowSurface>::new()
                .build(window.raw_window_handle(), width, height);
        log::debug!(
            "creating surface with attributes: {:?}",
            &surface_attributes
        );
        let gl_surface = unsafe {
            gl_display
                .create_window_surface(&gl_config, &surface_attributes)
                .unwrap()
        };
        log::debug!("surface created successfully: {gl_surface:?}.making context current");
        let gl_context = not_current_gl_context.make_current(&gl_surface).unwrap();

        gl_surface
            .set_swap_interval(
                &gl_context,
                glutin::surface::SwapInterval::Wait(NonZeroU32::MIN),
            )
            .unwrap();

        let glow_ctx = Arc::new(glow::Context::from_loader_function(|s| {
            let s = std::ffi::CString::new(s)
                .expect("failed to construct C string from string for gl proc address");
            gl_display.get_proc_address(&s)
        }));

        let egui_glow = egui_glow::EguiGlow::new(&event_loop, glow_ctx.clone(), None, None);
        let event_loop_proxy = egui::mutex::Mutex::new(event_loop.create_proxy());
        egui_glow
            .egui_ctx
            .set_request_repaint_callback(move |info| {
                event_loop_proxy
                    .lock()
                    .send_event(UserEvent::Redraw(info.delay))
                    .expect("Cannot send event");
            });

        if let Ok(memory) = read_persisted_egui_memory(&project_dirs) {
            egui_glow.egui_ctx.memory_mut(|slot| *slot = memory);
        }

        Ok(Self {
            project_dirs,
            event_loop: Some(event_loop),
            window,
            glow_ctx,
            gl_context,
            gl_display,
            gl_surface,
            egui_glow,
        })
    }

    pub fn egui_ctx(&self) -> &egui::Context {
        &self.egui_glow.egui_ctx
    }

    pub fn resize(&self, physical_size: winit::dpi::PhysicalSize<u32>) {
        self.gl_surface.resize(
            &self.gl_context,
            physical_size.width.try_into().unwrap(),
            physical_size.height.try_into().unwrap(),
        );
    }

    pub fn swap_buffers(&self) -> glutin::error::Result<()> {
        self.gl_surface.swap_buffers(&self.gl_context)
    }

    pub fn run_event_loop(&mut self, mut app: impl App) {
        let event_loop = self
            .event_loop
            .take()
            .expect("run_event_loop can only called once");

        let mut repaint_delay = std::time::Duration::MAX;
        let _ = event_loop.run(move |event, event_loop_window_target| {
            let mut redraw = || {
                self.egui_glow.run(&self.window, |egui_ctx| {
                    app.on_each_frame(egui_ctx);
                });

                event_loop_window_target.set_control_flow(if repaint_delay.is_zero() {
                    self.window.request_redraw();
                    winit::event_loop::ControlFlow::Poll
                } else if let Some(repaint_after_instant) =
                    std::time::Instant::now().checked_add(repaint_delay)
                {
                    winit::event_loop::ControlFlow::WaitUntil(repaint_after_instant)
                } else {
                    winit::event_loop::ControlFlow::Wait
                });

                {
                    unsafe {
                        use glow::HasContext as _;
                        let color: egui::Rgba = app.clear_color().into();
                        self.glow_ctx
                            .clear_color(color[0], color[1], color[2], color[3]);
                        self.glow_ctx.clear(glow::COLOR_BUFFER_BIT);
                    }

                    // draw things behind egui here

                    self.egui_glow.paint(&self.window);

                    // draw things on top of egui here

                    self.swap_buffers().unwrap();
                    self.window.set_visible(true);
                }
            };

            match event {
                winit::event::Event::WindowEvent { event, .. } => {
                    use winit::event::WindowEvent;
                    if matches!(event, WindowEvent::CloseRequested | WindowEvent::Destroyed) {
                        event_loop_window_target.exit();
                        return;
                    }

                    if matches!(event, WindowEvent::RedrawRequested) {
                        redraw();
                        return;
                    }

                    if let winit::event::WindowEvent::Resized(physical_size) = &event {
                        self.resize(*physical_size);
                    }

                    let should_repaint = if let WindowEvent::KeyboardInput {
                        event: key_event,
                        ..
                    } = event
                    {
                        // skip egui_glow and directly go to app for keybaord events since
                        // egui_glow uses egui_winit internally and that prevent all keyboard
                        // events from reaching the app
                        app.on_keyboard_event(key_event)
                    } else {
                        let response = self.egui_glow.on_window_event(&self.window, &event);
                        response.repaint
                    };

                    if should_repaint {
                        self.window.request_redraw();
                    }
                }

                winit::event::Event::UserEvent(UserEvent::Redraw(delay)) => {
                    repaint_delay = delay;
                }
                winit::event::Event::LoopExiting => {
                    if let Err(err) =
                        write_persisted_egui_memory(&self.project_dirs, &self.egui_glow.egui_ctx)
                    {
                        log::warn!("failed to persist applicate state: {err}");
                    }
                    if let Err(err) = app.on_exit(&self.project_dirs) {
                        log::warn!("failed to persist applicate config: {err}");
                    }
                    self.egui_glow.destroy();
                }
                winit::event::Event::NewEvents(winit::event::StartCause::ResumeTimeReached {
                    ..
                }) => {
                    self.window.request_redraw();
                }

                _ => (),
            }
        });
    }
}

fn read_persisted_egui_memory(project_dirs: &ProjectDirs) -> anyhow::Result<egui::Memory> {
    let data_path = project_dirs.data_dir().join("egui_state.toml");
    let contents = std::fs::read_to_string(data_path)?;
    ron::from_str(&contents).map_err(|err| anyhow::format_err!("toml deserialize failed: {err}"))
}

fn write_persisted_egui_memory(
    project_dirs: &ProjectDirs,
    ctx: &egui::Context,
) -> anyhow::Result<()> {
    std::fs::create_dir_all(project_dirs.data_dir())
        .context("failed to crate applicate directory")?;
    let data_path = project_dirs.data_dir().join("egui_state.toml");
    let state = ctx.memory(|mem| mem.clone());
    let serialized_state = ron::to_string(&state).context("ron serialization failed")?;
    std::fs::write(data_path, serialized_state)
        .map_err(|err| anyhow::format_err!("writing egui state failed: {err}"))
}

pub trait App {
    fn clear_color(&self) -> egui::Color32;
    fn on_each_frame(&mut self, ctx: &egui::Context);
    fn on_keyboard_event(&mut self, key_event: winit::event::KeyEvent) -> bool;
    fn on_exit(&mut self, project_dirs: &ProjectDirs) -> anyhow::Result<()>;
}
