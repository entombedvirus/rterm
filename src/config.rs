use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub regular_font: egui::FontFamily,
    pub font_size: f32,
    pub pixels_per_point: f32,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            regular_font: egui::FontFamily::Monospace,
            font_size: 24.0,
            pixels_per_point: 1.0,
        }
    }
}

pub fn get(ctx: &egui::Context) -> Config {
    ctx.data_mut(|d| d.get_persisted(id())).unwrap_or_default()
}

pub fn set(ctx: &egui::Context, config: Config) {
    ctx.data_mut(|d| d.insert_persisted(id(), config));
}

fn id() -> egui::Id {
    egui::Id::new("config")
}
