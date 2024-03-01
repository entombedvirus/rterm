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

pub fn get(storage: Option<&dyn eframe::Storage>) -> Config {
    storage
        .and_then(|storage| storage.get_string("config"))
        .and_then(|config_string| match toml::from_str(&config_string) {
            Ok(config) => Some(config),
            Err(err) => {
                log::warn!("config parsing failed: {err}");
                None
            }
        })
        .unwrap_or_default()
}

pub fn set(storage: &mut dyn eframe::Storage, config: Config) {
    match toml::to_string(&config) {
        Ok(config_string) => {
            storage.set_string("config", config_string);
        }
        Err(err) =>  {
            log::warn!("config serialization failed: {err}");
        },
    }
}
