use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub regular_font: String,
    pub bold_font: String,
    pub bold_italic_font: String,
    pub italic_font: String,
    pub font_size: f32,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            regular_font: "Hack".to_string(),
            bold_font: "Hack".to_string(),
            bold_italic_font: "Hack".to_string(),
            italic_font: "Hack".to_string(),
            font_size: 24.0,
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
        Err(err) => {
            log::warn!("config serialization failed: {err}");
        }
    }
}
