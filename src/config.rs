use std::path::Path;

use anyhow::Context;
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

pub fn get<P: AsRef<Path>>(storage: Option<P>) -> Config {
    storage
        .and_then(|storage_path| std::fs::read_to_string(storage_path).ok())
        .and_then(|config_string| match toml::from_str(&config_string) {
            Ok(config) => Some(config),
            Err(err) => {
                log::warn!("config parsing failed: {err}");
                None
            }
        })
        .unwrap_or_default()
}

pub fn set<P: AsRef<Path>>(storage: P, config: Config) -> anyhow::Result<()> {
    if let Some(dir_name) = storage.as_ref().parent() {
        std::fs::create_dir_all(dir_name)
            .context("failed to create config parent directory")
            .and_then(|()| toml::to_string(&config).context("failed to toml serialize config"))
            .and_then(|contents| {
                std::fs::write(storage.as_ref(), contents)
                    .context("failed to write serialized config to disk")
            })
    } else {
        anyhow::bail!("invalid storage path: {:?}", storage.as_ref());
    }
}
