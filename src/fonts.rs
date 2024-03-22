use std::{fs, path::PathBuf, sync::mpsc};

use anyhow::Context;

#[derive(Debug)]
pub struct FontManager {
    ctx: egui::Context,
    font_defs: egui::FontDefinitions,
}

impl FontManager {
    pub fn new(ctx: egui::Context) -> Self {
        let font_defs = egui::FontDefinitions::default();
        Self { ctx, font_defs }
    }

    pub fn async_search_for_monospace_fonts(
        &self,
    ) -> mpsc::Receiver<anyhow::Result<Vec<FontDesc>>> {
        let (tx, rx) = mpsc::sync_channel(1);
        std::thread::Builder::new()
            .name("rterm - font searcher".to_string())
            .spawn(move || {
                let search_result = native::search_for_monospace_fonts2();
                let _ = tx.send(search_result); // will not block since chan is buffered
            })
            .expect("spawning background thread failed");
        rx
    }

    pub fn get_or_init(
        &mut self,
        font_family_name: &str,
        font_postscript_name: &str,
    ) -> egui::FontFamily {
        let font_data = self.font_defs.font_data.get(font_postscript_name);
        match font_data {
            None => self.init_font(font_family_name, font_postscript_name),
            Some(_) => egui::FontFamily::Name(font_family_name.into()),
        }
    }

    fn init_font(
        &mut self,
        font_family_name: &str,
        font_postscript_name: &str,
    ) -> egui::FontFamily {
        native::lookup_font_path_by_postscript_name(font_postscript_name)
            .and_then(|font_path| {
                fs::read(&font_path)
                    .map(egui::FontData::from_owned)
                    .map_err(|err| anyhow::format_err!("{font_path:?}: {err}"))
            })
            .or_else(|err1| {
                log::warn!(
                    "failed to read font {font_postscript_name}: {err1}. using fallback font"
                );
                self.attempt_fallback(&font_postscript_name)
            })
            .and_then(|font_data| {
                self.register_font(font_family_name, font_postscript_name, font_data)
                    .map(|_| egui::FontFamily::Name(font_family_name.into()))
            })
            .unwrap_or_else(|err2| {
                log::warn!("failed to choose a fallback: {err2}");
                egui::FontFamily::Monospace
            })
    }

    fn attempt_fallback(&mut self, font_postscript_name: &str) -> anyhow::Result<egui::FontData> {
        self.font_defs
            .families
            .get(&egui::FontFamily::Monospace)
            .and_then(|font_names| font_names.first())
            .and_then(|font_name| self.font_defs.font_data.get(font_name))
            .cloned()
            .with_context(|| format!("failed to find fallback font for: {font_postscript_name}"))
    }

    fn register_font(
        &mut self,
        font_family_name: &str,
        font_postscript_name: &str,
        font_data: egui::FontData,
    ) -> anyhow::Result<()> {
        self.font_defs
            .font_data
            .insert(font_postscript_name.to_string(), font_data);
        self.font_defs.families.insert(
            egui::FontFamily::Name(font_family_name.into()),
            vec![font_postscript_name.to_string()],
        );
        self.ctx.set_fonts(self.font_defs.clone());
        Ok(())
    }
}

#[derive(Debug)]
pub struct FontDesc {
    pub display_name: String,
    pub postscript_name: String,
    pub font_path: PathBuf,
}

impl TryFrom<&core_text::font_descriptor::CTFontDescriptor> for FontDesc {
    type Error = anyhow::Error;

    fn try_from(fd: &core_text::font_descriptor::CTFontDescriptor) -> Result<Self, Self::Error> {
        let font_path = fd.font_path().context("font path is missing")?;
        let display_name = fd.display_name();
        let postscript_name = fd.font_name();
        Ok(FontDesc {
            display_name,
            postscript_name,
            font_path,
        })
    }
}

#[cfg(target_os = "macos")]
mod native {
    use super::FontDesc;
    use anyhow::Context;
    use core_foundation::{
        array::CFArray, base::TCFType, dictionary::CFDictionary, number::CFNumber, string::CFString,
    };
    use core_text::font_descriptor::{
        kCTFontMonoSpaceTrait, kCTFontSymbolicTrait, kCTFontTraitsAttribute,
    };
    use std::{ops::Deref, path::PathBuf};

    pub fn search_for_monospace_fonts2() -> anyhow::Result<Vec<FontDesc>> {
        // query os x font system for all installed monospace fonts
        let traits_dict = CFDictionary::from_CFType_pairs(&[(
            unsafe { CFString::wrap_under_get_rule(kCTFontSymbolicTrait) },
            CFNumber::from(kCTFontMonoSpaceTrait as i32),
        )]);
        let attributes = CFDictionary::from_CFType_pairs(&[(
            unsafe { CFString::wrap_under_get_rule(kCTFontTraitsAttribute) },
            traits_dict.as_CFType(),
        )]);
        let query =
            CFArray::from_CFTypes(&[core_text::font_descriptor::new_from_attributes(&attributes)]);
        let font_collection = core_text::font_collection::new_from_descriptors(&query);
        let matching_font_decs = font_collection
            .get_descriptors()
            .context("failed to get query font system")?;

        let mut fonts: Vec<FontDesc> = matching_font_decs
            .into_iter()
            .filter_map(|fd| fd.deref().try_into().ok())
            .collect();
        fonts.sort_unstable_by(|a, b| a.display_name.cmp(&b.display_name));
        Ok(fonts)
    }

    pub fn lookup_font_path_by_postscript_name(
        font_postscript_name: &str,
    ) -> anyhow::Result<PathBuf> {
        let font_name: CFString = font_postscript_name
            .parse()
            .map_err(|_| anyhow::format_err!("invalid font postscript name"))?;
        let font_desc = core_text::font_descriptor::new_from_postscript_name(&font_name);
        font_desc.font_path().context("font path is missing")
    }
}
