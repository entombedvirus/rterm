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

    pub fn font_family(&mut self, font_postscript_name: &str) -> egui::FontFamily {
        let font_data = self.font_defs.font_data.get(font_postscript_name);
        if font_data.is_none() {
            self.search_system(font_postscript_name)
                .or_else(|err1| {
                    log::warn!("failed to register font: {err1}. using fallback font");
                    self.attempt_fallback(font_postscript_name)
                })
                .and_then(|font_data| self.register_font(font_postscript_name, font_data))
                .map(|_| egui::FontFamily::Name(font_postscript_name.into()))
                .unwrap_or_else(|err2| {
                    log::warn!("failed to choose a fallback: {err2}");
                    egui::FontFamily::Monospace
                })
        } else {
            egui::FontFamily::Name(font_postscript_name.into())
        }
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

    fn search_and_register(&mut self, font_postscript_name: &str) -> anyhow::Result<()> {
        let font_data = self.search_system(font_postscript_name)?;
        self.register_font(font_postscript_name, font_data)
    }

    fn search_system(&self, font_postscript_name: &str) -> anyhow::Result<egui::FontData> {
        let font_source = font_kit::source::SystemSource::new();
        let font_handle = font_source
            .select_by_postscript_name(font_postscript_name)
            .with_context(|| format!("unable to resolve font with name {font_postscript_name}"))?;
        let font = font_handle.load().context("font loading failed")?;
        let font_bytes = font.copy_font_data().context("reading font data failed")?;
        Ok(egui::FontData::from_owned((*font_bytes).clone()))
    }

    fn register_font(
        &mut self,
        font_postscript_name: &str,
        font_data: egui::FontData,
    ) -> anyhow::Result<()> {
        self.font_defs
            .font_data
            .insert(font_postscript_name.to_string(), font_data);
        self.font_defs.families.insert(
            egui::FontFamily::Name(font_postscript_name.into()),
            vec![font_postscript_name.to_string()],
        );
        self.ctx.set_fonts(self.font_defs.clone());
        Ok(())
    }
}
