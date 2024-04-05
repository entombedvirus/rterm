use std::{collections::BTreeMap, fmt::Write};

use anyhow::{Context, Result};
use glow::{MAX, SHORT};
use winit::{
    event::{ElementState, KeyEvent, Modifiers},
    keyboard::{Key, KeyLocation, NamedKey, SmolStr},
    platform::modifier_supplement::KeyEventExtModifierSupplement,
};

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone)]
enum ProgressiveEnhancementFlag {
    #[default]
    DisambiguateEscapeCodes = 0b1,
    ReportEventTypes = 0b10,
    ReportAlternateKeys = 0b100,
    ReportAllKeysAsEscapeCodes = 0b1000,
    ReportAssociatedText = 0b10000,
}

#[derive(Debug, Clone, Copy)]
pub struct ProgressiveMode(pub u8);

impl ProgressiveMode {
    fn has(&self, flag: ProgressiveEnhancementFlag) -> bool {
        self.0 & flag as u8 != 0
    }
}

#[derive(Debug)]
enum KeyRepr {
    // for:
    //  - Enter | Tab | Space | Backspace | Escape
    C0(u8, egui::Modifiers),

    // for:
    //  - a-z 0-9 ` - = [ ] \ ; ' , . /
    LegacyAscii(u8, egui::Modifiers),

    CsiTilde {
        num: u8,
        modifiers: Option<SmolStr>,
    },
    CsiLetter {
        letter: char,
        modifiers: Option<SmolStr>,
    },
    Ss3 {
        letter: char,
    },
    CsiUnicode {
        code_point: u32,
        modifiers: Option<SmolStr>,
    },
    ProgressiveCsiUnicode {
        key_code: u32,
        shifted_key_code: Option<u32>,
        base_key_code: Option<u32>,
        modifiers: Option<SmolStr>,
        event_type: Option<SmolStr>,
        text_as_codepoints: Option<SmolStr>,
        suffix_ascii_byte: u8,
    },
}

impl std::fmt::Display for KeyRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyRepr::C0(normalized_ascii_byte, modifiers) => {
                format_normalized_byte(*normalized_ascii_byte, f, modifiers)
            }

            KeyRepr::LegacyAscii(normalized_ascii_byte, modifiers) => {
                format_normalized_byte(*normalized_ascii_byte, f, modifiers)
            }

            KeyRepr::CsiTilde {
                num,
                modifiers: None,
            } => write!(f, "\x1b[{num}~"),

            KeyRepr::CsiTilde {
                num,
                modifiers: Some(mods),
            } => write!(f, "\x1b[{num};{mods}~"),

            KeyRepr::CsiLetter {
                letter,
                modifiers: None,
            } => write!(f, "\x1b[{letter}"),

            KeyRepr::CsiLetter {
                letter,
                modifiers: Some(mods),
            } => write!(f, "\x1b[1;{mods}{letter}"),

            KeyRepr::Ss3 { letter } => write!(f, "\x1b\x4f{letter}"),

            KeyRepr::CsiUnicode {
                code_point,
                modifiers: None,
            } => write!(f, "\x1b[{code_point}u"),

            KeyRepr::CsiUnicode {
                code_point,
                modifiers: Some(mods),
            } => write!(f, "\x1b[{code_point};{mods}u"),

            KeyRepr::ProgressiveCsiUnicode {
                key_code,
                shifted_key_code,
                base_key_code,
                modifiers,
                event_type,
                text_as_codepoints,
                suffix_ascii_byte: suffix,
            } => {
                write!(f, "\x1b[")?;
                write!(f, "{key_code}")?;

                match (shifted_key_code, base_key_code) {
                    (None, None) => (),
                    (Some(shifted), None) => write!(f, ":{shifted}")?,
                    (None, Some(base)) => write!(f, "::{base}")?,
                    (Some(shifted), Some(base)) => write!(f, ":{shifted}:{base}")?,
                }

                match (modifiers, event_type) {
                    (None, None) => write!(f, ";")?,
                    (Some(mods), None) => write!(f, ";{mods}")?,
                    (None, Some(event_type)) => write!(f, ";1:{event_type}")?,
                    (Some(mods), Some(event_type)) => write!(f, ";{mods}:{event_type}")?,
                }

                if let Some(txt) = text_as_codepoints {
                    write!(f, ";{txt}")?;
                }
                write!(f, "{}", *suffix as char)
            }
        }
    }
}

fn format_normalized_byte(
    normalized_ascii_byte: u8,
    f: &mut std::fmt::Formatter<'_>,
    modifiers: &egui::Modifiers,
) -> std::fmt::Result {
    if !modifier_supported_in_legacy_mode(
        matches!(normalized_ascii_byte, 0xd | 0x1b | 0x8 | 0x9 | 0x20),
        modifiers,
    ) {
        let repr = KeyRepr::csi_unicode(normalized_ascii_byte as char as u32, modifiers);
        return f.write_fmt(format_args!("{repr}"));
    }
    if normalized_ascii_byte == 0x8 {
        let ctrl_shift = egui::Modifiers::CTRL | egui::Modifiers::SHIFT;
        let alt_shift = egui::Modifiers::ALT | egui::Modifiers::SHIFT;
        let ctrl_alt = egui::Modifiers::CTRL | egui::Modifiers::ALT;
        let as_str = match *modifiers {
            egui::Modifiers::NONE => "\x08",
            egui::Modifiers::CTRL => "\x7f",
            egui::Modifiers::ALT => "\x1b\x7f",
            egui::Modifiers::SHIFT => "\x7f",
            x if x == ctrl_shift => "\x08",
            x if x == alt_shift => "\x1b\x7f",
            x if x == ctrl_alt => "\x1b\x08",
            _ => unreachable!(
                "supported_in_legacy_mode check earlier eliminates other possibilities"
            ),
        };
        return f.write_str(as_str);
    }

    if modifiers.alt {
        f.write_char('\x1b')?;
    }

    if modifiers.ctrl {
        if let Some(ctrl_mapping) = legacy_ctrl_mapping(normalized_ascii_byte, modifiers.shift) {
            return f.write_str(ctrl_mapping.as_str());
        }
    }

    if modifiers.shift {
        if let Some(shifted) = shifted_lut(normalized_ascii_byte) {
            return f.write_str(shifted.as_str());
        }
    }

    f.write_char(normalized_ascii_byte as char)
}

impl KeyRepr {
    fn csi_tilde(num: u8, mods: &egui::Modifiers) -> Self {
        if modifier_supported_in_legacy_mode(false, mods) {
            let modifiers = mods.any().then(|| encode_modifiers(mods));
            Self::CsiTilde { num, modifiers }
        } else {
            Self::csi_unicode(num as u32, mods)
        }
    }

    fn csi_letter(letter: char, mods: &egui::Modifiers) -> Self {
        if modifier_supported_in_legacy_mode(false, mods) {
            let modifiers = mods.any().then(|| encode_modifiers(mods));
            Self::CsiLetter { letter, modifiers }
        } else {
            Self::csi_unicode(letter as u32, mods)
        }
    }

    fn ss3(letter: char, mods: &egui::Modifiers) -> Self {
        if mods.any() {
            Self::csi_letter(letter, mods)
        } else {
            Self::Ss3 { letter }
        }
    }

    fn csi_unicode(code_point: u32, mods: &egui::Modifiers) -> Self {
        let modifiers = mods.any().then(|| encode_modifiers(mods));
        Self::CsiUnicode {
            code_point,
            modifiers,
        }
    }

    fn progressive_csi_unicode(
        key_code: u32,
        ev: &winit::event::KeyEvent,
        modifiers: &egui::Modifiers,
        mode: ProgressiveMode,
    ) -> Self {
        let shifted_key_code =
            if mode.has(ProgressiveEnhancementFlag::ReportAlternateKeys) && modifiers.shift {
                ev.logical_key.to_text().and_then(|txt| {
                    txt.chars()
                        .next()
                        .map(u32::from)
                        .filter(|shifted| *shifted != key_code)
                })
            } else {
                None
            };
        let base_key_code = if mode.has(ProgressiveEnhancementFlag::ReportAlternateKeys) {
            Self::lookup_key_code(&ev.key_without_modifiers(), ev.location)
                .filter(|base_key_code| *base_key_code != key_code)
        } else {
            None
        };

        let modifiers = modifiers.any().then(|| {
            if ev.state == ElementState::Released {
                // reset the modifier being released
                let mut new_mods = modifiers.clone();
                match ev.logical_key {
                    Key::Named(NamedKey::Control) => new_mods.ctrl = false,
                    Key::Named(NamedKey::Shift) => new_mods.shift = false,
                    Key::Named(NamedKey::Alt) => new_mods.alt = false,
                    Key::Named(NamedKey::Super) => new_mods.mac_cmd = false,
                    _ => (),
                }
                encode_modifiers(&new_mods)
            } else {
                encode_modifiers(modifiers)
            }
        });
        fn excepted_key(key: &Key) -> bool {
            matches!(
                key,
                Key::Named(NamedKey::Enter | NamedKey::Tab | NamedKey::Backspace)
            )
        }

        let event_type = {
            let should_report = mode.has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes)
                || (mode.has(ProgressiveEnhancementFlag::ReportEventTypes)
                    && !excepted_key(&ev.logical_key));

            should_report.then_some(if ev.repeat {
                "2".into()
            } else if !ev.state.is_pressed() {
                "3".into()
            } else {
                "1".into()
            })
        };

        let text_as_codepoints = mode
            .has(ProgressiveEnhancementFlag::ReportAssociatedText)
            .then(|| {
                ev.text_with_all_modifiers()
                    .and_then(|txt| txt.chars().next())
                    .filter(|ch| ch.is_ascii_control())
                    .map(u32::from)
                    .filter(|alt_codepoint| *alt_codepoint != key_code)
                    .map(|code| format!("{code}").into())
            })
            .flatten();

        let suffix_ascii_byte = b'u';

        Self::ProgressiveCsiUnicode {
            key_code,
            shifted_key_code,
            base_key_code,
            modifiers,
            event_type,
            text_as_codepoints,
            suffix_ascii_byte,
        }
    }

    fn lookup_key_code(key: &Key, location: KeyLocation) -> Option<u32> {
        Some(match (key.as_ref(), location) {
            (Key::Character("0"), KeyLocation::Numpad) => 57399,
            (Key::Character("1"), KeyLocation::Numpad) => 57400,
            (Key::Character("2"), KeyLocation::Numpad) => 57401,
            (Key::Character("3"), KeyLocation::Numpad) => 57402,
            (Key::Character("4"), KeyLocation::Numpad) => 57403,
            (Key::Character("5"), KeyLocation::Numpad) => 57404,
            (Key::Character("6"), KeyLocation::Numpad) => 57405,
            (Key::Character("7"), KeyLocation::Numpad) => 57406,
            (Key::Character("8"), KeyLocation::Numpad) => 57407,
            (Key::Character("9"), KeyLocation::Numpad) => 57408,
            (Key::Character("."), KeyLocation::Numpad) => 57409,
            (Key::Character("/"), KeyLocation::Numpad) => 57410,
            (Key::Character("*"), KeyLocation::Numpad) => 57411,
            (Key::Character("-"), KeyLocation::Numpad) => 57412,
            (Key::Character("+"), KeyLocation::Numpad) => 57413,
            (Key::Character("="), KeyLocation::Numpad) => 57415,
            (Key::Named(named), KeyLocation::Numpad) => match named {
                NamedKey::Enter => 57414,
                NamedKey::ArrowLeft => 57417,
                NamedKey::ArrowRight => 57418,
                NamedKey::ArrowUp => 57419,
                NamedKey::ArrowDown => 57420,
                NamedKey::PageUp => 57421,
                NamedKey::PageDown => 57422,
                NamedKey::Home => 57423,
                NamedKey::End => 57424,
                NamedKey::Insert => 57425,
                NamedKey::Delete => 57426,
                _ => return None,
            },
            (Key::Named(named), _) => match (named, location) {
                (NamedKey::Tab, _) => 9,
                (NamedKey::Enter, _) => 13,
                (NamedKey::Escape, _) => 27,
                (NamedKey::Space, _) => 32,
                (NamedKey::Backspace, _) => 127,
                (NamedKey::Shift, KeyLocation::Left) => 57441,
                (NamedKey::Control, KeyLocation::Left) => 57442,
                (NamedKey::Alt, KeyLocation::Left) => 57443,
                (NamedKey::Super, KeyLocation::Left) => 57444,
                (NamedKey::Hyper, KeyLocation::Left) => 57445,
                (NamedKey::Meta, KeyLocation::Left) => 57446,
                (NamedKey::Shift, _) => 57447,
                (NamedKey::Control, _) => 57448,
                (NamedKey::Alt, _) => 57449,
                (NamedKey::Super, _) => 57450,
                (NamedKey::Hyper, _) => 57451,
                (NamedKey::Meta, _) => 57452,
                (NamedKey::CapsLock, _) => 57358,
                (NamedKey::NumLock, _) => 57360,
                _ => return None,
            },
            _ => return None,
        })
    }
}

fn modifier_supported_in_legacy_mode(is_c0_special: bool, mods: &egui::Modifiers) -> bool {
    use egui::Modifiers;
    let supported_combos = [
        Modifiers::NONE,
        Modifiers::CTRL,
        Modifiers::ALT,
        Modifiers::SHIFT,
        Modifiers::ALT | Modifiers::SHIFT,
        Modifiers::ALT | Modifiers::CTRL,
    ];

    let ret = supported_combos.contains(mods);
    if is_c0_special {
        ret || (*mods == (Modifiers::CTRL | Modifiers::SHIFT))
    } else {
        ret
    }
}

#[derive(Debug)]
struct KeyContext<'a> {
    mode: ProgressiveMode,
    event: &'a KeyEvent,
    modifiers: &'a egui::Modifiers,
}

impl<'a> KeyContext<'a> {
    fn should_ignore(&self) -> bool {
        if self
            .mode
            .has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes)
        {
            return false;
        }
        let txt = self.event.text_with_all_modifiers();
        !self.event.state.is_pressed() || txt.is_none()
    }

    fn should_emit_control_code(&self) -> bool {
        if self
            .mode
            .has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes)
        {
            return true;
        }

        let has_modifier = self.modifiers.any() && !self.modifiers.shift_only();
        let has_known_control_seq = true; // todo
        has_modifier && has_known_control_seq
    }

    fn try_numpad(&self) -> Option<KeyRepr> {
        if self.event.location != KeyLocation::Numpad
            || self.modifiers.is_none()
            || !self
                .mode
                .has(ProgressiveEnhancementFlag::DisambiguateEscapeCodes)
        {
            return None;
        }

        let code_point: u32 = match self.event.logical_key.as_ref() {
            Key::Character("0") => 57399,
            Key::Character("1") => 57400,
            Key::Character("2") => 57401,
            Key::Character("3") => 57402,
            Key::Character("4") => 57403,
            Key::Character("5") => 57404,
            Key::Character("6") => 57405,
            Key::Character("7") => 57406,
            Key::Character("8") => 57407,
            Key::Character("9") => 57408,
            Key::Character(".") => 57409,
            Key::Character("/") => 57410,
            Key::Character("*") => 57411,
            Key::Character("-") => 57412,
            Key::Character("+") => 57413,
            Key::Character("=") => 57415,
            Key::Named(named) => match named {
                NamedKey::Enter => 57414,
                NamedKey::ArrowLeft => 57417,
                NamedKey::ArrowRight => 57418,
                NamedKey::ArrowUp => 57419,
                NamedKey::ArrowDown => 57420,
                NamedKey::PageUp => 57421,
                NamedKey::PageDown => 57422,
                NamedKey::Home => 57423,
                NamedKey::End => 57424,
                NamedKey::Insert => 57425,
                NamedKey::Delete => 57426,
                _ => return None,
            },
            _ => return None,
        };
        Some(self.build_csi_repr(code_point))
    }

    fn try_c0_special_keys(&self) -> Option<KeyRepr> {
        if self
            .mode
            .has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes)
        {
            return None;
        }

        match self.event.logical_key.as_ref() {
            Key::Named(key @ (NamedKey::Enter | NamedKey::Tab | NamedKey::Backspace)) => {
                let code_point = key
                    .to_text()
                    .and_then(|txt| txt.as_bytes().first())
                    .copied()
                    .expect("enter, tab and backspaced are expected to have textual representaion");
                Some(KeyRepr::C0(code_point, *self.modifiers))
            }
            _ => None,
        }
    }

    fn try_as_csi_functional_key(&self) -> Option<KeyRepr> {
        let named_key = match self.event.logical_key.as_ref() {
            Key::Named(named_key) => named_key,
            _ => return None,
        };

        let code_point = match (named_key, self.event.location) {
            (NamedKey::Tab, _) => 9,
            (NamedKey::Enter, _) => 13,
            (NamedKey::Escape, _) => 27,
            (NamedKey::Space, _) => 32,
            (NamedKey::Backspace, _) => 127,
            (NamedKey::Shift, KeyLocation::Left) => 57441,
            (NamedKey::Control, KeyLocation::Left) => 57442,
            (NamedKey::Alt, KeyLocation::Left) => 57443,
            (NamedKey::Super, KeyLocation::Left) => 57444,
            (NamedKey::Hyper, KeyLocation::Left) => 57445,
            (NamedKey::Meta, KeyLocation::Left) => 57446,
            (NamedKey::Shift, _) => 57447,
            (NamedKey::Control, _) => 57448,
            (NamedKey::Alt, _) => 57449,
            (NamedKey::Super, _) => 57450,
            (NamedKey::Hyper, _) => 57451,
            (NamedKey::Meta, _) => 57452,
            (NamedKey::CapsLock, _) => 57358,
            (NamedKey::NumLock, _) => 57360,
            _ => return None,
        };

        Some(self.build_csi_repr(code_point))
    }

    fn try_as_csi_text(&self) -> Option<KeyRepr> {
        let txt = match self.event.logical_key.as_ref() {
            Key::Character(txt) => txt.to_lowercase(),
            _ => return None,
        };

        if txt.is_empty() {
            return None;
        }

        let code_point = if txt.chars().count() == 1 {
            txt.chars()
                .next()
                .map(u32::from)
                .expect("txt chars is checked above")
        } else {
            // we don't have a single unicode code point to report.
            // if we are in an appropriate mode, we can report it as text with
            // code point set to zero
            0
        };
        Some(self.build_csi_repr(code_point))
    }

    fn build_csi_repr(&self, code_point: u32) -> KeyRepr {
        KeyRepr::progressive_csi_unicode(code_point, self.event, self.modifiers, self.mode)
    }
}

#[derive(Debug)]
pub struct KeyboardHandler {
    /// CSI number ; modifier ~
    /// CSI 1 ; modifier {ABCDEFHPQS}
    /// SS3 {ABCDEFHPQRS}
    /// where CSI = \x1b[, SS3 = \x1b\x4f
    /// cursor_key_mode is turned on via the smkx/rmkx terminfo capabilities.
    cursor_key_mode: bool,

    /// Protocol implementation for https://sw.kovidgoyal.net/kitty/keyboard-protocol/
    /// non-empty stack means the handler is currently in progressive mode.
    progressive_mode_stack: Vec<ProgressiveMode>,

    /// whether to treat option key as meta in macos
    option_key_is_meta: bool,
}

impl Default for KeyboardHandler {
    fn default() -> Self {
        Self {
            cursor_key_mode: false,
            progressive_mode_stack: vec![],
            option_key_is_meta: true,
        }
    }
}

impl KeyboardHandler {
    const MAX_STACK_LEN: usize = 10;

    // cases this needs to handle:
    //
    // - printable chars without any modifiers, like 'a', 'b', '[' etc
    // - non-printable chars like backspace and escape
    // - option + key which should translate to option + logical_key if option_as_meta is turned on
    // - modifiers: any combination of ctrl, alt, shift plus another key
    // - pressed vs released vs repeat if in progressive handling mode
    pub fn on_keyboard_event(
        &self,
        mut ev: winit::event::KeyEvent,
        mut modifiers: egui::Modifiers,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        // on mac, if option is not meta, don't treat solitary option key as a modifier since
        // the user is trying to enter some unicode text
        if !self.option_key_is_meta && modifiers == egui::Modifiers::ALT {
            modifiers.alt = false;
        }
        if self.option_key_is_meta && modifiers.alt {
            ev.logical_key = ev.key_without_modifiers();
        }

        if let Some(mode) = self.current_progressive_mode() {
            self.progressive_mode(mode, &ev, modifiers, output)
        } else {
            self.legacy_mode(
                ev.logical_key.clone(),
                ev.key_without_modifiers(),
                modifiers,
                ev.state,
                output,
            )
        }
    }

    pub fn progressive_mode_set_flags(&self, flags: u8, set_mode: u8) {
        match set_mode {
            1 => todo!(),
            2 => todo!(),
            3 => todo!(),
            _ => todo!(),
        }
    }

    pub fn progressive_mode_push(&mut self, mode: ProgressiveMode) {
        if self.progressive_mode_stack.len() + 1 >= Self::MAX_STACK_LEN {
            self.progressive_mode_stack.remove(0);
        }
        self.progressive_mode_stack.push(mode);
        dbg!("pushing", mode);
    }

    pub fn progressive_mode_pop(&mut self, num: u8) {
        for _ in 0..(num as usize).min(Self::MAX_STACK_LEN) {
            self.progressive_mode_stack.pop();
        }
    }

    pub fn set_cursor_keys_mode(&mut self, on_or_off: bool) {
        self.cursor_key_mode = on_or_off;
    }

    pub fn progressive_mode_get_flags(&self, output: &mut String) {
        let flags = self
            .current_progressive_mode()
            .map(|ProgressiveMode(flags)| flags)
            .unwrap_or(0);
        write!(output, "\x1b[?{flags}u").expect("write failed");
    }

    fn current_progressive_mode(&self) -> Option<ProgressiveMode> {
        self.progressive_mode_stack.last().copied()
    }

    fn progressive_mode(
        &self,
        mode: ProgressiveMode,
        ev: &winit::event::KeyEvent,
        modifiers: egui::Modifiers,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        let ctx = KeyContext {
            mode,
            event: ev,
            modifiers: &modifiers,
        };

        if ctx.should_ignore() {
            log::info!(
                "ignoring key event: {:?}, physical_key: {:?}",
                ev.text_with_all_modifiers(),
                ev.physical_key
            );
            return Ok(false);
        }

        log::info!("event: {ev:?}");

        if ctx.should_emit_control_code() {
            let repr = ctx
                .try_numpad()
                .or_else(|| ctx.try_c0_special_keys())
                .or_else(|| ctx.try_as_csi_functional_key())
                .or_else(|| ctx.try_as_csi_text());

            dbg!(&repr);
            match repr {
                Some(repr) => {
                    write!(output, "{repr}")?;
                    return Ok(true);
                }
                None => {
                    log::info!("key ignored due to not being handled: {:?}", ev);
                }
            }
        }

        // pass through as utf-8 bytes
        if let Some(txt) = ev.text_with_all_modifiers() {
            dbg!(&txt);
            output.push_str(txt);
            return Ok(true);
        }

        Ok(false)
    }

    fn legacy_mode(
        &self,
        mut logical_key: Key,
        key_without_modifiers: Key,
        mut modifiers: egui::Modifiers,
        state: winit::event::ElementState,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        if !state.is_pressed() {
            // legacy mode can only handle press event and not release
            return Ok(false);
        }

        if modifiers.alt && self.option_key_is_meta {
            // ignore the special non-ascii char
            logical_key = key_without_modifiers;
        } else {
            // option_key_is_meta turns the case where there is only alt to a no modifier case
            modifiers.alt = false;
        }

        match logical_key {
            // - non-printable chars like backspace, tab, escape, F1 etc.
            Key::Named(functional_key) => {
                output.push_str(
                    handle_legacy_functional_encoding(
                        &functional_key,
                        &modifiers,
                        self.cursor_key_mode,
                    )
                    .as_str(),
                );
                Ok(true)
            }
            // - printable chars without any modifiers, like 'a', 'b', '[' etc
            Key::Character(ch) => {
                match ch.as_str().as_bytes() {
                    //  - a-z 0-9 ` - = [ ] \ ; ' , . /
                    [ch @ (b'a'..=b'z'
                    | b'0'..=b'9'
                    | b'`'
                    | b'-'
                    | b'='
                    | b'['
                    | b']'
                    | b'\\'
                    | b';'
                    | b'\''
                    | b','
                    | b'.'
                    | b'/')] => {
                        let _ = output
                            .write_fmt(format_args!("{}", KeyRepr::LegacyAscii(*ch, modifiers)))
                            .context("legacy ascii encoding failed")?;
                    }
                    _ => output.push_str(ch.as_str()),
                };
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    // one of the following two forms:
    // CSI number; modifier u
    // CSI 1; modifier [~ABCDEFHPQS]
    fn handle_progressive_encoding(
        &self,
        mode: ProgressiveMode,
        ev: &winit::event::KeyEvent,
        modifiers: &egui::Modifiers,
    ) -> Option<KeyRepr> {
        todo!()
    }
}

fn build_progressive_lut() -> BTreeMap<winit::keyboard::KeyCode, KeyRepr> {
    let mut lut = BTreeMap::new();

    macro_rules! impl_lut {
        ($($key:ident $num:literal $suffix:tt)*) => {
            $(lut.insert(
                winit::keyboard::KeyCode::$key,
                KeyRepr::ProgressiveCsiUnicode {
                    key_code: $num,
                    shifted_key_code: None,
                    base_key_code: None,
                    modifiers: None,
                    event_type: None,
                    text_as_codepoints: None,
                    suffix_ascii_byte: stringify!($suffix).as_bytes()[0],
                },
            );)+
        };
    }

    impl_lut![
    Escape	27 u
    Tab	9 u
    Insert	2 ~
    ArrowLeft	1 D
    ArrowUp	1 A
    PageUp	5 ~
    Home	1 H
    CapsLock	57358 u
    NumLock	57360 u
    Pause	57362 u
    F1	1 P
    F3	13 ~
    F5	15 ~
    F7	18 ~
    F9	20 ~
    F11	23 ~
    F13	57376 u
    F15	57378 u
    F17	57380 u
    F19	57382 u
    F21	57384 u
    F23	57386 u
    F25	57388 u
    F27	57390 u
    F29	57392 u
    F31	57394 u
    F33	57396 u
    F35	57398 u
    Numpad1	57400 u
    Numpad3	57402 u
    Numpad5	57404 u
    Numpad7	57406 u
    Numpad9	57408 u
    NumpadDivide	57410 u
    NumpadSubtract	57412 u
    NumpadEnter	57414 u
    NumpadHash	57416 u
    // KP_RIGHT	57418 u
    // KP_DOWN	57420 u
    // KP_PAGE_DOWN	57422 u
    // KP_END	57424 u
    // KP_DELETE	57426 u
    // MediaPlay	57428 u
    MediaPlayPause	57430 u
    MediaStop	57432 u
    // MediaRewind	57434 u
    MediaTrackPrevious	57436 u
    AudioVolumeDown	57438 u
    AudioVolumeMute	57440 u
    ControlLeft	57442 u
    SuperLeft	57444 u
    // LEFT_META	57446 u
    ControlRight	57448 u
    SuperRight	57450 u
    // RIGHT_META	57452 u
    // ISO_LEVEL5_SHIFT	57454 u
    Enter	13 u
    Backspace	127 u
    Delete	3 ~
    ArrowRight	1 C
    ArrowDown	1 B
    PageDown	6 ~
    End	1 F
    ScrollLock	57359 u
    PrintScreen	57361 u
    ContextMenu	57363 u
    F2	1 Q
    F4	1 S
    F6	17 ~
    F8	19 ~
    F10	21 ~
    F12	24 ~
    F14	57377 u
    F16	57379 u
    F18	57381 u
    F20	57383 u
    F22	57385 u
    F24	57387 u
    F26	57389 u
    F28	57391 u
    F30	57393 u
    F32	57395 u
    F34	57397 u
    Numpad0	57399 u
    Numpad2	57401 u
    Numpad4	57403 u
    Numpad6	57405 u
    Numpad8	57407 u
    NumpadDecimal	57409 u
    NumpadMultiply	57411 u
    NumpadAdd	57413 u
    NumpadEqual	57415 u
    // KP_LEFT	57417 u
    // KP_UP	57419 u
    // KP_PAGE_UP	57421 u
    // KP_HOME	57423 u
    // KP_INSERT	57425 u
    // KP_BEGIN	1 E or 57427 ~
    // MEDIA_PAUSE	57429 u
    // MEDIA_REVERSE	57431 u
    // MEDIA_FAST_FORWARD	57433 u
    MediaTrackNext	57435 u
    // MEDIA_RECORD	57437 u
    AudioVolumeUp	57439 u
    ShiftLeft	57441 u
    AltLeft	57443 u
    Hyper	57445 u
    ShiftRight	57447 u
    AltRight	57449 u
    // RIGHT_HYPER	57451 u
    // ISO_LEVEL3_SHIFT	57453 u
    ];

    lut
}

fn normalized_ascii_byte(key: &winit::keyboard::NamedKey) -> u8 {
    key.to_text()
        .and_then(|s| s.as_bytes().first())
        .copied()
        .expect("these keys have ascii representation")
}

fn physical_key_to_unicode_point(physical_key: winit::keyboard::PhysicalKey) -> Option<u32> {
    let winit::keyboard::PhysicalKey::Code(key_code) = physical_key else {
        return None;
    };

    use winit::keyboard::KeyCode::*;
    Some(match key_code {
        // numpad buttons
        Numpad0 => 57399,
        Numpad1 => 57400,
        Numpad2 => 57401,
        Numpad3 => 57402,
        Numpad4 => 57403,
        Numpad5 => 57404,
        Numpad6 => 57405,
        Numpad7 => 57406,
        Numpad8 => 57407,
        Numpad9 => 57408,
        NumpadDecimal => 57409,
        NumpadDivide => 57410,
        NumpadMultiply | NumpadStar => 57411,
        NumpadSubtract => 57412,
        NumpadAdd => 57413,
        NumpadEnter => 57414,
        NumpadEqual => 57415,
        NumpadComma | NumpadHash => 57416, // NumpadSeparator

        _ => return None,
        // mapped to their non-numpad version
        // NumpadParenLeft,
        // NumpadParenRight,
        // NumpadBackspace,
        // NumpadLeft
        // NumpadRight
        // NumpadUp
        // NumpadDown
        // NumpadPageUp
        // NumpadPageDown
        // NumpadHome
        // NumpadEnd
        // NumpadInsert
        // NumpadDelete
        // NumpanBegin
    })
}

fn handle_legacy_functional_encoding(
    functional_key: &winit::keyboard::NamedKey,
    modifiers: &egui::Modifiers,
    mut cursor_key_mode: bool,
) -> SmolStr {
    use winit::keyboard::NamedKey::*;

    if cursor_key_mode && modifiers.any() {
        // cursor keys mode is only supported when there are no modifiers
        cursor_key_mode = false;
    }

    let c0 = |normalized_byte: u8| KeyRepr::C0(normalized_byte, *modifiers);
    let csi_tilde = |num: u8| KeyRepr::csi_tilde(num, modifiers);
    let ss3 = |letter: char| KeyRepr::ss3(letter, modifiers);
    let csi_letter = |letter: char| KeyRepr::csi_letter(letter, modifiers);

    let repr = match functional_key {
        // C0 control code special handling
        Enter | Tab | Space | Backspace | Escape => c0(normalized_ascii_byte(functional_key)),

        Insert => csi_tilde(2),
        Delete => csi_tilde(3),

        ArrowUp if cursor_key_mode => ss3('A'),
        ArrowDown if cursor_key_mode => ss3('B'),
        ArrowRight if cursor_key_mode => ss3('C'),
        ArrowLeft if cursor_key_mode => ss3('D'),
        Home if cursor_key_mode => ss3('H'),
        End if cursor_key_mode => ss3('F'),

        ArrowUp => csi_letter('A'),
        ArrowDown => csi_letter('B'),
        ArrowRight => csi_letter('C'),
        ArrowLeft => csi_letter('D'),

        Home => csi_letter('H'),
        End => csi_letter('F'),
        PageUp => csi_tilde(5),
        PageDown => csi_tilde(6),

        F1 => ss3('P'),
        F2 => ss3('Q'),
        F3 => ss3('R'),
        F4 => ss3('S'),
        F5 => csi_tilde(15),
        F6 => csi_tilde(17),
        F7 => csi_tilde(18),
        F8 => csi_tilde(19),
        F9 => csi_tilde(20),
        F10 => csi_tilde(21),
        F11 => csi_tilde(23),
        F12 => csi_tilde(24),
        ContextMenu => csi_tilde(29),
        _ => return "".into(),
    };

    repr.to_string().into()
}

fn shifted_lut(normalized: u8) -> Option<winit::keyboard::SmolStr> {
    match normalized {
        b'\x08' => Some(SmolStr::new_static("\x7f")),
        b'\x09' => Some(SmolStr::new_static("\x1b[Z")),
        b'\x20' | b'\x0d' | b'\x1b' => Some(SmolStr::new_inline(unsafe {
            std::str::from_utf8_unchecked(&[normalized])
        })),
        b'`' => Some(SmolStr::new_static("~")),
        b'1' => Some(SmolStr::new_static("!")),
        b'2' => Some(SmolStr::new_static("@")),
        b'3' => Some(SmolStr::new_static("#")),
        b'4' => Some(SmolStr::new_static("$")),
        b'5' => Some(SmolStr::new_static("%")),
        b'6' => Some(SmolStr::new_static("^")),
        b'7' => Some(SmolStr::new_static("&")),
        b'8' => Some(SmolStr::new_static("*")),
        b'9' => Some(SmolStr::new_static("(")),
        b'0' => Some(SmolStr::new_static(")")),
        b'-' => Some(SmolStr::new_static("_")),
        b'=' => Some(SmolStr::new_static("+")),
        b'[' => Some(SmolStr::new_static("{")),
        b']' => Some(SmolStr::new_static("}")),
        b'\\' => Some(SmolStr::new_static("|")),
        b';' => Some(SmolStr::new_static(":")),
        b'\'' => Some(SmolStr::new_static("\"")),
        b',' => Some(SmolStr::new_static("<")),
        b'.' => Some(SmolStr::new_static(">")),
        b'/' => Some(SmolStr::new_static("?")),
        ch @ b'a'..=b'z' => Some((ch as char).to_uppercase().to_string().into()),
        _ => None,
    }
}

// See: https://sw.kovidgoyal.net/kitty/keyboard-protocol/#id10
fn legacy_ctrl_mapping(mut ch: u8, shift_pressed: bool) -> Option<SmolStr> {
    if matches!(ch, b'a'..=b'z') {
        ch.make_ascii_uppercase();
    }
    let ret = match ch {
        b'\x20' => 0,
        b'\x08' => b'\x7f',
        b'\x09' if shift_pressed => return Some(SmolStr::new_static("\x1b[Z")),
        b'\x0d' | b'\x1b' | b'\x09' => ch,
        // See: https://pbxbook.com/other/ctrlcods.html
        b'@'..=b'_' => ch & 0b0011_1111,
        b'/' => 31,
        b'0' => 48,
        b'1' => 49,
        b'2' => 0,
        b'3' => 27,
        b'4' => 28,
        b'5' => 29,
        b'6' => 30,
        b'7' => 31,
        b'8' => 127,
        b'9' => 57,
        b'?' => 127,
        _other => return None,
    };

    let str_bytes = [ret];
    let as_str = unsafe { std::str::from_utf8_unchecked(&str_bytes) };
    Some(SmolStr::new_inline(as_str))
}

fn encode_modifiers(mods: &egui::Modifiers) -> SmolStr {
    let mut ret = 0;
    if mods.shift {
        ret |= 0b1;
    }
    if mods.alt {
        ret |= 0b10;
    }
    if mods.ctrl {
        ret |= 0b100;
    }
    if mods.mac_cmd {
        ret |= 0b1000;
    }
    SmolStr::new_inline(&format!("{}", 1 + ret))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct LegacyHandlingTestCase {
        logical_key: winit::keyboard::Key,
        key_without_modifiers: winit::keyboard::Key,
        modifiers: egui::Modifiers,
        state: winit::event::ElementState,

        expected_output: &'static str,
    }

    impl LegacyHandlingTestCase {
        fn run(&self, handler: &KeyboardHandler) {
            let Self {
                logical_key,
                key_without_modifiers,
                modifiers,
                state,

                expected_output,
            } = self;
            let mut actual_output = String::new();
            let result = handler.legacy_mode(
                logical_key.clone(),
                key_without_modifiers.clone(),
                *modifiers,
                *state,
                &mut actual_output,
            );
            assert!(
                result.is_ok(),
                "expected handling to not return an error for {self:?}"
            );
            assert_eq!(
                actual_output.as_str(),
                *expected_output,
                "key handling output did not match expectation. {self:?}"
            );
        }
    }

    macro_rules! on_press {
        (key $k:literal -> $expected:literal) => {
            on_press![NONE+key $k -> $expected]
        };
        ($($modifier:ident),+ + key $k:literal -> $expected:literal) => {
            LegacyHandlingTestCase {
                logical_key: winit::keyboard::Key::Character(winit::keyboard::SmolStr::new_static($k)),
                key_without_modifiers: winit::keyboard::Key::Character(
                    winit::keyboard::SmolStr::new_static($k)
                ),
                modifiers: $(egui::Modifiers::$modifier)|+,
                state: winit::event::ElementState::Pressed,
                expected_output: $expected,
            }
        };
        ($key_variant:ident -> $expected:literal) => {
            on_press![NONE+$key_variant -> $expected]
        };
        ($($modifier:ident),+ + $key_variant:ident -> $expected:literal) => {
            LegacyHandlingTestCase {
                logical_key: winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant),
                key_without_modifiers: winit::keyboard::Key::Named(
                    winit::keyboard::NamedKey::$key_variant,
                ),
                modifiers: $(egui::Modifiers::$modifier)|+,
                state: winit::event::ElementState::Pressed,
                expected_output: $expected,
            }
        };
    }

    #[test]
    fn test_legacy_special_case_key_handling() {
        let handler = KeyboardHandler::default();

        let test_cases = vec![
            on_press![Enter                -> "\x0d"],
            on_press![CTRL+Enter           -> "\x0d"],
            on_press![ALT+Enter            -> "\x1b\x0d"],
            on_press![SHIFT+Enter          -> "\x0d"],
            on_press![CTRL,SHIFT+Enter     -> "\x0d"],
            on_press![ALT,SHIFT+Enter      -> "\x1b\x0d"],
            on_press![CTRL,ALT+Enter       -> "\x1b\x0d"],
            on_press![Escape               -> "\x1b"],
            on_press![CTRL+Escape          -> "\x1b"],
            on_press![ALT+Escape           -> "\x1b\x1b"],
            on_press![SHIFT+Escape         -> "\x1b"],
            on_press![CTRL,SHIFT+Escape    -> "\x1b"],
            on_press![ALT,SHIFT+Escape     -> "\x1b\x1b"],
            on_press![CTRL,ALT+Escape      -> "\x1b\x1b"],
            on_press![Backspace            -> "\x08"],
            on_press![CTRL+Backspace       -> "\x7f"],
            on_press![ALT+Backspace        -> "\x1b\x7f"],
            on_press![SHIFT+Backspace      -> "\x7f"],
            on_press![CTRL,SHIFT+Backspace -> "\x08"],
            on_press![ALT,SHIFT+Backspace  -> "\x1b\x7f"],
            on_press![CTRL,ALT+Backspace   -> "\x1b\x08"],
            on_press![Tab                  -> "\x09"],
            on_press![CTRL+Tab             -> "\x09"],
            on_press![ALT+Tab              -> "\x1b\x09"],
            on_press![SHIFT+Tab            -> "\x1b[Z"],
            on_press![CTRL,SHIFT+Tab       -> "\x1b[Z"],
            on_press![ALT,SHIFT+Tab        -> "\x1b\x1b[Z"],
            on_press![CTRL,ALT+Tab         -> "\x1b\x09"],
            on_press![Space                -> "\x20"],
            on_press![CTRL+Space           -> "\x00"],
            on_press![ALT+Space            -> "\x1b\x20"],
            on_press![SHIFT+Space          -> "\x20"],
            on_press![CTRL,SHIFT+Space     -> "\x00"],
            on_press![ALT,SHIFT+Space      -> "\x1b\x20"],
            on_press![CTRL,ALT+Space       -> "\x1b\x00"],
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_legacy_non_special_keys() {
        let handler = KeyboardHandler::default();

        let test_cases = vec![
            on_press![key "i" -> "i"],
            on_press![SHIFT+key "i" -> "I"],
            on_press![ALT+key "i" -> "\x1bi"],
            on_press![CTRL+key "i" -> "\t"],
            on_press![SHIFT,ALT+key "i" -> "\x1bI"],
            on_press![ALT,CTRL+key "i" -> "\x1b\t"],
            on_press![CTRL,SHIFT+key "i" -> "\x1b[105;6u"],
            on_press![key "3" -> "3"],
            on_press![SHIFT+key "3" -> "#"],
            on_press![ALT+key "3" -> "\x1b3"],
            on_press![CTRL+key "3" -> "\x1b"],
            on_press![SHIFT,ALT+key "3" -> "\x1b#"],
            on_press![ALT,CTRL+key "3" -> "\x1b\x1b"],
            on_press![CTRL,SHIFT+key "3" -> "\x1b[51;6u"],
            on_press![F1 -> "\x1b\x4fP"],
            on_press![CTRL+F1 -> "\x1b[1;5P"],
            on_press![ArrowUp -> "\x1b[A"],
            on_press![ALT+ArrowUp -> "\x1b[1;3A"],
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_cursor_key_mode() {
        let mut handler = KeyboardHandler::default();
        handler.set_cursor_keys_mode(true);

        let test_cases = vec![
            on_press![ArrowUp    -> "\x1b\x4fA"],
            on_press![ArrowDown  -> "\x1b\x4fB"],
            on_press![ArrowRight -> "\x1b\x4fC"],
            on_press![ArrowLeft  -> "\x1b\x4fD"],
            on_press![Home       -> "\x1b\x4fH"],
            on_press![End        -> "\x1b\x4fF"],
            // when modifiers are used, CSI form is used
            on_press![ALT,SHIFT+ArrowUp    -> "\x1b[1;4A"],
            on_press![ALT,SHIFT+ArrowDown  -> "\x1b[1;4B"],
            on_press![ALT,SHIFT+ArrowRight -> "\x1b[1;4C"],
            on_press![ALT,SHIFT+ArrowLeft  -> "\x1b[1;4D"],
            on_press![ALT,SHIFT+Home       -> "\x1b[1;4H"],
            on_press![ALT,SHIFT+End        -> "\x1b[1;4F"],
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }
}
