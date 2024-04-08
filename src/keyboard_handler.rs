use std::{collections::BTreeMap, fmt::Write};

use anyhow::Context;
use winit::{
    event::{ElementState, KeyEvent},
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

impl std::ops::BitOr for ProgressiveEnhancementFlag {
    type Output = ProgressiveMode;

    fn bitor(self, rhs: Self) -> Self::Output {
        ProgressiveMode(self as u8 | rhs as u8)
    }
}

impl std::ops::BitAnd for ProgressiveEnhancementFlag {
    type Output = ProgressiveMode;

    fn bitand(self, rhs: Self) -> Self::Output {
        ProgressiveMode(self as u8 & rhs as u8)
    }
}

impl std::ops::BitXor for ProgressiveEnhancementFlag {
    type Output = ProgressiveMode;

    fn bitxor(self, rhs: Self) -> Self::Output {
        ProgressiveMode(self as u8 ^ rhs as u8)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProgressiveMode(pub u8);

impl ProgressiveMode {
    fn has(&self, flag: ProgressiveEnhancementFlag) -> bool {
        self.0 & flag as u8 != 0
    }

    fn all() -> Self {
        use ProgressiveEnhancementFlag::*;
        let flags = [
            DisambiguateEscapeCodes,
            ReportEventTypes,
            ReportAlternateKeys,
            ReportAllKeysAsEscapeCodes,
            ReportAssociatedText,
        ];
        Self(flags.into_iter().fold(0u8, |a, b| a | b as u8))
    }
}

impl std::convert::From<ProgressiveEnhancementFlag> for ProgressiveMode {
    fn from(value: ProgressiveEnhancementFlag) -> Self {
        Self(value as u8)
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
                if *key_code != 1
                    || [modifiers, event_type, text_as_codepoints]
                        .into_iter()
                        .any(Option::is_some)
                    || [shifted_key_code, base_key_code]
                        .into_iter()
                        .any(Option::is_some)
                {
                    write!(f, "{key_code}")?;
                }

                match (shifted_key_code, base_key_code) {
                    (None, None) => (),
                    (Some(shifted), None) => write!(f, ":{shifted}")?,
                    (None, Some(base)) => write!(f, "::{base}")?,
                    (Some(shifted), Some(base)) => write!(f, ":{shifted}:{base}")?,
                }

                let mut needs_sep = false;
                match (modifiers, event_type) {
                    (None, None) => needs_sep = true,
                    (Some(mods), None) => write!(f, ";{mods}")?,
                    (None, Some(event_type)) => write!(f, ";1:{event_type}")?,
                    (Some(mods), Some(event_type)) => write!(f, ";{mods}:{event_type}")?,
                }

                if let Some(txt) = text_as_codepoints {
                    if needs_sep {
                        write!(f, ";")?;
                    }
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
        matches!(normalized_ascii_byte, 0xd | 0x1b | 0x8 | 0x9 | 0x20 | 0x7f),
        modifiers,
    ) {
        let repr = KeyRepr::csi_unicode(normalized_ascii_byte as char as u32, modifiers);
        return f.write_fmt(format_args!("{repr}"));
    }
    if normalized_ascii_byte == 0x7f {
        let ctrl_shift = egui::Modifiers::CTRL | egui::Modifiers::SHIFT;
        let alt_shift = egui::Modifiers::ALT | egui::Modifiers::SHIFT;
        let ctrl_alt = egui::Modifiers::CTRL | egui::Modifiers::ALT;
        let as_str = match *modifiers {
            egui::Modifiers::NONE => "\x7f",
            egui::Modifiers::CTRL => "\x08",
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
        suffix_ascii_byte: u8,
        mode: ProgressiveMode,
        ctx: &KeyContext,
    ) -> Self {
        let KeyContext {
            logical_key,
            text_with_all_modifiers,
            key_without_modifiers,
            state,
            location,
            repeat,
            modifiers,
        } = ctx;
        let shifted_key_code =
            if mode.has(ProgressiveEnhancementFlag::ReportAlternateKeys) && modifiers.shift {
                key_to_text(key_without_modifiers).and_then(|txt| {
                    let txt = txt
                        .as_bytes()
                        .first()
                        .copied()
                        .and_then(shifted_lut)
                        .unwrap_or(txt.into());
                    txt.chars()
                        .next()
                        .map(u32::from)
                        .filter(|shifted| *shifted != key_code)
                })
            } else {
                None
            };
        let base_key_code = if mode.has(ProgressiveEnhancementFlag::ReportAlternateKeys) {
            Self::lookup_key_code(key_without_modifiers, *location)
                .filter(|base_key_code| *base_key_code != key_code)
        } else {
            None
        };

        let modifiers = if matches!(
            logical_key,
            Key::Named(NamedKey::Control | NamedKey::Shift | NamedKey::Alt | NamedKey::Super)
        ) {
            // set or reset the modifier bit on the event for the modifier key since winit doesn't do that
            let mut new_mods = *modifiers;
            let is_set = *state == ElementState::Pressed;
            match logical_key {
                Key::Named(NamedKey::Control) => new_mods.ctrl = is_set,
                Key::Named(NamedKey::Shift) => new_mods.shift = is_set,
                Key::Named(NamedKey::Alt) => new_mods.alt = is_set,
                Key::Named(NamedKey::Super) => new_mods.mac_cmd = is_set,
                _ => (),
            }
            Some(encode_modifiers(&new_mods))
        } else if modifiers.any() {
            Some(encode_modifiers(&modifiers))
        } else {
            None
        };

        fn excepted_key(key: &Key) -> bool {
            matches!(
                key,
                Key::Named(NamedKey::Enter | NamedKey::Tab | NamedKey::Backspace)
            )
        }

        let event_type = {
            let mut should_report = mode
                .has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes)
                || (mode.has(ProgressiveEnhancementFlag::ReportEventTypes)
                    && !excepted_key(&logical_key));

            should_report &= !state.is_pressed() || *repeat;
            should_report.then_some(if *repeat { "2".into() } else { "3".into() })
        };

        let text_as_codepoints = mode
            .has(ProgressiveEnhancementFlag::ReportAssociatedText)
            .then(|| {
                text_with_all_modifiers
                    .as_ref()
                    .and_then(|txt| txt.chars().next())
                    .filter(|ch| !ch.is_ascii_control())
                    .map(u32::from)
                    .filter(|cp| *cp != key_code)
                    .map(|code| format!("{code}").into())
            })
            .flatten();

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

fn named_key_to_text(key: &NamedKey) -> Option<&'static str> {
    Some(match key {
        NamedKey::Enter => "\x0d",
        NamedKey::Tab => "\x09",
        NamedKey::Space => " ",
        NamedKey::Backspace => "\x7f",
        NamedKey::Escape => "\x1b",
        _ => return None,
    })
}

fn key_to_text(key: &Key) -> Option<SmolStr> {
    match key {
        Key::Named(named_key) => named_key_to_text(named_key).map(SmolStr::new_static),
        Key::Character(txt) => Some(txt.clone()),
        _ => None,
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

#[derive(Debug, Clone)]
pub struct KeyContext {
    logical_key: Key,
    key_without_modifiers: Key,
    text_with_all_modifiers: Option<SmolStr>,
    state: ElementState,
    location: KeyLocation,
    repeat: bool,
    modifiers: egui::Modifiers,
}

impl KeyContext {
    pub fn new(ev: &KeyEvent, mods: egui::Modifiers) -> Self {
        Self {
            logical_key: ev.logical_key.clone(),
            key_without_modifiers: ev.key_without_modifiers(),
            text_with_all_modifiers: ev.text_with_all_modifiers().map(SmolStr::new),
            state: ev.state,
            location: ev.location,
            repeat: ev.repeat,
            modifiers: mods,
        }
    }

    fn should_ignore(&self, mode: ProgressiveMode) -> bool {
        if mode.has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes) {
            return false;
        }
        if !self.state.is_pressed() {
            return if mode.has(ProgressiveEnhancementFlag::ReportEventTypes) {
                matches!(
                    &self.logical_key,
                    Key::Named(NamedKey::Enter | NamedKey::Tab | NamedKey::Backspace)
                )
            } else {
                true
            };
        }
        false
    }

    fn should_emit_control_code(&self, mode: ProgressiveMode, option_key_is_meta: bool) -> bool {
        if mode.has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes) {
            return true;
        }
        if mode.has(ProgressiveEnhancementFlag::DisambiguateEscapeCodes) {
            match &self.logical_key {
                Key::Named(NamedKey::Escape) => return true,
                _ => (),
            }
        }
        if mode.has(ProgressiveEnhancementFlag::ReportEventTypes) {
            if self.repeat || !self.state.is_pressed() {
                return true;
            }
        }

        match &self.logical_key {
            Key::Named(NamedKey::Enter | NamedKey::Tab | NamedKey::Backspace)
                if self.modifiers.is_none() =>
            {
                return false
            }
            Key::Named(
                NamedKey::Control
                | NamedKey::Shift
                | NamedKey::Alt
                | NamedKey::Super
                | NamedKey::Hyper,
            ) => false,
            Key::Character(_)
                if self.modifiers.is_none()
                    || self.modifiers == egui::Modifiers::SHIFT
                    || (!option_key_is_meta && self.modifiers.alt) =>
            {
                false
            }
            _ => true,
        }
    }

    fn try_numpad(&self, mode: ProgressiveMode) -> Option<KeyRepr> {
        if self.location != KeyLocation::Numpad {
            return None;
        }
        if mode.has(ProgressiveEnhancementFlag::DisambiguateEscapeCodes)
            && (self.modifiers.is_none() || self.modifiers.shift_only())
        {
            return None;
        }

        let code_point: u32 = match self.logical_key.as_ref() {
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
        Some(self.build_csi_repr(code_point, b'u', mode))
    }

    fn try_c0_special_keys(&self, mode: ProgressiveMode) -> Option<KeyRepr> {
        if mode.has(ProgressiveEnhancementFlag::ReportAllKeysAsEscapeCodes) {
            return None;
        }
        if mode.has(ProgressiveEnhancementFlag::DisambiguateEscapeCodes) && self.modifiers.any() {
            return None;
        }

        match &self.logical_key {
            Key::Named(named_key @ (NamedKey::Enter | NamedKey::Tab | NamedKey::Backspace)) => {
                let code_point = named_key_to_text(named_key)
                    .and_then(|txt| txt.as_bytes().first().copied())
                    .expect("enter, tab and backspaced are expected to have textual representaion");
                Some(KeyRepr::C0(code_point, self.modifiers))
            }
            _ => None,
        }
    }

    fn try_as_csi_functional_key(&self, mode: ProgressiveMode) -> Option<KeyRepr> {
        let named_key = match self.logical_key.as_ref() {
            Key::Named(named_key) => named_key,
            _ => return None,
        };

        let (code_point, suffix) = match (named_key, self.location) {
            (NamedKey::Tab, _) => (9, b'u'),
            (NamedKey::Enter, _) => (13, b'u'),
            (NamedKey::Escape, _) => (27, b'u'),
            (NamedKey::Space, _) => (32, b'u'),
            (NamedKey::Backspace, _) => (127, b'u'),
            (NamedKey::Shift, KeyLocation::Left) => (57441, b'u'),
            (NamedKey::Control, KeyLocation::Left) => (57442, b'u'),
            (NamedKey::Alt, KeyLocation::Left) => (57443, b'u'),
            (NamedKey::Super, KeyLocation::Left) => (57444, b'u'),
            (NamedKey::Hyper, KeyLocation::Left) => (57445, b'u'),
            (NamedKey::Meta, KeyLocation::Left) => (57446, b'u'),
            (NamedKey::Shift, _) => (57447, b'u'),
            (NamedKey::Control, _) => (57448, b'u'),
            (NamedKey::Alt, _) => (57449, b'u'),
            (NamedKey::Super, _) => (57450, b'u'),
            (NamedKey::Hyper, _) => (57451, b'u'),
            (NamedKey::Meta, _) => (57452, b'u'),
            (NamedKey::CapsLock, _) => (57358, b'u'),
            (NamedKey::NumLock, _) => (57360, b'u'),
            (NamedKey::ScrollLock, _) => (57359, b'u'),
            (NamedKey::PrintScreen, _) => (57361, b'u'),
            (NamedKey::Pause, _) => (57362, b'u'),

            (NamedKey::ArrowUp, _) => (1, b'A'),
            (NamedKey::ArrowDown, _) => (1, b'B'),
            (NamedKey::ArrowRight, _) => (1, b'C'),
            (NamedKey::ArrowLeft, _) => (1, b'D'),

            (NamedKey::Home, _) => (1, b'H'),
            (NamedKey::End, _) => (1, b'F'),
            (NamedKey::PageUp, _) => (5, b'~'),
            (NamedKey::PageDown, _) => (6, b'~'),

            (NamedKey::F1, _) => (1, b'P'),
            (NamedKey::F2, _) => (1, b'Q'),
            (NamedKey::F3, _) => (13, b'~'),
            (NamedKey::F4, _) => (1, b'S'),
            (NamedKey::F5, _) => (15, b'~'),
            (NamedKey::F6, _) => (17, b'~'),
            (NamedKey::F7, _) => (18, b'~'),
            (NamedKey::F8, _) => (19, b'~'),
            (NamedKey::F9, _) => (20, b'~'),
            (NamedKey::F10, _) => (21, b'~'),
            (NamedKey::F11, _) => (23, b'~'),
            (NamedKey::F12, _) => (24, b'~'),
            (NamedKey::F13, _) => (57376, b'u'),
            (NamedKey::F14, _) => (57377, b'u'),
            (NamedKey::F15, _) => (57378, b'u'),
            (NamedKey::F16, _) => (57379, b'u'),
            (NamedKey::F17, _) => (57380, b'u'),
            (NamedKey::F18, _) => (57381, b'u'),
            (NamedKey::F19, _) => (57382, b'u'),
            (NamedKey::F20, _) => (57383, b'u'),
            (NamedKey::F21, _) => (57384, b'u'),
            (NamedKey::F22, _) => (57385, b'u'),
            (NamedKey::F23, _) => (57386, b'u'),
            (NamedKey::F24, _) => (57387, b'u'),
            (NamedKey::F25, _) => (57388, b'u'),
            (NamedKey::F26, _) => (57389, b'u'),
            (NamedKey::F27, _) => (57390, b'u'),
            (NamedKey::F28, _) => (57391, b'u'),
            (NamedKey::F29, _) => (57392, b'u'),
            (NamedKey::F30, _) => (57393, b'u'),
            (NamedKey::F31, _) => (57394, b'u'),
            (NamedKey::F32, _) => (57395, b'u'),
            (NamedKey::F33, _) => (57396, b'u'),
            (NamedKey::F34, _) => (57397, b'u'),
            (NamedKey::F35, _) => (57398, b'u'),
            (NamedKey::ContextMenu, _) => (57363, b'u'),
            (NamedKey::MediaPlay, _) => (57428, b'u'),
            (NamedKey::MediaPause, _) => (57429, b'u'),
            (NamedKey::MediaPlayPause, _) => (57430, b'u'),
            (NamedKey::MediaStop, _) => (57432, b'u'),
            (NamedKey::MediaFastForward, _) => (57433, b'u'),
            (NamedKey::MediaRewind, _) => (57434, b'u'),
            (NamedKey::MediaTrackNext, _) => (57435, b'u'),
            (NamedKey::MediaTrackPrevious, _) => (57436, b'u'),
            (NamedKey::MediaRecord, _) => (57437, b'u'),
            (NamedKey::AudioVolumeDown, _) => (57438, b'u'),
            (NamedKey::AudioVolumeUp, _) => (57439, b'u'),
            (NamedKey::AudioVolumeMute, _) => (57440, b'u'),

            _ => return None,
        };

        Some(self.build_csi_repr(code_point, suffix, mode))
    }

    fn try_as_csi_text(&self, mode: ProgressiveMode) -> Option<KeyRepr> {
        let txt = match self.key_without_modifiers.as_ref() {
            Key::Character(txt) if !txt.is_empty() => txt.to_lowercase(),
            _ => return None,
        };

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
        Some(self.build_csi_repr(code_point, b'u', mode))
    }

    fn build_csi_repr(&self, code_point: u32, suffix: u8, mode: ProgressiveMode) -> KeyRepr {
        KeyRepr::progressive_csi_unicode(code_point, suffix, mode, self)
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
            option_key_is_meta: false,
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
        mut ctx: KeyContext,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        if self.option_key_is_meta && ctx.modifiers.alt {
            ctx.logical_key = ctx.key_without_modifiers.clone();
        }

        if let Some(_) = self.current_progressive_mode() {
            self.progressive_mode(ctx, output)
        } else {
            self.legacy_mode(ctx, output)
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

    pub fn push_progressive_mode<T: Into<ProgressiveMode>>(&mut self, mode: T) {
        if self.progressive_mode_stack.len() + 1 >= Self::MAX_STACK_LEN {
            self.progressive_mode_stack.remove(0);
        }
        self.progressive_mode_stack.push(mode.into());
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
        self.progressive_mode_stack
            .last()
            .filter(|pm| **pm != ProgressiveMode(0))
            .copied()
    }

    fn progressive_mode(&self, ctx: KeyContext, output: &mut String) -> anyhow::Result<bool> {
        log::info!("ctx: {ctx:?}");
        let KeyContext {
            logical_key,
            text_with_all_modifiers,
            ..
        } = &ctx;

        let mode = self
            .current_progressive_mode()
            .unwrap_or(ProgressiveMode(0));
        if ctx.should_ignore(mode) {
            log::info!(
                "ignoring key event: {:?}, logical_key: {:?}",
                text_with_all_modifiers,
                logical_key,
            );
            return Ok(false);
        }

        if ctx.should_emit_control_code(mode, self.option_key_is_meta) {
            let repr = ctx
                .try_numpad(mode)
                .or_else(|| ctx.try_c0_special_keys(mode))
                .or_else(|| ctx.try_as_csi_functional_key(mode))
                .or_else(|| ctx.try_as_csi_text(mode));

            match repr {
                Some(repr) => {
                    write!(output, "{repr}")?;
                    return Ok(true);
                }
                None => {
                    log::info!("key ignored due to not being handled: {:?}", logical_key);
                }
            }
        }

        // pass through as utf-8 bytes
        if let Some(txt) = key_to_text(logical_key).as_ref() {
            output.push_str(txt);
            return Ok(true);
        }

        Ok(false)
    }

    fn legacy_mode(&self, ctx: KeyContext, output: &mut String) -> anyhow::Result<bool> {
        let KeyContext {
            logical_key,
            state,
            modifiers,
            ..
        } = ctx;
        if !state.is_pressed() {
            // legacy mode can only handle press event and not release
            return Ok(false);
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
    named_key_to_text(key)
        .and_then(|s| s.as_bytes().first().copied())
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
    if mods.mac_cmd | mods.command {
        ret |= 0b1000;
    }
    SmolStr::new_inline(&format!("{}", 1 + ret))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct TestCase {
        ctx: KeyContext,
        // modifiers: egui::Modifiers,
        expected_output: &'static str,
    }

    impl Default for TestCase {
        fn default() -> Self {
            Self {
                ctx: KeyContext {
                    logical_key: Key::Named(NamedKey::Escape),
                    key_without_modifiers: Key::Named(NamedKey::Escape),
                    text_with_all_modifiers: None,
                    state: ElementState::Pressed,
                    location: KeyLocation::Standard,
                    repeat: false,
                    modifiers: egui::Modifiers::NONE,
                },
                expected_output: "",
            }
        }
    }

    impl TestCase {
        fn with_key_location(mut self, loc: KeyLocation) -> Self {
            self.ctx.location = loc;
            self
        }

        fn with_text(mut self, txt: Option<&str>) -> Self {
            self.ctx.text_with_all_modifiers = txt.map(SmolStr::new);
            self
        }

        fn with_key_without_modifiers(mut self, key: Key) -> Self {
            self.ctx.key_without_modifiers = key;
            self
        }

        fn with_repeat(mut self, b: bool) -> Self {
            self.ctx.repeat = b;
            self
        }

        fn with_modifiers(mut self, mods: egui::Modifiers) -> Self {
            self.ctx.modifiers = mods;
            self
        }

        fn with_logical_key(mut self, key: Key) -> Self {
            self.ctx.logical_key = key;
            self
        }

        fn with_expected_output(mut self, v: &'static str) -> Self {
            self.expected_output = v;
            self
        }

        fn with_char(
            mut self,
            modifier: egui::Modifiers,
            unmodified: &str,
            modified: &str,
        ) -> Self {
            self.ctx.key_without_modifiers = Key::Character(unmodified.into());
            self.ctx.text_with_all_modifiers = Some(modified.into());
            self.ctx.logical_key = Key::Character(modified.into());
            self.ctx.modifiers = modifier;
            self
        }

        fn with_named_key(
            mut self,
            modifier: egui::Modifiers,
            unmodified: NamedKey,
            modified: &str,
        ) -> Self {
            self.ctx.key_without_modifiers = Key::Named(unmodified);
            self.ctx.text_with_all_modifiers = Some(modified.into());
            self.ctx.logical_key = Key::Named(unmodified);
            self.ctx.modifiers = modifier;
            self
        }

        fn with_state(mut self, state: ElementState) -> Self {
            self.ctx.state = state;
            self
        }

        fn run(self, handler: &KeyboardHandler) {
            let mut actual_output = String::new();
            let result = handler.on_keyboard_event(self.ctx.clone(), &mut actual_output);
            assert!(
                result.is_ok(),
                "expected handling to not return an error for {self:?}"
            );
            assert_eq!(
                actual_output.as_str(),
                self.expected_output,
                "key handling output did not match expectation. {self:?}"
            );
        }
    }

    macro_rules! on_key {
        ($k:literal -> $expected:literal) => {
            on_key![NONE+ $k -> $expected]
        };
        ($key_variant:ident -> $expected:literal) => {
            on_key![NONE+$key_variant -> $expected]
        };
        ($($modifier:ident),+ + $k:literal -> $expected:literal) => {
            TestCase::default()
                .with_modifiers($(egui::Modifiers::$modifier)|+)
                .with_logical_key(winit::keyboard::Key::Character($k.into()))
                .with_key_without_modifiers(winit::keyboard::Key::Character($k.into()))
                .with_text(Some($k))
                .with_expected_output($expected)
        };
        ($($modifier:ident),+ + $key_variant:ident -> $expected:literal) => {
            TestCase::default()
                .with_modifiers($(egui::Modifiers::$modifier)|+)
                .with_logical_key(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_without_modifiers(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_expected_output($expected)
        };
        (LEFT $key_variant:ident -> $expected:literal) => {
            TestCase::default()
                .with_logical_key(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_without_modifiers(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_location(winit::keyboard::KeyLocation::Left)
                .with_expected_output($expected)
        };
        (RIGHT $key_variant:ident -> $expected:literal) => {
            TestCase::default()
                .with_logical_key(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_without_modifiers(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_location(winit::keyboard::KeyLocation::Right)
                .with_expected_output($expected)
        };
        (NUMPAD $($modifier:ident),+ + $key_variant:ident -> $expected:literal) => {
            TestCase::default()
                .with_modifiers($(egui::Modifiers::$modifier)|+)
                .with_logical_key(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_without_modifiers(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_location(winit::keyboard::KeyLocation::Numpad)
                .with_expected_output($expected)
        };
        (NUMPAD $($modifier:ident),+ + $char:literal -> $expected:literal) => {
            TestCase::default()
                .with_modifiers($(egui::Modifiers::$modifier)|+)
                .with_logical_key(winit::keyboard::Key::Character(winit::keyboard::SmolStr::new_static($char)))
                .with_key_without_modifiers(winit::keyboard::Key::Character(winit::keyboard::SmolStr::new_static($char)))
                .with_text(Some($char))
                .with_key_location(winit::keyboard::KeyLocation::Numpad)
                .with_expected_output($expected)
        };
        (released $($modifier:ident),+ + $key_variant:ident -> $expected:literal) => {
            TestCase::default()
                .with_modifiers($(egui::Modifiers::$modifier)|+)
                .with_logical_key(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_without_modifiers(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_text(named_key_to_text(&winit::keyboard::NamedKey::$key_variant))
                .with_state(winit::event::ElementState::Released)
                .with_expected_output($expected)

        };
        (repeated $($modifier:ident),+ + $key_variant:ident -> $expected:literal) => {
            TestCase::default()
                .with_modifiers($(egui::Modifiers::$modifier)|+)
                .with_logical_key(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_key_without_modifiers(winit::keyboard::Key::Named(winit::keyboard::NamedKey::$key_variant))
                .with_text(named_key_to_text(&winit::keyboard::NamedKey::$key_variant))
                .with_repeat(true)
                .with_expected_output($expected)

        };
        (released $($modifier:ident),+ + $char:literal -> $expected:literal) => {
            TestCase::default()
                .with_char($(egui::Modifiers::$modifier)|+, $char, $char)
                .with_state(winit::event::ElementState::Released)
                .with_expected_output($expected)

        };
        (repeated $($modifier:ident),+ + $char:literal -> $expected:literal) => {
            TestCase::default()
                .with_char($(egui::Modifiers::$modifier)|+, $char, $char)
                .with_repeat(true)
                .with_expected_output($expected)

        };
    }

    #[test]
    fn test_legacy_special_case_key_handling() {
        let handler = KeyboardHandler::default();

        let test_cases = vec![
            on_key![Enter                -> "\x0d"],
            on_key![CTRL+Enter           -> "\x0d"],
            on_key![ALT+Enter            -> "\x1b\x0d"],
            on_key![SHIFT+Enter          -> "\x0d"],
            on_key![CTRL,SHIFT+Enter     -> "\x0d"],
            on_key![ALT,SHIFT+Enter      -> "\x1b\x0d"],
            on_key![CTRL,ALT+Enter       -> "\x1b\x0d"],
            on_key![Escape               -> "\x1b"],
            on_key![CTRL+Escape          -> "\x1b"],
            on_key![ALT+Escape           -> "\x1b\x1b"],
            on_key![SHIFT+Escape         -> "\x1b"],
            on_key![CTRL,SHIFT+Escape    -> "\x1b"],
            on_key![ALT,SHIFT+Escape     -> "\x1b\x1b"],
            on_key![CTRL,ALT+Escape      -> "\x1b\x1b"],
            on_key![Backspace            -> "\x7f"],
            on_key![CTRL+Backspace       -> "\x08"],
            on_key![ALT+Backspace        -> "\x1b\x7f"],
            on_key![SHIFT+Backspace      -> "\x7f"],
            on_key![CTRL,SHIFT+Backspace -> "\x08"],
            on_key![ALT,SHIFT+Backspace  -> "\x1b\x7f"],
            on_key![CTRL,ALT+Backspace   -> "\x1b\x08"],
            on_key![Tab                  -> "\x09"],
            on_key![CTRL+Tab             -> "\x09"],
            on_key![ALT+Tab              -> "\x1b\x09"],
            on_key![SHIFT+Tab            -> "\x1b[Z"],
            on_key![CTRL,SHIFT+Tab       -> "\x1b[Z"],
            on_key![ALT,SHIFT+Tab        -> "\x1b\x1b[Z"],
            on_key![CTRL,ALT+Tab         -> "\x1b\x09"],
            on_key![Space                -> "\x20"],
            on_key![CTRL+Space           -> "\x00"],
            on_key![ALT+Space            -> "\x1b\x20"],
            on_key![SHIFT+Space          -> "\x20"],
            on_key![CTRL,SHIFT+Space     -> "\x00"],
            on_key![ALT,SHIFT+Space      -> "\x1b\x20"],
            on_key![CTRL,ALT+Space       -> "\x1b\x00"],
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_legacy_non_special_keys() {
        let handler = KeyboardHandler::default();

        let test_cases = vec![
            on_key!["i" -> "i"],
            on_key![SHIFT+"i" -> "I"],
            on_key![ALT+"i" -> "\x1bi"],
            on_key![CTRL+"i" -> "\t"],
            on_key![SHIFT,ALT+"i" -> "\x1bI"],
            on_key![ALT,CTRL+"i" -> "\x1b\t"],
            on_key![CTRL,SHIFT+"i" -> "\x1b[105;6u"],
            on_key!["3" -> "3"],
            on_key![SHIFT+"3" -> "#"],
            on_key![ALT+"3" -> "\x1b3"],
            on_key![CTRL+"3" -> "\x1b"],
            on_key![SHIFT,ALT+"3" -> "\x1b#"],
            on_key![ALT,CTRL+"3" -> "\x1b\x1b"],
            on_key![CTRL,SHIFT+"3" -> "\x1b[51;6u"],
            on_key![F1 -> "\x1b\x4fP"],
            on_key![CTRL+F1 -> "\x1b[1;5P"],
            on_key![ArrowUp -> "\x1b[A"],
            on_key![ALT+ArrowUp -> "\x1b[1;3A"],
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
            on_key![ArrowUp    -> "\x1b\x4fA"],
            on_key![ArrowDown  -> "\x1b\x4fB"],
            on_key![ArrowRight -> "\x1b\x4fC"],
            on_key![ArrowLeft  -> "\x1b\x4fD"],
            on_key![Home       -> "\x1b\x4fH"],
            on_key![End        -> "\x1b\x4fF"],
            // when modifiers are used, CSI form is used
            on_key![ALT,SHIFT+ArrowUp    -> "\x1b[1;4A"],
            on_key![ALT,SHIFT+ArrowDown  -> "\x1b[1;4B"],
            on_key![ALT,SHIFT+ArrowRight -> "\x1b[1;4C"],
            on_key![ALT,SHIFT+ArrowLeft  -> "\x1b[1;4D"],
            on_key![ALT,SHIFT+Home       -> "\x1b[1;4H"],
            on_key![ALT,SHIFT+End        -> "\x1b[1;4F"],
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_progressive_mode_option_is_alt() {
        let mut handler = KeyboardHandler::default();
        handler.option_key_is_meta = true;
        handler.push_progressive_mode(ProgressiveMode::all());

        let test_cases = vec![
            // modifier key, by itself
            on_key![LEFT Alt -> "\x1b[57443;3u"],
            on_key![RIGHT Alt -> "\x1b[57449;3u"],
            // keys with text representation
            on_key!["a" -> "\x1b[97u"],
            on_key![ALT+"a" -> "\x1b[97;3u"],
            on_key![SHIFT,ALT+"a" -> "\x1b[97:65;4u"],
            TestCase::default()
                .with_char(egui::Modifiers::SHIFT, "3", "#")
                .with_expected_output("\x1b[51:35;2;35u"),
            TestCase::default()
                .with_char(egui::Modifiers::SHIFT | egui::Modifiers::ALT, "3", "#")
                .with_expected_output("\x1b[51:35;4;35u"),
            // keys without text
            on_key![ArrowUp    -> "\x1b[A"],
            on_key![ALT,SHIFT+ArrowUp    -> "\x1b[1;4A"],
            on_key![PageDown    -> "\x1b[6~"],
            on_key![CTRL,SHIFT+PageDown    -> "\x1b[6;6~"],
            on_key![F5    -> "\x1b[15~"],
            on_key![CTRL+F5    -> "\x1b[15;5~"],
            on_key![Backspace    -> "\x1b[127u"],
            on_key![SHIFT+Backspace    -> "\x1b[127;2u"],
            on_key![F3 -> "\x1b[13~"],
            on_key![CTRL,SHIFT+F3 -> "\x1b[13;6~"],
            on_key![MediaPlayPause -> "\x1b[57430u"],
        ];

        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_progressive_option_is_not_alt() {
        let mut handler = KeyboardHandler {
            option_key_is_meta: false,
            ..Default::default()
        };
        handler.push_progressive_mode(ProgressiveMode::all());

        let test_cases = vec![
            // modifier key, by itself
            on_key![LEFT Alt -> "\x1b[57443;3u"],
            // keys with text representation
            TestCase::default()
                .with_char(egui::Modifiers::ALT, "a", "å")
                .with_expected_output("\x1b[97;3;229u"),
            TestCase::default()
                .with_char(egui::Modifiers::ALT | egui::Modifiers::SHIFT, "o", "Ø")
                .with_expected_output("\x1b[111:79;4;216u"),
        ];

        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_progressive_mode_disambiguate_escape_codes() {
        let mut handler = KeyboardHandler::default();
        handler.push_progressive_mode(ProgressiveEnhancementFlag::DisambiguateEscapeCodes);

        let test_cases = vec![
            // text producing keys are pass thru
            on_key!["a" -> "a"],
            // esc is special in this mode and generate the CSI u form
            on_key![Escape -> "\x1b[27u"],
            on_key![CTRL,SHIFT+Escape -> "\x1b[27;6u"],
            on_key![ALT,SHIFT+"f" -> "\x1b[102;4u"],
            // Ctrl-c generated CSI u code instead of 0x3
            on_key![CTRL+"c" -> "\x1b[99;5u"],
            // numpad keys are unchanged unless modifiers are involved
            on_key![NUMPAD NONE+"3" -> "3"],
            // on_key![base_case: Key::Character("3".into()), Key::Character("3".into()), Some("3".into()), KeyLocation::Numpad, ElementState::Pressed, NONE -> "3"],
            // on_key![base_case: Key::Character("3".into()), Key::Character("3".into()), Some("3".into()), KeyLocation::Numpad, ElementState::Pressed, ALT -> "\x1b[57402;3u"],
            on_key![NUMPAD ALT+"3" -> "\x1b[57402;3u"],
            // enter, tab and backspace still generate legacy encoding
            TestCase::default()
                .with_named_key(egui::Modifiers::NONE, NamedKey::Enter, "\r")
                .with_expected_output("\x0d"),
            TestCase::default()
                .with_named_key(egui::Modifiers::NONE, NamedKey::Backspace, "\x7f")
                .with_expected_output("\x7f"),
            TestCase::default()
                .with_named_key(egui::Modifiers::NONE, NamedKey::Tab, "\t")
                .with_expected_output("\x09"),
            // but once modifiers are involved, switch to CSI encoding
            TestCase::default()
                .with_named_key(egui::Modifiers::CTRL, NamedKey::Enter, "\r")
                .with_expected_output("\x1b[13;5u"),
            TestCase::default()
                .with_named_key(egui::Modifiers::ALT, NamedKey::Backspace, "\x7f")
                .with_expected_output("\x1b[127;3u"),
            TestCase::default()
                .with_named_key(egui::Modifiers::SHIFT, NamedKey::Tab, "\t")
                .with_expected_output("\x1b[9;2u"),
            // modifier keys by themselves do not generate any output
            on_key![Control -> ""],
            on_key![RIGHT Shift -> ""],
            on_key![Alt -> ""],
            on_key![LEFT Super -> ""],
            on_key![Hyper -> ""],
            // key release events do not generate any output
            TestCase::default()
                .with_char(egui::Modifiers::SHIFT, "9", "(")
                .with_state(ElementState::Released)
                .with_expected_output(""),
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_progressive_report_event_types() {
        let mut handler = KeyboardHandler::default();
        handler.push_progressive_mode(ProgressiveEnhancementFlag::ReportEventTypes);

        let test_cases = vec![
            // keys generate release events
            on_key![released NONE+"b" -> "\x1b[98;1:3u"],
            on_key![released NONE+F1 -> "\x1b[1;1:3P"],
            on_key![released ALT+"q" -> "\x1b[113;3:3u"],
            // keys generate repeat events
            on_key![repeated NONE+"b" -> "\x1b[98;1:2u"],
            on_key![repeated NONE+F1 -> "\x1b[1;1:2P"],
            on_key![repeated ALT+"q" -> "\x1b[113;3:2u"],
            // enter, tab, backspace do not generate release or repeat events
            on_key![released NONE+Enter -> ""],
            on_key![released NONE+Tab -> ""],
            on_key![released NONE+Backspace -> ""],
            on_key![repeated NONE+Enter -> "\x0d"],
            on_key![repeated NONE+Tab -> "\x09"],
            on_key![Backspace -> "\x7f"],
            on_key![repeated NONE+Backspace -> "\x7f"],
        ];

        for test_case in test_cases {
            test_case.run(&handler);
        }
    }

    #[test]
    fn test_progressive_report_alternate_keys() {
        let mut handler = KeyboardHandler::default();
        handler.option_key_is_meta = false;
        handler.push_progressive_mode(ProgressiveEnhancementFlag::ReportAlternateKeys);

        let test_cases = vec![
            // shift a -> A by itself does not generate control codes
            TestCase::default()
                .with_char(egui::Modifiers::SHIFT, "a", "A")
                .with_expected_output("A"),
            // mac's alt keys that send unicode works
            TestCase::default()
                .with_char(egui::Modifiers::ALT, "'", "æ")
                .with_expected_output("æ"),
            TestCase::default()
                .with_char(egui::Modifiers::ALT | egui::Modifiers::SHIFT, "'", "Æ")
                .with_expected_output("Æ"),
            // but does with other modifiers
            TestCase::default()
                .with_char(egui::Modifiers::SHIFT | egui::Modifiers::CTRL, "a", "A")
                .with_expected_output("\x1b[97:65;6u"),
            TestCase::default()
                .with_char(egui::Modifiers::SHIFT | egui::Modifiers::ALT, "y", "Y")
                .with_expected_output("\x1b[121:89;4u"),
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }
}
