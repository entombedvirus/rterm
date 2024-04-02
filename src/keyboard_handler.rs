use std::fmt::Write;

use anyhow::{Context, Result};
use glow::SHORT;
use winit::{
    event::Modifiers, keyboard::SmolStr,
    platform::modifier_supplement::KeyEventExtModifierSupplement,
};

#[repr(u8)]
#[derive(Debug, Default)]
enum ProgressiveEnhancementFlag {
    #[default]
    DisambiguateEscapeCodes = 0b1,
    ReportEventTypes = 0b10,
    ReportAlternateKeys = 0b100,
    ReportAllKeysAsEscapeCodes = 0b1000,
    ReportAssociatedText = 0b10000,
}

#[derive(Debug)]
pub struct ProgressiveMode(pub u8);

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
        }
    }
}

fn format_normalized_byte(
    normalized_ascii_byte: u8,
    f: &mut std::fmt::Formatter<'_>,
    modifiers: &egui::Modifiers,
) -> std::fmt::Result {
    if !supported_in_legacy_mode(
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
        if supported_in_legacy_mode(false, mods) {
            let modifiers = mods.any().then(|| encode_modifiers(mods));
            Self::CsiTilde { num, modifiers }
        } else {
            Self::csi_unicode(num as u32, mods)
        }
    }

    fn csi_letter(letter: char, mods: &egui::Modifiers) -> Self {
        if supported_in_legacy_mode(false, mods) {
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
}

fn supported_in_legacy_mode(is_c0_special: bool, mods: &egui::Modifiers) -> bool {
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
    // cases this needs to handle:
    //
    // - printable chars without any modifiers, like 'a', 'b', '[' etc
    // - non-printable chars like backspace and escape
    // - option + key which should translate to option + logical_key if option_as_meta is turned on
    // - modifiers: any combination of ctrl, alt, shift plus another key
    // - pressed vs released vs repeat if in progressive handling mode
    pub fn on_keyboard_event(
        &self,
        ev: &winit::event::KeyEvent,
        modifiers: egui::Modifiers,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        if self.is_in_progressive_mode() {
            self.progressive_mode(ev, modifiers, output)
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
        todo!()
    }

    pub fn progressive_mode_pop(&mut self, num: u8) {
        todo!()
    }

    pub fn set_cursor_keys_mode(&mut self, on_or_off: bool) {
        self.cursor_key_mode = on_or_off;
    }

    pub fn progressive_mode_get_flags(&self, output: &mut String) {
        todo!()
    }

    fn is_in_progressive_mode(&self) -> bool {
        !self.progressive_mode_stack.is_empty()
    }

    fn progressive_mode(
        &self,
        ev: &winit::event::KeyEvent,
        modifiers: egui::Modifiers,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        todo!()
    }

    fn legacy_mode(
        &self,
        mut logical_key: winit::keyboard::Key,
        key_without_modifiers: winit::keyboard::Key,
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
            winit::keyboard::Key::Named(functional_key) => {
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
            winit::keyboard::Key::Character(ch) => {
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
        Enter | Tab | Space | Backspace | Escape => c0(functional_key
            .to_text()
            .and_then(|s| s.as_bytes().first())
            .copied()
            .expect("these keys have ascii representation")),

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
