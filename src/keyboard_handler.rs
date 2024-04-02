use anyhow::Result;
use winit::{keyboard::SmolStr, platform::modifier_supplement::KeyEventExtModifierSupplement};

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
            winit::keyboard::Key::Character(_) => {
                output.push_str(encode_logical_key(&logical_key, &modifiers).as_str());
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}

fn encode_logical_key(logical_key: &winit::keyboard::Key, modifiers: &egui::Modifiers) -> SmolStr {
    let mut output = String::new();
    // alt always causes an ESC
    if modifiers.alt {
        output.push_str("\x1b");
    }

    let Some(normalized_ascii_byte) = logical_key
        .to_text()
        .and_then(|txt| txt.as_bytes().first().copied())
    else {
        return "".into();
    };

    if modifiers.ctrl {
        if let Some(ctrl_mapping) = legacy_ctrl_mapping(normalized_ascii_byte, modifiers.shift) {
            output.push_str(ctrl_mapping.as_str());
            return output.into();
        }
    }

    if modifiers.shift {
        if let Some(shifted) = shifted_lut(normalized_ascii_byte) {
            output.push_str(&shifted);
            return output.into();
        }
    }

    output.push(normalized_ascii_byte as char);
    output.into()
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

    let encoded_modifiers = modifiers.any().then_some(encode_modifiers(modifiers));
    // CSI number ; modifier ~, where modifier is optional
    let csi_num = |num: u8| -> SmolStr {
        if let Some(mods) = encoded_modifiers.as_ref() {
            format!("\x1b[{num};{mods}~").into()
        } else {
            format!("\x1b[{num}~").into()
        }
    };
    // CSI 1 ; modifier {ABCDEFHPQS}, where modifier is optional
    let csi_letter = |letter: char| -> SmolStr {
        if let Some(mods) = encoded_modifiers.as_ref() {
            format!("\x1b[1;{mods}{letter}").into()
        } else {
            format!("\x1b[{letter}").into()
        }
    };
    // SS3 {PQRS}
    let ss3_letter = |letter: char| -> SmolStr {
        if let Some(mods) = encoded_modifiers.as_ref() {
            format!("\x1b[1;{mods}{letter}").into()
        } else {
            format!("\x1b\x4f{letter}").into()
        }
    };

    let c0_special_handling = || -> SmolStr {
        let logical_key = winit::keyboard::Key::Named(*functional_key);
        encode_logical_key(&logical_key, modifiers)
    };

    match functional_key {
        Enter | Tab | Space | Backspace | Escape => c0_special_handling(),

        Insert => csi_num(2),
        Delete => csi_num(3),

        ArrowUp if cursor_key_mode => "\x1bOA".into(),
        ArrowDown if cursor_key_mode => "\x1bOB".into(),
        ArrowRight if cursor_key_mode => "\x1bOC".into(),
        ArrowLeft if cursor_key_mode => "\x1bDD".into(),
        Home if cursor_key_mode => "\x1bOH".into(),
        End if cursor_key_mode => "\x1bOF".into(),

        ArrowUp => csi_letter('A'),
        ArrowDown => csi_letter('B'),
        ArrowRight => csi_letter('C'),
        ArrowLeft => csi_letter('D'),

        Home => csi_letter('H'),
        End => csi_letter('F'),
        PageUp => csi_num(5),
        PageDown => csi_num(6),

        F1 => ss3_letter('P'),
        F2 => ss3_letter('Q'),
        F3 => ss3_letter('R'),
        F4 => ss3_letter('S'),
        F5 => csi_num(15),
        F6 => csi_num(17),
        F7 => csi_num(18),
        F8 => csi_num(19),
        F9 => csi_num(20),
        F10 => csi_num(21),
        F11 => csi_num(23),
        F12 => csi_num(24),
        ContextMenu => csi_num(29),
        _ => "".into(),
    }
}

fn shifted_lut(normalized: u8) -> Option<winit::keyboard::SmolStr> {
    match normalized {
        b'\x09' => Some(SmolStr::new_static("\x1b[Z")),
        b'\x20' | b'\x0d' | b'\x1b' | b'\x7f' => Some(SmolStr::new_inline(unsafe {
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
        b'\x7f' => b'\x08',
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
            on_press![Backspace            -> "\x7f"],
            on_press![CTRL+Backspace       -> "\x08"],
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
            on_press![ALT+ArrowUp -> "\x1b[1;3A"],
        ];
        for test_case in test_cases {
            test_case.run(&handler);
        }
    }
}
