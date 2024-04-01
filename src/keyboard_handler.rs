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
            self.legacy_mode(ev, modifiers, output)
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
        ev: &winit::event::KeyEvent,
        mut modifiers: egui::Modifiers,
        output: &mut String,
    ) -> anyhow::Result<bool> {
        if !ev.state.is_pressed() {
            // legacy mode can only handle press event and not release
            return Ok(false);
        }

        let mut logical_key = ev.logical_key.clone();

        if modifiers.alt && self.option_key_is_meta {
            // ignore the special non-ascii char
            logical_key = ev.key_without_modifiers();
        } else {
            // option_key_is_meta turns the case where there is only alt to a no modifier case
            modifiers.alt = false;
        }

        // - printable chars without any modifiers, like 'a', 'b', '[' etc
        // - non-printable chars like backspace, tab, escape, F1 etc.
        if modifiers.is_none() {
            output.push_str(handle_logical_key(&logical_key, self.cursor_key_mode).as_str());
            return Ok(true);
        }

        // alt always causes an ESC
        if modifiers.alt {
            output.push_str("\x1b");
        }

        let Some(normalized_ascii_byte) = logical_key_to_ascii(&logical_key) else {
            return Ok(false);
        };

        if modifiers.ctrl {
            if let Some(ctrl_mapping) = legacy_ctrl_mapping(normalized_ascii_byte) {
                output.push(ctrl_mapping as char);
                return Ok(true);
            }
        }

        if modifiers.shift {
            if let Some(shifted) = shifted_lut(normalized_ascii_byte) {
                output.push_str(&shifted);
                return Ok(true);
            }
        }

        Ok(false)
    }
}

fn logical_key_to_ascii(logical_key: &winit::keyboard::Key) -> Option<u8> {
    use winit::keyboard::Key::*;
    use winit::keyboard::NamedKey::*;
    match logical_key {
        Named(Enter) => b'\x0d'.into(),
        Named(Tab) => b'\x09'.into(),
        Named(Space) => b'\x20'.into(),
        Named(Backspace) => b'\x7f'.into(),
        Named(Escape) => b'\x1b'.into(),
        Character(ch) if ch.is_ascii() => ch.as_bytes().first().copied(),
        _ => None,
    }
}

fn handle_logical_key(logical_key: &winit::keyboard::Key, cursor_key_mode: bool) -> SmolStr {
    use winit::keyboard::Key::*;
    use winit::keyboard::NamedKey::*;
    match logical_key {
        Named(Enter) => "\x0d".into(),
        Named(Tab) => "\x09".into(),
        Named(Space) => "\x20".into(),
        Named(Backspace) => "\x7f".into(),
        Named(Insert) => "\x1b[2~".into(),
        Named(Delete) => "\x1b[3~".into(),
        Named(Escape) => "\x1b".into(),

        Named(ArrowUp) if cursor_key_mode => "\x1bOA".into(),
        Named(ArrowDown) if cursor_key_mode => "\x1bOB".into(),
        Named(ArrowRight) if cursor_key_mode => "\x1bOC".into(),
        Named(ArrowLeft) if cursor_key_mode => "\x1bDD".into(),
        Named(ArrowUp) => "\x1b[A".into(),
        Named(ArrowDown) => "\x1b[B".into(),
        Named(ArrowRight) => "\x1b[C".into(),
        Named(ArrowLeft) => "\x1b[D".into(),

        Named(Home) if cursor_key_mode => "\x1bOH".into(),
        Named(Home) => "\x1b[H".into(),
        Named(End) if cursor_key_mode => "\x1bOF".into(),
        Named(End) => "\x1b[F".into(),
        Named(PageUp) => "\x1b[5~".into(),
        Named(PageDown) => "\x1b[6~".into(),

        Named(F1) => "\x1bOP".into(),
        Named(F2) => "\x1bOQ".into(),
        Named(F3) => "\x1bOR".into(),
        Named(F4) => "\x1bOS".into(),
        Named(F5) => "\x1b[15~".into(),
        Named(F6) => "\x1b[17~".into(),
        Named(F7) => "\x1b[18~".into(),
        Named(F8) => "\x1b[19~".into(),
        Named(F9) => "\x1b[20~".into(),
        Named(F10) => "\x1b[21~".into(),
        Named(F11) => "\x1b[23~".into(),
        Named(F12) => "\x1b[24~".into(),
        Named(ContextMenu) => "\x1b[29~".into(),

        Character(txt) => txt.clone(),
        Named(_) | Unidentified(_) | Dead(_) => "".into(),
    }
}

fn shifted_lut(normalized: u8) -> Option<winit::keyboard::SmolStr> {
    match normalized as char {
        '`' => Some(SmolStr::new_static("~")),
        '1' => Some(SmolStr::new_static("!")),
        '2' => Some(SmolStr::new_static("@")),
        '3' => Some(SmolStr::new_static("#")),
        '4' => Some(SmolStr::new_static("$")),
        '5' => Some(SmolStr::new_static("%")),
        '6' => Some(SmolStr::new_static("^")),
        '7' => Some(SmolStr::new_static("&")),
        '8' => Some(SmolStr::new_static("*")),
        '9' => Some(SmolStr::new_static("(")),
        '0' => Some(SmolStr::new_static(")")),
        '-' => Some(SmolStr::new_static("_")),
        '=' => Some(SmolStr::new_static("+")),
        '[' => Some(SmolStr::new_static("{")),
        ']' => Some(SmolStr::new_static("}")),
        '\\' => Some(SmolStr::new_static("|")),
        ';' => Some(SmolStr::new_static(":")),
        '\'' => Some(SmolStr::new_static("\"")),
        ',' => Some(SmolStr::new_static("<")),
        '.' => Some(SmolStr::new_static(">")),
        '/' => Some(SmolStr::new_static("?")),
        ch @ 'a'..='z' => Some(ch.to_uppercase().to_string().into()),
        _ => None,
    }
}

// See: https://sw.kovidgoyal.net/kitty/keyboard-protocol/#id10
fn legacy_ctrl_mapping(mut ch: u8) -> Option<u8> {
    if matches!(ch, b'a'..=b'z') {
        ch.make_ascii_uppercase();
    }
    Some(match ch {
        b' ' => 0,
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
    })
}
