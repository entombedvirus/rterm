use anyhow::Result;

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
    progressive_mode_stack: Vec<ProgressiveMode>,
}

impl Default for KeyboardHandler {
    fn default() -> Self {
        Self {
            cursor_key_mode: false,
            progressive_mode_stack: vec![],
        }
    }
}

impl KeyboardHandler {
    pub fn on_keyboard_event(&self, ev: &egui::Event, output: &mut String) -> anyhow::Result<()> {
        log::trace!("logical on_keyboard_event, got ev: {ev:?}");
        if let egui::Event::Key {
            key,
            modifiers,
            pressed: true,
            ..
        } = ev
        {
            // See: https://sw.kovidgoyal.net/kitty/keyboard-protocol/#legacy-text-keys
            if modifiers.alt {
                output.push('\x1b');
            }
            if modifiers.ctrl {
                if let Some(ctrl_mapping) = legacy_ctrl_mapping(*key) {
                    output.push(ctrl_mapping as char);
                    return Ok(());
                }
            } else if modifiers.shift {
                // todo
            }
            Ok(())
        } else {
            anyhow::bail!(
                "on_keyboard_event can only handle egui::Event::Key events, but got: {ev:?}"
            );
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
        todo!()
    }

    pub fn progressive_mode_get_flags(&self, output: &mut String) {
        todo!()
    }
}

// See: https://sw.kovidgoyal.net/kitty/keyboard-protocol/#id10
fn legacy_ctrl_mapping(key: egui::Key) -> Option<u8> {
    use egui::Key::*;
    Some(match key {
        Space => 0,
        Slash => 31,
        Num0 => 48,
        Num1 => 49,
        Num2 => 0,
        Num3 => 27,
        Num4 => 28,
        Num5 => 29,
        Num6 => 30,
        Num7 => 31,
        Num8 => 127,
        Num9 => 57,
        Questionmark => 127,
        // @ is not supported in egui::Key
        OpenBracket => 27,
        Backslash => 28,
        CloseBracket => 29,
        // ^ is not supported in egui::Key
        // _ is not supported in egui::Key
        A => 1,
        B => 2,
        C => 3,
        D => 4,
        E => 5,
        F => 6,
        G => 7,
        H => 8,
        I => 9,
        J => 10,
        K => 11,
        L => 12,
        M => 13,
        N => 14,
        O => 15,
        P => 16,
        Q => 17,
        R => 18,
        S => 19,
        T => 20,
        U => 21,
        V => 22,
        W => 23,
        X => 24,
        Y => 25,
        Z => 26,
        _ => return None,
    })
}

fn key_to_ascii(key: egui::Key) -> u8 {
    use egui::Key::*;
    match key {
        // @ is missing
        A => todo!(),
        B => todo!(),
        C => todo!(),
        D => todo!(),
        E => todo!(),
        F => todo!(),
        G => todo!(),
        H => todo!(),
        I => todo!(),
        J => todo!(),
        K => todo!(),
        L => todo!(),
        M => todo!(),
        N => todo!(),
        O => todo!(),
        P => todo!(),
        Q => todo!(),
        R => todo!(),
        S => todo!(),
        T => todo!(),
        U => todo!(),
        V => todo!(),
        W => todo!(),
        X => todo!(),
        Y => todo!(),
        Z => todo!(),
        OpenBracket => todo!(),
        Backslash => todo!(),
        CloseBracket => todo!(),
        // ^ is missing
        // _ underscore is missing
        Backtick => todo!(),

        Escape => todo!(),
        Tab => todo!(),
        Backspace => todo!(),
        Enter => todo!(),
        Space => todo!(),
        Colon => todo!(),
        Comma => todo!(),
        Slash => todo!(),
        Pipe => todo!(),
        Questionmark => todo!(),
        Minus => todo!(),
        Period => todo!(),
        Plus => todo!(),
        Equals => todo!(),
        Semicolon => todo!(),
    }
}
