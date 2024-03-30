use anyhow::Context;
use bytes::{Buf, BytesMut};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct Parser {
    stream: BytesMut,
    pub(crate) parsed_tokens: VecDeque<AnsiToken>,
}

impl Parser {
    pub fn new() -> Self {
        let stream = BytesMut::new();
        let parsed_tokens = VecDeque::new();
        Self {
            stream,
            parsed_tokens,
        }
    }

    pub fn tokens<'a>(&'a mut self) -> VecDeque<AnsiToken> {
        std::mem::take(&mut self.parsed_tokens)
    }

    pub fn push_bytes(&mut self, incoming: &[u8]) {
        self.stream.extend_from_slice(incoming);

        // Things to handle:
        // - incomplete utf-8 sequence
        // - invalid utf-8 sequence
        // - incomplete escape sequence
        // - invalid escape sequence
        //
        // in case of incomplete cases, leave the trailing bytes alone in `stream` so that there is
        // a chance that the next read will bring in more bytes to complete the sequence.
        //
        // in case of invalid cases, push a AnsiToken::Unknown and continue parsing.
        let mut buf = self.stream.as_ref();
        while let Some((rem, token)) =
            parse_normal_text(buf).or_else(|| parse_control_sequence(buf))
        {
            self.parsed_tokens.push_back(token);
            buf = rem;
        }

        let consumed = self.stream.len() - buf.len();
        if consumed == 0 && self.stream.len() > 10 {
            log::warn!(
                "stream is not making progress: {stream:?}",
                stream = self.stream
            )
        }
        self.stream.advance(consumed);
    }

    pub(crate) fn has_tokens(&self) -> bool {
        !self.parsed_tokens.is_empty()
    }
}

fn parse_csi_escape_sequence(buf: &[u8]) -> Option<(&[u8], AnsiToken)> {
    use CursorControl::*;
    use EraseControl::*;
    let params: String = buf
        .iter()
        .map(|b| *b as char)
        .take_while(|ch| matches!(*ch, '\x30'..='\x3f' | '\x20'..='\x2f'))
        .collect();
    match &buf[params.len()..] {
        [] => None, // need more data
        [b'A', rem @ ..] => {
            let lines: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveUp { lines })))
        }
        [b'B', rem @ ..] => {
            let lines: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveDown { lines })))
        }
        [b'C', rem @ ..] => {
            let cols: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveRight { cols })))
        }
        [b'D', rem @ ..] => {
            let cols: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveLeft { cols })))
        }
        [b'E', rem @ ..] => {
            let lines: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveLineBeginDown { lines })))
        }
        [b'F', rem @ ..] => {
            let lines: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveLineBeginUp { lines })))
        }
        [b'G', rem @ ..] => {
            let col: usize = extract_param(&params, 0).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveColumn { col })))
        }
        [b'H', rem @ ..] => {
            let line: usize = extract_param(&params, 0).unwrap_or(1);
            let col: usize = extract_param(&params, 1).unwrap_or(1);
            Some((rem, AnsiToken::CursorControl(MoveTo { line, col })))
        }
        [b'J', rem @ ..] => Some((
            rem,
            match extract_param(&params, 0).unwrap_or(0) {
                0 => AnsiToken::EraseControl(FromCursorToEndOfScreen),
                1 => AnsiToken::EraseControl(FromCursorToStartOfScreen),
                2 => AnsiToken::EraseControl(Screen),
                3 => AnsiToken::EraseControl(ScreenAndScrollback),
                _unknown => AnsiToken::Unknown(format!("\u{1b}[{params}J")),
            },
        )),
        [b'K', rem @ ..] => Some((
            rem,
            match extract_param(&params, 0).unwrap_or(0) {
                0 => AnsiToken::EraseControl(FromCursorToEndOfLine),
                1 => AnsiToken::EraseControl(FromCursortoStartOfLine),
                2 => AnsiToken::EraseControl(Line),
                _unknown => AnsiToken::Unknown(format!("\u{1b}[{params}K")),
            },
        )),
        [b'c', rem @ ..] => Some((rem, {
            match params.as_str() {
                "" | "0" => AnsiToken::DA(DeviceAttributes::Primary),
                ">" => AnsiToken::DA(DeviceAttributes::Secondary),
                _unknown => AnsiToken::DA(DeviceAttributes::Unknown(params)),
            }
        })),
        [b'm', rem @ ..] => Some((rem, {
            if params.is_empty() {
                return Some((rem, AnsiToken::SGR(vec![SgrControl::Reset])));
            }
            if let Some(color_idx_str) = params.strip_prefix("38;5;") {
                // 256 color mode fg color
                let color_idx: u8 = color_idx_str.parse().unwrap_or_default();
                AnsiToken::SGR(vec![SgrControl::ForgroundColor(Color::Indexed(color_idx))])
            } else if let Some(color_idx_str) = params.strip_prefix("48;5;") {
                // 256 color mode bg color
                let color_idx: u8 = color_idx_str.parse().unwrap_or_default();
                AnsiToken::SGR(vec![SgrControl::BackgroundColor(Color::Indexed(color_idx))])
            } else if let Some(rgb_str) = params.strip_prefix("38;2;") {
                // 24 bit color mode fg color
                let bg_color = Color::from_components_str(rgb_str);
                AnsiToken::SGR(vec![bg_color
                    .map(SgrControl::ForgroundColor)
                    .unwrap_or(SgrControl::Unimplemented(params))])
            } else if let Some(rgb_str) = params.strip_prefix("48;2;") {
                // 24 bit color mode bg color
                let bg_color = Color::from_components_str(rgb_str);
                AnsiToken::SGR(vec![bg_color
                    .map(SgrControl::BackgroundColor)
                    .unwrap_or(SgrControl::Unimplemented(params))])
            } else if params == "39" {
                AnsiToken::SGR(vec![SgrControl::ResetFgColor])
            } else if params == "49" {
                AnsiToken::SGR(vec![SgrControl::ResetBgColor])
            } else {
                // 16 color mode
                let params = params.split(';').map(|p| {
                    p.parse::<usize>()
                        .map_err(|err| anyhow::format_err!("sgr parse error: {err}"))
                        .and_then(SgrControl::from_params)
                        .unwrap_or(SgrControl::Unimplemented(format!("\u{1b}[{p}m")))
                });
                AnsiToken::SGR(params.collect())
            }
        })),
        [b'h', rem @ ..] => Some((
            rem,
            match params.as_str() {
                "?1" => AnsiToken::ModeControl(ModeControl::CursorKeysEnter),
                "?2004" => AnsiToken::ModeControl(ModeControl::BracketedPasteEnter),
                "?1049" => AnsiToken::ModeControl(ModeControl::AlternateScreenEnter),
                "?1004" => AnsiToken::ModeControl(ModeControl::FocusTrackEnter),
                "?7727" => AnsiToken::ModeControl(ModeControl::ApplicationEscEnter),
                _unknown => AnsiToken::ModeControl(ModeControl::Unknown(params)),
            },
        )),
        [b'l', rem @ ..] => Some((
            rem,
            match params.as_str() {
                "?1" => AnsiToken::ModeControl(ModeControl::CursorKeysExit),
                "?2004" => AnsiToken::ModeControl(ModeControl::BracketedPasteExit),
                "?1049" => AnsiToken::ModeControl(ModeControl::AlternateScreenExit),
                "?1004" => AnsiToken::ModeControl(ModeControl::FocusTrackExit),
                "?7727" => AnsiToken::ModeControl(ModeControl::ApplicationEscExit),
                _unknown => AnsiToken::ModeControl(ModeControl::Unknown(params)),
            },
        )),
        // \e[>0q
        [b'q', rem @ ..] => Some((
            rem,
            match params.as_str() {
                ">" | ">0" => AnsiToken::DA(DeviceAttributes::XtVersion),
                _unknown => AnsiToken::DA(DeviceAttributes::Unknown(params)),
            },
        )),
        // progressive keyboard
        [b'u', rem @ ..] => Some((rem, {
            AnsiToken::PKC(match params.split_at(1) {
                ("=", args) => {
                    if let Some(flags) = extract_param(args, 0) {
                        let set_mode = extract_param(args, 1).unwrap_or(1);
                        ProgressiveKeyboardControl::SetFlags { flags, set_mode }
                    } else {
                        ProgressiveKeyboardControl::Unknown(params)
                    }
                }
                ("?", "") => ProgressiveKeyboardControl::QueryFlags,
                (">", args) => ProgressiveKeyboardControl::PushFlags {
                    flags: extract_param(args, 0).unwrap_or(0),
                },
                ("<", args) => ProgressiveKeyboardControl::PopFlags {
                    num: extract_param(args, 0).unwrap_or(1),
                },
                _ => ProgressiveKeyboardControl::Unknown(params),
            })
        })),

        [unknown, rem @ ..] => Some((
            rem,
            AnsiToken::Unknown(format!("\u{1b}[{params}{ch}", ch = *unknown as char)),
        )),
    }
}

fn parse_osc_escape_sequence(mut buf: &[u8]) -> Option<(&[u8], AnsiToken)> {
    let mut ctrl_buf = Vec::new();
    let mut terminated = false;
    while let Some((&b, rem)) = buf.split_first() {
        buf = rem;
        match b {
            0x07 => {
                terminated = true;
                break;
            }
            0x1b => {
                if let Some(0x5c) = rem.get(0) {
                    terminated = true;
                    buf = &buf[1..];
                    break;
                } else {
                    ctrl_buf.push(0x1b);
                }
            }
            _ => ctrl_buf.push(b),
        }
    }
    if !terminated {
        // reached EOF without seeing terminating seq. try again
        return None;
    }
    let token = match ctrl_buf.as_slice() {
        [] => AnsiToken::OSC(OscControl::Reset),
        // ESC ]0;this is the window title BEL
        [b'0', b';', title_bytes @ ..] => AnsiToken::OSC(OscControl::SetWindowTitle(
            String::from_utf8_lossy(title_bytes).to_string(),
        )),
        // [2024-03-24T19:58:46Z DEBUG rterm::terminal_input] read 56 bytes from pty: Ok("\u{1b}[?1049h\u{1b}[H\u{1b}[2J\u{1b}[?2004h\u{1b}[1;1H\u{1b}[c\u{1b}[>c\u{1b}[>q\u{1b}]10;?\u{1b }\\\u{1b}]11;?\u{1b}\\")
        // [2024-03-24T19:58:46Z DEBUG rterm::terminal_input] parsed_tokens: [ModeControl(AlternateScreenEnter), CursorControl(MoveTo { line: 1, col: 1 }), EraseControl(Screen), ModeCont rol(BracketedPasteEnter), CursorControl(MoveTo { line: 1, col: 1 }), Unknown("\u{1b}[c"), Unknown("\u{1b}[>c"), Unknown("\u{1b}[>q"), OSC(Unknown([49, 48, 59, 63])), OSC(Unkno wn([49, 49, 59, 63]))]
        [b'1', b'0', b';', b'?'] => AnsiToken::OSC(OscControl::GetDefaultFgColor),
        [b'1', b'1', b';', b'?'] => AnsiToken::OSC(OscControl::GetDefaultBgColor),
        _ => AnsiToken::OSC(OscControl::Unknown(ctrl_buf)),
    };

    Some((buf, token))
}

fn parse_escape_sequence(buf: &[u8]) -> Option<(&[u8], AnsiToken)> {
    use CursorControl::*;
    match buf {
        [b'[', rem @ ..] => parse_csi_escape_sequence(rem),
        [b']', rem @ ..] => parse_osc_escape_sequence(rem),
        [b'M', rem @ ..] => Some((rem, AnsiToken::CursorControl(ScrollUpFromHome))),
        [b'7', rem @ ..] => Some((rem, AnsiToken::CursorControl(SavePositionDEC))),
        [b'8', rem @ ..] => Some((rem, AnsiToken::CursorControl(RestorePositionDEC))),
        [b'c', rem @ ..] => Some((rem, AnsiToken::ResetToInitialState)),
        [other, rem @ ..] => Some((rem, AnsiToken::Unknown(format!("\u{1b}{other}")))),
        _ => None,
    }
}

fn parse_control_sequence(buf: &[u8]) -> Option<(&[u8], AnsiToken)> {
    match buf {
        [b'\x1b', rem @ ..] => parse_escape_sequence(rem),
        [first_byte, rem @ ..] if first_byte.is_ascii_control() => {
            let token = AnsiToken::AsciiControl(
                AsciiControl::try_from(*first_byte).expect("already checked"),
            );
            Some((rem, token))
        }
        _other => None,
    }
}

fn parse_normal_text(buf: &[u8]) -> Option<(&[u8], AnsiToken)> {
    let segment = buf.split(|ch: &u8| ch.is_ascii_control()).next()?;
    if segment.is_empty() {
        return None;
    }
    match std::str::from_utf8(segment) {
        Ok(txt) => Some((&buf[segment.len()..], AnsiToken::Text(txt.to_string()))),
        Err(err) => {
            match err.error_len() {
                Some(invalid_len) => {
                    // invalid utf-8 in the middle
                    //  - assemble that valid bytes, add a repalcement char and then return the
                    //  remainder as unprocessed.
                    let mut txt = String::new();
                    let (valid, after_valid) = segment.split_at(err.valid_up_to());
                    txt.push_str(unsafe { std::str::from_utf8_unchecked(valid) });
                    txt.push(std::char::REPLACEMENT_CHARACTER);
                    Some((&after_valid[invalid_len..], AnsiToken::Text(txt)))
                }
                None => {
                    // incomplete multi-byte utf8 sequence
                    // wait for more data
                    None
                }
            }
        }
    }
}

fn extract_param<'a, N: std::str::FromStr>(params: &'a str, num: usize) -> Option<N> {
    let p = params.split(';').nth(num)?;
    if p.is_empty() {
        None
    } else {
        p.parse().ok()
    }
}

// See: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnsiToken {
    ResetToInitialState,
    Text(String),
    AsciiControl(AsciiControl),
    CursorControl(CursorControl),
    EraseControl(EraseControl),
    SGR(Vec<SgrControl>),
    OSC(OscControl),
    DA(DeviceAttributes),
    ModeControl(ModeControl),
    PKC(ProgressiveKeyboardControl),
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeviceAttributes {
    XtVersion,
    Primary,
    Secondary,
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModeControl {
    BracketedPasteEnter,
    BracketedPasteExit,
    AlternateScreenEnter,
    AlternateScreenExit,
    FocusTrackEnter,
    FocusTrackExit,
    ApplicationEscEnter,
    ApplicationEscExit,
    CursorKeysEnter,
    CursorKeysExit,
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgressiveKeyboardControl {
    SetFlags { flags: u8, set_mode: u8 },
    QueryFlags,
    PushFlags { flags: u8 },
    PopFlags { num: u8 },
    Unknown(String),
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorControl {
    MoveTo { line: usize, col: usize },
    MoveUp { lines: usize },
    MoveDown { lines: usize },
    MoveRight { cols: usize },
    MoveLeft { cols: usize },
    MoveLineBeginDown { lines: usize },
    MoveLineBeginUp { lines: usize },
    MoveColumn { col: usize },
    RequestPosition,
    ScrollUpFromHome,
    SavePositionDEC,
    RestorePositionDEC,
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraseControl {
    FromCursorToEndOfScreen,
    FromCursorToStartOfScreen,
    Screen,
    ScreenAndScrollback,
    FromCursorToEndOfLine,
    FromCursortoStartOfLine,
    Line,
}

#[allow(unused)]
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SgrControl {
    Reset = 0,
    Bold,
    EnterItalicsMode = 3,
    ExitItalicsMode = 23,
    ForgroundColor(Color),
    BackgroundColor(Color),
    ResetFgColor = 39,
    ResetBgColor = 49,
    Unimplemented(String),
}
impl SgrControl {
    pub fn from_params(param: usize) -> anyhow::Result<SgrControl> {
        use SgrControl::*;
        match param {
            0 => Ok(Reset),
            1 => Ok(Bold),
            3 => Ok(EnterItalicsMode),
            23 => Ok(ExitItalicsMode),
            num @ (30..=37 | 39 | 90..=97) => Ok(ForgroundColor(
                Color::from_sgr_num(num)
                    .with_context(|| format!("invalid SGR foreground color param: {num}"))?,
            )),
            num @ (40..=47 | 49 | 100..=107) => Ok(BackgroundColor(
                Color::from_sgr_num(num)
                    .with_context(|| format!("invalid SGR background color param: {num}"))?,
            )),
            _unknown => Err(anyhow::format_err!("unknown sgr param: {param}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OscControl {
    SetWindowTitle(String),
    Unknown(Vec<u8>),
    Reset,
    GetDefaultFgColor,
    GetDefaultBgColor,
}

#[allow(unused)]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AsciiControl {
    NULL = 0,
    StartOfHeading,
    StartOfText,
    EndOfText,
    EndOfTransmission,
    Enquiry,
    Ack,
    Bell,
    Backspace,
    Tab,
    LineFeed,
    VerticalTab,
    FormFeed,
    CarriageReturn,
    ShiftOut,
    ShiftIn,
    DataLinkEscape,
    DeviceControl1,
    DeviceControl2,
    DeviceControl3,
    DeviceControl4,
    NegativeAck,
    SynchronousIdle,
    EndOfTransmissionBlock,
    Cancel,
    EndOfMedium,
    Substitute,
    Escape,
    FileSeparator,
    GroupSeparator,
    RecordSeparator,
    UnitSeparator,
    Delete = 127,
}

impl TryFrom<char> for AsciiControl {
    type Error = anyhow::Error;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        u8::try_from(value)
            .map_err(|err| anyhow::format_err!("char is not ascii: {err}"))
            .and_then(AsciiControl::try_from)
    }
}

impl TryFrom<u8> for AsciiControl {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value.is_ascii_control() {
            // SAFETY: enum is repr(u8) and range is mapped exactly to the one checked in
            // is_ascii_control
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(anyhow::anyhow!("invalid ascii control char: {value}"))
        }
    }
}

impl From<AsciiControl> for char {
    fn from(value: AsciiControl) -> Self {
        value as u8 as char
    }
}

impl Iterator for Parser {
    type Item = AnsiToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.parsed_tokens.pop_front()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
    DefaultFg,
    DefaultBg,
    Indexed(u8),
    TrueColor(u8, u8, u8),
}

static COLOR_LUT: [egui::Color32; 256] = Color::initialize_indexed_lut();

impl Color {
    fn from_sgr_num(num: usize) -> Option<Color> {
        Some(match num {
            30 | 40 => Color::Black,
            31 | 41 => Color::Red,
            32 | 42 => Color::Green,
            33 | 43 => Color::Yellow,
            34 | 44 => Color::Blue,
            35 | 45 => Color::Magenta,
            36 | 46 => Color::Cyan,
            37 | 47 => Color::White,
            39 => Color::DefaultFg,
            49 => Color::DefaultBg,
            90 | 100 => Color::BrightBlack,
            91 | 101 => Color::BrightRed,
            92 | 102 => Color::BrightGreen,
            93 | 103 => Color::BrightYellow,
            94 | 104 => Color::BrightBlue,
            95 | 105 => Color::BrightMagenta,
            96 | 106 => Color::BrightCyan,
            97 | 107 => Color::BrightWhite,
            _other => return None,
        })
    }

    pub fn brighter(color: Color) -> Self {
        match color {
            Color::Black => Color::BrightBlack,
            Color::Red => Color::BrightRed,
            Color::Green => Color::BrightGreen,
            Color::Yellow => Color::BrightYellow,
            Color::Blue => Color::BrightBlue,
            Color::Magenta => Color::BrightMagenta,
            Color::Cyan => Color::BrightCyan,
            Color::White => Color::BrightWhite,
            already_bright => already_bright,
        }
    }

    const fn initialize_indexed_lut() -> [egui::Color32; 256] {
        let mut lut = [egui::Color32::BLACK; 256];

        // named colors
        lut[0] = egui::Color32::BLACK;
        lut[1] = egui::Color32::RED;
        lut[2] = egui::Color32::GREEN;
        lut[3] = egui::Color32::YELLOW;
        lut[4] = egui::Color32::BLUE;
        lut[5] = egui::Color32::from_rgb(128, 0, 128);
        lut[6] = egui::Color32::from_rgb(0, 170, 170);
        lut[7] = egui::Color32::WHITE;
        lut[8] = egui::Color32::from_rgb(0x5f, 0x5f, 0x5f);
        lut[9] = egui::Color32::from_rgb(0xd7, 0x87, 0x87);
        lut[10] = egui::Color32::from_rgb(0x87, 0xd7, 0x87);
        lut[11] = egui::Color32::from_rgb(0xd7, 0xd7, 0x87);
        lut[12] = egui::Color32::from_rgb(0x87, 0x87, 0xd7);
        lut[13] = egui::Color32::from_rgb(0xd7, 0x87, 0xd7);
        lut[14] = egui::Color32::from_rgb(0x87, 0xd7, 0xd7);
        lut[15] = egui::Color32::from_rgb(0xd7, 0xd7, 0xd7);

        // computed 216 colors
        // see: https://stackoverflow.com/questions/27159322/rgb-values-of-the-colors-in-the-ansi-extended-colors-index-17-255
        const fn compute_middle_range(idx: u8) -> egui::Color32 {
            let color_idx = idx.saturating_sub(16);
            let index_r = color_idx / 36;
            let index_g = (color_idx % 36) / 6;
            let index_b = color_idx % 6;
            let r = if index_r > 0 { 55 + index_r * 40 } else { 0 };
            let g = if index_g > 0 { 55 + index_g * 40 } else { 0 };
            let b = if index_b > 0 { 55 + index_b * 40 } else { 0 };
            egui::Color32::from_rgb(r, g, b)
        }

        let mut i = 16_u8;
        while i <= 231 {
            lut[i as usize] = compute_middle_range(i);
            i += 1;
        }

        // computed greyscale
        i = 232;
        while i != 0 {
            let val = (i - 232) * 10 + 8;
            lut[i as usize] = egui::Color32::from_rgb(val, val, val);
            i = i.wrapping_add(1);
        }
        lut
    }

    fn from_components_str(rgb_str: &str) -> Option<Self> {
        // 10;20;30
        rgb_str
            .split(';')
            .take(3)
            .map(|c| c.parse::<u8>())
            .collect::<Result<Vec<u8>, _>>()
            .ok()
            .and_then(|components| match components.as_slice() {
                [r, g, b] => Some(Color::TrueColor(*r, *g, *b)),
                _ => None,
            })
    }
}

impl From<Color> for egui::Color32 {
    fn from(value: Color) -> Self {
        match value {
            Color::DefaultFg => egui::Color32::LIGHT_GRAY,
            Color::DefaultBg => egui::Color32::BLACK,
            Color::Indexed(i) => COLOR_LUT[i as usize],
            Color::TrueColor(r, g, b) => egui::Color32::from_rgb(r, g, b),

            Color::Black => COLOR_LUT[0],
            Color::Red => COLOR_LUT[1],
            Color::Green => COLOR_LUT[2],
            Color::Yellow => COLOR_LUT[3],
            Color::Blue => COLOR_LUT[4],
            Color::Magenta => COLOR_LUT[5],
            Color::Cyan => COLOR_LUT[6],
            Color::White => COLOR_LUT[7],
            Color::BrightBlack => COLOR_LUT[8],
            Color::BrightRed => COLOR_LUT[9],
            Color::BrightGreen => COLOR_LUT[10],
            Color::BrightYellow => COLOR_LUT[11],
            Color::BrightBlue => COLOR_LUT[12],
            Color::BrightMagenta => COLOR_LUT[13],
            Color::BrightCyan => COLOR_LUT[14],
            Color::BrightWhite => COLOR_LUT[15],
        }
    }
}

pub fn encode_color32(color: egui::Color32) -> String {
    format!(
        "rgb:{r:04x}/{g:04x}/{b:04x}",
        r = color.r(),
        g = color.g(),
        b = color.b()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use CursorControl::*;
    use EraseControl::*;

    #[test]
    fn test_utf8_decoding() -> anyhow::Result<()> {
        let stream = b"a\x80";
        let res = String::from_utf8_lossy(stream);
        eprintln!("{res:?}");
        Ok(())
    }

    #[test]
    fn test_parser_success_case() {
        let stream = b"\x1b[Hhi!";
        let mut parser = Parser::new();
        parser.push_bytes(stream);
        assert_eq!(
            parser.next(),
            Some(AnsiToken::CursorControl(MoveTo { line: 1, col: 1 }))
        );
        assert_eq!(parser.next(), Some(AnsiToken::Text("hi!".to_string())));
        assert_eq!(parser.next(), None);
    }

    #[test]
    fn test_real_ansi_input() {
        let byte_streams = [
            "\u{1b}[1m%\u{1b}[1m\u{1b}[0m\r \r",
            "\r\u{1b}[0m\u{1b}[J$ \u{1b}",
            "[K\u{1b}[?2004h\r\r\u{1b}[0m\u{1b}[J$ ",
        ];
        let expected = [
            vec![
                AnsiToken::SGR(vec![SgrControl::Bold]),
                AnsiToken::Text('%'.to_string()),
                AnsiToken::SGR(vec![SgrControl::Bold]),
                AnsiToken::SGR(vec![SgrControl::Reset]),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::Text(' '.to_string()),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
            ],
            vec![
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::SGR(vec![SgrControl::Reset]),
                AnsiToken::EraseControl(FromCursorToEndOfScreen),
                AnsiToken::Text("$ ".to_string()),
            ],
            vec![
                AnsiToken::EraseControl(FromCursorToEndOfLine),
                AnsiToken::Unknown("\u{1b}[?2004h".to_string()),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::SGR(vec![SgrControl::Reset]),
                AnsiToken::EraseControl(FromCursorToEndOfScreen),
                AnsiToken::Text("$ ".to_string()),
            ],
        ];

        let mut parser = Parser::new();
        for (byte_stream, expected) in byte_streams.into_iter().zip(expected) {
            parser.push_bytes(byte_stream.as_bytes());
            assert_eq!(expected, Vec::from(parser.tokens()));
        }

        parser.push_bytes(b"\x1b[A");
        assert_eq!(
            vec![AnsiToken::CursorControl(CursorControl::MoveUp { lines: 1 })],
            Vec::from(parser.tokens())
        );
    }

    #[test]
    fn test_extract_params() {
        let stream = b"\x1b[;20H";
        let mut parser = Parser::new();
        parser.push_bytes(stream);
        assert_eq!(
            parser.next(),
            Some(AnsiToken::CursorControl(MoveTo { line: 1, col: 20 }))
        );
        assert_eq!(parser.next(), None);
    }

    #[test]
    fn test_multiple_params_sgr() {
        use SgrControl::*;
        let stream = b"\x1b[1;31;2mhello";
        let mut parser = Parser::new();
        parser.push_bytes(stream);
        assert_eq!(
            parser.next(),
            Some(AnsiToken::SGR(vec![
                Bold,
                ForgroundColor(Color::Red),
                Unimplemented("\u{1b}[2m".to_string())
            ]))
        );
        assert_eq!(parser.next(), Some(AnsiToken::Text("hello".to_string())));
    }

    #[test]
    fn test_partial_utf8_parsing() {
        let emoji = "he\u{1f605}llo".as_bytes();

        let mut parser = Parser::new();
        parser.push_bytes(&emoji[0..4]);
        assert_eq!(parser.next(), None);

        parser.push_bytes(&emoji[4..]);
        assert_eq!(
            parser.next(),
            Some(AnsiToken::Text("he\u{1f605}llo".to_string()))
        );
    }

    #[test]
    fn test_partial_escape_sequence_parsing() {
        let payload = "he\u{1b}[1mllo".as_bytes();

        let mut parser = Parser::new();
        parser.push_bytes(&payload[0..3]);
        assert_eq!(parser.next(), Some(AnsiToken::Text("he".to_string())));
        assert_eq!(parser.next(), None);

        parser.push_bytes(&payload[3..]);
        assert_eq!(parser.next(), Some(AnsiToken::SGR(vec![SgrControl::Bold])));
        assert_eq!(parser.next(), Some(AnsiToken::Text("llo".to_string())));
    }
}
