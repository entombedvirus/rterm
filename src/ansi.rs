use anyhow::{bail, Context};
use bytes::{Buf, BytesMut};
use std::char::REPLACEMENT_CHARACTER;
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

    pub fn tokens<'a>(&'a mut self) -> impl Iterator<Item = AnsiToken> + 'a {
        self.parsed_tokens.drain(..)
    }

    pub fn push_bytes(&mut self, incoming: &[u8]) {
        use ParserState::*;
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

        // Ansi escape sequences are ascii and therefore also valid utf-8. So if we try to decode
        // the stream as str and it fails, it's one of two possibilities: the sequence is not
        // complete or it is invalid. We can determine which by introspecting the
        // std::str::Utf8Error.

        // ---
        let sentinel_hack = AnsiToken::SGR(SgrControl::Unimplemented(
            "HACK: dummy token to make shit work".to_string(),
        ));
        let mut to_advance = 0;
        let mut push = |token: AnsiToken, token_byte_len: usize| {
            if token != sentinel_hack {
                self.parsed_tokens.push_back(token);
            }
            to_advance += token_byte_len;
        };

        let decoded = String::from_utf8_lossy(&self.stream);
        // ignore any incomplete multi-byte utf-8 char at the end
        let mut remaining = decoded.trim_end_matches(REPLACEMENT_CHARACTER);

        let mut state = ExpectingChar;
        while !remaining.is_empty() {
            match state {
                ExpectingChar => {
                    let segment = remaining
                        .split_inclusive(|ch: char| ch.is_ascii_control())
                        .next()
                        .expect("remaining is not empty, so this should not happen");
                    let last_byte = segment.as_bytes().last().copied().expect("yeah...");
                    if let Ok(control) = AsciiControl::try_from(last_byte) {
                        let normal_text = &segment[..segment.len() - 1];
                        if !normal_text.is_empty() {
                            push(AnsiToken::Text(normal_text.to_string()), normal_text.len());
                        }
                        if let AsciiControl::Escape = control {
                            state = ExpectingEscapeSequence;
                        } else {
                            push(AnsiToken::AsciiControl(control), 1);
                        }
                    } else {
                        push(AnsiToken::Text(segment.to_string()), segment.len());
                    }
                    remaining = &remaining[segment.len()..];
                    continue;
                }
                ExpectingEscapeSequence => {
                    use CursorControl::*;
                    let token = match remaining.as_bytes()[0] {
                        b'[' => {
                            state = ExpectingCSI {
                                params: String::new(),
                            };
                            remaining = &remaining[1..];
                            continue;
                        }
                        b'M' => AnsiToken::CursorControl(MoveLineUp),
                        b'7' => AnsiToken::CursorControl(SavePositionDEC),
                        b'8' => AnsiToken::CursorControl(RestorePositionDEC),
                        other => AnsiToken::Unknown(format!("\u{1b}{other}")),
                    };
                    push(token, 1 + 1);
                    remaining = &remaining[1..];
                    state = ExpectingChar;
                    continue;
                }
                ExpectingCSI { ref mut params } => {
                    use CursorControl::*;
                    use EraseControl::*;
                    let next_char = remaining.as_bytes()[0] as char;
                    let token = match next_char {
                        //  "parameter bytes" in the range 0x30–0x3F (ASCII 0–9:;<=>?), then by any
                        //  number of "intermediate bytes" in the range 0x20–0x2F (ASCII space and
                        //  !"#$%&'()*+,-./)
                        param @ ('\x30'..='\x3f' | '\x20'..='\x2f') => {
                            params.push(param);
                            remaining = &remaining[1..];
                            continue;
                        }
                        'A' => {
                            let lines: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveUp { lines })
                        }
                        'B' => {
                            let lines: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveDown { lines })
                        }
                        'C' => {
                            let cols: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveRight { cols })
                        }
                        'D' => {
                            let cols: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveLeft { cols })
                        }
                        'E' => {
                            let lines: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveLineBeginDown { lines })
                        }
                        'F' => {
                            let lines: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveLineBeginUp { lines })
                        }
                        'G' => {
                            let col: usize = extract_param(&params, 0).unwrap_or(1);
                            AnsiToken::CursorControl(MoveColumn { col })
                        }
                        'H' | 'f' => {
                            let line: usize = extract_param(&params, 0).unwrap_or(1);
                            let col: usize = extract_param(&params, 1).unwrap_or(1);
                            AnsiToken::CursorControl(MoveTo { line, col })
                        }
                        'J' => match extract_param(&params, 0).unwrap_or(0) {
                            0 => AnsiToken::EraseControl(FromCursorToEndOfScreen),
                            1 => AnsiToken::EraseControl(FromCursorToStartOfScreen),
                            2 => AnsiToken::EraseControl(Screen),
                            3 => AnsiToken::EraseControl(ScreenAndScrollback),
                            _unknown => AnsiToken::Unknown(format!("\u{1b}[{params}J")),
                        },
                        'K' => match extract_param(&params, 0).unwrap_or(0) {
                            0 => AnsiToken::EraseControl(FromCursorToEndOfLine),
                            1 => AnsiToken::EraseControl(FromCursortoStartOfLine),
                            2 => AnsiToken::EraseControl(Line),
                            _unknown => AnsiToken::Unknown(format!("\u{1b}[{params}K")),
                        },
                        'm' => {
                            if params.is_empty() {
                                AnsiToken::SGR(SgrControl::Reset)
                            } else {
                                for param in params.split(';') {
                                    let sgr = param
                                        .parse::<usize>()
                                        .map_err(|err| {
                                            anyhow::format_err!("sgr parse error: {err}")
                                        })
                                        .and_then(SgrControl::from_params)
                                        .unwrap_or_else(|_| {
                                            SgrControl::Unimplemented(format!("\u{1b}[{param}m"))
                                        });
                                    push(AnsiToken::SGR(sgr), 0);
                                }
                                sentinel_hack.clone()
                            }
                        }
                        unknown => AnsiToken::Unknown(format!("\u{1b}[{params}{unknown}")),
                    };
                    push(token, 1 + 1 + params.len() + 1);
                    state = ExpectingChar;
                    remaining = &remaining[1..];
                }
            }
        }
        self.stream.advance(to_advance);
    }
}

fn extract_param<'a>(params: &'a str, num: usize) -> Option<usize> {
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
    Text(String),
    AsciiControl(AsciiControl),
    CursorControl(CursorControl),
    EraseControl(EraseControl),
    SGR(SgrControl),
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
    MoveLineUp,
    SavePositionDEC,
    RestorePositionDEC,
    SavePositionSCO,
    RestorePositionSCO,
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
    Dim,
    Italic,
    Underline,
    SlowBlink,
    RapidBlink,
    Invert,
    Conceal,
    Strike,
    PrimaryFont,
    AlternativeFont(u8),
    NotUnderlined = 24,
    NotInverted = 27,
    ForgroundColor(Color),
    BackgroundColor(Color),
    Unimplemented(String),
}
impl SgrControl {
    fn from_params(param: usize) -> anyhow::Result<SgrControl> {
        use SgrControl::*;
        match param {
            0 => Ok(Reset),
            1 => Ok(Bold),
            2 => Ok(Dim),
            3 => Ok(Italic),
            4 => Ok(Underline),
            5 => Ok(SlowBlink),
            6 => Ok(RapidBlink),
            7 => Ok(Invert),
            8 => Ok(Conceal),
            9 => Ok(Strike),
            10 => Ok(PrimaryFont),
            num @ 11..=19 => Ok(AlternativeFont(num as u8)),
            24 => Ok(NotUnderlined),
            27 => Ok(NotInverted),
            num @ (30..=37 | 90..=97) => Ok(ForgroundColor(
                Color::from_sgr_num(num)
                    .with_context(|| format!("invalid SGR foreground color param: {num}"))?,
            )),
            num @ (40..=47 | 100..=107) => Ok(BackgroundColor(
                Color::from_sgr_num(num)
                    .with_context(|| format!("invalid SGR foreground color param: {num}"))?,
            )),
            _unknown => Err(anyhow::format_err!("unknown sgr param: {param}")),
        }
    }
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
            bail!("invalid ascii control char: {value}")
        }
    }
}

impl From<AsciiControl> for char {
    fn from(value: AsciiControl) -> Self {
        value as u8 as char
    }
}

#[derive(Debug)]
enum ParserState {
    ExpectingChar,
    ExpectingEscapeSequence,
    ExpectingCSI { params: String },
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
}
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
}

impl From<Color> for egui::Color32 {
    fn from(value: Color) -> Self {
        match value {
            Color::Black => egui::Color32::BLACK,
            Color::Red => egui::Color32::RED,
            Color::Green => egui::Color32::GREEN,
            Color::Yellow => egui::Color32::YELLOW,
            Color::Blue => egui::Color32::BLUE,
            Color::Magenta => egui::Color32::from_rgb(128, 0, 128),
            Color::Cyan => egui::Color32::from_rgb(0, 170, 170),
            Color::White => egui::Color32::WHITE,
            Color::BrightBlack => egui::Color32::from_rgb(0x5f, 0x5f, 0x5f),
            Color::BrightRed => egui::Color32::from_rgb(0xd7, 0x87, 0x87),
            Color::BrightGreen => egui::Color32::from_rgb(0x87, 0xd7, 0x87),
            Color::BrightYellow => egui::Color32::from_rgb(0xd7, 0xd7, 0x87),
            Color::BrightBlue => egui::Color32::from_rgb(0x87, 0x87, 0xd7),
            Color::BrightMagenta => egui::Color32::from_rgb(0xd7, 0x87, 0xd7),
            Color::BrightCyan => egui::Color32::from_rgb(0x87, 0xd7, 0xd7),
            Color::BrightWhite => egui::Color32::from_rgb(0xd7, 0xd7, 0xd7),
        }
    }
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
            "\u{1b}[1m\u{1b}[7m%\u{1b}[27m\u{1b}[1m\u{1b}[0m\r \r",
            "\r\u{1b}[0m\u{1b}[27m\u{1b}[24m\u{1b}[J$ \u{1b}",
            "[K\u{1b}[?2004h\r\r\u{1b}[0m\u{1b}[27m\u{1b}[24m\u{1b}[J$ ",
        ];
        let expected = [
            vec![
                AnsiToken::SGR(SgrControl::Bold),
                AnsiToken::SGR(SgrControl::Invert),
                AnsiToken::Text('%'.to_string()),
                AnsiToken::SGR(SgrControl::NotInverted),
                AnsiToken::SGR(SgrControl::Bold),
                AnsiToken::SGR(SgrControl::Reset),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::Text(' '.to_string()),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
            ],
            vec![
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::SGR(SgrControl::Reset),
                AnsiToken::SGR(SgrControl::NotInverted),
                AnsiToken::SGR(SgrControl::NotUnderlined),
                AnsiToken::EraseControl(FromCursorToEndOfScreen),
                AnsiToken::Text("$ ".to_string()),
            ],
            vec![
                AnsiToken::EraseControl(FromCursorToEndOfLine),
                AnsiToken::Unknown("\u{1b}[?2004h".to_string()),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::SGR(SgrControl::Reset),
                AnsiToken::SGR(SgrControl::NotInverted),
                AnsiToken::SGR(SgrControl::NotUnderlined),
                AnsiToken::EraseControl(FromCursorToEndOfScreen),
                AnsiToken::Text("$ ".to_string()),
            ],
        ];

        let mut parser = Parser::new();
        for (byte_stream, expected) in byte_streams.into_iter().zip(expected) {
            parser.push_bytes(byte_stream.as_bytes());
            assert_eq!(expected, parser.tokens().collect::<Vec<_>>());
        }

        parser.push_bytes(b"\x1b[A");
        assert_eq!(
            vec![AnsiToken::CursorControl(CursorControl::MoveUp { lines: 1 })],
            parser.tokens().collect::<Vec<_>>()
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
}
