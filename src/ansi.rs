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
        [b'm', rem @ ..] => Some((rem, {
            if params.is_empty() {
                return Some((rem, AnsiToken::SGR(vec![SgrControl::Reset])));
            }
            let params = params.split(';').map(|p| {
                p.parse::<usize>()
                    .map_err(|err| anyhow::format_err!("sgr parse error: {err}"))
                    .and_then(SgrControl::from_params)
                    .unwrap_or(SgrControl::Unimplemented(format!("\u{1b}[{p}m")))
            });
            AnsiToken::SGR(params.collect())
        })),
        [unknown, rem @ ..] => Some((
            rem,
            AnsiToken::Unknown(format!("\u{1b}[{params}{ch}", ch = *unknown as char)),
        )),
    }
}

fn parse_escape_sequence(buf: &[u8]) -> Option<(&[u8], AnsiToken)> {
    use CursorControl::*;
    match buf {
        [b'[', rem @ ..] => parse_csi_escape_sequence(rem),
        [b'M', rem @ ..] => Some((rem, AnsiToken::CursorControl(ScrollUpFromHome))),
        [b'7', rem @ ..] => Some((rem, AnsiToken::CursorControl(SavePositionDEC))),
        [b'8', rem @ ..] => Some((rem, AnsiToken::CursorControl(RestorePositionDEC))),
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
    SGR(Vec<SgrControl>),
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
    ForgroundColor(Color),
    Unimplemented(String),
}
impl SgrControl {
    pub fn from_params(param: usize) -> anyhow::Result<SgrControl> {
        use SgrControl::*;
        match param {
            0 => Ok(Reset),
            1 => Ok(Bold),
            num @ (30..=37 | 90..=97) => Ok(ForgroundColor(
                Color::from_sgr_num(num)
                    .with_context(|| format!("invalid SGR foreground color param: {num}"))?,
            )),
            // num @ (40..=47 | 100..=107) => Ok(BackgroundColor(
            //     Color::from_sgr_num(num)
            //         .with_context(|| format!("invalid SGR foreground color param: {num}"))?,
            // )),
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
