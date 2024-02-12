use anyhow::bail;
use bytes::{Buf, BytesMut};
use std::char::REPLACEMENT_CHARACTER;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct Parser {
    stream: BytesMut,
    parsed_tokens: VecDeque<AnsiToken>,
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
        let decoded = String::from_utf8_lossy(&self.stream);
        let mut char_stream = decoded.chars().peekable();
        let mut state = ExpectingChar;

        let mut to_advance = 0;
        let mut push = |token: AnsiToken, token_byte_len: usize| {
            self.parsed_tokens.push_back(token);
            to_advance += token_byte_len;
        };

        while let Some(next_char) = char_stream.next() {
            if next_char == REPLACEMENT_CHARACTER && char_stream.peek().is_none() {
                // last replacement char is likely due to incomplete byte stream. instead of
                // treating it as an error, exit early and wait for more input
                break;
            }
            match state {
                ExpectingChar => {
                    let token = if let Ok(ascii_control) = AsciiControl::try_from(next_char) {
                        if ascii_control == AsciiControl::Escape {
                            state = ExpectingEscapeSequence;
                            continue;
                        }
                        AnsiToken::AsciiControl(ascii_control)
                    } else {
                        AnsiToken::Char(next_char)
                    };
                    push(token, next_char.len_utf8());
                }
                ExpectingEscapeSequence => {
                    use CursorControl::*;
                    let token = match next_char {
                        '[' => {
                            state = ExpectingCSI { args: Vec::new() };
                            continue;
                        }
                        'M' => AnsiToken::CursorControl(MoveLineUp),
                        '7' => AnsiToken::CursorControl(SavePositionDEC),
                        '8' => AnsiToken::CursorControl(RestorePositionDEC),
                        other => AnsiToken::Unknown(vec!['\x1b', other]),
                    };
                    push(token, 1 + next_char.len_utf8());
                }
                ExpectingCSI { ref mut args } => {
                    use CursorControl::*;
                    use EraseControl::*;
                    let separated_args = String::from_iter(args.iter().copied());
                    let mut separated_args = separated_args.split(';');
                    let token = match next_char {
                        'A' => {
                            let lines: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveUp { lines })
                        }
                        'B' => {
                            let lines: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveDown { lines })
                        }
                        'C' => {
                            let cols: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveRight { cols })
                        }
                        'D' => {
                            let cols: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveLeft { cols })
                        }
                        'E' => {
                            let lines: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveLineBeginDown { lines })
                        }
                        'F' => {
                            let lines: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveLineBeginUp { lines })
                        }
                        'G' => {
                            let col: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveColumn { col })
                        }
                        'H' | 'f' => {
                            let line: usize = next_usize(&mut separated_args, 1);
                            let col: usize = next_usize(&mut separated_args, 1);
                            AnsiToken::CursorControl(MoveTo { line, col })
                        }
                        'J' => match next_usize(&mut separated_args, 0) {
                            0 => AnsiToken::EraseControl(FromCursorToEndOfScreen),
                            1 => AnsiToken::EraseControl(FromCursorToStartOfScreen),
                            2 => AnsiToken::EraseControl(Screen),
                            3 => AnsiToken::EraseControl(ScreenAndScrollback),
                            _unknown => {
                                let mut seq = vec!['\x1b', '['];
                                seq.extend_from_slice(args);
                                seq.push('J');
                                AnsiToken::Unknown(seq)
                            }
                        },
                        'K' => match next_usize(&mut separated_args, 0) {
                            0 => AnsiToken::EraseControl(FromCursorToEndOfLine),
                            1 => AnsiToken::EraseControl(FromCursortoStartOfLine),
                            2 => AnsiToken::EraseControl(Line),
                            _unknown => {
                                let mut seq = vec!['\x1b', '['];
                                seq.extend_from_slice(args);
                                seq.push('K');
                                AnsiToken::Unknown(seq)
                            }
                        },
                        'm' => AnsiToken::SGR(SgrControl::from(next_usize(&mut separated_args, 0))),
                        //  "parameter bytes" in the range 0x30–0x3F (ASCII 0–9:;<=>?), then by any
                        //  number of "intermediate bytes" in the range 0x20–0x2F (ASCII space and
                        //  !"#$%&'()*+,-./)
                        param @ ('\x30'..='\x3f' | '\x20'..='\x2f') => {
                            args.push(param);
                            continue;
                        }
                        unknown => {
                            let mut seq = vec!['\x1b', '['];
                            seq.extend_from_slice(args);
                            seq.push(unknown);
                            AnsiToken::Unknown(seq)
                        }
                    };
                    push(
                        token,
                        2 + args.iter().map(|ch| ch.len_utf8()).sum::<usize>()
                            + next_char.len_utf8(),
                    );
                }
            };
            state = ExpectingChar;
        }
        self.stream.advance(to_advance);
    }
}

fn next_usize<'a>(mut iter: impl Iterator<Item = &'a str>, default: usize) -> usize {
    iter.next()
        .and_then(|item| item.parse().ok())
        .unwrap_or(default)
}
// See: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnsiToken {
    Char(char),
    AsciiControl(AsciiControl),
    CursorControl(CursorControl),
    EraseControl(EraseControl),
    SGR(SgrControl),
    Unknown(Vec<char>),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Unimplemented(usize),
}

impl From<usize> for SgrControl {
    fn from(value: usize) -> Self {
        use SgrControl::*;
        match value {
            0 => Reset,
            1 => Bold,
            2 => Dim,
            3 => Italic,
            4 => Underline,
            5 => SlowBlink,
            6 => RapidBlink,
            7 => Invert,
            8 => Conceal,
            9 => Strike,
            10 => PrimaryFont,
            24 => NotUnderlined,
            27 => NotInverted,
            num @ 11..=19 => AlternativeFont(num as u8),
            unknown => Unimplemented(unknown),
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

    fn try_from(value: char) -> std::prelude::v1::Result<Self, Self::Error> {
        if value.is_ascii_control() {
            // SAFETY: enum is repr(u8) and range is mapped exactly to the one checked in
            // is_ascii_control
            Ok(unsafe { std::mem::transmute(value as u8) })
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
    ExpectingCSI { args: Vec<char> },
}

impl Iterator for Parser {
    type Item = AnsiToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.parsed_tokens.pop_front()
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
        assert_eq!(parser.next(), Some(AnsiToken::Char('h')));
        assert_eq!(parser.next(), Some(AnsiToken::Char('i')));
        assert_eq!(parser.next(), Some(AnsiToken::Char('!')));
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
                AnsiToken::Char('%'),
                AnsiToken::SGR(SgrControl::NotInverted),
                AnsiToken::SGR(SgrControl::Bold),
                AnsiToken::SGR(SgrControl::Reset),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::Char(' '),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
            ],
            vec![
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::SGR(SgrControl::Reset),
                AnsiToken::SGR(SgrControl::NotInverted),
                AnsiToken::SGR(SgrControl::NotUnderlined),
                AnsiToken::EraseControl(FromCursorToEndOfScreen),
                AnsiToken::Char('$'),
                AnsiToken::Char(' '),
            ],
            vec![
                AnsiToken::EraseControl(FromCursorToEndOfLine),
                AnsiToken::Unknown(vec!['\x1b', '[', '?', '2', '0', '0', '4', 'h']),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::AsciiControl(AsciiControl::CarriageReturn),
                AnsiToken::SGR(SgrControl::Reset),
                AnsiToken::SGR(SgrControl::NotInverted),
                AnsiToken::SGR(SgrControl::NotUnderlined),
                AnsiToken::EraseControl(FromCursorToEndOfScreen),
                AnsiToken::Char('$'),
                AnsiToken::Char(' '),
            ],
        ];

        let mut parser = Parser::new();
        for (byte_stream, expected) in byte_streams.into_iter().zip(expected) {
            parser.push_bytes(byte_stream.as_bytes());
            assert_eq!(expected, parser.tokens().collect::<Vec<_>>());
        }
    }
}
