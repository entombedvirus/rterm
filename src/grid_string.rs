#![allow(unused, dead_code)]

use std::{
    collections::VecDeque,
    error::Error,
    ops::{Deref, DerefMut, Range, RangeBounds},
    str::FromStr,
};

use arrayvec::ArrayString;

use crate::terminal_emulator::SgrState;

#[cfg(not(debug_assertions))]
pub const MAX_BYTES: usize = 32;

#[cfg(debug_assertions)]
pub const MAX_BYTES: usize = 8;

type Result<T> = std::result::Result<T, GridStringError>;

/// GridString is a fixed capacity, stack allocated string together with its formatting details as
/// capctured by SgrState.
#[derive(Debug, Default, Clone)]
pub struct GridString {
    sgr: SgrState,
    nchars: u8,
    buf: ArrayString<MAX_BYTES>,
}

impl GridString {
    fn new(sgr: SgrState, s: &str) -> Result<Self> {
        let mut ret = Self::from_str(s)?;
        ret.sgr = sgr;
        Ok(ret)
    }

    fn with_sgr(sgr: SgrState) -> Self {
        Self {
            sgr,
            ..Default::default()
        }
    }

    pub fn from_str(s: &str) -> Result<Self> {
        let buf = ArrayString::from_str(s).map_err(GridStringError::CapacityError)?;
        let nchars = buf.chars().count() as u8;
        let sgr = SgrState::default();
        Ok(Self { sgr, nchars, buf })
    }

    pub fn as_str(&self) -> &str {
        self.buf.as_str()
    }

    pub fn sgr(&self) -> SgrState {
        self.sgr
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn len_chars(&self) -> usize {
        self.nchars as usize
    }

    pub const fn capacity(&self) -> usize {
        self.buf.capacity()
    }

    fn push_str<'a>(&mut self, s: &'a str) -> (usize, &'a str) {
        let rem = self.buf.remaining_capacity();
        let (prefix, suffix) = split_str_at_utf8_boundary(s, rem);
        self.buf.push_str(prefix);
        let written = prefix.chars().count();
        self.nchars += written as u8;
        (written, suffix)
    }

    pub fn slice_bytes<R: RangeBounds<usize>>(&self, byte_range: R) -> Option<GridStr> {
        let byte_range = resolve_range(byte_range, 0..self.len_bytes()).ok()?;
        let sliced_buf = self.buf.get(byte_range)?;
        Some(GridStr::new(self.sgr, sliced_buf))
    }

    pub fn slice_chars<R: RangeBounds<usize>>(&self, char_range: R) -> Option<GridStr> {
        let (byte_range, nchars) = self.resolve_char_to_byte_range(char_range).ok()?;
        let sliced_buf = self.buf.get(byte_range)?;
        Some(GridStr {
            sgr: self.sgr,
            nchars,
            buf: sliced_buf,
        })
    }

    pub fn remove_char_range<R: RangeBounds<usize>>(&mut self, char_range: R) -> Result<()> {
        let (byte_range, nchars) = self.resolve_char_to_byte_range(char_range)?;
        let temp = self.buf.clone();
        self.buf.truncate(byte_range.start);
        self.buf.push_str(&temp[byte_range.end..]);
        self.nchars -= nchars;
        Ok(())
    }

    pub fn clear(&mut self) {
        *self = Self::default();
    }

    fn resolve_char_to_byte_range<R: RangeBounds<usize>>(
        &self,
        char_range: R,
    ) -> Result<(Range<usize>, u8)> {
        let char_range = resolve_range(char_range, 0..self.len_chars())?;
        if char_range.is_empty() && char_range.start == 0 {
            return Ok((0..0, 0));
        }

        let mut iter = self.buf.char_indices();
        let (byte_start, _) = iter
            .by_ref()
            .nth(char_range.start)
            .expect("char_range checked above");

        if char_range.is_empty() {
            Ok((byte_start..byte_start, 0))
        } else {
            let (byte_end, _) = iter
                .nth(char_range.len() - 1)
                .unwrap_or((self.len_bytes(), ' '));
            Ok((byte_start..byte_end, char_range.len() as u8))
        }
    }

    /// edits the buf in place starting at char_idx, overwriting existing chars. Returns number of
    /// chars taken from `new_text` and the remainder string. This will not change `len_chars` but
    /// ccan change `len_bytes` depending on input.
    fn replace_str<'b>(&mut self, char_idx: usize, new_text: &'b str) -> (usize, &'b str) {
        if char_idx >= self.len_chars() {
            return (0, new_text);
        }

        // where each char came from
        enum Whence {
            OldText(char),
            NewText(char),
        };

        impl Whence {
            fn char(&self) -> char {
                match *self {
                    Whence::OldText(ch) => ch,
                    Whence::NewText(ch) => ch,
                }
            }
            fn update_remainder<'b>(&self, text: &'b str, nwritten: &mut usize) -> &'b str {
                match *self {
                    Whence::OldText(_) => text,
                    Whence::NewText(ch) => {
                        *nwritten += 1;
                        &text[ch.len_utf8()..]
                    }
                }
            }
        }

        let mut old_chars = self.buf.chars().map(Whence::OldText);
        let mut new_chars = new_text.chars().map(Whence::NewText);
        let splice_idx = self.len_chars().min(char_idx);
        let mut edited_chars = (0..self.len_chars()).filter_map(|ci| {
            if ci < splice_idx {
                old_chars.by_ref().next()
            } else {
                new_chars
                    .by_ref()
                    .next()
                    .map(|x| {
                        // skip corresponding char from old so that
                        // if we run out of chars in next, we can pick up
                        // trailing chars from old as a fallback
                        old_chars.by_ref().next();
                        x
                    })
                    .or_else(|| old_chars.by_ref().next())
            }
        });

        let mut edited_buf = ArrayString::new();
        let mut new_num_chars = 0;
        let mut nwritten = 0;
        let mut rem = new_text;
        for whence in edited_chars {
            if let Ok(_) = edited_buf.try_push(whence.char()) {
                new_num_chars += 1;
                rem = whence.update_remainder(rem, &mut nwritten);
            } else {
                break;
            }
        }
        self.buf = edited_buf;
        self.nchars = new_num_chars;
        (nwritten, rem)
    }
}

fn split_str_at_utf8_boundary(s: &str, index: usize) -> (&str, &str) {
    if index >= s.len() {
        (s, "")
    } else {
        // utf-8 code points are max 4 bytes, so we only need to check last 4 bytes or fewer if the
        // string is smaller that 4 bytes
        let start = index.saturating_sub(3);
        for candidate in (start..=index).rev() {
            if s.is_char_boundary(candidate) {
                return s.split_at(candidate);
            }
        }
        unreachable!("s was not a valid utf-8 string")
    }
}

fn resolve_range<R: RangeBounds<usize>>(r: R, valid: Range<usize>) -> Result<Range<usize>> {
    let start = match r.start_bound() {
        std::ops::Bound::Included(&n) => n,
        std::ops::Bound::Excluded(&n) => n + 1,
        std::ops::Bound::Unbounded => valid.start,
    };
    let end = match r.end_bound() {
        std::ops::Bound::Included(&n) => n + 1,
        std::ops::Bound::Excluded(&n) => n,
        std::ops::Bound::Unbounded => valid.end,
    };

    if !valid.contains(&start) {
        return Err(GridStringError::OutOfBounds {
            which: "start",
            given: start,
            bound: valid,
        });
    }

    if end > valid.end {
        return Err(GridStringError::OutOfBounds {
            which: "end",
            given: end,
            bound: valid,
        });
    }

    if start > end {
        return Err(GridStringError::OutOfBounds {
            which: "start",
            given: start,
            bound: valid,
        });
    }

    Ok(start..end)
}
impl FromStr for GridString {
    type Err = GridStringError;

    fn from_str(s: &str) -> Result<Self> {
        GridString::from_str(s)
    }
}

impl AsRef<str> for GridString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct GridStr<'a> {
    sgr: SgrState,
    nchars: u8,
    buf: &'a str,
}

impl<'a> GridStr<'a> {
    fn new(sgr: SgrState, buf: &'a str) -> Self {
        let nchars = buf.chars().count() as u8;
        Self { sgr, nchars, buf }
    }

    fn from(s: &'a GridString) -> Self {
        Self {
            sgr: s.sgr,
            nchars: s.nchars,
            buf: &s.buf,
        }
    }

    pub fn sgr(&self) -> SgrState {
        self.sgr
    }

    pub fn as_str(&self) -> &'a str {
        self.buf
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn len_chars(&self) -> usize {
        self.nchars as usize
    }
}

#[derive(Debug)]
struct StripedString<'a> {
    parts: Vec<&'a mut GridString>,
}

impl<'a> StripedString<'a> {
    pub fn from_iter<I: IntoIterator<Item = &'a mut GridString>>(iterable: I) -> Self {
        let parts = iterable.into_iter().collect();
        Self { parts }
    }

    pub fn replace_str<'b>(
        &mut self,
        mut char_offset: usize,
        new_text: &'b str,
    ) -> Result<(usize, &'b str)> {
        let first_part = self
            .parts
            .get(0)
            .ok_or_else(|| GridStringError::OutOfBounds {
                which: "replace_str with zero parts",
                given: 0,
                bound: 0..0,
            })?;
        if (char_offset > first_part.len_chars()) {
            return Err(GridStringError::OutOfBounds {
                which: "replace_str out of bounds write_char_idx",
                given: char_offset,
                bound: 0..first_part.len_chars(),
            });
        }

        let mut chars_written = 0;
        let mut to_write = new_text;
        for part in self.parts.iter_mut() {
            if to_write.is_empty() {
                break;
            }
            let (nwr, rest) = part.replace_str(std::mem::take(&mut char_offset), to_write);
            chars_written += nwr;
            to_write = rest;
        }

        Ok((chars_written, to_write))
    }

    fn push_str<'b>(&mut self, new_text: &'b str) -> (usize, &'b str) {
        let mut chars_written = 0;
        let mut to_write = new_text;
        for part in self.parts.iter_mut() {
            if to_write.is_empty() {
                break;
            }
            let (nwr, rest) = part.push_str(to_write);
            chars_written += nwr;
            to_write = rest;
        }

        (chars_written, to_write)
    }
}

#[derive(Debug, Clone)]
pub enum GridStringError {
    CapacityError(arrayvec::CapacityError),
    OutOfBounds {
        which: &'static str,
        given: usize,
        bound: Range<usize>,
    },
}

impl std::fmt::Display for GridStringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use GridStringError::*;
        match *self {
            CapacityError(inner) => write!(f, "out of capacity: {inner}. max: {}", MAX_BYTES),
            OutOfBounds {
                which,
                given,
                ref bound,
            } => {
                write!(f, "out of bounds: {which} given: {given}, bound: {bound:?}")
            }
        }
    }
}

impl std::error::Error for GridStringError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            GridStringError::CapacityError(ref inner) => Some(inner),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_1() -> Result<()> {
        let gs = GridString::from_str("hello, world");
        assert!(matches!(gs, Err(GridStringError::CapacityError(_))));

        let mut gs = GridString::from_str("hello")?;
        assert_eq!(gs.as_str(), "hello");
        assert_eq!(gs.len_bytes(), 5);
        assert_eq!(gs.len_chars(), 5);
        assert_eq!(gs.sgr(), SgrState::default());

        Ok(())
    }

    #[test]
    fn try_string_2() {
        let sgr = SgrState {
            bold: true,
            ..Default::default()
        };
        let mut gs = GridString::with_sgr(sgr);
        gs.push_str("hi hi");
        assert_eq!(gs.as_str(), "hi hi");
        assert_eq!(gs.sgr(), sgr);
    }

    #[test]
    fn test_string_3() {
        let mut gs = GridString::default();
        gs.push_str("hi hi");
        gs.replace_str(1, "eyo!");
        assert_eq!(gs.as_str(), "heyo!");

        // should not extend, even if there is capacity.
        // this behavior is necessary to implement proper striped writes
        gs.clear();
        gs.push_str("abcd");
        let (nwritten, rem) = gs.replace_str(2, "efgh");
        assert_eq!(gs.as_str(), "abef");
        assert_eq!(nwritten, 2);
        assert_eq!(rem, "gh");

        // replace in the middle
        gs.clear();
        gs.push_str("abcde");
        let (nwritten, rem) = gs.replace_str(1, "1\u{c3a9}3");
        assert_eq!(gs.as_str(), "a1\u{c3a9}3e");
        assert_eq!(nwritten, 3);
        assert_eq!(rem, "");
    }

    #[test]
    fn test_string_slice_1() {
        let mut sgr = SgrState::default();
        sgr.italic = true;

        let gs = GridString::new(sgr, "hello da").expect("should fit");
        let slice = gs.slice_bytes(1..7).expect("is valid range");
        assert_eq!(slice.sgr(), sgr);
        assert_eq!(slice.as_str(), "ello d");
        assert_eq!(slice.len_chars(), 6);

        let slice = gs.slice_bytes(..).expect("is valid range");
        assert_eq!(slice.sgr(), sgr);
        assert_eq!(slice.as_str(), "hello da");

        assert_eq!(gs.slice_bytes(1..9), None);
    }

    #[test]
    fn test_string_slice_2() {
        let mut sgr = SgrState::default();
        sgr.italic = true;
        sgr.bold = true;

        let gs = GridString::new(sgr, "h\u{c3a9}llo").expect("should fit");
        assert_eq!(gs.len_bytes(), 7);
        assert_eq!(gs.len_chars(), 5);

        let slice = gs.slice_chars(1..2).expect("valid range");
        assert_eq!(slice.sgr(), sgr);
        assert_eq!(slice.as_str(), "\u{c3a9}");
    }

    #[test]
    fn test_remove_range_1() {
        let mut gs = GridString::from_str("h\u{c3a9}llo").expect("should fit");
        gs.remove_char_range(..2).expect("valid range");
        assert_eq!(gs.as_str(), "llo");
        assert_eq!(gs.len_chars(), 3);
    }

    #[test]
    fn test_vectored_write_1() {
        let mut gs1 = GridString::from_str("").unwrap();
        let mut gs2 = GridString::from_str("").unwrap();

        let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
        let (nwritten, rem) = striped.push_str("a longer string");
        assert_eq!(gs1.as_str(), "a longer");
        assert_eq!(gs2.as_str(), " string");
        assert_eq!(rem, "");
        assert_eq!(nwritten, 15);

        let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
        let (nwritten, rem) = striped
            .replace_str(0, "a longer string that won't fit")
            .unwrap();
        assert_eq!(gs1.as_str(), "a longer");
        assert_eq!(gs2.as_str(), " string");
        assert_eq!(rem, " that won't fit");
        assert_eq!(nwritten, 15);

        let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
        let (nwritten, rem) = striped
            .replace_str(
                0,
                "a long\u{c3a9}er string that won't fit, with utf-8 boundary",
            )
            .unwrap();
        assert_eq!(gs1.as_str(), "a long");
        assert_eq!(gs2.as_str(), "\u{c3a9}er st");
        assert_eq!(rem, "ring that won't fit, with utf-8 boundary");
        assert_eq!(nwritten, 12);

        let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
        let (nwritten, rem) = striped.replace_str(6, "er").unwrap();
        assert_eq!(gs1.as_str(), "a long");
        assert_eq!(gs2.as_str(), "err st");
        assert_eq!(rem, "");
        assert_eq!(nwritten, 2);

        let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
        assert!(striped.replace_str(8, ", fit").is_err());
        assert_eq!(gs1.as_str(), "a long");
        assert_eq!(gs2.as_str(), "err st");
    }
}
