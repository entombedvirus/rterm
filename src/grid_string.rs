#![allow(unused, dead_code)]

use std::{
    collections::VecDeque,
    error::Error,
    ops::{Deref, DerefMut, Not as _, Range, RangeBounds},
    str::FromStr,
};

use arrayvec::{ArrayString, ArrayVec};

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
    buf: ArrayString<MAX_BYTES>,
    sgr: ArrayVec<SgrState, MAX_BYTES>,
}

impl GridString {
    fn new(sgr: SgrState, s: &str) -> Result<Self> {
        let buf = ArrayString::from_str(s).map_err(GridStringError::CapacityError)?;
        let nchars = buf.chars().count() as u8;
        let sgr = std::iter::repeat(sgr).take(nchars as usize).collect();
        Ok(Self { buf, sgr })
    }

    fn with_sgr(sgr: SgrState) -> Self {
        Self::new(sgr, "").expect("valid string")
    }

    pub fn from_str(s: &str) -> Result<Self> {
        Self::new(SgrState::default(), s)
    }

    pub fn as_str(&self) -> &str {
        self.buf.as_str()
    }

    pub fn sgr(&self) -> &[SgrState] {
        &self.sgr
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn len_chars(&self) -> usize {
        self.sgr.len()
    }

    pub const fn capacity(&self) -> usize {
        self.buf.capacity()
    }

    pub fn clear(&mut self) {
        *self = Self::default();
    }

    pub fn has_room(&self) -> bool {
        self.buf.remaining_capacity() > 0 && self.sgr.remaining_capacity() > 0
    }

    pub fn push_str<'a>(&mut self, s: &'a str, sgr: SgrState) -> (Option<Self>, usize, &'a str) {
        let (prefix, suffix) = split_str_at_utf8_boundary(s, self.buf.remaining_capacity());
        let written = prefix.chars().count();
        if written == 0 {
            Self::split(s, sgr)
        } else {
            self.buf.push_str(prefix);
            self.sgr.extend(std::iter::repeat(sgr).take(written));
            (None, written, suffix)
        }
    }

    /// edits the buf in place starting at char_idx, overwriting existing chars. Returns number of
    /// chars taken from `new_text` and the remainder string. This will not change `len_chars` but
    /// ccan change `len_bytes` depending on input.
    pub fn replace_str<'b>(
        &mut self,
        char_idx: usize,
        new_text: &'b str,
        new_sgr: SgrState,
    ) -> (usize, &'b str) {
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
        let mut edited_sgr = ArrayVec::new();
        let mut new_num_chars = 0;
        let mut nwritten = 0;
        let mut rem = new_text;
        for (char_idx, whence) in edited_chars.enumerate() {
            if let Ok(_) = edited_buf.try_push(whence.char()) {
                new_num_chars += 1;
                match whence {
                    Whence::OldText(ch) => {
                        edited_sgr.push(self.sgr[char_idx]);
                    }
                    Whence::NewText(ch) => {
                        edited_sgr.push(new_sgr);
                        nwritten += 1;
                        rem = &rem[ch.len_utf8()..];
                    }
                }
            } else {
                break;
            }
        }
        self.buf = edited_buf;
        self.sgr = edited_sgr;
        (nwritten, rem)
    }

    pub fn insert_str<'a>(
        &mut self,
        char_idx: usize,
        new_text: &'a str,
        new_sgr: SgrState,
    ) -> (Option<Self>, usize, &'a str) {
        if char_idx > self.len_chars() {
            panic!(
                "insert_str char_idx out of bounds: {char_idx} / {}",
                self.len_chars()
            );
        }

        let Some(first_char) = new_text.chars().next() else {
            // new_text is empty
            return (None, 0, "");
        };

        if char_idx == self.len_chars() && first_char.len_utf8() > self.buf.remaining_capacity() {
            return Self::split(new_text, new_sgr);
        }

        let mut edited_buf = ArrayString::new();
        let mut edited_sgr = ArrayVec::new();

        let byte_idx = self
            .buf
            .as_str()
            .char_indices()
            .nth(char_idx)
            .map(|(bi, _)| bi)
            .unwrap_or(self.buf.len());
        let (old_text_prefix, old_text_suffix) = self.buf.split_at(byte_idx);
        edited_buf
            .try_push_str(old_text_prefix)
            .expect("part of old text should always fit");
        edited_sgr.extend(self.sgr[..char_idx].into_iter().cloned());

        let (new_text_prefix, new_text_suffix) =
            split_str_at_utf8_boundary(new_text, edited_buf.remaining_capacity());
        let new_text_prefix_chars = new_text_prefix.chars().count();

        edited_buf
            .try_push_str(new_text_prefix)
            .expect("checked that prefix will fit in the remaining capacity");
        edited_sgr.extend(std::iter::repeat(new_sgr).take(new_text_prefix_chars));

        let ret = if new_text_suffix.is_empty() {
            let mut overflow: Option<Self> = None;
            for (i, (byte_idx, old_ch)) in old_text_suffix.char_indices().enumerate() {
                let sgr = self.sgr[(char_idx + i)];
                match edited_buf.try_push(old_ch) {
                    Ok(_) => {
                        edited_sgr.push(sgr);
                    }
                    Err(err) => {
                        overflow.get_or_insert_with(Default::default).push_str(
                            &old_text_suffix[byte_idx..byte_idx + old_ch.len_utf8()],
                            sgr,
                        );
                    }
                }
            }
            (overflow, new_text_prefix_chars, "")
        } else {
            let overflow = old_text_suffix.is_empty().not().then(|| {
                Self::from_str(old_text_suffix)
                    .expect("old_text_suffix was previously part of self, so it should be fine")
            });

            (overflow, new_text_prefix_chars, new_text_suffix)
        };

        self.buf = edited_buf;
        self.sgr = edited_sgr;

        ret
    }

    pub fn slice_bytes<R: RangeBounds<usize>>(&self, byte_range: R) -> Option<GridStr> {
        let byte_range = resolve_range(byte_range, 0..self.len_bytes()).ok()?;
        let char_range = self.resolve_byte_to_char_range(byte_range.clone()).ok()?;
        Some(self.slice(char_range, byte_range))
    }

    pub fn slice_chars<R: RangeBounds<usize>>(&self, char_range: R) -> Option<GridStr> {
        let char_range = resolve_range(char_range, 0..self.len_chars()).ok()?;
        let byte_range = self.resolve_char_to_byte_range(char_range.clone()).ok()?;
        Some(self.slice(char_range, byte_range))
    }

    fn slice(&self, char_range: Range<usize>, byte_range: Range<usize>) -> GridStr {
        let sgr = &self.sgr[char_range.clone()];
        GridStr {
            sgr: &self.sgr[char_range.clone()],
            buf: &self.buf[byte_range],
        }
    }

    pub fn remove_char_range<R: RangeBounds<usize>>(&mut self, char_range: R) -> Result<()> {
        let char_range = resolve_range(char_range, 0..self.len_chars())?;
        let byte_range = self.resolve_char_to_byte_range(char_range.clone())?;
        let temp = self.buf.clone();
        self.buf.truncate(byte_range.start);
        self.buf.push_str(&temp[byte_range.end..]);
        self.sgr.copy_within(char_range.end.., char_range.start);
        self.sgr.truncate(self.sgr.len() - char_range.len());
        Ok(())
    }

    fn resolve_char_to_byte_range(&self, char_range: Range<usize>) -> Result<Range<usize>> {
        if char_range.is_empty() && char_range.start == 0 {
            return Ok(0..0);
        }

        let mut iter = self.buf.char_indices();
        let (byte_start, _) = iter
            .by_ref()
            .nth(char_range.start)
            .expect("char_range checked above");

        if char_range.is_empty() {
            Ok(byte_start..byte_start)
        } else {
            let (byte_end, _) = iter
                .nth(char_range.len() - 1)
                .unwrap_or((self.len_bytes(), ' '));
            Ok(byte_start..byte_end)
        }
    }

    fn resolve_byte_to_char_range(&self, byte_range: Range<usize>) -> Result<Range<usize>> {
        let mut start = (byte_range.start == 0).then_some(0);
        let mut end = (byte_range.end == self.len_bytes()).then_some(self.len_chars());
        for (byte_idx, _) in self.buf.char_indices() {
            if start.zip(end).is_some() {
                break;
            }
            if byte_idx == byte_range.start {
                start = Some(byte_idx);
            }
            if byte_idx == byte_range.end {
                end = Some(byte_idx)
            }
        }

        start
            .zip(end)
            .map(|(start, end)| start..end)
            .ok_or_else(|| GridStringError::OutOfBounds {
                which: "byte range is not at a valid char boundary",
                given: byte_range.start,
                bound: 0..self.len_bytes(),
            })
    }

    fn split(new_text: &str, new_sgr: SgrState) -> (Option<GridString>, usize, &str) {
        let (prefix, suffix) = split_str_at_utf8_boundary(new_text, MAX_BYTES);
        let written = prefix.chars().count();
        let mut overflow = Self::default();
        overflow.buf.push_str(prefix);
        overflow
            .sgr
            .extend(std::iter::repeat(new_sgr).take(written));
        (Some(overflow), written, suffix)
    }

    pub(crate) fn buf_mut(&mut self) -> &mut ArrayString<MAX_BYTES> {
        &mut self.buf
    }

    pub(crate) fn sgr_mut(&mut self) -> &mut ArrayVec<SgrState, MAX_BYTES> {
        &mut self.sgr
    }
}

impl std::fmt::Display for GridString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

pub(crate) fn split_str_at_utf8_boundary(s: &str, byte_idx: usize) -> (&str, &str) {
    if byte_idx >= s.len() {
        (s, "")
    } else {
        // utf-8 code points are max 4 bytes, so we only need to check last 4 bytes or fewer if the
        // string is smaller that 4 bytes
        let start = byte_idx.saturating_sub(3);
        for candidate in (start..=byte_idx).rev() {
            if s.is_char_boundary(candidate) {
                return s.split_at(candidate);
            }
        }
        unreachable!("s was not a valid utf-8 string")
    }
}

pub fn resolve_range<R: RangeBounds<usize>>(r: R, valid: Range<usize>) -> Result<Range<usize>> {
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
    buf: &'a str,
    sgr: &'a [SgrState],
}

impl<'a> GridStr<'a> {
    fn new(sgr: &'a [SgrState], buf: &'a str) -> Self {
        Self { sgr, buf }
    }

    fn from(s: &'a GridString) -> Self {
        Self {
            sgr: s.sgr(),
            buf: &s.buf,
        }
    }

    pub fn sgr(&'a self) -> &'a [SgrState] {
        self.sgr
    }

    pub fn as_str(&self) -> &'a str {
        self.buf
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn len_chars(&self) -> usize {
        self.sgr.len()
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
        assert_eq!(gs.sgr(), &[SgrState::default(); 5]);

        Ok(())
    }

    #[test]
    fn try_string_2() {
        let sgr = SgrState {
            bold: true,
            ..Default::default()
        };
        let mut gs = GridString::from_str("").unwrap();
        gs.push_str("hi hi", sgr);
        assert_eq!(gs.as_str(), "hi hi");
        assert_eq!(gs.sgr(), &[sgr; 5]);
    }

    #[test]
    fn test_string_3() {
        let default = SgrState::default();
        let bold = SgrState {
            bold: true,
            ..Default::default()
        };
        let mut gs = GridString::default();
        gs.push_str("hi hi", default);
        gs.replace_str(1, "eyo!", bold);
        assert_eq!(gs.as_str(), "heyo!");
        assert_eq!(gs.sgr(), &[default, bold, bold, bold, bold]);

        // should not extend, even if there is capacity.
        // this behavior is necessary to implement proper striped writes
        gs.clear();
        gs.push_str("abcd", SgrState::default());
        let (nwritten, rem) = gs.replace_str(2, "efgh", SgrState::default());
        assert_eq!(gs.as_str(), "abef");
        assert_eq!(nwritten, 2);
        assert_eq!(rem, "gh");

        // replace in the middle
        gs.clear();
        gs.push_str("abcde", SgrState::default());
        let (nwritten, rem) = gs.replace_str(1, "1\u{c3a9}3", SgrState::default());
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
        assert_eq!(slice.as_str(), "ello d");
        assert_eq!(slice.len_chars(), 6);
        assert_eq!(slice.sgr().len(), 6);
        assert_eq!(slice.sgr(), &[sgr; 6]);

        let slice = gs.slice_bytes(..).expect("is valid range");
        assert_eq!(gs.sgr(), &[sgr; 8]);
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
        assert_eq!(slice.sgr(), &[sgr; 1]);
        assert_eq!(slice.as_str(), "\u{c3a9}");
    }

    #[test]
    fn test_remove_range_1() {
        let mut gs = GridString::from_str("h\u{c3a9}llo").expect("should fit");
        gs.remove_char_range(..2).expect("valid range");
        assert_eq!(gs.as_str(), "llo");
        assert_eq!(gs.len_chars(), 3);
    }

    // #[test]
    // fn test_vectored_write_1() {
    //     let mut gs1 = GridString::from_str("").unwrap();
    //     let mut gs2 = GridString::from_str("").unwrap();

    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
    //     let (nwritten, rem) = striped.push_str("a longer string", SgrState::default());
    //     assert_eq!(gs1.as_str(), "a longer");
    //     assert_eq!(gs2.as_str(), " string");
    //     assert_eq!(rem, "");
    //     assert_eq!(nwritten, 15);

    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
    //     let (nwritten, rem) = striped
    //         .replace_str(0, "a longer string that won't fit", SgrState::default())
    //         .unwrap();
    //     assert_eq!(gs1.as_str(), "a longer");
    //     assert_eq!(gs2.as_str(), " string");
    //     assert_eq!(rem, " that won't fit");
    //     assert_eq!(nwritten, 15);

    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
    //     let (nwritten, rem) = striped
    //         .replace_str(
    //             0,
    //             "a long\u{c3a9}er string that won't fit, with utf-8 boundary",
    //             SgrState::default(),
    //         )
    //         .unwrap();
    //     assert_eq!(gs1.as_str(), "a long");
    //     assert_eq!(gs2.as_str(), "\u{c3a9}er st");
    //     assert_eq!(rem, "ring that won't fit, with utf-8 boundary");
    //     assert_eq!(nwritten, 12);

    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
    //     let (nwritten, rem) = striped.replace_str(6, "er", SgrState::default()).unwrap();
    //     assert_eq!(gs1.as_str(), "a long");
    //     assert_eq!(gs2.as_str(), "err st");
    //     assert_eq!(rem, "");
    //     assert_eq!(nwritten, 2);

    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
    //     assert!(striped
    //         .replace_str(8, ", fit", SgrState::default())
    //         .is_err());
    //     assert_eq!(gs1.as_str(), "a long");
    //     assert_eq!(gs2.as_str(), "err st");
    // }

    // #[test]
    // fn test_vectored_write_2() {
    //     let mut bold_italic = SgrState {
    //         italic: true,
    //         bold: true,
    //         ..Default::default()
    //     };
    //     let default = SgrState::default();

    //     let mut gs1 = GridString::from_str("").unwrap();
    //     let mut gs2 = GridString::from_str("").unwrap();
    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);

    //     let text = "abcdefghijklmn";
    //     striped.push_str(text, SgrState::default());

    //     let mut striped = StripedString::from_iter([&mut gs1, &mut gs2]);
    //     striped.replace_str(5, "123456", bold_italic).unwrap();
    //     assert_eq!(gs1.as_str(), "abcde123");
    //     assert_eq!(
    //         gs1.sgr(),
    //         &[
    //             default,
    //             default,
    //             default,
    //             default,
    //             default,
    //             bold_italic,
    //             bold_italic,
    //             bold_italic,
    //         ]
    //     );
    //     assert_eq!(gs2.as_str(), "456lmn");
    //     assert_eq!(
    //         gs2.sgr(),
    //         &[
    //             bold_italic,
    //             bold_italic,
    //             bold_italic,
    //             default,
    //             default,
    //             default,
    //         ]
    //     )
    // }

    #[test]
    fn test_insert_1() {
        let mut gs = GridString::from_str("abcd").unwrap();
        let (overflow, _, rest) = gs.insert_str(2, "12345", SgrState::default());
        assert_eq!(gs.as_str(), "ab12345c");
        assert_eq!(rest, "");
        assert_eq!(overflow.as_ref().map(GridString::as_str), Some("d"));

        let (overflow, _, rest) = gs.insert_str(7, "6", SgrState::default());
        assert_eq!(gs.as_str(), "ab123456");
        assert_eq!(rest, "");
        assert_eq!(overflow.as_ref().map(GridString::as_str), Some("c"));

        let (overflow, _, rest) = gs.insert_str(8, "cd", SgrState::default());
        assert_eq!(gs.as_str(), "ab123456");
        assert_eq!(rest, "");
        assert_eq!(overflow.as_ref().map(GridString::as_str), Some("cd"));
    }

    #[test]
    fn test_insert_2() {
        let mut gs = GridString::from_str("❯~").unwrap();
        let _ = gs.insert_str(1, "hi", SgrState::default());
        assert_eq!(gs.as_str(), "❯hi~");

        let _ = gs.insert_str(4, "a", SgrState::default());
        assert_eq!(gs.as_str(), "❯hi~a");
    }

    #[test]
    fn test_insert_3() {
        // multi-byte char causing len_chars < capacity (8)
        let mut gs = GridString::from_str("❯45678").unwrap();
        let (overflow, _, rem) = gs.insert_str(1, "hi", SgrState::default());
        assert_eq!(gs.as_str(), "❯hi456");
        assert_eq!(overflow.as_ref().map(GridString::as_str), Some("78"));
        assert_eq!(rem, "");

        // there's some room at the end, but the first char won't fit
        let mut gs = GridString::from_str("123456").unwrap();
        let (overflow, _, rem) = gs.insert_str(6, "❯", SgrState::default());
        assert_eq!(overflow.as_ref().map(GridString::as_str), Some("❯"));
        assert_eq!(rem, "");
    }
}
