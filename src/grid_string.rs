#![allow(unused, dead_code)]

use std::{
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

    pub fn capacity(&self) -> usize {
        self.buf.capacity()
    }

    fn try_push_str(&mut self, s: &str) -> Result<()> {
        self.buf
            .try_push_str(s)
            .map_err(|inner| GridStringError::CapacityError(inner.simplify()))?;
        self.nchars += s.chars().count() as u8;
        Ok(())
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

    if start > valid.end {
        return Err(GridStringError::OutOfBounds {
            which: "start",
            given: start,
            bound: valid.end,
        });
    }

    if end > valid.end {
        return Err(GridStringError::OutOfBounds {
            which: "end",
            given: end,
            bound: valid.end,
        });
    }

    if start > end {
        return Err(GridStringError::OutOfBounds {
            which: "start",
            given: start,
            bound: end,
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

#[derive(Debug, Clone, Copy)]
pub enum GridStringError {
    CapacityError(arrayvec::CapacityError),
    OutOfBounds {
        which: &'static str,
        given: usize,
        bound: usize,
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
                bound,
            } => {
                write!(f, "out of bounds: {which} given: {given}, bound: {bound}")
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
        gs.try_push_str("hi hi").expect("string should fit");
        assert_eq!(gs.as_str(), "hi hi");
        assert_eq!(gs.sgr(), sgr);
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
}
