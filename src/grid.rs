use std::{
    collections::BTreeMap,
    ops::{Range, RangeBounds},
    rc::Rc,
};

use crate::puffin;

use crate::ansi;

#[derive(Debug)]
pub struct Grid2 {
    num_rows: ScreenCoord,
    num_cols: ScreenCoord,
    max_rows: BufferCoord, // always >= num_rows
    blank_line: Rc<String>,

    text: ropey::Rope,

    // TODO: ArrayVec<CursorState; 2> maybe?
    cursor_state: CursorState,
    saved_cursor_state: Option<CursorState>,
}

#[derive(Debug)]
pub struct DisplayLine<'a> {
    pub padded_text: String, // always enough to fill the grid line
    pub format_attributes: Vec<FormatAttribute<'a>>,
}

#[derive(Debug, Clone)]
pub struct FormatAttribute<'a> {
    pub sgr_state: &'a SgrState,
    pub byte_range: Range<usize>,
}

// Goals:
//  - addign to grid while full should be fast
//
// Nice to haves:
//  - can handle multi-unicode point glyphs correctly
//  - can do re-flow on resize with soft wrapping
//
// Invariants
//  - max_rows >= num_rows
//  - each newline separated line in the rope is exactly num_cols + 1 chars wide
impl Grid2 {
    #[cfg(test)]
    const FILL_CHAR: u8 = b'-';

    #[cfg(not(test))]
    const FILL_CHAR: u8 = b'-';

    pub fn new(num_rows: usize, num_cols: usize) -> Self {
        let cursor_state = CursorState::default();
        let saved_cursor_state = None;
        let blank_line = Rc::new(String::from_iter(
            std::iter::repeat(Self::FILL_CHAR as char).take(num_cols),
        ));
        let text = Self::rope_with_n_blank_lines(num_rows, blank_line.as_ref());

        Self {
            num_rows: ScreenCoord(num_rows),
            num_cols: ScreenCoord(num_cols),
            max_rows: BufferCoord(num_rows),
            text,
            cursor_state,
            saved_cursor_state,
            blank_line,
        }
    }

    pub fn num_rows(&self) -> usize {
        self.num_rows.0
    }

    pub fn num_cols(&self) -> usize {
        self.num_cols.0
    }

    // in range num_rows..=max_scrollback_rows
    pub fn num_current_display_rows(&self) -> usize {
        // TODO: will need to use len_graphemes() later
        // -1 for the last newline
        self.text.len_lines() - 1
    }

    pub fn cursor_position(&self) -> (usize, usize) {
        let (ScreenCoord(row), ScreenCoord(col)) = self.cursor_state.position;
        (row, col)
    }

    // for the grid: 24 rows x 80 cols with a max_scrollback_rows 100.
    //  - imagine there are 80 total rows currently in the buffer
    //  - first visible line no will be: 80 - 24 = 56
    //  - visible line range will be 56..80
    pub fn first_visible_line_no(&self) -> usize {
        self.num_current_display_rows()
            .saturating_sub(self.num_rows())
    }

    pub fn save_cursor_state(&mut self) {
        self.saved_cursor_state = Some(self.cursor_state);
    }

    pub fn restore_cursor_state(&mut self) {
        if let Some(saved) = self.saved_cursor_state.take() {
            self.cursor_state = saved;
        }
    }

    pub fn max_scrollback_lines(&mut self, n: usize) {
        self.max_rows = BufferCoord(n.max(self.num_rows.0));
    }

    pub fn clear_screen(&mut self) {
        puffin::profile_function!();
        let line_idx = self.first_visible_line_no();
        let char_idx = self.text.line_to_char(line_idx);
        let num_rows = self.num_rows();
        self.text.split_off(char_idx);
        Self::append_n_blank_rows(&mut self.text, self.blank_line.as_ref(), num_rows);
    }

    pub fn clear_including_scrollback(&mut self) {
        puffin::profile_function!();
        self.text = Self::rope_with_n_blank_lines(self.num_rows(), self.blank_line.as_ref());
        self.cursor_state = CursorState::default();
    }

    pub fn move_cursor_relative(&mut self, dr: isize, dc: isize) {
        let (row, col) = self.cursor_state.position;
        let mut new_row = row + dr;
        let mut new_col = col + dc;
        new_row += new_col / self.num_cols();
        new_col %= self.num_cols();
        self.move_cursor(new_row.0, new_col.0)
    }

    pub fn move_cursor(&mut self, new_row: usize, new_col: usize) {
        puffin::profile_function!();
        assert!(new_col < self.num_cols());
        self.cursor_state.pending_wrap = false;

        // new position is within bounds
        if new_row < self.num_rows() {
            self.cursor_state.position = (ScreenCoord(new_row), ScreenCoord(new_col));
            return;
        }

        let num_new_rows = new_row - self.num_rows() + 1;
        {
            puffin::profile_scope!("append_n_blank_rows");
            Self::append_n_blank_rows(&mut self.text, self.blank_line.as_ref(), num_new_rows);
        }
        if let Some(to_remove) = self.text.len_lines().checked_sub(self.max_rows.0 + 1) {
            puffin::profile_scope!("remove_screen_rows");
            self.remove_screen_rows(ScreenCoord(0)..ScreenCoord(to_remove));
        }
        self.cursor_state.position = (self.num_rows - 1, ScreenCoord(new_col));
    }

    pub fn cursor_format_mut(&mut self) -> &mut SgrState {
        &mut self.cursor_state.sgr_state
    }

    pub fn resize(&mut self, new_num_rows: usize, new_num_cols: usize) -> bool {
        if self.num_rows() == new_num_rows && self.num_cols() == new_num_cols {
            return false;
        }
        log::info!(
            "resize: {:?} x {:?} -> {new_num_rows} x {new_num_cols}",
            self.num_rows,
            self.num_cols
        );

        let new_blank_line =
            String::from_iter(std::iter::repeat(Self::FILL_CHAR as char).take(new_num_cols));

        // add new blank lines at the end if we are resizing to have more rows that we have now
        if let Some(to_add) = new_num_rows.checked_sub(self.num_rows()) {
            Self::append_n_blank_rows(&mut self.text, &new_blank_line, to_add);
        }

        // need to maintain the invariant that each line is num_cols + 1 long

        match new_num_cols.cmp(&self.num_cols()) {
            std::cmp::Ordering::Equal => (),
            std::cmp::Ordering::Less => {
                // need to truncate all lines
                for line_idx in 0..self.num_current_display_rows() {
                    let char_idx = self.text.line_to_char(line_idx);
                    let remove_start = char_idx + self.num_cols();
                    let remove_end = self.text.line_to_char(line_idx + 1) - 1;
                    self.text.remove(remove_start..remove_end);
                }
            }
            std::cmp::Ordering::Greater => {
                // need to pad lines with blanks, while preserving newline at end
                let n = new_num_cols - self.num_cols();
                let pad_str = &new_blank_line[0..n];
                for line_idx in 1..self.num_current_display_rows() {
                    let char_idx = self.text.line_to_char(line_idx);
                    self.text.insert(char_idx - 1, pad_str);
                }
            }
        }

        self.num_rows = ScreenCoord(new_num_rows);
        self.max_rows = BufferCoord(new_num_rows.max(self.max_rows.0));
        self.num_cols = ScreenCoord(new_num_cols);
        self.cursor_state
            .clamp_position(self.num_rows, self.num_cols);
        self.blank_line = Rc::new(new_blank_line);
        debug_assert!(self.blank_line.len() == new_num_cols);

        true
    }

    pub fn write_text_at_cursor(&mut self, txt: &str) {
        puffin::profile_function!();

        debug_assert!(!txt.contains('\n'));

        // before inserting txt, check if a wrap is pending
        if self.cursor_state.pending_wrap {
            let (ScreenCoord(row), _) = self.cursor_state.position;
            // moving will clear the pending wrap flag
            self.move_cursor(row + 1, 0);
        }

        let mut to_write = txt;
        while !to_write.is_empty() {
            let (_, cur_col) = self.cursor_position();

            // calculate the substring to write on the current line
            let space_left_on_line = self.num_cols() - cur_col;
            let prefix_to_write =
                // TODO: chars.count() -> graphemes.count()
                if let Some((byte_idx, _)) = to_write.char_indices().nth(space_left_on_line) {
                    &to_write[..byte_idx]
                } else {
                    to_write
                };

            let BufferCoord(edit_point) = self.screen_pos_to_char_idx(self.cursor_state.position);
            let edit_len = prefix_to_write.chars().count();
            self.text.remove(edit_point..edit_point + edit_len);
            self.text.insert(edit_point, prefix_to_write);
            to_write = &to_write[prefix_to_write.len()..];

            if to_write.is_empty() && cur_col + edit_len == self.num_cols() {
                self.move_cursor_relative(0, edit_len as isize - 1);
                self.cursor_state.pending_wrap = true;
            } else {
                self.move_cursor_relative(0, edit_len as isize);
            }
        }
    }

    // scan the rope counting the number of display lines (with soft-wrapping) until
    // we reach the beginning of query_range. Then take query_range.len() number of lines.
    // Note that a display line may begin the middle of a rope line because of soft-wrapping.
    pub fn display_lines<'a, R: RangeBounds<usize>>(
        &'a self,
        query_range: R,
    ) -> impl Iterator<Item = DisplayLine> + ExactSizeIterator + 'a {
        let query_range = resolve_range(
            query_range,
            // -1 for last newline
            0..self.text.len_lines() - 1,
        )
        .expect("query_range is out of bounds");

        // TODO: actual SgrState
        let fake = Box::leak(Box::new(SgrState::default()));

        self.text
            .lines_at(query_range.start)
            .take(query_range.len())
            .map(|display_slice: ropey::RopeSlice<'a>| {
                let mut padded_text = display_slice.to_string();
                // pop off the newline
                padded_text.pop();

                let byte_range = 0..padded_text.len();
                DisplayLine {
                    padded_text,
                    format_attributes: vec![FormatAttribute {
                        sgr_state: fake,
                        byte_range,
                    }],
                }
            })
    }

    pub fn erase_from_cursor_to_eol(&mut self) {
        let (row, _) = self.cursor_state.position;
        self.erase(self.cursor_state.position, row + 1);
    }

    pub fn erase_from_cursor_to_screen(&mut self) {
        self.erase(self.cursor_state.position, self.num_rows);
    }

    pub fn move_lines_down(&mut self, start_line_no: usize, n: usize) {
        assert!(start_line_no < self.num_rows());
        let char_idx = self.text.line_to_char(start_line_no);
        Self::insert_n_blank_rows(
            &mut self.text,
            self.blank_line.as_ref(),
            BufferCoord(char_idx),
            n,
        );
        self.remove_screen_rows((self.num_rows - n as isize)..self.num_rows);
    }

    fn rope_with_n_blank_lines(num_rows: usize, blank_line: &str) -> ropey::Rope {
        puffin::profile_function!();
        let mut builder = ropey::RopeBuilder::new();
        for _ in 0..num_rows {
            builder.append(blank_line);
            builder.append("\n");
        }
        builder.finish()
    }

    fn append_n_blank_rows(text: &mut ropey::Rope, blank_line: &str, num_rows: usize) {
        text.append(Self::rope_with_n_blank_lines(num_rows, blank_line));
    }

    fn insert_n_blank_rows(
        text: &mut ropey::Rope,
        blank_line: &str,
        start_char_idx: BufferCoord,
        n: usize,
    ) {
        let right = text.split_off(start_char_idx.0);
        text.append(Self::rope_with_n_blank_lines(n, blank_line));
        text.append(right);
    }

    fn remove_screen_rows(
        &mut self,
        Range {
            start: ScreenCoord(start),
            end: ScreenCoord(end),
        }: Range<ScreenCoord>,
    ) {
        let start_idx = self.text.line_to_char(start);
        let end_idx = self.text.line_to_char(end);
        self.text.remove(start_idx..end_idx)
    }

    fn erase(&mut self, from: (ScreenCoord, ScreenCoord), to_row: ScreenCoord) {
        let (from_row, _) = from;
        assert!(to_row >= from_row);

        // starting partial row
        let BufferCoord(row_start) = self.screen_pos_to_char_idx(from);
        let BufferCoord(row_end) = self.screen_pos_to_char_idx((from_row + 1, ScreenCoord(0)));
        let row_end = row_end - 1; // \n
        self.text.remove(row_start..row_end);
        self.text
            .insert(row_start, &self.blank_line[..row_end - row_start]);

        // remaining complete rows
        self.remove_screen_rows(from_row + 1..to_row);
        Self::append_n_blank_rows(
            &mut self.text,
            self.blank_line.as_ref(),
            to_row.0 - from_row.0 - 1,
        );
    }

    fn screen_pos_to_char_idx(
        &self,
        (ScreenCoord(row), ScreenCoord(col)): (ScreenCoord, ScreenCoord),
    ) -> BufferCoord {
        let line_idx = self.first_visible_line_no() + row;
        let char_idx = self.text.line_to_char(line_idx);
        BufferCoord(char_idx + col)
    }

    #[cfg(test)]
    fn get_line(&self, line_idx: usize) -> Option<String> {
        self.text
            .get_lines_at(line_idx)?
            .next()
            .map(|rslice| rslice.to_string())
    }
}

fn resolve_range<R: RangeBounds<usize>>(
    query_range: R,
    valid_range: Range<usize>,
) -> anyhow::Result<Range<usize>> {
    use std::ops::Bound::{Excluded, Included, Unbounded};

    let start = match query_range.start_bound() {
        Included(&n) => n,
        Excluded(&n) => n + 1,
        Unbounded => valid_range.start,
    };
    let end = match query_range.end_bound() {
        Included(&n) => n + 1,
        Excluded(&n) => n,
        Unbounded => valid_range.end,
    };

    if start > end {
        anyhow::bail!(
            "range out of bounds: start {} is smaller than end: {:?}",
            start,
            end,
        );
    }
    if start < valid_range.start {
        anyhow::bail!(
            "range out of bounds: {} is outside valid_range: {:?}",
            start,
            valid_range
        );
    }
    if end > valid_range.end {
        anyhow::bail!(
            "range out of bounds: {} is outside valid_range: {:?}",
            end,
            valid_range
        );
    }
    Ok(start..end)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SgrState {
    pub fg_color: ansi::Color,
    pub bg_color: ansi::Color,
    pub bold: bool,
    pub italic: bool,
}

impl Default for SgrState {
    fn default() -> Self {
        Self {
            fg_color: ansi::Color::DefaultFg,
            bg_color: ansi::Color::DefaultBg,
            bold: false,
            italic: false,
        }
    }
}

// #[derive(Debug)]
// struct RingBuffer<T> {
//     // VecDeque's capacity is different from what we pass in
//     capacity: usize,
//     inner: VecDeque<T>,
// }

// impl<T> RingBuffer<T>
// where
//     T: Clone,
// {
//     pub fn with_capacity(capacity: usize) -> Self {
//         Self {
//             capacity,
//             inner: VecDeque::new(), // allocate lazily
//         }
//     }

//     pub fn push(&mut self, item: T) {
//         if self.inner.len() == self.capacity {
//             self.inner.pop_front();
//             self.inner.push_back(item);
//             debug_assert!(self.inner.len() == self.capacity);
//         } else {
//             self.inner.push_back(item);
//         }
//     }

//     pub fn pop(&mut self) -> Option<T> {
//         self.inner.pop_front()
//     }

//     fn fill(&mut self, item: T) {
//         self.inner.clear();
//         for _ in 0..self.capacity {
//             self.push(item.clone());
//         }
//     }

//     fn len(&self) -> usize {
//         self.inner.len()
//     }

//     fn capacity(&self) -> usize {
//         self.capacity
//     }

//     fn iter(&self) -> impl ExactSizeIterator<Item = &T> {
//         self.inner.iter()
//     }

//     fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> + DoubleEndedIterator {
//         self.inner.iter_mut()
//     }
//     // fn resize_with(&mut self, new_len: usize, fill_elem: &T) {
//     //     match self.len().cmp(&new_len) {
//     //         std::cmp::Ordering::Equal => return,
//     //         std::cmp::Ordering::Less => {
//     //             let additional = new_len - self.len();
//     //             self.inner.reserve(additional);
//     //             self.inner.resize_with(new_len, || fill_elem.clone());
//     //         }
//     //         std::cmp::Ordering::Greater => {
//     //             let to_remove = self.len() - new_len;
//     //             self.inner.drain(..to_remove);
//     //         }
//     //     }
//     // }

//     fn ensure_capacity(&mut self, new_capacity: usize) {
//         self.capacity = std::cmp::max(self.capacity, new_capacity);
//     }

//     fn clear(&mut self) {
//         self.inner.clear()
//     }

//     fn remove<R: RangeBounds<usize>>(&mut self, r: R) {
//         self.inner.drain(r);
//     }
// }

#[derive(Debug, Clone)]
struct Rope<T> {
    nodes: BTreeMap<usize, T>,
}
impl<T> Rope<T> {
    fn new() -> Self {
        Self {
            nodes: BTreeMap::default(),
        }
    }

    fn ordered_iter<'a>(&'a self) -> impl Iterator<Item = (usize, &'a T)> + 'a {
        self.nodes.iter().map(|(idx, item)| (*idx, item))
    }

    fn clear(&mut self) {
        self.nodes.clear();
    }
}

// #[derive(Debug, Clone)]
// enum LineText {
//     Empty(Rc<String>),
//     Ascii(String),
//     Utf8(String),
// }

// impl LineText {
//     fn as_str(&self) -> &str {
//         match self {
//             LineText::Empty(t) => t.as_ref(),
//             LineText::Ascii(t) => t.as_str(),
//             LineText::Utf8(t) => t.as_str(),
//         }
//     }

//     fn resize_with(&mut self, new_num_cols: usize, blank_line: &Rc<String>) {
//         // TODO: do a grpaheme count instead to account for glyphs that takes
//         // multiple unicode code points
//         let cur_cols = self.num_cols();
//         match cur_cols.cmp(&new_num_cols) {
//             std::cmp::Ordering::Equal => return,
//             std::cmp::Ordering::Less => {
//                 self.extend(blank_line.chars().take(new_num_cols - cur_cols));
//             }
//             std::cmp::Ordering::Greater => match self {
//                 LineText::Ascii(txt) => txt.truncate(new_num_cols),
//                 LineText::Utf8(txt) => *txt = txt.chars().take(new_num_cols).collect(),
//                 LineText::Empty(txt) => *txt = Rc::clone(blank_line),
//             },
//         }
//     }

//     fn num_cols(&self) -> usize {
//         match self {
//             LineText::Ascii(txt) => txt.len(),
//             LineText::Utf8(txt) => txt.chars().count(),
//             LineText::Empty(txt) => {
//                 debug_assert!(txt.is_ascii());
//                 txt.len()
//             }
//         }
//     }

//     fn extend(&mut self, additional: impl Iterator<Item = char>) {
//         match self {
//             LineText::Ascii(txt) => txt.extend(additional),
//             LineText::Utf8(txt) => txt.extend(additional),
//             LineText::Empty(_) => {
//                 let new_contents = additional.collect::<String>();
//                 *self = if new_contents.is_ascii() {
//                     LineText::Ascii(new_contents)
//                 } else {
//                     LineText::Utf8(new_contents)
//                 };
//             }
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Line {
//     text: ropey::Rope,
//     format_attributes: Rope<SgrState>,
// }

// impl Line {
//     pub fn text_and_format<'a>(&'a self) -> (String, Vec<FormatAttribute<'a>>) {
//         let wrapped = self.text.to_string();
//         let attrs = self.format_attributes().collect();
//         (wrapped, attrs)
//     }

//     fn format_attributes<'a>(&'a self) -> impl Iterator<Item = FormatAttribute<'a>> + 'a {
//         let mut iter = self.format_attributes.ordered_iter().peekable();
//         let mut start = iter.next();

//         std::iter::from_fn(move || -> Option<FormatAttribute> {
//             let (start_off, sgr_state) = start?;
//             if let Some((next_start_off, _)) = iter.peek() {
//                 let attr = FormatAttribute {
//                     sgr_state,
//                     byte_range: start_off..*next_start_off,
//                 };
//                 start = iter.next();
//                 Some(attr)
//             } else {
//                 let end_off = self.text.len_bytes();
//                 Some(FormatAttribute {
//                     sgr_state,
//                     byte_range: start_off..end_off,
//                 })
//             }
//         })
//     }

//     fn empty(blank_line: &str) -> Self {
//         Self {
//             text: ropey::Rope::from_str(blank_line),
//             format_attributes: Rope::<SgrState>::new(),
//         }
//     }

//     fn append_str(&mut self, to_append: &str) {
//         self.text.insert(self.text.len_chars(), to_append);
//     }
// }

#[derive(Debug, Default, Copy, Clone)]
struct CursorState {
    position: (ScreenCoord, ScreenCoord), // in the range (0..num_rows, 0..num_cols)
    sgr_state: SgrState,
    pending_wrap: bool,
}
impl CursorState {
    fn clamp_position(&mut self, new_num_rows: ScreenCoord, new_num_cols: ScreenCoord) {
        let (r, c) = &mut self.position;
        *r = std::cmp::min(*r, new_num_rows - 1);
        *c = std::cmp::min(*c, new_num_cols - 1);
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ScreenCoord(usize); // always in the range [0..grid.num_rows]

impl std::ops::Add<isize> for ScreenCoord {
    type Output = Self;

    fn add(self, rhs: isize) -> Self::Output {
        Self((self.0 as isize + rhs) as usize)
    }
}
impl std::ops::AddAssign<usize> for ScreenCoord {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}
impl std::ops::Sub<isize> for ScreenCoord {
    type Output = Self;

    fn sub(self, rhs: isize) -> Self::Output {
        Self((self.0 as isize).saturating_sub(rhs) as usize)
    }
}
impl std::ops::Div<usize> for ScreenCoord {
    type Output = usize;

    fn div(self, rhs: usize) -> Self::Output {
        self.0 / rhs
    }
}
impl std::ops::Rem<usize> for ScreenCoord {
    type Output = usize;

    fn rem(self, rhs: usize) -> Self::Output {
        self.0 % rhs
    }
}
impl std::ops::RemAssign<usize> for ScreenCoord {
    fn rem_assign(&mut self, rhs: usize) {
        self.0 %= rhs;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BufferCoord(usize); // always in the range [0..grid.max_rows*grid.num_cols)

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rope_counts_newlines_as_chars() {
        let mut r = ropey::Rope::from_str("a12\nb");
        assert_eq!(r.len_chars(), 5);
        assert_eq!(r.lines().next().unwrap().to_string().as_str(), "a12\n");
        assert_eq!(
            r.lines().next().unwrap().slice(1..).to_string().as_str(),
            "12\n"
        );
        let line = r.lines().nth(1).unwrap();
        assert_eq!(line.byte_to_char(0), 0);

        let line_char_idx = r.line_to_char(1);
        assert_eq!(line_char_idx, 4);
        r.insert(line_char_idx - 1, "foo");
        assert_eq!(&r.to_string(), "a12foo\nb");

        let right = r.split_off(4);
        assert_eq!(&right.to_string(), "oo\nb");
    }

    #[test]
    fn test_grid_1() {
        let grid = Grid2::new(3, 10);
        assert_eq!(grid.display_lines(..).count(), 3);
        assert_is_blank(&grid, 0..3);
    }

    #[test]
    fn test_grid_2() {
        let mut grid = Grid2::new(3, 10);
        grid.write_text_at_cursor("foo");

        assert_nth_line(&grid, 0, "foo-------");
        assert_is_blank(&grid, 1..3);
        assert_eq!(grid.cursor_position(), (0, 3));

        // cause wrapping
        grid.write_text_at_cursor(" bar baz");
        assert_eq!(grid.cursor_position(), (1, 1));
        assert_nth_line(&grid, 0, "foo bar ba");
        assert_nth_line(&grid, 1, "z---------");

        // check pending wrapping
        grid.write_text_at_cursor("aaaaaaaaa");
        assert_eq!(grid.cursor_position(), (1, 9));
        grid.write_text_at_cursor("b");
        assert_nth_line(&grid, 2, "b---------");

        // check buffer overflow + truncation
        grid.write_text_at_cursor("ccccccccc");
        assert_nth_line(&grid, 2, "bccccccccc");
        grid.write_text_at_cursor("d");
        assert_nth_line(&grid, 0, "zaaaaaaaaa");
        assert_nth_line(&grid, 1, "bccccccccc");
        assert_nth_line(&grid, 2, "d---------");
    }

    #[test]
    fn test_grid_3() {
        let mut grid = Grid2::new(3, 10);
        grid.max_scrollback_lines(100);
        grid.resize(5, 10);
        assert_eq!(grid.num_current_display_rows(), 5);

        grid.write_text_at_cursor("aaaaaaaaaa");
        grid.write_text_at_cursor("bbbbbbbbbb");
        grid.write_text_at_cursor("cccccccccc");
        grid.write_text_at_cursor("dddddddddd");
        grid.write_text_at_cursor("eeeeeeeeee");
        grid.write_text_at_cursor("ffffffffff");
        assert_eq!(grid.num_current_display_rows(), 6);
        assert_nth_line(&grid, 0, "aaaaaaaaaa");
        assert_nth_line(&grid, 5, "ffffffffff");
    }

    fn assert_nth_line(grid: &Grid2, line_idx: usize, expected: &str) {
        let line = grid.display_lines(line_idx..).next().unwrap().padded_text;
        assert_eq!(&line, expected);
    }

    fn assert_is_blank(grid: &Grid2, line_range: Range<usize>) {
        let lines = grid.display_lines(line_range.clone());
        assert_eq!(lines.len(), line_range.len());

        let blank_line = grid.blank_line.to_string();
        for (line_idx, line) in line_range.zip(lines) {
            assert_eq!(
                line.padded_text.as_str(),
                &blank_line,
                "for line: {line_idx}"
            );
        }
    }
}
