use std::{
    num::NonZeroUsize,
    ops::{Range, RangeBounds},
    rc::Rc,
};

use crate::{
    puffin,
    terminal_emulator::SgrState,
    tree::{self, Tree},
};

#[derive(Debug)]
pub struct Grid {
    num_screen_rows: ScreenCoord,
    num_screen_cols: ScreenCoord,
    max_rows: BufferCoord, // always >= num_screen_rows
    blank_line: Rc<String>,

    text: tree::Tree,

    // TODO: ArrayVec<CursorState; 2> maybe?
    cursor_state: CursorState,
    saved_cursor_state: Option<CursorState>,
}

#[derive(Debug)]
pub struct DisplayLine {
    pub padded_text: String, // always enough to fill the grid line
    pub format_attributes: Vec<FormatAttribute>,
}

#[derive(Debug, Clone)]
pub struct FormatAttribute {
    pub sgr_state: SgrState,
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
impl Grid {
    #[cfg(test)]
    const FILL_CHAR: u8 = b'-';

    #[cfg(not(test))]
    const FILL_CHAR: u8 = b' ';

    pub fn new(num_rows: usize, num_cols: usize) -> Self {
        let cursor_state = CursorState::default();
        let saved_cursor_state = None;
        let blank_line = Rc::new(String::from_iter(
            std::iter::repeat(Self::FILL_CHAR as char)
                .take(num_cols)
                .chain(std::iter::once('\n')),
        ));
        let text = Tree::new();

        Self {
            num_screen_rows: ScreenCoord(num_rows),
            num_screen_cols: ScreenCoord(num_cols),
            max_rows: BufferCoord(num_rows),
            text,
            cursor_state,
            saved_cursor_state,
            blank_line,
        }
    }

    pub fn num_rows(&self) -> usize {
        self.num_screen_rows.0
    }

    pub fn num_cols(&self) -> usize {
        self.num_screen_cols.0
    }

    // in range num_rows..=max_scrollback_rows
    pub fn total_rows(&self) -> usize {
        // TODO: will need to use len_graphemes() later
        self.max_cursor_position().total_rows()
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
        self.total_rows().saturating_sub(self.num_rows())
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
        self.max_rows = BufferCoord(n.max(self.num_screen_rows.0));
    }

    pub fn clear_screen(&mut self) {
        puffin::profile_function!();
        let line_idx = tree::SeekSoftWrapPosition::new(
            self.num_cols()
                .try_into()
                .expect("num_cols must be non-zero"),
            self.first_visible_line_no(),
        );
        self.text.remove_range(line_idx..);
        for _ in 0..self.num_rows() {
            self.text
                .push_str(self.blank_line.as_ref(), SgrState::default());
        }
    }

    pub fn clear_including_scrollback(&mut self) {
        puffin::profile_function!();
        self.text = Self::rope_with_n_blank_lines(self.num_rows(), self.blank_line.as_ref());
        self.cursor_state = CursorState::default();
    }

    pub fn move_cursor_relative(&mut self, dr: isize, dc: isize) {
        let (row, col) = self.cursor_position();
        let mut new_row = (row as isize + dr) as usize;
        let mut new_col = (col as isize + dc) as usize;
        new_row += new_col / self.num_cols();
        new_col %= self.num_cols();
        self.move_cursor(new_row, new_col)
    }

    pub fn move_cursor(&mut self, new_row: usize, new_col: usize) {
        puffin::profile_function!();
        assert!(new_col < self.num_cols());
        self.cursor_state.pending_wrap = false;

        // if let Some(n) = new_row.checked_sub(self.num_rows()) {
        //     let rows_to_add = n + 1;
        //     for _ in 0..rows_to_add {
        //         self.text.push_str(&self.blank_line, SgrState::default());
        //     }
        // }
        self.cursor_state.position = (ScreenCoord(new_row), ScreenCoord(new_col));
        self.cursor_state
            .clamp_position(self.num_rows(), self.num_cols());
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
            self.num_screen_rows,
            self.num_screen_cols
        );

        let new_blank_line = String::from_iter(
            std::iter::repeat(Self::FILL_CHAR as char)
                .take(new_num_cols)
                .chain(std::iter::once('\n')),
        );
        self.blank_line = Rc::new(new_blank_line);

        self.num_screen_rows = ScreenCoord(new_num_rows);
        self.num_screen_cols = ScreenCoord(new_num_cols);
        self.cursor_state
            .clamp_position(self.num_rows(), self.num_cols());

        let new_num_rows = BufferCoord(new_num_rows);
        self.max_rows = self.max_rows.max(new_num_rows);

        true
    }

    pub fn write_text_at_cursor(&mut self, txt: &str) {
        puffin::profile_function!();

        debug_assert!(!txt.contains('\n'));

        // before inserting txt, check if a wrap is pending
        if self.cursor_state.pending_wrap {
            // moving will clear the pending wrap flag
            let (row, _) = self.cursor_position();
            self.move_cursor(row + 1, 0);
        }

        self.text.replace_str(
            self.screen_to_buffer_pos(),
            txt,
            self.cursor_state.sgr_state,
        );
        let edit_len = txt.chars().count();
        let (_, cur_col) = self.cursor_position();
        if cur_col + (edit_len % self.num_cols()) == self.num_cols() {
            self.move_cursor_relative(0, edit_len as isize - 1);
            self.cursor_state.pending_wrap = true;
        } else {
            self.move_cursor_relative(0, edit_len as isize);
        }
    }

    // scan the rope counting the number of display lines (with soft-wrapping) until
    // we reach the beginning of query_range. Then take query_range.len() number of lines.
    // Note that a display line may begin the middle of a rope line because of soft-wrapping.
    pub fn display_lines<'a, R: RangeBounds<usize>>(
        &'a self,
        query_range: R,
    ) -> impl Iterator<Item = DisplayLine> + 'a {
        puffin::profile_function!();
        self.text
            .iter_soft_wrapped_lines(
                NonZeroUsize::new(self.num_cols()).expect("columns must be non-zero"),
                query_range,
            )
            .expect("query_range to be valid")
            .map(|display_slice: tree::TreeSlice| {
                let mut padded_text: String = display_slice.text;
                // pop off the newline since that is not rendered
                if padded_text.as_bytes().last() == Some(&b'\n') {
                    padded_text.pop();
                }

                let mut format_attributes = vec![];
                let mut cur = None;
                for (sgr_state, ch) in display_slice.sgr.into_iter().zip(padded_text.chars()) {
                    let state = cur.get_or_insert_with(|| FormatAttribute {
                        sgr_state,
                        byte_range: 0..0,
                    });
                    if state.sgr_state == sgr_state {
                        state.byte_range.end += ch.len_utf8();
                    } else {
                        let x = std::mem::replace(
                            state,
                            FormatAttribute {
                                sgr_state,
                                byte_range: state.byte_range.end
                                    ..state.byte_range.end + ch.len_utf8(),
                            },
                        );
                        format_attributes.push(x);
                    }
                }
                if let Some(state) = cur.take() {
                    format_attributes.push(state);
                }

                // add padding so that the line is at least num_cols wide
                {
                    let n = padded_text.chars().count();
                    if let Some(to_add) = self.num_cols().checked_sub(n) {
                        let blanks = &self.blank_line[..to_add];
                        if let Some(FormatAttribute { byte_range, .. }) = format_attributes
                            .last_mut()
                            .filter(|format| format.sgr_state == SgrState::default())
                        {
                            byte_range.end += blanks.len();
                        } else {
                            format_attributes.push(FormatAttribute {
                                sgr_state: SgrState::default(),
                                byte_range: padded_text.len()..padded_text.len() + blanks.len(),
                            });
                        }
                        padded_text.push_str(blanks);
                    }
                }

                DisplayLine {
                    padded_text,
                    format_attributes,
                }
            })
    }

    pub fn erase_from_cursor_to_eol(&mut self) {
        let cursor_pos = self.screen_to_buffer_pos();
        let mut next_line = cursor_pos.clone();
        next_line.line_idx = cursor_pos.line_idx + 1;
        next_line.trailing_line_chars = 0;
    }

    pub fn erase_from_cursor_to_screen(&mut self) {
        self.text.truncate(self.screen_to_buffer_pos());
    }

    pub fn move_lines_down(&mut self, start_line_no: usize, n: usize) {
        assert!(start_line_no < self.num_rows());
        todo!()
    }

    fn screen_to_buffer_pos(&self) -> tree::SeekSoftWrapPosition {
        let wrap_width = self
            .num_cols()
            .try_into()
            .expect("num_cols must be non-zero");
        let end_pos = self
            .text
            .max_bound(tree::SeekSoftWrapPosition::new(wrap_width, 0));
        let (row, col) = self.cursor_position();
        if end_pos.total_rows() >= self.num_rows() {
            let mut delta = tree::SeekSoftWrapPosition::new(wrap_width, 0);
            delta.line_idx = self.num_rows() - row - 1;
            delta.trailing_line_chars = self.num_cols() - col - 1;
            end_pos - delta
        } else {
            let mut ret = tree::SeekSoftWrapPosition::new(wrap_width, row);
            ret.trailing_line_chars = col;
            ret
        }
    }

    fn rope_with_n_blank_lines(num_rows: usize, blank_line: &str) -> Tree {
        puffin::profile_function!();
        let mut tree = tree::Tree::new();
        for _ in 0..num_rows {
            tree.push_str(blank_line, SgrState::default());
        }
        tree
    }

    /// Inserts a hard line-wrap `\n` if the cursor is on the last row of the buffer. Otherwise,
    /// does nothing.
    pub fn insert_linebreak_if_needed(&mut self) {
        if self.screen_to_buffer_pos().line_idx == self.max_cursor_position().line_idx {
            self.text.push_str("\n", SgrState::default());
        }
    }

    fn max_cursor_position(&self) -> tree::SeekSoftWrapPosition {
        self.text.max_bound(tree::SeekSoftWrapPosition::new(
            self.num_cols()
                .try_into()
                .expect("num_cols must be non-zero"),
            0,
        ))
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

#[derive(Debug, Default, Copy, Clone)]
struct CursorState {
    position: (ScreenCoord, ScreenCoord), // in the range (0..num_rows, 0..num_cols)
    sgr_state: SgrState,
    pending_wrap: bool,
}
impl CursorState {
    fn clamp_position(&mut self, new_num_rows: usize, new_num_cols: usize) {
        let (ScreenCoord(row), ScreenCoord(col)) = &mut self.position;
        *row = (*row).min(new_num_rows - 1);
        *col = (*col).min(new_num_cols - 1);
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
impl std::ops::Sub<ScreenCoord> for ScreenCoord {
    type Output = usize;

    fn sub(self, rhs: ScreenCoord) -> Self::Output {
        self.0.saturating_sub(rhs.0)
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
// always in the range [0..grid.max_rows*grid.num_cols)
struct BufferCoord(usize);

impl std::ops::Mul<usize> for BufferCoord {
    type Output = Self;

    fn mul(self, rhs: usize) -> Self::Output {
        Self(self.0 * rhs)
    }
}
impl std::ops::AddAssign<usize> for BufferCoord {
    fn add_assign(&mut self, rhs: usize) {
        *self = Self(self.0 + rhs)
    }
}
impl std::ops::SubAssign<usize> for BufferCoord {
    fn sub_assign(&mut self, rhs: usize) {
        *self = Self(self.0 - rhs)
    }
}

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
        let grid = Grid::new(3, 10);
        assert_eq!(grid.display_lines(..).count(), 3);
        assert_is_blank(&grid, 0..3);
    }

    #[test]
    fn test_grid_2() {
        let mut grid = Grid::new(3, 10);
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
        // assert_nth_line(&grid, 0, "zaaaaaaaaa");
        // assert_nth_line(&grid, 1, "bccccccccc");
        // assert_nth_line(&grid, 2, "d---------");
    }

    #[test]
    fn test_grid_3() {
        let mut grid = Grid::new(3, 10);
        grid.max_scrollback_lines(100);
        grid.resize(5, 10);
        assert_eq!(grid.total_rows(), 5);
        assert_eq!(grid.text.len_lines(), 6);

        grid.write_text_at_cursor("aaaaaaaaaa");
        grid.write_text_at_cursor("bbbbbbbbbb");
        grid.write_text_at_cursor("cccccccccc");
        grid.write_text_at_cursor("dddddddddd");
        grid.write_text_at_cursor("eeeeeeeeee");
        grid.write_text_at_cursor("ffffffffff");
        assert_eq!(grid.total_rows(), 6);
        assert_nth_line(&grid, 0, "aaaaaaaaaa");
        assert_nth_line(&grid, 5, "ffffffffff");
    }

    #[test]
    fn test_erase_after_resizing_down_num_columns() {
        let mut grid = Grid::new(10, 50);
        grid.resize(10, 30);
        // should not panic
        grid.erase_from_cursor_to_screen();
    }

    #[test]
    fn test_write_after_resize_blank_lines() {
        let mut grid = Grid::new(20, 100);
        let input = r#"total 224
drwxr-xr-x  10 rravi  staff    320 Apr 12 16:17 .
drwxr-xr-x   6 rravi  staff    192 Apr 12 16:08 ..
drwxr-xr-x  15 rravi  staff    480 Apr 15 14:12 .git
-rw-r--r--   1 rravi  staff      8 Mar 18 21:01 .gitignore
-rw-r--r--@  1 rravi  staff  99233 Apr 12 16:17 Cargo.lock
-rw-------@  1 rravi  staff    631 Apr 12 16:17 Cargo.toml
-rw-r--r--   1 rravi  staff   1857 Mar 26 17:17 TODO
drwxr-xr-x   3 rravi  staff     96 Apr  9 13:38 res
drwxr-xr-x  10 rravi  staff    320 Apr 15 14:06 src
drwxr-xr-x@  7 rravi  staff    224 Apr 14 15:11 target"#;
        for line in input.lines() {
            grid.write_text_at_cursor(line);
            let (row, _) = grid.cursor_position();
            grid.move_cursor(row, 0); // \r
            grid.move_cursor(row + 1, 0); // \n
        }
        assert_nth_line(
            &grid,
            10,
            "drwxr-xr-x@  7 rravi  staff    224 Apr 14 15:11 target----------------------------------------------",
        );

        // that should work even when the window gets sized smaller than current output
        grid.resize(input.lines().count() / 2, 80);
        assert_nth_line(
            &grid,
            10,
            "drwxr-xr-x@  7 rravi  staff    224 Apr 14 15:11 target--------------------------",
        );
    }

    fn assert_nth_line(grid: &Grid, line_idx: usize, expected: &str) {
        let line = grid.display_lines(line_idx..).next().unwrap().padded_text;
        assert_eq!(&line, expected);
    }

    fn assert_is_blank(grid: &Grid, line_range: Range<usize>) {
        let lines = grid.display_lines(line_range.clone());

        let mut blank_line = grid.blank_line.to_string();
        // tailing newline won't be in the output
        blank_line.pop();
        for (line_idx, line) in line_range.zip(lines) {
            assert_eq!(
                line.padded_text.as_str(),
                &blank_line,
                "for line: {line_idx}"
            );
        }
    }
}
