use std::{
    num::NonZeroU32,
    ops::{Range, RangeBounds},
    rc::Rc,
};

use crate::{
    puffin,
    terminal_emulator::SgrState,
    tree::{self, SeekCharIdx, SeekSoftWrapPosition, Tree},
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

    pub fn new(num_rows: u32, num_cols: u32) -> Self {
        let cursor_state = CursorState::default();
        let saved_cursor_state = None;
        let blank_line = Rc::new(String::from_iter(
            std::iter::repeat(Self::FILL_CHAR as char)
                .take(num_cols as usize)
                .chain(std::iter::once('\n')),
        ));
        let mut text = Tree::new();
        text.rewrap(num_cols.try_into().expect("num_cols to be non-zero"));

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

    pub fn num_rows(&self) -> u32 {
        self.num_screen_rows.0 as u32
    }

    pub fn num_cols(&self) -> u32 {
        self.num_screen_cols.0 as u32
    }

    // in range num_rows..=max_scrollback_rows
    pub fn total_rows(&self) -> u32 {
        // TODO: will need to use len_graphemes() later
        self.text.max_bound::<SeekSoftWrapPosition>().total_rows()
    }

    pub fn cursor_position(&self) -> (u32, u32) {
        let (ScreenCoord(row), ScreenCoord(col)) = self.cursor_state.position;
        (row, col)
    }

    // for the grid: 24 rows x 80 cols with a max_scrollback_rows 100.
    //  - imagine there are 80 total rows currently in the buffer
    //  - first visible line no will be: 80 - 24 = 56
    //  - visible line range will be 56..80
    pub fn first_visible_line_no(&self) -> u32 {
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

    pub fn max_scrollback_lines(&mut self, n: u32) {
        self.max_rows = BufferCoord(n.max(self.num_screen_rows.0));
    }

    pub fn clear_screen(&mut self) {
        puffin::profile_function!();
        let line_idx = tree::SeekSoftWrapPosition::new(self.first_visible_line_no(), 0);
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

    pub fn move_cursor_relative(&mut self, dr: i32, dc: i32) {
        let (row, col) = self.cursor_position();
        let mut new_row = (row as i32 + dr) as u32;
        let mut new_col = (col as i32 + dc) as u32;
        new_row += new_col / self.num_cols();
        new_col %= self.num_cols();
        self.move_cursor(new_row, new_col)
    }

    pub fn move_cursor(&mut self, new_row: u32, new_col: u32) {
        puffin::profile_function!();
        assert!(new_col < self.num_cols());
        self.cursor_state.pending_wrap = false;
        self.cursor_state.position = (ScreenCoord(new_row), ScreenCoord(new_col));
        self.cursor_state
            .clamp_position(self.num_rows(), self.num_cols());
    }

    pub fn cursor_format_mut(&mut self) -> &mut SgrState {
        &mut self.cursor_state.sgr_state
    }

    pub fn resize(&mut self, new_num_rows: u32, new_num_cols: u32) -> bool {
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
                .take(new_num_cols as usize)
                .chain(std::iter::once('\n')),
        );
        self.blank_line = Rc::new(new_blank_line);

        self.num_screen_rows = ScreenCoord(new_num_rows);
        self.num_screen_cols = ScreenCoord(new_num_cols);
        self.max_rows = self.max_rows.max(BufferCoord(new_num_rows));

        self.text.rewrap(
            self.num_cols()
                .try_into()
                .expect("num_cols must be non-zero"),
        );

        // move cursor to the last cell
        let max_pos = self.text.max_bound::<SeekSoftWrapPosition>();
        self.move_cursor(max_pos.line_idx, max_pos.col_idx);

        true
    }

    pub fn write_text_at_cursor(&mut self, txt: &str) {
        puffin::profile_function!();

        debug_assert!(!txt.contains('\n'));

        // before inserting txt, check if a wrap is pending
        if self.cursor_state.pending_wrap {
            let (row, _) = self.cursor_position();
            self.move_cursor(row + 1, 0);
            self.cursor_state.pending_wrap = false;
        }

        let x = self.text.to_string();
        self.sync_buffer_to_cursor_position();
        let x = self.text.to_string();
        self.text.replace_str(
            self.screen_to_buffer_pos(),
            txt,
            self.cursor_state.sgr_state,
        );
        let x = self.text.to_string();

        // move cursor along
        let edit_len = txt.chars().count() as u32;
        let (_, cur_col) = self.cursor_position();
        if cur_col + (edit_len % self.num_cols()) == self.num_cols() {
            self.move_cursor_relative(0, edit_len as i32 - 1);
            self.cursor_state.pending_wrap = true;
        } else {
            self.move_cursor_relative(0, edit_len as i32);
        }

        if let Some(lines_to_remove) = self
            .total_rows()
            .checked_sub(self.max_rows.0)
            .and_then(NonZeroU32::new)
        {
            self.text
                .remove_range(..tree::SeekSoftWrapPosition::new(lines_to_remove.get(), 0));
        }
    }

    // the screen pos can go out of sync with valid positions inside the text buffer. This can
    // happen due to the fact that lines in the buffer are not always `self.num_cols()` wide
    // (holding that invariant makes resizing prohibitively expensive; scanning every line in
    // the scrollback). Additionally, when the buffer has fewer rows than number of lines one
    // the screen, the cursor might attempt to jump to a position way past the end.
    //
    // To combat this, we check if the position we are about to modify actually exists within
    // the buffer and if it is not, adding blanks spaces or new lines as appropriate in order
    // to make the position valid.
    fn sync_buffer_to_cursor_position(&mut self) {
        let desired_pos = self.screen_to_buffer_pos();
        if let Err(
            err @ tree::OutOfBounds {
                mut last_valid_position,
                mut last_char_idx,
                ..
            },
        ) = self.text.resolve_dimension(desired_pos.clone())
        {
            let before = self.text.to_string();
            dbg!(&err);
            let x = self.text.resolve_dimension(desired_pos.clone());
            if let Some(lines_to_add) = desired_pos
                .line_idx
                .checked_sub(last_valid_position.line_idx)
                .and_then(NonZeroU32::new)
            {
                self.text.push_str(
                    "\n".repeat(lines_to_add.get() as usize).as_str(),
                    SgrState::default(),
                );
                last_char_idx += lines_to_add.get() as usize;
                last_valid_position.line_idx = desired_pos.line_idx;
                last_valid_position.col_idx = 0;
            }
            let after = self.text.to_string();

            // TODO: this will always insert 1 blank
            let n = desired_pos.col_idx - last_valid_position.col_idx + 1;
            self.text.insert_str(
                SeekCharIdx(last_char_idx),
                &self.blank_line[..n as usize],
                SgrState::default(),
            );
            let after = self.text.to_string();

            if !self.text.resolve_dimension(desired_pos).is_ok() {
                // break point
                let _ = after.len();
            }
            debug_assert!(self.text.resolve_dimension(desired_pos).is_ok());
            self.cursor_state
                .clamp_position(self.num_rows(), self.num_cols());
        }
    }

    // scan the rope counting the number of display lines (with soft-wrapping) until
    // we reach the beginning of query_range. Then take query_range.len() number of lines.
    // Note that a display line may begin the middle of a rope line because of soft-wrapping.
    pub fn display_lines<'a, R: RangeBounds<u32>>(
        &'a self,
        query_range: R,
    ) -> impl Iterator<Item = DisplayLine> + 'a {
        puffin::profile_function!();
        self.text
            .iter_soft_wrapped_lines(query_range)
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
                    let n = padded_text.chars().count() as u32;
                    if let Some(to_add) = self.num_cols().checked_sub(n) {
                        let blanks = &self.blank_line[..to_add as usize];
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
        if let Err(_) = self.text.resolve_dimension(cursor_pos.clone()) {
            // cursor is already outside anywhere we have text anyway.
            // Nothing to erase in this case.
            return;
        }

        let mut next_line = tree::SeekSoftWrapPosition::new(cursor_pos.line_idx + 1, 0);
        if let Err(err) = self.text.resolve_dimension(next_line.clone()) {
            next_line = err.last_valid_position;
        }
        self.text.remove_range(cursor_pos..next_line);
    }

    pub fn erase_from_cursor_to_screen(&mut self) {
        let cursor_pos = self.screen_to_buffer_pos();
        match self.text.resolve_dimension(cursor_pos) {
            Ok(char_idx) => {
                self.text.truncate(SeekCharIdx(char_idx));
            }
            Err(err) => {
                // hmm? won't this cause text before the cursor to get erased?
                let _ = self.text.to_string();
                self.text.truncate(SeekCharIdx(err.last_char_idx + 1));
            }
        }
    }

    pub fn move_lines_down(&mut self, start_line_no: u32, n: u32) {
        assert!(start_line_no < self.num_rows());
        todo!()
    }

    fn screen_to_buffer_pos(&self) -> tree::SeekSoftWrapPosition {
        let (row, col) = self.cursor_position();
        let max_pos: tree::SeekSoftWrapPosition = self.text.max_bound();
        let scrollback_rows = (max_pos.line_idx + 1).saturating_sub(self.num_rows());
        tree::SeekSoftWrapPosition::new(scrollback_rows + row, col)
    }

    fn rope_with_n_blank_lines(num_rows: u32, blank_line: &str) -> Tree {
        puffin::profile_function!();
        let mut tree = tree::Tree::new();
        for _ in 0..num_rows {
            tree.push_str(blank_line, SgrState::default());
        }
        tree
    }

    /// Inserts a hard line-wrap `\n` if the cursor is on the last row of the buffer. Otherwise,
    /// moves the cursor down a line.
    pub fn insert_linebreak_if_needed(&mut self) {
        if self.screen_to_buffer_pos().line_idx
            == self.text.max_bound::<SeekSoftWrapPosition>().line_idx
        {
            self.text.push_str("\n", SgrState::default());
        }

        self.move_cursor_relative(1, 0);
    }

    pub fn text_contents(&self) -> String {
        self.text.to_string()
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
    fn clamp_position(&mut self, new_num_rows: u32, new_num_cols: u32) {
        let (ScreenCoord(row), ScreenCoord(col)) = &mut self.position;
        *row = (*row).min(new_num_rows - 1);
        *col = (*col).min(new_num_cols - 1);
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ScreenCoord(u32); // always in the range [0..grid.num_rows]

impl std::ops::Add<i32> for ScreenCoord {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Self((self.0 as i32 + rhs) as u32)
    }
}
impl std::ops::AddAssign<u32> for ScreenCoord {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}
impl std::ops::Sub<i32> for ScreenCoord {
    type Output = Self;

    fn sub(self, rhs: i32) -> Self::Output {
        Self((self.0 as i32).saturating_sub(rhs) as u32)
    }
}
impl std::ops::Sub<ScreenCoord> for ScreenCoord {
    type Output = u32;

    fn sub(self, rhs: ScreenCoord) -> Self::Output {
        self.0.saturating_sub(rhs.0)
    }
}
impl std::ops::Div<u32> for ScreenCoord {
    type Output = u32;

    fn div(self, rhs: u32) -> Self::Output {
        self.0 / rhs
    }
}
impl std::ops::Rem<u32> for ScreenCoord {
    type Output = u32;

    fn rem(self, rhs: u32) -> Self::Output {
        self.0 % rhs
    }
}
impl std::ops::RemAssign<u32> for ScreenCoord {
    fn rem_assign(&mut self, rhs: u32) {
        self.0 %= rhs;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// always in the range [0..grid.max_rows*grid.num_cols)
struct BufferCoord(u32);

impl std::ops::Mul<u32> for BufferCoord {
    type Output = Self;

    fn mul(self, rhs: u32) -> Self::Output {
        Self(self.0 * rhs)
    }
}
impl std::ops::AddAssign<u32> for BufferCoord {
    fn add_assign(&mut self, rhs: u32) {
        *self = Self(self.0 + rhs)
    }
}
impl std::ops::SubAssign<u32> for BufferCoord {
    fn sub_assign(&mut self, rhs: u32) {
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
        assert_eq!(
            grid.display_lines(..).count(),
            0,
            "blank grid is not expected to produce any lines"
        );
    }

    #[test]
    fn test_grid_2() {
        let mut grid = Grid::new(3, 10);
        grid.write_text_at_cursor("foo");

        assert_nth_line(&grid, 0, "foo-------");
        assert_eq!(grid.display_lines(..).count(), 1);
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
        let mut grid = Grid::new(3, 10);
        grid.max_scrollback_lines(100);
        grid.resize(5, 10);
        assert_eq!(grid.total_rows(), 0);
        assert_eq!(grid.text.len_lines(), 0);

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
    fn test_erase_after_resizing_1() {
        let mut grid = Grid::new(10, 50);
        grid.resize(10, 30);
        // should not panic
        grid.erase_from_cursor_to_screen();
    }

    #[test]
    fn test_erase_after_resize_2() {
        let mut grid = Grid::new(24, 80);
        grid_feed_input(
            &mut grid,
            [
                "\nuser@host: /some/path\n",
                "> ls\n",
                "Cargo.lock\tCargo.toml\tTODO\t\tres\t\tsrc\t\ttarget\n",
                "\nuser@host: /some/path\n",
                "> ",
            ],
        );

        // simulate user resizing the window a bunch
        for (r, c) in [(24, 15), (5, 15), (50, 100)] {
            grid.resize(r, c);
            grid.erase_from_cursor_to_screen();
        }
    }

    #[test]
    fn test_erase_after_resize_3() {
        let mut grid = Grid::new(17, 57);
        grid_feed_input(&mut grid, ["\n~/work/rterm rohith-rendering-perf*\n❯ ls -al\ntotal 224\ndrwxr-xr-x  10 rravi  staff    320 Apr 30 18:56 .\ndrwxr-xr-x   6 rravi  staff    192 Apr 12 16:0
8 ..\ndrwxr-xr-x  15 rravi  staff    480 May 14 18:02 .git\n-rw-r--r--   1 rravi  staff      8 Mar 18 21:01 .gitignore\n-rw-r--r--@  1 rravi  staff  99246 Apr 25
15:02 Cargo.lock\n-rw-r--r--@  1 rravi  staff    650 Apr 25 15:02 Cargo.toml\n-rw-r--r--   1 rravi  staff   1857 Mar 26 17:17 TODO\ndrwxr-xr-x   3 rravi  staff
  96 May 14 16:26 res\ndrwxr-xr-x  12 rravi  staff    384 May 14 18:07 src\ndrwxr-xr-x@  7 rravi  staff    224 May  7 21:29 target\n\n\n~/work/rterm rohith-render
ing-perf*\n\n"]);

        // should not panic
        grid.resize(17, 58);
        grid.erase_from_cursor_to_screen();
        grid_feed_input(&mut grid, ["❯"]);
    }

    #[test]
    fn test_cursor_pos_after_resize_1() {
        let mut grid = Grid::new(10, 50);
        // resize cols to be smaller
        grid.move_cursor(9, 49);
        grid.sync_buffer_to_cursor_position();
        let expected_contents = {
            let mut sb = "\n".repeat(9);
            for _ in 0..=49 {
                sb.push(Grid::FILL_CHAR as char);
            }
            sb
        };

        assert_eq!(grid.text_contents(), expected_contents);
        grid.resize(10, 49);
        // contents should not change due to resize
        assert_eq!(grid.text_contents(), expected_contents);
        assert_eq!(
            grid.cursor_position(),
            (9, 1),
            "expected due to wrapping one space"
        );

        // resize rows and cols to be bigger
        grid.resize(20, 100);
        assert_eq!(
            grid.cursor_position(),
            (9, 50),
            "expected cursor to be at the end after resize"
        );
        grid.move_cursor(5, 30);
        grid.sync_buffer_to_cursor_position();
        assert_eq!(grid.text_contents(), {
            let mut sb = "\n".repeat(5);
            for _ in 0..=30 {
                sb.push(Grid::FILL_CHAR as char);
            }
            sb.push('\n');
            for _ in 6..9 {
                sb.push('\n');
            }
            for _ in 0..50 {
                sb.push(Grid::FILL_CHAR as char);
            }
            sb
        });

        // resize rows to be smaller
        grid.resize(6, 100);
        assert_eq!(grid.cursor_position(), (5, 50));
    }

    #[test]
    fn test_cursor_pos_after_resize_2() {
        let mut grid = Grid::new(10, 50);
        grid_feed_input(&mut grid, ["user@host ~/work/rterm\n", "> "]);
        assert_eq!(grid.cursor_position(), (1, 2));

        // resize to be tight fit, but not cause any wrapping
        grid.resize(10, 23);
        assert_eq!(grid.cursor_position(), (1, 2));

        // resize smaller to cause some wrapping
        grid.resize(10, 22);
        assert_eq!(grid.cursor_position(), (2, 2));

        // resize bigger to go back to original pos
        grid.resize(10, 23);
        assert_eq!(grid.cursor_position(), (1, 2));
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
drwxr-xr-x@  7 rravi  staff    224 Apr 14 15:11 target
"#;
        for line in input.lines() {
            grid.write_text_at_cursor(line);

            let (row, _) = grid.cursor_position();
            grid.move_cursor(row, 0); // \r
            grid.insert_linebreak_if_needed(); // \n
        }
        assert_nth_line(
            &grid,
            10,
            "drwxr-xr-x@  7 rravi  staff    224 Apr 14 15:11 target----------------------------------------------",
        );

        // that should work even when the window gets sized smaller than current output
        grid.resize(input.lines().count() as u32 / 2, 80);
        assert_nth_line(
            &grid,
            10,
            "drwxr-xr-x@  7 rravi  staff    224 Apr 14 15:11 target--------------------------",
        );
    }

    fn assert_nth_line(grid: &Grid, line_idx: u32, expected: &str) {
        let line = grid.display_lines(line_idx..).next().unwrap().padded_text;
        assert_eq!(&line, expected);
    }

    fn assert_is_blank(grid: &Grid, line_range: Range<u32>) {
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

    fn grid_feed_input<'a, I: IntoIterator<Item = &'a str>>(grid: &mut Grid, parts: I) {
        for p in parts {
            for line in p.split_inclusive('\n') {
                if line.ends_with('\n') {
                    grid.write_text_at_cursor(&line[..line.len() - 1]);
                    let (row, _) = grid.cursor_position();
                    grid.move_cursor(row, 0); // \r
                    grid.insert_linebreak_if_needed(); // \n
                } else {
                    grid.write_text_at_cursor(line);
                }
            }
        }
    }
}
