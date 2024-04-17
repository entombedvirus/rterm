use std::{ops::RangeBounds, str::FromStr, sync::Arc};

use anyhow::Context;
use arrayvec::{ArrayString, ArrayVec};

use constants::*;

// different constants for tests to avoid having to create huge test inputs
mod constants {
    #[cfg(test)]
    pub use debug_constants::*;

    #[cfg(not(test))]
    pub use release_constants::*;

    pub const MIN_CHILDREN: usize = TREE_BASE;
    pub const MIN_VALUES: usize = TREE_BASE;
    pub const MAX_CHILDREN: usize = TREE_BASE * 2;
    pub const MAX_VALUES: usize = TREE_BASE * 2;

    #[allow(dead_code)]
    mod debug_constants {
        pub const TREE_BASE: usize = 3;
        pub const MAX_BYTES: usize = 8;
        pub const MAX_CHARS: usize = 8;
    }

    #[allow(dead_code)]
    mod release_constants {
        pub const TREE_BASE: usize = 16;
        pub const MAX_BYTES: usize = 64;
        pub const MAX_CHARS: usize = 64;
    }
}

pub trait FormatMetadata: Clone + Default + std::fmt::Debug {}

/// A specialized datastructure that stores terminal buffer contents in a B+-like tree structure
/// with log N times to insert or remove text. Additionally, for each "cell" in the terminal, it
/// stores an opaque FormatMetadat type together with the cell text contents for efficient retrieval
/// and modification.
#[derive(Debug)]
pub struct Rope<T> {
    fill_char: char,
    root: Arc<Node<T>>,
}

impl<FM> Rope<FM>
where
    FM: FormatMetadata,
{
    pub fn new(fill_char: char) -> Self {
        let root = Arc::new(Node::new_leaf(fill_char));
        Self { fill_char, root }
    }

    pub fn len_lines(&self) -> usize {
        todo!()
    }

    pub fn lines_at<R: RangeBounds<usize>>(&self, line_range: R) -> ! {
        todo!()
    }

    pub fn clear_lines<R: RangeBounds<usize>>(&mut self, line_range: R) {
        todo!()
    }

    pub fn append_blank_line(&mut self) {
        self.insert_blank_line(self.len_lines());
    }

    fn insert_blank_line(&self, line_idx: usize) {
        todo!()
    }

    pub fn erase<R: RangeBounds<Position>>(&mut self, erase_range: R) {
        todo!()
    }

    pub fn replace<R: RangeBounds<Position>>(
        &mut self,
        edit_range: R,
        replacement_text: &str,
        text_format: &FM,
    ) {
        todo!()
    }

    pub fn remove_rows<R: RangeBounds<usize>>(&mut self, remove_range: R) {
        todo!()
    }
}

#[derive(Debug)]
enum Node<FM> {
    Internal {
        height: u8,
        summary: ChunkSummary,
        children: ArrayVec<Arc<Node<FM>>, MAX_CHILDREN>,
        child_summaries: ArrayVec<ChunkSummary, MAX_CHILDREN>,
    },
    Leaf {
        summary: ChunkSummary,
        chunks: ArrayVec<Chunk<FM>, MAX_VALUES>,
        chunk_summaries: ArrayVec<ChunkSummary, MAX_VALUES>,
    },
}

impl<FM> Node<FM>
where
    FM: FormatMetadata,
{
    fn new_leaf(fill_char: char) -> Self {
        let chunk = Chunk::new(fill_char);
        let chunks: ArrayVec<Chunk<FM>, MAX_VALUES> =
            std::iter::repeat(chunk).take(MAX_VALUES).collect();
        let chunk_summaries: ArrayVec<ChunkSummary, MAX_VALUES> =
            chunks.iter().map(ChunkSummary::from_chunk).collect();
        let summary = ChunkSummary::summarize(&chunk_summaries);
        Node::Leaf {
            summary,
            chunks,
            chunk_summaries,
        }
    }
}

#[derive(Debug, Default)]
struct ChunkSummary {
    bytes: usize,
    lines: usize,
    chars: usize,
}

impl ChunkSummary {
    fn from_chunk<FM: FormatMetadata>(chunk: &Chunk<FM>) -> Self {
        Self {
            bytes: chunk.text.len(),
            lines: chunk.text.lines().count(),
            chars: chunk.char_count(),
        }
    }

    fn summarize(chunk_summaries: &[ChunkSummary]) -> ChunkSummary {
        let mut ret = ChunkSummary::default();
        for summary in chunk_summaries {
            ret.add_summary(summary)
        }
        ret
    }

    fn add_summary(&mut self, summary: &Self) {
        self.bytes += summary.bytes;
        self.lines += summary.lines;
        self.chars += summary.chars;
    }
}

#[derive(Debug, Clone)]
struct Chunk<T> {
    text: ArrayString<MAX_BYTES>,
    format: ArrayVec<T, MAX_CHARS>,
}

impl<T: Default + Clone> Chunk<T> {
    fn new(fill_char: char) -> Self {
        let num_chars = MAX_BYTES / fill_char.len_utf8();
        assert!(num_chars > 0, "fill_char does not fit into a chunk");
        let num_chars = num_chars.min(MAX_CHARS);

        let blanks = String::from_iter(std::iter::repeat(fill_char).take(num_chars));
        let text = ArrayString::from_str(&blanks)
            .expect("ArrayString is always big enough to contain num_chars copies of fill_char");
        let format = ArrayVec::from_iter(std::iter::repeat(T::default()).take(num_chars));
        Self { text, format }
    }

    fn char_count(&self) -> usize {
        // cheaper than self.text.chars.count() and we hold the invariant that number of chars in
        // `self.text` is always equal to `self.format.len()`
        self.format.len()
    }

    fn as_str(&self) -> &str {
        self.text.as_str()
    }

    // returns the overflow that must be written to the next chunk
    fn write_at<'a>(
        &mut self,
        char_offset: usize,
        new_text: &'a str,
        format: &T,
    ) -> Option<SmolString> {
        let byte_offset = char_to_byte_idx(self.text.as_str(), char_offset)
            .with_context(|| format!("chunk current has {} chars", self.char_count()))
            .expect("char_offset is out of bounds");

        let new_text_chars = new_text.chars().count();
        assert!(new_text_chars < MAX_CHARS);

        let mut edited = ArrayString::new();
        edited.push_str(&self.text.as_str()[0..byte_offset]);

        let mut overflow = None;
        let mut written = 0;
        for (new_text_byte_idx, ch) in new_text.char_indices() {
            if let Ok(_) = edited.try_push(ch) {
                written += 1;
            } else {
                // push can fail because we ran out of space in `edited`
                let rem = &new_text[new_text_byte_idx..];
                overflow
                    .get_or_insert_with(|| SmolString::new())
                    .push_str(rem);
                break;
            }
        }

        let suffix = self.text.char_indices().skip(char_offset + new_text_chars);
        for (suffix_byte_idx, ch) in suffix {
            if let Ok(_) = edited.try_push(ch) {
                // noop
            } else {
                // push can fail because we ran out of space in `edited`
                let rem = &self.text.as_str()[suffix_byte_idx..];
                overflow
                    .get_or_insert_with(|| SmolString::new())
                    .push_str(rem);
                break;
            }
        }

        self.text = edited;
        self.format[char_offset..char_offset + written].fill(format.clone());

        overflow
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: usize,
    pub cell: usize,
}

type SmolString = ArrayString<MAX_BYTES>;

fn char_to_byte_idx(s: &str, char_idx: usize) -> Option<usize> {
    s.char_indices().nth(char_idx).map(|(byte_idx, _)| byte_idx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    #[derive(Debug, Default, Clone, Copy)]
    struct FakeFormat {
        foo: bool,
    }

    fn format_stub() -> FakeFormat {
        FakeFormat { foo: true }
    }

    #[test]
    fn test_chunk_1() {
        let chunk: Chunk<FakeFormat> = Chunk::new('x');
        assert!(chunk.text.is_full());
        assert!(chunk.format.is_full());
    }

    #[test]
    fn test_chunk_write_at() {
        assert_eq!(MAX_BYTES, 8);

        let mut chunk: Chunk<FakeFormat> = Chunk::new('x');
        assert_eq!(None, chunk.write_at(0, "hello", &format_stub()));
        assert_eq!(MAX_CHARS, chunk.char_count());
        assert_eq!("helloxxx", chunk.as_str());

        // write in the middle
        assert_eq!(None, chunk.write_at(5, " U", &format_stub()));
        assert_eq!("hello Ux", chunk.as_str());

        // with overflow
        let overflow = chunk.write_at(6, "world", &format_stub());
        assert_eq!("hello wo", chunk.as_str());
        assert_eq!(Some("rld"), overflow.as_deref());

        // overflow with multi-byte chars
        let overflow = chunk.write_at(5, "\u{FF37}\u{FF37}", &format_stub());
        assert_eq!("hello\u{FF37}", chunk.as_str());
        assert_eq!(Some("\u{FF37}o"), overflow.as_deref());
    }
}
