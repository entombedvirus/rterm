use std::{
    ops::{Not, RangeBounds},
    sync::Arc,
};

use anyhow::Context;
use arrayvec::{ArrayString, ArrayVec};

use constants::*;

// different constants for tests to avoid having to create huge test inputs
#[allow(dead_code)]
mod constants {
    #[cfg(test)]
    pub use debug_constants::*;

    #[cfg(not(test))]
    pub use release_constants::*;

    pub const MIN_CHILDREN: usize = TREE_BASE;
    pub const MIN_VALUES: usize = TREE_BASE;
    pub const MAX_CHILDREN: usize = TREE_BASE * 2;
    pub const MAX_VALUES: usize = TREE_BASE * 2;
    pub const MIN_BYTES: usize = (MAX_BYTES + (MAX_BYTES % 2)) / 2;
    pub const MIN_CHARS: usize = (MAX_CHARS + (MAX_CHARS % 2)) / 2;

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
pub struct Rope<FM: FormatMetadata> {
    fill_char: char,
    root: Arc<Node<FM>>,
}

impl<FM> Rope<FM>
where
    FM: FormatMetadata,
{
    pub fn new(fill_char: char) -> Self {
        let root = Arc::new(Node::new_leaf());
        Self { fill_char, root }
    }

    pub fn len_lines(&self) -> usize {
        self.root.summary().lines
    }

    pub fn len_chars(&self) -> usize {
        self.root.summary().chars
    }

    pub fn lines_at<R: RangeBounds<usize>>(&self, line_range: R) -> ! {
        todo!()
    }

    pub fn clear_lines<R: RangeBounds<usize>>(&mut self, line_range: R) {
        todo!()
    }

    pub fn erase<R: RangeBounds<Position>>(&mut self, erase_range: R) {
        todo!("replace with blank char")
    }

    pub fn replace<R: RangeBounds<Position>>(
        &mut self,
        edit_range: R,
        replacement_text: &str,
        text_format: &FM,
    ) {
        todo!("remove -> insert")
    }

    pub fn remove_rows<R: RangeBounds<usize>>(&mut self, remove_range: R) {
        todo!()
    }

    pub fn append(&mut self, new_text: &str, format: &FM) {
        todo!("find the right leaf and insert it")
    }

    pub fn insert_at(&mut self, char_idx: usize, new_text: &str, format: &FM) {
        if char_idx == self.len_chars() {
            self.append(new_text, format);
            return;
        }

        let mut write_offset = char_idx;
        let mut to_write = new_text;
        while !to_write.is_empty() {
            let (chunk, sub_offset) = self
                .get_chunk_mut(write_offset)
                .with_context(|| format!("write_offset: {write_offset} / {}", self.len_chars()))
                .expect("invalid write_offset");

            let prefix = take_prefix_chars(to_write, MAX_CHARS);
            if let Some((right_split, split_char_idx)) = chunk.insert(sub_offset, prefix, format) {
                self.insert_chunk(write_offset + split_char_idx, right_split);
            }
            to_write = &to_write[prefix.len()..];
            write_offset += prefix.chars().count();
        }
    }

    // returns the chunk that contains char_idx and the offset within the chunk to char_idx
    fn get_chunk_mut(&mut self, char_idx: usize) -> Option<(&mut Chunk<FM>, usize)> {
        let node = Arc::make_mut(&mut self.root);
        node.get_chunk_mut(char_idx)
    }

    // char_idx must be a chunk boundary
    fn insert_chunk(&mut self, char_idx: usize, chunk: Chunk<FM>) {
        let node = Arc::make_mut(&mut self.root);
        if let Some(split_node) = node.insert_chunk(char_idx, chunk) {
            *self = Node::new_internal(self.clone(), split_node);
        }
    }
}

fn take_prefix_chars(text: &str, n: usize) -> &str {
    text.char_indices()
        .nth(n)
        .map_or(text, |(byte_idx, _)| &text[..byte_idx])
}

#[derive(Debug, Clone)]
enum Node<FM: FormatMetadata> {
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
    fn new_leaf() -> Self {
        let chunk = Chunk::new();
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

    fn is_leaf(&self) -> bool {
        matches!(*self, Node::Leaf { .. })
    }

    fn summary(&self) -> &ChunkSummary {
        match *self {
            Node::Internal { ref summary, .. } => summary,
            Node::Leaf { ref summary, .. } => summary,
        }
    }

    fn child_summaries(&self) -> &[ChunkSummary] {
        match *self {
            Node::Internal {
                ref child_summaries,
                ..
            } => child_summaries,
            Node::Leaf {
                ref chunk_summaries,
                ..
            } => chunk_summaries,
        }
    }

    fn child_nodes_mut(&mut self) -> &mut [Arc<Node<FM>>] {
        match *self {
            Node::Internal {
                ref mut children, ..
            } => children,
            Node::Leaf { .. } => panic!("cannot call child_nodes on leaf node"),
        }
    }

    fn child_nodes(&self) -> &[Arc<Node<FM>>] {
        match *self {
            Node::Internal { ref children, .. } => children,
            Node::Leaf { .. } => panic!("cannot call child_nodes on leaf node"),
        }
    }

    fn chunks(&self) -> &[Chunk<FM>] {
        match *self {
            Node::Internal { .. } => panic!("cannot call chunks on an internal node"),
            Node::Leaf { ref chunks, .. } => chunks,
        }
    }

    fn chunks_mut(&mut self) -> &mut [Chunk<FM>] {
        match *self {
            Node::Internal { .. } => panic!("cannot call chunks on an internal node"),
            Node::Leaf { ref mut chunks, .. } => chunks,
        }
    }

    fn height(&self) -> usize {
        match *self {
            Node::Internal { height, .. } => height as usize,
            Node::Leaf { .. } => 0,
        }
    }

    fn len_chars(&self) -> usize {
        self.summary().chars
    }

    fn get_chunk_mut(&mut self, char_idx: usize) -> Option<(&mut Chunk<FM>, usize)> {
        if char_idx >= self.len_chars() {
            // not in this node: leaf or otherwise
            return None;
        }

        let mut char_idx = char_idx;
        match *self {
            Node::Internal {
                ref mut children,
                ref child_summaries,
                ..
            } => {
                for (child, summary) in children.iter_mut().zip(child_summaries) {
                    if char_idx < summary.chars {
                        let child = Arc::make_mut(child);
                        return child.get_chunk_mut(char_idx);
                    } else {
                        char_idx -= summary.chars;
                    }
                }
            }
            Node::Leaf {
                ref mut chunks,
                ref chunk_summaries,
                ..
            } => {
                for (chunk, summary) in chunks.iter_mut().zip(chunk_summaries) {
                    if char_idx < summary.chars {
                        return Some((chunk, char_idx));
                    } else {
                        char_idx -= summary.chars;
                    }
                }
            }
        }
        unreachable!("char_idx should be present in one of the children");
    }
}

#[derive(Debug, Default, Clone, Copy)]
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
            chars: chunk.len_chars(),
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

#[derive(Debug, Default, Clone)]
struct Chunk<FM: FormatMetadata> {
    text: ArrayString<MAX_BYTES>,
    format: ArrayVec<FM, MAX_CHARS>,
}

#[derive(Debug, Default)]
struct WriteOverflow<FM: FormatMetadata> {
    chars_written: usize,
    text: ArrayString<MAX_BYTES>,
    format: ArrayVec<FM, MAX_CHARS>,
}

impl<FM: FormatMetadata> Chunk<FM> {
    fn new() -> Self {
        Self::default()
    }

    fn len_chars(&self) -> usize {
        // cheaper than self.text.chars.count() and we hold the invariant that number of chars in
        // `self.text` is always equal to `self.format.len()`
        self.format.len()
    }

    fn as_str(&self) -> &str {
        self.text.as_str()
    }

    // if the write caused the chunk to split, this will return the right split and the
    // char_index where it should be inserted into the tree
    fn insert<'a>(
        &mut self,
        char_offset: usize,
        new_text: &'a str,
        new_format: &FM,
    ) -> Option<(Chunk<FM>, usize)> {
        let new_text_chars = new_text.chars().count();
        assert!(new_text.len() <= MAX_BYTES && new_text_chars <= MAX_CHARS);

        let must_split = char_offset + new_text_chars > MAX_CHARS
            || self.text.len() + new_text.len() > MAX_BYTES;

        if must_split {
            let mid_char_idx = (self.len_chars() + new_text_chars) / 2;
            let (left_text, right_text) =
                combine_and_split_smolstr(&self.text, new_text, mid_char_idx);
            let (left_format, right_format) =
                split_format(&self.format, new_format, new_text_chars, mid_char_idx);
            let left_chunk = Chunk {
                text: left_text,
                format: left_format,
            };
            let right_chunk = Chunk {
                text: right_text,
                format: right_format,
            };
            *self = left_chunk;
            Some((right_chunk, mid_char_idx))
        } else {
            self.text.push_str(new_text);
            self.format
                .extend(std::iter::repeat(new_format).take(new_text_chars).cloned());
            None
        }
    }
}

fn split_format<FM: FormatMetadata>(
    existing_formats: &[FM],
    new_format: &FM,
    new_rep_count: usize,
    split_at: usize,
) -> (ArrayVec<FM, MAX_CHARS>, ArrayVec<FM, MAX_CHARS>) {
    debug_assert!(existing_formats.len() + new_rep_count <= 2 * MAX_CHARS);
    let mut joined: ArrayVec<FM, { 2 * MAX_CHARS }> = ArrayVec::new();
    joined.extend(existing_formats.into_iter().cloned());
    joined.extend(std::iter::repeat(new_format).take(new_rep_count).cloned());
    let (left, right) = joined.split_at(split_at);
    (
        left.try_into().expect("len checked"),
        right.try_into().expect("len checked"),
    )
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

fn combine_and_split_smolstr(s1: &str, s2: &str, split_at: usize) -> (SmolString, SmolString) {
    debug_assert!(s1.len() <= MAX_BYTES && s2.len() <= MAX_BYTES);

    let mut combined: ArrayString<{ 2 * MAX_BYTES }> = ArrayString::new();
    combined.push_str(s1);
    combined.push_str(s2);

    let mut left = SmolString::new();
    let mut right = SmolString::new();

    for (i, ch) in combined.chars().enumerate() {
        if i < split_at {
            left.push(ch);
        } else {
            right.push(ch)
        }
    }

    (left, right)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    #[derive(Debug, Default, Clone)]
    struct FakeFormat {
        foo: bool,
    }
    impl FormatMetadata for FakeFormat {}

    fn format_stub() -> FakeFormat {
        FakeFormat { foo: true }
    }

    #[test]
    fn test_chunk_1() {
        let chunk: Chunk<FakeFormat> = Chunk::new();
        assert!(chunk.text.is_empty());
        assert!(chunk.format.is_empty());
    }

    #[test]
    fn test_chunk_write_at() {
        assert_eq!(MAX_BYTES, 8);

        let mut chunk: Chunk<FakeFormat> = Chunk::new();
        assert_eq!(None, chunk.insert(0, "hello", &format_stub()));
        assert_eq!(5, chunk.len_chars());
        assert_eq!("hello", chunk.as_str());

        // write in the middle
        assert_eq!(None, chunk.insert(5, " U", &format_stub()));
        assert_eq!("hello U", chunk.as_str());

        // with overflow
        let overflow = chunk.insert(6, "world", &format_stub());
        assert_eq!("hello wo", chunk.as_str());
        assert_eq!(Some("rld"), overflow.as_deref());

        // overflow with multi-byte chars
        let overflow = chunk.insert(5, "\u{FF37}\u{FF37}", &format_stub());
        assert_eq!("hello\u{FF37}", chunk.as_str());
        assert_eq!(Some("\u{FF37}o"), overflow.as_deref());
    }

    #[test]
    fn test_rope_1() {
        let mut rope = Rope::<FakeFormat>::new(' ');
        assert_eq!(0, rope.len_lines());

        let mut blank_line = " ".repeat(10);
        blank_line.push('\n');
        for _ in 0..5 {
            rope.append(&blank_line, &format_stub());
        }
        assert_eq!(5, rope.len_lines());
    }
}
