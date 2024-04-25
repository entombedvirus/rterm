#![allow(dead_code)]

use std::{ops::RangeBounds, sync::Arc};

use arrayvec::{ArrayString, ArrayVec};

use crate::{
    grid_string::{resolve_range, split_str_at_utf8_boundary, GridString},
    terminal_emulator::SgrState,
};

const B: usize = 3;
pub const MAX_CHILDREN: usize = B * 2;
pub const MIN_CHILDREN: usize = B;

#[derive(Debug)]
pub struct Tree {
    root: Arc<Node>,
}

impl std::fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for grid_string in self.iter_strings() {
            grid_string.fmt(f)?;
        }
        Ok(())
    }
}

impl Tree {
    pub fn new() -> Self {
        let root = Arc::new(Node::new_leaf([GridString::default()]));
        Self { root }
    }

    pub fn len_bytes(&self) -> usize {
        self.root.node_summary().bytes
    }

    pub fn len_chars(&self) -> usize {
        self.root.node_summary().chars
    }

    pub fn len_lines(&self) -> usize {
        self.root.node_summary().lines
    }

    pub fn push_str(&mut self, mut new_text: &str, sgr: SgrState) {
        // TODO: if new_text is larger than what will fit in a leaf node, create a tree from it and
        // merge
        while !new_text.is_empty() {
            self.find_string_segment_mut(
                DimensionCharIdx(self.len_chars()),
                |segment: &mut GridString, _segment_char_idx: usize| -> Option<GridString> {
                    let (overflow, _, rem) = segment.push_str(new_text, sgr);
                    new_text = rem;
                    overflow
                },
            );
        }
    }

    pub fn replace_str(&mut self, mut char_idx: usize, mut new_text: &str, sgr: SgrState) {
        while !new_text.is_empty() {
            self.find_string_segment_mut(
                DimensionCharIdx(char_idx),
                |segment: &mut GridString, segment_char_idx: usize| -> Option<GridString> {
                    let (chars_written, rem) = segment.replace_str(segment_char_idx, new_text, sgr);
                    if chars_written == 0 {
                        todo!()
                    }
                    new_text = rem;
                    char_idx += chars_written;
                    None
                },
            );
        }
    }

    pub fn insert_str(&mut self, mut char_idx: usize, mut new_text: &str, sgr: SgrState) {
        while !new_text.is_empty() {
            self.find_string_segment_mut(
                DimensionCharIdx(char_idx),
                |segment: &mut GridString, segment_char_idx: usize| -> Option<GridString> {
                    let (overflow, chars_written, rem) =
                        segment.insert_str(segment_char_idx, new_text, sgr);
                    new_text = rem;
                    char_idx += chars_written;
                    overflow
                },
            );
        }
    }

    pub fn remove_range<R: RangeBounds<usize>>(&mut self, char_range: R) {
        let mut char_range =
            resolve_range(char_range, 0..self.len_chars()).expect("invalid range supplied");
        while !char_range.is_empty() {
            self.find_string_segment_mut(
                DimensionCharIdx(char_range.start),
                |segment: &mut GridString, segment_char_idx: usize| -> Option<GridString> {
                    let end_char_idx =
                        (segment_char_idx + char_range.len()).min(segment.len_chars());
                    let remove_range = segment_char_idx..end_char_idx;
                    segment
                        .remove_char_range(remove_range.clone())
                        .expect("bounds checked");
                    char_range.end -= remove_range.len();
                    None
                },
            );
        }
    }

    fn iter_strings(&self) -> iter::StringIter {
        iter::StringIter::new(&self.root)
    }

    fn iter_nodes(&self) -> iter::NodeIter {
        iter::NodeIter::new(&self.root)
    }

    fn find_string_segment_mut<'a, D: Dimension>(
        &mut self,
        seek_target: D,
        edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) {
        if let Some(node) =
            Arc::make_mut(&mut self.root).seek_and_edit_segment(seek_target, edit_op)
        {
            self.root = Arc::new(Node::new_internal(
                self.root.height() + 1,
                [self.root.clone(), node],
            ));
        }
    }
}

mod iter {
    use std::collections::VecDeque;

    use crate::grid_string::GridString;

    use super::Node;

    #[derive(Debug)]
    pub struct NodeIter<'a> {
        root: Option<&'a Node>,
        queue: VecDeque<&'a Node>,
    }

    impl<'a> NodeIter<'a> {
        pub fn new(node: &'a Node) -> Self {
            let root = Some(node);
            let queue = VecDeque::new();
            Self { root, queue }
        }
    }

    impl<'a> Iterator for NodeIter<'a> {
        type Item = &'a Node;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(root) = self.root.take() {
                self.queue.push_back(root);
            }

            let node = self.queue.pop_front()?;
            match node {
                Node::Leaf { .. } => (),
                Node::Internal { children, .. } => {
                    self.queue
                        .extend(children.into_iter().rev().map(|child| child.as_ref()));
                }
            }

            Some(node)
        }
    }

    #[derive(Debug)]
    pub struct StringIter<'a> {
        root: Option<&'a Node>,
        stack: Vec<(&'a Node, usize)>,
    }

    impl<'a> StringIter<'a> {
        pub fn new(node: &'a Node) -> Self {
            let root = Some(node);
            let stack = vec![];
            Self { root, stack }
        }

        // establish the invariant that top of the stack is always pointing to a leaf node and the
        // index is valid within that node
        fn fixup(&mut self) {
            let needs_fixup = |(node, index): &&mut (&Node, usize)| {
                *index >= node.child_summaries().len() || !node.is_leaf()
            };
            while let Some((node, index)) = self.stack.last_mut().filter(needs_fixup) {
                match node {
                    Node::Leaf { children, .. } => {
                        debug_assert!(*index >= children.len());
                        self.stack.pop();
                    }
                    Node::Internal { children, .. } => {
                        if *index >= children.len() {
                            self.stack.pop();
                        } else {
                            *index += 1;
                            let child = children[*index - 1].as_ref();
                            self.stack.push((child, 0));
                        }
                    }
                }
            }
        }
    }

    impl<'a> Iterator for StringIter<'a> {
        type Item = &'a GridString;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(root) = self.root.take() {
                self.stack.push((root, 0));
            }

            self.fixup();

            let (node, child_idx) = self.stack.last_mut()?;
            let ret = node
                .child_strings()
                .get(*child_idx)
                .expect("top of stack should always be valid");
            *child_idx += 1;
            Some(ret)
        }
    }

    impl<'a> std::iter::FusedIterator for StringIter<'a> {}
}
#[derive(Clone)]
pub enum Node {
    Leaf {
        node_summary: TextSummary,
        children: ArrayVec<GridString, MAX_CHILDREN>,
        child_summaries: ArrayVec<TextSummary, MAX_CHILDREN>,
    },
    Internal {
        height: u8,
        node_summary: TextSummary,
        children: ArrayVec<Arc<Node>, MAX_CHILDREN>,
        child_summaries: ArrayVec<TextSummary, MAX_CHILDREN>,
    },
}

impl Node {
    fn height(&self) -> u8 {
        match *self {
            Node::Leaf { .. } => 0,
            Node::Internal { height, .. } => height,
        }
    }

    fn len_bytes(&self) -> usize {
        self.node_summary().bytes
    }

    fn len_chars(&self) -> usize {
        self.node_summary().chars
    }

    fn len_lines(&self) -> usize {
        self.node_summary().lines
    }

    fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }

    fn node_summary(&self) -> &TextSummary {
        match *self {
            Node::Leaf {
                ref node_summary, ..
            } => node_summary,
            Node::Internal {
                ref node_summary, ..
            } => node_summary,
        }
    }

    fn new_leaf<I: IntoIterator<Item = GridString>>(children: I) -> Node {
        let children: ArrayVec<GridString, MAX_CHILDREN> = children.into_iter().collect();
        debug_assert!(!children.is_empty());
        let mut n = Node::Leaf {
            children,
            node_summary: TextSummary::default(),
            child_summaries: ArrayVec::new(),
        };
        n.recompute_summaries();
        n
    }

    fn new_internal<I: IntoIterator<Item = Arc<Node>>>(height: u8, children: I) -> Node {
        let children: ArrayVec<Arc<Node>, MAX_CHILDREN> = children.into_iter().collect();
        debug_assert!(!children.is_empty());
        let mut n = Self::Internal {
            height,
            children,
            node_summary: TextSummary::default(),
            child_summaries: ArrayVec::new(),
        };
        n.recompute_summaries();
        n
    }

    fn recompute_summaries(&mut self) {
        match self {
            Node::Leaf {
                children,
                child_summaries,
                node_summary,
                ..
            } => {
                *node_summary = TextSummary::default();
                child_summaries.clear();
                children
                    .into_iter()
                    .map(|c| TextSummary::from(&*c))
                    .for_each(|cs| {
                        child_summaries.push(cs);
                        *node_summary += cs;
                    })
            }
            Node::Internal {
                children,
                child_summaries,
                node_summary,
                ..
            } => {
                *node_summary = TextSummary::default();
                child_summaries.clear();
                children
                    .into_iter()
                    .map(|c| c.node_summary().clone())
                    .for_each(|cs| {
                        child_summaries.push(cs);
                        *node_summary += cs;
                    })
            }
        }
    }

    /// scans this node's children from left to right while the running sum of dimension is less
    /// than the passed in `seek_target`. Returns the index of the child that will cause the
    /// running sum to exceed the target and the remaining seek_target.
    fn child_position<D: Dimension>(&self, seek_target: D) -> (usize, D) {
        debug_assert!(
            seek_target <= D::from(self.node_summary()),
            "seek_target is outside bounds: {seek_target:?} / {:?}",
            D::from(self.node_summary())
        );

        if seek_target == D::from(self.node_summary()) {
            // seeking to the end
            let last_child_summary = self
                .child_summaries()
                .last()
                // .or_else(|| {
                //     eprintln!("node: {self:?}");
                //     None
                // })
                .expect("all nodes have at least one child");
            let rem_seek_target = D::from(last_child_summary);
            let child_idx = self.child_summaries().len() - 1;
            return (child_idx, rem_seek_target);
        }

        let mut running_sum = D::default();
        for (child_idx, child_summary) in self.child_summaries().into_iter().enumerate() {
            let next_sum = running_sum + D::from(child_summary);
            if next_sum > seek_target {
                return (child_idx, seek_target - running_sum);
            }
            running_sum = next_sum;
        }

        unreachable!("validated that seek_target is within range earlier")
    }

    fn child_nodes_mut(&mut self) -> &mut ArrayVec<Arc<Node>, MAX_CHILDREN> {
        match self {
            Node::Leaf { .. } => panic!("child_node_mut cannot be called on leaf nodes"),
            Node::Internal { children, .. } => children,
        }
    }

    fn child_strings(&self) -> &ArrayVec<GridString, MAX_CHILDREN> {
        match self {
            Node::Leaf { children, .. } => children,
            Node::Internal { .. } => panic!("cannot call child_strings on internal nodes"),
        }
    }

    fn child_strings_mut(&mut self) -> &mut ArrayVec<GridString, MAX_CHILDREN> {
        match self {
            Node::Leaf { children, .. } => children,
            Node::Internal { .. } => panic!("cannot call child_strings on internal nodes"),
        }
    }

    fn child_summaries(&self) -> &[TextSummary] {
        match *self {
            Node::Leaf {
                ref child_summaries,
                ..
            } => child_summaries,
            Node::Internal {
                ref child_summaries,
                ..
            } => child_summaries,
        }
    }

    fn is_leaf(&self) -> bool {
        matches!(self, Node::Leaf { .. })
    }

    fn seek_and_edit_segment<D: Dimension>(
        &mut self,
        seek_target: D,
        mut edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) -> Option<Arc<Node>> {
        let (child_idx, seek_target) = self.child_position(seek_target);
        let split = match self {
            Node::Leaf { children, .. } => {
                let child = &mut children[child_idx];
                let char_idx = seek_target.to_char_idx(child.as_str());
                edit_op(child, char_idx).and_then(|overflow| {
                    let overflow_pos = child_idx + 1;
                    children
                        .try_insert(overflow_pos, overflow)
                        .and_then(|()| Ok(None))
                        .unwrap_or_else(|err| -> Option<Arc<Node>> {
                            match Compactable::compact_items_with_seam(children, overflow_pos) {
                                Some(new_overflow_pos) => {
                                    children
                                        .try_insert(new_overflow_pos, err.element())
                                        .inspect_err(|err| {
                                            println!("{err}");
                                        })
                                        .expect(
                                            "insertion should succeed after successful compaction",
                                        );
                                    None
                                }
                                None => {
                                    let right_children =
                                        split_children(children, overflow_pos, err.element());
                                    Some(Arc::new(Node::new_leaf(right_children)))
                                }
                            }
                        })
                })
            }
            Node::Internal {
                height, children, ..
            } => {
                let child = Arc::make_mut(&mut children[child_idx]);
                child.seek_and_edit_segment(seek_target, edit_op).and_then(
                    |split_node| -> Option<Arc<Node>> {
                        let split_node_pos = child_idx + 1;
                        children
                            .try_insert(split_node_pos, split_node)
                            .and_then(|()| Ok(None))
                            .unwrap_or_else(|err| {
                                match Compactable::compact_items_with_seam(children, split_node_pos)
                                {
                                    Some(new_split_node_pos) => {
                                        children
                                            .try_insert(new_split_node_pos, err.element())
                                            .expect(
                                            "insertion should succeed after successful compaction",
                                        );
                                        None
                                    }
                                    None => {
                                        let right_children =
                                            split_children(children, split_node_pos, err.element());
                                        Some(Arc::new(Node::new_internal(*height, right_children)))
                                    }
                                }
                            })
                    },
                )
            }
        };
        self.recompute_summaries();
        split
    }

    fn has_room_for_children(&self) -> bool {
        self.child_summaries().len() < MAX_CHILDREN
    }
}

trait Compactable
where
    Self: Sized,
{
    /// Takes two mutable references to T and compacts them together, possibly modifying them both.
    /// Retruns true if the loop should move on from `a` as compact destination.
    fn compact_two(a: &mut Self, b: &mut Self) -> bool;

    fn is_empty(&self) -> bool;

    /// compacts items to the left and right of `seam_idx` while:
    /// - preserving order of items
    /// - not merging across the seam boundary
    ///
    /// Returns where the `seam_idx` falls after merging, if any compaction was done at all.
    fn compact_items_with_seam(
        items: &mut ArrayVec<Self, MAX_CHILDREN>,
        seam_idx: usize,
    ) -> Option<usize> {
        debug_assert!(items.len() == items.capacity());

        let old_seam_idx = seam_idx;
        let empty_idx = items.iter().position(|it| it.is_empty()).or_else(|| {
            let (prefix, suffix) = items.split_at_mut(old_seam_idx);
            Self::compact_slice(prefix);
            // to ensure that we keep at least B children, we should stop compaction as soon as we free
            // up exactly one slot. So lazily evaluate the suffix compaction.
            let prefix_has_empty = prefix.last().map_or(false, |it| it.is_empty());
            if !prefix_has_empty {
                Self::compact_slice(suffix);
            }
            items.iter().position(|it| it.is_empty())
        });

        if let Some(empty_idx) = empty_idx {
            items.remove(empty_idx);
            Some(if empty_idx < old_seam_idx {
                old_seam_idx.saturating_sub(1)
            } else {
                old_seam_idx
            })
        } else {
            None
        }
    }

    fn compact_slice(items: &mut [Self]) {
        let mut i = 0;
        'outer: while i + 1 < items.len() {
            let (prefix, suffix) = items.split_at_mut(i + 1);
            let a = &mut prefix[i];
            for b in suffix {
                if b.is_empty() {
                    continue;
                }
                if a.is_empty() {
                    std::mem::swap(a, b);
                    continue 'outer;
                }
                if Self::compact_two(a, b) {
                    i += 1;
                    continue 'outer;
                }
            }
            break;
        }
    }
}

impl Compactable for Arc<Node> {
    fn is_empty(&self) -> bool {
        Node::is_empty(self.as_ref())
    }

    fn compact_two(a: &mut Self, b: &mut Self) -> bool {
        let (a, b) = (Arc::make_mut(a), Arc::make_mut(b));
        if a.is_leaf() && b.is_leaf() {
            Compactable::compact_two(a.child_strings_mut(), b.child_strings_mut());
        } else {
            Compactable::compact_two(a.child_nodes_mut(), b.child_nodes_mut());
        }
        a.recompute_summaries();
        b.recompute_summaries();
        !a.has_room_for_children()
    }
}

impl Compactable for GridString {
    fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }

    fn compact_two(a: &mut Self, b: &mut Self) -> bool {
        Compactable::compact_two(a.buf_mut(), b.buf_mut());
        Compactable::compact_two(a.sgr_mut(), b.sgr_mut());
        !a.has_room()
    }
}

impl<T, const N: usize> Compactable for ArrayVec<T, N> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn compact_two(a: &mut Self, b: &mut Self) -> bool {
        let room_available = a.remaining_capacity();
        let num_children = b.len();
        a.extend(b.drain(0..room_available.min(num_children)));
        a.remaining_capacity() == 0
    }
}

impl<const N: usize> Compactable for ArrayString<N> {
    fn is_empty(&self) -> bool {
        ArrayString::<N>::is_empty(self)
    }

    fn compact_two(a: &mut Self, b: &mut Self) -> bool {
        if a.remaining_capacity() > 0 && b.len() > 0 {
            let (to_write, rem) = split_str_at_utf8_boundary(b.as_str(), a.remaining_capacity());
            a.push_str(to_write);
            let mut temp = Self::new();
            temp.push_str(rem);
            *b = temp;
        }
        a.remaining_capacity() == 0 || !b.is_empty()
    }
}

fn split_children<T, const N: usize>(
    children: &mut ArrayVec<T, N>,
    insert_at: usize,
    overflow: T,
) -> ArrayVec<T, N> {
    let mut right_children: ArrayVec<T, N> = children.drain(B..).collect();
    if insert_at < B {
        children.insert(insert_at, overflow);
    } else {
        right_children.insert(insert_at - B, overflow);
    }
    right_children
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[derive(Debug)]
        struct ChildSummary(usize);
        match self {
            Self::Leaf { children, .. } => {
                write!(f, "Leaf({}, <", children.len())?;
                for child in children {
                    write!(f, "{:?}|", child.as_str())?;
                }
                write!(f, ">)")
            }
            Self::Internal {
                height, children, ..
            } => {
                writeln!(f, "Height: {height}, {} children", children.len())?;
                for (idx, child) in children.into_iter().enumerate() {
                    writeln!(
                        f,
                        "\t(child {idx} / {num_children}): {child:?}",
                        idx = idx + 1,
                        num_children = children.len()
                    )?;
                }
                Ok(())
            }
        }
    }
}
/// Any type that can be used to seek to a specific leaf node via searching `TextSummary`-s.
pub trait Dimension:
    for<'a> From<&'a TextSummary>
    + Default
    + Clone
    + Copy
    + std::ops::Add<Output = Self>
    + std::ops::Sub<Output = Self>
    + std::cmp::Ord
    + std::fmt::Debug
{
    fn to_char_idx(&self, segment: &str) -> usize;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
struct DimensionCharIdx(usize);

impl Dimension for DimensionCharIdx {
    fn to_char_idx(&self, _: &str) -> usize {
        self.0
    }
}

impl<'a> From<&'a TextSummary> for DimensionCharIdx {
    fn from(value: &'a TextSummary) -> Self {
        Self(value.chars)
    }
}
impl std::ops::Add for DimensionCharIdx {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl std::ops::Sub for DimensionCharIdx {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

fn summarize<'a, T: IntoIterator<Item = &'a TextSummary>>(summaries: T) -> TextSummary {
    summaries
        .into_iter()
        .fold(TextSummary::default(), |agg, s| agg + s)
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TextSummary {
    chars: usize,
    bytes: usize,
    lines: usize,
}
impl std::ops::Add<&TextSummary> for TextSummary {
    type Output = TextSummary;

    fn add(self, rhs: &TextSummary) -> Self::Output {
        TextSummary {
            chars: self.chars + rhs.chars,
            bytes: self.bytes + rhs.bytes,
            lines: self.lines + rhs.lines,
        }
    }
}
impl std::ops::AddAssign for TextSummary {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + &rhs;
    }
}
impl<'a> From<&'a GridString> for TextSummary {
    fn from(value: &'a GridString) -> Self {
        Self {
            chars: value.len_chars(),
            bytes: value.len_bytes(),
            lines: value.as_str().matches('\n').count(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal_emulator::SgrState;

    use super::*;
    const LARGE_TEXT: &'static str = include_str!("tree.rs");

    #[test]
    fn test_tree_1() {
        let bold = SgrState {
            bold: true,
            ..Default::default()
        };
        let mut tree = Tree::new();
        assert_eq!(tree.len_bytes(), 0);
        assert_eq!(tree.len_chars(), 0);
        assert_eq!(tree.len_lines(), 0);

        tree.push_str(
            "hello \u{0d30}\u{0d4b}\u{0d39}\u{0d3f}\u{0d24}\u{0d4d}!\n",
            SgrState::default(),
        );
        assert_eq!(tree.len_bytes(), 26);
        assert_eq!(tree.len_chars(), 14);
        assert_eq!(tree.len_lines(), 1);

        tree.replace_str(6, "back1!", bold);
        assert_eq!(tree.to_string().as_str(), "hello back1!!\n");
        assert_eq!(tree.len_bytes(), 14);
        assert_eq!(tree.len_chars(), 14);
        assert_eq!(tree.len_lines(), 1);
    }

    #[test]
    fn test_large_input_1() {
        let mut tree = Tree::new();
        let input = &LARGE_TEXT[..];
        tree.push_str(input, SgrState::default());
        assert_eq!(tree.to_string().as_str(), input);

        // all levels other than the root should have at least B children
        for (idx, node) in tree.iter_nodes().enumerate() {
            assert!(
                idx == 0 || node.is_leaf() || node.child_summaries().len() >= B,
                "{node:?}\nTree:\n {tree:?}"
            );
        }
    }

    #[test]
    fn test_large_input_2() {
        let mut tree = Tree::new();
        let input = &LARGE_TEXT[..];
        for line in input.split_inclusive('\n').rev() {
            tree.insert_str(0, line, SgrState::default());
        }

        assert_eq!(tree.to_string().as_str(), input);

        // all levels other than the root should have at least B children
        for (idx, node) in tree.iter_nodes().enumerate() {
            assert!(
                idx == 0 || node.is_leaf() || node.child_summaries().len() >= B,
                "{node:?}\nTree:\n {tree:?}"
            );
        }
    }

    #[test]
    fn test_insert_1() {
        let mut tree = Tree::new();
        tree.push_str("Line 1\nLine 2\nLine 3\n", SgrState::default());
        tree.insert_str(14, "Line 2.5\n", SgrState::default());
        assert_eq!(
            tree.to_string().as_str(),
            "Line 1\nLine 2\nLine 2.5\nLine 3\n"
        );
        assert_eq!(tree.len_lines(), 4)
    }

    #[test]
    fn test_remove_range() {
        let mut tree = Tree::new();
        tree.push_str("Line 1\nLine 2\nLine 3\n", SgrState::default());
        tree.remove_range(7..14);
        assert_eq!(tree.to_string().as_str(), "Line 1\nLine 3\n");
        assert_eq!(tree.len_lines(), 2)
    }
}
