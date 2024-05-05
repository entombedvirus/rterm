#![allow(dead_code)]

use std::num::NonZeroUsize;
use std::ops::{Bound, Range, RangeBounds};
use std::sync::Arc;

use arrayvec::{ArrayString, ArrayVec};

use crate::{
    grid_string::{split_str_at_utf8_boundary, GridString},
    terminal_emulator::SgrState,
};

const B: usize = 3;
pub const MAX_CHILDREN: usize = B * 2;
pub const MIN_CHILDREN: usize = B;

#[derive(Debug)]
pub struct OutOfBounds<T> {
    pub attempted_position: T,
    pub last_valid_position: T,
}

impl<T> std::fmt::Display for OutOfBounds<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "OutOfBounds: last_valid_position: {:?}",
            self.last_valid_position
        )
    }
}
impl<T> std::error::Error for OutOfBounds<T> where T: std::fmt::Debug {}

#[derive(Debug, Clone)]
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

    pub fn max_bound<T: SeekTarget>(&self, target: T) -> T {
        target.zero() + self.root.node_summary()
    }

    pub fn push_str(&mut self, mut new_text: &str, sgr: SgrState) {
        // TODO: if new_text is larger than what will fit in a leaf node, create a tree from it and
        // merge
        while !new_text.is_empty() {
            self.find_string_segment_mut(
                SeekCharIdx(self.len_chars()),
                |segment: &mut GridString, _segment_char_idx: usize| -> Option<GridString> {
                    let (overflow, _, rem) = segment.push_str(new_text, sgr);
                    new_text = rem;
                    overflow
                },
            );
        }
    }

    pub fn replace_str<D: SeekTarget>(&mut self, target: D, mut new_text: &str, sgr: SgrState) {
        let mut char_idx = self.resolve_dimension(target).expect("invalid target");
        while !new_text.is_empty() {
            self.find_string_segment_mut(
                SeekCharIdx(char_idx),
                |segment: &mut GridString, segment_char_idx: usize| -> Option<GridString> {
                    if segment_char_idx == segment.len_chars() {
                        let (overflow, chars_written, rem) = segment.push_str(new_text, sgr);
                        new_text = rem;
                        char_idx += chars_written;
                        return overflow;
                    }
                    let (chars_written, rem) = segment.replace_str(segment_char_idx, new_text, sgr);
                    new_text = rem;
                    char_idx += chars_written;
                    None
                },
            );
        }
    }

    pub fn insert_str<D: SeekTarget>(&mut self, target: D, mut new_text: &str, sgr: SgrState) {
        let mut char_idx = self
            .resolve_dimension(target)
            .expect("invalid insert target position");
        while !new_text.is_empty() {
            self.find_string_segment_mut(
                SeekCharIdx(char_idx),
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

    pub fn remove_range<R: RangeBounds<D>, D: SeekTarget>(&mut self, target_range: R) {
        let mut char_range = self
            .resolve_dimensions(target_range)
            .expect("invalid range supplied");
        // let mut char_range =
        //     resolve_range(char_range, 0..self.len_chars()).expect("invalid range supplied");
        while !char_range.is_empty() {
            let n = char_range.len();
            self.find_string_segment_mut(
                SeekCharIdx(char_range.start),
                |segment: &mut GridString, segment_char_idx: usize| -> Option<GridString> {
                    let foo = segment.as_str();
                    let end_char_idx = (segment_char_idx + n).min(segment.len_chars());
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

    pub fn iter_soft_wrapped_lines<R: RangeBounds<usize>>(
        &self,
        wrap_width: NonZeroUsize,
        target_range: R,
    ) -> anyhow::Result<iter::SoftWrappedLineIter> {
        let start_line_idx = match target_range.start_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(n) => *n + 1,
            Bound::Unbounded => 0,
        };
        let end_line_idx = match target_range.end_bound() {
            Bound::Included(n) => *n + 1,
            Bound::Excluded(n) => *n,
            Bound::Unbounded => {
                SeekSoftWrapPosition::compute_max_line(wrap_width, self.root.node_summary())
            }
        };

        Ok(iter::SoftWrappedLineIter::new(
            self,
            wrap_width,
            start_line_idx..end_line_idx,
        ))
    }

    fn find_string_segment_mut<'a, D: SeekTarget>(
        &mut self,
        seek_target: D,
        edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) {
        if let Some(node) = Arc::make_mut(&mut self.root).seek_and_edit_segment(
            seek_target.zero(),
            seek_target,
            edit_op,
        ) {
            self.root = Arc::new(Node::new_internal(
                self.root.height() + 1,
                [self.root.clone(), node],
            ));
        }
    }

    fn from_str(input: &str) -> Self {
        let mut tree = Self::new();
        tree.push_str(input, SgrState::default());
        tree
    }

    pub fn resolve_dimension<D: SeekTarget>(
        &self,
        seek_target: D,
    ) -> Result<usize, OutOfBounds<D>> {
        if seek_target == seek_target.zero() {
            Ok(0)
        } else if seek_target == seek_target.zero() + self.root.node_summary() {
            Ok(self.len_chars())
        } else {
            let foo = self.to_string();
            let agg_summary = TextSummary::default();
            let running_sum = seek_target.zero();
            self.root
                .dimension_to_char_idx(agg_summary, running_sum, seek_target)
        }
    }

    /// resolves a range of Dimensions to corresponding char range.
    fn resolve_dimensions<R: RangeBounds<D>, D: SeekTarget>(
        &self,
        target_range: R,
    ) -> Result<Range<usize>, OutOfBounds<D>> {
        let start = match target_range
            .start_bound()
            .map(|d| self.resolve_dimension(d.clone()))
        {
            std::ops::Bound::Included(char_idx) => char_idx?,
            std::ops::Bound::Excluded(char_idx) => char_idx? + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match target_range
            .end_bound()
            .map(|d| self.resolve_dimension(d.clone()))
        {
            std::ops::Bound::Included(char_idx) => char_idx? + 1,
            std::ops::Bound::Excluded(char_idx) => char_idx?,
            std::ops::Bound::Unbounded => self.len_chars(),
        };
        Ok(start..end)
    }

    fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }

    pub fn truncate<T: SeekTarget>(&mut self, target: T) {
        self.remove_range(target..);
    }
}

#[derive(Debug)]
pub struct TreeSlice {
    pub text: String,
    pub sgr: Vec<SgrState>,
}

mod iter {
    use std::{collections::VecDeque, num::NonZeroUsize, ops::Range};

    use crate::{grid_string::GridString, terminal_emulator::SgrState};

    use super::{
        Node, OutOfBounds, SeekCharIdx, SeekSoftWrapPosition, SeekTarget, Tree, TreeSlice,
    };

    #[derive(Debug)]
    pub struct SoftWrappedLineIter<'a> {
        tree: &'a Tree,
        wrap_width: NonZeroUsize,
        target_line_range: Range<usize>,
        cursor: Option<Cursor<'a>>,
    }

    impl<'a> SoftWrappedLineIter<'a> {
        pub(crate) fn new(
            tree: &'a super::Tree,
            wrap_width: NonZeroUsize,
            target_line_range: Range<usize>,
        ) -> SoftWrappedLineIter {
            Self {
                tree,
                wrap_width,
                target_line_range,
                cursor: None,
            }
        }
    }

    impl<'a> Iterator for SoftWrappedLineIter<'a> {
        type Item = TreeSlice;

        fn next(&mut self) -> Option<Self::Item> {
            if self.target_line_range.is_empty() || self.tree.is_empty() {
                return None;
            }

            if self.cursor.is_none() {
                let mut cursor = Cursor::new(&self.tree);
                cursor
                    .seek_to_char(SeekSoftWrapPosition::new(
                        self.wrap_width,
                        self.target_line_range.start,
                    ))
                    .ok()?;
                self.cursor = Some(cursor);
            }
            let mut cursor = self.cursor.take().expect("cursor initialized above");

            let mut text = String::new();
            let mut sgr = Vec::new();
            let step = SeekSoftWrapPosition::new(self.wrap_width, self.target_line_range.start + 1);
            for (text_chunk, sgr_chunk) in cursor.iter_until(step) {
                text.push_str(text_chunk);
                sgr.extend_from_slice(sgr_chunk);
            }

            self.target_line_range.start += 1;
            self.cursor = Some(cursor);
            Some(TreeSlice { text, sgr })
        }
    }

    #[derive(Debug)]
    struct StackEntry<'a> {
        running_sum: usize,
        node: &'a Node,
        child_idx: usize,
    }
    impl StackEntry<'_> {
        fn next_sum(&self) -> usize {
            self.running_sum
                + self
                    .node
                    .child_summaries()
                    .get(self.child_idx)
                    .map(|summary| summary.chars)
                    .expect("child_idx is not valid")
        }

        fn next_child(&mut self) {
            self.running_sum = self.next_sum();
            self.child_idx += 1;
        }
    }

    #[derive(Debug)]
    /// Points to a specific char in the string stored in the tree. Can be used to seek to specific
    /// char_idx and do operations.
    pub struct Cursor<'a> {
        tree: &'a Tree,
        /// the char_idx in the range [0..tree.len_chars())
        char_position: usize,
        stack: Vec<StackEntry<'a>>,
    }

    impl<'a> Cursor<'a> {
        pub fn new(tree: &'a Tree) -> Self {
            Self {
                tree,
                stack: vec![],
                char_position: 0,
            }
        }

        pub fn seek_to_char<T: SeekTarget>(
            &mut self,
            seek_target: T,
        ) -> Result<(), OutOfBounds<T>> {
            let target_char_idx = self.tree.resolve_dimension(seek_target)?;
            self.prune_stack(target_char_idx);

            if target_char_idx == self.tree.len_chars() {
                self.char_position = target_char_idx;
                return Ok(());
            }

            while let Some(StackEntry {
                node: parent,
                running_sum,
                child_idx,
            }) = self.stack.last_mut()
            {
                let Some(child_summary) = parent.child_summaries().get(*child_idx) else {
                    self.stack.pop();
                    if let Some(parent_entry) = self.stack.last_mut() {
                        parent_entry.next_child();
                    }
                    continue;
                };

                let next_sum = *running_sum + child_summary.chars;
                if next_sum <= target_char_idx {
                    *child_idx += 1;
                    *running_sum = next_sum;
                    continue;
                }

                match **parent {
                    Node::Leaf { .. } => {
                        self.char_position = target_char_idx;
                        return Ok(());
                    }
                    Node::Internal { ref children, .. } => {
                        let child = children[*child_idx].as_ref();
                        let running_sum_for_child = *running_sum;
                        self.stack.push(StackEntry {
                            node: child,
                            running_sum: running_sum_for_child,
                            child_idx: 0,
                        });
                    }
                }
            }
            Err(OutOfBounds {
                attempted_position: seek_target,
                last_valid_position: self.tree.max_bound(seek_target),
            })
        }

        fn prune_stack(&mut self, target_char_idx: usize) {
            self.stack.retain(|entry| {
                entry.running_sum <= target_char_idx && entry.next_sum() > target_char_idx
            });

            if self.stack.is_empty() {
                self.stack.push(StackEntry {
                    node: self.tree.root.as_ref(),
                    running_sum: 0,
                    child_idx: 0,
                });
            }
        }

        pub fn segment(&self) -> Option<&GridString> {
            let StackEntry {
                node, child_idx, ..
            } = self.stack.last()?;

            node.child_segments().get(*child_idx)
        }

        pub fn advance_char_offset(
            &mut self,
            delta: usize,
        ) -> Result<(), OutOfBounds<SeekCharIdx>> {
            self.seek_to_char(SeekCharIdx(self.char_position + delta))
        }

        pub fn segment_str(&self) -> Option<&str> {
            let StackEntry {
                node,
                child_idx,
                running_sum,
                ..
            } = self.stack.last()?;

            node.child_segments().get(*child_idx).and_then(|segment| {
                let offset = self.char_position - *running_sum;
                segment.as_str().get(offset..)
            })
        }

        fn segment_sgr(&self) -> Option<&[SgrState]> {
            let StackEntry {
                node,
                child_idx,
                running_sum,
                ..
            } = self.stack.last()?;

            node.child_segments().get(*child_idx).and_then(|segment| {
                let offset = self.char_position - *running_sum;
                segment.sgr().get(offset..)
            })
        }

        pub fn iter_until<'b, T: SeekTarget + 'b>(
            &'b mut self,
            seek_target: T,
        ) -> impl Iterator<Item = (&'a str, &'a [SgrState])> + 'b {
            std::iter::from_fn(move || {
                let target_char_idx = self
                    .tree
                    .resolve_dimension(seek_target)
                    .unwrap_or(self.tree.len_chars());
                if self.char_position >= target_char_idx {
                    return None;
                }
                let StackEntry {
                    node,
                    child_idx,
                    running_sum,
                    ..
                } = self.stack.pop()?;
                let segment = node.child_segments().get(child_idx)?;
                let segment_offset = self.char_position - running_sum;
                let segment_chars_remaining = segment.len_chars() - segment_offset;
                let num_chars = std::cmp::min(
                    target_char_idx - self.char_position,
                    segment_chars_remaining,
                );

                let start_byte_idx = if segment_offset == 0 {
                    0
                } else {
                    let mut char_iter = segment.as_str().char_indices();
                    char_iter
                        .nth(segment_offset)
                        .map(|(byte_offset, _)| byte_offset)?
                };
                let end_byte_idx = if num_chars < segment_chars_remaining {
                    let mut char_iter = segment.as_str().char_indices();
                    let (byte_offset, _) = char_iter.nth(segment_offset + num_chars)?;
                    byte_offset
                } else {
                    segment.len_bytes()
                };

                self.stack.push(StackEntry {
                    node,
                    child_idx,
                    running_sum,
                });
                self.advance_char_offset(num_chars)
                    .expect("should be able to advance as much as the data we are returning");
                let segment_str = &segment.as_str()[start_byte_idx..end_byte_idx];
                let segment_sgr = &segment.sgr()[segment_offset..segment_offset + num_chars];

                Some((segment_str, segment_sgr))
            })
        }
    }

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
                .child_segments()
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
    fn child_position<D: SeekTarget>(
        &self,
        mut running_sum: D,
        seek_target: D,
    ) -> Option<(usize, D)> {
        let end = running_sum + self.node_summary();
        if seek_target > end {
            return None;
        }

        // TODO: maybe remove the need for this?
        if seek_target == end {
            // seeking to the end
            let (_, prefix) = self
                .child_summaries()
                .split_last()
                .expect("all nodes have at least one child");
            for child_summary in prefix {
                running_sum = running_sum + child_summary;
            }
            let child_idx = self.child_summaries().len() - 1;
            return Some((child_idx, running_sum));
        }

        for (child_idx, child_summary) in self.child_summaries().into_iter().enumerate() {
            let next_sum = running_sum + child_summary;
            if next_sum > seek_target {
                return Some((child_idx, running_sum));
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

    fn child_segments(&self) -> &ArrayVec<GridString, MAX_CHILDREN> {
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

    fn seek_and_edit_segment<D: SeekTarget>(
        &mut self,
        running_sum: D,
        seek_target: D,
        mut edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) -> Option<Arc<Node>> {
        let (child_idx, running_sum) = self
            .child_position(running_sum, seek_target)
            .expect("unable to determine child position");
        let split = match self {
            Node::Leaf { children, .. } => {
                let child = &mut children[child_idx];
                let char_idx = seek_target.to_char_idx(running_sum, child.as_str()).ok()?;
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
                child
                    .seek_and_edit_segment(running_sum, seek_target, edit_op)
                    .and_then(|split_node| -> Option<Arc<Node>> {
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
                    })
            }
        };
        self.recompute_summaries();
        split
    }

    fn has_room_for_children(&self) -> bool {
        self.child_summaries().len() < MAX_CHILDREN
    }

    fn dimension_to_char_idx<D: SeekTarget>(
        &self,
        mut agg_summary: TextSummary,
        mut running_sum: D,
        seek_target: D,
    ) -> Result<usize, OutOfBounds<D>> {
        for (child_idx, child_summary) in self.child_summaries().into_iter().enumerate() {
            {
                match self {
                    Node::Leaf { children, .. } => {
                        let foo = children[child_idx].as_str();
                        let _ = foo.len();
                    }
                    Node::Internal {
                        children,
                        child_summaries,
                        node_summary,
                        ..
                    } => {
                        let sub_tree = Tree {
                            root: children[child_idx].clone(),
                        };
                        let sub_tree_str = sub_tree.to_string();
                        let _ = sub_tree_str.len();
                    }
                }
            }
            let next_sum = running_sum + child_summary;
            if next_sum > seek_target {
                return match self {
                    Node::Leaf { children, .. } => {
                        let segment = &children[child_idx];
                        let child_str = segment.as_str();
                        let child_char_idx = seek_target.to_char_idx(running_sum, child_str)?;
                        Ok(agg_summary.chars + child_char_idx)
                    }
                    Node::Internal { children, .. } => children[child_idx].dimension_to_char_idx(
                        agg_summary,
                        running_sum,
                        seek_target,
                    ),
                };
            }
            running_sum = next_sum;
            agg_summary = agg_summary + child_summary;
        }
        Err(OutOfBounds {
            attempted_position: seek_target,
            last_valid_position: running_sum,
        })
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
            // to ensure that we preserve the seam and not compact items at old_seam_idx and the
            // next one, we do the compaction on two separate partitions.
            let (prefix, suffix) = items.split_at_mut(old_seam_idx);
            // to ensure that we keep at least B children, we should stop compaction as soon as we free
            // up exactly one slot. So lazily evaluate the suffix compaction.
            Self::compact_slice(prefix)
                .or_else(|| Self::compact_slice(suffix).map(|i| prefix.len() + i))
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

    /// returns the index of the slot we freed up, if any
    fn compact_slice(items: &mut [Self]) -> Option<usize> {
        let mut i = 0;
        while i + 1 < items.len() {
            let (prefix, suffix) = items.split_at_mut(i + 1);
            let a = &mut prefix[i];
            let b = &mut suffix[0];
            Self::compact_two(a, b);
            if b.is_empty() {
                // must only free up one slot to maintain tree semantics
                return Some(i + 1);
            } else {
                i += 1;
            }
        }
        None
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
        let (to_write, rem) =
            split_str_at_utf8_boundary(b.as_str(), a.buf_mut().remaining_capacity());
        let chars_written = to_write.chars().count();

        a.buf_mut().push_str(to_write);
        let mut temp = ArrayString::new();
        temp.push_str(rem);
        *b.buf_mut() = temp;
        a.sgr_mut().extend(b.sgr_mut().drain(0..chars_written));

        a.buf_mut().remaining_capacity() == 0 || !b.is_empty()
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
                write!(f, "Leaf({}, {} chars, <", children.len(), self.len_chars())?;
                for child in children {
                    write!(f, "{:?}|", child.as_str())?;
                }
                write!(f, ">)")
            }
            Self::Internal {
                height, children, ..
            } => {
                writeln!(
                    f,
                    "Height: {height}, {} children, {} chars",
                    children.len(),
                    self.len_chars()
                )?;
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
pub trait SeekTarget:
    Default
    + Clone
    + Copy
    + for<'a> std::ops::Add<&'a TextSummary, Output = Self>
    + std::ops::Add<Self, Output = Self>
    + std::ops::Sub<Self, Output = Self>
    + std::cmp::Ord
    + std::fmt::Debug
{
    fn to_char_idx(&self, running_sum: Self, segment: &str) -> Result<usize, OutOfBounds<Self>>;

    fn zero(&self) -> Self {
        Self::default()
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekCharIdx(pub usize);

impl SeekTarget for SeekCharIdx {
    fn to_char_idx(
        &self,
        SeekCharIdx(running_sum): Self,
        _s: &str,
    ) -> Result<usize, OutOfBounds<Self>> {
        Ok(self.0 - running_sum)
    }
}
impl<'a> std::ops::Add<&'a TextSummary> for SeekCharIdx {
    type Output = Self;

    fn add(self, rhs: &'a TextSummary) -> Self::Output {
        Self(self.0 + rhs.chars)
    }
}
impl std::ops::Add for SeekCharIdx {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl std::ops::Sub for SeekCharIdx {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekSoftWrapPosition {
    pub wrap_width: NonZeroUsize,
    pub line_idx: usize,
    pub trailing_line_chars: usize,
}

impl Default for SeekSoftWrapPosition {
    fn default() -> Self {
        Self {
            wrap_width: NonZeroUsize::new(1).expect("1 is non-zero"),
            line_idx: Default::default(),
            trailing_line_chars: Default::default(),
        }
    }
}

impl SeekSoftWrapPosition {
    pub fn new(wrap_width: NonZeroUsize, line_idx: usize) -> Self {
        Self {
            wrap_width,
            line_idx,
            trailing_line_chars: 0,
        }
    }

    pub fn total_rows(&self) -> usize {
        if self.trailing_line_chars > 0 {
            self.line_idx + 1
        } else {
            self.line_idx
        }
    }

    fn compute_max_line(wrap_width: NonZeroUsize, summary: &TextSummary) -> usize {
        let ret = Self::new(wrap_width, 0) + summary;
        ret.total_rows()
    }

    fn add_char(mut self, ch: char) -> Self {
        if ch == '\n' || self.trailing_line_chars + 1 == self.wrap_width.get() {
            self.line_idx += 1;
            self.trailing_line_chars = 0;
        } else {
            self.trailing_line_chars += 1;
        }
        self
    }
}
impl SeekTarget for SeekSoftWrapPosition {
    fn zero(&self) -> Self {
        Self {
            wrap_width: self.wrap_width,
            ..Default::default()
        }
    }

    fn to_char_idx(&self, mut running_sum: Self, s: &str) -> Result<usize, OutOfBounds<Self>> {
        assert!(
            self.trailing_line_chars < self.wrap_width.get(),
            "trailing chars must be less than the line wrap width"
        );
        for (i, ch) in s.chars().enumerate() {
            match running_sum.cmp(self) {
                std::cmp::Ordering::Less => {
                    let next_sum = running_sum.add_char(ch);
                    if next_sum > *self {
                        // we overshot the target; return the last valid target
                        break;
                    }
                    running_sum = next_sum
                }
                std::cmp::Ordering::Equal => return Ok(i),
                std::cmp::Ordering::Greater => break,
            };
        }
        Err(OutOfBounds {
            attempted_position: *self,
            last_valid_position: running_sum,
        })
    }
}

impl<'a> std::ops::Add<&'a TextSummary> for SeekSoftWrapPosition {
    type Output = Self;

    fn add(self, rhs: &'a TextSummary) -> Self {
        let partials = self.trailing_line_chars + rhs.leading_line_chars;
        let new_lines;
        let trailing_line_chars;
        if rhs.lines == 0 {
            new_lines = partials / self.wrap_width;
            trailing_line_chars = partials % self.wrap_width;
        } else {
            new_lines =
                // any lines from the leading joined partial line
                (partials / self.wrap_width)
                // any lines from the middle hard-wrapped lines
                + std::cmp::max(
                    // all hard wrap lines also count as soft wrap ones
                    rhs.lines,
                    (rhs.chars - rhs.leading_line_chars - rhs.trailing_line_chars) / self.wrap_width,
                )
                // any lines from the trailing partial line
                + (rhs.trailing_line_chars / self.wrap_width);
            trailing_line_chars = rhs.trailing_line_chars % self.wrap_width;
        }
        Self {
            wrap_width: self.wrap_width,
            line_idx: self.line_idx + new_lines,
            trailing_line_chars,
        }
    }
}
impl std::ops::Sub for SeekSoftWrapPosition {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        todo!()
    }
}
impl std::ops::Add for SeekSoftWrapPosition {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

fn summarize<'a, T: IntoIterator<Item = &'a TextSummary>>(summaries: T) -> TextSummary {
    summaries
        .into_iter()
        .fold(TextSummary::default(), |agg, s| agg + s)
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TextSummary {
    chars: usize,
    bytes: usize,
    lines: usize,
    leading_line_chars: usize,
    trailing_line_chars: usize,
}
impl std::ops::Add<&TextSummary> for TextSummary {
    type Output = TextSummary;

    fn add(self, rhs: &TextSummary) -> Self::Output {
        let leading_line_chars = if self.lines == 0 {
            self.leading_line_chars + rhs.leading_line_chars
        } else {
            self.leading_line_chars
        };
        let trailing_line_chars = if rhs.lines == 0 {
            self.trailing_line_chars + rhs.trailing_line_chars
        } else {
            rhs.trailing_line_chars
        };
        TextSummary {
            chars: self.chars + rhs.chars,
            bytes: self.bytes + rhs.bytes,
            lines: self.lines + rhs.lines,
            leading_line_chars,
            trailing_line_chars,
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
            leading_line_chars: value
                .as_str()
                .split('\n')
                .next()
                .map(|partial_line| partial_line.chars().count())
                .unwrap_or(value.len_chars()),
            trailing_line_chars: value
                .as_str()
                .rsplit('\n')
                .next()
                .map(|partial_line| partial_line.chars().count())
                .unwrap_or(value.len_chars()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal_emulator::SgrState;

    use self::iter::Cursor;

    use super::*;
    const LARGE_TEXT: &'static str = include_str!("tree.rs");

    #[test]
    fn test_text_summary_1() {
        let summary_1 = TextSummary::from(&GridString::from_str("abcd").unwrap());
        assert_eq!(summary_1.trailing_line_chars, 4);

        let summary_2 = TextSummary::from(&GridString::from_str("\nabcd").unwrap());
        assert_eq!(summary_2.trailing_line_chars, 4);

        let summary_3 = TextSummary::from(&GridString::from_str("a\nb\ncd").unwrap());
        assert_eq!(summary_3.trailing_line_chars, 2);

        let summary_4 = TextSummary::from(&GridString::from_str("abcd\n").unwrap());
        assert_eq!(summary_4.trailing_line_chars, 0);

        assert_eq!(
            [summary_1, summary_2, summary_3, summary_4]
                .into_iter()
                .reduce(|a, b| a + &b)
                .map(|sum| sum.trailing_line_chars),
            Some(0)
        );
    }

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

        tree.replace_str(SeekCharIdx(6), "back1!", bold);
        assert_eq!(tree.to_string().as_str(), "hello back1!!\n");
        assert_eq!(tree.len_bytes(), 14);
        assert_eq!(tree.len_chars(), 14);
        assert_eq!(tree.len_lines(), 1);
    }

    #[test]
    fn test_large_input_1() {
        let mut tree = Tree::new();
        let input = &LARGE_TEXT[..LARGE_TEXT.len()];
        tree.push_str(input, SgrState::default());
        assert_eq!(tree.to_string().as_str(), input);

        // all levels other than the root should have at least B children
        for (idx, node) in tree.iter_nodes().enumerate() {
            assert!(
                idx == 0 || node.is_leaf() || node.child_summaries().len() >= B,
                "{node:?}\n{tree:?}"
            );
        }
    }

    #[test]
    fn test_large_input_2() {
        let mut tree = Tree::new();
        let input = &LARGE_TEXT[..];
        for line in input.split_inclusive('\n').rev() {
            tree.insert_str(SeekCharIdx(0), line, SgrState::default());
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
        tree.insert_str(SeekCharIdx(14), "Line 2.5\n", SgrState::default());
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
        tree.remove_range(SeekCharIdx(7)..SeekCharIdx(14));
        assert_eq!(tree.to_string().as_str(), "Line 1\nLine 3\n");
        assert_eq!(tree.len_lines(), 2)
    }

    #[test]
    fn test_compact_grid_string_1() {
        let bold = SgrState {
            bold: true,
            ..Default::default()
        };
        let d = SgrState::default();

        let mut gs1 = GridString::from_str("abcdef").unwrap();
        let mut gs2 = GridString::from_str("").unwrap();
        gs2.push_str("g\u{0d30}2345", bold);
        Compactable::compact_two(&mut gs1, &mut gs2);

        assert_eq!(gs1.as_str(), "abcdefg");
        assert_eq!(gs1.sgr().len(), 7);
        assert_eq!(gs1.sgr(), &[d, d, d, d, d, d, bold]);
        assert_eq!(gs2.as_str(), "\u{0d30}2345");
        assert_eq!(gs2.sgr().len(), 5);
        assert_eq!(gs2.sgr(), &[bold, bold, bold, bold, bold]);
    }

    #[test]
    fn test_dim_line_char() {
        let mut tree = Tree::new();
        const N: usize = 100;
        for i in 0..N {
            tree.push_str(format!("Line {:03}\n", i).as_str(), SgrState::default());
        }
        assert_eq!(tree.len_lines(), N);

        let line_bytes_len = "Line 000\n".len();
        assert_eq!(tree.len_bytes(), N * line_bytes_len);

        assert_eq!(
            tree.resolve_dimension(SeekSoftWrapPosition::new(
                usize::MAX.try_into().unwrap(),
                42
            ))
            .unwrap(),
            42 * line_bytes_len
        )
    }

    #[test]
    fn test_cursor_1() -> anyhow::Result<()> {
        let tree = Tree::from_str("Line 1\nLine 2\nLine 3");
        let mut cursor = Cursor::new(&tree);
        cursor.seek_to_char(SeekCharIdx(19))?;
        assert_eq!(cursor.segment_str(), Some("3"));
        cursor.seek_to_char(SeekCharIdx(16))?;
        assert_eq!(cursor.segment_str(), Some("ne 3"));
        cursor.seek_to_char(SeekCharIdx(0))?;
        assert_eq!(cursor.segment_str(), Some("Line 1\nL"));
        Ok(())
    }

    #[test]
    fn test_cursor_2() -> anyhow::Result<()> {
        let input = &LARGE_TEXT;
        let tree = Tree::from_str(input);
        let mut cursor = Cursor::new(&tree);
        let target_line = line!() as usize - 1; // 1 based
        cursor.seek_to_char(SeekSoftWrapPosition::new(
            usize::MAX.try_into().unwrap(),
            target_line,
        ))?;
        let mut output = String::new();
        for (text, _sgr) in cursor.iter_until(SeekSoftWrapPosition::new(
            usize::MAX.try_into().unwrap(),
            target_line + 1,
        )) {
            output.push_str(text);
        }
        assert_eq!(
            output.as_str(),
            "        let target_line = line!() as usize - 1; // 1 based\n"
        );
        Ok(())
    }

    #[test]
    fn test_dim_soft_wrap_1() {
        let mut tree = Tree::new();
        const N: usize = 10;
        for i in 0..N {
            let line = if i % 2 == 0 {
                // even lines are longer and without a trailing newline
                format!("Long Line {:03}.", i)
            } else {
                // odd lines are shorter with a newLine
                format!("<{:02}>\n", i)
            };
            tree.push_str(line.as_str(), SgrState::default());
        }

        let wrap_width: NonZeroUsize = 8.try_into().unwrap();
        // with hard wraps, lines looks like:
        //
        // 00: |Long Line 000. <01>|
        // 01: |Long Line 002. <03>|
        // 02: |Long Line 004. <05>|
        // 03: |Long Line 006. <07>|
        // 04: |Long Line 008. <09>|
        //
        // with soft wrap at 8, lines look like:
        //
        // 00: |Long Lin|    <-- soft wrap
        // 01: |e 000. <|    <-- soft wrap
        // 02: |01>     |    <-- hard wrap
        // 03: |Long Lin|
        // 04: |e 002. <|
        // 05: |03>     |
        // 06: |Long Lin|
        // 07: |e 004. <|
        // 08: |05>     |
        // 09: |Long Lin|
        // 10: |e 006. <|
        // 11: |07>     |
        // 12: |Long Lin|
        // 13: |e 008. <|
        // 14: |09>     |

        // valid position
        assert_eq!(
            tree.resolve_dimension(SeekSoftWrapPosition {
                wrap_width,
                line_idx: 2,
                trailing_line_chars: 1,
            })
            .unwrap(),
            17
        );

        // invalid position
        assert_eq!(
            tree.resolve_dimension(SeekSoftWrapPosition {
                wrap_width,
                line_idx: 2,
                trailing_line_chars: 3,
            })
            .map_err(|err| err.last_valid_position),
            Err(SeekSoftWrapPosition {
                wrap_width,
                line_idx: 2,
                trailing_line_chars: 2,
            }),
        );
    }

    #[test]
    fn test_dim_soft_wrap_2() {
        let summaries = vec![
            TextSummary {
                chars: 24,
                bytes: 24,
                lines: 1,
                leading_line_chars: 0,
                trailing_line_chars: 24,
            },
            TextSummary {
                chars: 22,
                bytes: 24,
                lines: 2,
                leading_line_chars: 12,
                trailing_line_chars: 4,
            },
            TextSummary {
                chars: 24,
                bytes: 24,
                lines: 0,
                leading_line_chars: 24,
                trailing_line_chars: 24,
            },
            TextSummary {
                chars: 24,
                bytes: 24,
                lines: 1,
                leading_line_chars: 1,
                trailing_line_chars: 22,
            },
            TextSummary {
                chars: 13,
                bytes: 13,
                lines: 2,
                leading_line_chars: 9,
                trailing_line_chars: 2,
            },
            TextSummary {
                chars: 36,
                bytes: 38,
                lines: 1,
                leading_line_chars: 33,
                trailing_line_chars: 2,
            },
        ];

        let target = SeekSoftWrapPosition {
            wrap_width: 35.try_into().unwrap(),
            line_idx: 6,
            trailing_line_chars: 2,
        };

        let mut running_sum = target.zero();
        for s in &summaries {
            let next_sum = running_sum + s;
            if next_sum > target {
                break;
            }
            running_sum = next_sum;
        }

        assert_eq!(running_sum.line_idx, 5);
        assert_eq!(running_sum.trailing_line_chars, 22);

        let input = "abcdefghijklmnopqrstuvwxyz";
        let partial_line_chars = 35 - 22;
        assert_eq!(
            target.to_char_idx(running_sum, input).unwrap(),
            partial_line_chars + target.trailing_line_chars
        );
    }

    #[test]
    fn test_soft_wrap_iter_1() {
        let tree = Tree::from_str("abcdef");
        let mut iter = tree
            .iter_soft_wrapped_lines(2.try_into().unwrap(), ..)
            .unwrap();
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("ab"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("cd"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("ef"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), None);
    }

    #[test]
    fn test_soft_wrap_iter_2() {
        let tree = Tree::from_str("a\nb\nc\nd\ne\nf");
        let mut iter = tree
            .iter_soft_wrapped_lines(2.try_into().unwrap(), ..)
            .unwrap();
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("a\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("b\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("c\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("d\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("e\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("f"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), None);
    }
}
