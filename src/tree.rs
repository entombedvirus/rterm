#![allow(dead_code)]

use std::num::NonZeroU32;
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
    pub last_char_idx: usize,
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct SummarizeContext {
    wrap_width: Option<NonZeroU32>,
}

#[derive(Debug, Clone)]
pub struct Tree {
    summarize_context: SummarizeContext,
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
        let summarize_context = SummarizeContext::default();
        let root = Arc::new(Node::new_leaf(summarize_context, [GridString::default()]));
        Self {
            summarize_context,
            root,
        }
    }

    pub fn rewrap(&mut self, new_wrap_width: NonZeroU32) {
        if self.summarize_context.wrap_width != Some(new_wrap_width) {
            let old_wrap_width = self.summarize_context.wrap_width.replace(new_wrap_width);
            Arc::make_mut(&mut self.root).rewrap(self.summarize_context, old_wrap_width);
        }
    }

    pub fn len_bytes(&self) -> usize {
        self.root.node_summary().bytes
    }

    pub fn len_chars(&self) -> usize {
        self.root.node_summary().chars
    }

    pub fn len_lines(&self) -> usize {
        self.root.node_summary().lines.num_complete as usize
    }

    pub fn max_bound<T: SeekTarget>(&self) -> T {
        T::from_summary(self.summarize_context, self.root.node_summary())
    }

    pub fn push_str(&mut self, mut new_text: &str, sgr: SgrState) {
        if new_text.is_empty() {
            return;
        }

        let cx = self.summarize_context;
        let root = Arc::make_mut(&mut self.root);
        let rem = root.push_str(cx, new_text, sgr);
        new_text = rem;

        if !new_text.is_empty() {
            self.append_tree(Self::from_str2(cx, new_text, sgr));
        }
        // while !new_text.is_empty() {
        //     // self.find_string_segment_mut(
        //     //     SeekCharIdx(self.len_chars()),
        //     //     |segment: &mut GridString, _segment_char_idx: usize| -> Option<GridString> {
        //     //         let (overflow, _, rem) = segment.push_str(new_text, sgr);
        //     //         new_text = rem;
        //     //         overflow
        //     //     },
        //     // );
        // }
    }

    pub fn replace_str<D: SeekTarget>(&mut self, target: D, mut new_text: &str, sgr: SgrState) {
        let mut char_idx = self
            .resolve_dimension(target.clone())
            .expect("replace_str: invalid target");
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
        while !char_range.is_empty() {
            let n = char_range.len();
            self.find_string_segment_mut(
                SeekCharIdx(char_range.start),
                |segment: &mut GridString, segment_char_idx: usize| -> Option<GridString> {
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

    pub fn iter_soft_wrapped_lines<R: RangeBounds<u32>>(
        &self,
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
            Bound::Unbounded => self.max_bound::<SeekSoftWrapPosition>().total_rows(),
        };

        Ok(iter::SoftWrappedLineIter::new(
            self,
            start_line_idx..end_line_idx,
        ))
    }

    fn find_string_segment_mut<D: SeekTarget>(
        &mut self,
        seek_target: D,
        edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) {
        if let Some(node) = Arc::make_mut(&mut self.root).seek_and_edit_segment(
            self.summarize_context,
            TextSummary::default(),
            seek_target,
            edit_op,
        ) {
            self.root = Arc::new(Node::new_internal(
                self.summarize_context,
                self.root.height() + 1,
                [self.root.clone(), node],
            ));
        }
    }

    pub fn resolve_dimension<T: SeekTarget>(
        &self,
        seek_target: T,
    ) -> Result<usize, OutOfBounds<T>> {
        if seek_target == seek_target.zero() {
            Ok(0)
        } else if seek_target == self.max_bound() {
            Ok(self.len_chars())
        } else {
            let running_sum = TextSummary::default();
            self.root
                .dimension_to_char_idx(self.summarize_context, running_sum, seek_target)
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

    pub fn clear(&mut self) {
        let cx = self.summarize_context;
        *self = Self::new();
        self.summarize_context = cx;
    }

    pub fn get_char(&self, char_idx: usize) -> Option<char> {
        if char_idx >= self.len_chars() {
            return None;
        }
        let mut cursor = iter::Cursor::new(self);
        cursor.seek_to_char(SeekCharIdx(char_idx)).ok()?;
        cursor.segment_str()?.chars().next()
    }

    pub fn ends_with_newline(&self) -> bool {
        self.root.node_summary().lines.ends_with_newline
    }

    fn append_tree(&mut self, other: Tree) {
        assert_eq!(
            self.summarize_context, other.summarize_context,
            "self: {self:?}, other: {other:?}"
        );
        let cx = self.summarize_context;

        match self.root.height().cmp(&other.root.height()) {
            std::cmp::Ordering::Less => {
                let Node::Internal { children, .. } = other.root.as_ref() else {
                    unreachable!("other has a bigger height implies internal node")
                };
                for child in children {
                    let subtree = Tree {
                        root: child.clone(),
                        summarize_context: cx,
                    };
                    self.append_tree(subtree);
                }
            }
            std::cmp::Ordering::Equal => {
                match Arc::make_mut(&mut self.root) {
                    Node::Leaf { .. } => {
                        // both self and other are just single leaf nodes
                        let new_root =
                            Node::new_internal(cx, 1, [self.root.clone(), other.root.clone()]);
                        self.root = Arc::new(new_root);
                    }
                    Node::Internal {
                        height,
                        node_summary,
                        child_summaries,
                        children,
                        ..
                    } => {
                        let Node::Internal {
                            children: other_children,
                            child_summaries: other_child_summaries,
                            ..
                        } = other.root.as_ref()
                        else {
                            unreachable!(
                                "other is also an internal node because the height matches"
                            )
                        };
                        if children.remaining_capacity() >= other_children.len() {
                            // will fit: combine children
                            children.extend(other_children.iter().cloned());
                            child_summaries.extend(other_child_summaries.iter().cloned());
                            let summary = other.root.node_summary().clone();
                            *node_summary = node_summary.add(cx, &summary);
                        } else {
                            // won't fit: split the root node
                            self.root = Arc::new(Node::new_internal(
                                cx,
                                *height + 1,
                                [self.root.clone(), other.root.clone()],
                            ));
                        }
                    }
                }
            }
            std::cmp::Ordering::Greater => {
                let Node::Internal {
                    height,
                    node_summary,
                    child_summaries,
                    children,
                    ..
                } = Arc::make_mut(&mut self.root)
                else {
                    unreachable!("self has a bigger height implies internal node")
                };
                if other.root.height() + 1 == *height {
                    // we can add other as a child tree
                    match children.try_push(other.root.clone()) {
                        Ok(_) => {
                            let summary = other.root.node_summary().clone();
                            *node_summary = node_summary.add(cx, &summary);
                            child_summaries.push(summary);
                        }
                        Err(err) => {
                            let right_children =
                                split_children(children, children.len(), err.element());
                            child_summaries.truncate(children.len());
                            *node_summary = summarize(cx, &*child_summaries);
                            let right_node =
                                Arc::new(Node::new_internal(cx, *height, right_children));
                            self.root = Arc::new(Node::new_internal(
                                cx,
                                *height + 1,
                                [self.root.clone(), right_node],
                            ));
                        }
                    }
                } else {
                    // other cannot be added directly as a child, but deeper
                    let last_child = children
                        .pop()
                        .expect("internal nodes have at least one child");
                    let _ = child_summaries.pop();
                    *node_summary = summarize(cx, &*child_summaries);

                    let mut subtree = Tree {
                        summarize_context: cx,
                        root: last_child.clone(),
                    };
                    subtree.append_tree(other);
                    self.append_tree(subtree);
                }
            }
        }
    }

    pub fn from_str(input: &str) -> Self {
        Self::from_str2(SummarizeContext::default(), input, SgrState::default())
    }

    fn from_str2(cx: SummarizeContext, mut new_text: &str, sgr: SgrState) -> Tree {
        if new_text.is_empty() {
            return Self::new();
        }

        let mut nodes = Vec::new();
        while !new_text.is_empty() {
            let (new_leaf, rem) = Node::leaf_from_str(cx, new_text, sgr);
            nodes.push(Arc::new(new_leaf));
            new_text = rem;
        }

        let mut height = 0;
        while nodes.len() > 1 {
            height += 1;
            nodes = nodes
                .chunks(MAX_CHILDREN)
                .map(|children| {
                    let children = children.iter().cloned();
                    Arc::new(Node::new_internal(cx, height, children))
                })
                .collect();
        }

        Tree {
            summarize_context: cx,
            root: nodes.pop().unwrap(),
        }
    }
}

#[derive(Debug)]
pub struct TreeSlice {
    pub text: String,
    pub sgr: Vec<SgrState>,
}

mod iter {
    use std::{collections::VecDeque, ops::Range};

    use crate::{grid_string::GridString, terminal_emulator::SgrState};

    use super::{
        Node, OutOfBounds, SeekCharIdx, SeekSoftWrapPosition, SeekTarget, Tree, TreeSlice,
    };

    #[derive(Debug)]
    pub struct SoftWrappedLineIter<'a> {
        tree: &'a Tree,
        target_line_range: Range<u32>,
        cursor: Option<Cursor<'a>>,
    }

    impl<'a> SoftWrappedLineIter<'a> {
        pub(crate) fn new(
            tree: &'a super::Tree,
            target_line_range: Range<u32>,
        ) -> SoftWrappedLineIter {
            Self {
                tree,
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
                let mut cursor = Cursor::new(self.tree);
                cursor
                    .seek_to_char(SeekSoftWrapPosition::new(self.target_line_range.start, 0))
                    .ok()?;
                self.cursor = Some(cursor);
            }
            let mut cursor = self.cursor.take().expect("cursor initialized above");

            let mut text = String::new();
            let mut sgr = Vec::new();
            let step = SeekSoftWrapPosition::new(self.target_line_range.start + 1, 0);
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
            let target_char_idx = self.tree.resolve_dimension(seek_target.clone())?;
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
                attempted_position: seek_target.clone(),
                last_valid_position: self.tree.max_bound(),
                last_char_idx: self.tree.len_chars(),
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
            let target_char_idx = self
                .tree
                .resolve_dimension(seek_target.clone())
                .unwrap_or_else(|err| (err.last_char_idx + 1).min(self.tree.len_chars()));
            std::iter::from_fn(move || {
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
        self.node_summary().lines.num_complete as usize
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

    fn new_leaf<I: IntoIterator<Item = GridString>>(cx: SummarizeContext, children: I) -> Node {
        let children: ArrayVec<GridString, MAX_CHILDREN> = children.into_iter().collect();
        debug_assert!(!children.is_empty());
        let mut n = Node::Leaf {
            children,
            node_summary: TextSummary::default(),
            child_summaries: ArrayVec::new(),
        };
        n.recompute_summaries(cx);
        n
    }

    fn new_internal<I: IntoIterator<Item = Arc<Node>>>(
        cx: SummarizeContext,
        height: u8,
        children: I,
    ) -> Node {
        let children: ArrayVec<Arc<Node>, MAX_CHILDREN> = children.into_iter().collect();
        debug_assert!(!children.is_empty());
        let mut n = Self::Internal {
            height,
            children,
            node_summary: TextSummary::default(),
            child_summaries: ArrayVec::new(),
        };
        n.recompute_summaries(cx);
        n
    }

    fn recompute_summaries(&mut self, cx: SummarizeContext) {
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
                    .map(|c| TextSummary::from_segment(cx, &*c))
                    .for_each(|cs| {
                        *node_summary = node_summary.add(cx, &cs);
                        child_summaries.push(cs);
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
                for child in children {
                    *node_summary = node_summary.add(cx, child.node_summary());
                    child_summaries.push(child.node_summary().clone());
                }
            }
        }
    }

    /// scans this node's children from left to right while the running sum of dimension is less
    /// than the passed in `seek_target`. Returns the index of the child that will cause the
    /// running sum to exceed the target and the running sum so far.
    fn child_position<D: SeekTarget>(
        &self,
        cx: SummarizeContext,
        mut running_sum: TextSummary,
        seek_target: D,
    ) -> Option<(usize, TextSummary)> {
        let end = running_sum.add(cx, self.node_summary());

        match seek_target.cmp_summary(cx, &end) {
            std::cmp::Ordering::Less => {
                for (child_idx, child_summary) in self.child_summaries().iter().enumerate() {
                    let next_sum = running_sum.add(cx, child_summary);
                    if seek_target.cmp_summary(cx, &next_sum) == std::cmp::Ordering::Less {
                        return Some((child_idx, running_sum));
                    }
                    running_sum = next_sum;
                }
                unreachable!("all nodes have at least one child");
            }
            std::cmp::Ordering::Equal => {
                // seeking to the end
                let (_, prefix) = self
                    .child_summaries()
                    .split_last()
                    .expect("all nodes have at least one child");
                for child_summary in prefix {
                    running_sum = running_sum.add(cx, child_summary);
                }
                let child_idx = self.child_summaries().len() - 1;
                Some((child_idx, running_sum))
            }
            std::cmp::Ordering::Greater => None,
        }
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
        cx: SummarizeContext,
        running_sum: TextSummary,
        seek_target: D,
        mut edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) -> Option<Arc<Node>> {
        let (child_idx, running_sum) = self
            .child_position(cx, running_sum, seek_target.clone())
            .expect("unable to determine child position");
        let split = match self {
            Node::Leaf { children, .. } => {
                let child = &mut children[child_idx];
                let char_idx = seek_target
                    .to_char_idx(cx, &running_sum, child.as_str())
                    .expect("to_char_idx failed unexpectedly");
                edit_op(child, char_idx - running_sum.chars).and_then(|overflow| {
                    let overflow_pos = child_idx + 1;
                    children
                        .try_insert(overflow_pos, overflow)
                        .map(|()| None)
                        .unwrap_or_else(|err| -> Option<Arc<Node>> {
                            match Compactable::compact_items_with_seam(cx, children, overflow_pos) {
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
                                    Some(Arc::new(Node::new_leaf(cx, right_children)))
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
                    .seek_and_edit_segment(cx, running_sum, seek_target, edit_op)
                    .and_then(|split_node| -> Option<Arc<Node>> {
                        let split_node_pos = child_idx + 1;
                        children
                            .try_insert(split_node_pos, split_node)
                            .map(|()| None)
                            .unwrap_or_else(|err| {
                                match Compactable::compact_items_with_seam(
                                    cx,
                                    children,
                                    split_node_pos,
                                ) {
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
                                        Some(Arc::new(Node::new_internal(
                                            cx,
                                            *height,
                                            right_children,
                                        )))
                                    }
                                }
                            })
                    })
            }
        };
        self.recompute_summaries(cx);
        split
    }

    fn has_room_for_children(&self) -> bool {
        self.child_summaries().len() < MAX_CHILDREN
    }

    fn dimension_to_char_idx<T: SeekTarget>(
        &self,
        cx: SummarizeContext,
        mut running_sum: TextSummary,
        seek_target: T,
    ) -> Result<usize, OutOfBounds<T>> {
        for (child_idx, child_summary) in self.child_summaries().iter().enumerate() {
            let next_sum = running_sum.add(cx, child_summary);
            if seek_target.cmp_summary(cx, &next_sum) == std::cmp::Ordering::Less {
                return match self {
                    Node::Leaf { children, .. } => {
                        let segment = &children[child_idx];
                        let child_str = segment.as_str();
                        seek_target.to_char_idx(cx, &running_sum, child_str)
                    }
                    Node::Internal { children, .. } => {
                        children[child_idx].dimension_to_char_idx(cx, running_sum, seek_target)
                    }
                };
            }
            running_sum = next_sum;
        }
        Err(OutOfBounds {
            attempted_position: seek_target.clone(),
            last_valid_position: T::from_summary(cx, &running_sum),
            last_char_idx: running_sum.chars,
        })
    }

    fn rewrap(&mut self, cx: SummarizeContext, old_wrap_width: Option<NonZeroU32>) {
        if let Some((new_wrap_width, old_wrap_width)) = cx.wrap_width.zip(old_wrap_width) {
            let longest_line = self.node_summary().lines.longest_line_chars;
            if longest_line < old_wrap_width.get() && longest_line < new_wrap_width.get() {
                return;
            }
        }

        match self {
            Node::Leaf { .. } => self.recompute_summaries(cx),
            Node::Internal { children, .. } => {
                for child in children {
                    Arc::make_mut(child).rewrap(cx, old_wrap_width);
                }
                self.recompute_summaries(cx);
            }
        }
    }

    fn leaf_from_str(cx: SummarizeContext, mut new_text: &str, sgr: SgrState) -> (Node, &str) {
        let mut children: ArrayVec<GridString, MAX_CHILDREN> = ArrayVec::new();
        while !new_text.is_empty() {
            let (segment, _, rem) = GridString::split(new_text, sgr);
            if children.try_push(segment.unwrap()).is_err() {
                break;
            }
            new_text = rem;
        }
        (Node::new_leaf(cx, children), new_text)
    }

    fn push_str<'a>(
        &mut self,
        cx: SummarizeContext,
        mut new_text: &'a str,
        sgr: SgrState,
    ) -> &'a str {
        debug_assert!(
            !new_text.is_empty(),
            "caller should check to avoid repeated checks"
        );

        match self {
            Node::Leaf {
                node_summary,
                children,
                child_summaries,
                ..
            } => {
                let mut needs_recompute = false;
                while !new_text.is_empty() && !children.is_full() {
                    let last_child = children
                        .last_mut()
                        .expect("leaf nodes always have at least one segment");
                    let (split, _, rem) = last_child.push_str(new_text, sgr);
                    match split {
                        Some(segment) => {
                            let summary = TextSummary::from_segment(cx, &segment);
                            if !needs_recompute {
                                // do the work to keep the running tallies if we aren't gonna do an
                                // onverall recompute at the end
                                *node_summary = node_summary.add(cx, &summary);
                                child_summaries.push(summary);
                            }
                            children.push(segment);
                        }
                        None => {
                            // it wrote some prefix, update the last child_summary and node_summary
                            needs_recompute = true;
                        }
                    }
                    new_text = rem;
                }
                if needs_recompute {
                    self.recompute_summaries(cx);
                }
                new_text
            }
            Node::Internal { children, .. } => {
                let last_child = children
                    .last_mut()
                    .expect("internal nodes always have at least one child");
                let rem = Arc::make_mut(last_child).push_str(cx, new_text, sgr);
                self.recompute_summaries(cx);
                rem
            }
        }
    }
}

trait Compactable
where
    Self: Sized,
{
    /// Takes two mutable references to T and compacts them together, possibly modifying them both.
    /// Retruns true if the loop should move on from `a` as compact destination.
    fn compact_two(cx: SummarizeContext, a: &mut Self, b: &mut Self) -> bool;

    fn is_empty(&self) -> bool;

    /// compacts items to the left and right of `seam_idx` while:
    /// - preserving order of items
    /// - not merging across the seam boundary
    ///
    /// Returns where the `seam_idx` falls after merging, if any compaction was done at all.
    fn compact_items_with_seam(
        cx: SummarizeContext,
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
            Self::compact_slice(cx, prefix)
                .or_else(|| Self::compact_slice(cx, suffix).map(|i| prefix.len() + i))
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
    fn compact_slice(cx: SummarizeContext, items: &mut [Self]) -> Option<usize> {
        let mut i = 0;
        while i + 1 < items.len() {
            let (prefix, suffix) = items.split_at_mut(i + 1);
            let a = &mut prefix[i];
            let b = &mut suffix[0];
            Self::compact_two(cx, a, b);
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

    fn compact_two(cx: SummarizeContext, a: &mut Self, b: &mut Self) -> bool {
        let (a, b) = (Arc::make_mut(a), Arc::make_mut(b));
        let updated = if a.is_leaf() && b.is_leaf() {
            Compactable::compact_two(cx, a.child_strings_mut(), b.child_strings_mut())
        } else {
            Compactable::compact_two(cx, a.child_nodes_mut(), b.child_nodes_mut())
        };
        if updated {
            a.recompute_summaries(cx);
            b.recompute_summaries(cx);
        }
        updated
    }
}

impl Compactable for GridString {
    fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }

    fn compact_two(_cx: SummarizeContext, a: &mut Self, b: &mut Self) -> bool {
        let (to_write, rem) =
            split_str_at_utf8_boundary(b.as_str(), a.buf_mut().remaining_capacity());
        let chars_written = to_write.chars().count();

        a.buf_mut().push_str(to_write);
        let mut temp = ArrayString::new();
        temp.push_str(rem);
        *b.buf_mut() = temp;
        a.sgr_mut().extend(b.sgr_mut().drain(0..chars_written));

        chars_written > 0
    }
}

impl<T, const N: usize> Compactable for ArrayVec<T, N> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn compact_two(_cx: SummarizeContext, a: &mut Self, b: &mut Self) -> bool {
        let room_available = a.remaining_capacity();
        let num_children = b.len();
        let to_write = room_available.min(num_children);
        a.extend(b.drain(0..to_write));
        to_write > 0
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
pub trait SeekTarget: Default + Clone + std::fmt::Debug + PartialEq + Eq {
    fn from_summary(cx: SummarizeContext, summary: &TextSummary) -> Self;

    fn cmp_summary(&self, cx: SummarizeContext, summary: &TextSummary) -> std::cmp::Ordering;

    fn to_char_idx(
        &self,
        cx: SummarizeContext,
        running_sum: &TextSummary,
        segment: &str,
    ) -> Result<usize, OutOfBounds<Self>>;

    fn zero(&self) -> Self {
        Self::default()
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekCharIdx(pub usize);

impl SeekTarget for SeekCharIdx {
    fn from_summary(_cx: SummarizeContext, summary: &TextSummary) -> Self {
        Self(summary.chars)
    }

    fn cmp_summary(&self, _cx: SummarizeContext, summary: &TextSummary) -> std::cmp::Ordering {
        self.0.cmp(&summary.chars)
    }

    fn to_char_idx(
        &self,
        _cx: SummarizeContext,
        _running_sum: &TextSummary,
        _s: &str,
    ) -> Result<usize, OutOfBounds<Self>> {
        Ok(self.0)
    }

    fn zero(&self) -> Self {
        Self::default()
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekSoftWrapPosition {
    pub line_idx: u32,
    pub col_idx: u32,
}

impl SeekSoftWrapPosition {
    pub fn new(line_idx: u32, col_idx: u32) -> Self {
        Self { line_idx, col_idx }
    }

    pub fn total_rows(&self) -> u32 {
        if self.col_idx > 0 {
            self.line_idx + 1
        } else {
            self.line_idx
        }
    }

    fn add_char(mut self, wrap_width: Option<u32>, ch: char) -> Self {
        if ch == '\n' || Some(self.col_idx + 1) == wrap_width {
            self.line_idx += 1;
            self.col_idx = 0;
        } else {
            self.col_idx += 1;
        }
        self
    }
}

impl SeekTarget for SeekSoftWrapPosition {
    fn from_summary(cx: SummarizeContext, summary: &TextSummary) -> Self {
        let line_idx = summary.lines.num_complete;
        let last_line = summary.lines.line_chars.last().copied().unwrap_or_default();
        let col_idx = if summary.lines.ends_with_newline {
            0
        } else if let Some(wrap_width) = cx.wrap_width {
            last_line % wrap_width.get()
        } else {
            last_line
        };
        Self { line_idx, col_idx }
    }

    fn cmp_summary(&self, cx: SummarizeContext, summary: &TextSummary) -> std::cmp::Ordering {
        self.line_idx
            .cmp(&summary.lines.num_complete)
            .then_with(|| {
                let trailing_chars = if summary.lines.ends_with_newline {
                    0
                } else {
                    let last_line_chars = summary.lines.line_chars.last().copied().unwrap_or(0);
                    if let Some(wrap_width) = cx.wrap_width {
                        last_line_chars % wrap_width.get()
                    } else {
                        last_line_chars
                    }
                };
                self.col_idx.cmp(&trailing_chars)
            })
    }

    fn to_char_idx(
        &self,
        cx: SummarizeContext,
        agg_summary: &TextSummary,
        s: &str,
    ) -> Result<usize, OutOfBounds<Self>> {
        let wrap_width = cx.wrap_width.map(|w| w.get());

        let mut last_char_idx = agg_summary.chars;
        let mut next_pos = Self::from_summary(cx, agg_summary);
        let mut last_valid_position =
            Self::new(next_pos.line_idx, next_pos.col_idx.saturating_sub(1));

        for (i, ch) in s.chars().enumerate() {
            match next_pos.cmp(self) {
                std::cmp::Ordering::Less => {
                    last_char_idx += 1;
                    last_valid_position = next_pos;
                    next_pos = next_pos.add_char(wrap_width, ch);
                }
                std::cmp::Ordering::Equal => {
                    return Ok(agg_summary.chars + i);
                }
                std::cmp::Ordering::Greater => {
                    // we overshot the target; return the last valid target
                    break;
                }
            };
        }
        Err(OutOfBounds {
            attempted_position: *self,
            last_valid_position,
            last_char_idx: last_char_idx.saturating_sub(1),
        })
    }
}

fn summarize<'a, T: IntoIterator<Item = &'a TextSummary>>(
    cx: SummarizeContext,
    summaries: T,
) -> TextSummary {
    summaries
        .into_iter()
        .fold(TextSummary::default(), |agg, s| agg.add(cx, s))
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TextSummary {
    chars: usize,
    bytes: usize,
    lines: LineSummary,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct LineSummary {
    /// Number of complete lines (soft-wrapped or hard)
    num_complete: u32,
    /// Number of chars in the longest hard wrapped line
    longest_line_chars: u32,
    /// Number of chars in the first and last hard wrapped line, respectively
    line_chars: ArrayVec<u32, 2>,
    /// Whether or not the final char of the text was a newline
    ends_with_newline: bool,
}
impl LineSummary {
    fn from_str(cx: SummarizeContext, s: &str) -> LineSummary {
        let mut lsum = LineSummary::default();
        for mut hard_line in s.split_inclusive('\n') {
            lsum.ends_with_newline = false;
            if let Some('\n') = hard_line.chars().last() {
                lsum.ends_with_newline = true;
                hard_line = &hard_line[..hard_line.len() - 1];
            }
            let line_chars = hard_line.chars().count() as u32;

            lsum.longest_line_chars = std::cmp::max(lsum.longest_line_chars, line_chars);
            lsum.push_line_chars(line_chars);
            lsum.num_complete += cx
                .wrap_width
                .map(|wrap_width| {
                    if lsum.ends_with_newline {
                        line_chars.div_ceil(wrap_width.get()).max(1)
                    } else {
                        line_chars / wrap_width.get()
                    }
                })
                .or_else(|| lsum.ends_with_newline.then_some(1))
                .unwrap_or_default();
        }
        lsum
    }

    fn push_line_chars(&mut self, line_chars: u32) {
        let cap = self.line_chars.capacity();
        if let Some(last_slot) = self.line_chars.get_mut(cap - 1) {
            *last_slot = line_chars;
        } else {
            self.line_chars.push(line_chars);
        }
    }

    fn add(&mut self, cx: SummarizeContext, rhs: &Self) {
        if rhs.line_chars.is_empty() {
            // rhs is empty
            return;
        }
        let Some(last_line) = self.line_chars.last_mut() else {
            // lhs is empty
            *self = rhs.clone();
            return;
        };

        let mut rhs_line_chars = rhs.line_chars.iter();
        if !self.ends_with_newline {
            // fuse lhs last line with rhs first line
            let Some(first_line) = rhs_line_chars.next() else {
                // rhs is emtpy
                return;
            };
            if let Some(wrap_width) = cx.wrap_width {
                let wrap_width = wrap_width.get();
                let lhs_partial = *last_line % wrap_width;
                let rhs_partial = first_line % wrap_width;
                let joined = lhs_partial + rhs_partial;
                if joined >= wrap_width {
                    self.num_complete += 1;
                }
            }
            *last_line += first_line;
            self.longest_line_chars = self.longest_line_chars.max(*last_line);
        }

        for lc in rhs_line_chars.cloned() {
            self.push_line_chars(lc);
        }

        self.ends_with_newline = rhs.ends_with_newline;
        self.longest_line_chars = self.longest_line_chars.max(rhs.longest_line_chars);
        self.num_complete += rhs.num_complete;
    }
}

impl TextSummary {
    fn from_str(cx: SummarizeContext, value: &str) -> TextSummary {
        Self {
            chars: value.chars().count(),
            bytes: value.len(),
            lines: LineSummary::from_str(cx, value),
        }
    }

    fn from_segment(cx: SummarizeContext, value: &GridString) -> TextSummary {
        Self {
            chars: value.len_chars(),
            bytes: value.len_bytes(),
            lines: LineSummary::from_str(cx, value.as_str()),
        }
    }

    fn add(&self, cx: SummarizeContext, rhs: &TextSummary) -> Self {
        let mut this = self.clone();
        this.chars += rhs.chars;
        this.bytes += rhs.bytes;
        this.lines.add(cx, &rhs.lines);
        this
    }
}

impl<'a> From<&'a GridString> for TextSummary {
    fn from(value: &'a GridString) -> Self {
        TextSummary::from_segment(SummarizeContext::default(), value)
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal_emulator::SgrState;

    use self::iter::Cursor;

    use super::*;
    const LARGE_TEXT: &str = include_str!("tree.rs");

    #[test]
    fn test_text_summary_1() {
        let summary_1 = TextSummary::from(&GridString::from_str("abcd").unwrap());
        assert_eq!(summary_1.lines.line_chars.last().copied().unwrap(), 4);

        let summary_2 = TextSummary::from(&GridString::from_str("\nabcd").unwrap());
        assert_eq!(summary_2.lines.line_chars.last().copied().unwrap(), 4);

        let summary_3 = TextSummary::from(&GridString::from_str("a\nb\ncd").unwrap());
        assert_eq!(summary_3.lines.line_chars.last().copied().unwrap(), 2);

        let summary_4 = TextSummary::from(&GridString::from_str("abcd\n").unwrap());
        assert_eq!(summary_4.lines.line_chars.last().copied().unwrap(), 4);

        let cx = SummarizeContext { wrap_width: None };
        assert_eq!(
            [summary_1, summary_2, summary_3, summary_4]
                .into_iter()
                .reduce(|a, b| { a.add(cx, &b) })
                .map(|sum| sum.lines.line_chars.last().copied().unwrap()),
            Some(6)
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
        let input = &LARGE_TEXT[..128];
        tree.push_str(input, SgrState::default());
        assert_eq!(tree.to_string().as_str(), input);
        assert_invariants(&tree);
    }

    #[track_caller]
    fn assert_invariants(tree: &Tree) {
        // all levels other than the root should have at least B children
        for (idx, node) in tree.iter_nodes().enumerate() {
            assert!(
                idx == 0 || node.is_leaf() || node.child_summaries().len() >= B,
                "idx: {idx}, Node: {node:?}\n\n{tree:?}"
            );
        }
    }

    #[test]
    fn test_large_input_2() {
        let mut tree = Tree::new();
        let input = LARGE_TEXT;
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
        Compactable::compact_two(SummarizeContext::default(), &mut gs1, &mut gs2);

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
            tree.resolve_dimension(SeekSoftWrapPosition::new(42, 3))
                .unwrap(),
            42 * line_bytes_len + 3
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
        let target_line = line!() - 1; // 1 based
        cursor.seek_to_char(SeekSoftWrapPosition::new(target_line, 0))?;
        let mut output = String::new();
        for (text, _sgr) in cursor.iter_until(SeekSoftWrapPosition::new(target_line + 1, 0)) {
            output.push_str(text);
        }
        assert_eq!(
            output.as_str(),
            "        let target_line = line!() - 1; // 1 based\n"
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
                format!(" <{:02}>\n", i)
            };
            tree.push_str(line.as_str(), SgrState::default());
        }

        let wrap_width: NonZeroU32 = 8.try_into().unwrap();
        tree.rewrap(wrap_width);

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
            tree.resolve_dimension(SeekSoftWrapPosition::new(2, 1))
                .unwrap(),
            17
        );

        // invalid position
        assert_eq!(
            tree.resolve_dimension(SeekSoftWrapPosition::new(2, 4))
                .map_err(|err| err.last_valid_position),
            Err(SeekSoftWrapPosition::new(2, 3)),
        );
    }

    #[test]
    fn test_dim_soft_wrap_2() {
        let summaries = vec![
            TextSummary {
                chars: 24,
                bytes: 24,
                lines: LineSummary {
                    num_complete: 1,
                    longest_line_chars: 23,
                    line_chars: [0, 23].into(),
                    ends_with_newline: false,
                },
            },
            TextSummary {
                chars: 22,
                bytes: 24,
                lines: LineSummary {
                    num_complete: 2,
                    longest_line_chars: 12,
                    line_chars: [12, 4].into(),
                    ends_with_newline: false,
                },
            },
            TextSummary {
                chars: 24,
                bytes: 24,
                lines: LineSummary {
                    num_complete: 0,
                    longest_line_chars: 24,
                    line_chars: ArrayVec::from_iter([24]),
                    ends_with_newline: false,
                },
            },
            TextSummary {
                chars: 24,
                bytes: 24,
                lines: LineSummary {
                    num_complete: 1,
                    longest_line_chars: 22,
                    line_chars: [1, 22].into(),
                    ends_with_newline: false,
                },
            },
            TextSummary {
                chars: 13,
                bytes: 13,
                lines: LineSummary {
                    num_complete: 2,
                    longest_line_chars: 8,
                    line_chars: [8, 2].into(),
                    ends_with_newline: false,
                },
            },
            TextSummary {
                chars: 36,
                bytes: 38,
                lines: LineSummary {
                    num_complete: 1,
                    longest_line_chars: 33,
                    line_chars: ArrayVec::from_iter([33, 2]),
                    ends_with_newline: false,
                },
            },
        ];

        let cx = SummarizeContext {
            wrap_width: Some(35.try_into().unwrap()),
        };
        let target = SeekSoftWrapPosition::new(6, 2);

        let mut running_sum = TextSummary::default();
        for s in &summaries {
            let next_sum = running_sum.add(cx, s);
            if target.cmp_summary(cx, &next_sum) == std::cmp::Ordering::Less {
                break;
            }
            running_sum = next_sum;
        }

        assert_eq!(running_sum.lines.num_complete, 5);
        assert_eq!(running_sum.lines.line_chars.last(), Some(&22));

        let input = "abcdefghijklmnopqrstuvwxyz";
        let partial_line_chars = 35 - 22;
        assert_eq!(
            target.to_char_idx(cx, &running_sum, input).unwrap(),
            running_sum.chars + partial_line_chars + target.col_idx as usize
        );
    }

    #[test]
    fn test_soft_wrap_iter_1() {
        let mut tree = Tree::from_str("abcdef");
        tree.rewrap(2.try_into().unwrap());
        let mut iter = tree.iter_soft_wrapped_lines(..).unwrap();
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("ab"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("cd"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("ef"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), None);
    }

    #[test]
    fn test_soft_wrap_iter_2() {
        let mut tree = Tree::from_str("a\nb\nc\nd\ne\nf");
        tree.rewrap(2.try_into().unwrap());
        let mut iter = tree.iter_soft_wrapped_lines(..).unwrap();
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("a\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("b\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("c\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("d\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("e\n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("f"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), None);
    }

    #[test]
    fn test_soft_wrap_iter_3() {
        let mut tree = Tree::from_str("\ncwd\n> \n\ncwd\n> ");
        tree.rewrap(100.try_into().unwrap());
        let mut iter = tree.iter_soft_wrapped_lines(..).unwrap();
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("\n"));
        assert_eq!(
            iter.next().map(|slice| slice.text).as_deref(),
            Some("cwd\n")
        );
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("> \n"));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("\n"));
        assert_eq!(
            iter.next().map(|slice| slice.text).as_deref(),
            Some("cwd\n")
        );
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), Some("> "));
        assert_eq!(iter.next().map(|slice| slice.text).as_deref(), None);
    }

    #[test]
    fn test_summary_2() {
        let s1 = TextSummary::from(&GridString::from_str("abcd\n").unwrap());
        let zero = TextSummary::default();
        let cx = SummarizeContext { wrap_width: None };
        let sum = s1.add(cx, &zero);
        assert_eq!(sum, s1);
    }

    #[test]
    fn test_tree_2() {
        let mut tree = Tree::from_str("\n");
        tree.rewrap(80.try_into().unwrap());
        assert_eq!(tree.len_lines(), 1);

        let mut tree = Tree::new();
        tree.rewrap(80.try_into().unwrap());
        tree.push_str("\n", SgrState::default());
        assert_eq!(tree.len_lines(), 1);
        assert_eq!(tree.root.node_summary().lines.num_complete, 1);
    }

    #[test]
    fn test_soft_wrap_3() {
        let mut tree = Tree::from_str("Line 001\nLine 002\nLine 003\n");
        //                                              [---------)
        tree.remove_range(SeekSoftWrapPosition::new(1, 7)..SeekSoftWrapPosition::new(2, 7));
        assert_eq!(tree.to_string().as_str(), "Line 001\nLine 003\n");
        tree.remove_range(SeekSoftWrapPosition::new(1, 0)..);
        assert_eq!(tree.to_string().as_str(), "Line 001\n");
    }

    #[test]
    fn test_from_summary() {
        let tree = Tree::from_str("\n012345678\n9abcde");
        assert_eq!(
            tree.max_bound::<SeekSoftWrapPosition>(),
            SeekSoftWrapPosition::new(2, 6)
        );

        let tree = Tree::from_str("012345678\n9abcde\n");
        assert_eq!(
            tree.max_bound::<SeekSoftWrapPosition>(),
            SeekSoftWrapPosition::new(2, 0)
        );

        let mut tree = Tree::from_str("012345678\n9abcde\n");
        tree.rewrap(5.try_into().unwrap());
        // 01234
        // 5678\n
        // 9abcd
        // e\n
        assert_eq!(
            tree.max_bound::<SeekSoftWrapPosition>(),
            SeekSoftWrapPosition::new(4, 0)
        );
    }

    #[test]
    fn test_max_bound_insert_1() {
        let mut tree = Tree::from_str("hello\n\n");
        let max_pos: SeekSoftWrapPosition = tree.max_bound();
        assert_eq!(max_pos, SeekSoftWrapPosition::new(2, 0));

        let err = tree
            .resolve_dimension(SeekSoftWrapPosition::new(2, 1))
            .unwrap_err();
        tree.truncate(SeekCharIdx(err.last_char_idx));
        tree.insert_str(
            SeekCharIdx(err.last_char_idx - 1),
            "world",
            SgrState::default(),
        );
        assert_eq!(tree.to_string().as_str(), "hello\nworld\n");
    }

    #[test]
    fn test_get_char_1() {
        let mut tree = Tree::new();
        assert_eq!(tree.get_char(0), None);

        tree.push_str("hello\n", SgrState::default());
        assert_eq!(tree.get_char(4), Some('o'));
        assert_eq!(tree.get_char(5), Some('\n'));

        tree.push_str("world", SgrState::default());
        assert_eq!(tree.get_char(6), Some('w'));
        assert_eq!(tree.get_char(10), Some('d'));
        assert_eq!(tree.get_char(11), None);
    }

    #[test]
    fn test_append_tree_1() {
        // merging two leaf node only trees
        let mut t1 = Tree::from_str("abcd");
        let t2 = Tree::from_str("efgh");
        t1.append_tree(t2);
        assert_eq!(t1.to_string().as_str(), "abcdefgh");
        assert_invariants(&t1);
    }

    #[test]
    fn test_append_tree_2() {
        // merging equal height, non-leaf trees
        let mut t1 = Tree::from_str("abcd".repeat(20).as_str());
        let t2 = Tree::from_str("efgh".repeat(20).as_str());
        t1.append_tree(t2);

        let mut expected = "abcd".repeat(20);
        expected.extend(std::iter::repeat("efgh").take(20));
        assert_eq!(t1.to_string().as_str(), expected);
        assert_invariants(&t1);
    }

    #[test]
    fn test_append_tree_3() {
        // merging bigger tree with smaller tree
        let mut t1 = Tree::from_str("abcd".repeat(100).as_str());
        let t2 = Tree::from_str("efgh".repeat(10).as_str());
        t1.append_tree(t2);

        let mut expected = "abcd".repeat(100);
        expected.extend(std::iter::repeat("efgh").take(10));
        assert_eq!(t1.to_string().as_str(), expected);
        assert_invariants(&t1);
    }

    #[test]
    fn test_append_tree_4() {
        // merging smaller tree with bigger tree
        let mut t1 = Tree::from_str("abcd".repeat(10).as_str());
        let t2 = Tree::from_str("efgh".repeat(100).as_str());
        t1.append_tree(t2);

        let mut expected = "abcd".repeat(10);
        expected.extend(std::iter::repeat("efgh").take(100));
        assert_eq!(t1.to_string().as_str(), expected);
        assert_invariants(&t1);
    }
}
