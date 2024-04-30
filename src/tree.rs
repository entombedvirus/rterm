#![allow(dead_code)]

use std::ops::{Not as _, Range, RangeBounds};
use std::sync::Arc;

use anyhow::Context;
use arrayvec::{ArrayString, ArrayVec};

use crate::{
    grid_string::{split_str_at_utf8_boundary, GridString},
    terminal_emulator::SgrState,
};

const B: usize = 3;
pub const MAX_CHILDREN: usize = B * 2;
pub const MIN_CHILDREN: usize = B;

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

    pub fn insert_str<D: SeekTarget>(&mut self, target: D, mut new_text: &str, sgr: SgrState) {
        let mut char_idx = self.resolve_dimension(target).expect("invalid target");
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

    pub fn iter_lines<R: RangeBounds<D>, D: SeekTarget>(
        &self,
        target_range: R,
    ) -> anyhow::Result<iter::LineIter> {
        let target_char_range = self.resolve_dimensions(target_range)?;
        Ok(iter::LineIter::new(self, target_char_range))
    }

    fn find_string_segment_mut<'a, D: SeekTarget>(
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

    fn from_str(input: &str) -> Self {
        let mut tree = Self::new();
        tree.push_str(input, SgrState::default());
        tree
    }

    fn resolve_dimension<D: SeekTarget>(&self, seek_target: D) -> Option<usize> {
        self.root.dimension_to_char_idx(seek_target)
    }

    /// resolves a range of Dimensions to corresponding char range.
    fn resolve_dimensions<R: RangeBounds<D>, D: SeekTarget>(
        &self,
        target_range: R,
    ) -> anyhow::Result<Range<usize>> {
        let start = match target_range.start_bound().map(|d| {
            self.resolve_dimension(d.clone())
                .context("invalid start bound: could not resolve to char_idx")
        }) {
            std::ops::Bound::Included(char_idx) => char_idx?,
            std::ops::Bound::Excluded(char_idx) => char_idx? + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match target_range.end_bound().map(|d| {
            self.resolve_dimension(d.clone())
                .context("invalid end bound: could not resolve to char_idx")
        }) {
            std::ops::Bound::Included(char_idx) => char_idx? + 1,
            std::ops::Bound::Excluded(char_idx) => char_idx?,
            std::ops::Bound::Unbounded => self.len_chars(),
        };
        Ok(start..end)
    }

    fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }
}

#[derive(Debug)]
pub struct TreeSlice {
    pub text: String,
    pub sgr: Vec<SgrState>,
}

mod iter {
    use std::{collections::VecDeque, ops::Range, sync::Arc};

    use crate::{grid_string::GridString, terminal_emulator::SgrState};

    use super::{Node, SeekCharIdx, SeekTarget, Tree, TreeSlice};

    #[derive(Debug)]
    pub struct LineIter<'a> {
        tree: &'a Tree,
        target_char_range: Range<usize>,
        cursor: Option<Cursor<'a>>,
    }
    impl<'a> LineIter<'a> {
        pub(crate) fn new(tree: &'a super::Tree, target_char_range: Range<usize>) -> LineIter {
            Self {
                tree,
                target_char_range,
                cursor: None,
            }
        }
    }

    impl Iterator for LineIter<'_> {
        type Item = TreeSlice;

        fn next(&mut self) -> Option<Self::Item> {
            if self.target_char_range.is_empty() || self.tree.is_empty() {
                return None;
            }

            let cursor = self.cursor.get_or_insert_with(|| {
                let mut c = Cursor::new(&self.tree);
                // TODO
                // c.seek_to_char(self.target_char_range.start);
                c
            });

            let mut text = String::new();
            let mut sgr = Vec::new();
            let mut text_char_count = 0;
            while let Some(prefix) = cursor.segment_str()?.split_inclusive('\n').next() {
                text.push_str(prefix);
                let prefix_chars = prefix.chars().count();
                text_char_count += prefix_chars;
                sgr.extend_from_slice(
                    cursor
                        .segment_sgr()
                        .map(|sgr_slice| &sgr_slice[..prefix_chars])
                        .unwrap_or(&[]),
                );
                if text.ends_with('\n') {
                    break;
                }
                cursor.advance_char_offset(prefix_chars);
            }
            if text.is_empty() {
                None
            } else {
                self.target_char_range.start += text_char_count;
                Some(TreeSlice { text, sgr })
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            todo!()
        }
    }

    impl ExactSizeIterator for LineIter<'_> {}

    #[derive(Debug)]
    struct StackEntry<'a> {
        running_sum: usize,
        node: &'a Node,
        child_idx: usize,
    }
    impl StackEntry<'_> {
        fn next_child(&mut self) {
            self.running_sum += self
                .node
                .child_summaries()
                .get(self.child_idx)
                .map(|summary| summary.chars)
                .expect("child_idx is not valid");
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

        pub fn seek_to_char<T: SeekTarget>(&mut self, seek_target: T) {
            let Some(target_char_idx) = self.tree.resolve_dimension(seek_target) else {
                return;
            };
            self.prune_stack(target_char_idx);

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
                        return;
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
            debug_assert!(self.stack.is_empty() || self.stack[0].node.is_leaf());
        }

        fn prune_stack(&mut self, target_char_idx: usize) {
            self.stack
                .retain(|entry| entry.running_sum <= target_char_idx);

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

        pub fn advance_char_offset(&mut self, delta: usize) {
            self.seek_to_char(SeekCharIdx(self.char_position + delta));
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

        pub fn iter_until<T: SeekTarget>(
            &'a mut self,
            seek_target: T,
        ) -> impl Iterator<Item = (&str, &[SgrState])> + 'a {
            let target_char_idx = self.tree.resolve_dimension(seek_target);
            std::iter::from_fn(move || {
                let target_char_idx = target_char_idx?;
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

                let mut char_iter = segment.as_str().char_indices();
                let start_byte_idx = if segment_offset == 0 {
                    0
                } else {
                    char_iter
                        .nth(segment_offset)
                        .map(|(byte_offset, _)| byte_offset)?
                };
                let end_byte_idx = if num_chars < segment_chars_remaining {
                    let (byte_offset, _) = char_iter.nth(num_chars)?;
                    byte_offset
                } else {
                    segment.len_bytes()
                };

                self.stack.push(StackEntry {
                    node,
                    child_idx,
                    running_sum,
                });
                self.advance_char_offset(num_chars);
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
    fn child_position<D: SeekTarget>(&self, seek_target: D) -> Option<(usize, D)> {
        let end = seek_target.zero() + self.node_summary();
        if seek_target > end {
            return None;
        }

        if seek_target == end {
            // seeking to the end
            let last_child_summary = self
                .child_summaries()
                .last()
                // .or_else(|| {
                //     eprintln!("node: {self:?}");
                //     None
                // })
                .expect("all nodes have at least one child");
            let rem_seek_target = seek_target.zero() + last_child_summary;
            let child_idx = self.child_summaries().len() - 1;
            return Some((child_idx, rem_seek_target));
        }

        let mut running_sum = seek_target.zero();
        for (child_idx, child_summary) in self.child_summaries().into_iter().enumerate() {
            let next_sum = running_sum + child_summary;
            if next_sum > seek_target {
                return Some((child_idx, seek_target - running_sum));
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
        seek_target: D,
        mut edit_op: impl FnMut(&mut GridString, usize) -> Option<GridString>,
    ) -> Option<Arc<Node>> {
        let (child_idx, seek_target) = self.child_position(seek_target)?;
        let split = match self {
            Node::Leaf { children, .. } => {
                let child = &mut children[child_idx];
                let char_idx = seek_target.to_char_idx(child.as_str())?;
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

    fn dimension_to_char_idx<D: SeekTarget>(&self, seek_target: D) -> Option<usize> {
        let mut running_sum = seek_target.zero();
        let mut agg_summary = TextSummary::default();
        for (child_idx, child_summary) in self.child_summaries().into_iter().enumerate() {
            {
                match self {
                    Node::Leaf { children, .. } => {
                        let foo = children[child_idx].as_str();
                        let x = 1;
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
                        let x = 1;
                    }
                }
            }
            let next_sum = running_sum + child_summary;
            if next_sum >= seek_target {
                let child_seek_target = seek_target - running_sum;
                return match self {
                    Node::Leaf { children, .. } => {
                        let child_str = children[child_idx].as_str();
                        let child_char_idx = child_seek_target.to_char_idx(child_str)?;
                        Some(agg_summary.chars + child_char_idx)
                    }
                    Node::Internal { children, .. } => {
                        let child_char_idx =
                            children[child_idx].dimension_to_char_idx(child_seek_target);
                        child_char_idx.map(|child_chars| agg_summary.chars + child_chars)
                    }
                };
            }
            running_sum = next_sum;
            agg_summary = agg_summary + &child_summary;
        }
        None
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
pub trait SeekTarget:
    Default
    + Clone
    + Copy
    + for<'a> std::ops::Add<&'a TextSummary, Output = Self>
    + std::ops::Sub<Self, Output = Self>
    + std::cmp::Ord
    + std::fmt::Debug
{
    fn to_char_idx(&self, segment: &str) -> Option<usize>;

    fn zero(&self) -> Self {
        Self::default()
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekCharIdx(pub usize);

impl SeekTarget for SeekCharIdx {
    fn to_char_idx(&self, s: &str) -> Option<usize> {
        Some(self.0)
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekLineIdx(pub usize);
impl SeekTarget for SeekLineIdx {
    fn to_char_idx(&self, s: &str) -> Option<usize> {
        let Self(line_idx) = *self;
        if line_idx == 0 {
            return Some(0);
        }
        let mut char_idx = 0;
        for (i, line) in s.split_inclusive('\n').enumerate() {
            char_idx += line.chars().count();
            if i + 1 == line_idx {
                return Some(char_idx);
            }
        }
        panic!("cannot find line with idx: {line_idx} in input: {s:?}")
    }
}

impl<'a> std::ops::Add<&'a TextSummary> for SeekLineIdx {
    type Output = Self;

    fn add(self, rhs: &'a TextSummary) -> Self {
        Self(self.0 + rhs.lines)
    }
}
impl std::ops::Add for SeekLineIdx {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl std::ops::Sub for SeekLineIdx {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekCursorPosition {
    pub line_idx: usize,
    pub trailing_char_idx: usize,
}
impl SeekTarget for SeekCursorPosition {
    fn to_char_idx(&self, s: &str) -> Option<usize> {
        let line_start_idx = SeekLineIdx(self.line_idx).to_char_idx(s)?;
        let n = line_start_idx + self.trailing_char_idx;
        if s.chars().nth(n).is_some() {
            Some(n)
        } else {
            None
        }
    }
}

impl<'a> std::ops::Add<&'a TextSummary> for SeekCursorPosition {
    type Output = Self;

    fn add(self, rhs: &'a TextSummary) -> Self::Output {
        Self {
            line_idx: self.line_idx + rhs.lines,
            trailing_char_idx: rhs.trailing_line_chars,
        }
    }
}
impl std::ops::Add for SeekCursorPosition {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            line_idx: self.line_idx + rhs.line_idx,
            trailing_char_idx: rhs.trailing_char_idx,
        }
    }
}
impl std::ops::Sub for SeekCursorPosition {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            line_idx: self.line_idx - rhs.line_idx,
            trailing_char_idx: self.trailing_char_idx,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeekSoftWrapPosition {
    pub wrap_width: usize,
    pub line_idx: usize,
    pub trailing_line_chars: usize,
}
impl SeekTarget for SeekSoftWrapPosition {
    fn zero(&self) -> Self {
        Self {
            wrap_width: self.wrap_width,
            ..Default::default()
        }
    }

    fn to_char_idx(&self, s: &str) -> Option<usize> {
        s.split('\n').next().and_then(|line| {
            let n = self.wrap_width * self.line_idx + self.trailing_line_chars;
            if line.chars().nth(n).is_some() {
                Some(n)
            } else {
                None
            }
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
impl std::ops::Add for SeekSoftWrapPosition {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        unimplemented!()
    }
}
impl std::ops::Sub for SeekSoftWrapPosition {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let line_idx = self.line_idx - rhs.line_idx;
        let trailing_line_chars = if line_idx == 0 {
            self.trailing_line_chars - rhs.trailing_line_chars
        } else {
            self.trailing_line_chars
        };
        Self {
            wrap_width: self.wrap_width,
            line_idx,
            trailing_line_chars,
        }
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
    fn test_dim_line_idx() {
        let tree = Tree::from_str("Line 1\nLine 2\n\u{0d30}\u{0d4b}\nLine 3\n");
        assert_eq!(tree.resolve_dimension(SeekLineIdx(0)), Some(0));
        assert_eq!(tree.resolve_dimension(SeekLineIdx(1)), Some(7));
        assert_eq!(tree.resolve_dimension(SeekLineIdx(2)), Some(14));
        assert_eq!(tree.resolve_dimension(SeekLineIdx(3)), Some(17));
        assert_eq!(tree.resolve_dimension(SeekLineIdx(4)), Some(24));
        assert_eq!(tree.resolve_dimension(SeekLineIdx(5)), None);
    }

    #[test]
    fn test_dim_line_char() {
        let mut tree = Tree::new();
        const N: usize = 100;
        for i in 0..N {
            tree.push_str(format!("Line {:03}\n", i).as_str(), SgrState::default());
        }
        let line_bytes_len = "Line 000\n".len();
        assert_eq!(tree.len_bytes(), N * line_bytes_len);

        assert_eq!(
            tree.resolve_dimension(SeekCursorPosition {
                line_idx: 42,
                trailing_char_idx: 6,
            }),
            Some(42 * line_bytes_len + 6)
        )
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
                wrap_width: 8,
                line_idx: 2,
                trailing_line_chars: 1,
            }),
            Some(17)
        );

        // invalid position
        assert_eq!(
            tree.resolve_dimension(SeekSoftWrapPosition {
                wrap_width: 8,
                line_idx: 2,
                trailing_line_chars: 3,
            }),
            None,
        );
    }

    #[test]
    fn test_cursor_1() {
        let tree = Tree::from_str("Line 1\nLine 2\nLine 3");
        let mut cursor = Cursor::new(&tree);
        cursor.seek_to_char(SeekCharIdx(19));
        assert_eq!(cursor.segment_str(), Some("3"));
        cursor.seek_to_char(SeekCharIdx(16));
        assert_eq!(cursor.segment_str(), Some("ne 3"));
        cursor.seek_to_char(SeekCharIdx(0));
        assert_eq!(cursor.segment_str(), Some("Line 1\nL"));
    }

    #[test]
    fn test_cursor_2() {
        let input = &LARGE_TEXT;
        let tree = Tree::from_str(input);
        let mut cursor = Cursor::new(&tree);
        let target_line = line!() as usize - 1; // 1 based
        cursor.seek_to_char(SeekLineIdx(target_line));
        let mut output = String::new();
        for (text, _sgr) in cursor.iter_until(SeekLineIdx(target_line + 1)) {
            output.push_str(text);
        }
        assert_eq!(
            output.as_str(),
            "        let target_line = line!() as usize - 1; // 1 based\n"
        );
    }
}
