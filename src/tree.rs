#![allow(dead_code)]

use std::sync::Arc;

use arrayvec::ArrayVec;

use crate::{
    grid_string::{GridString, StripedString},
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
        let root = Arc::new(Node::Leaf {
            node_summary: TextSummary::default(),
            children: ArrayVec::new(),
            child_summaries: ArrayVec::new(),
        });
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

    pub fn push_str(&mut self, new_text: &str, sgr: SgrState) {
        // TODO: if new_text is larger than what will fit in a leaf node, create a tree from it and
        // merge
        if let Some(node) = Arc::make_mut(&mut self.root).push_str(new_text, sgr) {
            self.root = Node::new_internal(self.root.height() + 1, [self.root.clone(), node]);
        }
    }

    pub fn replace_str(&mut self, char_idx: usize, new_text: &str, sgr: SgrState) {
        if let Some(node) = Arc::make_mut(&mut self.root).replace_str(char_idx, new_text, sgr) {
            self.root = Node::new_internal(self.root.height() + 1, [self.root.clone(), node]);
        }
    }

    fn iter_strings(&self) -> iter::StringIter {
        iter::StringIter::new(&self.root)
    }
}

mod iter {
    use crate::grid_string::GridString;

    use super::Node;

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
            let is_not_valid = |(node, index): &&mut (&Node, usize)| {
                !node.is_leaf() || *index >= node.child_summaries().len()
            };
            while let Some((node, index)) = self.stack.last_mut().filter(is_not_valid) {
                match node {
                    Node::Leaf { children, .. } => {
                        debug_assert!(*index >= children.len());
                        self.stack.pop();
                    }
                    Node::Internal { children, .. } => {
                        *index += 1;
                        let child = children[*index - 1].as_ref();
                        self.stack.push((child, 0));
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
#[derive(Debug, Clone)]
enum Node {
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
            Node::Internal { height, .. } => height + 1,
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

    fn push_str(&mut self, new_text: &str, sgr: SgrState) -> Option<Arc<Self>> {
        match self {
            Node::Leaf { children, .. } => {
                let split_node = push_str_leaf(children, new_text, sgr);
                self.recompute_summaries();
                split_node
            }
            Node::Internal {
                height, children, ..
            } => {
                let split_node = push_str_internal(*height, children, new_text, sgr);
                self.recompute_summaries();
                split_node
            }
        }
    }

    fn replace_str(&mut self, char_idx: usize, new_text: &str, sgr: SgrState) -> Option<Arc<Node>> {
        if char_idx > self.len_chars() {
            panic!(
                "replace_str with out of bounds char_idx: {char_idx} / {}",
                self.len_chars()
            );
        }
        if char_idx == self.len_chars() {
            return self.push_str(new_text, sgr);
        }

        let (child_idx, DimensionCharIdx(rem_char_idx)) =
            self.child_position(DimensionCharIdx(char_idx));

        match *self {
            Node::Leaf {
                ref mut children, ..
            } => {
                let writer = StripedString::from_iter(&mut children[child_idx..]);
                let (_, rest) = writer
                    .replace_str(rem_char_idx, new_text, sgr)
                    .expect("bounds checked already");
                self.recompute_summaries();
                if !rest.is_empty() {
                    return self.push_str(rest, sgr);
                }
                None
            }
            Node::Internal {
                ref mut children, ..
            } => {
                let child = Arc::make_mut(&mut children[child_idx]);
                let split = child.replace_str(rem_char_idx, new_text, sgr);
                self.recompute_summaries();
                split
            }
        }
    }

    fn new_leaf<I: IntoIterator<Item = GridString>>(children: I) -> Arc<Node> {
        let children: ArrayVec<GridString, MAX_CHILDREN> = children.into_iter().collect();
        let mut n = Node::Leaf {
            children,
            node_summary: TextSummary::default(),
            child_summaries: ArrayVec::new(),
        };
        n.recompute_summaries();
        Arc::new(n)
    }

    fn new_internal<I: IntoIterator<Item = Arc<Node>>>(height: u8, children: I) -> Arc<Node> {
        let children: ArrayVec<Arc<Node>, MAX_CHILDREN> = children.into_iter().collect();
        let mut n = Self::Internal {
            height,
            children,
            node_summary: TextSummary::default(),
            child_summaries: ArrayVec::new(),
        };
        n.recompute_summaries();
        Arc::new(n)
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

    // fn child_nodes(&mut self) -> &mut ArrayVec<Arc<Node>, MAX_CHILDREN> {
    //     match self {
    //         Node::Leaf { .. } => panic!("child_node_mut cannot be called on leaf nodes"),
    //         Node::Internal { children, .. } => children,
    //     }
    // }

    fn child_strings(&self) -> &ArrayVec<GridString, MAX_CHILDREN> {
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
}

/// Any type that can be used to seek to a specific leaf node via searching `TextSummary`-s.
trait Dimension:
    for<'a> From<&'a TextSummary>
    + Default
    + Clone
    + Copy
    + std::ops::Add<Output = Self>
    + std::ops::Sub<Output = Self>
    + std::cmp::Ord
    + std::fmt::Debug
{
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
struct DimensionCharIdx(usize);
impl Dimension for DimensionCharIdx {}

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

fn push_str_internal(
    height: u8,
    children: &mut ArrayVec<Arc<Node>, MAX_CHILDREN>,
    new_text: &str,
    sgr: SgrState,
) -> Option<Arc<Node>> {
    let last_child = Arc::make_mut(
        children
            .last_mut()
            .expect("internal nodes have non-zero children"),
    );
    let split_node = last_child.push_str(new_text, sgr)?;
    match children.try_push(split_node) {
        Ok(_) => None,
        Err(err) => {
            let split_node = err.element();
            let right_children = children.drain(B..).chain(std::iter::once(split_node));
            let new_node = Node::new_internal(height, right_children);
            Some(new_node)
        }
    }
}

fn push_str_leaf(
    children: &mut ArrayVec<GridString, MAX_CHILDREN>,
    new_text: &str,
    sgr: SgrState,
) -> Option<Arc<Node>> {
    if new_text.is_empty() {
        return None;
    }
    if children.is_empty() {
        children.push(GridString::default());
    }
    let last_child = children
        .last_mut()
        .expect("just made sure children is not empty");
    let (_, mut to_write) = last_child.push_str(new_text, sgr);

    while !to_write.is_empty() {
        let mut new_child = GridString::default();
        let (_, rest) = new_child.push_str(to_write, sgr);
        to_write = rest;
        if let Err(err) = children.try_push(new_child) {
            // need to split
            let mut right_children: ArrayVec<GridString, MAX_CHILDREN> =
                children.drain(B..).collect();
            right_children.push(err.element());
            let split_node = match push_str_leaf(&mut right_children, to_write, sgr) {
                None => Node::new_leaf(right_children),
                Some(inner_split) => Node::new_internal(
                    inner_split.height() + 1,
                    [Node::new_leaf(right_children), inner_split],
                ),
            };
            return Some(split_node);
        };
    }
    None
}

fn summarize<'a, T: IntoIterator<Item = &'a TextSummary>>(summaries: T) -> TextSummary {
    summaries
        .into_iter()
        .fold(TextSummary::default(), |agg, s| agg + s)
}

#[derive(Debug, Default, Clone, Copy)]
struct TextSummary {
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
}
