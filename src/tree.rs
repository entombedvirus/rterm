#![allow(dead_code)]

use std::sync::Arc;

use arrayvec::ArrayVec;

use crate::{grid_string::GridString, terminal_emulator::SgrState};

const B: usize = 3;
pub const MAX_CHILDREN: usize = B * 2;
pub const MIN_CHILDREN: usize = B;

#[derive(Debug)]
pub struct Tree {
    root: Arc<Node>,
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

        tree.push_str("hello world!\n", SgrState::default());
        assert_eq!(tree.len_bytes(), 13);
        assert_eq!(tree.len_chars(), 13);
        assert_eq!(tree.len_lines(), 1);

        // tree.replace_str(6, "back!!", bold);
        // let line0 = tree.get_line(0).to_string();
        // assert_eq!(line0, "hello back!!\n");
    }
}
