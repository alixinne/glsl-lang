use std::fmt;

use crate::position::{LexerPosition, NodeSpan};

pub trait NodeContent: fmt::Debug + Clone + PartialEq + Sized {
    /// Convert the contents into a node
    fn into_node<T>(self) -> Node<T>
    where
        T: From<Self> + NodeContent,
    {
        Node::new(self.into(), None)
    }

    /// Add span information to a syntax node
    fn spanned(self, start: LexerPosition, end: LexerPosition) -> Node<Self> {
        assert_eq!(start.source_id, end.source_id);

        Node {
            content: self,
            span: Some(NodeSpan {
                source_id: start.source_id,
                start: start.offset,
                end: end.offset,
            }),
        }
    }
}

/// A syntax node with span information
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T: NodeContent> {
    pub content: T,
    pub span: Option<NodeSpan>,
}

impl<T: NodeContent> Node<T> {
    /// Create a new syntax node with span information
    pub fn new(content: T, span: Option<NodeSpan>) -> Self {
        Self { content, span }
    }

    /// Return the wrapped syntax node, discarding the span information
    pub fn into_inner(self) -> T {
        self.content
    }

    /// Map this content of this node into a new node
    pub fn map<U: NodeContent>(self, f: impl FnOnce(T) -> U) -> Node<U> {
        Node {
            content: f(self.content),
            span: self.span,
        }
    }

    /// Map this content of this node into a new node with the same span
    pub fn map_spanned<U: NodeContent>(self, f: impl FnOnce(Self) -> U) -> Node<U> {
        let span = self.span;

        Node {
            content: f(self),
            span,
        }
    }
}

impl<T: NodeContent> std::ops::Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl<T: NodeContent> std::ops::DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.content
    }
}

// Trivial copy for the node if the wrapped content are Copy
impl<T: NodeContent + Copy> Copy for Node<T> {}

// Display implementation for wrapped node
impl<T: NodeContent + fmt::Display> fmt::Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as fmt::Display>::fmt(&self.content, f)
    }
}

impl<T: NodeContent> From<T> for Node<T> {
    fn from(inner: T) -> Self {
        Node::new(inner, None)
    }
}

impl NodeContent for &'static str {}

/// Trait for comparing the content of syntax nodes
pub trait NodeContentEq {
    fn content_eq(&self, other: &Self) -> bool;
}

impl<T: NodeContent + NodeContentEq> NodeContentEq for Node<T> {
    fn content_eq(&self, other: &Self) -> bool {
        self.content.content_eq(&other.content)
    }
}

impl<T: NodeContentEq, U: PartialEq> NodeContentEq for Result<T, U> {
    fn content_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ok(a), Ok(b)) => a.content_eq(b),
            (Err(a), Err(b)) => a.eq(b),
            _ => false,
        }
    }
}

impl<T: NodeContentEq, U: PartialEq> NodeContentEq for Result<(&str, T), U> {
    fn content_eq(&self, other: &Result<(&str, T), U>) -> bool {
        match (self, other) {
            (Ok((a1, a2)), Ok((b1, b2))) => a2.content_eq(b2) && a1 == b1,
            (Err(a), Err(b)) => a.eq(b),
            _ => false,
        }
    }
}

impl<T: NodeContentEq> NodeContentEq for Option<T> {
    fn content_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.content_eq(b),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T: NodeContentEq> NodeContentEq for Vec<T> {
    fn content_eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (a, b) in self.iter().zip(other.iter()) {
            if !a.content_eq(b) {
                return false;
            }
        }

        true
    }
}

impl<T: NodeContentEq> NodeContentEq for Box<T> {
    fn content_eq(&self, other: &Self) -> bool {
        (**self).content_eq(&**other)
    }
}

macro_rules! impl_node_content_eq {
    ($t:ty) => {
        impl NodeContentEq for $t {
            fn content_eq(&self, other: &Self) -> bool {
                *self == *other
            }
        }
    };
}

impl_node_content_eq!(());
impl_node_content_eq!(bool);
impl_node_content_eq!(char);
impl_node_content_eq!(u16);
impl_node_content_eq!(i32);
impl_node_content_eq!(u32);
impl_node_content_eq!(f32);
impl_node_content_eq!(f64);
impl_node_content_eq!(usize);
impl_node_content_eq!(&str);
impl_node_content_eq!(String);
impl_node_content_eq!(std::borrow::Cow<'_, str>);

#[macro_export]
/// Replacement for assert_eq but using [`NodeContentEq`] instead of [`PartialEq`]
macro_rules! assert_ceq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !::lang_util::node::NodeContentEq::content_eq(left_val, right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion failed: `left.content_eq(right)`
  left: `{:?}`,
 right: `{:?}`"#, &*left_val, &*right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        assert_ceq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !::lang_util::node::NodeContentEq::content_eq(left_val, right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion failed: `left.content_eq(right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, &*left_val, &*right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}
