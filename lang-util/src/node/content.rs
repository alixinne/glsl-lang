use std::fmt;

use crate::position::{LexerPosition, NodeSpan};

/// Trait for AST node contents.
///
/// All nodes which will be stored in a [Node] need to implement this.
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
#[derive(Debug, Clone)]
pub struct Node<T: NodeContent> {
    /// Contents of this syntax node
    pub content: T,
    /// Span in the input this node was parsed from
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

impl<U, T: NodeContent + AsRef<U>> AsRef<U> for Node<T> {
    fn as_ref(&self) -> &U {
        self.content.as_ref()
    }
}

impl<T: NodeContent + PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.content.eq(&other.content)
    }
}

impl<T: NodeContent + Eq> Eq for Node<T> {}

impl<T: NodeContent + PartialOrd> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.content.partial_cmp(&other.content)
    }
}

impl<T: NodeContent + Ord> Ord for Node<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.content.cmp(&other.content)
    }
}

impl<T: NodeContent + std::hash::Hash> std::hash::Hash for Node<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.content.hash(state)
    }
}

#[cfg(feature = "serde")]
impl<T: NodeContent + serde::ser::Serialize> serde::ser::Serialize for Node<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.content.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, T: NodeContent + serde::de::Deserialize<'de>> serde::de::Deserialize<'de> for Node<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self {
            content: T::deserialize(deserializer)?,
            span: None,
        })
    }
}
