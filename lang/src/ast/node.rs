use std::fmt;

/// Span information for a node, constructed from a nom_locate::LocatedSpan
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, Hash)]
pub struct NodeSpan {
    /// The index of this span into the list of parsed units. This is used to
    /// identify which source string this span refers to when combining multiple ASTs
    pub source_id: usize,

    /// Start of the node in the input slice
    pub start: usize,

    /// End of the node in the input slice
    pub end: usize,
}

impl NodeSpan {
    /// Return a 0-length span located at the start of the given source
    ///
    /// This may be used in span range queries.
    pub fn new_start(source_id: usize) -> Self {
        Self {
            source_id,
            start: 0,
            end: 0,
        }
    }

    /// Return a 0-length span located at the end of the given source (as indicated by the offset)
    ///
    /// This may be used in span range queries.
    pub fn new_end(source_id: usize, length: usize) -> Self {
        Self {
            source_id,
            start: length,
            end: length,
        }
    }

    /// Return a 0-length span located at the end point of this span.
    ///
    /// This may be used in span range queries. Note that the line and column information will not be
    /// accurate.
    pub fn to_end_location(&self) -> Self {
        Self {
            source_id: self.source_id,
            start: self.end,
            end: self.end,
        }
    }

    /// Return the length of this span
    pub fn length(&self) -> usize {
        self.end - self.start
    }
}

impl std::cmp::PartialOrd for NodeSpan {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.source_id.cmp(&other.source_id).then_with(|| {
            self.start
                .cmp(&other.start)
                .then_with(|| other.length().cmp(&self.length()))
        }))
    }
}

pub trait NodeContents: fmt::Debug + Clone + PartialEq + Sized {
    /// Add span information to a syntax node
    fn spanned(self, start: (usize, usize), end: (usize, usize)) -> Node<Self> {
        assert_eq!(start.0, end.0);

        Node {
            contents: self,
            span: Some(NodeSpan {
                source_id: start.0,
                start: start.1,
                end: end.1,
            }),
        }
    }
}

/// A syntax node with span information
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T: NodeContents> {
    pub contents: T,
    pub span: Option<NodeSpan>,
}

impl<T: NodeContents> Node<T> {
    /// Create a new syntax node with span information
    pub fn new(contents: T, span: Option<NodeSpan>) -> Self {
        Self { contents, span }
    }

    /// Return the wrapped syntax node, discarding the span information
    pub fn into_inner(self) -> T {
        self.contents
    }

    /// Map this contents of this node into a new node
    pub fn map<U: NodeContents>(self, f: impl FnOnce(T) -> U) -> Node<U> {
        Node {
            contents: f(self.contents),
            span: self.span,
        }
    }
}

impl<T: NodeContents> std::ops::Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<T: NodeContents> std::ops::DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.contents
    }
}

// Trivial copy for the node if the wrapped contents are Copy
impl<T: NodeContents + Copy> Copy for Node<T> {}

// Display implementation for wrapped node
impl<T: NodeContents + fmt::Display> fmt::Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as fmt::Display>::fmt(&self.contents, f)
    }
}

/// Trait for comparing the contents of syntax nodes
pub trait NodeContentsEq {
    fn contents_eq(&self, other: &Self) -> bool;
}

impl<T: NodeContents + NodeContentsEq> NodeContentsEq for Node<T> {
    fn contents_eq(&self, other: &Self) -> bool {
        self.contents.contents_eq(&other.contents)
    }
}

impl<T: NodeContentsEq, U: PartialEq> NodeContentsEq for Result<T, U> {
    fn contents_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ok(a), Ok(b)) => a.contents_eq(b),
            (Err(a), Err(b)) => a.eq(b),
            _ => false,
        }
    }
}

impl<T: NodeContentsEq, U: PartialEq> NodeContentsEq for Result<(&str, T), U> {
    fn contents_eq(&self, other: &Result<(&str, T), U>) -> bool {
        match (self, other) {
            (Ok((a1, a2)), Ok((b1, b2))) => a2.contents_eq(b2) && a1 == b1,
            (Err(a), Err(b)) => a.eq(b),
            _ => false,
        }
    }
}

impl<T: NodeContentsEq> NodeContentsEq for Option<T> {
    fn contents_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.contents_eq(b),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T: NodeContentsEq> NodeContentsEq for Vec<T> {
    fn contents_eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (a, b) in self.iter().zip(other.iter()) {
            if !a.contents_eq(b) {
                return false;
            }
        }

        true
    }
}

impl<T: NodeContentsEq> NodeContentsEq for Box<T> {
    fn contents_eq(&self, other: &Self) -> bool {
        (**self).contents_eq(&**other)
    }
}

macro_rules! impl_node_contents_eq {
    ($t:ty) => {
        impl NodeContentsEq for $t {
            fn contents_eq(&self, other: &Self) -> bool {
                *self == *other
            }
        }
    };
}

impl_node_contents_eq!(());
impl_node_contents_eq!(bool);
impl_node_contents_eq!(char);
impl_node_contents_eq!(u16);
impl_node_contents_eq!(i32);
impl_node_contents_eq!(u32);
impl_node_contents_eq!(f32);
impl_node_contents_eq!(f64);
impl_node_contents_eq!(usize);
impl_node_contents_eq!(&str);
impl_node_contents_eq!(String);
impl_node_contents_eq!(std::borrow::Cow<'_, str>);

#[macro_export]
/// Replacement for assert_eq but using [`NodeContentsEq`] instead of [`PartialEq`]
macro_rules! assert_ceq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !$crate::ast::NodeContentsEq::contents_eq(left_val, right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion failed: `left.contents_eq(right)`
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
                if !$crate::ast::NodeContentsEq::contents_eq(left_val, right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion failed: `left.contents_eq(right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, &*left_val, &*right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}
