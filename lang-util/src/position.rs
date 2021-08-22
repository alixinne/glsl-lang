//! Input position representation types

use std::{
    convert::{TryFrom, TryInto},
    fmt::Display,
};

use text_size::{TextRange, TextSize};

use crate::{located::PointOrRange, FileId};

/// A position in the lexer's input
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LexerPosition {
    /// Source id
    pub source_id: FileId,
    /// Raw byte offset
    pub offset: TextSize,
}

impl LexerPosition {
    /// Create a new [LexerPosition]
    ///
    /// # Parameters
    ///
    /// * `source_id`: source id
    /// * `offset`: byte offset in the input
    pub fn new(source_id: FileId, offset: TextSize) -> Self {
        Self { source_id, offset }
    }

    /// Create a new [LexerPosition]
    ///
    /// # Parameters
    ///
    /// * `source_id`: source id
    /// * `offset`: raw byte offset in the input
    ///
    /// # Panics
    ///
    /// Panics if the offset can't be converted to a u32.
    pub fn new_raw<E: std::fmt::Debug>(
        source_id: FileId,
        offset: impl TryInto<u32, Error = E>,
    ) -> Self {
        Self {
            source_id,
            offset: offset.try_into().expect("input too large").into(),
        }
    }
}

impl Display for LexerPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.source_id, u32::from(self.offset))
    }
}

impl From<LexerPosition> for FileId {
    fn from(value: LexerPosition) -> Self {
        value.source_id
    }
}

impl From<LexerPosition> for TextSize {
    fn from(value: LexerPosition) -> Self {
        value.offset
    }
}

impl From<LexerPosition> for PointOrRange {
    fn from(value: LexerPosition) -> Self {
        Self::Point(value.into())
    }
}

/// Span information for a node, constructed from a pair of LexerPositions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeSpan {
    /// The index of this span into the list of parsed units. This is used to
    /// identify which source string this span refers to when combining multiple ASTs
    source_id: FileId,

    /// Range of the node in the input slice
    range: TextRange,
}

impl NodeSpan {
    /// Create a new node span
    pub fn new(source_id: FileId, range: TextRange) -> Self {
        Self { source_id, range }
    }

    /// Create a new node span from two lexer positions
    pub fn from_lexer(start: LexerPosition, end: LexerPosition) -> Self {
        Self {
            source_id: start.source_id,
            range: TextRange::new(start.offset, end.offset),
        }
    }

    /// Return a 0-length span located at the start of the given source
    ///
    /// This may be used in span range queries.
    pub fn new_start(source_id: FileId) -> Self {
        Self {
            source_id,
            range: TextRange::default(),
        }
    }

    /// Return a 0-length span located at the end of the given source (as indicated by the offset)
    ///
    /// This may be used in span range queries.
    pub fn new_end(source_id: FileId, length: usize) -> Self {
        let length = TextSize::try_from(length).expect("length is too large");

        Self {
            source_id,
            range: TextRange::new(length, length),
        }
    }

    /// Return the source identifier for this node span
    pub fn source_id(&self) -> FileId {
        self.source_id
    }

    /// Return the span range
    pub fn range(&self) -> TextRange {
        self.range
    }

    /// Return the length of this span
    pub fn length(&self) -> usize {
        self.range.len().into()
    }

    /// Return the start of this span as a LexerPosition
    pub fn start(&self) -> LexerPosition {
        LexerPosition::new(self.source_id, self.range.start())
    }

    /// Return the end of this span as a LexerPosition
    pub fn end(&self) -> LexerPosition {
        LexerPosition::new(self.source_id, self.range.end())
    }
}

impl PartialOrd for NodeSpan {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NodeSpan {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.source_id
            .cmp(&other.source_id)
            .then(self.range.start().cmp(&other.range.start()))
            .then(self.range.len().cmp(&other.range.len()))
    }
}
