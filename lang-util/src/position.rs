//! Input position representation types

use std::fmt::Display;

/// A position in the lexer's input
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LexerPosition {
    /// Source id
    pub source_id: usize,
    /// Raw byte offset
    pub offset: usize,
}

impl LexerPosition {
    /// Create a new [LexerPosition]
    ///
    /// # Parameters
    ///
    /// * `source_id`: source id
    /// * `offset`: raw byte offset in the input
    pub fn new(source_id: usize, offset: usize) -> Self {
        Self { source_id, offset }
    }
}

impl Display for LexerPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.source_id, self.offset)
    }
}

/// Span information for a node, constructed from a pair of LexerPositions
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    /// This may be used in span range queries.
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

    /// Return the start of this span as a LexerPosition
    pub fn start(&self) -> LexerPosition {
        LexerPosition::new(self.source_id, self.start)
    }

    /// Return the end of this span as a LexerPosition
    pub fn end(&self) -> LexerPosition {
        LexerPosition::new(self.source_id, self.end)
    }
}
