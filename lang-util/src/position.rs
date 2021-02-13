use parse_display::Display;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display("{source_id}:{offset}")]
pub struct LexerPosition {
    pub source_id: usize,
    pub offset: usize,
}

impl LexerPosition {
    pub fn new(source_id: usize, offset: usize) -> Self {
        Self { source_id, offset }
    }
}

/// Span information for a node, constructed from a nom_locate::LocatedSpan
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
