//! Error type definitions

use std::error::Error;
use std::fmt;

use crate::position::LexerPosition;

/// Trait to implement for a token to be used with `lang_util`'s infrastructure
pub trait Token: fmt::Display {}

/// An error produced by lexical analysis
pub trait LexicalError: Error {
    /// Return the location at which this error occurred
    ///
    /// # Returns
    ///
    /// [LexerPosition] structure that indicates at which offset in the input the error occurred.
    fn location(&self) -> LexerPosition;
}

/// A position in the input stream that has been resolved into the more user-friendly line and
/// column indices
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedPosition {
    raw: LexerPosition,
    line_index: usize,
    pos_index: usize,
}

impl ResolvedPosition {
    /// Create a new resolved position
    ///
    /// If the offset doesn't correspond to the input string, the results are undefined.
    ///
    /// # Parameters
    ///
    /// * `raw`: raw byte offset into the input
    /// * `input`: input string the offset is to be taken into
    ///
    /// # Returns
    ///
    /// The resolved position.
    pub fn new(raw: LexerPosition, input: &str) -> ResolvedPosition {
        let offset = if raw.offset >= input.len() {
            input.len().max(1) - 1
        } else {
            raw.offset
        };

        // Find line start offset
        let line_start = line_span::find_line_start(input, offset);

        // Count newlines
        let line_index = input
            .bytes()
            .take(line_start)
            .filter(|c| *c == b'\n')
            .count();

        // Find column
        let pos_index = offset - line_start;

        Self {
            raw,
            line_index,
            pos_index,
        }
    }

    /// Source string id for this position
    pub fn source_id(&self) -> usize {
        self.raw.source_id
    }

    /// Raw byte offset into the source string
    pub fn offset(&self) -> usize {
        self.raw.offset
    }

    /// Line index (0 based)
    pub fn line(&self) -> usize {
        self.line_index
    }

    /// Column index (0 based)
    pub fn col(&self) -> usize {
        self.pos_index
    }
}

impl fmt::Display for ResolvedPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.source_id(), self.line() + 1, self.col())
    }
}

/// A parsing error wrapped from lalrpop_util's error type
#[derive(Debug, PartialEq)]
pub struct ParseError<E: LexicalError> {
    /// Position in the input stream at which the error occurred
    pub position: ResolvedPosition,
    /// Type of the error
    pub kind: ParseErrorKind<E>,
}

impl<E: LexicalError> ParseError<E> {
    /// Create a new [ParseError]
    ///
    /// # Parameters
    ///
    /// * `error`: lalrpop_util parsing error
    /// * `input`: input string in which the error occurred
    pub fn new<T: Token>(
        error: lalrpop_util::ParseError<LexerPosition, T, E>,
        input: &str,
    ) -> Self {
        // Resolve position into something that is user readable
        let position = ResolvedPosition::new(
            match &error {
                lalrpop_util::ParseError::InvalidToken { location } => *location,
                lalrpop_util::ParseError::UnrecognizedEOF { location, .. } => *location,
                lalrpop_util::ParseError::UnrecognizedToken { token, .. } => token.0,
                lalrpop_util::ParseError::ExtraToken { token } => token.0,
                lalrpop_util::ParseError::User { error } => error.location(),
            },
            input,
        );

        // Map the error kind
        let kind = match error {
            lalrpop_util::ParseError::InvalidToken { .. } => ParseErrorKind::InvalidToken,
            lalrpop_util::ParseError::UnrecognizedEOF { expected, .. } => {
                ParseErrorKind::UnrecognizedEOF { expected }
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                ParseErrorKind::UnrecognizedToken {
                    token: token.1.to_string(),
                    expected,
                }
            }
            lalrpop_util::ParseError::ExtraToken { token } => ParseErrorKind::ExtraToken {
                token: token.1.to_string(),
            },
            lalrpop_util::ParseError::User { error } => ParseErrorKind::LexicalError { error },
        };

        Self { position, kind }
    }
}

impl<E: LexicalError> fmt::Display for ParseError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.kind)
    }
}

impl<E: LexicalError> Error for ParseError<E> {}

// We represent tokens as formatted string since we only want to display them
/// Parsing error kind
#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind<E: LexicalError> {
    /// An invalid token was encountered during lexical analysis
    InvalidToken,
    /// Unexpected end of file
    UnrecognizedEOF {
        /// List of expected token names
        expected: Vec<String>,
    },
    /// Unexpected token
    UnrecognizedToken {
        /// The unexpected token
        token: String,
        /// List of expected token names
        expected: Vec<String>,
    },
    /// Extra token after input
    ExtraToken {
        /// The extra token
        token: String,
    },
    /// Lexical analysis error
    LexicalError {
        /// Lexical error
        error: E,
    },
}

impl<E: LexicalError> fmt::Display for ParseErrorKind<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::InvalidToken => write!(f, "invalid token"),
            ParseErrorKind::UnrecognizedEOF { expected } => {
                write!(f, "unexpected end of input, expected any of {:?}", expected)
            }
            ParseErrorKind::UnrecognizedToken { token, expected } => write!(
                f,
                "unexpected token `{}`, expected any of {:?}",
                token, expected
            ),
            ParseErrorKind::ExtraToken { token } => {
                write!(f, "extra token `{}` at end of input", token)
            }
            ParseErrorKind::LexicalError { error } => write!(f, "{}", error),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolved_position() {
        let s = r#"
Hello,
World"#;

        let pos = LexerPosition::new(0, s.find('r').unwrap());
        let resolved = ResolvedPosition::new(pos, s);

        assert_eq!(resolved.line(), 2);
        assert_eq!(resolved.col(), 2);
        assert_eq!(format!("{}", resolved), "0:3:2");
    }

    #[test]
    fn resolved_position_last_char() {
        let s = r#"
Hello,
World"#;

        let pos = LexerPosition::new(0, s.find('d').unwrap());
        let resolved = ResolvedPosition::new(pos, s);

        assert_eq!(resolved.line(), 2);
        assert_eq!(resolved.col(), 4);
    }

    #[test]
    fn resolved_position_out_of_bounds() {
        let pos = LexerPosition::new(0, 1);
        assert_eq!(ResolvedPosition::new(pos, "").line(), 0);
    }
}
