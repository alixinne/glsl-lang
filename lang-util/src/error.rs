use std::error::Error;
use std::fmt;

use crate::position::LexerPosition;

pub trait Token: fmt::Display {}

pub trait LexicalError: Error {
    fn location(&self) -> LexerPosition;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedPosition {
    raw: LexerPosition,
    line_index: usize,
    pos_index: usize,
}

impl ResolvedPosition {
    pub fn new(raw: LexerPosition, input: &str) -> Option<ResolvedPosition> {
        if raw.offset >= input.len() {
            return None;
        }

        // Find line start offset
        let line_start = line_span::find_line_start(input, raw.offset);

        // Count newlines
        let line_index = input
            .bytes()
            .take(line_start)
            .filter(|c| *c == b'\n')
            .count();

        // Find column
        let pos_index = raw.offset - line_start;

        Some(Self {
            raw,
            line_index,
            pos_index,
        })
    }

    pub fn source_id(&self) -> usize {
        self.raw.source_id
    }

    pub fn offset(&self) -> usize {
        self.raw.offset
    }

    pub fn line(&self) -> usize {
        self.line_index
    }

    pub fn col(&self) -> usize {
        self.pos_index
    }
}

impl fmt::Display for ResolvedPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.source_id(), self.line() + 1, self.col())
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError<E: LexicalError> {
    pub position: ResolvedPosition,
    pub kind: ParseErrorKind<E>,
}

impl<E: LexicalError> ParseError<E> {
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
        )
        .expect("invalid location");

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
#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind<E: LexicalError> {
    InvalidToken,
    UnrecognizedEOF {
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
    },
    ExtraToken {
        token: String,
    },
    LexicalError {
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
        let resolved = ResolvedPosition::new(pos, s).unwrap();

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
        let resolved = ResolvedPosition::new(pos, s).unwrap();

        assert_eq!(resolved.line(), 2);
        assert_eq!(resolved.col(), 4);
    }

    #[test]
    fn resolved_position_out_of_bounds() {
        let pos = LexerPosition::new(0, 1);
        assert_eq!(None, ResolvedPosition::new(pos, ""));
    }
}
