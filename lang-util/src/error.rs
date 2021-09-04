//! Error type definitions

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;

use text_size::{TextRange, TextSize};

use crate::{located::Located, position::LexerPosition, token::Token, FileId};

/// Information about a lexed token
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde", derive(rserde::Serialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct TokenDescription {
    /// String representation of the lexed token
    pub formatted: String,

    /// Variant name
    pub variant_name: &'static str,

    /// Parser token name
    pub parser_token: &'static str,

    /// List of kinds this token belongs to
    pub kinds: &'static [&'static str],
}

impl<'t, T: Token> From<&'t T> for TokenDescription {
    fn from(token: &'t T) -> Self {
        Self {
            formatted: token.to_string(),
            variant_name: token.variant_name(),
            parser_token: token.parser_token(),
            kinds: token.kinds(),
        }
    }
}

impl fmt::Display for TokenDescription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted)
    }
}

/// Methods provided for all [Token] implementations
pub trait TokenExt {
    /// Return a descriptor for the current token
    fn description(&self) -> TokenDescription;
}

impl<T: Token> TokenExt for T {
    fn description(&self) -> TokenDescription {
        TokenDescription::from(self)
    }
}

/// An error produced by lexical analysis
pub trait LexicalError: Error {
    /// Return the location at which this error occurred
    ///
    /// # Returns
    ///
    /// [LexerPosition] structure that indicates at which offset in the input the error occurred,
    /// and length of the range.
    fn location(&self) -> (LexerPosition, TextSize);
}

/// A parsing error wrapped from lalrpop_util's error type
pub type ParseError<E> = Located<ParseErrorKind<E>>;

/// Return the LexerLocation of a lalrpop_util::ParseError
pub fn error_location<T, E: LexicalError>(
    error: &lalrpop_util::ParseError<LexerPosition, T, E>,
) -> (FileId, TextRange) {
    let (location, len) = match error {
        // TODO: Find out invalid token length
        lalrpop_util::ParseError::InvalidToken { location } => (*location, TextSize::default()),
        lalrpop_util::ParseError::UnrecognizedEOF { location, .. } => {
            (*location, TextSize::default())
        }
        lalrpop_util::ParseError::UnrecognizedToken { token, .. } => {
            (token.0, token.2.offset - token.0.offset)
        }
        lalrpop_util::ParseError::ExtraToken { token } => {
            (token.0, token.2.offset - token.0.offset)
        }
        lalrpop_util::ParseError::User { error } => error.location(),
    };

    (
        location.source_id,
        TextRange::new(location.offset, location.offset + len),
    )
}

// We represent tokens as formatted string since we only want to display them
/// Parsing error kind
#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind<E: LexicalError> {
    /// An invalid token was encountered during lexical analysis
    InvalidToken,
    /// Unexpected end of file
    UnrecognizedEof {
        /// List of expected token names
        expected: Vec<String>,
    },
    /// Unexpected token
    UnrecognizedToken {
        /// The unexpected token
        token: TokenDescription,
        /// List of expected token names
        expected: Vec<String>,
    },
    /// Extra token after input
    ExtraToken {
        /// The extra token
        token: TokenDescription,
    },
    /// Lexical analysis error
    LexicalError {
        /// Lexical error
        error: E,
    },
}

impl<E: std::error::Error + LexicalError + 'static> std::error::Error for ParseErrorKind<E> {}

impl<T: Token, E: LexicalError> From<lalrpop_util::ParseError<LexerPosition, T, E>>
    for ParseErrorKind<E>
{
    fn from(error: lalrpop_util::ParseError<LexerPosition, T, E>) -> Self {
        // Simplification function
        let simplify = || {
            // Lookup structure for token names and kinds
            let mut token_descriptors = HashMap::new();
            let mut token_kinds: HashMap<&'static str, HashSet<&'static str>> = HashMap::new();
            for descriptor in T::all_tokens() {
                // Add the token descriptor
                token_descriptors.insert(descriptor.parser_token, descriptor);

                // Add the token descriptor to the various kinds
                for kind in descriptor.kinds {
                    if let Some(existing) = token_kinds.get_mut(kind) {
                        existing.insert(descriptor.parser_token);
                    } else {
                        token_kinds
                            .insert(kind, std::iter::once(descriptor.parser_token).collect());
                    }
                }
            }

            move |expected: Vec<String>| -> Vec<String> {
                let expected: HashSet<_> = expected.iter().map(String::as_str).collect();
                let mut seen_tokens = HashSet::new();
                let mut result = Vec::new();

                for (kind, members) in &token_kinds {
                    if members.is_subset(&expected) {
                        // Add all the tokens of this kind as seen
                        seen_tokens.extend(members);
                        // Add the kind of token to the expected list
                        result.push(*kind);
                    }
                }

                // Some expected groups might be subsets of others, try to reduce this
                let mut delete = HashSet::new();
                for expected_set_name in &result {
                    for other_set_name in &result {
                        if expected_set_name != other_set_name
                            && token_kinds
                                .get(*expected_set_name)
                                .unwrap()
                                .is_subset(token_kinds.get(*other_set_name).unwrap())
                        {
                            delete.insert(expected_set_name);
                        }
                    }
                }

                // Remove extra subsets
                let mut result: Vec<_> = result
                    .iter()
                    .filter(|item| !delete.contains(item))
                    .collect();

                // Leftover tokens should still be expected
                for leftover in expected.difference(&seen_tokens) {
                    result.push(leftover);
                }

                // Sort the result for deterministic results
                result.sort_unstable_by(|a, b| {
                    // TODO: Standalone token kinds should be last
                    let a_spaces = a.contains(' ');
                    let b_spaces = b.contains(' ');
                    if a_spaces && b_spaces {
                        a.cmp(b)
                    } else if a_spaces {
                        Ordering::Less
                    } else if b_spaces {
                        Ordering::Greater
                    } else {
                        a.len().cmp(&b.len()).reverse().then_with(|| a.cmp(b))
                    }
                });

                result.into_iter().map(|it| (**it).to_string()).collect()
            }
        };

        // Map the error kind
        match error {
            lalrpop_util::ParseError::InvalidToken { .. } => ParseErrorKind::InvalidToken,
            lalrpop_util::ParseError::UnrecognizedEOF { expected, .. } => {
                ParseErrorKind::UnrecognizedEof {
                    expected: simplify()(expected),
                }
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                ParseErrorKind::UnrecognizedToken {
                    token: token.1.description(),
                    expected: simplify()(expected),
                }
            }
            lalrpop_util::ParseError::ExtraToken { token } => ParseErrorKind::ExtraToken {
                token: token.1.description(),
            },
            lalrpop_util::ParseError::User { error } => ParseErrorKind::LexicalError { error },
        }
    }
}

struct ListDisplay<'s>(&'s [String]);
struct KindDisplay<'s>(&'s str);

impl<'s> fmt::Display for KindDisplay<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self
            .0
            .chars()
            .next()
            .map(char::is_alphabetic)
            .unwrap_or(false)
        {
            write!(f, "{}", self.0)
        } else {
            write!(f, "`{}`", self.0)
        }
    }
}

impl<'s> fmt::Display for ListDisplay<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "nothing")
        } else {
            let first = self.0.first().unwrap();
            match first.chars().next() {
                Some('a') | Some('e') | Some('i') | Some('u') | Some('o') | Some('y') => {
                    write!(f, "an ")?
                }
                _ => write!(f, "a ")?,
            }

            write!(f, "{}", KindDisplay(first))?;

            for rest in self.0.iter().skip(1).take(self.0.len() - 2) {
                write!(f, ", {}", KindDisplay(rest))?;
            }

            if self.0.len() > 1 {
                write!(f, " or {}", KindDisplay(self.0.last().unwrap()))?;
            }

            Ok(())
        }
    }
}

impl<E: LexicalError> fmt::Display for ParseErrorKind<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::InvalidToken => write!(f, "invalid token"),
            ParseErrorKind::UnrecognizedEof { expected } => {
                write!(
                    f,
                    "unexpected end of input, expected {}",
                    ListDisplay(expected)
                )
            }
            ParseErrorKind::UnrecognizedToken { token, expected } => {
                write!(
                    f,
                    "unexpected {}, expected {}",
                    token,
                    ListDisplay(expected)
                )
            }
            ParseErrorKind::ExtraToken { token } => {
                write!(f, "extra {} at end of input", token)
            }
            ParseErrorKind::LexicalError { error } => write!(f, "{}", error),
        }
    }
}
