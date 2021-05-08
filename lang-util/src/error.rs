//! Error type definitions

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;

use crate::position::LexerPosition;

/// Information about a known token
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct TokenDescriptor {
    /// Variant name
    pub variant_name: &'static str,

    /// Parser token name
    pub parser_token: &'static str,

    /// List of kinds this token belongs to
    pub kinds: &'static [&'static str],
}

impl TokenDescriptor {
    /// Create a new token descriptor
    pub const fn new(
        variant_name: &'static str,
        parser_token: &'static str,
        kinds: &'static [&'static str],
    ) -> Self {
        Self {
            variant_name,
            parser_token,
            kinds,
        }
    }
}

/// Information about a lexed token
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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

/// Trait to implement for a token to be used with `lang_util`'s infrastructure
pub trait Token: fmt::Display {
    /// Return the variant name of the current token
    fn variant_name(&self) -> &'static str;

    /// Return the name used by the lalrpop parser for this token
    fn parser_token(&self) -> &'static str;

    /// Return the token kinds this token belongs to
    fn kinds(&self) -> &'static [&'static str];

    /// Return the descriptions for all known tokens
    fn all_tokens() -> &'static [TokenDescriptor];
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

    /// Display the resolved position without source number
    pub fn without_source_number(self) -> OptionalSourceNumber<Self> {
        OptionalSourceNumber(self)
    }
}

impl fmt::Display for ResolvedPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.source_id(), self.line() + 1, self.col())
    }
}

/// Display wrapper for optional source numbers in resolved positions
pub struct OptionalSourceNumber<T>(T);

impl<T> fmt::Display for OptionalSourceNumber<T>
where
    ResolvedPosition: From<T>,
    T: Copy,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pos = ResolvedPosition::from(self.0);

        if pos.source_id() == 0 {
            write!(f, "{}:{}", pos.line() + 1, pos.col())
        } else {
            write!(f, "{}", pos)
        }
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
        let position = ResolvedPosition::from((&error, input));

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
        let kind = match error {
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
        };

        Self { position, kind }
    }
}

impl<'s, T, E: LexicalError> From<(&'s lalrpop_util::ParseError<LexerPosition, T, E>, &'s str)>
    for ResolvedPosition
{
    fn from((error, input): (&'s lalrpop_util::ParseError<LexerPosition, T, E>, &'s str)) -> Self {
        // Resolve position into something that is user readable
        Self::new(
            match error {
                lalrpop_util::ParseError::InvalidToken { location } => *location,
                lalrpop_util::ParseError::UnrecognizedEOF { location, .. } => *location,
                lalrpop_util::ParseError::UnrecognizedToken { token, .. } => token.0,
                lalrpop_util::ParseError::ExtraToken { token } => token.0,
                lalrpop_util::ParseError::User { error } => error.location(),
            },
            input,
        )
    }
}

impl<E: LexicalError> fmt::Display for ParseError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.kind)
    }
}

impl<E: LexicalError> Error for ParseError<E> {}

impl<'e, E: LexicalError> From<&'e ParseError<E>> for ResolvedPosition {
    fn from(error: &'e ParseError<E>) -> Self {
        error.position
    }
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
                    ListDisplay(&expected)
                )
            }
            ParseErrorKind::UnrecognizedToken { token, expected } => {
                write!(
                    f,
                    "unexpected {}, expected {}",
                    token,
                    ListDisplay(&expected)
                )
            }
            ParseErrorKind::ExtraToken { token } => {
                write!(f, "extra {} at end of input", token)
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
