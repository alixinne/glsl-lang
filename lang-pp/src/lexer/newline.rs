//! First stage lexer declaration

use std::{iter::Peekable, str::CharIndices};

use crate::TextToken;
use rowan::{TextRange, TextSize};

/// Type of token for line splitting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum NewlineTokenKind {
    LETTER,
    DIGIT,
    PUNCT,
    NEWLINE,
    WS,
}

/// First stage token with location
pub type NewlineToken = TextToken<NewlineTokenKind>;

/// Basic lexer to split input lines according to the GLSL spec
///
/// This only detects \r\n sequences and classifies other characters following the types declared
/// in [NewlineTokenKind](NewlineTokenKind).
pub struct NewlineSplitter<'i> {
    end: TextSize,
    chars: Peekable<CharIndices<'i>>,
}

impl<'i> NewlineSplitter<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            end: TextSize::of(input),
            chars: input.char_indices().peekable(),
        }
    }

    fn current_pos(&mut self, start_pos: usize) -> TextRange {
        TextRange::new(
            TextSize::from(start_pos as u32),
            self.chars
                .peek()
                .map(|(pos, _)| TextSize::from(*pos as u32))
                .unwrap_or(self.end),
        )
    }
}

impl<'i> Iterator for NewlineSplitter<'i> {
    type Item = NewlineToken;

    fn next(&mut self) -> Option<Self::Item> {
        use NewlineTokenKind::*;

        let c = self.chars.next();

        // GLSL spec: Lines are relevant for compiler diagnostic messages and the
        // preprocessor.  They are terminated by carriage-return or line-feed. If both
        // are used together, it will count as only a single line termination.

        match c {
            Some((pos, ch)) if ch == '\r' || ch == '\n' => {
                // Advance to next char if it's also part of the newline
                let range = if let Some((next_pos, next_ch)) = self.chars.peek() {
                    // End boundary of the newline token
                    let end = if (*next_ch == '\r' || *next_ch == '\n') && *next_ch != ch {
                        self.chars.next();

                        // Peek to get the next char boundary
                        self.chars
                            .peek()
                            .map(|(pos, _)| TextSize::from(*pos as u32))
                            .unwrap_or(self.end)
                    } else {
                        TextSize::from(*next_pos as u32)
                    };

                    TextRange::new(TextSize::from(pos as u32), end)
                } else {
                    // No more characters
                    TextRange::new(TextSize::from(pos as u32), self.end)
                };

                Some(TextToken::new(NEWLINE, range))
            }
            Some((pos, ch)) if ch.is_ascii_alphabetic() => {
                Some(TextToken::new(LETTER, self.current_pos(pos)))
            }
            Some((pos, ch)) if ch.is_ascii_digit() => {
                Some(TextToken::new(DIGIT, self.current_pos(pos)))
            }
            Some((pos, ch)) if ch.is_ascii_whitespace() => {
                // \n and \r have been already matched
                Some(TextToken::new(WS, self.current_pos(pos)))
            }
            Some((pos, _)) => Some(TextToken::new(PUNCT, self.current_pos(pos))),
            None => None,
        }
    }
}
