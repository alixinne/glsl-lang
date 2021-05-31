/// First stage lexer declaration
use crate::{Input, TextToken};
use rowan::TextRange;

/// Type of token for line splitting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
pub struct NewlineSplitter<I: Input> {
    input: I,
}

impl<I: Input> NewlineSplitter<I> {
    pub fn new(input: I) -> Self {
        Self { input }
    }

    pub fn input(&self) -> &dyn Input {
        &self.input
    }
}

impl<I: Input> Iterator for NewlineSplitter<I> {
    type Item = NewlineToken;

    fn next(&mut self) -> Option<Self::Item> {
        use NewlineTokenKind::*;

        let c = self.input.next_char();

        // GLSL spec: Lines are relevant for compiler diagnostic messages and the
        // preprocessor.  They are terminated by carriage-return or line-feed. If both
        // are used together, it will count as only a single line termination.

        match c {
            Some('\n') | Some('\r') => {
                let mut newline_start_pos = self.input.current_pos();
                let next_char = self.input.peek_char();

                // Advance to next char if it's also part of the newline
                if next_char == Some('\r') || next_char == Some('\n') && next_char != c {
                    self.input.next_char();
                    newline_start_pos =
                        TextRange::new(newline_start_pos.start(), self.input.current_pos().end());
                }

                return Some(TextToken::new(NEWLINE, newline_start_pos));
            }
            Some(ch) if ch.is_ascii_alphabetic() => {
                return Some(TextToken::new(LETTER, self.input.current_pos()));
            }
            Some(ch) if ch.is_ascii_digit() => {
                return Some(TextToken::new(DIGIT, self.input.current_pos()));
            }
            Some(ch) if ch.is_ascii_whitespace() => {
                // \n and \r have been already matched
                return Some(TextToken::new(WS, self.input.current_pos()));
            }
            Some(_) => {
                return Some(TextToken::new(PUNCT, self.input.current_pos()));
            }
            None => {
                return None;
            }
        }
    }
}
