//! Second stage lexer declaration

use crate::Unescaped;

use super::{LineMap, NewlineSplitter, NewlineToken, NewlineTokenKind};
use rowan::TextRange;

mod token;
pub use token::Token;
use Token::*;

pub type TextToken = crate::TextToken<token::Token>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum State {
    /// Initial state of the preprocessor
    Init,
    /// Possibly a start of comment
    Slash,
    /// Line continuation character seen
    Backslash,
    /// Building an identifier
    Ident,
    /// Building a digit sequence
    Digits,
    /// Single-line comment
    SingleComment,
    /// Multi-line comment
    MultiComment,
    /// Multi-line comment, saw a *
    MultiCommentStar,
    /// Any kind of horizontal whitespace
    Whitespace,
    /// Inside a quote string
    QuoteString,
    /// Insite an angle string
    AngleString,
}

impl Default for State {
    fn default() -> Self {
        Self::Init
    }
}

/// A lexer for early lexical analysis stages.
///
/// This lexer does the following:
/// * Assemble digit sequences into single tokens
/// * Assemble identifier characters into single tokens
/// * Eliminate backslash-escaped newlines
/// * Identify single and multi-line comments
/// * Tokenize double-quoted strings and (when asked to) angle-quoted strings
#[derive(Debug, Clone)]
pub struct PreLexer<'i> {
    source: &'i str,
    input: NewlineSplitter<'i>,
    peeked: Option<Option<NewlineToken>>,
    state: State,
    start: TextRange,
    return_to: State,
    expect_angle_string: bool,
}

impl<'i> PreLexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            source: input,
            input: NewlineSplitter::new(input),
            state: Default::default(),
            start: Default::default(),
            return_to: Default::default(),
            peeked: None,
            expect_angle_string: false,
        }
    }

    pub fn line_map(&self) -> &LineMap {
        self.input.line_map()
    }

    pub fn into_line_map(self) -> LineMap {
        self.input.into_line_map()
    }

    pub fn set_expect_angle_string(&mut self, expect_angle_string: bool) {
        self.expect_angle_string = expect_angle_string;
    }

    fn peek_token(&mut self) -> Option<(NewlineToken, &'i str)> {
        self.peeked
            .unwrap_or_else(|| {
                let next = self.input.next();
                self.peeked = Some(next);
                next
            })
            .map(move |token| (token, token.raw(self.source)))
    }

    fn next_token(&mut self) -> Option<NewlineToken> {
        let result = if let Some(token) = self.peeked.take() {
            token
        } else {
            self.input.next()
        };

        // Expand the current token
        if let Some(token) = &result {
            self.start = TextRange::new(self.start.start(), token.range.end());
        }

        result
    }
}

impl<'i> Iterator for PreLexer<'i> {
    type Item = TextToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.state = match std::mem::take(&mut self.state) {
                State::Init => {
                    let c = self.next_token();

                    // All the following states need a recorded start point
                    if let Some(t) = &c {
                        self.start = t.range;
                    }

                    match c {
                        Some(NewlineToken {
                            token: NewlineTokenKind::LETTER,
                            ..
                        }) => {
                            // Start an identifier
                            State::Ident
                        }
                        Some(NewlineToken {
                            token: NewlineTokenKind::DIGIT,
                            ..
                        }) => {
                            // Start a digit sequence
                            State::Digits
                        }
                        Some(NewlineToken {
                            token: NewlineTokenKind::PUNCT,
                            range,
                        }) => {
                            let t = c.unwrap();
                            let text = t.raw(self.source);

                            match text {
                                "\"" => {
                                    // Also clear the string flag, since it should've started with
                                    // < instead
                                    self.expect_angle_string = false;
                                    State::QuoteString
                                }
                                "<" if self.expect_angle_string => {
                                    // Clear the string flag
                                    self.expect_angle_string = false;
                                    State::AngleString
                                }
                                "\\" => {
                                    self.return_to = State::Init;
                                    State::Backslash
                                }
                                "/" => State::Slash,
                                "_" => State::Ident,
                                _ => {
                                    // Punctuation
                                    return Some(TextToken {
                                        token: Token::from_punct(text),
                                        range,
                                    });
                                }
                            }
                        }
                        Some(NewlineToken {
                            token: NewlineTokenKind::NEWLINE,
                            range,
                        }) => {
                            // A newline, this completes a potential #include
                            self.expect_angle_string = false;

                            // A newline
                            return Some(TextToken {
                                token: NEWLINE,
                                range,
                            });
                        }
                        Some(NewlineToken {
                            token: NewlineTokenKind::WS,
                            ..
                        }) => State::Whitespace,
                        None => {
                            return None;
                        }
                    }
                }

                State::Slash => {
                    match self.peek_token() {
                        Some((_, "/")) => {
                            self.next_token();
                            State::SingleComment
                        }
                        Some((_, "*")) => {
                            self.next_token();
                            State::MultiComment
                        }
                        _ => {
                            // Another char or EOI, so we saw a '/' followed by something else
                            // Emit the '/' and then we'll reparse the char next round
                            return Some(TextToken::new(SLASH, self.start));
                        }
                    }
                }

                State::Backslash => {
                    // Either there's a newline and we should skip it, or there's something else
                    // and we pass the backslash forward
                    match self.peek_token() {
                        Some((
                            NewlineToken {
                                token: NewlineTokenKind::NEWLINE,
                                ..
                            },
                            _,
                        )) => {
                            self.next_token();
                            if self.return_to == State::Init {
                                // This line continuation is included nowhere, so we should emit it
                                return Some(TextToken::new(LINECONT, self.start));
                            } else {
                                // This line continuation is part of some other token
                                self.return_to
                            }
                        }
                        _ => {
                            if self.return_to == State::SingleComment
                                || self.return_to == State::MultiComment
                            {
                                // In a comment, consume the backslash, don't emit it
                                self.next_token();
                                self.return_to
                            } else {
                                return Some(TextToken::new(BACKSLASH, self.start));
                            }
                        }
                    }
                }

                State::Ident => {
                    match self.peek_token() {
                        Some((
                            NewlineToken {
                                token: NewlineTokenKind::LETTER,
                                ..
                            },
                            _,
                        ))
                        | Some((
                            NewlineToken {
                                token: NewlineTokenKind::DIGIT,
                                ..
                            },
                            _,
                        ))
                        | Some((
                            NewlineToken {
                                token: NewlineTokenKind::PUNCT,
                                ..
                            },
                            "_",
                        )) => {
                            // Continue the ident
                            self.next_token();
                            State::Ident
                        }
                        Some((_, "\\")) => {
                            self.next_token();
                            self.return_to = State::Ident;
                            State::Backslash
                        }
                        _ => {
                            // Not an ident anymore, return the ident
                            let token = TextToken::new(Token::IDENT_KW, self.start);

                            // Check if IDENT_KW is the defined keyword
                            if Unescaped::new(token.raw(self.source)) == "defined" {
                                return Some(TextToken::new(Token::DEFINED, self.start));
                            }

                            return Some(token);
                        }
                    }
                }

                State::Digits => {
                    match self.peek_token() {
                        Some((
                            NewlineToken {
                                token: NewlineTokenKind::DIGIT,
                                ..
                            },
                            _,
                        ))
                        | Some((
                            NewlineToken {
                                token: NewlineTokenKind::PUNCT,
                                ..
                            },
                            ".",
                        )) => {
                            self.next_token();
                            State::Digits
                        }
                        Some((
                            NewlineToken {
                                token: NewlineTokenKind::LETTER,
                                ..
                            },
                            _,
                        )) => {
                            self.next_token();
                            State::Digits
                        }
                        Some((_, "\\")) => {
                            self.next_token();
                            self.return_to = State::Digits;
                            State::Backslash
                        }
                        _ => {
                            // Not an ident anymore, return the ident
                            return Some(TextToken::new(Token::DIGITS, self.start));
                        }
                    }
                }

                State::SingleComment => {
                    match self.peek_token() {
                        Some((
                            NewlineToken {
                                token: NewlineTokenKind::NEWLINE,
                                ..
                            },
                            _,
                        ))
                        | None => {
                            // Do not eat the newline yet
                            return Some(TextToken::new(Token::COMMENT, self.start));
                        }
                        Some((_, "\\")) => {
                            self.next_token();
                            self.return_to = State::SingleComment;
                            State::Backslash
                        }
                        _ => {
                            // Any other char
                            self.next_token();
                            State::SingleComment
                        }
                    }
                }

                State::MultiComment => {
                    match self.peek_token() {
                        Some((_, "*")) => {
                            self.next_token();
                            State::MultiCommentStar
                        }
                        None => {
                            // Unfinished comment
                            return Some(TextToken::new(Token::ERROR, self.start));
                        }
                        _ => {
                            // Any other char
                            self.next_token();
                            State::MultiComment
                        }
                    }
                }

                State::MultiCommentStar => {
                    match self.peek_token() {
                        Some((_, "/")) => {
                            self.next_token();
                            return Some(TextToken::new(Token::COMMENT, self.start));
                        }
                        None => {
                            // Unfinished comment
                            return Some(TextToken::new(Token::ERROR, self.start));
                        }
                        _ => {
                            // Any other char
                            self.next_token();
                            State::MultiComment
                        }
                    }
                }

                State::Whitespace => {
                    if self
                        .peek_token()
                        .map(|(token, _)| token.token == NewlineTokenKind::WS)
                        .unwrap_or(false)
                    {
                        // More whitespace
                        self.next_token();
                        State::Whitespace
                    } else {
                        // No more whitespace
                        return Some(TextToken::new(WS, self.start));
                    }
                }

                State::QuoteString => {
                    if let Some((token, text)) = self.peek_token() {
                        // Release text borrow
                        let end_quote = text == "\"";

                        // Always consume the token
                        self.next_token();

                        // Extend the range
                        self.start = TextRange::new(self.start.start(), token.range.end());

                        if end_quote {
                            return Some(TextToken::new(QUOTE_STRING, self.start));
                        } else {
                            State::QuoteString
                        }
                    } else {
                        // No more tokens, bump an error
                        return Some(TextToken::new(ERROR, self.start));
                    }
                }

                State::AngleString => {
                    if let Some((token, text)) = self.peek_token() {
                        // Release text borrow
                        let end_quote = text == ">";

                        // Always consume the token
                        self.next_token();

                        // Extend the range
                        self.start = TextRange::new(self.start.start(), token.range.end());

                        if end_quote {
                            return Some(TextToken::new(ANGLE_STRING, self.start));
                        } else {
                            State::AngleString
                        }
                    } else {
                        // No more tokens, bump an error
                        return Some(TextToken::new(ERROR, self.start));
                    }
                }
            };
        }
    }
}
