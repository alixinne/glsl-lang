/// Preprocessor directive parser declaration
use std::collections::VecDeque;

use rowan::{TextRange, TextSize};

use crate::{
    lexer::{self, Lexer},
    Unescaped,
};

mod ast;
pub use ast::*;

mod error;
pub use error::*;

mod lang;
pub use lang::*;

mod syntax_kind;
pub use syntax_kind::*;

mod syntax;

type SyntaxBitset = cbitset::BitSet256;
// SyntaxBitset::capacity() isn't a const fn, keep this in sync
static_assertions::const_assert!((SyntaxKind::_LAST as usize) < 256);

pub type SyntaxNode = rowan::SyntaxNode<PreprocessorLang>;
pub type SyntaxToken = rowan::SyntaxToken<PreprocessorLang>;

pub struct Parser<'i> {
    builder: rowan::GreenNodeBuilder<'static>,
    source: &'i str,
    input: Lexer<'i>,
    peeked: Option<Option<lexer::TextToken>>,
    trivia_buffer: VecDeque<lexer::TextToken>,
    errors: Vec<Error>,
}

// Public parser API
impl<'i> Parser<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            builder: Default::default(),
            source: input,
            input: Lexer::new(input),
            peeked: None,
            trivia_buffer: VecDeque::with_capacity(4),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Ast {
        self.start_node(SyntaxKind::ROOT);
        syntax::file(&mut self);
        self.finish_node();

        Ast::new(
            self.builder.finish(),
            self.errors,
            self.input.into_line_map(),
        )
    }

    pub fn parse_define_body(mut self) -> Option<SyntaxNode> {
        self.start_node(SyntaxKind::ROOT);
        syntax::define_body(&mut self);
        self.finish_node();

        if self.errors.is_empty() {
            SyntaxNode::new_root(self.builder.finish()).first_child()
        } else {
            None
        }
    }
}

// Builder wrapper methods
impl<'i> Parser<'i> {
    fn checkpoint(&mut self) -> rowan::Checkpoint {
        self.builder.checkpoint()
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    fn start_node_at(&mut self, checkpoint: rowan::Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(checkpoint, kind.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }
}

// Private parser API
impl<'i> Parser<'i> {
    fn skip(&mut self, what: impl Fn(&lexer::Token) -> bool) {
        while self.peek().map(|tk| what(&tk.token)).unwrap_or(false) {
            self.bump();
        }
    }

    fn skip_trivia(&mut self) {
        self.skip(lexer::Token::is_trivia)
    }

    fn peek(&mut self) -> Option<lexer::TextToken> {
        if self.peeked.is_none() {
            // No token was peeked, read one from the input
            self.peeked = Some(self.input.next());
        }

        // TODO: Move this to unwrap_unchecked when it is stable
        // unwrap: self.peeked is necessarily Some
        self.peeked.unwrap()
    }

    fn raw(&self, token: lexer::TextToken) -> &'i str {
        token.raw(self.source)
    }

    fn text(&self, token: lexer::TextToken) -> Unescaped {
        Unescaped::new(self.raw(token))
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.trivia_buffer.pop_front() {
            self.builder
                .token(SyntaxKind::from(token.token).into(), token.raw(self.source));
        }
    }

    fn buffer_trivia(&mut self) {
        while let Some(current) = self.peek() {
            if current.token.is_trivia() {
                self.trivia_buffer.push_back(current);
            } else {
                break;
            }

            // Advance
            self.peeked.take();
        }
    }

    fn push_error(&mut self, error_kind: ErrorKind, range: TextRange) {
        self.errors
            .push(Error::new(error_kind, range, self.input.line_map()));
    }

    #[must_use = "None is returned if the expected token was not found"]
    fn expect_any(
        &mut self,
        expected: &[lexer::Token],
        dont_bump: &[lexer::Token],
    ) -> Option<lexer::TextToken> {
        let bitset: SyntaxBitset = expected.iter().map(|&k| k as u16).collect();
        let dont_bump_bitset: SyntaxBitset = dont_bump.iter().map(|&k| k as u16).collect();

        if let Some(token) = self.peek() {
            // Always bump, expect consumes
            if !dont_bump_bitset.contains(*token as _) {
                self.bump();
            }

            if bitset.contains(*token as _) {
                return Some(token);
            } else {
                self.push_error(
                    ErrorKind::Unexpected {
                        actual: *token,
                        expected: expected.into(),
                    },
                    token.range,
                );

                return None;
            }
        }

        self.push_error(
            ErrorKind::EndOfInput {
                expected: expected.into(),
            },
            TextRange::new(TextSize::of(self.source), TextSize::of(self.source)),
        );

        None
    }

    fn bump(&mut self) {
        // Get input token
        let token = if let Some(token) = self.peeked.take() {
            token
        } else {
            self.input.next()
        };

        if let Some(token) = token {
            self.builder
                .token(SyntaxKind::from(token.token).into(), token.raw(self.source));
        } else {
            panic!("tried to bump at end of input");
        }
    }
}

#[cfg(test)]
mod tests;
