//! glsl-lang-pp based lexer

use glsl_lang_pp::{processor, types};

use lang_util::{
    located::Located,
    position::{LexerPosition, NodeSpan},
    TextSize,
};

use super::Token;

mod core;

pub mod fs;
pub mod str;

/// Lexical analysis error
#[derive(Debug)]
pub enum LexicalError<E: std::error::Error + 'static> {
    /// Invalid token in lexical analysis
    Token {
        /// Type of invalid token error
        kind: types::token::ErrorKind,
        /// Location of the error
        pos: NodeSpan,
    },
    /// Preprocessor error
    Processor(processor::event::Error),
    /// i/o error
    Io(Located<E>),
}

impl<E: std::error::Error + 'static> std::cmp::PartialEq for LexicalError<E> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LexicalError::Token { kind, pos } => match other {
                LexicalError::Token {
                    kind: other_kind,
                    pos: other_pos,
                } => kind == other_kind && pos == other_pos,
                _ => false,
            },
            LexicalError::Processor(p) => match other {
                LexicalError::Processor(other_p) => p == other_p,
                _ => false,
            },
            LexicalError::Io(_) => false,
        }
    }
}

impl<E: std::error::Error + 'static> std::fmt::Display for LexicalError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalError::Token { kind, .. } => write!(f, "{}", kind),
            LexicalError::Processor(error) => write!(f, "{}", error.inner()),
            LexicalError::Io(io) => write!(f, "{}", io.inner()),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for LexicalError<E> {}

impl<E: std::error::Error + 'static> lang_util::error::LexicalError for LexicalError<E> {
    fn location(&self) -> (LexerPosition, TextSize) {
        match self {
            LexicalError::Token { pos, .. } => (pos.start(), pos.len()),
            LexicalError::Processor(err) => (
                LexerPosition::new(err.current_file().unwrap(), err.pos().start()),
                err.pos().len(),
            ),
            LexicalError::Io(io) => (
                LexerPosition::new(io.current_file().unwrap(), io.pos().start()),
                io.pos().len(),
            ),
        }
    }
}

impl<E: std::error::Error + 'static> From<processor::event::Error> for LexicalError<E> {
    fn from(error: processor::event::Error) -> Self {
        Self::Processor(error)
    }
}
