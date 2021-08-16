//! glsl-lang-pp based lexer

use glsl_lang_pp::{
    last,
    processor::{self, event::Located, str::ProcessStrError},
};

use super::{LexerPosition, Token};

mod core;

pub mod fs;
pub mod str;

/// Lexical analysis error
#[derive(Debug)]
pub enum LexicalError<E: std::error::Error + 'static> {
    /// Invalid token in lexical analysis
    Token {
        /// Type of invalid token error
        kind: last::token::ErrorKind,
        /// Location of the error
        pos: LexerPosition,
    },
    /// Include requested in memory-based parsing
    ProcessStrError {
        /// Source error
        error: ProcessStrError,
        /// Location of the error
        pos: LexerPosition,
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
            LexicalError::ProcessStrError { error, pos } => match other {
                LexicalError::ProcessStrError {
                    error: other_error,
                    pos: other_pos,
                } => error == other_error && pos == other_pos,
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
            LexicalError::ProcessStrError { error, .. } => write!(f, "{}", error),
            LexicalError::Processor(error) => write!(f, "{}", error.kind()),
            LexicalError::Io(io) => write!(f, "{}", io.inner()),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for LexicalError<E> {}

impl<E: std::error::Error + 'static> lang_util::error::LexicalError for LexicalError<E> {
    fn location(&self) -> LexerPosition {
        match self {
            LexicalError::Token { pos, .. } => *pos,
            LexicalError::ProcessStrError { pos, .. } => *pos,
            LexicalError::Processor(err) => {
                LexerPosition::new(err.current_file(), err.pos().start().into())
            }
            LexicalError::Io(io) => LexerPosition::new(io.current_file(), io.pos().start().into()),
        }
    }
}
