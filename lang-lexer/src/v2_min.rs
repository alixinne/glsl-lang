//! glsl-lang-pp/min based lexer

use glsl_lang_pp::types;

use lang_util::{
    position::{LexerPosition, NodeSpan},
    TextSize,
};

pub mod str;

/// Lexical analysis error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexicalError {
    /// Invalid token in lexical analysis
    Token {
        /// Type of invalid token error
        kind: types::token::ErrorKind,
        /// Location of the error
        pos: NodeSpan,
    },
}

impl std::fmt::Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalError::Token { kind, .. } => write!(f, "{}", kind),
        }
    }
}

impl std::error::Error for LexicalError {}

impl lang_util::error::LexicalError for LexicalError {
    fn location(&self) -> (LexerPosition, TextSize) {
        match self {
            LexicalError::Token { pos, .. } => (pos.start(), pos.len()),
        }
    }
}
