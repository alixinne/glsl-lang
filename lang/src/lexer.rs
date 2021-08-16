//! Lexical analysis definitions

mod token;
pub use token::*;

#[cfg(feature = "lexer-v1")]
pub mod v1;
#[cfg(feature = "lexer-v2")]
pub mod v2;

pub use lang_util::position::LexerPosition;

/// Type alias for contextual lexing context
pub type LexerContext = crate::parse::ParseContext;
