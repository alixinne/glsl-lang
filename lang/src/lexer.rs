mod token;
pub use token::*;

#[cfg(feature = "lexer-v1")]
pub mod v1;
#[cfg(feature = "lexer-v2")]
pub mod v2;
#[cfg(not(any(feature = "lexer-v1", feature = "lexer-v2")))]
compile_error!(
    "no lexer selected, please enable glsl-lang/lexer-v1 or glsl-lang/lexer-v2 to use this crate"
);

pub use lang_util::position::LexerPosition;

pub type LexerContext = crate::parse::ParseContext;
