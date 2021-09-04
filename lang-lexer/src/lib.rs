mod context;
pub use context::*;

mod token;
use lang_util::position::LexerPosition;
pub use token::*;

#[cfg(feature = "v1")]
pub mod v1;

#[cfg(feature = "v2")]
pub mod v2;

/// Language lexer error definition
pub trait HasLexerError {
    /// Type of lexical analysis error
    type Error: lang_util::error::LexicalError;
}

/// GLSL language lexer
pub trait LangLexer<'i>: HasLexerError + Sized {
    /// Type of the input for this lexer
    type Input: 'i;
    /// Type of the iterator returned by this lexer
    type Iter: LangLexerIterator + HasLexerError<Error = Self::Error>;

    /// Instantiate the lexer
    ///
    /// # Parameters
    ///
    /// * `source`: input for the lexer
    /// * `opts`: parsing options
    fn new(source: Self::Input, opts: &ParseOptions) -> Self;

    /// Run the lexer
    fn run(self, ctx: ParseContext) -> Self::Iter;
}

/// GLSL language lexer iterator
pub trait LangLexerIterator:
    Iterator<Item = Result<(LexerPosition, Token, LexerPosition), Self::Error>> + HasLexerError
{
    #[cfg(feature = "lalrpop")]
    fn resolve_err(
        &self,
        err: lalrpop_util::ParseError<LexerPosition, Token, Self::Error>,
    ) -> lang_util::error::ParseError<Self::Error>;
}
