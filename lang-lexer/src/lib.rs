mod context;
pub use context::*;

mod token;
use lang_util::position::LexerPosition;
pub use token::*;

mod lang_token;

pub mod min;

#[cfg(feature = "full")]
pub mod full;

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
    fn resolve_err(
        &self,
        err: lalrpop_util::ParseError<LexerPosition, Token, Self::Error>,
    ) -> lang_util::error::ParseError<Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::*;

    const HASH_IDENT_TEST_CASE: &str = "# (ident) = hello";

    fn test_hash_ident_with_lexer<'i>(lexer: impl LangLexer<'i, Input = &'i str>) {
        let tokens: Vec<_> = lexer.run(ParseContext::default()).collect();
        eprintln!("{tokens:#?}");
        assert!(tokens.len() > 1);
    }

    #[test]
    fn test_hash_ident_min() {
        test_hash_ident_with_lexer(min::str::Lexer::new(
            HASH_IDENT_TEST_CASE,
            &ParseOptions {
                allow_rs_ident: true,
                ..Default::default()
            },
        ));
    }

    #[cfg(feature = "full")]
    #[test]
    fn test_hash_ident_full() {
        test_hash_ident_with_lexer(full::str::Lexer::new(
            HASH_IDENT_TEST_CASE,
            &ParseOptions {
                allow_rs_ident: true,
                ..Default::default()
            },
        ));
    }
}
