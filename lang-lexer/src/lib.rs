mod context;
pub use context::*;

mod token;
use lang_util::position::LexerPosition;
pub use token::*;

#[cfg(feature = "v1")]
#[deprecated(
    since = "0.6.0",
    note = "the v1 lexer is not spec-compliant and relies on outdated dependencies. It will be removed in glsl-lang 0.8.0"
)]
pub mod v1;

#[cfg(any(feature = "v2-min", feature = "v2-full"))]
mod v2;

#[cfg(feature = "v2-min")]
pub mod v2_min;

#[cfg(feature = "v2-full")]
pub mod v2_full;

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

#[cfg(test)]
mod tests {
    use super::*;

    const HASH_IDENT_TEST_CASE: &str = "# (ident) = hello";

    fn test_hash_ident_with_lexer<'i>(lexer: impl LangLexer<'i, Input = &'i str>) {
        let tokens: Vec<_> = lexer.run(ParseContext::default()).collect();
        eprintln!("{:#?}", tokens);
        assert!(tokens.len() > 1);
    }

    #[cfg(feature = "v2-min")]
    #[test]
    fn test_hash_ident_v2_min() {
        test_hash_ident_with_lexer(v2_min::str::Lexer::new(
            HASH_IDENT_TEST_CASE,
            &ParseOptions {
                allow_rs_ident: true,
                ..Default::default()
            },
        ));
    }

    #[cfg(feature = "v2-full")]
    #[test]
    fn test_hash_ident_v2_full() {
        test_hash_ident_with_lexer(v2_full::str::Lexer::new(
            HASH_IDENT_TEST_CASE,
            &ParseOptions {
                allow_rs_ident: true,
                ..Default::default()
            },
        ));
    }
}
