//! Parsing utilities and entry points

use lang_util::position::LexerPosition;

use crate::{ast, parser};

use glsl_lang_lexer::{HasLexerError, LangLexer, Token};

pub use glsl_lang_lexer::{ParseContext, ParseOptions};

mod builder;
pub use builder::*;

mod parsable;
pub use parsable::Extractable;
#[cfg(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
))]
pub use parsable::Parsable;

/// GLSL language parser
pub trait LangParser: Sized {
    /// AST node returned by this parser
    type Item;

    /// Instantiate the parser
    fn new() -> Self;

    /// Parse the input
    fn parse<
        L: HasLexerError + Iterator<Item = Result<(LexerPosition, Token, LexerPosition), L::Error>>,
    >(
        &self,
        ctx: ParseContext,
        input: &mut L,
    ) -> Result<Self::Item, lalrpop_util::ParseError<LexerPosition, Token, L::Error>>;
}

/// GLSL language parsing capability
pub trait HasParser: Sized {
    /// Type of the parser to create
    type Parser: LangParser<Item = Self>;
}

/// GLSL language parsing functions
pub trait Parse: HasParser {
    /// Parse the input source
    fn parse<'i, L: LangLexer<'i>>(
        source: L::Input,
    ) -> Result<Self, ParseError<<L::Iter as HasLexerError>::Error>>;

    /// Parse the input source with the given options
    fn parse_with_options<'i, L: LangLexer<'i>>(
        source: L::Input,
        opts: &ParseOptions,
    ) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, Self>;

    /// Parse the input source with the given context
    fn parse_with_context<'i, L: LangLexer<'i>>(
        source: L::Input,
        ctx: &ParseContext,
    ) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, Self>;
}

impl<T: HasParser> Parse for T {
    fn parse<'i, L: LangLexer<'i>>(
        source: L::Input,
    ) -> Result<Self, ParseError<<L::Iter as HasLexerError>::Error>> {
        ParseBuilder::<L, Self>::new(source)
            .parse()
            .map(|(parsed, _names, _lexer)| parsed)
    }

    fn parse_with_options<'i, L: LangLexer<'i>>(
        source: L::Input,
        opts: &ParseOptions,
    ) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, Self> {
        ParseBuilder::<L, Self>::new(source).opts(opts).parse()
    }

    fn parse_with_context<'i, L: LangLexer<'i>>(
        source: L::Input,
        ctx: &ParseContext,
    ) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, Self> {
        ParseBuilder::<L, Self>::new(source).context(ctx).parse()
    }
}

/// Result of a parsing operation
pub type ParseResult<L, E, T> = Result<(T, ParseContext, L), ParseError<E>>;

/// Errors returned by the parsing operation
pub type ParseError<E> = lang_util::error::ParseError<E>;

/// Default lexer to use for parsing sources
#[cfg(all(
    feature = "lexer-v1",
    not(any(feature = "lexer-v2-min", feature = "lexer-v2-full"))
))]
pub type DefaultLexer<'i> = glsl_lang_lexer::v1::Lexer<'i>;

/// Default lexer to use for parsing sources
#[cfg(all(feature = "lexer-v2-min", not(feature = "lexer-v2-full")))]
pub type DefaultLexer<'i> = glsl_lang_lexer::v2_min::str::Lexer<'i>;

/// Default lexer to use for parsing sources
#[cfg(feature = "lexer-v2-full")]
pub type DefaultLexer<'i> = glsl_lang_lexer::v2_full::str::Lexer<'i>;

/// GLSL parsing with the default lexer
#[cfg(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
))]
pub trait DefaultParse: Parse {
    /// Parse the input source
    fn parse<'i>(
        source: <DefaultLexer<'i> as LangLexer<'i>>::Input,
    ) -> Result<Self, ParseError<<<DefaultLexer<'i> as LangLexer<'i>>::Iter as HasLexerError>::Error>>;

    /// Parse the input source with the given options
    fn parse_with_options<'i>(
        source: <DefaultLexer<'i> as LangLexer<'i>>::Input,
        opts: &ParseOptions,
    ) -> ParseResult<
        <DefaultLexer<'i> as LangLexer<'i>>::Iter,
        <<DefaultLexer<'i> as LangLexer<'i>>::Iter as HasLexerError>::Error,
        Self,
    >;

    /// Parse the input source with the given context
    fn parse_with_context<'i>(
        source: <DefaultLexer<'i> as LangLexer<'i>>::Input,
        ctx: &ParseContext,
    ) -> ParseResult<
        <DefaultLexer<'i> as LangLexer<'i>>::Iter,
        <<DefaultLexer<'i> as LangLexer<'i>>::Iter as HasLexerError>::Error,
        Self,
    >;
}

#[cfg(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
))]
impl<T: Parse> DefaultParse for T {
    fn parse<'i>(
        source: <DefaultLexer<'i> as LangLexer<'i>>::Input,
    ) -> Result<Self, ParseError<<<DefaultLexer<'i> as LangLexer<'i>>::Iter as HasLexerError>::Error>>
    {
        <T as Parse>::parse::<DefaultLexer<'i>>(source)
    }

    fn parse_with_options<'i>(
        source: <DefaultLexer<'i> as LangLexer<'i>>::Input,
        opts: &ParseOptions,
    ) -> ParseResult<
        <DefaultLexer<'i> as LangLexer<'i>>::Iter,
        <<DefaultLexer<'i> as LangLexer<'i>>::Iter as HasLexerError>::Error,
        Self,
    > {
        <T as Parse>::parse_with_options::<DefaultLexer<'i>>(source, opts)
    }

    fn parse_with_context<'i>(
        source: <DefaultLexer<'i> as LangLexer<'i>>::Input,
        ctx: &ParseContext,
    ) -> ParseResult<
        <DefaultLexer<'i> as LangLexer<'i>>::Iter,
        <<DefaultLexer<'i> as LangLexer<'i>>::Iter as HasLexerError>::Error,
        Self,
    > {
        <T as Parse>::parse_with_context::<DefaultLexer<'i>>(source, ctx)
    }
}

macro_rules! impl_parse {
    ($t:ty => $p:ty) => {
        impl LangParser for $p {
            type Item = $t;

            fn new() -> Self {
                <$p>::new()
            }

            fn parse<
                L: HasLexerError
                    + Iterator<Item = Result<(LexerPosition, Token, LexerPosition), L::Error>>,
            >(
                &self,
                ctx: ParseContext,
                input: &mut L,
            ) -> Result<Self::Item, lalrpop_util::ParseError<LexerPosition, Token, L::Error>> {
                self.parse::<L, _, _>(&ctx, input)
            }
        }

        impl HasParser for $t {
            type Parser = $p;
        }
    };
}

#[cfg(feature = "parser-expr")]
impl_parse!(ast::Expr            => parser::ExprParser);
#[cfg(feature = "parser-statement")]
impl_parse!(ast::Statement       => parser::StatementParser);
impl_parse!(ast::TranslationUnit => parser::TranslationUnitParser);
