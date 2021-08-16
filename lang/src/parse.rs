//! Parsing utilities and entry points

use crate::{
    ast,
    lexer::{self, LexerPosition},
    parser,
};

#[cfg(all(feature = "lexer-v1", not(feature = "lexer-v2")))]
use lexer::v1::Lexer;
#[cfg(feature = "lexer-v2")]
use lexer::v2::str::Lexer;

mod parsable;
#[cfg(feature = "lexer")]
pub use parsable::Parsable;

mod context;
pub use context::*;

/// GLSL lexer builder
pub trait IntoLexer {
    /// Type of the resulting lexer
    type Lexer: LangLexer;

    /// Instantiate the lexer
    fn into_lexer(
        self,
        source: <Self::Lexer as LangLexer>::Input,
        opts: ParseContext,
    ) -> Self::Lexer;
}

#[cfg(feature = "lexer")]
impl<'i> IntoLexer for &'i str {
    type Lexer = Lexer<'i>;

    fn into_lexer(self, source: &'i str, opts: ParseContext) -> Self::Lexer {
        Lexer::new(source, opts)
    }
}

/// GLSL lexer builder extensions
pub trait IntoLexerExt: IntoLexer {
    /// Create a parse builder for this lexer template
    fn builder<'o, 'p, T>(self) -> ParseBuilder<'o, 'p, <Self as IntoLexer>::Lexer, T>
    where
        T: HasParser<<Self as IntoLexer>::Lexer>,
        <Self as IntoLexer>::Lexer: LangLexer<Input = Self>;
}

impl<I: IntoLexer> IntoLexerExt for I {
    fn builder<'o, 'p, T>(self) -> ParseBuilder<'o, 'p, <Self as IntoLexer>::Lexer, T>
    where
        T: HasParser<<Self as IntoLexer>::Lexer>,
        <Self as IntoLexer>::Lexer: LangLexer<Input = Self>,
    {
        ParseBuilder::new(self)
    }
}

/// GLSL language lexer
pub trait LangLexer: Sized {
    /// Type of the input for this lexer
    type Input;
    /// Type of lexical analysis error
    type Error: lang_util::error::LexicalError;

    /// Instantiate the lexer
    fn new(source: Self::Input, opts: ParseContext) -> Self;

    /// Run the given parser with this lexer as input
    fn chain<P: LangParser<Self>>(self, parser: &P) -> Result<P::Item, ParseError<Self>>;
}

impl<T: LangLexer> IntoLexer for T {
    type Lexer = Self;

    fn into_lexer(
        self,
        source: <Self::Lexer as LangLexer>::Input,
        opts: ParseContext,
    ) -> Self::Lexer {
        Self::new(source, opts)
    }
}

/// GLSL language parser
pub trait LangParser<L: LangLexer>: Sized {
    /// AST node returned by this parser
    type Item;

    /// Instantiate the parser
    fn new() -> Self;

    /// Parse the input
    fn parse(
        &self,
        source: &str,
        input: L,
    ) -> Result<Self::Item, lalrpop_util::ParseError<LexerPosition, lexer::Token, L::Error>>;
}

/// GLSL language parsing capability
pub trait HasParser<L: LangLexer>: Sized {
    /// Type of the parser to create
    type Parser: LangParser<L, Item = Self>;
}

/// GLSL language parsing functions
#[cfg(feature = "lexer")]
pub trait Parse<'i>: HasParser<Lexer<'i>> {
    /// Parse the input source
    fn parse(source: &'i str) -> Result<Self, ParseError<Lexer<'i>>>;

    /// Parse the input source with the given options
    fn parse_with_options(source: &'i str, opts: &ParseContext) -> ParseResult<Lexer<'i>, Self>;

    /// Parse the input source with the given options and already instantiated parser
    fn parse_with_parser(
        source: &'i str,
        opts: &ParseContext,
        parser: &Self::Parser,
    ) -> ParseResult<Lexer<'i>, Self>;
}

#[cfg(feature = "lexer")]
impl<'i, T: HasParser<Lexer<'i>>> Parse<'i> for T {
    fn parse(source: &'i str) -> Result<Self, ParseError<Lexer<'i>>> {
        ParseBuilder::<Lexer, Self>::new(source)
            .parse()
            .map(|(parsed, _names)| parsed)
    }

    fn parse_with_options(source: &'i str, opts: &ParseContext) -> ParseResult<Lexer<'i>, Self> {
        ParseBuilder::<Lexer, Self>::new(source).opts(opts).parse()
    }

    fn parse_with_parser(
        source: &'i str,
        opts: &ParseContext,
        parser: &Self::Parser,
    ) -> ParseResult<Lexer<'i>, Self> {
        ParseBuilder::<Lexer, Self>::new(source)
            .opts(opts)
            .parser(parser)
            .parse()
    }
}

/// Builder structure for a parsing operation
pub struct ParseBuilder<'o, 'p, L: IntoLexer, T: HasParser<L::Lexer>> {
    source: <L::Lexer as LangLexer>::Input,
    opts: Option<&'o ParseContext>,
    lexer: Option<L>,
    parser: Option<&'p T::Parser>,
}

/// Result of a parsing operation
pub type ParseResult<L, T> = Result<(T, ParseContext), ParseError<L>>;

/// Errors returned by the parsing operation
pub type ParseError<L> =
    lang_util::error::ParseError<<<L as IntoLexer>::Lexer as LangLexer>::Error>;

impl<'o, 'p, L: IntoLexer, T: HasParser<L::Lexer>> ParseBuilder<'o, 'p, L, T> {
    /// Create a new parse builder from the given input string
    pub fn new(source: <L::Lexer as LangLexer>::Input) -> Self {
        Self {
            source,
            opts: None,
            lexer: None,
            parser: None,
        }
    }

    /// Set the parse context for this parse
    pub fn opts(self, opts: &'o ParseContext) -> Self {
        Self {
            opts: Some(opts),
            ..self
        }
    }

    /// Set the parser instance to use for this parse
    pub fn parser(self, parser: &'p T::Parser) -> Self {
        Self {
            parser: Some(parser),
            ..self
        }
    }

    /// Execute the parsing operation
    pub fn parse(mut self) -> ParseResult<L, T> {
        let source = self.source;

        // Clone the input context, or create a new one
        let cloned_opts = if let Some(existing) = self.opts.take() {
            existing.clone_inner()
        } else {
            Default::default()
        };

        // Create the lexer
        let lexer = if let Some(lexer) = self.lexer.take() {
            lexer.into_lexer(source, cloned_opts.clone())
        } else {
            L::Lexer::new(source, cloned_opts.clone())
        };

        // Create the parser
        let created_parser;
        let parser = if let Some(parser) = self.parser.take() {
            parser
        } else {
            created_parser = <T as HasParser<L::Lexer>>::Parser::new();
            &created_parser
        };

        // Invoke the parser
        lexer.chain(parser).map(|parsed| (parsed, cloned_opts))
    }
}

macro_rules! impl_parse {
    ($t:ty => $p:ty) => {
        impl<
                L: LangLexer
                    + IntoIterator<
                        Item = Result<(LexerPosition, lexer::Token, LexerPosition), L::Error>,
                    >,
            > LangParser<L> for $p
        {
            type Item = $t;

            fn new() -> Self {
                <$p>::new()
            }

            fn parse(
                &self,
                source: &str,
                input: L,
            ) -> Result<Self::Item, lalrpop_util::ParseError<LexerPosition, lexer::Token, L::Error>>
            {
                self.parse::<L, _, _>(source, input)
            }
        }

        impl<
                L: LangLexer
                    + IntoIterator<
                        Item = Result<(LexerPosition, lexer::Token, LexerPosition), L::Error>,
                    >,
            > HasParser<L> for $t
        {
            type Parser = $p;
        }
    };
}

#[cfg(feature = "parser-expr")]
impl_parse!(ast::Expr            => parser::ExprParser);
#[cfg(feature = "parser-statement")]
impl_parse!(ast::Statement       => parser::StatementParser);
impl_parse!(ast::TranslationUnit => parser::TranslationUnitParser);
