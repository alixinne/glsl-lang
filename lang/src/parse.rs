//! Parsing utilities and entry points

use crate::{
    ast,
    lexer::{self, v1::Lexer},
    parser,
};

pub use crate::lexer::{LexerPosition, Token};

mod parsable;
pub use parsable::Parsable;

mod context;
pub use context::*;

/// GLSL lexer builder
pub trait IntoLexer<'i> {
    /// Type of the resulting lexer
    type Lexer: LangLexer<'i>;

    /// Instantiate the lexer
    fn into_lexer(self, source: &'i str, opts: ParseContext) -> Self::Lexer;
}

/// GLSL language lexer
pub trait LangLexer<'i>:
    IntoIterator<Item = (LexerPosition, lexer::Token<'i>, LexerPosition)> + Sized
{
    /// Type of lexical analysis error
    type Error: lang_util::error::LexicalError;

    /// Instantiate the lexer
    fn new(source: &'i str, opts: ParseContext) -> Self;
}

impl<'i, T: LangLexer<'i>> IntoLexer<'i> for T {
    type Lexer = Self;

    fn into_lexer(self, source: &'i str, opts: ParseContext) -> Self::Lexer {
        Self::new(source, opts)
    }
}

/// GLSL language parser
pub trait LangParser<'i, L: LangLexer<'i>>: Sized {
    /// AST node returned by this parser
    type Item;

    /// Instantiate the parser
    fn new() -> Self;

    /// Parse the input
    fn parse(
        &self,
        source: &'i str,
        input: L,
    ) -> Result<Self::Item, lalrpop_util::ParseError<LexerPosition, lexer::Token<'i>, L::Error>>;
}

/// GLSL language parsing capability
pub trait Parse<'i>: Sized {
    /// Type of the parser to create
    type Parser: LangParser<'i, Lexer<'i>, Item = Self>;

    /// Parse the input source
    fn parse(source: &'i str) -> Result<Self, ParseError<'i, Lexer<'i>>> {
        ParseBuilder::<Lexer, Self::Parser>::new(source)
            .parse()
            .map(|(parsed, _names)| parsed)
    }

    /// Parse the input source with the given options
    fn parse_with_options(
        source: &'i str,
        opts: &ParseContext,
    ) -> ParseResult<'i, Lexer<'i>, Self::Parser> {
        ParseBuilder::<Lexer, Self::Parser>::new(source)
            .opts(opts)
            .parse()
    }

    /// Parse the input source with the given options and already instantiated parser
    fn parse_with_parser(
        source: &'i str,
        opts: &ParseContext,
        parser: &Self::Parser,
    ) -> ParseResult<'i, Lexer<'i>, Self::Parser> {
        ParseBuilder::<Lexer, Self::Parser>::new(source)
            .opts(opts)
            .parser(parser)
            .parse()
    }
}

/// Builder structure for a parsing operation
pub struct ParseBuilder<'i, 'o, 'p, L: IntoLexer<'i>, P: LangParser<'i, L::Lexer> + 'p> {
    source: &'i str,
    opts: Option<&'o ParseContext>,
    lexer: Option<L>,
    parser: Option<&'p P>,
}

/// Result of a parsing operation
pub type ParseResult<'i, L, P> = Result<
    (
        <P as LangParser<'i, <L as IntoLexer<'i>>::Lexer>>::Item,
        ParseContext,
    ),
    ParseError<'i, L>,
>;

/// Errors returned by the parsing operation
pub type ParseError<'i, L> =
    lang_util::error::ParseError<<<L as IntoLexer<'i>>::Lexer as LangLexer<'i>>::Error>;

impl<'i, 'o, 'p, L: IntoLexer<'i>, P: LangParser<'i, L::Lexer> + 'p>
    ParseBuilder<'i, 'o, 'p, L, P>
{
    /// Create a new parse builder from the given input string
    pub fn new(source: &'i str) -> Self {
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
    pub fn parser(self, parser: &'p P) -> Self {
        Self {
            parser: Some(parser),
            ..self
        }
    }

    /// Execute the parsing operation
    pub fn parse(mut self) -> ParseResult<'i, L, P> {
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
            created_parser = P::new();
            &created_parser
        };

        // Invoke the parser
        parser
            .parse(source, lexer)
            .map(|parsed| (parsed, cloned_opts))
            .map_err(|err| lang_util::error::ParseError::new(err, source))
    }
}

macro_rules! impl_parse {
    ($t:ty => $p:ty) => {
        impl<'i, L: LangLexer<'i>> LangParser<'i, L> for $p {
            type Item = $t;

            fn new() -> Self {
                <$p>::new()
            }

            fn parse(
                &self,
                source: &'i str,
                input: L,
            ) -> Result<
                Self::Item,
                lalrpop_util::ParseError<LexerPosition, lexer::Token<'i>, L::Error>,
            > {
                self.parse::<L, _, _>(source, input)
            }
        }

        impl Parse<'_> for $t {
            type Parser = $p;
        }
    };
}

#[cfg(feature = "parser-expr")]
impl_parse!(ast::Expr            => parser::ExprParser);
#[cfg(feature = "parser-statement")]
impl_parse!(ast::Statement       => parser::StatementParser);
impl_parse!(ast::TranslationUnit => parser::TranslationUnitParser);
