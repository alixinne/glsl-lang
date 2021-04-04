//! Parsing utilities and entry points

use crate::{ast, lexer::Lexer, parser};

pub use crate::lexer::{LexerPosition, LexicalError, Token};

mod parsable;
pub use parsable::Parsable;

mod context;
pub use context::*;

/// GLSL parsing error
pub type ParseError = lang_util::error::ParseError<LexicalError>;

/// GLSL language parser
pub trait LangParser: Sized {
    /// Instantiate the parser
    fn new() -> Self;
}

/// GLSL language parsing capability
pub trait Parse: Sized {
    /// Type of the parser to create
    type Parser: LangParser;

    /// Parse the input source
    fn parse(source: &str) -> Result<Self, ParseError> {
        <Self as Parse>::parse_with_options(source, &Default::default())
            .map(|(parsed, _names)| parsed)
    }

    /// Parse the input source with the given options
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError>;

    /// Parse the input source with the given options and already instantiated parser
    fn parse_with_parser<'i>(
        source: &'i str,
        opts: &ParseContext,
        parser: &Self::Parser,
    ) -> Result<(Self, ParseContext), ParseError>;
}

macro_rules! impl_parse {
    ($t:ty => $p:ty) => {
        impl LangParser for $p {
            fn new() -> Self {
                <$p>::new()
            }
        }

        impl Parse for $t {
            type Parser = $p;

            fn parse_with_options<'i>(
                source: &'i str,
                opts: &ParseContext,
            ) -> Result<(Self, ParseContext), ParseError> {
                let parser = <$p>::new();
                Self::parse_with_parser(source, opts, &parser)
            }

            fn parse_with_parser<'i>(
                source: &'i str,
                opts: &ParseContext,
                parser: &$p,
            ) -> Result<(Self, ParseContext), ParseError> {
                // Clone the input structure
                let cloned_opts = opts.clone_inner();

                // Invoke the parser
                let lexer = Lexer::new(source, cloned_opts.clone());
                parser
                    .parse(source, lexer)
                    .map(|parsed| (parsed, cloned_opts))
                    .map_err(|err| ParseError::new(err, source))
            }
        }
    };
}

#[cfg(feature = "parser-expr")]
impl_parse!(ast::Expr            => parser::ExprParser);
#[cfg(feature = "parser-preprocessor")]
impl_parse!(ast::Preprocessor    => parser::PreprocessorParser);
#[cfg(feature = "parser-statement")]
impl_parse!(ast::Statement       => parser::StatementParser);
impl_parse!(ast::TranslationUnit => parser::TranslationUnitParser);
