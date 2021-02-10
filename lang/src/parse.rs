use crate::{ast, lexer::Lexer, parser};

pub use crate::lexer::{
    IdentifierContext, LexicalError, Token, TokenKind, TypeNames, TypeTablePolicy,
};

mod parsable;
pub use parsable::Parsable;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ParseOptions {
    pub target_vulkan: bool,
    pub source_id: usize,
    pub type_names: TypeNames,
}

impl ParseOptions {
    pub fn new() -> Self {
        Self::default()
    }
}

pub type ParseError<'i> = lalrpop_util::ParseError<(usize, usize), Token<'i>, LexicalError>;
pub type ParseErrorStatic = lalrpop_util::ParseError<(usize, usize), TokenKind, LexicalError>;

pub trait Parse: Sized {
    fn parse(source: &str) -> Result<Self, ParseError> {
        <Self as Parse>::parse_with_options(source, &Default::default())
            .map(|(parsed, _names)| parsed)
    }

    fn parse_with_options<'i>(
        source: &'i str,
        opts: &ParseOptions,
    ) -> Result<(Self, TypeNames), ParseError<'i>>;
}

macro_rules! impl_parse {
    ($t:ty => $p:ty) => {
        impl Parse for $t {
            fn parse_with_options<'i>(
                source: &'i str,
                opts: &ParseOptions,
            ) -> Result<(Self, TypeNames), ParseError<'i>> {
                // Clone the input structure
                let cloned_opts = opts.clone();

                // Clone the type names
                let cloned_type_names = cloned_opts.type_names.clone_inner();
                let cloned_opts = ParseOptions {
                    type_names: cloned_type_names.clone(),
                    ..cloned_opts
                };

                // Invoke the parser
                let lexer = Lexer::new(source, cloned_opts);
                let parser = <$p>::new();
                parser
                    .parse(source, lexer)
                    .map(|parsed| (parsed, cloned_type_names))
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
