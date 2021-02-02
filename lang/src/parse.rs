use crate::lexer::Lexer;
use crate::ast;
use crate::parser;
pub use crate::lexer::{LexicalError, Token};

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct ParseOptions {
  pub target_vulkan: bool,
}

pub type ParseError<'i> = lalrpop_util::ParseError<(usize, usize), Token<'i>, LexicalError>;

pub trait Parse: Sized {
    fn parse(source: &str) -> Result<Self, ParseError> {
        <Self as Parse>::parse_with_id(source, 0)
    }

    fn parse_with_id(source: &str, id: usize) -> Result<Self, ParseError> {
      <Self as Parse>::parse_with_options(source, id, &Default::default())
    }

    fn parse_with_options<'i>(source: &'i str, id: usize, opts: &ParseOptions)  -> Result<Self, ParseError<'i>>;
}

macro_rules! impl_parse {
  ($t:ty => $p:ty) => {
    impl Parse for $t {
      fn parse_with_options<'i>(source: &'i str, id: usize, opts: &ParseOptions) -> Result<Self, ParseError<'i>> {
        let lexer = Lexer::new(source, id, opts.clone());
        let parser = <$p>::new();
        parser.parse(source, lexer)
      }
    }
  }
}

impl_parse!(ast::ArraySpecifier => parser::ArraySpecifierParser);
impl_parse!(ast::ArrayedIdentifier => parser::ArrayedIdentifierParser);
impl_parse!(ast::AssignmentOp => parser::AssignmentOpParser);
impl_parse!(ast::CompoundStatement => parser::CompoundStatementParser);
impl_parse!(ast::Declaration => parser::DeclarationParser);
impl_parse!(ast::Expr => parser::ExprParser);
impl_parse!(ast::ExternalDeclaration => parser::ExternalDeclarationParser);
impl_parse!(ast::Identifier => parser::IdentifierParser);
impl_parse!(ast::InterpolationQualifier => parser::InterpolationQualifierParser);
impl_parse!(ast::LayoutQualifier => parser::LayoutQualifierParser);
impl_parse!(ast::PrecisionQualifier => parser::PrecisionQualifierParser);
impl_parse!(ast::SimpleStatement => parser::SimpleStatementParser);
impl_parse!(ast::Statement => parser::StatementParser);
impl_parse!(ast::StorageQualifier => parser::StorageQualifierParser);
impl_parse!(ast::StructSpecifier => parser::StructSpecifierParser);
impl_parse!(ast::TranslationUnit => parser::TranslationUnitParser);
impl_parse!(ast::TypeName => parser::TypeNameParser);
impl_parse!(ast::TypeSpecifier => parser::TypeSpecifierParser);
impl_parse!(ast::TypeSpecifierNonArray => parser::TypeSpecifierNonArrayParser);
impl_parse!(ast::UnaryOp => parser::UnaryOpParser);
impl_parse!(ast::JumpStatement => parser::JumpStatementParser);
impl_parse!(ast::IterationStatement => parser::IterationStatementParser);
impl_parse!(ast::CaseLabel => parser::CaseLabelParser);
impl_parse!(ast::ArraySpecifierDimension => parser::ArraySpecifierDimensionParser);
impl_parse!(ast::TypeQualifier => parser::TypeQualifierParser);
impl_parse!(ast::StructFieldSpecifier => parser::StructFieldSpecifierParser);
impl_parse!(ast::FullySpecifiedType => parser::FullySpecifiedTypeParser);
impl_parse!(ast::FunIdentifier => parser::FunIdentifierParser);
impl_parse!(ast::ExprStatement => parser::ExprStatementParser);
impl_parse!(ast::SelectionStatement => parser::SelectionStatementParser);
impl_parse!(ast::SwitchStatement => parser::SwitchStatementParser);
impl_parse!(ast::FunctionDefinition => parser::FunctionDefinitionParser);
