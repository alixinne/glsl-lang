use crate::{
    ast,
    parse::{self, ParseContext, ParseError},
};

/// A parsable is something we can parse either directly, or embedded in some other syntax
/// structure.
///
/// This allows us to parse specific AST items even though we don't export a LALR parser for it.
/// Due to the way it is currently implemented, we have to generate extra code around the input,
/// thus, if you are matching on span positions, you will get a different result than if using the
/// parser directly.
pub trait Parsable: Sized {
    /// Parse the input source
    fn parse(source: &str) -> Result<Self, ParseError> {
        <Self as Parsable>::parse_with_options(source, &Default::default())
            .map(|(parsed, _names)| parsed)
    }

    /// Parse the input source with the given options
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError>;
}

impl<T: parse::Parse> Parsable for T {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        <Self as parse::Parse>::parse_with_options(source, opts)
    }
}

impl Parsable for ast::FunctionDefinition {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        match ast::TranslationUnit::parse_with_options(source, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content: ast::ExternalDeclarationData::FunctionDefinition(fndef),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((fndef, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::UnaryOp {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ {}x; }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::StatementData::Expression(ast::ExprStatement {
                        content:
                            ast::ExprStatementData(Some(ast::Expr {
                                content: ast::ExprData::Unary(u, _),
                                ..
                            })),
                        ..
                    }) = statement_list.into_iter().next().unwrap().into_inner()
                    {
                        return Ok((u, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::AssignmentOp {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ x {} 2; }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::StatementData::Expression(ast::ExprStatement {
                        content:
                            ast::ExprStatementData(Some(ast::Expr {
                                content: ast::ExprData::Assignment(_, o, _),
                                ..
                            })),
                        ..
                    }) = statement_list.into_iter().next().unwrap().into_inner()
                    {
                        return Ok((o, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

macro_rules! impl_parsable_statement {
    ($i:ident => $t:ty) => {
        impl Parsable for $t {
            fn parse_with_options(
                source: &str,
                opts: &ParseContext,
            ) -> Result<(Self, ParseContext), ParseError> {
                let src = format!("void main() {{ {} }}", source);
                match ast::TranslationUnit::parse_with_options(&src, opts) {
                    Ok((ast::TranslationUnit(extdecls), oo)) => {
                        if let ast::Node {
                            content:
                                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                                    content:
                                        ast::FunctionDefinitionData {
                                            statement:
                                                ast::Node {
                                                    content:
                                                        ast::CompoundStatementData {
                                                            statement_list,
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        } = extdecls.into_iter().next().unwrap()
                        {
                            if let ast::StatementData::$i(expr) =
                                statement_list.into_iter().next().unwrap().into_inner()
                            {
                                return Ok((expr, oo));
                            }
                        }
                    }
                    Err(error) => {
                        return Err(error);
                    }
                }

                panic!("invalid parsable result");
            }
        }
    };
}

impl_parsable_statement!(Expression => ast::ExprStatement);
impl_parsable_statement!(Selection => ast::SelectionStatement);
impl_parsable_statement!(Switch => ast::SwitchStatement);
impl_parsable_statement!(CaseLabel => ast::CaseLabel);
impl_parsable_statement!(Iteration => ast::IterationStatement);
impl_parsable_statement!(Jump => ast::JumpStatement);
impl_parsable_statement!(Compound => ast::CompoundStatement);

impl Parsable for ast::ArraySpecifierDimension {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ vec2{}(); }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::StatementData::Expression(ast::ExprStatement {
                        content:
                            ast::ExprStatementData(Some(ast::Expr {
                                content:
                                    ast::ExprData::FunCall(
                                        ast::FunIdentifier {
                                            content:
                                                ast::FunIdentifierData::TypeSpecifier(type_specifier),
                                            ..
                                        },
                                        _,
                                    ),
                                ..
                            })),
                        ..
                    }) = statement_list.into_iter().next().unwrap().into_inner()
                    {
                        if let ast::TypeSpecifier {
                            content:
                                ast::TypeSpecifierData {
                                    array_specifier:
                                        Some(ast::ArraySpecifier { content: array, .. }),
                                    ..
                                },
                            ..
                        } = *type_specifier
                        {
                            return Ok((array.dimensions.into_iter().next().unwrap(), oo));
                        }
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::ArraySpecifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ vec2{}(); }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::StatementData::Expression(ast::ExprStatement {
                        content:
                            ast::ExprStatementData(Some(ast::Expr {
                                content:
                                    ast::ExprData::FunCall(
                                        ast::FunIdentifier {
                                            content:
                                                ast::FunIdentifierData::TypeSpecifier(type_specifier),
                                            ..
                                        },
                                        _,
                                    ),
                                ..
                            })),
                        ..
                    }) = statement_list.into_iter().next().unwrap().into_inner()
                    {
                        if let ast::TypeSpecifier {
                            content:
                                ast::TypeSpecifierData {
                                    array_specifier: Some(array),
                                    ..
                                },
                            ..
                        } = *type_specifier
                        {
                            return Ok((array, oo));
                        }
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::FunIdentifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ {}(); }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::StatementData::Expression(ast::ExprStatement {
                        content:
                            ast::ExprStatementData(Some(ast::Expr {
                                content: ast::ExprData::FunCall(fi, _),
                                ..
                            })),
                        ..
                    }) = statement_list.into_iter().next().unwrap().into_inner()
                    {
                        return Ok((fi, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::InterpolationQualifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} float x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            qualifier:
                                                                                Some(ast::TypeQualifier {
                                                                                    content:
                                                                                        ast::TypeQualifierData {
                                                                                            qualifiers,
                                                                                        },
                                                                                    ..
                                                                                }),
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::TypeQualifierSpecData::Interpolation(interp) =
                        qualifiers.into_iter().next().unwrap().content
                    {
                        return Ok((interp, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::ArrayedIdentifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("uniform Block {{ float x; }} {};", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::Block(ast::Block {
                                    content:
                                        ast::BlockData {
                                            identifier: Some(a),
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((a, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::PrecisionQualifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} float x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            qualifier:
                                                                                Some(ast::TypeQualifier {
                                                                                    content:
                                                                                        ast::TypeQualifierData {
                                                                                            qualifiers,
                                                                                        },
                                                                                    ..
                                                                                }),
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::TypeQualifierSpecData::Precision(q) = qualifiers.into_iter().next().unwrap().content {
                        return Ok((q, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::StorageQualifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} float x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            qualifier:
                                                                                Some(ast::TypeQualifier {
                                                                                    content:
                                                                                        ast::TypeQualifierData {
                                                                                            qualifiers,
                                                                                        },
                                                                                    ..
                                                                                }),
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::TypeQualifierSpecData::Storage(q) = qualifiers.into_iter().next().unwrap().content {
                        return Ok((q, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::LayoutQualifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} float x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            qualifier:
                                                                                Some(ast::TypeQualifier {
                                                                                    content:
                                                                                        ast::TypeQualifierData {
                                                                                            qualifiers,
                                                                                        },
                                                                                    ..
                                                                                }),
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::TypeQualifierSpecData::Layout(q) = qualifiers.into_iter().next().unwrap().content {
                        return Ok((q, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::TypeQualifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} float x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            qualifier: Some(q),
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((q, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::TypeSpecifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            ty,
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((ty, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::TypeSpecifierNonArray {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content:
                                                        ast::SingleDeclarationData {
                                                            ty:
                                                                ast::FullySpecifiedType {
                                                                    content:
                                                                        ast::FullySpecifiedTypeData {
                                                                            ty:
                                                                                ast::TypeSpecifier {
                                                                                    content:
                                                                                        ast::TypeSpecifierData {
                                                                                            ty, ..
                                                                                        },
                                                                                    ..
                                                                                },
                                                                            ..
                                                                        },
                                                                    ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((ty, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::FullySpecifiedType {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{} x;", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content:
                                        ast::InitDeclaratorListData {
                                            head:
                                                ast::SingleDeclaration {
                                                    content: ast::SingleDeclarationData { ty, .. },
                                                    ..
                                                },
                                            ..
                                        },
                                    ..
                                }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((ty, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::Declaration {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{};", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content: ast::ExternalDeclarationData::Declaration(decl),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((decl, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::StructFieldSpecifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("struct A {{ {} }};", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content: ast::InitDeclaratorListData {
                                    head:
                                        ast::SingleDeclaration {
                                            content: ast::SingleDeclarationData {
                                            ty:
                                                ast::FullySpecifiedType {
                                                    content: ast::FullySpecifiedTypeData {
                                                        ty:
                                                        ast::TypeSpecifier {
                                                            content: ast::TypeSpecifierData {
                                                                ty:
                                                                    ast::TypeSpecifierNonArray {
                                                                        content: ast::TypeSpecifierNonArrayData::Struct(
                                                                             ast::StructSpecifier {
                                                                                 content: ast::StructSpecifierData {
                                                                                     fields,
                                                                                     ..
                                                                                 },
                                                                                 ..
                                                                             },
                                                                         ),
                                                                         ..
                                                                    },
                                                                    ..
                                                            },
                                                            ..
                                                        },
                                                        ..
                                                    },
                                                    ..
                                                },
                                            ..
                                        },
                                        ..
                                        },
                                    ..
                                },
                                    .. }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((fields.into_iter().next().unwrap(), oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

impl Parsable for ast::StructSpecifier {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("{};", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::Declaration(ast::Node {
                            content:
                                ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
                                    content: ast::InitDeclaratorListData {
                                    head:
                                        ast::SingleDeclaration {
                                            content: ast::SingleDeclarationData {
                                            ty:
                                                ast::FullySpecifiedType {
                                                    content: ast::FullySpecifiedTypeData {
                                                        ty:
                                                        ast::TypeSpecifier {
                                                            content: ast::TypeSpecifierData {
                                                                ty:
                                                                    ast::TypeSpecifierNonArray {
                                                                        content: ast::TypeSpecifierNonArrayData::Struct(s),
                                                                        ..
                                                                    },
                                                                ..
                                                                },
                                                            ..
                                                        },
                                                    ..
                                                    },
                                                    ..
                                                },
                                            ..
                                        },
                                        ..
                                        },
                                    ..
                                },
                                    .. }),
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((s, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

#[cfg(not(feature = "parser-expr"))]
impl Parsable for ast::Expr {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ {}; }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    if let ast::StatementData::Expression(ast::ExprStatement {
                        content: ast::ExprStatementData(Some(expr)),
                        ..
                    }) = statement_list.into_iter().next().unwrap().into_inner()
                    {
                        return Ok((expr, oo));
                    }
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

#[cfg(not(feature = "parser-preprocessor"))]
impl Parsable for ast::Preprocessor {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        match ast::TranslationUnit::parse_with_options(source, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content: ast::ExternalDeclarationData::Preprocessor(pp),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((pp, oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}

#[cfg(not(feature = "parser-statement"))]
impl Parsable for ast::Statement {
    fn parse_with_options(
        source: &str,
        opts: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError> {
        let src = format!("void main() {{ {} }}", source);
        match ast::TranslationUnit::parse_with_options(&src, opts) {
            Ok((ast::TranslationUnit(extdecls), oo)) => {
                if let ast::Node {
                    content:
                        ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                            content:
                                ast::FunctionDefinitionData {
                                    statement:
                                        ast::Node {
                                            content:
                                                ast::CompoundStatementData { statement_list, .. },
                                            ..
                                        },
                                    ..
                                },
                            ..
                        }),
                    ..
                } = extdecls.into_iter().next().unwrap()
                {
                    return Ok((statement_list.into_iter().next().unwrap(), oo));
                }
            }
            Err(error) => {
                return Err(error);
            }
        }

        panic!("invalid parsable result");
    }
}
