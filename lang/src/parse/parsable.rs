use std::borrow::Cow;

use crate::ast;

#[cfg(any(feature = "lexer-v1", feature = "lexer-v2-full"))]
use super::{DefaultLexer, HasLexerError, ParseContext, ParseError, ParseOptions};

/// A parsable is something we can parse either directly, or embedded in some other syntax
/// structure.
///
/// This allows us to parse specific AST items even though we don't export a LALR parser for it.
/// Due to the way it is currently implemented, we have to generate extra code around the input,
/// thus, if you are matching on span positions, you will get a different result than if using the
/// parser directly.
#[cfg(any(feature = "lexer-v1", feature = "lexer-v2-full"))]
pub trait Parsable: Sized {
    /// Parse the input source
    fn parse(source: &str) -> Result<Self, ParseError<<DefaultLexer as HasLexerError>::Error>> {
        <Self as Parsable>::parse_with_options(source, &Default::default())
            .map(|(parsed, _names)| parsed)
    }

    /// Parse the input source with the given options
    fn parse_with_options<'i>(
        source: &'i str,
        opts: &ParseOptions,
    ) -> Result<(Self, ParseContext), ParseError<<DefaultLexer<'i> as HasLexerError>::Error>>;

    /// Parse the input source with the given context
    fn parse_with_context<'i>(
        source: &'i str,
        ctx: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError<<DefaultLexer<'i> as HasLexerError>::Error>>;
}

#[cfg(any(feature = "lexer-v1", feature = "lexer-v2-full"))]
impl<T: Extractable<ast::TranslationUnit>> Parsable for T {
    fn parse_with_options<'i>(
        source: &'i str,
        opts: &ParseOptions,
    ) -> Result<(Self, ParseContext), ParseError<<DefaultLexer<'i> as HasLexerError>::Error>> {
        <ast::TranslationUnit as super::DefaultParse>::parse_with_options(&Self::wrap(source), opts)
            .map(|(tu, oo, _lexer)| (Self::extract(tu).expect("invalid parse result"), oo))
    }

    fn parse_with_context<'i>(
        source: &'i str,
        ctx: &ParseContext,
    ) -> Result<(Self, ParseContext), ParseError<<DefaultLexer<'i> as HasLexerError>::Error>> {
        <ast::TranslationUnit as super::DefaultParse>::parse_with_context(&Self::wrap(source), ctx)
            .map(|(tu, oo, _lexer)| (Self::extract(tu).expect("invalid parse result"), oo))
    }
}

/// Part of the syntax tree that can be extracted from a parent tree
pub trait Extractable<R>: Sized {
    /// Wrap the given source which parses as Self into something that parses as R
    fn wrap(source: &str) -> Cow<str>;
    /// Extract the subtree for Self from a parent tree R
    fn extract(tu: R) -> Option<Self>;
}

impl Extractable<ast::TranslationUnit> for ast::TranslationUnit {
    fn wrap(source: &str) -> Cow<str> {
        source.into()
    }

    fn extract(tu: ast::TranslationUnit) -> Option<Self> {
        Some(tu)
    }
}

impl Extractable<ast::TranslationUnit> for ast::FunctionDefinition {
    fn wrap(source: &str) -> Cow<str> {
        source.into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content: ast::ExternalDeclarationData::FunctionDefinition(fndef),
            ..
        } = extdecls.into_iter().next().unwrap()
        {
            return Some(fndef);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::UnaryOp {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ {}x; }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
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
                return Some(u);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::AssignmentOp {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ x {} 2; }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
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
                return Some(o);
            }
        }

        None
    }
}

macro_rules! impl_parsable_statement {
    ($i:ident => $t:ty) => {
        impl Extractable<ast::TranslationUnit> for $t {
            fn wrap(source: &str) -> Cow<str> {
                format!("void main() {{ {} }}", source).into()
            }

            fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
                    if let ast::StatementData::$i(expr) =
                        statement_list.into_iter().next().unwrap().into_inner()
                    {
                        return Some(expr);
                    }
                }

                None
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

impl Extractable<ast::TranslationUnit> for ast::ArraySpecifierDimension {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ vec2{}(); }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
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
                                    content: ast::FunIdentifierData::TypeSpecifier(type_specifier),
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
                            array_specifier: Some(ast::ArraySpecifier { content: array, .. }),
                            ..
                        },
                    ..
                } = *type_specifier
                {
                    return array.dimensions.into_iter().next();
                }
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::ArraySpecifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ vec2{}(); }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
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
                                    content: ast::FunIdentifierData::TypeSpecifier(type_specifier),
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
                    return Some(array);
                }
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::FunIdentifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ {}(); }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
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
                return Some(fi);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::InterpolationQualifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} float x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
                return Some(interp);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::ArrayedIdentifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("uniform Block {{ float x; }} {};", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            return Some(a);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::PrecisionQualifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} float x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            if let ast::TypeQualifierSpecData::Precision(q) =
                qualifiers.into_iter().next().unwrap().content
            {
                return Some(q);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::StorageQualifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} float x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            if let ast::TypeQualifierSpecData::Storage(q) =
                qualifiers.into_iter().next().unwrap().content
            {
                return Some(q);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::LayoutQualifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} float x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            if let ast::TypeQualifierSpecData::Layout(q) =
                qualifiers.into_iter().next().unwrap().content
            {
                return Some(q);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::TypeQualifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} float x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            return Some(q);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::TypeSpecifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
                        }),
                    ..
                }),
            ..
        } = extdecls.into_iter().next().unwrap()
        {
            return Some(ty);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::TypeSpecifierNonArray {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            return Some(ty);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::FullySpecifiedType {
    fn wrap(source: &str) -> Cow<str> {
        format!("{} x;", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            return Some(ty);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::Declaration {
    fn wrap(source: &str) -> Cow<str> {
        format!("{};", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content: ast::ExternalDeclarationData::Declaration(decl),
            ..
        } = extdecls.into_iter().next().unwrap()
        {
            return Some(decl);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::StructFieldSpecifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("struct A {{ {} }};", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            return fields.into_iter().next();
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::StructSpecifier {
    fn wrap(source: &str) -> Cow<str> {
        format!("{};", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
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
            return Some(s);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::Expr {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ {}; }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
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
                return Some(expr);
            }
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::Preprocessor {
    fn wrap(source: &str) -> Cow<str> {
        source.into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content: ast::ExternalDeclarationData::Preprocessor(pp),
            ..
        } = extdecls.into_iter().next().unwrap()
        {
            return Some(pp);
        }

        None
    }
}

impl Extractable<ast::TranslationUnit> for ast::Statement {
    fn wrap(source: &str) -> Cow<str> {
        format!("void main() {{ {} }}", source).into()
    }

    fn extract(ast::TranslationUnit(extdecls): ast::TranslationUnit) -> Option<Self> {
        if let ast::Node {
            content:
                ast::ExternalDeclarationData::FunctionDefinition(ast::Node {
                    content:
                        ast::FunctionDefinitionData {
                            statement:
                                ast::Node {
                                    content: ast::CompoundStatementData { statement_list, .. },
                                    ..
                                },
                            ..
                        },
                    ..
                }),
            ..
        } = extdecls.into_iter().next().unwrap()
        {
            return statement_list.into_iter().next();
        }

        None
    }
}
