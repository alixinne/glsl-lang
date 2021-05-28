//! AST visitors (i.e. on-the-fly mutation at different places in the AST).
//!
//! Visitors are mutable objects that can mutate parts of an AST while traversing it. You can see
//! them as flexible mutations taking place on *patterns* representing your AST – they get called
//! everytime an interesting node gets visited. Because of their mutable nature, you can accumulate
//! a state as you traverse the AST and implement exotic filtering.
//!
//! Visitors must implement the [`Visitor`] trait in order to be usable.
//!
//! In order to visit any part of an AST (from its very top root or from any part of it), you must
//! use the [`Host`] interface, that provides the [`Host::visit`] function.
//!
//! For instance, we can imagine visiting an AST to count how many variables are declared:
//!
//! ```
//! use glsl_lang::ast::{CompoundStatement, CompoundStatementData, ExprData, SingleDeclaration, StatementData, TypeSpecifierNonArrayData, NodeContent};
//! use glsl_lang::visitor::{Host, Visit, Visitor};
//! use std::iter::FromIterator;
//!
//! let decl0 = StatementData::declare_var(
//!   TypeSpecifierNonArrayData::Float,
//!   "x",
//!   None,
//!   Some(ExprData::from(3.14).into_node())
//! );
//!
//! let decl1 = StatementData::declare_var(
//!   TypeSpecifierNonArrayData::Int,
//!   "y",
//!   None,
//!   None
//! );
//!
//! let decl2 = StatementData::declare_var(
//!   TypeSpecifierNonArrayData::Vec4,
//!   "z",
//!   None,
//!   None
//! );
//!
//! let compound: CompoundStatement = CompoundStatementData::from_iter(
//!   vec![decl0.into(), decl1.into(), decl2.into()]
//! ).into();
//!
//! // our visitor that will count the number of variables it saw
//! struct Counter {
//!   var_nb: usize
//! }
//!
//! impl Visitor for Counter {
//!   // we are only interested in single declaration with a name
//!   fn visit_single_declaration(&mut self, declaration: &SingleDeclaration) -> Visit {
//!     if declaration.name.is_some() {
//!       self.var_nb += 1;
//!     }
//!
//!     // do not go deeper
//!     Visit::Parent
//!   }
//! }
//!
//! let mut counter = Counter { var_nb: 0 };
//! compound.visit(&mut counter);
//! assert_eq!(counter.var_nb, 3);
//! ```
//!
//! [`Host`]: crate::visitor::Host
//! [`Host::visit`]: crate::visitor::Host::visit
//! [`Visitor`]: crate::visitor::Visitor

use crate::ast;

/// Visit strategy after having visited an AST node.
///
/// Some AST nodes have *children* – in enum’s variants, in some fields as nested in [`Vec`], etc.
/// Those nodes can be visited depending on the strategy you chose.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Visit {
    /// The visitor will go deeper in the AST by visiting all the children, if any. If no children are
    /// present or if having children doesn’t make sense for a specific part of the AST, this
    /// strategy will be ignored.
    Children,
    /// The visitor won’t visit children nor siblings and will go up.
    Parent,
}

macro_rules! make_visitor_trait {
  ($t:ident, $($ref:tt)*) => {
    /// Visitor object, visiting AST nodes.
    ///
    /// This trait exists in two flavors, depending on whether you want to mutate the AST or not: [`Visitor`] doesn’t
    /// allow for mutation while [`VisitorMut`] does.
    #[allow(missing_docs)]
    pub trait $t {
      fn visit_translation_unit(&mut self, _: $($ref)* ast::TranslationUnit) -> Visit {
        Visit::Children
      }

      fn visit_external_declaration(&mut self, _: $($ref)* ast::ExternalDeclaration) -> Visit {
        Visit::Children
      }

      fn visit_identifier(&mut self, _: $($ref)* ast::Identifier) -> Visit {
        Visit::Children
      }

      fn visit_arrayed_identifier(&mut self, _: $($ref)* ast::ArrayedIdentifier) -> Visit {
        Visit::Children
      }

      fn visit_type_name(&mut self, _: $($ref)* ast::TypeName) -> Visit {
        Visit::Children
      }

      fn visit_block(&mut self, _: $($ref)* ast::Block) -> Visit {
        Visit::Children
      }

      fn visit_for_init_statement(&mut self, _: $($ref)* ast::ForInitStatement) -> Visit {
        Visit::Children
      }

      fn visit_for_rest_statement(&mut self, _: $($ref)* ast::ForRestStatement) -> Visit {
        Visit::Children
      }

      fn visit_function_definition(&mut self, _: $($ref)* ast::FunctionDefinition) -> Visit {
        Visit::Children
      }

      fn visit_function_parameter_declarator(
        &mut self,
        _: $($ref)* ast::FunctionParameterDeclarator,
      ) -> Visit {
        Visit::Children
      }

      fn visit_function_prototype(&mut self, _: $($ref)* ast::FunctionPrototype) -> Visit {
        Visit::Children
      }

      fn visit_init_declarator_list(&mut self, _: $($ref)* ast::InitDeclaratorList) -> Visit {
        Visit::Children
      }

      fn visit_layout_qualifier(&mut self, _: $($ref)* ast::LayoutQualifier) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor(&mut self, _: $($ref)* ast::Preprocessor) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_define(&mut self, _: $($ref)* ast::PreprocessorDefine) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_elseif(&mut self, _: $($ref)* ast::PreprocessorElseIf) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_error(&mut self, _: $($ref)* ast::PreprocessorError) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_extension(&mut self, _: $($ref)* ast::PreprocessorExtension) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_extension_behavior(
        &mut self,
        _: $($ref)* ast::PreprocessorExtensionBehavior,
      ) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_extension_name(
        &mut self,
        _: $($ref)* ast::PreprocessorExtensionName,
      ) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_if(&mut self, _: $($ref)* ast::PreprocessorIf) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_ifdef(&mut self, _: $($ref)* ast::PreprocessorIfDef) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_ifndef(&mut self, _: $($ref)* ast::PreprocessorIfNDef) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_include(&mut self, _: $($ref)* ast::PreprocessorInclude) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_line(&mut self, _: $($ref)* ast::PreprocessorLine) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_pragma(&mut self, _: $($ref)* ast::PreprocessorPragma) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_undef(&mut self, _: $($ref)* ast::PreprocessorUndef) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_version(&mut self, _: $($ref)* ast::PreprocessorVersion) -> Visit {
        Visit::Children
      }

      fn visit_preprocessor_version_profile(
        &mut self,
        _: $($ref)* ast::PreprocessorVersionProfile,
      ) -> Visit {
        Visit::Children
      }

      fn visit_selection_statement(&mut self, _: $($ref)* ast::SelectionStatement) -> Visit {
        Visit::Children
      }

      fn visit_selection_rest_statement(&mut self, _: $($ref)* ast::SelectionRestStatement) -> Visit {
        Visit::Children
      }

      fn visit_single_declaration(&mut self, _: $($ref)* ast::SingleDeclaration) -> Visit {
        Visit::Children
      }

      fn visit_single_declaration_no_type(&mut self, _: $($ref)* ast::SingleDeclarationNoType) -> Visit {
        Visit::Children
      }

      fn visit_struct_field_specifier(&mut self, _: $($ref)* ast::StructFieldSpecifier) -> Visit {
        Visit::Children
      }

      fn visit_struct_specifier(&mut self, _: $($ref)* ast::StructSpecifier) -> Visit {
        Visit::Children
      }

      fn visit_switch_statement(&mut self, _: $($ref)* ast::SwitchStatement) -> Visit {
        Visit::Children
      }

      fn visit_type_qualifier(&mut self, _: $($ref)* ast::TypeQualifier) -> Visit {
        Visit::Children
      }

      fn visit_type_specifier(&mut self, _: $($ref)* ast::TypeSpecifier) -> Visit {
        Visit::Children
      }

      fn visit_full_specified_type(&mut self, _: $($ref)* ast::FullySpecifiedType) -> Visit {
        Visit::Children
      }

      fn visit_array_specifier(&mut self, _: $($ref)* ast::ArraySpecifier) -> Visit {
        Visit::Children
      }

      fn visit_array_specifier_dimension(&mut self, _: $($ref)* ast::ArraySpecifierDimension) -> Visit {
        Visit::Children
      }

      fn visit_assignment_op(&mut self, _: $($ref)* ast::AssignmentOp) -> Visit {
        Visit::Children
      }

      fn visit_binary_op(&mut self, _: $($ref)* ast::BinaryOp) -> Visit {
        Visit::Children
      }

      fn visit_case_label(&mut self, _: $($ref)* ast::CaseLabel) -> Visit {
        Visit::Children
      }

      fn visit_condition(&mut self, _: $($ref)* ast::Condition) -> Visit {
        Visit::Children
      }

      fn visit_declaration(&mut self, _: $($ref)* ast::Declaration) -> Visit {
        Visit::Children
      }

      fn visit_expr(&mut self, _: $($ref)* ast::Expr) -> Visit {
        Visit::Children
      }

      fn visit_fun_identifier(&mut self, _: $($ref)* ast::FunIdentifier) -> Visit {
        Visit::Children
      }

      fn visit_function_parameter_declaration(
        &mut self,
        _: $($ref)* ast::FunctionParameterDeclaration,
      ) -> Visit {
        Visit::Children
      }

      fn visit_initializer(&mut self, _: $($ref)* ast::Initializer) -> Visit {
        Visit::Children
      }

      fn visit_interpolation_qualifier(&mut self, _: $($ref)* ast::InterpolationQualifier) -> Visit {
        Visit::Children
      }

      fn visit_iteration_statement(&mut self, _: $($ref)* ast::IterationStatement) -> Visit {
        Visit::Children
      }

      fn visit_jump_statement(&mut self, _: $($ref)* ast::JumpStatement) -> Visit {
        Visit::Children
      }

      fn visit_layout_qualifier_spec(&mut self, _: $($ref)* ast::LayoutQualifierSpec) -> Visit {
        Visit::Children
      }

      fn visit_precision_qualifier(&mut self, _: $($ref)* ast::PrecisionQualifier) -> Visit {
        Visit::Children
      }

      fn visit_statement(&mut self, _: $($ref)* ast::Statement) -> Visit {
        Visit::Children
      }

      fn visit_compound_statement(&mut self, _: $($ref)* ast::CompoundStatement) -> Visit {
        Visit::Children
      }

      fn visit_storage_qualifier(&mut self, _: $($ref)* ast::StorageQualifier) -> Visit {
        Visit::Children
      }

      fn visit_type_qualifier_spec(&mut self, _: $($ref)* ast::TypeQualifierSpec) -> Visit {
        Visit::Children
      }

      fn visit_type_specifier_non_array(&mut self, _: $($ref)* ast::TypeSpecifierNonArray) -> Visit {
        Visit::Children
      }

      fn visit_unary_op(&mut self, _: $($ref)* ast::UnaryOp) -> Visit {
        Visit::Children
      }

      fn visit_expr_statement(&mut self, _: $($ref)* ast::ExprStatement) -> Visit {
        Visit::Children
      }
    }
  }
}

macro_rules! make_host_trait {
  ($host_ty:ident, $visitor_ty:ident, $mthd_name:ident, $iter:ident, $($ref:tt)*) => {
    /// Part of the AST that can be visited.
    ///
    /// You shouldn’t have to worry about this type nor how to implement it – it’s completely
    /// implemented for you. However, it works in a pretty simple way: any implementor of the host trait can
    /// be used with a visitor.
    ///
    /// The idea is that visiting an AST node is a two-step process:
    ///
    ///   - First, you *can* get your visitor called once as soon as an interesting node gets visited.
    ///     For instance, if your visitor has an implementation for visiting expressions, everytime an
    ///     expression gets visited, your visitor will run.
    ///   - If your implementation of visiting an AST node returns [`Visit::Children`] and if the given
    ///     node has children, the visitor will go deeper, invoking other calls if you have defined any.
    ///     A typical pattern you might want to do is to implement your visitor to gets run on all
    ///     typenames. Since expressions contains variables, you will get your visitor called once again
    ///     there.
    ///   - Notice that since visitors are mutable, you can accumulate a state as you go deeper in the
    ///     AST to implement various checks and validations.
    ///
    /// Note that this trait exists in two versions: an immutable one, [`Host`], which doesn’t allow you to mutate the
    /// AST (but takes a `&`), and a mutable one, [`HostMut`], which allows for AST mutation.
    pub trait $host_ty {
      /// Visit an AST node.
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty;
    }

    impl<T> $host_ty for Option<T>
      where
          T: $host_ty,
      {
        fn $mthd_name<V>($($ref)* self, visitor: &mut V)
        where
            V: $visitor_ty,
        {
          if let Some(x) = self {
            x.$mthd_name(visitor);
          }
        }
      }

    impl<T> $host_ty for Box<T>
      where
          T: $host_ty,
      {
        fn $mthd_name<V>($($ref)* self, visitor: &mut V)
        where
            V: $visitor_ty,
        {
          (**self).$mthd_name(visitor);
        }
      }

    impl $host_ty for ast::TranslationUnit {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_translation_unit(self);

        if visit == Visit::Children {
          for ed in $($ref)* self.0 {
            ed.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::ExternalDeclaration {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_external_declaration(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::ExternalDeclarationData::Preprocessor(p) => p.$mthd_name(visitor),
            ast::ExternalDeclarationData::FunctionDefinition(fd) => fd.$mthd_name(visitor),
            ast::ExternalDeclarationData::Declaration(d) => d.$mthd_name(visitor),
          }
        }
      }
    }

    impl $host_ty for ast::Preprocessor {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::PreprocessorData::Define(pd) => pd.$mthd_name(visitor),
            ast::PreprocessorData::Else => (),
            ast::PreprocessorData::ElseIf(pei) => pei.$mthd_name(visitor),
            ast::PreprocessorData::EndIf => (),
            ast::PreprocessorData::Error(pe) => pe.$mthd_name(visitor),
            ast::PreprocessorData::If(pi) => pi.$mthd_name(visitor),
            ast::PreprocessorData::IfDef(pid) => pid.$mthd_name(visitor),
            ast::PreprocessorData::IfNDef(pind) => pind.$mthd_name(visitor),
            ast::PreprocessorData::Include(pi) => pi.$mthd_name(visitor),
            ast::PreprocessorData::Line(pl) => pl.$mthd_name(visitor),
            ast::PreprocessorData::Pragma(pp) => pp.$mthd_name(visitor),
            ast::PreprocessorData::Undef(pu) => pu.$mthd_name(visitor),
            ast::PreprocessorData::Version(pv) => pv.$mthd_name(visitor),
            ast::PreprocessorData::Extension(ext) => ext.$mthd_name(visitor),
          }
        }
      }
    }

    impl $host_ty for ast::PreprocessorDefine {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor_define(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::PreprocessorDefineData::ObjectLike { ident, .. } => {
              ident.$mthd_name(visitor);
            }

            ast::PreprocessorDefineData::FunctionLike {
              ident,
              args,
              ..
            } => {
              ident.$mthd_name(visitor);

              for arg in args {
                arg.$mthd_name(visitor);
              }
            }
          }
        }
      }
    }

    impl $host_ty for ast::PreprocessorElseIf {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_elseif(self);
      }
    }

    impl $host_ty for ast::PreprocessorError {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_error(self);
      }
    }

    impl $host_ty for ast::PreprocessorIf {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_if(self);
      }
    }

    impl $host_ty for ast::PreprocessorIfDef {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor_ifdef(self);

        if visit == Visit::Children {
          self.ident.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::PreprocessorIfNDef {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor_ifndef(self);

        if visit == Visit::Children {
          self.ident.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::PreprocessorInclude {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_include(self);
      }
    }

    impl $host_ty for ast::PreprocessorLine {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_line(self);
      }
    }

    impl $host_ty for ast::PreprocessorPragma {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_pragma(self);
      }
    }

    impl $host_ty for ast::PreprocessorUndef {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor_undef(self);

        if visit == Visit::Children {
          self.name.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::PreprocessorVersion {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor_version(self);

        if visit == Visit::Children {
          self.profile.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::PreprocessorVersionProfile {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_version_profile(self);
      }
    }

    impl $host_ty for ast::PreprocessorExtension {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_preprocessor_extension(self);

        if visit == Visit::Children {
          self.name.$mthd_name(visitor);
          self.behavior.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::PreprocessorExtensionBehavior {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_extension_behavior(self);
      }
    }

    impl $host_ty for ast::PreprocessorExtensionName {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_preprocessor_extension_name(self);
      }
    }

    impl $host_ty for ast::FunctionPrototype {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_function_prototype(self);

        if visit == Visit::Children {
          self.ty.$mthd_name(visitor);
          self.name.$mthd_name(visitor);

          for param in $($ref)* self.parameters {
            param.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::FunctionParameterDeclaration {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_function_parameter_declaration(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::FunctionParameterDeclarationData::Named(tq, fpd) => {
              tq.$mthd_name(visitor);
              fpd.$mthd_name(visitor);
            }

            ast::FunctionParameterDeclarationData::Unnamed(tq, ty) => {
              tq.$mthd_name(visitor);
              ty.$mthd_name(visitor);
            }
          }
        }
      }
    }

    impl $host_ty for ast::FunctionParameterDeclarator {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_function_parameter_declarator(self);

        if visit == Visit::Children {
          self.ty.$mthd_name(visitor);
          self.ident.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::FunctionDefinition {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_function_definition(self);

        if visit == Visit::Children {
          self.prototype.$mthd_name(visitor);
          self.statement.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::Declaration {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_declaration(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::DeclarationData::FunctionPrototype(fp) => fp.$mthd_name(visitor),

            ast::DeclarationData::InitDeclaratorList(idl) => idl.$mthd_name(visitor),

            ast::DeclarationData::Precision(pq, ty) => {
              pq.$mthd_name(visitor);
              ty.$mthd_name(visitor);
            }

            ast::DeclarationData::Block(block) => block.$mthd_name(visitor),
          }
        }
      }
    }

    impl $host_ty for ast::Block {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_block(self);

        if visit == Visit::Children {
          self.qualifier.$mthd_name(visitor);
          self.name.$mthd_name(visitor);

          for field in $($ref)* self.fields {
            field.$mthd_name(visitor);
          }

          self.identifier.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::InitDeclaratorList {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_init_declarator_list(self);

        if visit == Visit::Children {
          self.head.$mthd_name(visitor);

          for d in $($ref)* self.tail {
            d.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::SingleDeclaration {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_single_declaration(self);

        if visit == Visit::Children {
          self.ty.$mthd_name(visitor);
          self.name.$mthd_name(visitor);
          self.array_specifier.$mthd_name(visitor);
          self.initializer.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::SingleDeclarationNoType {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_single_declaration_no_type(self);

        if visit == Visit::Children {
          self.ident.$mthd_name(visitor);
          self.initializer.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::FullySpecifiedType {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_full_specified_type(self);

        if visit == Visit::Children {
          self.qualifier.$mthd_name(visitor);
          self.ty.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::TypeSpecifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_type_specifier(self);

        if visit == Visit::Children {
          self.ty.$mthd_name(visitor);
          self.array_specifier.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::TypeSpecifierNonArray {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_type_specifier_non_array(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::TypeSpecifierNonArrayData::Struct(ss) => ss.$mthd_name(visitor),
            ast::TypeSpecifierNonArrayData::TypeName(tn) => tn.$mthd_name(visitor),
            _ => (),
          }
        }
      }
    }

    impl $host_ty for ast::TypeQualifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_type_qualifier(self);

        if visit == Visit::Children {
          for tqs in $($ref)* self.qualifiers {
            tqs.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::TypeQualifierSpec {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_type_qualifier_spec(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::TypeQualifierSpecData::Storage(sq) => sq.$mthd_name(visitor),
            ast::TypeQualifierSpecData::Layout(lq) => lq.$mthd_name(visitor),
            ast::TypeQualifierSpecData::Precision(pq) => pq.$mthd_name(visitor),
            ast::TypeQualifierSpecData::Interpolation(iq) => iq.$mthd_name(visitor),
            _ => (),
          }
        }
      }
    }

    impl $host_ty for ast::StorageQualifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_storage_qualifier(self);

        if visit == Visit::Children {
          if let ast::StorageQualifierData::Subroutine(names) = $($ref)* **self {
            for name in names {
              name.$mthd_name(visitor);
            }
          }
        }
      }
    }

    impl $host_ty for ast::LayoutQualifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_layout_qualifier(self);

        if visit == Visit::Children {
          for lqs in $($ref)* self.ids {
            lqs.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::LayoutQualifierSpec {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_layout_qualifier_spec(self);

        if visit == Visit::Children {
          if let ast::LayoutQualifierSpecData::Identifier(ident, expr) = $($ref)* **self {
            ident.$mthd_name(visitor);

            if let Some(e) = expr {
              e.$mthd_name(visitor);
            }
          }
        }
      }
    }

    impl $host_ty for ast::PrecisionQualifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_precision_qualifier(self);
      }
    }

    impl $host_ty for ast::InterpolationQualifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_interpolation_qualifier(self);
      }
    }

    impl $host_ty for ast::TypeName {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_type_name(self);
      }
    }

    impl $host_ty for ast::Identifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_identifier(self);
      }
    }

    impl $host_ty for ast::ArrayedIdentifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_arrayed_identifier(self);

        if visit == Visit::Children {
          self.ident.$mthd_name(visitor);
          self.array_spec.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::Expr {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_expr(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::ExprData::Variable(ident) => ident.$mthd_name(visitor),

            ast::ExprData::Unary(op, e) => {
              op.$mthd_name(visitor);
              e.$mthd_name(visitor);
            }

            ast::ExprData::Binary(op, a, b) => {
              op.$mthd_name(visitor);
              a.$mthd_name(visitor);
              b.$mthd_name(visitor);
            }

            ast::ExprData::Ternary(a, b, c) => {
              a.$mthd_name(visitor);
              b.$mthd_name(visitor);
              c.$mthd_name(visitor);
            }

            ast::ExprData::Assignment(lhs, op, rhs) => {
              lhs.$mthd_name(visitor);
              op.$mthd_name(visitor);
              rhs.$mthd_name(visitor);
            }

            ast::ExprData::Bracket(e, arr_spec) => {
              e.$mthd_name(visitor);
              arr_spec.$mthd_name(visitor);
            }

            ast::ExprData::FunCall(fi, params) => {
              fi.$mthd_name(visitor);

              for param in params {
                param.$mthd_name(visitor);
              }
            }

            ast::ExprData::Dot(e, i) => {
              e.$mthd_name(visitor);
              i.$mthd_name(visitor);
            }

            ast::ExprData::PostInc(e) => e.$mthd_name(visitor),

            ast::ExprData::PostDec(e) => e.$mthd_name(visitor),

            ast::ExprData::Comma(a, b) => {
              a.$mthd_name(visitor);
              b.$mthd_name(visitor);
            }

            _ => (),
          }
        }
      }
    }

    impl $host_ty for ast::UnaryOp {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_unary_op(self);
      }
    }

    impl $host_ty for ast::BinaryOp {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_binary_op(self);
      }
    }

    impl $host_ty for ast::AssignmentOp {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let _ = visitor.visit_assignment_op(self);
      }
    }

    impl $host_ty for ast::ArraySpecifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_array_specifier(self);

        if visit == Visit::Children {
          for dimension in $($ref)* self.dimensions {
            dimension.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::ArraySpecifierDimension {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_array_specifier_dimension(self);

        if visit == Visit::Children {
          if let ast::ArraySpecifierDimensionData::ExplicitlySized(e) = $($ref)* **self {
            e.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::FunIdentifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_fun_identifier(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::FunIdentifierData::TypeSpecifier(t) => t.$mthd_name(visitor),
            ast::FunIdentifierData::Expr(e) => e.$mthd_name(visitor),
          }
        }
      }
    }

    impl $host_ty for ast::StructSpecifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_struct_specifier(self);

        if visit == Visit::Children {
          self.name.$mthd_name(visitor);

          for field in $($ref)* self.fields {
            field.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::StructFieldSpecifier {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_struct_field_specifier(self);

        if visit == Visit::Children {
          self.qualifier.$mthd_name(visitor);
          self.ty.$mthd_name(visitor);

          for identifier in $($ref)* self.identifiers {
            identifier.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::Statement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_statement(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::StatementData::Declaration(d) => d.$mthd_name(visitor),
            ast::StatementData::Expression(e) => e.$mthd_name(visitor),
            ast::StatementData::Selection(s) => s.$mthd_name(visitor),
            ast::StatementData::Switch(s) => s.$mthd_name(visitor),
            ast::StatementData::CaseLabel(cl) => cl.$mthd_name(visitor),
            ast::StatementData::Iteration(i) => i.$mthd_name(visitor),
            ast::StatementData::Jump(j) => j.$mthd_name(visitor),
            ast::StatementData::Compound(cs) => cs.$mthd_name(visitor),
          }
        }
      }
    }

    impl $host_ty for ast::CompoundStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_compound_statement(self);

        if visit == Visit::Children {
          for stmt in $($ref)* self.statement_list {
            stmt.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::ExprStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_expr_statement(self);

        if visit == Visit::Children {
          self.0.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::SelectionStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_selection_statement(self);

        if visit == Visit::Children {
          self.cond.$mthd_name(visitor);
          self.rest.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::SelectionRestStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_selection_rest_statement(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::SelectionRestStatementData::Statement(s) => s.$mthd_name(visitor),

            ast::SelectionRestStatementData::Else(a, b) => {
              a.$mthd_name(visitor);
              b.$mthd_name(visitor);
            }
          }
        }
      }
    }

    impl $host_ty for ast::SwitchStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_switch_statement(self);

        if visit == Visit::Children {
          self.head.$mthd_name(visitor);

          for s in $($ref)* self.body {
            s.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::CaseLabel {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_case_label(self);

        if visit == Visit::Children {
          if let ast::CaseLabelData::Case(e) = $($ref)* **self {
            e.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::IterationStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_iteration_statement(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::IterationStatementData::While(c, s) => {
              c.$mthd_name(visitor);
              s.$mthd_name(visitor);
            }

            ast::IterationStatementData::DoWhile(s, e) => {
              s.$mthd_name(visitor);
              e.$mthd_name(visitor);
            }

            ast::IterationStatementData::For(fis, frs, s) => {
              fis.$mthd_name(visitor);
              frs.$mthd_name(visitor);
              s.$mthd_name(visitor);
            }
          }
        }
      }
    }

    impl $host_ty for ast::ForInitStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_for_init_statement(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::ForInitStatementData::Expression(e) => e.$mthd_name(visitor),
            ast::ForInitStatementData::Declaration(d) => d.$mthd_name(visitor),
          }
        }
      }
    }

    impl $host_ty for ast::ForRestStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_for_rest_statement(self);

        if visit == Visit::Children {
          self.condition.$mthd_name(visitor);
          self.post_expr.$mthd_name(visitor);
        }
      }
    }

    impl $host_ty for ast::JumpStatement {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_jump_statement(self);

        if visit == Visit::Children {
          if let ast::JumpStatementData::Return(r) = $($ref)* **self {
            r.$mthd_name(visitor);
          }
        }
      }
    }

    impl $host_ty for ast::Condition {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_condition(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::ConditionData::Expr(e) => e.$mthd_name(visitor),

            ast::ConditionData::Assignment(fst, ident, init) => {
              fst.$mthd_name(visitor);
              ident.$mthd_name(visitor);
              init.$mthd_name(visitor);
            }
          }
        }
      }
    }

    impl $host_ty for ast::Initializer {
      fn $mthd_name<V>($($ref)* self, visitor: &mut V)
      where
          V: $visitor_ty,
      {
        let visit = visitor.visit_initializer(self);

        if visit == Visit::Children {
          match $($ref)* **self {
            ast::InitializerData::Simple(e) => e.$mthd_name(visitor),

            ast::InitializerData::List(i) => {
              for i in i.$iter() {
                i.$mthd_name(visitor);
              }
            }
          }
        }
      }
    }
  }
}

// immutable
make_visitor_trait!(Visitor, &);
make_host_trait!(Host, Visitor, visit, iter, &);

// mutable
make_visitor_trait!(VisitorMut, &mut);
make_host_trait!(HostMut, VisitorMut, visit_mut, iter_mut, &mut);

#[cfg(test)]
mod tests {
    use lang_util::NodeContent;
    use std::iter::FromIterator;

    use super::*;

    #[test]
    #[allow(clippy::approx_constant)]
    fn count_variables() {
        let decl0 = ast::StatementData::declare_var(
            ast::TypeSpecifierNonArrayData::Float,
            "x",
            None,
            Some(ast::ExprData::from(3.14).into_node()),
        );

        let decl1 =
            ast::StatementData::declare_var(ast::TypeSpecifierNonArrayData::Int, "y", None, None);

        let decl2 =
            ast::StatementData::declare_var(ast::TypeSpecifierNonArrayData::Vec4, "z", None, None);

        let compound: ast::CompoundStatement =
            ast::CompoundStatementData::from_iter(vec![decl0.into(), decl1.into(), decl2.into()])
                .into();

        // our visitor that will count the number of variables it saw
        struct Counter {
            var_nb: usize,
        }

        impl Visitor for Counter {
            // we are only interested in single declaration with a name
            fn visit_single_declaration(&mut self, declaration: &ast::SingleDeclaration) -> Visit {
                if declaration.name.is_some() {
                    self.var_nb += 1;
                }

                // do not go deeper
                Visit::Parent
            }
        }

        let mut counter = Counter { var_nb: 0 };
        compound.visit(&mut counter);
        assert_eq!(counter.var_nb, 3);
    }
}
