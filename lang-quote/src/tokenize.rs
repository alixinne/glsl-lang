//! The [`Tokenize`] trait, turning [glsl](https://crates.io/crates/glsl) into [`TokenStream`]s.

use glsl_lang::ast;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::iter::once;

use crate::quoted::Quoted;

/// Tokenize a value into a stream of tokens.
pub trait Tokenize {
    /// Inject self into a [`TokenStream`].
    fn tokenize(&self, stream: &mut TokenStream);
}

impl Tokenize for bool {
    fn tokenize(&self, stream: &mut TokenStream) {
        self.to_tokens(stream)
    }
}

impl Tokenize for i32 {
    fn tokenize(&self, stream: &mut TokenStream) {
        self.to_tokens(stream)
    }
}

impl Tokenize for u32 {
    fn tokenize(&self, stream: &mut TokenStream) {
        self.to_tokens(stream)
    }
}

impl Tokenize for f32 {
    fn tokenize(&self, stream: &mut TokenStream) {
        self.to_tokens(stream)
    }
}

impl Tokenize for f64 {
    fn tokenize(&self, stream: &mut TokenStream) {
        self.to_tokens(stream)
    }
}

macro_rules! impl_tokenize {
    ($type_name:ty, $tokenizer:ident) => {
        impl Tokenize for $type_name {
            fn tokenize(&self, stream: &mut TokenStream) {
                stream.extend(once($tokenizer(self)))
            }
        }
    };
}

impl_tokenize!(ast::TypeName, tokenize_type_name);
impl_tokenize!(
    ast::TypeSpecifierNonArray,
    tokenize_type_specifier_non_array
);
impl_tokenize!(ast::TypeSpecifier, tokenize_type_specifier);
impl_tokenize!(ast::UnaryOp, tokenize_unary_op);
impl_tokenize!(ast::StructFieldSpecifier, tokenize_struct_field);
impl_tokenize!(ast::StructSpecifier, tokenize_struct_non_declaration);
impl_tokenize!(ast::StorageQualifier, tokenize_storage_qualifier);
impl_tokenize!(ast::LayoutQualifier, tokenize_layout_qualifier);
impl_tokenize!(ast::PrecisionQualifier, tokenize_precision_qualifier);
impl_tokenize!(
    ast::InterpolationQualifier,
    tokenize_interpolation_qualifier
);
impl_tokenize!(ast::TypeQualifier, tokenize_type_qualifier);
impl_tokenize!(ast::TypeQualifierSpec, tokenize_type_qualifier_spec);
impl_tokenize!(ast::FullySpecifiedType, tokenize_fully_specified_type);
impl_tokenize!(ast::ArraySpecifier, tokenize_array_spec);
impl_tokenize!(ast::Expr, tokenize_expr);
impl_tokenize!(ast::Declaration, tokenize_declaration);
impl_tokenize!(ast::FunctionPrototype, tokenize_function_prototype);
impl_tokenize!(ast::InitDeclaratorList, tokenize_init_declarator_list);
impl_tokenize!(ast::SingleDeclaration, tokenize_single_declaration);
impl_tokenize!(ast::Initializer, tokenize_initializer);
impl_tokenize!(ast::FunIdentifier, tokenize_function_identifier);
impl_tokenize!(ast::AssignmentOp, tokenize_assignment_op);
impl_tokenize!(ast::ExprStatement, tokenize_expr_statement);
impl_tokenize!(ast::SelectionStatement, tokenize_selection_statement);
impl_tokenize!(ast::SwitchStatement, tokenize_switch_statement);
impl_tokenize!(ast::CaseLabel, tokenize_case_label);
impl_tokenize!(ast::IterationStatement, tokenize_iteration_statement);
impl_tokenize!(ast::JumpStatement, tokenize_jump_statement);
impl_tokenize!(ast::Condition, tokenize_condition);
impl_tokenize!(ast::Statement, tokenize_statement);
impl_tokenize!(ast::CompoundStatement, tokenize_compound_statement);
impl_tokenize!(ast::FunctionDefinition, tokenize_function_definition);
impl_tokenize!(ast::ExternalDeclaration, tokenize_external_declaration);
impl_tokenize!(ast::TranslationUnit, tokenize_translation_unit);
impl_tokenize!(ast::Preprocessor, tokenize_preprocessor);
impl_tokenize!(ast::PreprocessorDefine, tokenize_preprocessor_define);
impl_tokenize!(ast::PreprocessorElseIf, tokenize_preprocessor_elseif);
impl_tokenize!(ast::PreprocessorError, tokenize_preprocessor_error);
impl_tokenize!(ast::PreprocessorIf, tokenize_preprocessor_if);
impl_tokenize!(ast::PreprocessorIfDef, tokenize_preprocessor_ifdef);
impl_tokenize!(ast::PreprocessorIfNDef, tokenize_preprocessor_ifndef);
impl_tokenize!(ast::PreprocessorInclude, tokenize_preprocessor_include);
impl_tokenize!(ast::PreprocessorLine, tokenize_preprocessor_line);
impl_tokenize!(ast::PreprocessorPragma, tokenize_preprocessor_pragma);
impl_tokenize!(ast::PreprocessorUndef, tokenize_preprocessor_undef);
impl_tokenize!(ast::PreprocessorVersion, tokenize_preprocessor_version);
impl_tokenize!(
    ast::PreprocessorVersionProfile,
    tokenize_preprocessor_version_profile
);
impl_tokenize!(
    ast::PreprocessorExtensionName,
    tokenize_preprocessor_extension_name
);
impl_tokenize!(
    ast::PreprocessorExtensionBehavior,
    tokenize_preprocessor_extension_behavior
);
impl_tokenize!(ast::PreprocessorExtension, tokenize_preprocessor_extension);

fn tokenize_path(p: &ast::Path) -> TokenStream {
    match p {
        ast::Path::Absolute(s) => quote! { glsl_lang::ast::Path::Absolute(#s.to_owned()) },
        ast::Path::Relative(s) => quote! { glsl_lang::ast::Path::Relative(#s.to_owned()) },
    }
}

fn tokenize_identifier(i: &ast::Identifier) -> TokenStream {
    if let Some(rs_ident) = i.as_rs_ident() {
        // Rust identifier
        let ident = format_ident!("{}", rs_ident);
        quote! { #ident }
    } else {
        // Regular identifier
        let t = i.0.to_owned().quote();
        let span = tokenize_span(&i.span);
        quote! { glsl_lang::ast::Identifier::new(glsl_lang::ast::IdentifierData(#t), #span) }
    }
}

fn tokenize_type_name(tn: &ast::TypeName) -> TokenStream {
    let t = tn.0.to_owned().quote();
    let span = tokenize_span(&tn.span);
    quote! { glsl_lang::ast::TypeName::new(glsl_lang::ast::TypeNameData(#t), #span) }
}

fn tokenize_type_specifier_non_array(t: &ast::TypeSpecifierNonArray) -> TokenStream {
    match *t {
        ast::TypeSpecifierNonArray::Void => quote! { glsl_lang::ast::TypeSpecifierNonArray::Void },
        ast::TypeSpecifierNonArray::Bool => quote! { glsl_lang::ast::TypeSpecifierNonArray::Bool },
        ast::TypeSpecifierNonArray::Int => quote! { glsl_lang::ast::TypeSpecifierNonArray::Int },
        ast::TypeSpecifierNonArray::UInt => quote! { glsl_lang::ast::TypeSpecifierNonArray::UInt },
        ast::TypeSpecifierNonArray::Float => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Float }
        }
        ast::TypeSpecifierNonArray::Double => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Double }
        }
        ast::TypeSpecifierNonArray::Vec2 => quote! { glsl_lang::ast::TypeSpecifierNonArray::Vec2 },
        ast::TypeSpecifierNonArray::Vec3 => quote! { glsl_lang::ast::TypeSpecifierNonArray::Vec3 },
        ast::TypeSpecifierNonArray::Vec4 => quote! { glsl_lang::ast::TypeSpecifierNonArray::Vec4 },
        ast::TypeSpecifierNonArray::DVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DVec2 }
        }
        ast::TypeSpecifierNonArray::DVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DVec3 }
        }
        ast::TypeSpecifierNonArray::DVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DVec4 }
        }
        ast::TypeSpecifierNonArray::BVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::BVec2 }
        }
        ast::TypeSpecifierNonArray::BVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::BVec3 }
        }
        ast::TypeSpecifierNonArray::BVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::BVec4 }
        }
        ast::TypeSpecifierNonArray::IVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IVec2 }
        }
        ast::TypeSpecifierNonArray::IVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IVec3 }
        }
        ast::TypeSpecifierNonArray::IVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IVec4 }
        }
        ast::TypeSpecifierNonArray::UVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UVec2 }
        }
        ast::TypeSpecifierNonArray::UVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UVec3 }
        }
        ast::TypeSpecifierNonArray::UVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UVec4 }
        }
        ast::TypeSpecifierNonArray::Mat2 => quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat2 },
        ast::TypeSpecifierNonArray::Mat3 => quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat3 },
        ast::TypeSpecifierNonArray::Mat4 => quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat4 },
        ast::TypeSpecifierNonArray::Mat22 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat22 }
        }
        ast::TypeSpecifierNonArray::Mat23 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat23 }
        }
        ast::TypeSpecifierNonArray::Mat24 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat24 }
        }
        ast::TypeSpecifierNonArray::Mat32 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat32 }
        }
        ast::TypeSpecifierNonArray::Mat33 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat33 }
        }
        ast::TypeSpecifierNonArray::Mat34 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat34 }
        }
        ast::TypeSpecifierNonArray::Mat42 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat42 }
        }
        ast::TypeSpecifierNonArray::Mat43 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat43 }
        }
        ast::TypeSpecifierNonArray::Mat44 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Mat44 }
        }
        ast::TypeSpecifierNonArray::DMat2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat2 }
        }
        ast::TypeSpecifierNonArray::DMat3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat3 }
        }
        ast::TypeSpecifierNonArray::DMat4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat4 }
        }
        ast::TypeSpecifierNonArray::DMat22 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat22 }
        }
        ast::TypeSpecifierNonArray::DMat23 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat23 }
        }
        ast::TypeSpecifierNonArray::DMat24 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat24 }
        }
        ast::TypeSpecifierNonArray::DMat32 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat32 }
        }
        ast::TypeSpecifierNonArray::DMat33 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat33 }
        }
        ast::TypeSpecifierNonArray::DMat34 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat34 }
        }
        ast::TypeSpecifierNonArray::DMat42 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat42 }
        }
        ast::TypeSpecifierNonArray::DMat43 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat43 }
        }
        ast::TypeSpecifierNonArray::DMat44 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::DMat44 }
        }
        ast::TypeSpecifierNonArray::Sampler1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler1D }
        }
        ast::TypeSpecifierNonArray::Image1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image1D }
        }
        ast::TypeSpecifierNonArray::Sampler2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2D }
        }
        ast::TypeSpecifierNonArray::Image2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image2D }
        }
        ast::TypeSpecifierNonArray::Sampler3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler3D }
        }
        ast::TypeSpecifierNonArray::Image3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image3D }
        }
        ast::TypeSpecifierNonArray::SamplerCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::SamplerCube }
        }
        ast::TypeSpecifierNonArray::ImageCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ImageCube }
        }
        ast::TypeSpecifierNonArray::Sampler2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DRect }
        }
        ast::TypeSpecifierNonArray::Image2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image2DRect }
        }
        ast::TypeSpecifierNonArray::Sampler1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler1DArray }
        }
        ast::TypeSpecifierNonArray::Image1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image1DArray }
        }
        ast::TypeSpecifierNonArray::Sampler2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DArray }
        }
        ast::TypeSpecifierNonArray::Image2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image2DArray }
        }
        ast::TypeSpecifierNonArray::SamplerBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::SamplerBuffer }
        }
        ast::TypeSpecifierNonArray::ImageBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ImageBuffer }
        }
        ast::TypeSpecifierNonArray::Sampler2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DMs }
        }
        ast::TypeSpecifierNonArray::Image2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image2DMs }
        }
        ast::TypeSpecifierNonArray::Sampler2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DMsArray }
        }
        ast::TypeSpecifierNonArray::Image2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Image2DMsArray }
        }
        ast::TypeSpecifierNonArray::SamplerCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::SamplerCubeArray }
        }
        ast::TypeSpecifierNonArray::ImageCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ImageCubeArray }
        }
        ast::TypeSpecifierNonArray::Sampler1DShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler1DShadow }
        }
        ast::TypeSpecifierNonArray::Sampler2DShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DShadow }
        }
        ast::TypeSpecifierNonArray::Sampler2DRectShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DRectShadow }
        }
        ast::TypeSpecifierNonArray::Sampler1DArrayShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler1DArrayShadow }
        }
        ast::TypeSpecifierNonArray::Sampler2DArrayShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Sampler2DArrayShadow }
        }
        ast::TypeSpecifierNonArray::SamplerCubeShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::SamplerCubeShadow }
        }
        ast::TypeSpecifierNonArray::SamplerCubeArrayShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::SamplerCubeArrayShadow }
        }
        ast::TypeSpecifierNonArray::ISampler1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler1D }
        }
        ast::TypeSpecifierNonArray::IImage1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage1D }
        }
        ast::TypeSpecifierNonArray::ISampler2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler2D }
        }
        ast::TypeSpecifierNonArray::IImage2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage2D }
        }
        ast::TypeSpecifierNonArray::ISampler3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler3D }
        }
        ast::TypeSpecifierNonArray::IImage3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage3D }
        }
        ast::TypeSpecifierNonArray::ISamplerCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISamplerCube }
        }
        ast::TypeSpecifierNonArray::IImageCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImageCube }
        }
        ast::TypeSpecifierNonArray::ISampler2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler2DRect }
        }
        ast::TypeSpecifierNonArray::IImage2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage2DRect }
        }
        ast::TypeSpecifierNonArray::ISampler1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler1DArray }
        }
        ast::TypeSpecifierNonArray::IImage1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage1DArray }
        }
        ast::TypeSpecifierNonArray::ISampler2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler2DArray }
        }
        ast::TypeSpecifierNonArray::IImage2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage2DArray }
        }
        ast::TypeSpecifierNonArray::ISamplerBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISamplerBuffer }
        }
        ast::TypeSpecifierNonArray::IImageBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImageBuffer }
        }
        ast::TypeSpecifierNonArray::ISampler2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler2DMs }
        }
        ast::TypeSpecifierNonArray::IImage2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage2DMs }
        }
        ast::TypeSpecifierNonArray::ISampler2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISampler2DMsArray }
        }
        ast::TypeSpecifierNonArray::IImage2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImage2DMsArray }
        }
        ast::TypeSpecifierNonArray::ISamplerCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::ISamplerCubeArray }
        }
        ast::TypeSpecifierNonArray::IImageCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::IImageCubeArray }
        }
        ast::TypeSpecifierNonArray::AtomicUInt => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::AtomicUInt }
        }
        ast::TypeSpecifierNonArray::USampler1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler1D }
        }
        ast::TypeSpecifierNonArray::UImage1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage1D }
        }
        ast::TypeSpecifierNonArray::USampler2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler2D }
        }
        ast::TypeSpecifierNonArray::UImage2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage2D }
        }
        ast::TypeSpecifierNonArray::USampler3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler3D }
        }
        ast::TypeSpecifierNonArray::UImage3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage3D }
        }
        ast::TypeSpecifierNonArray::USamplerCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USamplerCube }
        }
        ast::TypeSpecifierNonArray::UImageCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImageCube }
        }
        ast::TypeSpecifierNonArray::USampler2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler2DRect }
        }
        ast::TypeSpecifierNonArray::UImage2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage2DRect }
        }
        ast::TypeSpecifierNonArray::USampler1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler1DArray }
        }
        ast::TypeSpecifierNonArray::UImage1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage1DArray }
        }
        ast::TypeSpecifierNonArray::USampler2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler2DArray }
        }
        ast::TypeSpecifierNonArray::UImage2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage2DArray }
        }
        ast::TypeSpecifierNonArray::USamplerBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USamplerBuffer }
        }
        ast::TypeSpecifierNonArray::UImageBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImageBuffer }
        }
        ast::TypeSpecifierNonArray::USampler2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler2DMs }
        }
        ast::TypeSpecifierNonArray::UImage2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage2DMs }
        }
        ast::TypeSpecifierNonArray::USampler2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USampler2DMsArray }
        }
        ast::TypeSpecifierNonArray::UImage2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImage2DMsArray }
        }
        ast::TypeSpecifierNonArray::USamplerCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::USamplerCubeArray }
        }
        ast::TypeSpecifierNonArray::UImageCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArray::UImageCubeArray }
        }

        ast::TypeSpecifierNonArray::Struct(ref s) => {
            let s = tokenize_struct_non_declaration(s);
            quote! { glsl_lang::ast::TypeSpecifierNonArray::Struct(#s) }
        }

        ast::TypeSpecifierNonArray::TypeName(ref tn) => {
            let tn = tokenize_type_name(tn);
            quote! { glsl_lang::ast::TypeSpecifierNonArray::TypeName(#tn) }
        }
    }
}

fn tokenize_type_specifier(t: &ast::TypeSpecifier) -> TokenStream {
    let ty = tokenize_type_specifier_non_array(&t.ty);
    let array_specifier = t.array_specifier.as_ref().map(tokenize_array_spec).quote();

    quote! {
      glsl_lang::ast::TypeSpecifier {
        ty: #ty,
        array_specifier: #array_specifier
      }
    }
}

fn tokenize_fully_specified_type(t: &ast::FullySpecifiedType) -> TokenStream {
    let qual = t.qualifier.as_ref().map(tokenize_type_qualifier).quote();
    let ty = tokenize_type_specifier(&t.ty);

    quote! {
      glsl_lang::ast::FullySpecifiedType {
        qualifier: #qual,
        ty: #ty
      }
    }
}

fn tokenize_struct_non_declaration(s: &ast::StructSpecifier) -> TokenStream {
    let name = s.name.as_ref().map(tokenize_type_name);
    let fields = s.fields.iter().map(tokenize_struct_field);

    quote! {
      glsl_lang::ast::StructSpecifier {
        name: Some(#name),
        fields: vec![#(#fields),*]
      }
    }
}

fn tokenize_struct_field(field: &ast::StructFieldSpecifier) -> TokenStream {
    let qual = field
        .qualifier
        .as_ref()
        .map(tokenize_type_qualifier)
        .quote();
    let ty = tokenize_type_specifier(&field.ty);
    let identifiers = field.identifiers.iter().map(tokenize_arrayed_identifier);

    quote! {
      glsl_lang::ast::StructFieldSpecifier {
        qualifier: #qual,
        ty: #ty,
        identifiers: vec![#(#identifiers),*]
      }
    }
}

fn tokenize_array_spec(a: &ast::ArraySpecifier) -> TokenStream {
    let dimensions = a.dimensions.iter().map(tokenize_array_spec_dim);

    quote! {
      glsl_lang::ast::ArraySpecifier { dimensions: vec![#(#dimensions),*] }
    }
}

fn tokenize_array_spec_dim(a: &ast::ArraySpecifierDimension) -> TokenStream {
    match *a {
        ast::ArraySpecifierDimension::Unsized => {
            quote! { glsl_lang::ast::ArraySpecifierDimension::Unsized }
        }
        ast::ArraySpecifierDimension::ExplicitlySized(ref e) => {
            let expr = Box::new(tokenize_expr(&e)).quote();
            quote! { glsl_lang::ast::ArraySpecifierDimension::ExplicitlySized(#expr) }
        }
    }
}

fn tokenize_arrayed_identifier(identifier: &ast::ArrayedIdentifier) -> TokenStream {
    let ident = tokenize_identifier(&identifier.ident);
    let array_spec = identifier
        .array_spec
        .as_ref()
        .map(tokenize_array_spec)
        .quote();

    quote! {
      glsl_lang::ast::ArrayedIdentifier::new(#ident, #array_spec)
    }
}

fn tokenize_type_qualifier(q: &ast::TypeQualifier) -> TokenStream {
    let quals = q.qualifiers.iter().map(tokenize_type_qualifier_spec);

    quote! {
      glsl_lang::ast::TypeQualifier {
        qualifiers: vec![#(#quals),*]
      }
    }
}

fn tokenize_type_qualifier_spec(q: &ast::TypeQualifierSpec) -> TokenStream {
    match *q {
        ast::TypeQualifierSpec::Storage(ref s) => {
            let s = tokenize_storage_qualifier(s);
            quote! { glsl_lang::ast::TypeQualifierSpec::Storage(#s) }
        }

        ast::TypeQualifierSpec::Layout(ref l) => {
            let l = tokenize_layout_qualifier(l);
            quote! { glsl_lang::ast::TypeQualifierSpec::Layout(#l) }
        }

        ast::TypeQualifierSpec::Precision(ref p) => {
            let p = tokenize_precision_qualifier(p);
            quote! { glsl_lang::ast::TypeQualifierSpec::Precision(#p) }
        }

        ast::TypeQualifierSpec::Interpolation(ref i) => {
            let i = tokenize_interpolation_qualifier(i);
            quote! { glsl_lang::ast::TypeQualifierSpec::Interpolation(#i) }
        }

        ast::TypeQualifierSpec::Invariant => {
            quote! { glsl_lang::ast::TypeQualifierSpec::Invariant }
        }

        ast::TypeQualifierSpec::Precise => quote! { glsl_lang::ast::TypeQualifierSpec::Precise },
    }
}

fn tokenize_storage_qualifier(q: &ast::StorageQualifier) -> TokenStream {
    match *q {
        ast::StorageQualifier::Const => quote! { glsl_lang::ast::StorageQualifier::Const },
        ast::StorageQualifier::InOut => quote! { glsl_lang::ast::StorageQualifier::InOut },
        ast::StorageQualifier::In => quote! { glsl_lang::ast::StorageQualifier::In },
        ast::StorageQualifier::Out => quote! { glsl_lang::ast::StorageQualifier::Out },
        ast::StorageQualifier::Centroid => quote! { glsl_lang::ast::StorageQualifier::Centroid },
        ast::StorageQualifier::Patch => quote! { glsl_lang::ast::StorageQualifier::Patch },
        ast::StorageQualifier::Sample => quote! { glsl_lang::ast::StorageQualifier::Sample },
        ast::StorageQualifier::Uniform => quote! { glsl_lang::ast::StorageQualifier::Uniform },
        ast::StorageQualifier::Buffer => quote! { glsl_lang::ast::StorageQualifier::Buffer },
        ast::StorageQualifier::Shared => quote! { glsl_lang::ast::StorageQualifier::Shared },
        ast::StorageQualifier::Coherent => quote! { glsl_lang::ast::StorageQualifier::Coherent },
        ast::StorageQualifier::Volatile => quote! { glsl_lang::ast::StorageQualifier::Volatile },
        ast::StorageQualifier::Restrict => quote! { glsl_lang::ast::StorageQualifier::Restrict },
        ast::StorageQualifier::ReadOnly => quote! { glsl_lang::ast::StorageQualifier::ReadOnly },
        ast::StorageQualifier::WriteOnly => quote! { glsl_lang::ast::StorageQualifier::WriteOnly },

        ast::StorageQualifier::Subroutine(ref n) => {
            let n = n.iter().map(|t| tokenize_type_specifier(t));

            quote! {
              StorageQualifier::Subroutine(vec![#(#n),*])
            }
        }
    }
}

fn tokenize_layout_qualifier(l: &ast::LayoutQualifier) -> TokenStream {
    let ids = l.ids.iter().map(tokenize_layout_qualifier_spec);

    quote! {
      glsl_lang::ast::LayoutQualifier {
        ids: vec![#(#ids),*]
      }
    }
}

fn tokenize_layout_qualifier_spec(l: &ast::LayoutQualifierSpec) -> TokenStream {
    match *l {
        ast::LayoutQualifierSpec::Identifier(ref i, ref e) => {
            let i = tokenize_identifier(i);
            let expr = e
                .as_ref()
                .map(|e| Box::new(tokenize_expr(&e)).quote())
                .quote();
            quote! { glsl_lang::ast::LayoutQualifierSpec::Identifier(#i, #expr) }
        }

        ast::LayoutQualifierSpec::Shared => quote! { glsl_lang::ast::LayoutQualifierSpec::Shared },
    }
}

fn tokenize_precision_qualifier(p: &ast::PrecisionQualifier) -> TokenStream {
    match *p {
        ast::PrecisionQualifier::High => quote! { glsl_lang::ast::PrecisionQualifier::High },
        ast::PrecisionQualifier::Medium => quote! { glsl_lang::ast::PrecisionQualifier::Medium },
        ast::PrecisionQualifier::Low => quote! { glsl_lang::ast::PrecisionQualifier::Low },
    }
}

fn tokenize_interpolation_qualifier(i: &ast::InterpolationQualifier) -> TokenStream {
    match *i {
        ast::InterpolationQualifier::Smooth => {
            quote! { glsl_lang::ast::InterpolationQualifier::Smooth }
        }
        ast::InterpolationQualifier::Flat => {
            quote! { glsl_lang::ast::InterpolationQualifier::Flat }
        }
        ast::InterpolationQualifier::NoPerspective => {
            quote! { glsl_lang::ast::InterpolationQualifier::NoPerspective }
        }
    }
}

fn tokenize_expr(expr: &ast::Expr) -> TokenStream {
    if let Some(rs_ident) = expr.as_rs_ident() {
        let ident = format_ident!("{}", rs_ident);
        return quote! { #ident };
    }

    match *expr {
        ast::Expr::Variable(ref i) => {
            let i = tokenize_identifier(i);
            quote! { glsl_lang::ast::Expr::Variable(#i) }
        }

        ast::Expr::IntConst(ref x) => quote! { glsl_lang::ast::Expr::IntConst(#x) },

        ast::Expr::UIntConst(ref x) => quote! { glsl_lang::ast::Expr::UIntConst(#x) },

        ast::Expr::BoolConst(ref x) => quote! { glsl_lang::ast::Expr::BoolConst(#x) },

        ast::Expr::FloatConst(ref x) => quote! { glsl_lang::ast::Expr::FloatConst(#x) },

        ast::Expr::DoubleConst(ref x) => quote! { glsl_lang::ast::Expr::DoubleConst(#x) },

        ast::Expr::Unary(ref op, ref e) => {
            let op = tokenize_unary_op(op);
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Expr::Unary(#op, #e) }
        }

        ast::Expr::Binary(ref op, ref l, ref r) => {
            let op = tokenize_binary_op(op);
            let l = Box::new(tokenize_expr(l)).quote();
            let r = Box::new(tokenize_expr(r)).quote();
            quote! { glsl_lang::ast::Expr::Binary(#op, #l, #r) }
        }

        ast::Expr::Ternary(ref c, ref s, ref e) => {
            let c = Box::new(tokenize_expr(c)).quote();
            let s = Box::new(tokenize_expr(s)).quote();
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Expr::Ternary(#c, #s, #e) }
        }

        ast::Expr::Assignment(ref v, ref op, ref e) => {
            let v = Box::new(tokenize_expr(v)).quote();
            let op = tokenize_assignment_op(op);
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Expr::Assignment(#v, #op, #e) }
        }

        ast::Expr::Bracket(ref e, ref a) => {
            let e = Box::new(tokenize_expr(e)).quote();
            let a = tokenize_expr(a);
            quote! { glsl_lang::ast::Expr::Bracket(#e, #a) }
        }

        ast::Expr::FunCall(ref fun, ref args) => {
            let fun = tokenize_function_identifier(fun);
            let args = args.iter().map(tokenize_expr);
            quote! { glsl_lang::ast::Expr::FunCall(#fun, vec![#(#args),*]) }
        }

        ast::Expr::Dot(ref e, ref i) => {
            let e = Box::new(tokenize_expr(e)).quote();
            let i = tokenize_identifier(i);

            quote! { glsl_lang::ast::Expr::Dot(#e, #i) }
        }

        ast::Expr::PostInc(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Expr::PostInc(#e) }
        }

        ast::Expr::PostDec(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Expr::PostDec(#e) }
        }

        ast::Expr::Comma(ref a, ref b) => {
            let a = Box::new(tokenize_expr(a)).quote();
            let b = Box::new(tokenize_expr(b)).quote();
            quote! { glsl_lang::ast::Expr::Comma(#a, #b) }
        }
    }
}

fn tokenize_unary_op(op: &ast::UnaryOp) -> TokenStream {
    match *op {
        ast::UnaryOp::Inc => quote! { glsl_lang::ast::UnaryOp::Inc },
        ast::UnaryOp::Dec => quote! { glsl_lang::ast::UnaryOp::Dec },
        ast::UnaryOp::Add => quote! { glsl_lang::ast::UnaryOp::Add },
        ast::UnaryOp::Minus => quote! { glsl_lang::ast::UnaryOp::Minus },
        ast::UnaryOp::Not => quote! { glsl_lang::ast::UnaryOp::Not },
        ast::UnaryOp::Complement => quote! { glsl_lang::ast::UnaryOp::Complement },
    }
}

fn tokenize_binary_op(op: &ast::BinaryOp) -> TokenStream {
    match *op {
        ast::BinaryOp::Or => quote! { glsl_lang::ast::BinaryOp::Or },
        ast::BinaryOp::Xor => quote! { glsl_lang::ast::BinaryOp::Xor },
        ast::BinaryOp::And => quote! { glsl_lang::ast::BinaryOp::And },
        ast::BinaryOp::BitOr => quote! { glsl_lang::ast::BinaryOp::BitOr },
        ast::BinaryOp::BitXor => quote! { glsl_lang::ast::BinaryOp::BitXor },
        ast::BinaryOp::BitAnd => quote! { glsl_lang::ast::BinaryOp::BitAnd },
        ast::BinaryOp::Equal => quote! { glsl_lang::ast::BinaryOp::Equal },
        ast::BinaryOp::NonEqual => quote! { glsl_lang::ast::BinaryOp::NonEqual },
        ast::BinaryOp::Lt => quote! { glsl_lang::ast::BinaryOp::Lt },
        ast::BinaryOp::Gt => quote! { glsl_lang::ast::BinaryOp::Gt },
        ast::BinaryOp::Lte => quote! { glsl_lang::ast::BinaryOp::Lte },
        ast::BinaryOp::Gte => quote! { glsl_lang::ast::BinaryOp::Gte },
        ast::BinaryOp::LShift => quote! { glsl_lang::ast::BinaryOp::LShift },
        ast::BinaryOp::RShift => quote! { glsl_lang::ast::BinaryOp::RShift },
        ast::BinaryOp::Add => quote! { glsl_lang::ast::BinaryOp::Add },
        ast::BinaryOp::Sub => quote! { glsl_lang::ast::BinaryOp::Sub },
        ast::BinaryOp::Mult => quote! { glsl_lang::ast::BinaryOp::Mult },
        ast::BinaryOp::Div => quote! { glsl_lang::ast::BinaryOp::Div },
        ast::BinaryOp::Mod => quote! { glsl_lang::ast::BinaryOp::Mod },
    }
}

fn tokenize_assignment_op(op: &ast::AssignmentOp) -> TokenStream {
    match *op {
        ast::AssignmentOp::Equal => quote! { glsl_lang::ast::AssignmentOp::Equal },
        ast::AssignmentOp::Mult => quote! { glsl_lang::ast::AssignmentOp::Mult },
        ast::AssignmentOp::Div => quote! { glsl_lang::ast::AssignmentOp::Div },
        ast::AssignmentOp::Mod => quote! { glsl_lang::ast::AssignmentOp::Mod },
        ast::AssignmentOp::Add => quote! { glsl_lang::ast::AssignmentOp::Add },
        ast::AssignmentOp::Sub => quote! { glsl_lang::ast::AssignmentOp::Sub },
        ast::AssignmentOp::LShift => quote! { glsl_lang::ast::AssignmentOp::LShift },
        ast::AssignmentOp::RShift => quote! { glsl_lang::ast::AssignmentOp::RShift },
        ast::AssignmentOp::And => quote! { glsl_lang::ast::AssignmentOp::And },
        ast::AssignmentOp::Xor => quote! { glsl_lang::ast::AssignmentOp::Xor },
        ast::AssignmentOp::Or => quote! { AssignmentOp::Or },
    }
}

fn tokenize_function_identifier(i: &ast::FunIdentifier) -> TokenStream {
    if let Some(rs_ident) = i.as_rs_ident() {
        let ident = format_ident!("{}", rs_ident);
        return quote! { #ident };
    }

    match *i {
        ast::FunIdentifier::TypeSpecifier(ref n) => {
            let n = tokenize_type_specifier(n);
            quote! { glsl_lang::ast::FunIdentifier::TypeSpecifier(#n) }
        }

        ast::FunIdentifier::Expr(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::FunIdentifier::Expr(#e) }
        }
    }
}

fn tokenize_declaration(d: &ast::Declaration) -> TokenStream {
    let decl = match **d {
        ast::DeclarationData::FunctionPrototype(ref proto) => {
            let p = tokenize_function_prototype(proto);
            quote! { glsl_lang::ast::DeclarationData::FunctionPrototype(#p) }
        }

        ast::DeclarationData::InitDeclaratorList(ref list) => {
            let l = tokenize_init_declarator_list(list);
            quote! { glsl_lang::ast::DeclarationData::InitDeclaratorList(#l) }
        }

        ast::DeclarationData::Precision(ref qual, ref ty) => {
            let qual = tokenize_precision_qualifier(qual);
            let ty = tokenize_type_specifier(ty);
            quote! { glsl_lang::ast::DeclarationData::Precision(#qual, #ty) }
        }

        ast::DeclarationData::Block(ref block) => {
            let block = tokenize_block(block);
            quote! { glsl_lang::ast::DeclarationData::Block(#block) }
        }
    };

    let span = tokenize_span(&d.span);
    quote! { glsl_lang::ast::Declaration::new(#decl, #span) }
}

fn tokenize_function_prototype(fp: &ast::FunctionPrototype) -> TokenStream {
    let ty = tokenize_fully_specified_type(&fp.ty);
    let name = tokenize_identifier(&fp.name);
    let params = fp
        .parameters
        .iter()
        .map(tokenize_function_parameter_declaration);

    let span = tokenize_span(&fp.span);
    quote! {
      glsl_lang::ast::FunctionPrototype::new(
        glsl_lang::ast::FunctionPrototypeData {
          ty: #ty,
          name: #name,
          parameters: vec![#(#params),*]
        },
        #span
      )
    }
}

fn tokenize_function_parameter_declaration(p: &ast::FunctionParameterDeclaration) -> TokenStream {
    let decl = match **p {
        ast::FunctionParameterDeclarationData::Named(ref qual, ref fpd) => {
            let qual = qual.as_ref().map(tokenize_type_qualifier).quote();
            let fpd = tokenize_function_parameter_declarator(fpd);
            quote! { glsl_lang::ast::FunctionParameterDeclarationData::Named(#qual, #fpd) }
        }

        ast::FunctionParameterDeclarationData::Unnamed(ref qual, ref ty) => {
            let qual = qual.as_ref().map(tokenize_type_qualifier).quote();
            let ty = tokenize_type_specifier(ty);
            quote! { glsl_lang::ast::FunctionParameterDeclarationData::Unnamed(#qual, #ty) }
        }
    };

    let span = tokenize_span(&p.span);
    quote! { glsl_lang::ast::FunctionParameterDeclaration::new(#decl, #span) }
}

fn tokenize_function_parameter_declarator(p: &ast::FunctionParameterDeclarator) -> TokenStream {
    let ty = tokenize_type_specifier(&p.ty);
    let ident = tokenize_arrayed_identifier(&p.ident);

    quote! {
      glsl_lang::ast::FunctionParameterDeclarator {
        ty: #ty,
        ident: #ident
      }
    }
}

fn tokenize_init_declarator_list(i: &ast::InitDeclaratorList) -> TokenStream {
    let head = tokenize_single_declaration(&i.head);
    let tail = i.tail.iter().map(tokenize_single_declaration_no_type);

    quote! {
      glsl_lang::ast::InitDeclaratorList {
        head: #head,
        tail: vec![#(#tail),*]
      }
    }
}

fn tokenize_single_declaration(d: &ast::SingleDeclaration) -> TokenStream {
    let ty = tokenize_fully_specified_type(&d.ty);
    let name = d.name.as_ref().map(|i| tokenize_identifier(i)).quote();
    let array_specifier = d.array_specifier.as_ref().map(tokenize_array_spec).quote();
    let initializer = d.initializer.as_ref().map(tokenize_initializer).quote();

    quote! {
      glsl_lang::ast::SingleDeclaration {
        ty: #ty,
        name: #name,
        array_specifier: #array_specifier,
        initializer: #initializer
      }
    }
}

fn tokenize_single_declaration_no_type(d: &ast::SingleDeclarationNoType) -> TokenStream {
    let ident = tokenize_arrayed_identifier(&d.ident);
    let initializer = d.initializer.as_ref().map(tokenize_initializer).quote();

    quote! {
      glsl_lang::ast::SingleDeclarationNoType {
        ident: #ident,
        initializer: #initializer
      }
    }
}

fn tokenize_initializer(i: &ast::Initializer) -> TokenStream {
    match *i {
        ast::Initializer::Simple(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Initializer::Simple(#e) }
        }

        ast::Initializer::List(ref list) => {
            let l = list.iter().map(tokenize_initializer);
            quote! { glsl_lang::ast::Initializer::List(vec![#(#l),*]) }
        }
    }
}

fn tokenize_block(b: &ast::Block) -> TokenStream {
    let qual = tokenize_type_qualifier(&b.qualifier);
    let name = tokenize_identifier(&b.name);
    let fields = b.fields.iter().map(tokenize_struct_field);
    let identifier = b
        .identifier
        .as_ref()
        .map(tokenize_arrayed_identifier)
        .quote();

    quote! {
      glsl_lang::ast::Block {
        qualifier: #qual,
        name: #name,
        fields: vec![#(#fields),*],
        identifier: #identifier
      }
    }
}

fn tokenize_function_definition(fd: &ast::FunctionDefinition) -> TokenStream {
    let p = tokenize_function_prototype(&fd.prototype);
    let s = tokenize_compound_statement(&fd.statement);

    let span = tokenize_span(&fd.span);
    quote! {
      glsl_lang::ast::FunctionDefinition::new(
        glsl_lang::ast::FunctionDefinitionData {
          prototype: #p,
          statement: #s
        },
        #span
      )
    }
}

fn tokenize_compound_statement(cst: &ast::CompoundStatement) -> TokenStream {
    let s = cst.statement_list.iter().map(tokenize_statement);

    let span = tokenize_span(&cst.span);
    quote! {
      glsl_lang::ast::CompoundStatement::new(
        glsl_lang::ast::CompoundStatementData {
          statement_list: vec![#(#s),*]
        },
        #span
      )
    }
}

fn tokenize_statement(sst: &ast::Statement) -> TokenStream {
    let st = match **sst {
        ast::StatementData::Declaration(ref d) => {
            let d = tokenize_declaration(d);
            quote! { glsl_lang::ast::StatementData::Declaration(#d) }
        }

        ast::StatementData::Expression(ref e) => {
            let e = tokenize_expr_statement(e);
            quote! { glsl_lang::ast::StatementData::Expression(#e) }
        }

        ast::StatementData::Selection(ref s) => {
            let s = tokenize_selection_statement(s);
            quote! { glsl_lang::ast::StatementData::Selection(#s) }
        }

        ast::StatementData::Switch(ref s) => {
            let s = tokenize_switch_statement(s);
            quote! { glsl_lang::ast::StatementData::Switch(#s) }
        }

        ast::StatementData::CaseLabel(ref cl) => {
            let cl = tokenize_case_label(cl);
            quote! { glsl_lang::ast::StatementData::CaseLabel(#cl) }
        }

        ast::StatementData::Iteration(ref i) => {
            let i = tokenize_iteration_statement(i);
            quote! { glsl_lang::ast::StatementData::Iteration(#i) }
        }

        ast::StatementData::Jump(ref j) => {
            let j = tokenize_jump_statement(j);
            quote! { glsl_lang::ast::StatementData::Jump(#j) }
        }

        ast::StatementData::Compound(ref c) => {
            let c = tokenize_compound_statement(c);
            quote! { glsl_lang::ast::StatementData::Compound(#c) }
        }
    };

    let span = tokenize_span(&sst.span);
    quote! { glsl_lang::ast::Statement::new(#st, #span) }
}

fn tokenize_expr_statement(est: &ast::ExprStatement) -> TokenStream {
    let e = est.0.as_ref().map(|e| tokenize_expr(&e)).quote();
    quote! { glsl_lang::ast::ExprStatement(#e) }
}

fn tokenize_selection_statement(sst: &ast::SelectionStatement) -> TokenStream {
    let cond = Box::new(tokenize_expr(&sst.cond)).quote();
    let rest = tokenize_selection_rest_statement(&sst.rest);

    quote! {
      glsl_lang::ast::SelectionStatement {
        cond: #cond,
        rest: #rest
      }
    }
}

fn tokenize_selection_rest_statement(sst: &ast::SelectionRestStatement) -> TokenStream {
    match *sst {
        ast::SelectionRestStatement::Statement(ref if_st) => {
            let e = Box::new(tokenize_statement(if_st)).quote();
            quote! { glsl_lang::ast::SelectionRestStatement::Statement(#e) }
        }

        ast::SelectionRestStatement::Else(ref if_st, ref else_st) => {
            let if_st = Box::new(tokenize_statement(if_st)).quote();
            let else_st = Box::new(tokenize_statement(else_st)).quote();
            quote! { glsl_lang::ast::SelectionRestStatement::Else(#if_st, #else_st) }
        }
    }
}

fn tokenize_switch_statement(sst: &ast::SwitchStatement) -> TokenStream {
    let head = Box::new(tokenize_expr(&sst.head)).quote();
    let body = sst.body.iter().map(tokenize_statement);

    quote! {
      glsl_lang::ast::SwitchStatement {
        head: #head,
        body: vec![#(#body),*]
      }
    }
}

fn tokenize_case_label(cl: &ast::CaseLabel) -> TokenStream {
    match *cl {
        ast::CaseLabel::Case(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::CaseLabel::Case(#e) }
        }

        ast::CaseLabel::Def => quote! { glsl_lang::ast::CaseLabel::Def },
    }
}

fn tokenize_iteration_statement(ist: &ast::IterationStatement) -> TokenStream {
    match *ist {
        ast::IterationStatement::While(ref cond, ref body) => {
            let cond = tokenize_condition(cond);
            let body = Box::new(tokenize_statement(body)).quote();
            quote! { glsl_lang::ast::IterationStatement::While(#cond, #body) }
        }

        ast::IterationStatement::DoWhile(ref body, ref cond) => {
            let body = Box::new(tokenize_statement(body)).quote();
            let cond = Box::new(tokenize_expr(cond)).quote();
            quote! { glsl_lang::ast::IterationStatement::DoWhile(#body, #cond) }
        }

        ast::IterationStatement::For(ref init, ref rest, ref body) => {
            let init = tokenize_for_init_statement(init);
            let rest = tokenize_for_rest_statement(rest);
            let body = Box::new(tokenize_statement(body)).quote();
            quote! { glsl_lang::ast::IterationStatement::For(#init, #rest, #body) }
        }
    }
}

fn tokenize_condition(c: &ast::Condition) -> TokenStream {
    match *c {
        ast::Condition::Expr(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::Condition::Expr(#e) }
        }

        ast::Condition::Assignment(ref ty, ref name, ref initializer) => {
            let ty = tokenize_fully_specified_type(ty);
            let name = tokenize_identifier(name);
            let initializer = tokenize_initializer(initializer);

            quote! { glsl_lang::ast::Condition::Assignment(#ty, #name, #initializer) }
        }
    }
}

fn tokenize_for_init_statement(i: &ast::ForInitStatement) -> TokenStream {
    match *i {
        ast::ForInitStatement::Expression(ref expr) => {
            let e = expr.as_ref().map(|e| tokenize_expr(&e)).quote();
            quote! { glsl_lang::ast::ForInitStatement::Expression(#e) }
        }

        ast::ForInitStatement::Declaration(ref d) => {
            let d = Box::new(tokenize_declaration(d)).quote();
            quote! { glsl_lang::ast::ForInitStatement::Declaration(#d) }
        }
    }
}

fn tokenize_for_rest_statement(r: &ast::ForRestStatement) -> TokenStream {
    let cond = r.condition.as_ref().map(tokenize_condition).quote();
    let post = r
        .post_expr
        .as_ref()
        .map(|e| Box::new(tokenize_expr(&e)).quote())
        .quote();

    quote! {
      glsl_lang::ast::ForRestStatement {
        condition: #cond,
        post: #post
      }
    }
}

fn tokenize_jump_statement(j: &ast::JumpStatement) -> TokenStream {
    match *j {
        ast::JumpStatement::Continue => quote! { glsl_lang::ast::JumpStatement::Continue },
        ast::JumpStatement::Break => quote! { glsl_lang::ast::JumpStatement::Break },
        ast::JumpStatement::Discard => quote! { glsl_lang::ast::JumpStatement::Discard },
        ast::JumpStatement::Return(ref e) => {
            let e = e
                .as_ref()
                .map(|e| Box::new(tokenize_expr(e)).quote())
                .quote();
            quote! { glsl_lang::ast::JumpStatement::Return(#e) }
        }
    }
}

fn tokenize_preprocessor(pp: &ast::Preprocessor) -> TokenStream {
    let decl = match **pp {
        ast::PreprocessorData::Define(ref pd) => {
            let pd = tokenize_preprocessor_define(pd);
            quote! { glsl_lang::ast::PreprocessorData::Define(#pd) }
        }

        ast::PreprocessorData::Else => {
            quote! { glsl_lang::ast::PreprocessorData::Else }
        }

        ast::PreprocessorData::ElseIf(ref pei) => {
            let pei = tokenize_preprocessor_elseif(pei);
            quote! { glsl_lang::ast::PreprocessorData::ElseIf(#pei) }
        }

        ast::PreprocessorData::EndIf => {
            quote! { glsl_lang::ast::PreprocessorData::EndIf }
        }

        ast::PreprocessorData::Error(ref pe) => {
            let pe = tokenize_preprocessor_error(pe);
            quote! { glsl_lang::ast::PreprocessorData::Error(#pe) }
        }

        ast::PreprocessorData::If(ref pi) => {
            let pi = tokenize_preprocessor_if(pi);
            quote! { glsl_lang::ast::PreprocessorData::If(#pi) }
        }

        ast::PreprocessorData::IfDef(ref pid) => {
            let pid = tokenize_preprocessor_ifdef(pid);
            quote! { glsl_lang::ast::PreprocessorData::IfDef(#pid) }
        }

        ast::PreprocessorData::IfNDef(ref pind) => {
            let pind = tokenize_preprocessor_ifndef(pind);
            quote! { glsl_lang::ast::PreprocessorData::IfNDef(#pind) }
        }

        ast::PreprocessorData::Include(ref pi) => {
            let pi = tokenize_preprocessor_include(pi);
            quote! { glsl_lang::ast::PreprocessorData::Include(#pi) }
        }

        ast::PreprocessorData::Line(ref pl) => {
            let pl = tokenize_preprocessor_line(pl);
            quote! { glsl_lang::ast::PreprocessorData::Line(#pl) }
        }

        ast::PreprocessorData::Pragma(ref pp) => {
            let pp = tokenize_preprocessor_pragma(pp);
            quote! { glsl_lang::ast::PreprocessorData::Pragma(#pp) }
        }

        ast::PreprocessorData::Undef(ref pu) => {
            let pu = tokenize_preprocessor_undef(pu);
            quote! { glsl_lang::ast::PreprocessorData::Undef(#pu) }
        }

        ast::PreprocessorData::Version(ref pv) => {
            let pv = tokenize_preprocessor_version(pv);
            quote! { glsl_lang::ast::PreprocessorData::Version(#pv) }
        }

        ast::PreprocessorData::Extension(ref pe) => {
            let pe = tokenize_preprocessor_extension(pe);
            quote! { glsl_lang::ast::PreprocessorData::Extension(#pe) }
        }
    };

    let span = tokenize_span(&pp.span);
    quote! { glsl_lang::ast::Preprocessor::new(#decl, #span) }
}

fn tokenize_preprocessor_define(pd: &ast::PreprocessorDefine) -> TokenStream {
    match *pd {
        ast::PreprocessorDefine::ObjectLike {
            ref ident,
            ref value,
        } => {
            let ident = tokenize_identifier(ident);
            let value = value.quote();

            quote! {
              glsl_lang::ast::PreprocessorDefine::ObjectLike {
                ident: #ident,
                value: #value
              }
            }
        }

        ast::PreprocessorDefine::FunctionLike {
            ref ident,
            ref args,
            ref value,
        } => {
            let ident = tokenize_identifier(ident);
            let args = args.iter().map(tokenize_identifier);
            let value = value.quote();

            quote! {
              glsl_lang::ast::PreprocessorDefine::FunctionLike {
                ident: #ident,
                args: vec![#(#args),*],
                value: #value
              }
            }
        }
    }
}

fn tokenize_preprocessor_elseif(pei: &ast::PreprocessorElseIf) -> TokenStream {
    let condition = pei.condition.quote();

    quote! {
      glsl_lang::ast::PreprocessorElseIf {
        condition: #condition
      }
    }
}

fn tokenize_preprocessor_error(pe: &ast::PreprocessorError) -> TokenStream {
    let message = &pe.message;

    quote! {
      glsl_lang::ast::PreprocessorError {
        message: #message.to_owned()
      }
    }
}

fn tokenize_preprocessor_if(pi: &ast::PreprocessorIf) -> TokenStream {
    let condition = pi.condition.quote();

    quote! {
      glsl_lang::ast::PreprocessorIf {
        condition: #condition
      }
    }
}

fn tokenize_preprocessor_ifdef(pid: &ast::PreprocessorIfDef) -> TokenStream {
    let ident = tokenize_identifier(&pid.ident);

    quote! {
      glsl_lang::ast::PreprocessorIfDef {
        ident: #ident
      }
    }
}

fn tokenize_preprocessor_ifndef(pind: &ast::PreprocessorIfNDef) -> TokenStream {
    let ident = tokenize_identifier(&pind.ident);

    quote! {
      glsl_lang::ast::PreprocessorIfNDef {
        ident: #ident
      }
    }
}

fn tokenize_preprocessor_include(pi: &ast::PreprocessorInclude) -> TokenStream {
    let path = tokenize_path(&pi.path);

    quote! {
      glsl_lang::ast::PreprocessorInclude {
        path: #path
      }
    }
}

fn tokenize_preprocessor_line(pl: &ast::PreprocessorLine) -> TokenStream {
    let line = pl.line;
    let source_string_number = pl.source_string_number.quote();

    quote! {
      glsl_lang::ast::PreprocessorLine {
        line: #line,
        source_string_number: #source_string_number
      }
    }
}

fn tokenize_preprocessor_pragma(pp: &ast::PreprocessorPragma) -> TokenStream {
    let command = &pp.command;

    quote! {
      glsl_lang::ast::PreprocessorPragma {
        command: #command.to_owned()
      }
    }
}

fn tokenize_preprocessor_undef(pu: &ast::PreprocessorUndef) -> TokenStream {
    let name = tokenize_identifier(&pu.name);

    quote! {
      glsl_lang::ast::PreprocessorUndef {
        name: #name
      }
    }
}

fn tokenize_preprocessor_version(pv: &ast::PreprocessorVersion) -> TokenStream {
    let version = pv.version;
    let profile = pv
        .profile
        .as_ref()
        .map(tokenize_preprocessor_version_profile)
        .quote();

    quote! {
      glsl_lang::ast::PreprocessorVersion {
        version: #version,
        profile: #profile
      }
    }
}

fn tokenize_preprocessor_version_profile(profile: &ast::PreprocessorVersionProfile) -> TokenStream {
    match *profile {
        ast::PreprocessorVersionProfile::Core => {
            quote! { glsl_lang::ast::PreprocessorVersionProfile::Core }
        }
        ast::PreprocessorVersionProfile::Compatibility => {
            quote! { glsl_lang::ast::PreprocessorVersionProfile::Compatibility }
        }
        ast::PreprocessorVersionProfile::Es => {
            quote! { glsl_lang::ast::PreprocessorVersionProfile::Es }
        }
    }
}

fn tokenize_preprocessor_extension(pe: &ast::PreprocessorExtension) -> TokenStream {
    let name = tokenize_preprocessor_extension_name(&pe.name);
    let behavior = pe
        .behavior
        .as_ref()
        .map(tokenize_preprocessor_extension_behavior)
        .quote();

    quote! {
      glsl_lang::ast::PreprocessorExtension {
        name: #name,
        behavior: #behavior
      }
    }
}

fn tokenize_preprocessor_extension_name(name: &ast::PreprocessorExtensionName) -> TokenStream {
    match *name {
        ast::PreprocessorExtensionName::All => {
            quote! { glsl_lang::ast::PreprocessorExtensionName::All }
        }
        ast::PreprocessorExtensionName::Specific(ref n) => {
            let n = n.quote();
            quote! { glsl_lang::ast::PreprocessorExtensionName::Specific(#n) }
        }
    }
}

fn tokenize_preprocessor_extension_behavior(
    behavior: &ast::PreprocessorExtensionBehavior,
) -> TokenStream {
    match *behavior {
        ast::PreprocessorExtensionBehavior::Require => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehavior::Require }
        }
        ast::PreprocessorExtensionBehavior::Enable => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehavior::Enable }
        }
        ast::PreprocessorExtensionBehavior::Warn => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehavior::Warn }
        }
        ast::PreprocessorExtensionBehavior::Disable => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehavior::Disable }
        }
    }
}

fn tokenize_span(s: &Option<ast::NodeSpan>) -> TokenStream {
    if let Some(s) = s {
        let ast::NodeSpan {
            source_id,
            start,
            end,
        } = s;

        quote! { Some(glsl_lang::ast::NodeSpan { source_id: #source_id, start: #start, end: #end }) }
    } else {
        quote! { None }
    }
}

fn tokenize_external_declaration(ed: &ast::ExternalDeclaration) -> TokenStream {
    let contents = match **ed {
        ast::ExternalDeclarationData::Preprocessor(ref pp) => {
            let pp = tokenize_preprocessor(pp);
            quote! { glsl_lang::ast::ExternalDeclarationData::Preprocessor(#pp) }
        }

        ast::ExternalDeclarationData::FunctionDefinition(ref fd) => {
            let fd = tokenize_function_definition(fd);
            quote! { glsl_lang::ast::ExternalDeclarationData::FunctionDefinition(#fd) }
        }

        ast::ExternalDeclarationData::Declaration(ref d) => {
            let d = tokenize_declaration(d);
            quote! { glsl_lang::ast::ExternalDeclarationData::Declaration(#d) }
        }
    };

    let span = tokenize_span(&ed.span);
    quote! { glsl_lang::ast::Node::new(#contents, #span) }
}

fn tokenize_translation_unit(tu: &ast::TranslationUnit) -> TokenStream {
    let tu = (tu.0).iter().map(|d| tokenize_external_declaration(&d));
    quote! { glsl_lang::ast::TranslationUnit(vec![#(#tu),*]) }
}
