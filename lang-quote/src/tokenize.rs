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
    let span = tokenize_span(&p.span);
    let p = &p.content;

    match p {
        ast::PathData::Absolute(ref s) => {
            quote! { glsl_lang::ast::Path::new(glsl_lang::ast::PathData::Absolute(#s.to_owned()), #span) }
        }
        ast::PathData::Relative(ref s) => {
            quote! { glsl_lang::ast::Path::new(glsl_lang::ast::PathData::Relative(#s.to_owned()), #span) }
        }
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
    let span = tokenize_span(&t.span);
    let t = match **t {
        ast::TypeSpecifierNonArrayData::Void => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Void }
        }
        ast::TypeSpecifierNonArrayData::Bool => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Bool }
        }
        ast::TypeSpecifierNonArrayData::Int => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Int }
        }
        ast::TypeSpecifierNonArrayData::UInt => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UInt }
        }
        ast::TypeSpecifierNonArrayData::Float => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Float }
        }
        ast::TypeSpecifierNonArrayData::Double => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Double }
        }
        ast::TypeSpecifierNonArrayData::Vec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Vec2 }
        }
        ast::TypeSpecifierNonArrayData::Vec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Vec3 }
        }
        ast::TypeSpecifierNonArrayData::Vec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Vec4 }
        }
        ast::TypeSpecifierNonArrayData::DVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DVec2 }
        }
        ast::TypeSpecifierNonArrayData::DVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DVec3 }
        }
        ast::TypeSpecifierNonArrayData::DVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DVec4 }
        }
        ast::TypeSpecifierNonArrayData::BVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::BVec2 }
        }
        ast::TypeSpecifierNonArrayData::BVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::BVec3 }
        }
        ast::TypeSpecifierNonArrayData::BVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::BVec4 }
        }
        ast::TypeSpecifierNonArrayData::IVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IVec2 }
        }
        ast::TypeSpecifierNonArrayData::IVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IVec3 }
        }
        ast::TypeSpecifierNonArrayData::IVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IVec4 }
        }
        ast::TypeSpecifierNonArrayData::UVec2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UVec2 }
        }
        ast::TypeSpecifierNonArrayData::UVec3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UVec3 }
        }
        ast::TypeSpecifierNonArrayData::UVec4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UVec4 }
        }
        ast::TypeSpecifierNonArrayData::Mat2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat2 }
        }
        ast::TypeSpecifierNonArrayData::Mat3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat3 }
        }
        ast::TypeSpecifierNonArrayData::Mat4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat4 }
        }
        ast::TypeSpecifierNonArrayData::Mat22 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat22 }
        }
        ast::TypeSpecifierNonArrayData::Mat23 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat23 }
        }
        ast::TypeSpecifierNonArrayData::Mat24 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat24 }
        }
        ast::TypeSpecifierNonArrayData::Mat32 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat32 }
        }
        ast::TypeSpecifierNonArrayData::Mat33 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat33 }
        }
        ast::TypeSpecifierNonArrayData::Mat34 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat34 }
        }
        ast::TypeSpecifierNonArrayData::Mat42 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat42 }
        }
        ast::TypeSpecifierNonArrayData::Mat43 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat43 }
        }
        ast::TypeSpecifierNonArrayData::Mat44 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Mat44 }
        }
        ast::TypeSpecifierNonArrayData::DMat2 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat2 }
        }
        ast::TypeSpecifierNonArrayData::DMat3 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat3 }
        }
        ast::TypeSpecifierNonArrayData::DMat4 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat4 }
        }
        ast::TypeSpecifierNonArrayData::DMat22 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat22 }
        }
        ast::TypeSpecifierNonArrayData::DMat23 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat23 }
        }
        ast::TypeSpecifierNonArrayData::DMat24 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat24 }
        }
        ast::TypeSpecifierNonArrayData::DMat32 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat32 }
        }
        ast::TypeSpecifierNonArrayData::DMat33 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat33 }
        }
        ast::TypeSpecifierNonArrayData::DMat34 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat34 }
        }
        ast::TypeSpecifierNonArrayData::DMat42 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat42 }
        }
        ast::TypeSpecifierNonArrayData::DMat43 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat43 }
        }
        ast::TypeSpecifierNonArrayData::DMat44 => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::DMat44 }
        }
        ast::TypeSpecifierNonArrayData::Sampler1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler1D }
        }
        ast::TypeSpecifierNonArrayData::Image1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image1D }
        }
        ast::TypeSpecifierNonArrayData::Sampler2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2D }
        }
        ast::TypeSpecifierNonArrayData::Image2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image2D }
        }
        ast::TypeSpecifierNonArrayData::Sampler3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler3D }
        }
        ast::TypeSpecifierNonArrayData::Image3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image3D }
        }
        ast::TypeSpecifierNonArrayData::SamplerCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::SamplerCube }
        }
        ast::TypeSpecifierNonArrayData::ImageCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ImageCube }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DRect }
        }
        ast::TypeSpecifierNonArrayData::Image2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image2DRect }
        }
        ast::TypeSpecifierNonArrayData::Sampler1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler1DArray }
        }
        ast::TypeSpecifierNonArrayData::Image1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image1DArray }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DArray }
        }
        ast::TypeSpecifierNonArrayData::Image2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image2DArray }
        }
        ast::TypeSpecifierNonArrayData::SamplerBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::SamplerBuffer }
        }
        ast::TypeSpecifierNonArrayData::ImageBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ImageBuffer }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DMs }
        }
        ast::TypeSpecifierNonArrayData::Image2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image2DMs }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DMsArray }
        }
        ast::TypeSpecifierNonArrayData::Image2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Image2DMsArray }
        }
        ast::TypeSpecifierNonArrayData::SamplerCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::SamplerCubeArray }
        }
        ast::TypeSpecifierNonArrayData::ImageCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ImageCubeArray }
        }
        ast::TypeSpecifierNonArrayData::Sampler1DShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler1DShadow }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DShadow }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DRectShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DRectShadow }
        }
        ast::TypeSpecifierNonArrayData::Sampler1DArrayShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler1DArrayShadow }
        }
        ast::TypeSpecifierNonArrayData::Sampler2DArrayShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Sampler2DArrayShadow }
        }
        ast::TypeSpecifierNonArrayData::SamplerCubeShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::SamplerCubeShadow }
        }
        ast::TypeSpecifierNonArrayData::SamplerCubeArrayShadow => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::SamplerCubeArrayShadow }
        }
        ast::TypeSpecifierNonArrayData::ISampler1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler1D }
        }
        ast::TypeSpecifierNonArrayData::IImage1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage1D }
        }
        ast::TypeSpecifierNonArrayData::ISampler2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler2D }
        }
        ast::TypeSpecifierNonArrayData::IImage2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage2D }
        }
        ast::TypeSpecifierNonArrayData::ISampler3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler3D }
        }
        ast::TypeSpecifierNonArrayData::IImage3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage3D }
        }
        ast::TypeSpecifierNonArrayData::ISamplerCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISamplerCube }
        }
        ast::TypeSpecifierNonArrayData::IImageCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImageCube }
        }
        ast::TypeSpecifierNonArrayData::ISampler2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler2DRect }
        }
        ast::TypeSpecifierNonArrayData::IImage2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage2DRect }
        }
        ast::TypeSpecifierNonArrayData::ISampler1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler1DArray }
        }
        ast::TypeSpecifierNonArrayData::IImage1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage1DArray }
        }
        ast::TypeSpecifierNonArrayData::ISampler2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler2DArray }
        }
        ast::TypeSpecifierNonArrayData::IImage2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage2DArray }
        }
        ast::TypeSpecifierNonArrayData::ISamplerBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISamplerBuffer }
        }
        ast::TypeSpecifierNonArrayData::IImageBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImageBuffer }
        }
        ast::TypeSpecifierNonArrayData::ISampler2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler2DMs }
        }
        ast::TypeSpecifierNonArrayData::IImage2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage2DMs }
        }
        ast::TypeSpecifierNonArrayData::ISampler2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISampler2DMsArray }
        }
        ast::TypeSpecifierNonArrayData::IImage2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImage2DMsArray }
        }
        ast::TypeSpecifierNonArrayData::ISamplerCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::ISamplerCubeArray }
        }
        ast::TypeSpecifierNonArrayData::IImageCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::IImageCubeArray }
        }
        ast::TypeSpecifierNonArrayData::AtomicUInt => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::AtomicUInt }
        }
        ast::TypeSpecifierNonArrayData::USampler1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler1D }
        }
        ast::TypeSpecifierNonArrayData::UImage1D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage1D }
        }
        ast::TypeSpecifierNonArrayData::USampler2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler2D }
        }
        ast::TypeSpecifierNonArrayData::UImage2D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage2D }
        }
        ast::TypeSpecifierNonArrayData::USampler3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler3D }
        }
        ast::TypeSpecifierNonArrayData::UImage3D => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage3D }
        }
        ast::TypeSpecifierNonArrayData::USamplerCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USamplerCube }
        }
        ast::TypeSpecifierNonArrayData::UImageCube => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImageCube }
        }
        ast::TypeSpecifierNonArrayData::USampler2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler2DRect }
        }
        ast::TypeSpecifierNonArrayData::UImage2DRect => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage2DRect }
        }
        ast::TypeSpecifierNonArrayData::USampler1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler1DArray }
        }
        ast::TypeSpecifierNonArrayData::UImage1DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage1DArray }
        }
        ast::TypeSpecifierNonArrayData::USampler2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler2DArray }
        }
        ast::TypeSpecifierNonArrayData::UImage2DArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage2DArray }
        }
        ast::TypeSpecifierNonArrayData::USamplerBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USamplerBuffer }
        }
        ast::TypeSpecifierNonArrayData::UImageBuffer => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImageBuffer }
        }
        ast::TypeSpecifierNonArrayData::USampler2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler2DMs }
        }
        ast::TypeSpecifierNonArrayData::UImage2DMs => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage2DMs }
        }
        ast::TypeSpecifierNonArrayData::USampler2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USampler2DMsArray }
        }
        ast::TypeSpecifierNonArrayData::UImage2DMsArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImage2DMsArray }
        }
        ast::TypeSpecifierNonArrayData::USamplerCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::USamplerCubeArray }
        }
        ast::TypeSpecifierNonArrayData::UImageCubeArray => {
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::UImageCubeArray }
        }

        ast::TypeSpecifierNonArrayData::Struct(ref s) => {
            let s = tokenize_struct_non_declaration(s);
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::Struct(#s) }
        }

        ast::TypeSpecifierNonArrayData::TypeName(ref tn) => {
            let tn = tokenize_type_name(tn);
            quote! { glsl_lang::ast::TypeSpecifierNonArrayData::TypeName(#tn) }
        }
    };

    quote! { glsl_lang::ast::TypeSpecifierNonArray::new(#t, #span) }
}

fn tokenize_type_specifier(t: &ast::TypeSpecifier) -> TokenStream {
    let span = tokenize_span(&t.span);
    let data = {
        let ty = tokenize_type_specifier_non_array(&t.ty);
        let array_specifier = t.array_specifier.as_ref().map(tokenize_array_spec).quote();

        quote! {
          glsl_lang::ast::TypeSpecifierData {
            ty: #ty,
            array_specifier: #array_specifier
          }
        }
    };

    quote! {
        glsl_lang::ast::TypeSpecifier::new(#data, #span)
    }
}

fn tokenize_fully_specified_type(t: &ast::FullySpecifiedType) -> TokenStream {
    let span = tokenize_span(&t.span);
    let t = {
        let qual = t.qualifier.as_ref().map(tokenize_type_qualifier).quote();
        let ty = tokenize_type_specifier(&t.ty);

        quote! {
          glsl_lang::ast::FullySpecifiedTypeData {
            qualifier: #qual,
            ty: #ty
          }
        }
    };

    quote! { glsl_lang::ast::FullySpecifiedType::new(#t, #span) }
}

fn tokenize_struct_non_declaration(s: &ast::StructSpecifier) -> TokenStream {
    let span = tokenize_span(&s.span);
    let s = {
        let name = s.name.as_ref().map(tokenize_type_name);
        let fields = s.fields.iter().map(tokenize_struct_field);

        quote! {
          glsl_lang::ast::StructSpecifierData {
            name: Some(#name),
            fields: vec![#(#fields),*]
          }
        }
    };

    quote! { glsl_lang::ast::StructSpecifier::new(#s, #span) }
}

fn tokenize_struct_field(field: &ast::StructFieldSpecifier) -> TokenStream {
    let span = tokenize_span(&field.span);
    let field = {
        let qual = field
            .qualifier
            .as_ref()
            .map(tokenize_type_qualifier)
            .quote();
        let ty = tokenize_type_specifier(&field.ty);
        let identifiers = field.identifiers.iter().map(tokenize_arrayed_identifier);

        quote! {
          glsl_lang::ast::StructFieldSpecifierData {
            qualifier: #qual,
            ty: #ty,
            identifiers: vec![#(#identifiers),*]
          }
        }
    };

    quote! { glsl_lang::ast::StructFieldSpecifier::new(#field, #span) }
}

fn tokenize_array_spec(a: &ast::ArraySpecifier) -> TokenStream {
    let span = tokenize_span(&a.span);
    let a = {
        let dimensions = a.dimensions.iter().map(tokenize_array_spec_dim);

        quote! {
          glsl_lang::ast::ArraySpecifierData { dimensions: vec![#(#dimensions),*] }
        }
    };

    quote! { glsl_lang::ast::ArraySpecifier::new(#a, #span) }
}

fn tokenize_array_spec_dim(a: &ast::ArraySpecifierDimension) -> TokenStream {
    let span = tokenize_span(&a.span);
    let a = match a.content {
        ast::ArraySpecifierDimensionData::Unsized => {
            quote! { glsl_lang::ast::ArraySpecifierDimensionData::Unsized }
        }
        ast::ArraySpecifierDimensionData::ExplicitlySized(ref e) => {
            let expr = Box::new(tokenize_expr(&e)).quote();
            quote! { glsl_lang::ast::ArraySpecifierDimensionData::ExplicitlySized(#expr) }
        }
    };

    quote! { glsl_lang::ast::ArraySpecifierDimension::new(#a, #span) }
}

fn tokenize_arrayed_identifier(identifier: &ast::ArrayedIdentifier) -> TokenStream {
    let span = tokenize_span(&identifier.span);
    let identifier = {
        let ident = tokenize_identifier(&identifier.ident);
        let array_spec = identifier
            .array_spec
            .as_ref()
            .map(tokenize_array_spec)
            .quote();

        quote! {
          glsl_lang::ast::ArrayedIdentifierData::new(#ident, #array_spec)
        }
    };

    quote! { glsl_lang::ast::ArrayedIdentifier::new(#identifier, #span) }
}

fn tokenize_type_qualifier(q: &ast::TypeQualifier) -> TokenStream {
    let span = tokenize_span(&q.span);
    let q = {
        let quals = q.qualifiers.iter().map(tokenize_type_qualifier_spec);

        quote! {
          glsl_lang::ast::TypeQualifierData {
            qualifiers: vec![#(#quals),*]
          }
        }
    };

    quote! { glsl_lang::ast::TypeQualifier::new(#q, #span) }
}

fn tokenize_type_qualifier_spec(q: &ast::TypeQualifierSpec) -> TokenStream {
    let span = tokenize_span(&q.span);
    let q = match **q {
        ast::TypeQualifierSpecData::Storage(ref s) => {
            let s = tokenize_storage_qualifier(s);
            quote! { glsl_lang::ast::TypeQualifierSpecData::Storage(#s) }
        }

        ast::TypeQualifierSpecData::Layout(ref l) => {
            let l = tokenize_layout_qualifier(l);
            quote! { glsl_lang::ast::TypeQualifierSpecData::Layout(#l) }
        }

        ast::TypeQualifierSpecData::Precision(ref p) => {
            let p = tokenize_precision_qualifier(p);
            quote! { glsl_lang::ast::TypeQualifierSpecData::Precision(#p) }
        }

        ast::TypeQualifierSpecData::Interpolation(ref i) => {
            let i = tokenize_interpolation_qualifier(i);
            quote! { glsl_lang::ast::TypeQualifierSpecData::Interpolation(#i) }
        }

        ast::TypeQualifierSpecData::Invariant => {
            quote! { glsl_lang::ast::TypeQualifierSpecData::Invariant }
        }

        ast::TypeQualifierSpecData::Precise => {
            quote! { glsl_lang::ast::TypeQualifierSpecData::Precise }
        }
    };

    quote! { glsl_lang::ast::TypeQualifierSpec::new(#q, #span) }
}

fn tokenize_storage_qualifier(q: &ast::StorageQualifier) -> TokenStream {
    let span = tokenize_span(&q.span);
    let q = match **q {
        ast::StorageQualifierData::Const => {
            quote! { glsl_lang::ast::StorageQualifierData::Const }
        }
        ast::StorageQualifierData::InOut => {
            quote! { glsl_lang::ast::StorageQualifierData::InOut }
        }
        ast::StorageQualifierData::In => quote! { glsl_lang::ast::StorageQualifierData::In },
        ast::StorageQualifierData::Out => quote! { glsl_lang::ast::StorageQualifierData::Out },
        ast::StorageQualifierData::Centroid => {
            quote! { glsl_lang::ast::StorageQualifierData::Centroid }
        }
        ast::StorageQualifierData::Patch => {
            quote! { glsl_lang::ast::StorageQualifierData::Patch }
        }
        ast::StorageQualifierData::Sample => {
            quote! { glsl_lang::ast::StorageQualifierData::Sample }
        }
        ast::StorageQualifierData::Uniform => {
            quote! { glsl_lang::ast::StorageQualifierData::Uniform }
        }
        ast::StorageQualifierData::Buffer => {
            quote! { glsl_lang::ast::StorageQualifierData::Buffer }
        }
        ast::StorageQualifierData::Shared => {
            quote! { glsl_lang::ast::StorageQualifierData::Shared }
        }
        ast::StorageQualifierData::Coherent => {
            quote! { glsl_lang::ast::StorageQualifierData::Coherent }
        }
        ast::StorageQualifierData::Volatile => {
            quote! { glsl_lang::ast::StorageQualifierData::Volatile }
        }
        ast::StorageQualifierData::Restrict => {
            quote! { glsl_lang::ast::StorageQualifierData::Restrict }
        }
        ast::StorageQualifierData::ReadOnly => {
            quote! { glsl_lang::ast::StorageQualifierData::ReadOnly }
        }
        ast::StorageQualifierData::WriteOnly => {
            quote! { glsl_lang::ast::StorageQualifierData::WriteOnly }
        }

        ast::StorageQualifierData::Subroutine(ref n) => {
            let n = n.iter().map(|t| tokenize_type_specifier(t));

            quote! {
                glsl_lang::ast::StorageQualifierData::Subroutine(vec![#(#n),*])
            }
        }
    };

    quote! { glsl_lang::ast::StorageQualifier::new(#q, #span) }
}

fn tokenize_layout_qualifier(l: &ast::LayoutQualifier) -> TokenStream {
    let span = tokenize_span(&l.span);
    let l = {
        let ids = l.ids.iter().map(tokenize_layout_qualifier_spec);

        quote! {
          glsl_lang::ast::LayoutQualifierData {
            ids: vec![#(#ids),*]
          }
        }
    };

    quote! { glsl_lang::ast::LayoutQualifier::new(#l, #span) }
}

fn tokenize_layout_qualifier_spec(l: &ast::LayoutQualifierSpec) -> TokenStream {
    let span = tokenize_span(&l.span);
    let l = match l.content {
        ast::LayoutQualifierSpecData::Identifier(ref i, ref e) => {
            let i = tokenize_identifier(i);
            let expr = e
                .as_ref()
                .map(|e| Box::new(tokenize_expr(&e)).quote())
                .quote();
            quote! { glsl_lang::ast::LayoutQualifierSpecData::Identifier(#i, #expr) }
        }

        ast::LayoutQualifierSpecData::Shared => {
            quote! { glsl_lang::ast::LayoutQualifierSpecData::Shared }
        }
    };

    quote! { glsl_lang::ast::LayoutQualifierSpec::new(#l, #span) }
}

fn tokenize_precision_qualifier(p: &ast::PrecisionQualifier) -> TokenStream {
    let span = tokenize_span(&p.span);
    let p = match p.content {
        ast::PrecisionQualifierData::High => {
            quote! { glsl_lang::ast::PrecisionQualifierData::High }
        }
        ast::PrecisionQualifierData::Medium => {
            quote! { glsl_lang::ast::PrecisionQualifierData::Medium }
        }
        ast::PrecisionQualifierData::Low => quote! { glsl_lang::ast::PrecisionQualifierData::Low },
    };

    quote! { glsl_lang::ast::PrecisionQualifier::new(#p, #span) }
}

fn tokenize_interpolation_qualifier(i: &ast::InterpolationQualifier) -> TokenStream {
    let span = tokenize_span(&i.span);
    let i = match i.content {
        ast::InterpolationQualifierData::Smooth => {
            quote! { glsl_lang::ast::InterpolationQualifierData::Smooth }
        }
        ast::InterpolationQualifierData::Flat => {
            quote! { glsl_lang::ast::InterpolationQualifierData::Flat }
        }
        ast::InterpolationQualifierData::NoPerspective => {
            quote! { glsl_lang::ast::InterpolationQualifierData::NoPerspective }
        }
    };

    quote! { glsl_lang::ast::InterpolationQualifier::new(#i, #span) }
}

fn tokenize_expr(expr: &ast::Expr) -> TokenStream {
    if let Some(rs_ident) = expr.as_rs_ident() {
        let ident = format_ident!("{}", rs_ident);
        return quote! { #ident };
    }

    let span = tokenize_span(&expr.span);
    let expr = match expr.content {
        ast::ExprData::Variable(ref i) => {
            let i = tokenize_identifier(i);
            quote! { glsl_lang::ast::ExprData::Variable(#i) }
        }

        ast::ExprData::IntConst(ref x) => quote! { glsl_lang::ast::ExprData::IntConst(#x) },

        ast::ExprData::UIntConst(ref x) => quote! { glsl_lang::ast::ExprData::UIntConst(#x) },

        ast::ExprData::BoolConst(ref x) => quote! { glsl_lang::ast::ExprData::BoolConst(#x) },

        ast::ExprData::FloatConst(ref x) => quote! { glsl_lang::ast::ExprData::FloatConst(#x) },

        ast::ExprData::DoubleConst(ref x) => quote! { glsl_lang::ast::ExprData::DoubleConst(#x) },

        ast::ExprData::Unary(ref op, ref e) => {
            let op = tokenize_unary_op(op);
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::ExprData::Unary(#op, #e) }
        }

        ast::ExprData::Binary(ref op, ref l, ref r) => {
            let op = tokenize_binary_op(op);
            let l = Box::new(tokenize_expr(l)).quote();
            let r = Box::new(tokenize_expr(r)).quote();
            quote! { glsl_lang::ast::ExprData::Binary(#op, #l, #r) }
        }

        ast::ExprData::Ternary(ref c, ref s, ref e) => {
            let c = Box::new(tokenize_expr(c)).quote();
            let s = Box::new(tokenize_expr(s)).quote();
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::ExprData::Ternary(#c, #s, #e) }
        }

        ast::ExprData::Assignment(ref v, ref op, ref e) => {
            let v = Box::new(tokenize_expr(v)).quote();
            let op = tokenize_assignment_op(op);
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::ExprData::Assignment(#v, #op, #e) }
        }

        ast::ExprData::Bracket(ref e, ref a) => {
            let e = Box::new(tokenize_expr(e)).quote();
            let a = tokenize_expr(a);
            quote! { glsl_lang::ast::ExprData::Bracket(#e, #a) }
        }

        ast::ExprData::FunCall(ref fun, ref args) => {
            let fun = tokenize_function_identifier(fun);
            let args = args.iter().map(tokenize_expr);
            quote! { glsl_lang::ast::ExprData::FunCall(#fun, vec![#(#args),*]) }
        }

        ast::ExprData::Dot(ref e, ref i) => {
            let e = Box::new(tokenize_expr(e)).quote();
            let i = tokenize_identifier(i);

            quote! { glsl_lang::ast::ExprData::Dot(#e, #i) }
        }

        ast::ExprData::PostInc(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::ExprData::PostInc(#e) }
        }

        ast::ExprData::PostDec(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::ExprData::PostDec(#e) }
        }

        ast::ExprData::Comma(ref a, ref b) => {
            let a = Box::new(tokenize_expr(a)).quote();
            let b = Box::new(tokenize_expr(b)).quote();
            quote! { glsl_lang::ast::ExprData::Comma(#a, #b) }
        }
    };

    quote! { glsl_lang::ast::Expr::new(#expr, #span) }
}

fn tokenize_unary_op(op: &ast::UnaryOp) -> TokenStream {
    let span = tokenize_span(&op.span);
    let op = match op.content {
        ast::UnaryOpData::Inc => quote! { glsl_lang::ast::UnaryOpData::Inc },
        ast::UnaryOpData::Dec => quote! { glsl_lang::ast::UnaryOpData::Dec },
        ast::UnaryOpData::Add => quote! { glsl_lang::ast::UnaryOpData::Add },
        ast::UnaryOpData::Minus => quote! { glsl_lang::ast::UnaryOpData::Minus },
        ast::UnaryOpData::Not => quote! { glsl_lang::ast::UnaryOpData::Not },
        ast::UnaryOpData::Complement => quote! { glsl_lang::ast::UnaryOpData::Complement },
    };

    quote! { glsl_lang::ast::UnaryOp::new(#op, #span) }
}

fn tokenize_binary_op(op: &ast::BinaryOp) -> TokenStream {
    let span = tokenize_span(&op.span);
    let op = match op.content {
        ast::BinaryOpData::Or => quote! { glsl_lang::ast::BinaryOpData::Or },
        ast::BinaryOpData::Xor => quote! { glsl_lang::ast::BinaryOpData::Xor },
        ast::BinaryOpData::And => quote! { glsl_lang::ast::BinaryOpData::And },
        ast::BinaryOpData::BitOr => quote! { glsl_lang::ast::BinaryOpData::BitOr },
        ast::BinaryOpData::BitXor => quote! { glsl_lang::ast::BinaryOpData::BitXor },
        ast::BinaryOpData::BitAnd => quote! { glsl_lang::ast::BinaryOpData::BitAnd },
        ast::BinaryOpData::Equal => quote! { glsl_lang::ast::BinaryOpData::Equal },
        ast::BinaryOpData::NonEqual => quote! { glsl_lang::ast::BinaryOpData::NonEqual },
        ast::BinaryOpData::Lt => quote! { glsl_lang::ast::BinaryOpData::Lt },
        ast::BinaryOpData::Gt => quote! { glsl_lang::ast::BinaryOpData::Gt },
        ast::BinaryOpData::Lte => quote! { glsl_lang::ast::BinaryOpData::Lte },
        ast::BinaryOpData::Gte => quote! { glsl_lang::ast::BinaryOpData::Gte },
        ast::BinaryOpData::LShift => quote! { glsl_lang::ast::BinaryOpData::LShift },
        ast::BinaryOpData::RShift => quote! { glsl_lang::ast::BinaryOpData::RShift },
        ast::BinaryOpData::Add => quote! { glsl_lang::ast::BinaryOpData::Add },
        ast::BinaryOpData::Sub => quote! { glsl_lang::ast::BinaryOpData::Sub },
        ast::BinaryOpData::Mult => quote! { glsl_lang::ast::BinaryOpData::Mult },
        ast::BinaryOpData::Div => quote! { glsl_lang::ast::BinaryOpData::Div },
        ast::BinaryOpData::Mod => quote! { glsl_lang::ast::BinaryOpData::Mod },
    };

    quote! { glsl_lang::ast::BinaryOp::new(#op, #span) }
}

fn tokenize_assignment_op(op: &ast::AssignmentOp) -> TokenStream {
    let span = tokenize_span(&op.span);
    let op = match op.content {
        ast::AssignmentOpData::Equal => quote! { glsl_lang::ast::AssignmentOpData::Equal },
        ast::AssignmentOpData::Mult => quote! { glsl_lang::ast::AssignmentOpData::Mult },
        ast::AssignmentOpData::Div => quote! { glsl_lang::ast::AssignmentOpData::Div },
        ast::AssignmentOpData::Mod => quote! { glsl_lang::ast::AssignmentOpData::Mod },
        ast::AssignmentOpData::Add => quote! { glsl_lang::ast::AssignmentOpData::Add },
        ast::AssignmentOpData::Sub => quote! { glsl_lang::ast::AssignmentOpData::Sub },
        ast::AssignmentOpData::LShift => quote! { glsl_lang::ast::AssignmentOpData::LShift },
        ast::AssignmentOpData::RShift => quote! { glsl_lang::ast::AssignmentOpData::RShift },
        ast::AssignmentOpData::And => quote! { glsl_lang::ast::AssignmentOpData::And },
        ast::AssignmentOpData::Xor => quote! { glsl_lang::ast::AssignmentOpData::Xor },
        ast::AssignmentOpData::Or => quote! { AssignmentOpData::Or },
    };

    quote! { glsl_lang::ast::AssignmentOp::new(#op, #span) }
}

fn tokenize_function_identifier(i: &ast::FunIdentifier) -> TokenStream {
    if let Some(rs_ident) = i.as_rs_ident() {
        let ident = format_ident!("{}", rs_ident);
        return quote! { #ident };
    }

    let span = tokenize_span(&i.span);
    let i = match i.content {
        ast::FunIdentifierData::TypeSpecifier(ref n) => {
            let n = Box::new(tokenize_type_specifier(n)).quote();
            quote! { glsl_lang::ast::FunIdentifierData::TypeSpecifier(#n) }
        }

        ast::FunIdentifierData::Expr(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::FunIdentifierData::Expr(#e) }
        }
    };

    quote! { glsl_lang::ast::FunIdentifier::new(#i, #span) }
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
    let span = tokenize_span(&p.span);
    let p = {
        let ty = tokenize_type_specifier(&p.ty);
        let ident = tokenize_arrayed_identifier(&p.ident);

        quote! {
          glsl_lang::ast::FunctionParameterDeclaratorData {
            ty: #ty,
            ident: #ident
          }
        }
    };

    quote! { glsl_lang::ast::FunctionParameterDeclarator::new(#p, #span) }
}

fn tokenize_init_declarator_list(i: &ast::InitDeclaratorList) -> TokenStream {
    let span = tokenize_span(&i.span);
    let i = {
        let head = tokenize_single_declaration(&i.head);
        let tail = i.tail.iter().map(tokenize_single_declaration_no_type);

        quote! {
          glsl_lang::ast::InitDeclaratorListData {
            head: #head,
            tail: vec![#(#tail),*]
          }
        }
    };

    quote! { glsl_lang::ast::InitDeclaratorList::new(#i, #span) }
}

fn tokenize_single_declaration(d: &ast::SingleDeclaration) -> TokenStream {
    let span = tokenize_span(&d.span);
    let d = {
        let ty = tokenize_fully_specified_type(&d.ty);
        let name = d.name.as_ref().map(|i| tokenize_identifier(i)).quote();
        let array_specifier = d.array_specifier.as_ref().map(tokenize_array_spec).quote();
        let initializer = d.initializer.as_ref().map(tokenize_initializer).quote();

        quote! {
          glsl_lang::ast::SingleDeclarationData {
            ty: #ty,
            name: #name,
            array_specifier: #array_specifier,
            initializer: #initializer
          }
        }
    };

    quote! { glsl_lang::ast::SingleDeclaration::new(#d, #span) }
}

fn tokenize_single_declaration_no_type(d: &ast::SingleDeclarationNoType) -> TokenStream {
    let span = tokenize_span(&d.span);
    let d = {
        let ident = tokenize_arrayed_identifier(&d.ident);
        let initializer = d.initializer.as_ref().map(tokenize_initializer).quote();

        quote! {
          glsl_lang::ast::SingleDeclarationNoTypeData {
            ident: #ident,
            initializer: #initializer
          }
        }
    };

    quote! { glsl_lang::ast::SingleDeclarationNoType::new(#d, #span) }
}

fn tokenize_initializer(i: &ast::Initializer) -> TokenStream {
    let span = tokenize_span(&i.span);
    let i = match i.content {
        ast::InitializerData::Simple(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::InitializerData::Simple(#e) }
        }

        ast::InitializerData::List(ref list) => {
            let l = list.iter().map(tokenize_initializer);
            quote! { glsl_lang::ast::InitializerData::List(vec![#(#l),*]) }
        }
    };

    quote! { glsl_lang::ast::Initializer::new(#i, #span) }
}

fn tokenize_block(b: &ast::Block) -> TokenStream {
    let span = tokenize_span(&b.span);
    let b = {
        let qual = tokenize_type_qualifier(&b.qualifier);
        let name = tokenize_identifier(&b.name);
        let fields = b.fields.iter().map(tokenize_struct_field);
        let identifier = b
            .identifier
            .as_ref()
            .map(tokenize_arrayed_identifier)
            .quote();

        quote! {
          glsl_lang::ast::BlockData {
            qualifier: #qual,
            name: #name,
            fields: vec![#(#fields),*],
            identifier: #identifier
          }
        }
    };

    quote! { glsl_lang::ast::Block::new(#b, #span) }
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
    let span = tokenize_span(&est.span);
    let est = {
        let e = est.0.as_ref().map(|e| tokenize_expr(&e)).quote();
        quote! { glsl_lang::ast::ExprStatementData(#e) }
    };

    quote! { glsl_lang::ast::ExprStatement::new(#est, #span) }
}

fn tokenize_selection_statement(sst: &ast::SelectionStatement) -> TokenStream {
    let span = tokenize_span(&sst.span);
    let sst = {
        let cond = Box::new(tokenize_expr(&sst.cond)).quote();
        let rest = tokenize_selection_rest_statement(&sst.rest);

        quote! {
          glsl_lang::ast::SelectionStatementData {
            cond: #cond,
            rest: #rest
          }
        }
    };

    quote! { glsl_lang::ast::SelectionStatement::new(#sst, #span) }
}

fn tokenize_selection_rest_statement(sst: &ast::SelectionRestStatement) -> TokenStream {
    let span = tokenize_span(&sst.span);
    let sst = match sst.content {
        ast::SelectionRestStatementData::Statement(ref if_st) => {
            let e = Box::new(tokenize_statement(if_st)).quote();
            quote! { glsl_lang::ast::SelectionRestStatementData::Statement(#e) }
        }

        ast::SelectionRestStatementData::Else(ref if_st, ref else_st) => {
            let if_st = Box::new(tokenize_statement(if_st)).quote();
            let else_st = Box::new(tokenize_statement(else_st)).quote();
            quote! { glsl_lang::ast::SelectionRestStatementData::Else(#if_st, #else_st) }
        }
    };

    quote! { glsl_lang::ast::SelectionRestStatement::new(#sst, #span) }
}

fn tokenize_switch_statement(sst: &ast::SwitchStatement) -> TokenStream {
    let span = tokenize_span(&sst.span);
    let sst = {
        let head = Box::new(tokenize_expr(&sst.head)).quote();
        let body = sst.body.iter().map(tokenize_statement);

        quote! {
          glsl_lang::ast::SwitchStatementData {
            head: #head,
            body: vec![#(#body),*]
          }
        }
    };

    quote! { glsl_lang::ast::SwitchStatement::new(#sst, #span) }
}

fn tokenize_case_label(cl: &ast::CaseLabel) -> TokenStream {
    let span = tokenize_span(&cl.span);
    let cl = match cl.content {
        ast::CaseLabelData::Case(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::CaseLabelData::Case(#e) }
        }

        ast::CaseLabelData::Def => quote! { glsl_lang::ast::CaseLabelData::Def },
    };

    quote! { glsl_lang::ast::CaseLabel::new(#cl, #span) }
}

fn tokenize_iteration_statement(ist: &ast::IterationStatement) -> TokenStream {
    let span = tokenize_span(&ist.span);
    let ist = match ist.content {
        ast::IterationStatementData::While(ref cond, ref body) => {
            let cond = tokenize_condition(cond);
            let body = Box::new(tokenize_statement(body)).quote();
            quote! { glsl_lang::ast::IterationStatementData::While(#cond, #body) }
        }

        ast::IterationStatementData::DoWhile(ref body, ref cond) => {
            let body = Box::new(tokenize_statement(body)).quote();
            let cond = Box::new(tokenize_expr(cond)).quote();
            quote! { glsl_lang::ast::IterationStatementData::DoWhile(#body, #cond) }
        }

        ast::IterationStatementData::For(ref init, ref rest, ref body) => {
            let init = tokenize_for_init_statement(init);
            let rest = tokenize_for_rest_statement(rest);
            let body = Box::new(tokenize_statement(body)).quote();
            quote! { glsl_lang::ast::IterationStatementData::For(#init, #rest, #body) }
        }
    };

    quote! { glsl_lang::ast::IterationStatement::new(#ist, #span) }
}

fn tokenize_condition(c: &ast::Condition) -> TokenStream {
    let span = tokenize_span(&c.span);
    let c = match c.content {
        ast::ConditionData::Expr(ref e) => {
            let e = Box::new(tokenize_expr(e)).quote();
            quote! { glsl_lang::ast::ConditionData::Expr(#e) }
        }

        ast::ConditionData::Assignment(ref ty, ref name, ref initializer) => {
            let ty = Box::new(tokenize_fully_specified_type(ty)).quote();
            let name = tokenize_identifier(name);
            let initializer = tokenize_initializer(initializer);

            quote! { glsl_lang::ast::ConditionData::Assignment(#ty, #name, #initializer) }
        }
    };

    quote! { glsl_lang::ast::Condition::new(#c, #span) }
}

fn tokenize_for_init_statement(i: &ast::ForInitStatement) -> TokenStream {
    let span = tokenize_span(&i.span);
    let i = match i.content {
        ast::ForInitStatementData::Expression(ref expr) => {
            let e = expr.as_ref().map(|e| tokenize_expr(&e)).quote();
            quote! { glsl_lang::ast::ForInitStatementData::Expression(#e) }
        }

        ast::ForInitStatementData::Declaration(ref d) => {
            let d = Box::new(tokenize_declaration(d)).quote();
            quote! { glsl_lang::ast::ForInitStatementData::Declaration(#d) }
        }
    };

    quote! { glsl_lang::ast::ForInitStatement::new(#i, #span) }
}

fn tokenize_for_rest_statement(r: &ast::ForRestStatement) -> TokenStream {
    let span = tokenize_span(&r.span);
    let r = {
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
    };

    quote! { glsl_lang::ast::ForRestStatement::new(#r, #span) }
}

fn tokenize_jump_statement(j: &ast::JumpStatement) -> TokenStream {
    let span = tokenize_span(&j.span);
    let j = match j.content {
        ast::JumpStatementData::Continue => quote! { glsl_lang::ast::JumpStatementData::Continue },
        ast::JumpStatementData::Break => quote! { glsl_lang::ast::JumpStatementData::Break },
        ast::JumpStatementData::Discard => quote! { glsl_lang::ast::JumpStatementData::Discard },
        ast::JumpStatementData::Return(ref e) => {
            let e = e
                .as_ref()
                .map(|e| Box::new(tokenize_expr(e)).quote())
                .quote();
            quote! { glsl_lang::ast::JumpStatementData::Return(#e) }
        }
    };

    quote! { glsl_lang::ast::JumpStatement::new(#j, #span) }
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
    let span = tokenize_span(&pd.span);
    let pd = match pd.content {
        ast::PreprocessorDefineData::ObjectLike {
            ref ident,
            ref value,
        } => {
            let ident = tokenize_identifier(ident);
            let value = value.quote();

            quote! {
              glsl_lang::ast::PreprocessorDefineData::ObjectLike {
                ident: #ident,
                value: #value
              }
            }
        }

        ast::PreprocessorDefineData::FunctionLike {
            ref ident,
            ref args,
            ref value,
        } => {
            let ident = tokenize_identifier(ident);
            let args = args.iter().map(tokenize_identifier);
            let value = value.quote();

            quote! {
              glsl_lang::ast::PreprocessorDefineData::FunctionLike {
                ident: #ident,
                args: vec![#(#args),*],
                value: #value
              }
            }
        }
    };

    quote! { glsl_lang::ast::PreprocessorDefine::new(#pd, #span) }
}

fn tokenize_preprocessor_elseif(pei: &ast::PreprocessorElseIf) -> TokenStream {
    let span = tokenize_span(&pei.span);
    let pei = {
        let condition = pei.condition.quote();

        quote! {
          glsl_lang::ast::PreprocessorElseIfData {
            condition: #condition
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorElseIf::new(#pei, #span) }
}

fn tokenize_preprocessor_error(pe: &ast::PreprocessorError) -> TokenStream {
    let span = tokenize_span(&pe.span);
    let pe = {
        let message = &pe.message;

        quote! {
          glsl_lang::ast::PreprocessorErrorData {
            message: #message.to_owned()
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorError::new(#pe, #span) }
}

fn tokenize_preprocessor_if(pi: &ast::PreprocessorIf) -> TokenStream {
    let span = tokenize_span(&pi.span);
    let pi = {
        let condition = pi.condition.quote();

        quote! {
          glsl_lang::ast::PreprocessorIfData {
            condition: #condition
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorIf::new(#pi, #span) }
}

fn tokenize_preprocessor_ifdef(pid: &ast::PreprocessorIfDef) -> TokenStream {
    let span = tokenize_span(&pid.span);
    let pid = {
        let ident = tokenize_identifier(&pid.ident);

        quote! {
          glsl_lang::ast::PreprocessorIfDefData {
            ident: #ident
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorIfDef::new(#pid, #span) }
}

fn tokenize_preprocessor_ifndef(pind: &ast::PreprocessorIfNDef) -> TokenStream {
    let span = tokenize_span(&pind.span);
    let pind = {
        let ident = tokenize_identifier(&pind.ident);

        quote! {
          glsl_lang::ast::PreprocessorIfNDefData {
            ident: #ident
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorIfNDef::new(#pind, #span) }
}

fn tokenize_preprocessor_include(pi: &ast::PreprocessorInclude) -> TokenStream {
    let span = tokenize_span(&pi.span);
    let pi = {
        let path = tokenize_path(&pi.path);

        quote! {
          glsl_lang::ast::PreprocessorIncludeData {
            path: #path
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorInclude::new(#pi, #span) }
}

fn tokenize_preprocessor_line(pl: &ast::PreprocessorLine) -> TokenStream {
    let span = tokenize_span(&pl.span);
    let pl = {
        let line = pl.line;
        let source_string_number = pl.source_string_number.quote();

        quote! {
          glsl_lang::ast::PreprocessorLineData {
            line: #line,
            source_string_number: #source_string_number
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorLine::new(#pl, #span) }
}

fn tokenize_preprocessor_pragma(pp: &ast::PreprocessorPragma) -> TokenStream {
    let span = tokenize_span(&pp.span);
    let pp = {
        let command = &pp.command;

        quote! {
          glsl_lang::ast::PreprocessorPragmaData {
            command: #command.to_owned()
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorPragma::new(#pp, #span) }
}

fn tokenize_preprocessor_undef(pu: &ast::PreprocessorUndef) -> TokenStream {
    let span = tokenize_span(&pu.span);
    let pu = {
        let name = tokenize_identifier(&pu.name);

        quote! {
          glsl_lang::ast::PreprocessorUndefData {
            name: #name
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorUndef::new(#pu, #span) }
}

fn tokenize_preprocessor_version(pv: &ast::PreprocessorVersion) -> TokenStream {
    let span = tokenize_span(&pv.span);
    let pv = {
        let version = pv.version;
        let profile = pv
            .profile
            .as_ref()
            .map(tokenize_preprocessor_version_profile)
            .quote();

        quote! {
          glsl_lang::ast::PreprocessorVersionData {
            version: #version,
            profile: #profile
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorVersion::new(#pv, #span) }
}

fn tokenize_preprocessor_version_profile(profile: &ast::PreprocessorVersionProfile) -> TokenStream {
    let span = tokenize_span(&profile.span);
    let profile = match profile.content {
        ast::PreprocessorVersionProfileData::Core => {
            quote! { glsl_lang::ast::PreprocessorVersionProfileData::Core }
        }
        ast::PreprocessorVersionProfileData::Compatibility => {
            quote! { glsl_lang::ast::PreprocessorVersionProfileData::Compatibility }
        }
        ast::PreprocessorVersionProfileData::Es => {
            quote! { glsl_lang::ast::PreprocessorVersionProfileData::Es }
        }
    };

    quote! { glsl_lang::ast::PreprocessorVersionProfile::new(#profile, #span) }
}

fn tokenize_preprocessor_extension(pe: &ast::PreprocessorExtension) -> TokenStream {
    let span = tokenize_span(&pe.span);
    let pe = {
        let name = tokenize_preprocessor_extension_name(&pe.name);
        let behavior = pe
            .behavior
            .as_ref()
            .map(tokenize_preprocessor_extension_behavior)
            .quote();

        quote! {
          glsl_lang::ast::PreprocessorExtensionData {
            name: #name,
            behavior: #behavior
          }
        }
    };

    quote! { glsl_lang::ast::PreprocessorExtension::new(#pe, #span) }
}

fn tokenize_preprocessor_extension_name(name: &ast::PreprocessorExtensionName) -> TokenStream {
    let span = tokenize_span(&name.span);
    let name = match name.content {
        ast::PreprocessorExtensionNameData::All => {
            quote! { glsl_lang::ast::PreprocessorExtensionNameData::All }
        }
        ast::PreprocessorExtensionNameData::Specific(ref n) => {
            let n = n.quote();
            quote! { glsl_lang::ast::PreprocessorExtensionNameData::Specific(#n) }
        }
    };

    quote! { glsl_lang::ast::PreprocessorExtensionName::new(#name, #span) }
}

fn tokenize_preprocessor_extension_behavior(
    behavior: &ast::PreprocessorExtensionBehavior,
) -> TokenStream {
    let span = tokenize_span(&behavior.span);
    let behavior = match behavior.content {
        ast::PreprocessorExtensionBehaviorData::Require => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehaviorData::Require }
        }
        ast::PreprocessorExtensionBehaviorData::Enable => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehaviorData::Enable }
        }
        ast::PreprocessorExtensionBehaviorData::Warn => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehaviorData::Warn }
        }
        ast::PreprocessorExtensionBehaviorData::Disable => {
            quote! { glsl_lang::ast::PreprocessorExtensionBehaviorData::Disable }
        }
    };

    quote! { glsl_lang::ast::PreprocessorExtensionBehavior::new(#behavior, #span) }
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
