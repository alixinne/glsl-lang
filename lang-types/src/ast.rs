//! GLSL abstract syntax tree and grammar.
//!
//! This module exports all the grammar syntax that defines GLSL. You’ll be handling ASTs
//! representing your GLSL source.
//!
//! The most external form of a GLSL parsed AST is [`TranslationUnit`] (a shader). Some parts of the
//! tree are *boxed*. This is due to two facts:
//!
//! - Recursion is used, hence we need a way to give our types a static size.
//! - Because of some very deep variants, runtime size would explode if no indirection weren’t
//!   in place.
//!
//! The types are commented so feel free to inspect each of theme. As a starter, you should read
//! the documentation of [`Expr`], [`FunctionDefinition`], [`Statement`] and [`TranslationUnit`].
//!
//! [`Statement`]: crate::ast::Statement
//! [`TranslationUnit`]: crate::ast::TranslationUnit
//! [`Expr`]: crate::ast::Expr
//! [`FunctionDefinition`]: crate::ast::FunctionDefinition

use std::{fmt, iter::FromIterator};

#[cfg(feature = "serde")]
use rserde::{Deserialize, Serialize};

pub use lang_util::{
    node::{Node, NodeDisplay},
    position::NodeSpan,
    FileId, NodeContent, NodeContentDisplay, SmolStr, TextRange, TextSize,
};

macro_rules! impl_node_content {
    (
        $(#[$m:meta])* $v:vis type $t:ident = Node<$tdata:ident>;
    ) => {
        impl NodeContent for $tdata {}

        $(#[$m])* $v type $t = Node<$tdata>;

        impl From<Node<$tdata>> for $tdata {
            fn from(node: Node<$tdata>) -> Self {
                node.content
            }
        }
    };
}

/// A generic identifier.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Hash, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
#[lang_util(display(leaf))]
pub struct IdentifierData(#[lang_util(display(extra))] pub SmolStr);

impl_node_content! {
    /// Type alias for `Node<IdentifierData>`.
    pub type Identifier = Node<IdentifierData>;
}

impl IdentifierData {
    /// Parses this identifier as a glsl-lang-quote Rust identifier
    ///
    /// # Returns
    ///
    /// `None` if this identifier is not a Rust identifier, otherwise returns the parsed
    /// identifier.
    pub fn as_rs_ident(&self) -> Option<&str> {
        if self.0.starts_with('#') & self.0.ends_with(')') {
            // Remove #\s* and )
            let s = self.0[1..self.0.len() - 1].trim();
            // Remove ( and whitespace
            Some(s[1..].trim())
        } else {
            None
        }
    }

    /// Returns this identifier as a string slice
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<&str> for IdentifierData {
    fn from(ident: &str) -> Self {
        Self(ident.into())
    }
}

impl fmt::Display for IdentifierData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

/// Any type name.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Hash, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
#[lang_util(display(leaf))]
pub struct TypeNameData(pub SmolStr);

impl_node_content! {
    /// Type alias for `Node<TypeNameData>`.
    pub type TypeName = Node<TypeNameData>;
}

impl TypeNameData {
    /// Return this type name as a string slice
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<IdentifierData> for TypeNameData {
    fn from(ident: IdentifierData) -> Self {
        Self(ident.0)
    }
}

impl From<&str> for TypeNameData {
    fn from(ident: &str) -> Self {
        Self(ident.into())
    }
}

impl fmt::Display for TypeNameData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

/// A path literal.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PathData {
    /// Specified with angle brackets.
    Absolute(String),
    /// Specified with double quotes.
    Relative(String),
}

impl_node_content! {
    /// Type alias for `Node<PathData>`.
    pub type Path = Node<PathData>;
}

/// Type specifier (non-array).
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum TypeSpecifierNonArrayData {
    /// `void` type specifier
    #[lang_util(display(extra = "void"))]
    Void,
    /// `bool` type specifier
    #[lang_util(display(extra = "bool"))]
    Bool,
    /// `int` type specifier
    #[lang_util(display(extra = "int"))]
    Int,
    /// `uint` type specifier
    #[lang_util(display(extra = "uint"))]
    UInt,
    /// `float` type specifier
    #[lang_util(display(extra = "float"))]
    Float,
    /// `double` type specifier
    #[lang_util(display(extra = "double"))]
    Double,
    /// `vec2` type specifier
    #[lang_util(display(extra = "vec2"))]
    Vec2,
    /// `vec3` type specifier
    #[lang_util(display(extra = "vec3"))]
    Vec3,
    /// `vec4` type specifier
    #[lang_util(display(extra = "vec4"))]
    Vec4,
    /// `dvec2` type specifier
    #[lang_util(display(extra = "dvec2"))]
    DVec2,
    /// `dvec3` type specifier
    #[lang_util(display(extra = "dvec3"))]
    DVec3,
    /// `dvec4` type specifier
    #[lang_util(display(extra = "dvec4"))]
    DVec4,
    /// `bvec2` type specifier
    #[lang_util(display(extra = "bvec2"))]
    BVec2,
    /// `bvec3` type specifier
    #[lang_util(display(extra = "bvec3"))]
    BVec3,
    /// `bvec4` type specifier
    #[lang_util(display(extra = "bvec4"))]
    BVec4,
    /// `ivec2` type specifier
    #[lang_util(display(extra = "ivec2"))]
    IVec2,
    /// `ivec3` type specifier
    #[lang_util(display(extra = "ivec3"))]
    IVec3,
    /// `ivec4` type specifier
    #[lang_util(display(extra = "ivec4"))]
    IVec4,
    /// `uvec2` type specifier
    #[lang_util(display(extra = "uvec2"))]
    UVec2,
    /// `uvec3` type specifier
    #[lang_util(display(extra = "uvec3"))]
    UVec3,
    /// `uvec4` type specifier
    #[lang_util(display(extra = "uvec4"))]
    UVec4,
    /// `mat2` type specifier
    #[lang_util(display(extra = "mat2"))]
    Mat2,
    /// `mat3` type specifier
    #[lang_util(display(extra = "mat3"))]
    Mat3,
    /// `mat4` type specifier
    #[lang_util(display(extra = "mat4"))]
    Mat4,
    /// `mat2x2` type specifier
    #[lang_util(display(extra = "mat2x2"))]
    Mat22,
    /// `mat2x3` type specifier
    #[lang_util(display(extra = "mat2x3"))]
    Mat23,
    /// `mat2x4` type specifier
    #[lang_util(display(extra = "mat2x4"))]
    Mat24,
    /// `mat3x2` type specifier
    #[lang_util(display(extra = "mat3x2"))]
    Mat32,
    /// `mat3x3` type specifier
    #[lang_util(display(extra = "mat3x3"))]
    Mat33,
    /// `mat3x4` type specifier
    #[lang_util(display(extra = "mat3x4"))]
    Mat34,
    /// `mat4x2` type specifier
    #[lang_util(display(extra = "mat4x2"))]
    Mat42,
    /// `mat4x3` type specifier
    #[lang_util(display(extra = "mat4x3"))]
    Mat43,
    /// `mat4x4` type specifier
    #[lang_util(display(extra = "mat4x4"))]
    Mat44,
    /// `dmat2` type specifier
    #[lang_util(display(extra = "dmat2"))]
    DMat2,
    /// `dmat3` type specifier
    #[lang_util(display(extra = "dmat3"))]
    DMat3,
    /// `dmat4` type specifier
    #[lang_util(display(extra = "dmat4"))]
    DMat4,
    /// `dmat2x2` type specifier
    #[lang_util(display(extra = "dmat2x2"))]
    DMat22,
    /// `dmat2x3` type specifier
    #[lang_util(display(extra = "dmat2x3"))]
    DMat23,
    /// `dmat2x4` type specifier
    #[lang_util(display(extra = "dmat2x4"))]
    DMat24,
    /// `dmat3x2` type specifier
    #[lang_util(display(extra = "dmat3x2"))]
    DMat32,
    /// `dmat3x3` type specifier
    #[lang_util(display(extra = "dmat3x3"))]
    DMat33,
    /// `dmat3x4` type specifier
    #[lang_util(display(extra = "dmat3x4"))]
    DMat34,
    /// `dmat4x2` type specifier
    #[lang_util(display(extra = "dmat4x2"))]
    DMat42,
    /// `dmat4x3` type specifier
    #[lang_util(display(extra = "dmat4x3"))]
    DMat43,
    /// `dmat4x4` type specifier
    #[lang_util(display(extra = "dmat4x4"))]
    DMat44,
    // floating point opaque types
    /// `sampler1D` type specifier
    #[lang_util(display(extra = "sampler1D"))]
    Sampler1D,
    /// `image1D` type specifier
    #[lang_util(display(extra = "image1D"))]
    Image1D,
    /// `sampler2D` type specifier
    #[lang_util(display(extra = "sampler2D"))]
    Sampler2D,
    /// `image2D` type specifier
    #[lang_util(display(extra = "image2D"))]
    Image2D,
    /// `sampler3D` type specifier
    #[lang_util(display(extra = "sampler3D"))]
    Sampler3D,
    /// `image3D` type specifier
    #[lang_util(display(extra = "image3D"))]
    Image3D,
    /// `samplerCube` type specifier
    #[lang_util(display(extra = "samplerCube"))]
    SamplerCube,
    /// `imageCube` type specifier
    #[lang_util(display(extra = "imageCube"))]
    ImageCube,
    /// `sampler2DRect` type specifier
    #[lang_util(display(extra = "sampler2DRect"))]
    Sampler2DRect,
    /// `image2DRect` type specifier
    #[lang_util(display(extra = "image2DRect"))]
    Image2DRect,
    /// `sampler1DArray` type specifier
    #[lang_util(display(extra = "sampler1DArray"))]
    Sampler1DArray,
    /// `image1DArray` type specifier
    #[lang_util(display(extra = "image1DArray"))]
    Image1DArray,
    /// `sampler2DArray` type specifier
    #[lang_util(display(extra = "sampler2DArray"))]
    Sampler2DArray,
    /// `image2DArray` type specifier
    #[lang_util(display(extra = "image2DArray"))]
    Image2DArray,
    /// `samplerBuffer` type specifier
    #[lang_util(display(extra = "samplerBuffer"))]
    SamplerBuffer,
    /// `imageBuffer` type specifier
    #[lang_util(display(extra = "imageBuffer"))]
    ImageBuffer,
    /// `sampler2DMS` type specifier
    #[lang_util(display(extra = "sampler2DMS"))]
    Sampler2DMs,
    /// `image2DMS` type specifier
    #[lang_util(display(extra = "image2DMS"))]
    Image2DMs,
    /// `sampler2DMSArray` type specifier
    #[lang_util(display(extra = "sampler2DMSArray"))]
    Sampler2DMsArray,
    /// `image2DMSArray` type specifier
    #[lang_util(display(extra = "image2DMSArray"))]
    Image2DMsArray,
    /// `samplerCubeArray` type specifier
    #[lang_util(display(extra = "samplerCubeArray"))]
    SamplerCubeArray,
    /// `imageCubeArray` type specifier
    #[lang_util(display(extra = "imageCubeArray"))]
    ImageCubeArray,
    /// `sampler1DShadow` type specifier
    #[lang_util(display(extra = "sampler1DShadow"))]
    Sampler1DShadow,
    /// `sampler2DShadow` type specifier
    #[lang_util(display(extra = "sampler2DShadow"))]
    Sampler2DShadow,
    /// `sampler2DRectShadow` type specifier
    #[lang_util(display(extra = "sampler2DRectShadow"))]
    Sampler2DRectShadow,
    /// `sampler1DArrayShadow` type specifier
    #[lang_util(display(extra = "sampler1DArrayShadow"))]
    Sampler1DArrayShadow,
    /// `sampler2DArrayShadow` type specifier
    #[lang_util(display(extra = "sampler2DArrayShadow"))]
    Sampler2DArrayShadow,
    /// `samplerCubeShadow` type specifier
    #[lang_util(display(extra = "samplerCubeShadow"))]
    SamplerCubeShadow,
    /// `samplerCubeArrayShadow` type specifier
    #[lang_util(display(extra = "samplerCubeArrayShadow"))]
    SamplerCubeArrayShadow,
    // signed integer opaque types
    /// `isampler1D` type specifier
    #[lang_util(display(extra = "isampler1D"))]
    ISampler1D,
    /// `iimage1D` type specifier
    #[lang_util(display(extra = "iimage1D"))]
    IImage1D,
    /// `isampler2D` type specifier
    #[lang_util(display(extra = "isampler2D"))]
    ISampler2D,
    /// `iimage2D` type specifier
    #[lang_util(display(extra = "iimage2D"))]
    IImage2D,
    /// `isampler3D` type specifier
    #[lang_util(display(extra = "isampler3D"))]
    ISampler3D,
    /// `iimage3D` type specifier
    #[lang_util(display(extra = "iimage3D"))]
    IImage3D,
    /// `isamplerCube` type specifier
    #[lang_util(display(extra = "isamplerCube"))]
    ISamplerCube,
    /// `iimageCube` type specifier
    #[lang_util(display(extra = "iimageCube"))]
    IImageCube,
    /// `isampler2DRect` type specifier
    #[lang_util(display(extra = "isampler2DRect"))]
    ISampler2DRect,
    /// `iimage2DRect` type specifier
    #[lang_util(display(extra = "iimage2DRect"))]
    IImage2DRect,
    /// `isampler1DArray` type specifier
    #[lang_util(display(extra = "isampler1DArray"))]
    ISampler1DArray,
    /// `iimage1DArray` type specifier
    #[lang_util(display(extra = "iimage1DArray"))]
    IImage1DArray,
    /// `isampler2DArray` type specifier
    #[lang_util(display(extra = "isampler2DArray"))]
    ISampler2DArray,
    /// `iimage2DArray` type specifier
    #[lang_util(display(extra = "iimage2DArray"))]
    IImage2DArray,
    /// `isamplerBuffer` type specifier
    #[lang_util(display(extra = "isamplerBuffer"))]
    ISamplerBuffer,
    /// `iimageBuffer` type specifier
    #[lang_util(display(extra = "iimageBuffer"))]
    IImageBuffer,
    /// `isampler2DMS` type specifier
    #[lang_util(display(extra = "isampler2DMS"))]
    ISampler2DMs,
    /// `iimage2DMS` type specifier
    #[lang_util(display(extra = "iimage2DMS"))]
    IImage2DMs,
    /// `isampler2DMSArray` type specifier
    #[lang_util(display(extra = "isampler2DMSArray"))]
    ISampler2DMsArray,
    /// `iimage2DMSArray` type specifier
    #[lang_util(display(extra = "iimage2DMSArray"))]
    IImage2DMsArray,
    /// `isamplerCubeArray` type specifier
    #[lang_util(display(extra = "isamplerCubeArray"))]
    ISamplerCubeArray,
    /// `iimageCubeArray` type specifier
    #[lang_util(display(extra = "iimageCubeArray"))]
    IImageCubeArray,
    // unsigned integer opaque types
    /// `atomic_uint` type specifier
    #[lang_util(display(extra = "atomic_uint"))]
    AtomicUInt,
    /// `usampler1D` type specifier
    #[lang_util(display(extra = "usampler1D"))]
    USampler1D,
    /// `uimage1D` type specifier
    #[lang_util(display(extra = "uimage1D"))]
    UImage1D,
    /// `usampler2D` type specifier
    #[lang_util(display(extra = "usampler2D"))]
    USampler2D,
    /// `uimage2D` type specifier
    #[lang_util(display(extra = "uimage2D"))]
    UImage2D,
    /// `usampler3D` type specifier
    #[lang_util(display(extra = "usampler3D"))]
    USampler3D,
    /// `uimage3D` type specifier
    #[lang_util(display(extra = "uimage3D"))]
    UImage3D,
    /// `usamplerCube` type specifier
    #[lang_util(display(extra = "usamplerCube"))]
    USamplerCube,
    /// `uimageCube` type specifier
    #[lang_util(display(extra = "uimageCube"))]
    UImageCube,
    /// `usampler2DRect` type specifier
    #[lang_util(display(extra = "usampler2DRect"))]
    USampler2DRect,
    /// `uimage2DRect` type specifier
    #[lang_util(display(extra = "uimage2DRect"))]
    UImage2DRect,
    /// `usampler1DArray` type specifier
    #[lang_util(display(extra = "usampler1DArray"))]
    USampler1DArray,
    /// `uimage1DArray` type specifier
    #[lang_util(display(extra = "uimage1DArray"))]
    UImage1DArray,
    /// `usampler2DArray` type specifier
    #[lang_util(display(extra = "usampler2DArray"))]
    USampler2DArray,
    /// `uimage2DArray` type specifier
    #[lang_util(display(extra = "uimage2DArray"))]
    UImage2DArray,
    /// `usamplerBuffer` type specifier
    #[lang_util(display(extra = "usamplerBuffer"))]
    USamplerBuffer,
    /// `uimageBuffer` type specifier
    #[lang_util(display(extra = "uimageBuffer"))]
    UImageBuffer,
    /// `usampler2DMS` type specifier
    #[lang_util(display(extra = "usampler2DMS"))]
    USampler2DMs,
    /// `uimage2DMS` type specifier
    #[lang_util(display(extra = "uimage2DMS"))]
    UImage2DMs,
    /// `usampler2DMSArray` type specifier
    #[lang_util(display(extra = "usampler2DMSArray"))]
    USampler2DMsArray,
    /// `uimage2DMSArray` type specifier
    #[lang_util(display(extra = "uimage2DMSArray"))]
    UImage2DMsArray,
    /// `usamplerCubeArray` type specifier
    #[lang_util(display(extra = "usamplerCubeArray"))]
    USamplerCubeArray,
    /// `uimageCubeArray` type specifier
    #[lang_util(display(extra = "uimageCubeArray"))]
    UImageCubeArray,

    // GL_KHR_vulkan_glsl types
    /// `texture1D` type specifier
    #[lang_util(display(extra = "texture1D"))]
    Texture1D,
    /// `texture2D` type specifier
    #[lang_util(display(extra = "texture2D"))]
    Texture2D,
    /// `texture3D` type specifier
    #[lang_util(display(extra = "texture3D"))]
    Texture3D,
    /// `textureCube` type specifier
    #[lang_util(display(extra = "textureCube"))]
    TextureCube,
    /// `texture2DRect` type specifier
    #[lang_util(display(extra = "texture2DRect"))]
    Texture2DRect,
    /// `texture1DArray` type specifier
    #[lang_util(display(extra = "texture1DArray"))]
    Texture1DArray,
    /// `texture2DArray` type specifier
    #[lang_util(display(extra = "texture2DArray"))]
    Texture2DArray,
    /// `textureBuffer` type specifier
    #[lang_util(display(extra = "textureBuffer"))]
    TextureBuffer,
    /// `texture2DMs` type specifier
    #[lang_util(display(extra = "texture2DMS"))]
    Texture2DMs,
    /// `texture2DMsArray` type specifier
    #[lang_util(display(extra = "texture2DMSArray"))]
    Texture2DMsArray,
    /// `textureCubeArray` type specifier
    #[lang_util(display(extra = "textureCubeArray"))]
    TextureCubeArray,
    /// `itexture1D` type specifier
    #[lang_util(display(extra = "itexture1D"))]
    ITexture1D,
    /// `itexture2D` type specifier
    #[lang_util(display(extra = "itexture2D"))]
    ITexture2D,
    /// `itexture3D` type specifier
    #[lang_util(display(extra = "itexture3D"))]
    ITexture3D,
    /// `itextureCube` type specifier
    #[lang_util(display(extra = "itextureCube"))]
    ITextureCube,
    /// `itexture2DRect` type specifier
    #[lang_util(display(extra = "itexture2DRect"))]
    ITexture2DRect,
    /// `itexture1DArray` type specifier
    #[lang_util(display(extra = "itexture1DArray"))]
    ITexture1DArray,
    /// `itexture2DArray` type specifier
    #[lang_util(display(extra = "itexture2DArray"))]
    ITexture2DArray,
    /// `itextureBuffer` type specifier
    #[lang_util(display(extra = "itextureBuffer"))]
    ITextureBuffer,
    /// `itexture2DMs` type specifier
    #[lang_util(display(extra = "itexture2DMS"))]
    ITexture2DMs,
    /// `itexture2DMsArray` type specifier
    #[lang_util(display(extra = "itexture2DMSArray"))]
    ITexture2DMsArray,
    /// `itextureCubeArray` type specifier
    #[lang_util(display(extra = "itextureCubeArray"))]
    ITextureCubeArray,
    /// `sampler` type specifier
    #[lang_util(display(extra = "sampler"))]
    Sampler,
    /// `samplerShadow` type specifier
    #[lang_util(display(extra = "samplerShadow"))]
    SamplerShadow,
    /// `subpassInput` type specifier
    #[lang_util(display(extra = "subpassInput"))]
    SubpassInput,
    /// `isubpassInput` type specifier
    #[lang_util(display(extra = "isubpassInput"))]
    ISubpassInput,
    /// `usubpassInput` type specifier
    #[lang_util(display(extra = "usubpassInput"))]
    USubpassInput,
    /// `subpassInputMs` type specifier
    #[lang_util(display(extra = "subpassInputMS"))]
    SubpassInputMs,
    /// `isubpassInputMs` type specifier
    #[lang_util(display(extra = "isubpassInputMS"))]
    ISubpassInputMs,
    /// `usubpassInputMs` type specifier
    #[lang_util(display(extra = "usubpassInputMS"))]
    USubpassInputMs,

    // end GL_KHR_vulkan_glsl types
    /// `struct` type specifier
    #[lang_util(display(extra = "struct"))]
    Struct(StructSpecifier),
    /// Raw type name
    TypeName(TypeName),
}

impl_node_content! {
    /// Type alias for `Node<TypeSpecifierNonArrayData>`.
    pub type TypeSpecifierNonArray = Node<TypeSpecifierNonArrayData>;
}

impl From<TypeName> for TypeSpecifierNonArrayData {
    fn from(tn: TypeName) -> Self {
        Self::TypeName(tn)
    }
}

/// Type specifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct TypeSpecifierData {
    /// Type name portion of the specifier
    pub ty: TypeSpecifierNonArray,
    /// Array part of the specifier
    pub array_specifier: Option<ArraySpecifier>,
}

impl_node_content! {
    /// Type alias for `Node<TypeSpecifierData>`.
    pub type TypeSpecifier = Node<TypeSpecifierData>;
}

impl From<TypeSpecifierNonArray> for TypeSpecifierData {
    fn from(ty: TypeSpecifierNonArray) -> Self {
        Self {
            ty,
            array_specifier: None,
        }
    }
}

impl From<TypeSpecifierNonArrayData> for TypeSpecifierData {
    fn from(ty: TypeSpecifierNonArrayData) -> Self {
        Self {
            ty: ty.into(),
            array_specifier: None,
        }
    }
}

/// Struct specifier. Used to create new, user-defined types.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct StructSpecifierData {
    /// Structure name
    pub name: Option<TypeName>,
    /// Field specifications
    pub fields: Vec<StructFieldSpecifier>,
}

impl_node_content! {
    /// Type alias for `Node<StructSpecifierData>`.
    pub type StructSpecifier = Node<StructSpecifierData>;
}

/// Struct field specifier. Used to add fields to struct specifiers.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct StructFieldSpecifierData {
    /// Type qualifiers for the field
    pub qualifier: Option<TypeQualifier>,
    /// Type of the field
    pub ty: TypeSpecifier,
    /// List of declared identifiers for this field
    pub identifiers: Vec<ArrayedIdentifier>, // several identifiers of the same type
}

impl_node_content! {
    /// Type alias for `Node<StructFieldSpecifierData>`.
    pub type StructFieldSpecifier = Node<StructFieldSpecifierData>;
}

/// An identifier with an optional array specifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct ArrayedIdentifierData {
    /// Raw identifier
    pub ident: Identifier,
    /// Attached array specification
    pub array_spec: Option<ArraySpecifier>,
}

impl_node_content! {
    /// Type alias for `Node<ArrayedIdentifierData>`.
    pub type ArrayedIdentifier = Node<ArrayedIdentifierData>;
}

impl ArrayedIdentifierData {
    /// Create a new [ArrayedIdentifier] from a raw identifier and a specification
    pub fn new<I, AS>(ident: I, array_spec: AS) -> Self
    where
        I: Into<Identifier>,
        AS: Into<Option<ArraySpecifier>>,
    {
        Self {
            ident: ident.into(),
            array_spec: array_spec.into(),
        }
    }
}

impl From<&str> for ArrayedIdentifierData {
    fn from(ident: &str) -> Self {
        Self {
            ident: IdentifierData::from(ident).into(),
            array_spec: None,
        }
    }
}

/// Type qualifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct TypeQualifierData {
    /// List of type qualifiers
    pub qualifiers: Vec<TypeQualifierSpec>,
}

impl_node_content! {
    /// Type alias for `Node<TypeQualifierData>`.
    pub type TypeQualifier = Node<TypeQualifierData>;
}

/// Type qualifier spec.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum TypeQualifierSpecData {
    /// Storage qualifier
    Storage(StorageQualifier),
    /// Layout qualifier
    Layout(LayoutQualifier),
    /// Precision qualifier
    Precision(PrecisionQualifier),
    /// Interpolation qualifier
    Interpolation(InterpolationQualifier),
    /// `invariant` qualifier
    Invariant,
    /// `precise` qualifier
    Precise,
}

impl_node_content! {
    /// Type alias for `Node<TypeQualifierSpecData>`.
    pub type TypeQualifierSpec = Node<TypeQualifierSpecData>;
}

/// Storage qualifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum StorageQualifierData {
    /// `const` storage qualifier
    #[lang_util(display(extra = "const"))]
    Const,
    /// `inout` storage qualifier
    #[lang_util(display(extra = "inout"))]
    InOut,
    /// `in` storage qualifier
    #[lang_util(display(extra = "in"))]
    In,
    /// `out` storage qualifier
    #[lang_util(display(extra = "out"))]
    Out,
    /// `centroid` storage qualifier
    #[lang_util(display(extra = "centroid"))]
    Centroid,
    /// `patch` storage qualifier
    #[lang_util(display(extra = "patch"))]
    Patch,
    /// `sample` storage qualifier
    #[lang_util(display(extra = "sample"))]
    Sample,
    /// `uniform` storage qualifier
    #[lang_util(display(extra = "uniform"))]
    Uniform,
    /// `buffer` storage qualifier
    #[lang_util(display(extra = "buffer"))]
    Buffer,
    /// `shared` storage qualifier
    #[lang_util(display(extra = "shared"))]
    Shared,
    /// `coherent` storage qualifier
    #[lang_util(display(extra = "coherent"))]
    Coherent,
    /// `volatile` storage qualifier
    #[lang_util(display(extra = "volatile"))]
    Volatile,
    /// `restrict` storage qualifier
    #[lang_util(display(extra = "restrict"))]
    Restrict,
    /// `readonly` storage qualifier
    #[lang_util(display(extra = "readonly"))]
    ReadOnly,
    /// `writeonly` storage qualifier
    #[lang_util(display(extra = "writeonly"))]
    WriteOnly,
    /// `attribute` storage qualifier
    #[lang_util(display(extra = "attribute"))]
    Attribute,
    /// `varying` storage qualifier
    #[lang_util(display(extra = "varying"))]
    Varying,
    // Note: the grammar says TYPE_NAME but type_specifier makes more sense given the definition of
    // subroutine. The reference implementation is marked "to do".
    /// `subroutine` storage qualifier
    #[lang_util(display(extra = "subroutine"))]
    Subroutine(Vec<TypeSpecifier>),
}

impl_node_content! {
    /// Type alias for `Node<StorageQualifierData>`.
    pub type StorageQualifier = Node<StorageQualifierData>;
}

/// Layout qualifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct LayoutQualifierData {
    /// List of layout qualifiers
    pub ids: Vec<LayoutQualifierSpec>,
}

impl_node_content! {
    /// Type alias for `Node<LayoutQualifierData>`.
    pub type LayoutQualifier = Node<LayoutQualifierData>;
}

/// Layout qualifier spec.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum LayoutQualifierSpecData {
    /// An `ident = expr` layout qualifier
    Identifier(Identifier, Option<Box<Expr>>),
    /// `shared` layout qualifier
    #[lang_util(display(extra = "shared"))]
    Shared,
}

impl_node_content! {
    /// Type alias for `Node<LayoutQualifierSpecData>`.
    pub type LayoutQualifierSpec = Node<LayoutQualifierSpecData>;
}

/// Precision qualifier.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PrecisionQualifierData {
    /// `high` precision qualifier
    #[lang_util(display(extra = "high"))]
    High,
    /// `medium` precision qualifier
    #[lang_util(display(extra = "medium"))]
    Medium,
    /// `low` precision qualifier
    #[lang_util(display(extra = "low"))]
    Low,
}

impl_node_content! {
    /// Type alias for `Node<PrecisionQualifierData>`.
    pub type PrecisionQualifier = Node<PrecisionQualifierData>;
}

/// Interpolation qualifier.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum InterpolationQualifierData {
    /// `smooth` interpolation qualifier
    #[lang_util(display(extra = "smooth"))]
    Smooth,
    /// `flat` interpolation qualifier
    #[lang_util(display(extra = "flat"))]
    Flat,
    /// `noperspective` interpolation qualifier
    #[lang_util(display(extra = "noperspective"))]
    NoPerspective,
}

impl_node_content! {
    /// Type alias for `Node<InterpolationQualifierData>`.
    pub type InterpolationQualifier = Node<InterpolationQualifierData>;
}

/// Fully specified type.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct FullySpecifiedTypeData {
    /// Optional type qualifier
    pub qualifier: Option<TypeQualifier>,
    /// Type specifier
    pub ty: TypeSpecifier,
}

impl_node_content! {
    /// Type alias for `Node<FullySpecifiedTypeData>`.
    pub type FullySpecifiedType = Node<FullySpecifiedTypeData>;
}

impl FullySpecifiedTypeData {
    /// Create a new [FullySpecifiedType] from a [TypeSpecifierNonArray], with no qualifier and no
    /// array specifier
    pub fn new(ty: TypeSpecifierNonArray) -> Self {
        Self {
            qualifier: None,
            ty: TypeSpecifierData {
                ty,
                array_specifier: None,
            }
            .into(),
        }
    }
}

impl From<TypeSpecifierNonArrayData> for FullySpecifiedTypeData {
    fn from(ty: TypeSpecifierNonArrayData) -> Self {
        Self::new(ty.into())
    }
}

/// Dimensionality of an array.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct ArraySpecifierData {
    /// List of all the dimensions – possibly unsized or explicitly-sized.
    pub dimensions: Vec<ArraySpecifierDimension>,
}

impl_node_content! {
    /// Type alias for `Node<ArraySpecifierData>`.
    pub type ArraySpecifier = Node<ArraySpecifierData>;
}

/// One array specifier dimension.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum ArraySpecifierDimensionData {
    /// `[]` dimension
    Unsized,
    /// `[expr]` dimension
    ExplicitlySized(Box<Expr>),
}

impl_node_content! {
    /// Type alias for `Node<ArraySpecifierDimensionData>`.
    pub type ArraySpecifierDimension = Node<ArraySpecifierDimensionData>;
}

/// A declaration.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum DeclarationData {
    /// Function prototype declaration
    FunctionPrototype(FunctionPrototype),
    /// List of declarators and initializers
    InitDeclaratorList(InitDeclaratorList),
    /// Precision declaration
    Precision(PrecisionQualifier, TypeSpecifier),
    /// Block declaration
    Block(Block),
    /// Invariant declaration
    Invariant(Identifier),
    /// Type-only declaration
    TypeOnly(TypeQualifier),
}

impl_node_content! {
    /// Type alias for `Node<DeclarationData>`.
    pub type Declaration = Node<DeclarationData>;
}

/// A general purpose block, containing fields and possibly a list of declared identifiers. Semantic
/// is given with the storage qualifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct BlockData {
    /// Block type qualifier
    pub qualifier: TypeQualifier,
    /// Block name
    pub name: Identifier,
    /// Declared fields
    pub fields: Vec<StructFieldSpecifier>,
    /// Associated identifiers
    pub identifier: Option<ArrayedIdentifier>,
}

impl_node_content! {
    /// Type alias for `Node<BlockData>`.
    pub type Block = Node<BlockData>;
}

/// Function identifier.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum FunIdentifierData {
    /// Type name used for the function name (as a constructor)
    TypeSpecifier(Box<TypeSpecifier>),
    /// Expression used for the function name
    Expr(Box<Expr>),
}

impl_node_content! {
    /// Type alias for `Node<FunIdentifierData>`.
    pub type FunIdentifier = Node<FunIdentifierData>;
}

impl FunIdentifierData {
    /// Create a function identifier from an identifier
    pub fn ident(i: impl Into<IdentifierData>) -> Self {
        Self::Expr(Box::new(ExprData::Variable(i.into().into()).into()))
    }

    /// Try to parse this function identifier as a raw identifier
    pub fn as_ident(&self) -> Option<&Identifier> {
        match self {
            Self::Expr(expr) => match &***expr {
                ExprData::Variable(ident) => Some(ident),
                _ => None,
            },
            _ => None,
        }
    }

    /// Try to parse this function identifier as a mutable raw identifier
    pub fn as_ident_mut(&mut self) -> Option<&mut Identifier> {
        match self {
            Self::Expr(expr) => match &mut ***expr {
                ExprData::Variable(ident) => Some(ident),
                _ => None,
            },
            _ => None,
        }
    }

    /// Try to parse this function identifier as a `glsl-lang-quote` Rust identifier
    pub fn as_rs_ident(&self) -> Option<&str> {
        if let Some(ident) = self.as_ident() {
            ident.as_rs_ident()
        } else {
            None
        }
    }
}

/// Function prototype.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct FunctionPrototypeData {
    /// Return type
    pub ty: FullySpecifiedType,
    /// Function name
    pub name: Identifier,
    /// Function parameters
    pub parameters: Vec<FunctionParameterDeclaration>,
}

impl_node_content! {
    /// Type alias for `Node<FunctionPrototypeData>`.
    pub type FunctionPrototype = Node<FunctionPrototypeData>;
}

/// Function parameter declaration.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum FunctionParameterDeclarationData {
    /// Named parameter
    Named(Option<TypeQualifier>, FunctionParameterDeclarator),
    /// Unnamed parameter
    Unnamed(Option<TypeQualifier>, TypeSpecifier),
}

impl_node_content! {
    /// Type alias for `Node<FunctionParameterDeclarationData>`.
    pub type FunctionParameterDeclaration = Node<FunctionParameterDeclarationData>;
}

/// Function parameter declarator.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct FunctionParameterDeclaratorData {
    /// Parameter type
    pub ty: TypeSpecifier,
    /// Parameter name
    pub ident: ArrayedIdentifier,
}

impl_node_content! {
    /// Type alias for `Node<FunctionParameterDeclaratorData>`.
    pub type FunctionParameterDeclarator = Node<FunctionParameterDeclaratorData>;
}

/// Init declarator list.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct InitDeclaratorListData {
    /// First declaration
    pub head: SingleDeclaration,
    /// Following declarations
    pub tail: Vec<SingleDeclarationNoType>,
}

impl_node_content! {
    /// Type alias for `Node<InitDeclaratorListData>`.
    pub type InitDeclaratorList = Node<InitDeclaratorListData>;
}

/// Single declaration.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct SingleDeclarationData {
    /// Declaration type
    pub ty: FullySpecifiedType,
    /// Declared identifier
    pub name: Option<Identifier>,
    /// Array specification
    pub array_specifier: Option<ArraySpecifier>,
    /// Initializer expression
    pub initializer: Option<Initializer>,
}

impl_node_content! {
    /// Type alias for `Node<SingleDeclarationData>`.
    pub type SingleDeclaration = Node<SingleDeclarationData>;
}

/// A single declaration with implicit, already-defined type.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct SingleDeclarationNoTypeData {
    /// Declared identifier
    pub ident: ArrayedIdentifier,
    /// Initializer expression
    pub initializer: Option<Initializer>,
}

impl_node_content! {
    /// Type alias for `Node<SingleDeclarationNoTypeData>`.
    pub type SingleDeclarationNoType = Node<SingleDeclarationNoTypeData>;
}

/// Initializer.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum InitializerData {
    /// Simple initializer
    Simple(Box<Expr>),
    /// Multiple initializer
    List(Vec<Initializer>),
}

impl_node_content! {
    /// Type alias for `Node<InitializerData>`.
    pub type Initializer = Node<InitializerData>;
}

impl From<ExprData> for InitializerData {
    fn from(e: ExprData) -> Self {
        Self::Simple(Box::new(e.into()))
    }
}

impl From<Expr> for InitializerData {
    fn from(e: Expr) -> Self {
        Self::Simple(Box::new(e))
    }
}

/// The most general form of an expression.
///
/// As you can see if you read the variant list, in GLSL, an assignment is an expression. This is a
/// bit silly but think of an assignment as a statement first then an expression which evaluates to
/// what the statement “returns”.
///
/// An expression is either an assignment or a list (comma) of assignments.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum ExprData {
    /// A variable expression, using an identifier.
    Variable(Identifier),
    /// Integral constant expression.
    IntConst(i32),
    /// Unsigned integral constant expression.
    UIntConst(u32),
    /// Boolean constant expression.
    BoolConst(bool),
    /// Single precision floating expression.
    FloatConst(f32),
    /// Double precision floating expression.
    DoubleConst(f64),
    /// A unary expression, gathering a single expression and a unary operator.
    Unary(UnaryOp, Box<Expr>),
    /// A binary expression, gathering two expressions and a binary operator.
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    /// A ternary conditional expression, gathering three expressions.
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    /// An assignment is also an expression. Gathers an expression that defines what to assign to, an
    /// assignment operator and the value to associate with.
    Assignment(Box<Expr>, AssignmentOp, Box<Expr>),
    /// Add an array specifier to an expression.
    Bracket(Box<Expr>, Box<Expr>),
    /// A functional call. It has a function identifier and a list of expressions (arguments).
    FunCall(FunIdentifier, Vec<Expr>),
    /// An expression associated with a field selection (struct).
    Dot(Box<Expr>, Identifier),
    /// Post-incrementation of an expression.
    PostInc(Box<Expr>),
    /// Post-decrementation of an expression.
    PostDec(Box<Expr>),
    /// An expression that contains several, separated with comma.
    Comma(Box<Expr>, Box<Expr>),
}

impl_node_content! {
    /// Type alias for `Node<ExprData>`.
    pub type Expr = Node<ExprData>;
}

impl ExprData {
    /// Construct an `Expr::Variable(name)` from an identifier `name`
    pub fn variable(name: impl Into<IdentifierData>) -> Self {
        Self::Variable(name.into().into())
    }

    /// Try to parse this function identifier as a `glsl-lang-quote` Rust identifier
    pub fn as_rs_ident(&self) -> Option<&str> {
        match self {
            Self::Variable(ident) => ident.as_rs_ident(),
            _ => None,
        }
    }
}

impl From<i32> for ExprData {
    fn from(x: i32) -> ExprData {
        Self::IntConst(x)
    }
}

impl From<u32> for ExprData {
    fn from(x: u32) -> ExprData {
        Self::UIntConst(x)
    }
}

impl From<bool> for ExprData {
    fn from(x: bool) -> ExprData {
        Self::BoolConst(x)
    }
}

impl From<f32> for ExprData {
    fn from(x: f32) -> ExprData {
        Self::FloatConst(x)
    }
}

impl From<f64> for ExprData {
    fn from(x: f64) -> ExprData {
        Self::DoubleConst(x)
    }
}

/// All unary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum UnaryOpData {
    /// `++` unary operator
    #[lang_util(display(extra = "++"))]
    Inc,
    /// `--` unary operator
    #[lang_util(display(extra = "--"))]
    Dec,
    /// `+` unary operator
    #[lang_util(display(extra = "+"))]
    Add,
    /// `-` unary operator
    #[lang_util(display(extra = "-"))]
    Minus,
    /// `!` unary operator
    #[lang_util(display(extra = "!"))]
    Not,
    /// `~` unary operator
    #[lang_util(display(extra = "~"))]
    Complement,
}

impl_node_content! {
    /// Type alias for `Node<UnaryOpData>`.
    pub type UnaryOp = Node<UnaryOpData>;
}

/// All binary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum BinaryOpData {
    /// `||` binary operator
    #[lang_util(display(extra = "||"))]
    Or,
    /// `^^` binary operator
    #[lang_util(display(extra = "^^"))]
    Xor,
    /// `&&` binary operator
    #[lang_util(display(extra = "&&"))]
    And,
    /// `|` binary operator
    #[lang_util(display(extra = "|"))]
    BitOr,
    /// `^` binary operator
    #[lang_util(display(extra = "^"))]
    BitXor,
    /// `&` binary operator
    #[lang_util(display(extra = "&"))]
    BitAnd,
    /// `==` binary operator
    #[lang_util(display(extra = "=="))]
    Equal,
    /// `!=` binary operator
    #[lang_util(display(extra = "!="))]
    NonEqual,
    /// `<` binary operator
    #[lang_util(display(extra = "<"))]
    Lt,
    /// `>` binary operator
    #[lang_util(display(extra = ">"))]
    Gt,
    /// `<=` binary operator
    #[lang_util(display(extra = "<="))]
    Lte,
    /// `>=` binary operator
    #[lang_util(display(extra = ">="))]
    Gte,
    /// `<<` binary operator
    #[lang_util(display(extra = "<<"))]
    LShift,
    /// `>>` binary operator
    #[lang_util(display(extra = ">>"))]
    RShift,
    /// `+` binary operator
    #[lang_util(display(extra = "+"))]
    Add,
    /// `-` binary operator
    #[lang_util(display(extra = "-"))]
    Sub,
    /// `*` binary operator
    #[lang_util(display(extra = "*"))]
    Mult,
    /// `/` binary operator
    #[lang_util(display(extra = "/"))]
    Div,
    /// `%` binary operator
    #[lang_util(display(extra = "%"))]
    Mod,
}

impl_node_content! {
    /// Type alias for `Node<BinaryOpData>`.
    pub type BinaryOp = Node<BinaryOpData>;
}

/// All possible operators for assigning expressions.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum AssignmentOpData {
    /// `=` assignment operator
    #[lang_util(display(extra = "="))]
    Equal,
    /// `*` assignment operator
    #[lang_util(display(extra = "*"))]
    Mult,
    /// `/=` assignment operator
    #[lang_util(display(extra = "/="))]
    Div,
    /// `%=` assignment operator
    #[lang_util(display(extra = "%="))]
    Mod,
    /// `+=` assignment operator
    #[lang_util(display(extra = "+="))]
    Add,
    /// `-=` assignment operator
    #[lang_util(display(extra = "-="))]
    Sub,
    /// `<<=` assignment operator
    #[lang_util(display(extra = "<<="))]
    LShift,
    /// `>>=` assignment operator
    #[lang_util(display(extra = ">>="))]
    RShift,
    /// `&=` assignment operator
    #[lang_util(display(extra = "&="))]
    And,
    /// `^=` assignment operator
    #[lang_util(display(extra = "^="))]
    Xor,
    /// `|=` assignment operator
    #[lang_util(display(extra = "|="))]
    Or,
}

impl_node_content! {
    /// Type alias for `Node<AssignmentOpData>`.
    pub type AssignmentOp = Node<AssignmentOpData>;
}

/// Starting rule.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct TranslationUnit(pub Vec<ExternalDeclaration>);

impl NodeContent for TranslationUnit {}

/// External declaration.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum ExternalDeclarationData {
    /// Preprocessor directive
    Preprocessor(Preprocessor),
    /// Function definition
    FunctionDefinition(FunctionDefinition),
    /// Declaration
    Declaration(Declaration),
}

impl_node_content! {
    /// Type alias for `Node<ExternalDeclarationData>`.
    pub type ExternalDeclaration = Node<ExternalDeclarationData>;
}

/// Function definition.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct FunctionDefinitionData {
    /// Function prototype
    pub prototype: FunctionPrototype,
    /// Function body
    pub statement: CompoundStatement,
}

impl_node_content! {
    /// Type alias for `Node<FunctionDefinitionData>`.
    pub type FunctionDefinition = Node<FunctionDefinitionData>;
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct CompoundStatementData {
    /// List of statements
    pub statement_list: Vec<Statement>,
}

impl_node_content! {
    /// Type alias for `Node<CompoundStatementData>`.
    pub type CompoundStatement = Node<CompoundStatementData>;
}

impl FromIterator<Statement> for CompoundStatementData {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Statement>,
    {
        Self {
            statement_list: iter.into_iter().collect(),
        }
    }
}

/// Statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum StatementData {
    /// Declaration
    Declaration(Declaration),
    /// Expression statement
    Expression(ExprStatement),
    /// `if/...` statement
    Selection(SelectionStatement),
    /// `switch` statement
    Switch(SwitchStatement),
    /// Switch statement case label
    CaseLabel(CaseLabel),
    /// Iteration statement
    Iteration(IterationStatement),
    /// Jump statement
    Jump(JumpStatement),
    /// Statement block
    Compound(CompoundStatement),
}

impl_node_content! {
    /// Type alias for `Node<StatementData>`.
    pub type Statement = Node<StatementData>;
}

impl StatementData {
    /// Declare a new variable.
    ///
    /// `ty` is the type of the variable, `name` the name of the binding to create,
    /// `array_specifier` an optional argument to make your binding an array and
    /// `initializer`
    pub fn declare_var<T, N, A, I>(ty: T, name: N, array_specifier: A, initializer: I) -> Self
    where
        T: Into<FullySpecifiedTypeData>,
        N: Into<IdentifierData>,
        A: Into<Option<ArraySpecifier>>,
        I: Into<Option<Initializer>>,
    {
        Self::Declaration(
            DeclarationData::InitDeclaratorList(
                InitDeclaratorListData {
                    head: SingleDeclarationData {
                        ty: ty.into().into(),
                        name: Some(name.into().into()),
                        array_specifier: array_specifier.into(),
                        initializer: initializer.into(),
                    }
                    .into(),
                    tail: Vec::new(),
                }
                .into(),
            )
            .into(),
        )
    }
}

/// Expression statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct ExprStatementData(pub Option<Expr>);

impl_node_content! {
    /// Type alias for `Node<ExprStatementData>`.
    pub type ExprStatement = Node<ExprStatementData>;
}

/// Selection statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct SelectionStatementData {
    /// Condition to evaluate
    pub cond: Box<Expr>,
    /// Rest of the selection statement
    pub rest: SelectionRestStatement,
}

impl_node_content! {
    /// Type alias for `Node<SelectionStatementData>`.
    pub type SelectionStatement = Node<SelectionStatementData>;
}

/// Condition.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum ConditionData {
    /// An expression
    Expr(Expr),
    /// A variable declaration used as a condition
    Assignment(Box<FullySpecifiedType>, Identifier, Initializer),
}

impl_node_content! {
    /// Type alias for `Node<ConditionData>`.
    pub type Condition = Node<ConditionData>;
}

/// Selection rest statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum SelectionRestStatementData {
    /// Body of the if.
    Statement(Box<Statement>),
    /// The first argument is the body of the if, the rest is the next statement.
    Else(Box<Statement>, Box<Statement>),
}

impl_node_content! {
    /// Type alias for `Node<SelectionRestStatementData>`.
    pub type SelectionRestStatement = Node<SelectionRestStatementData>;
}

/// Switch statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct SwitchStatementData {
    /// Expression to evaluate and switch on
    pub head: Box<Expr>,
    /// Body of the switch statement
    pub body: Vec<Statement>,
}

impl_node_content! {
    /// Type alias for `Node<SwitchStatementData>`.
    pub type SwitchStatement = Node<SwitchStatementData>;
}

/// Case label statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum CaseLabelData {
    /// `case:` case label
    Case(Box<Expr>),
    /// `default:` case label
    Def,
}

impl_node_content! {
    /// Type alias for `Node<CaseLabelData>`.
    pub type CaseLabel = Node<CaseLabelData>;
}

/// Iteration statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum IterationStatementData {
    /// `while` iteration statement
    #[lang_util(display(extra = "while"))]
    While(Condition, Box<Statement>),
    /// `do` iteration statement
    #[lang_util(display(extra = "do"))]
    DoWhile(Box<Statement>, Box<Expr>),
    /// `for` iteration statement
    #[lang_util(display(extra = "for"))]
    For(ForInitStatement, ForRestStatement, Box<Statement>),
}

impl_node_content! {
    /// Type alias for `Node<IterationStatementData>`.
    pub type IterationStatement = Node<IterationStatementData>;
}

/// For init statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum ForInitStatementData {
    /// Expression
    Expression(Option<Expr>),
    /// Variable declaration
    Declaration(Box<Declaration>),
}

impl_node_content! {
    /// Type alias for `Node<ForInitStatementData>`.
    pub type ForInitStatement = Node<ForInitStatementData>;
}

/// For init statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct ForRestStatementData {
    /// Loop condition
    pub condition: Option<Condition>,
    /// Loop increment operation
    pub post_expr: Option<Box<Expr>>,
}

impl_node_content! {
    /// Type alias for `Node<ForRestStatementData>`.
    pub type ForRestStatement = Node<ForRestStatementData>;
}

/// Jump statement.
#[derive(Clone, Debug, PartialEq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum JumpStatementData {
    /// `continue` jump statement
    #[lang_util(display(extra = "continue"))]
    Continue,
    /// `break` jump statement
    #[lang_util(display(extra = "break"))]
    Break,
    /// `return` jump statement
    #[lang_util(display(extra = "return"))]
    Return(Option<Box<Expr>>),
    /// `discard` jump statement
    #[lang_util(display(extra = "discard"))]
    Discard,
}

impl_node_content! {
    /// Type alias for `Node<JumpStatementData>`.
    pub type JumpStatement = Node<JumpStatementData>;
}

/// Some basic preprocessor directives.
///
/// As it’s important to carry them around the AST because they cannot be substituted in a normal
/// preprocessor (they’re used by GPU’s compilers), those preprocessor directives are available for
/// inspection.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PreprocessorData {
    /// `#define` preprocessor directive
    #[lang_util(display(extra = "#define"))]
    Define(PreprocessorDefine),
    /// `#else` preprocessor directive
    #[lang_util(display(extra = "#else"))]
    Else,
    /// `#elseif` preprocessor directive
    #[lang_util(display(extra = "#elseif"))]
    ElseIf(PreprocessorElseIf),
    /// `#endif` preprocessor directive
    #[lang_util(display(extra = "#endif"))]
    EndIf,
    /// `#error` preprocessor directive
    #[lang_util(display(extra = "#error"))]
    Error(PreprocessorError),
    /// `#if` preprocessor directive
    #[lang_util(display(extra = "#if"))]
    If(PreprocessorIf),
    /// `#ifdef` preprocessor directive
    #[lang_util(display(extra = "#ifdef"))]
    IfDef(PreprocessorIfDef),
    /// `#ifndef` preprocessor directive
    #[lang_util(display(extra = "#ifndef"))]
    IfNDef(PreprocessorIfNDef),
    /// `#include` preprocessor directive
    #[lang_util(display(extra = "#include"))]
    Include(PreprocessorInclude),
    /// `#line` preprocessor directive
    #[lang_util(display(extra = "#line"))]
    Line(PreprocessorLine),
    /// `#pragma` preprocessor directive
    #[lang_util(display(extra = "#pragma"))]
    Pragma(PreprocessorPragma),
    /// `#undef` preprocessor directive
    #[lang_util(display(extra = "#undef"))]
    Undef(PreprocessorUndef),
    /// `#version` preprocessor directive
    #[lang_util(display(extra = "#version"))]
    Version(PreprocessorVersion),
    /// `#extension` preprocessor directive
    #[lang_util(display(extra = "#extension"))]
    Extension(PreprocessorExtension),
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorData>`.
    pub type Preprocessor = Node<PreprocessorData>;
}

/// A #define preprocessor directive.
///
/// Allows any expression but only Integer and Float literals make sense
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PreprocessorDefineData {
    /// A preprocessor definition
    ObjectLike {
        /// Identifier for the definition
        ident: Identifier,
        /// Associated value
        value: String,
    },

    /// A preprocessor function definition
    FunctionLike {
        /// Identifier for the definition
        ident: Identifier,
        /// List of arguments for the function
        args: Vec<Identifier>,
        /// Associated value
        value: String,
    },
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorDefineData>`.
    pub type PreprocessorDefine = Node<PreprocessorDefineData>;
}

/// An #else preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorElseIfData {
    /// Condition expression
    #[lang_util(display(extra))]
    pub condition: String,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorElseIfData>`.
    pub type PreprocessorElseIf = Node<PreprocessorElseIfData>;
}

/// An #error preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorErrorData {
    /// Error message
    #[lang_util(display(extra))]
    pub message: String,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorErrorData>`.
    pub type PreprocessorError = Node<PreprocessorErrorData>;
}

/// An #if preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorIfData {
    /// Condition expression
    #[lang_util(display(extra))]
    pub condition: String,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorIfData>`.
    pub type PreprocessorIf = Node<PreprocessorIfData>;
}

/// An #ifdef preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorIfDefData {
    /// Identifier to test
    #[lang_util(display(extra))]
    pub ident: Identifier,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorIfDefData>`.
    pub type PreprocessorIfDef = Node<PreprocessorIfDefData>;
}

/// A #ifndef preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorIfNDefData {
    /// Identifier to test
    #[lang_util(display(extra))]
    pub ident: Identifier,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorIfNDefData>`.
    pub type PreprocessorIfNDef = Node<PreprocessorIfNDefData>;
}

/// An #include name annotation.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorIncludeData {
    /// Include path
    pub path: Path,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorIncludeData>`.
    pub type PreprocessorInclude = Node<PreprocessorIncludeData>;
}

/// A #line preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorLineData {
    /// Line index
    #[lang_util(display(extra))]
    pub line: u32,
    /// Source index
    pub source_string_number: Option<u32>,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorLineData>`.
    pub type PreprocessorLine = Node<PreprocessorLineData>;
}

/// A #pragma preprocessor directive.
/// Holds compiler-specific command.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorPragmaData {
    /// Raw pragma text
    #[lang_util(display(extra))]
    pub command: String,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorPragmaData>`.
    pub type PreprocessorPragma = Node<PreprocessorPragmaData>;
}

/// A #undef preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorUndefData {
    /// Identifier to undefine
    #[lang_util(display(extra))]
    pub name: Identifier,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorUndefData>`.
    pub type PreprocessorUndef = Node<PreprocessorUndefData>;
}

/// A #version preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorVersionData {
    /// Version number
    #[lang_util(display(extra))]
    pub version: u16,
    /// Version profile
    pub profile: Option<PreprocessorVersionProfile>,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorVersionData>`.
    pub type PreprocessorVersion = Node<PreprocessorVersionData>;
}

/// A #version profile annotation.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PreprocessorVersionProfileData {
    /// `core` version profile
    #[lang_util(display(extra = "core"))]
    Core,
    /// `compatibility` version profile
    #[lang_util(display(extra = "compatibility"))]
    Compatibility,
    /// `es` version profile
    #[lang_util(display(extra = "es"))]
    Es,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorVersionProfileData>`.
    pub type PreprocessorVersionProfile = Node<PreprocessorVersionProfileData>;
}

/// An #extension preprocessor directive.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct PreprocessorExtensionData {
    /// Name of the target extension
    pub name: PreprocessorExtensionName,
    /// Behavior for the extension
    pub behavior: Option<PreprocessorExtensionBehavior>,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorExtensionData>`.
    pub type PreprocessorExtension = Node<PreprocessorExtensionData>;
}

/// An #extension name annotation.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PreprocessorExtensionNameData {
    /// All extensions you could ever imagine in your whole lifetime (how crazy is that!).
    #[lang_util(display(extra = "all"))]
    All,
    /// A specific extension.
    Specific(SmolStr),
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorExtensionNameData>`.
    pub type PreprocessorExtensionName = Node<PreprocessorExtensionNameData>;
}

/// An #extension behavior annotation.
#[derive(Clone, Debug, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum PreprocessorExtensionBehaviorData {
    /// `require` preprocessor extension behavior
    #[lang_util(display(extra = "require"))]
    Require,
    /// `enable` preprocessor extension behavior
    #[lang_util(display(extra = "enable"))]
    Enable,
    /// `warn` preprocessor extension behavior
    #[lang_util(display(extra = "warn"))]
    Warn,
    /// `disable` preprocessor extension behavior
    #[lang_util(display(extra = "disable"))]
    Disable,
}

impl_node_content! {
    /// Type alias for `Node<PreprocessorExtensionBehaviorData>`.
    pub type PreprocessorExtensionBehavior = Node<PreprocessorExtensionBehaviorData>;
}

/// A comment
#[derive(Debug, Clone, PartialEq, Eq, NodeContentDisplay)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub enum CommentData {
    /// Single-line comment
    Single(String),
    /// Multi-line comment
    Multi(String),
}

impl_node_content! {
    /// Type alias for `Node<CommentData>`.
    pub type Comment = Node<CommentData>;
}

impl CommentData {
    /// Get the comment's text, regardless of its type
    pub fn text(&self) -> &str {
        match self {
            Self::Single(s) => s,
            Self::Multi(s) => s,
        }
    }

    /// true if this comment is a single-line comment
    pub fn is_single(&self) -> bool {
        matches!(self, Self::Multi(_))
    }

    /// true if this comment is a multi-line comment
    pub fn is_multi(&self) -> bool {
        matches!(self, Self::Multi(_))
    }
}
