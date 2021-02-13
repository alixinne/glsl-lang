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
//! [`Statement`]: crate::syntax::Statement
//! [`TranslationUnit`]: crate::syntax::TranslationUnit
//! [`Expr`]: crate::syntax::Expr
//! [`FunctionDefinition`]: crate::syntax::FunctionDefinition

use std::fmt;
use std::iter::FromIterator;

use glsl_lang_impl::NodeContent;

mod node;
pub use node::*;

mod display;
pub use display::*;

/// A path literal.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum Path {
    /// Specified with angle brackets.
    Absolute(String),
    /// Specified with double quotes.
    Relative(String),
}

/// A generic identifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
#[glsl_lang(display(leaf))]
pub struct IdentifierData(#[glsl_lang(display(extra))] pub String);

impl IdentifierData {
    pub fn as_rs_ident(&self) -> Option<&str> {
        if self.0.starts_with('#') & self.0.ends_with(')') {
            // Remove #\s* and )
            let s = (&self.0[1..self.0.len() - 1]).trim();
            // Remove ( and whitespace
            Some(s[1..].trim())
        } else {
            None
        }
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<&str> for IdentifierData {
    fn from(ident: &str) -> Self {
        Self(ident.to_owned())
    }
}

impl fmt::Display for IdentifierData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

/// Any type name.
#[derive(Clone, Debug, PartialEq, NodeContent)]
#[glsl_lang(display(leaf))]
pub struct TypeNameData(pub String);

impl TypeNameData {
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
        Self(ident.to_owned())
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

/// Type specifier (non-array).
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum TypeSpecifierNonArray {
    // transparent types
    #[glsl_lang(display(extra = "void"))]
    Void,
    #[glsl_lang(display(extra = "bool"))]
    Bool,
    #[glsl_lang(display(extra = "int"))]
    Int,
    #[glsl_lang(display(extra = "uint"))]
    UInt,
    #[glsl_lang(display(extra = "float"))]
    Float,
    #[glsl_lang(display(extra = "double"))]
    Double,
    #[glsl_lang(display(extra = "vec2"))]
    Vec2,
    #[glsl_lang(display(extra = "vec3"))]
    Vec3,
    #[glsl_lang(display(extra = "vec4"))]
    Vec4,
    #[glsl_lang(display(extra = "dvec2"))]
    DVec2,
    #[glsl_lang(display(extra = "dvec3"))]
    DVec3,
    #[glsl_lang(display(extra = "dvec4"))]
    DVec4,
    #[glsl_lang(display(extra = "bvec2"))]
    BVec2,
    #[glsl_lang(display(extra = "bvec3"))]
    BVec3,
    #[glsl_lang(display(extra = "bvec4"))]
    BVec4,
    #[glsl_lang(display(extra = "ivec2"))]
    IVec2,
    #[glsl_lang(display(extra = "ivec3"))]
    IVec3,
    #[glsl_lang(display(extra = "ivec4"))]
    IVec4,
    #[glsl_lang(display(extra = "uvec2"))]
    UVec2,
    #[glsl_lang(display(extra = "uvec3"))]
    UVec3,
    #[glsl_lang(display(extra = "uvec4"))]
    UVec4,
    #[glsl_lang(display(extra = "mat2"))]
    Mat2,
    #[glsl_lang(display(extra = "mat3"))]
    Mat3,
    #[glsl_lang(display(extra = "mat4"))]
    Mat4,
    #[glsl_lang(display(extra = "mat2x2"))]
    Mat22,
    #[glsl_lang(display(extra = "mat2x3"))]
    Mat23,
    #[glsl_lang(display(extra = "mat2x4"))]
    Mat24,
    #[glsl_lang(display(extra = "mat3x2"))]
    Mat32,
    #[glsl_lang(display(extra = "mat3x3"))]
    Mat33,
    #[glsl_lang(display(extra = "mat3x4"))]
    Mat34,
    #[glsl_lang(display(extra = "mat4x2"))]
    Mat42,
    #[glsl_lang(display(extra = "mat4x3"))]
    Mat43,
    #[glsl_lang(display(extra = "mat4x4"))]
    Mat44,
    #[glsl_lang(display(extra = "dmat2"))]
    DMat2,
    #[glsl_lang(display(extra = "dmat3"))]
    DMat3,
    #[glsl_lang(display(extra = "dmat4"))]
    DMat4,
    #[glsl_lang(display(extra = "dmat2x2"))]
    DMat22,
    #[glsl_lang(display(extra = "dmat2x3"))]
    DMat23,
    #[glsl_lang(display(extra = "dmat2x4"))]
    DMat24,
    #[glsl_lang(display(extra = "dmat3x2"))]
    DMat32,
    #[glsl_lang(display(extra = "dmat3x3"))]
    DMat33,
    #[glsl_lang(display(extra = "dmat3x4"))]
    DMat34,
    #[glsl_lang(display(extra = "dmat4x2"))]
    DMat42,
    #[glsl_lang(display(extra = "dmat4x3"))]
    DMat43,
    #[glsl_lang(display(extra = "dmat4x4"))]
    DMat44,
    // floating point opaque types
    #[glsl_lang(display(extra = "sampler1D"))]
    Sampler1D,
    #[glsl_lang(display(extra = "image1D"))]
    Image1D,
    #[glsl_lang(display(extra = "sampler2D"))]
    Sampler2D,
    #[glsl_lang(display(extra = "image2D"))]
    Image2D,
    #[glsl_lang(display(extra = "sampler3D"))]
    Sampler3D,
    #[glsl_lang(display(extra = "image3D"))]
    Image3D,
    #[glsl_lang(display(extra = "samplerCube"))]
    SamplerCube,
    #[glsl_lang(display(extra = "imageCube"))]
    ImageCube,
    #[glsl_lang(display(extra = "sampler2DRect"))]
    Sampler2DRect,
    #[glsl_lang(display(extra = "image2DRect"))]
    Image2DRect,
    #[glsl_lang(display(extra = "sampler1DArray"))]
    Sampler1DArray,
    #[glsl_lang(display(extra = "image1DArray"))]
    Image1DArray,
    #[glsl_lang(display(extra = "sampler2DArray"))]
    Sampler2DArray,
    #[glsl_lang(display(extra = "image2DArray"))]
    Image2DArray,
    #[glsl_lang(display(extra = "samplerBuffer"))]
    SamplerBuffer,
    #[glsl_lang(display(extra = "imageBuffer"))]
    ImageBuffer,
    #[glsl_lang(display(extra = "sampler2DMS"))]
    Sampler2DMS,
    #[glsl_lang(display(extra = "image2DMS"))]
    Image2DMS,
    #[glsl_lang(display(extra = "sampler2DMSArray"))]
    Sampler2DMSArray,
    #[glsl_lang(display(extra = "image2DMSArray"))]
    Image2DMSArray,
    #[glsl_lang(display(extra = "samplerCubeArray"))]
    SamplerCubeArray,
    #[glsl_lang(display(extra = "imageCubeArray"))]
    ImageCubeArray,
    #[glsl_lang(display(extra = "sampler1DShadow"))]
    Sampler1DShadow,
    #[glsl_lang(display(extra = "sampler2DShadow"))]
    Sampler2DShadow,
    #[glsl_lang(display(extra = "sampler2DRectShadow"))]
    Sampler2DRectShadow,
    #[glsl_lang(display(extra = "sampler1DArrayShadow"))]
    Sampler1DArrayShadow,
    #[glsl_lang(display(extra = "sampler2DArrayShadow"))]
    Sampler2DArrayShadow,
    #[glsl_lang(display(extra = "samplerCubeShadow"))]
    SamplerCubeShadow,
    #[glsl_lang(display(extra = "samplerCubeArrayShadow"))]
    SamplerCubeArrayShadow,
    // signed integer opaque types
    #[glsl_lang(display(extra = "isampler1D"))]
    ISampler1D,
    #[glsl_lang(display(extra = "iimage1D"))]
    IImage1D,
    #[glsl_lang(display(extra = "isampler2D"))]
    ISampler2D,
    #[glsl_lang(display(extra = "iimage2D"))]
    IImage2D,
    #[glsl_lang(display(extra = "isampler3D"))]
    ISampler3D,
    #[glsl_lang(display(extra = "iimage3D"))]
    IImage3D,
    #[glsl_lang(display(extra = "isamplerCube"))]
    ISamplerCube,
    #[glsl_lang(display(extra = "iimageCube"))]
    IImageCube,
    #[glsl_lang(display(extra = "isampler2DRect"))]
    ISampler2DRect,
    #[glsl_lang(display(extra = "iimage2DRect"))]
    IImage2DRect,
    #[glsl_lang(display(extra = "isampler1DArray"))]
    ISampler1DArray,
    #[glsl_lang(display(extra = "iimage1DArray"))]
    IImage1DArray,
    #[glsl_lang(display(extra = "isampler2DArray"))]
    ISampler2DArray,
    #[glsl_lang(display(extra = "iimage2DArray"))]
    IImage2DArray,
    #[glsl_lang(display(extra = "isamplerBuffer"))]
    ISamplerBuffer,
    #[glsl_lang(display(extra = "iimageBuffer"))]
    IImageBuffer,
    #[glsl_lang(display(extra = "isampler2DMS"))]
    ISampler2DMS,
    #[glsl_lang(display(extra = "iimage2DMS"))]
    IImage2DMS,
    #[glsl_lang(display(extra = "isampler2DMSArray"))]
    ISampler2DMSArray,
    #[glsl_lang(display(extra = "iimage2DMSArray"))]
    IImage2DMSArray,
    #[glsl_lang(display(extra = "isamplerCubeArray"))]
    ISamplerCubeArray,
    #[glsl_lang(display(extra = "iimageCubeArray"))]
    IImageCubeArray,
    // unsigned integer opaque types
    #[glsl_lang(display(extra = "atomic_uint"))]
    AtomicUInt,
    #[glsl_lang(display(extra = "usampler1D"))]
    USampler1D,
    #[glsl_lang(display(extra = "uimage1D"))]
    UImage1D,
    #[glsl_lang(display(extra = "usampler2D"))]
    USampler2D,
    #[glsl_lang(display(extra = "uimage2D"))]
    UImage2D,
    #[glsl_lang(display(extra = "usampler3D"))]
    USampler3D,
    #[glsl_lang(display(extra = "uimage3D"))]
    UImage3D,
    #[glsl_lang(display(extra = "usamplerCube"))]
    USamplerCube,
    #[glsl_lang(display(extra = "uimageCube"))]
    UImageCube,
    #[glsl_lang(display(extra = "usampler2DRect"))]
    USampler2DRect,
    #[glsl_lang(display(extra = "uimage2DRect"))]
    UImage2DRect,
    #[glsl_lang(display(extra = "usampler1DArray"))]
    USampler1DArray,
    #[glsl_lang(display(extra = "uimage1DArray"))]
    UImage1DArray,
    #[glsl_lang(display(extra = "usampler2DArray"))]
    USampler2DArray,
    #[glsl_lang(display(extra = "uimage2DArray"))]
    UImage2DArray,
    #[glsl_lang(display(extra = "usamplerBuffer"))]
    USamplerBuffer,
    #[glsl_lang(display(extra = "uimageBuffer"))]
    UImageBuffer,
    #[glsl_lang(display(extra = "usampler2DMS"))]
    USampler2DMS,
    #[glsl_lang(display(extra = "uimage2DMS"))]
    UImage2DMS,
    #[glsl_lang(display(extra = "usampler2DMSArray"))]
    USampler2DMSArray,
    #[glsl_lang(display(extra = "uimage2DMSArray"))]
    UImage2DMSArray,
    #[glsl_lang(display(extra = "usamplerCubeArray"))]
    USamplerCubeArray,
    #[glsl_lang(display(extra = "uimageCubeArray"))]
    UImageCubeArray,
    #[glsl_lang(display(extra = "struct"))]
    Struct(StructSpecifier),
    TypeName(TypeName),
}

impl From<TypeName> for TypeSpecifierNonArray {
    fn from(tn: TypeName) -> Self {
        Self::TypeName(tn)
    }
}

/// Type specifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct TypeSpecifier {
    pub ty: TypeSpecifierNonArray,
    pub array_specifier: Option<ArraySpecifier>,
}

impl<T: Into<TypeSpecifierNonArray>> From<T> for TypeSpecifier {
    fn from(ty: T) -> Self {
        Self {
            ty: ty.into(),
            array_specifier: None,
        }
    }
}

/// Struct specifier. Used to create new, user-defined types.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct StructSpecifier {
    pub name: Option<TypeName>,
    pub fields: Vec<StructFieldSpecifier>,
}

/// Struct field specifier. Used to add fields to struct specifiers.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct StructFieldSpecifier {
    pub qualifier: Option<TypeQualifier>,
    pub ty: TypeSpecifier,
    pub identifiers: Vec<ArrayedIdentifier>, // several identifiers of the same type
}

/// An identifier with an optional array specifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct ArrayedIdentifier {
    pub ident: Identifier,
    pub array_spec: Option<ArraySpecifier>,
}

impl ArrayedIdentifier {
    pub fn new<I, AS>(ident: I, array_spec: AS) -> Self
    where
        I: Into<Identifier>,
        AS: Into<Option<ArraySpecifier>>,
    {
        ArrayedIdentifier {
            ident: ident.into(),
            array_spec: array_spec.into(),
        }
    }
}

impl From<&str> for ArrayedIdentifier {
    fn from(ident: &str) -> Self {
        ArrayedIdentifier {
            ident: ident.into(),
            array_spec: None,
        }
    }
}

/// Type qualifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct TypeQualifier {
    pub qualifiers: Vec<TypeQualifierSpec>,
}

/// Type qualifier spec.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum TypeQualifierSpec {
    Storage(StorageQualifier),
    Layout(LayoutQualifier),
    Precision(PrecisionQualifier),
    Interpolation(InterpolationQualifier),
    Invariant,
    Precise,
}

/// Storage qualifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum StorageQualifier {
    #[glsl_lang(display(extra = "const"))]
    Const,
    #[glsl_lang(display(extra = "inout"))]
    InOut,
    #[glsl_lang(display(extra = "in"))]
    In,
    #[glsl_lang(display(extra = "out"))]
    Out,
    #[glsl_lang(display(extra = "centroid"))]
    Centroid,
    #[glsl_lang(display(extra = "patch"))]
    Patch,
    #[glsl_lang(display(extra = "sample"))]
    Sample,
    #[glsl_lang(display(extra = "uniform"))]
    Uniform,
    #[glsl_lang(display(extra = "buffer"))]
    Buffer,
    #[glsl_lang(display(extra = "shared"))]
    Shared,
    #[glsl_lang(display(extra = "coherent"))]
    Coherent,
    #[glsl_lang(display(extra = "volatile"))]
    Volatile,
    #[glsl_lang(display(extra = "restrict"))]
    Restrict,
    #[glsl_lang(display(extra = "readonly"))]
    ReadOnly,
    #[glsl_lang(display(extra = "writeonly"))]
    WriteOnly,
    // Note: the grammar says TYPE_NAME but type_specifier makes more sense given the definition of
    // subroutine. The reference implementation is marked "to do".
    #[glsl_lang(display(extra = "subroutine"))]
    Subroutine(Vec<TypeSpecifier>),
}

/// Layout qualifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct LayoutQualifier {
    pub ids: Vec<LayoutQualifierSpec>,
}

/// Layout qualifier spec.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum LayoutQualifierSpec {
    Identifier(Identifier, Option<Box<Expr>>),
    #[glsl_lang(display(extra = "shared"))]
    Shared,
}

/// Precision qualifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum PrecisionQualifier {
    #[glsl_lang(display(extra = "high"))]
    High,
    #[glsl_lang(display(extra = "medium"))]
    Medium,
    #[glsl_lang(display(extra = "low"))]
    Low,
}

/// Interpolation qualifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum InterpolationQualifier {
    #[glsl_lang(display(extra = "smooth"))]
    Smooth,
    #[glsl_lang(display(extra = "flat"))]
    Flat,
    #[glsl_lang(display(extra = "noperspective"))]
    NoPerspective,
}

/// Fully specified type.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct FullySpecifiedType {
    pub qualifier: Option<TypeQualifier>,
    pub ty: TypeSpecifier,
}

impl FullySpecifiedType {
    pub fn new(ty: TypeSpecifierNonArray) -> Self {
        Self {
            qualifier: None,
            ty: TypeSpecifier {
                ty,
                array_specifier: None,
            },
        }
    }
}

impl From<TypeSpecifierNonArray> for FullySpecifiedType {
    fn from(ty: TypeSpecifierNonArray) -> Self {
        FullySpecifiedType::new(ty)
    }
}

/// Dimensionality of an array.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct ArraySpecifier {
    /// List of all the dimensions – possibly unsized or explicitly-sized.
    pub dimensions: Vec<ArraySpecifierDimension>,
}

/// One array specifier dimension.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum ArraySpecifierDimension {
    Unsized,
    ExplicitlySized(Box<Expr>),
}

/// A declaration.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum DeclarationData {
    FunctionPrototype(FunctionPrototype),
    InitDeclaratorList(InitDeclaratorList),
    Precision(PrecisionQualifier, TypeSpecifier),
    Block(Block),
}

/// A general purpose block, containing fields and possibly a list of declared identifiers. Semantic
/// is given with the storage qualifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct Block {
    pub qualifier: TypeQualifier,
    pub name: Identifier,
    pub fields: Vec<StructFieldSpecifier>,
    pub identifier: Option<ArrayedIdentifier>,
}

/// Function identifier.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum FunIdentifier {
    TypeSpecifier(TypeSpecifier),
    Expr(Box<Expr>),
}

impl FunIdentifier {
    pub fn ident(i: impl Into<Identifier>) -> Self {
        Self::Expr(Box::new(Expr::Variable(i.into())))
    }

    pub fn as_ident(&self) -> Option<&Identifier> {
        match self {
            Self::Expr(expr) => match &**expr {
                Expr::Variable(ident) => Some(ident),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_ident_mut(&mut self) -> Option<&mut Identifier> {
        match self {
            Self::Expr(expr) => match &mut **expr {
                Expr::Variable(ident) => Some(ident),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_rs_ident(&self) -> Option<&str> {
        if let Some(ident) = self.as_ident() {
            ident.as_rs_ident()
        } else {
            None
        }
    }
}

/// Function prototype.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct FunctionPrototypeData {
    pub ty: FullySpecifiedType,
    pub name: Identifier,
    pub parameters: Vec<FunctionParameterDeclaration>,
}

/// Function parameter declaration.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum FunctionParameterDeclarationData {
    Named(Option<TypeQualifier>, FunctionParameterDeclarator),
    Unnamed(Option<TypeQualifier>, TypeSpecifier),
}

/// Function parameter declarator.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct FunctionParameterDeclarator {
    pub ty: TypeSpecifier,
    pub ident: ArrayedIdentifier,
}

/// Init declarator list.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct InitDeclaratorList {
    pub head: SingleDeclaration,
    pub tail: Vec<SingleDeclarationNoType>,
}

/// Single declaration.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct SingleDeclaration {
    pub ty: FullySpecifiedType,
    pub name: Option<Identifier>,
    pub array_specifier: Option<ArraySpecifier>,
    pub initializer: Option<Initializer>,
}

/// A single declaration with implicit, already-defined type.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct SingleDeclarationNoType {
    pub ident: ArrayedIdentifier,
    pub initializer: Option<Initializer>,
}

/// Initializer.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum Initializer {
    Simple(Box<Expr>),
    List(Vec<Initializer>),
}

impl From<Expr> for Initializer {
    fn from(e: Expr) -> Self {
        Initializer::Simple(Box::new(e))
    }
}

/// The most general form of an expression. As you can see if you read the variant list, in GLSL, an
/// assignment is an expression. This is a bit silly but think of an assignment as a statement first
/// then an expression which evaluates to what the statement “returns”.
///
/// An expression is either an assignment or a list (comma) of assignments.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum Expr {
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

impl Expr {
    pub fn variable(name: impl Into<IdentifierData>) -> Self {
        Self::Variable(name.into().into())
    }

    pub fn as_rs_ident(&self) -> Option<&str> {
        match self {
            Self::Variable(ident) => ident.as_rs_ident(),
            _ => None,
        }
    }
}

impl From<i32> for Expr {
    fn from(x: i32) -> Expr {
        Expr::IntConst(x)
    }
}

impl From<u32> for Expr {
    fn from(x: u32) -> Expr {
        Expr::UIntConst(x)
    }
}

impl From<bool> for Expr {
    fn from(x: bool) -> Expr {
        Expr::BoolConst(x)
    }
}

impl From<f32> for Expr {
    fn from(x: f32) -> Expr {
        Expr::FloatConst(x)
    }
}

impl From<f64> for Expr {
    fn from(x: f64) -> Expr {
        Expr::DoubleConst(x)
    }
}

/// All unary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum UnaryOp {
    #[glsl_lang(display(extra = "++"))]
    Inc,
    #[glsl_lang(display(extra = "--"))]
    Dec,
    #[glsl_lang(display(extra = "+"))]
    Add,
    #[glsl_lang(display(extra = "-"))]
    Minus,
    #[glsl_lang(display(extra = "!"))]
    Not,
    #[glsl_lang(display(extra = "~"))]
    Complement,
}

/// All binary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum BinaryOp {
    #[glsl_lang(display(extra = "||"))]
    Or,
    #[glsl_lang(display(extra = "^^"))]
    Xor,
    #[glsl_lang(display(extra = "&&"))]
    And,
    #[glsl_lang(display(extra = "|"))]
    BitOr,
    #[glsl_lang(display(extra = "^"))]
    BitXor,
    #[glsl_lang(display(extra = "&"))]
    BitAnd,
    #[glsl_lang(display(extra = "=="))]
    Equal,
    #[glsl_lang(display(extra = "!="))]
    NonEqual,
    #[glsl_lang(display(extra = "<"))]
    LT,
    #[glsl_lang(display(extra = ">"))]
    GT,
    #[glsl_lang(display(extra = "<="))]
    LTE,
    #[glsl_lang(display(extra = ">="))]
    GTE,
    #[glsl_lang(display(extra = "<<"))]
    LShift,
    #[glsl_lang(display(extra = ">>"))]
    RShift,
    #[glsl_lang(display(extra = "+"))]
    Add,
    #[glsl_lang(display(extra = "-"))]
    Sub,
    #[glsl_lang(display(extra = "*"))]
    Mult,
    #[glsl_lang(display(extra = "/"))]
    Div,
    #[glsl_lang(display(extra = "%"))]
    Mod,
}

/// All possible operators for assigning expressions.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum AssignmentOp {
    #[glsl_lang(display(extra = "="))]
    Equal,
    #[glsl_lang(display(extra = "*"))]
    Mult,
    #[glsl_lang(display(extra = "/="))]
    Div,
    #[glsl_lang(display(extra = "%="))]
    Mod,
    #[glsl_lang(display(extra = "+="))]
    Add,
    #[glsl_lang(display(extra = "-="))]
    Sub,
    #[glsl_lang(display(extra = "<<="))]
    LShift,
    #[glsl_lang(display(extra = ">>="))]
    RShift,
    #[glsl_lang(display(extra = "&="))]
    And,
    #[glsl_lang(display(extra = "^="))]
    Xor,
    #[glsl_lang(display(extra = "|="))]
    Or,
}

/// Starting rule.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct TranslationUnit(pub Vec<ExternalDeclaration>);

/// External declaration.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum ExternalDeclarationData {
    Preprocessor(Preprocessor),
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}

/// Function definition.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct FunctionDefinitionData {
    pub prototype: FunctionPrototype,
    pub statement: CompoundStatement,
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct CompoundStatementData {
    pub statement_list: Vec<Statement>,
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
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum StatementData {
    Declaration(Declaration),
    Expression(ExprStatement),
    Selection(SelectionStatement),
    Switch(SwitchStatement),
    CaseLabel(CaseLabel),
    Iteration(IterationStatement),
    Jump(JumpStatement),
    Compound(CompoundStatement),
}

impl StatementData {
    /// Declare a new variable.
    ///
    /// `ty` is the type of the variable, `name` the name of the binding to create,
    /// `array_specifier` an optional argument to make your binding an array and
    /// `initializer`
    pub fn declare_var<T, N, A, I>(ty: T, name: N, array_specifier: A, initializer: I) -> Self
    where
        T: Into<FullySpecifiedType>,
        N: Into<Identifier>,
        A: Into<Option<ArraySpecifier>>,
        I: Into<Option<Initializer>>,
    {
        Self::Declaration(
            DeclarationData::InitDeclaratorList(InitDeclaratorList {
                head: SingleDeclaration {
                    ty: ty.into(),
                    name: Some(name.into()),
                    array_specifier: array_specifier.into(),
                    initializer: initializer.into(),
                },
                tail: Vec::new(),
            })
            .into(),
        )
    }
}

/// Expression statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct ExprStatement(pub Option<Expr>);

/// Selection statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct SelectionStatement {
    pub cond: Box<Expr>,
    pub rest: SelectionRestStatement,
}

/// Condition.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum Condition {
    Expr(Expr),
    Assignment(FullySpecifiedType, Identifier, Initializer),
}

/// Selection rest statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum SelectionRestStatement {
    /// Body of the if.
    Statement(Box<Statement>),
    /// The first argument is the body of the if, the rest is the next statement.
    Else(Box<Statement>, Box<Statement>),
}

/// Switch statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct SwitchStatement {
    pub head: Box<Expr>,
    pub body: Vec<Statement>,
}

/// Case label statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum CaseLabel {
    Case(Box<Expr>),
    Def,
}

/// Iteration statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum IterationStatement {
    #[glsl_lang(display(extra = "while"))]
    While(Condition, Box<Statement>),
    #[glsl_lang(display(extra = "do"))]
    DoWhile(Box<Statement>, Box<Expr>),
    #[glsl_lang(display(extra = "for"))]
    For(ForInitStatement, ForRestStatement, Box<Statement>),
}

/// For init statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum ForInitStatement {
    Expression(Option<Expr>),
    Declaration(Box<Declaration>),
}

/// For init statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct ForRestStatement {
    pub condition: Option<Condition>,
    pub post_expr: Option<Box<Expr>>,
}

/// Jump statement.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum JumpStatement {
    #[glsl_lang(display(extra = "continue"))]
    Continue,
    #[glsl_lang(display(extra = "break"))]
    Break,
    #[glsl_lang(display(extra = "return"))]
    Return(Option<Box<Expr>>),
    #[glsl_lang(display(extra = "discard"))]
    Discard,
}

/// Some basic preprocessor directives.
///
/// As it’s important to carry them around the AST because they cannot be substituted in a normal
/// preprocessor (they’re used by GPU’s compilers), those preprocessor directives are available for
/// inspection.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum PreprocessorData {
    #[glsl_lang(display(extra = "#define"))]
    Define(PreprocessorDefine),
    #[glsl_lang(display(extra = "#else"))]
    Else,
    #[glsl_lang(display(extra = "#elseif"))]
    ElseIf(PreprocessorElseIf),
    #[glsl_lang(display(extra = "#endif"))]
    EndIf,
    #[glsl_lang(display(extra = "#error"))]
    Error(PreprocessorError),
    #[glsl_lang(display(extra = "#if"))]
    If(PreprocessorIf),
    #[glsl_lang(display(extra = "#ifdef"))]
    IfDef(PreprocessorIfDef),
    #[glsl_lang(display(extra = "#ifndef"))]
    IfNDef(PreprocessorIfNDef),
    #[glsl_lang(display(extra = "#include"))]
    Include(PreprocessorInclude),
    #[glsl_lang(display(extra = "#line"))]
    Line(PreprocessorLine),
    #[glsl_lang(display(extra = "#pragma"))]
    Pragma(PreprocessorPragma),
    #[glsl_lang(display(extra = "#undef"))]
    Undef(PreprocessorUndef),
    #[glsl_lang(display(extra = "#version"))]
    Version(PreprocessorVersion),
    #[glsl_lang(display(extra = "#extension"))]
    Extension(PreprocessorExtension),
}

/// A #define preprocessor directive.
///
/// Allows any expression but only Integer and Float literals make sense
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum PreprocessorDefine {
    ObjectLike {
        ident: Identifier,
        value: String,
    },

    FunctionLike {
        ident: Identifier,
        args: Vec<Identifier>,
        value: String,
    },
}

/// An #else preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorElseIf {
    #[glsl_lang(display(extra))]
    pub condition: String,
}

/// An #error preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorError {
    #[glsl_lang(display(extra))]
    pub message: String,
}

/// An #if preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorIf {
    #[glsl_lang(display(extra))]
    pub condition: String,
}

/// An #ifdef preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorIfDef {
    #[glsl_lang(display(extra))]
    pub ident: Identifier,
}

/// A #ifndef preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorIfNDef {
    #[glsl_lang(display(extra))]
    pub ident: Identifier,
}

/// An #include name annotation.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorInclude {
    pub path: Path,
}

/// A #line preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorLine {
    #[glsl_lang(display(extra))]
    pub line: u32,
    pub source_string_number: Option<u32>,
}

/// A #pragma preprocessor directive.
/// Holds compiler-specific command.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorPragma {
    #[glsl_lang(display(extra))]
    pub command: String,
}

/// A #undef preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorUndef {
    #[glsl_lang(display(extra))]
    pub name: Identifier,
}

/// A #version preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorVersion {
    #[glsl_lang(display(extra))]
    pub version: u16,
    pub profile: Option<PreprocessorVersionProfile>,
}

/// A #version profile annotation.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum PreprocessorVersionProfile {
    #[glsl_lang(display(extra = "core"))]
    Core,
    #[glsl_lang(display(extra = "compatibility"))]
    Compatibility,
    #[glsl_lang(display(extra = "es"))]
    ES,
}

/// An #extension preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub struct PreprocessorExtension {
    pub name: PreprocessorExtensionName,
    pub behavior: Option<PreprocessorExtensionBehavior>,
}

/// An #extension name annotation.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum PreprocessorExtensionName {
    /// All extensions you could ever imagine in your whole lifetime (how crazy is that!).
    #[glsl_lang(display(extra = "all"))]
    All,
    /// A specific extension.
    Specific(String),
}

/// An #extension behavior annotation.
#[derive(Clone, Debug, PartialEq, NodeContent)]
pub enum PreprocessorExtensionBehavior {
    #[glsl_lang(display(extra = "require"))]
    Require,
    #[glsl_lang(display(extra = "enable"))]
    Enable,
    #[glsl_lang(display(extra = "warn"))]
    Warn,
    #[glsl_lang(display(extra = "disable"))]
    Disable,
}

/// A comment
#[derive(Debug, Clone, PartialEq, NodeContent)]
pub enum CommentData {
    /// Single-line comment
    Single(String),
    /// Multi-line comment
    Multi(String),
}

impl CommentData {
    pub fn text(&self) -> &str {
        match self {
            Self::Single(s) => s,
            Self::Multi(s) => s,
        }
    }

    pub fn is_single(&self) -> bool {
        matches!(self, Self::Multi(_))
    }

    pub fn is_multi(&self) -> bool {
        matches!(self, Self::Multi(_))
    }
}
