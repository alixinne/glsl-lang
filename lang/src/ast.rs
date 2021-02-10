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

use std::borrow::Cow;
use std::fmt;
use std::iter::FromIterator;

use glsl_lang_impl::NodeContents;

mod node;
pub use node::*;

/// A path literal.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum Path {
    /// Specified with angle brackets.
    Absolute(String),
    /// Specified with double quotes.
    Relative(String),
}

/// A generic identifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct IdentifierData(pub String);

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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct TypeNameData(pub String);

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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum TypeSpecifierNonArray {
    // transparent types
    Void,
    Bool,
    Int,
    UInt,
    Float,
    Double,
    Vec2,
    Vec3,
    Vec4,
    DVec2,
    DVec3,
    DVec4,
    BVec2,
    BVec3,
    BVec4,
    IVec2,
    IVec3,
    IVec4,
    UVec2,
    UVec3,
    UVec4,
    Mat2,
    Mat3,
    Mat4,
    Mat22,
    Mat23,
    Mat24,
    Mat32,
    Mat33,
    Mat34,
    Mat42,
    Mat43,
    Mat44,
    DMat2,
    DMat3,
    DMat4,
    DMat22,
    DMat23,
    DMat24,
    DMat32,
    DMat33,
    DMat34,
    DMat42,
    DMat43,
    DMat44,
    // floating point opaque types
    Sampler1D,
    Image1D,
    Sampler2D,
    Image2D,
    Sampler3D,
    Image3D,
    SamplerCube,
    ImageCube,
    Sampler2DRect,
    Image2DRect,
    Sampler1DArray,
    Image1DArray,
    Sampler2DArray,
    Image2DArray,
    SamplerBuffer,
    ImageBuffer,
    Sampler2DMS,
    Image2DMS,
    Sampler2DMSArray,
    Image2DMSArray,
    SamplerCubeArray,
    ImageCubeArray,
    Sampler1DShadow,
    Sampler2DShadow,
    Sampler2DRectShadow,
    Sampler1DArrayShadow,
    Sampler2DArrayShadow,
    SamplerCubeShadow,
    SamplerCubeArrayShadow,
    // signed integer opaque types
    ISampler1D,
    IImage1D,
    ISampler2D,
    IImage2D,
    ISampler3D,
    IImage3D,
    ISamplerCube,
    IImageCube,
    ISampler2DRect,
    IImage2DRect,
    ISampler1DArray,
    IImage1DArray,
    ISampler2DArray,
    IImage2DArray,
    ISamplerBuffer,
    IImageBuffer,
    ISampler2DMS,
    IImage2DMS,
    ISampler2DMSArray,
    IImage2DMSArray,
    ISamplerCubeArray,
    IImageCubeArray,
    // unsigned integer opaque types
    AtomicUInt,
    USampler1D,
    UImage1D,
    USampler2D,
    UImage2D,
    USampler3D,
    UImage3D,
    USamplerCube,
    UImageCube,
    USampler2DRect,
    UImage2DRect,
    USampler1DArray,
    UImage1DArray,
    USampler2DArray,
    UImage2DArray,
    USamplerBuffer,
    UImageBuffer,
    USampler2DMS,
    UImage2DMS,
    USampler2DMSArray,
    UImage2DMSArray,
    USamplerCubeArray,
    UImageCubeArray,
    Struct(StructSpecifier),
    TypeName(TypeName),
}

impl From<TypeName> for TypeSpecifierNonArray {
    fn from(tn: TypeName) -> Self {
        Self::TypeName(tn)
    }
}

/// Type specifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct StructSpecifier {
    pub name: Option<TypeName>,
    pub fields: Vec<StructFieldSpecifier>,
}

/// Struct field specifier. Used to add fields to struct specifiers.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct StructFieldSpecifier {
    pub qualifier: Option<TypeQualifier>,
    pub ty: TypeSpecifier,
    pub identifiers: Vec<ArrayedIdentifier>, // several identifiers of the same type
}

/// An identifier with an optional array specifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct TypeQualifier {
    pub qualifiers: Vec<TypeQualifierSpec>,
}

/// Type qualifier spec.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum TypeQualifierSpec {
    Storage(StorageQualifier),
    Layout(LayoutQualifier),
    Precision(PrecisionQualifier),
    Interpolation(InterpolationQualifier),
    Invariant,
    Precise,
}

/// Storage qualifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum StorageQualifier {
    Const,
    InOut,
    In,
    Out,
    Centroid,
    Patch,
    Sample,
    Uniform,
    Buffer,
    Shared,
    Coherent,
    Volatile,
    Restrict,
    ReadOnly,
    WriteOnly,
    // Note: the grammar says TYPE_NAME but type_specifier makes more sense given the definition of
    // subroutine. The reference implementation is marked "to do".
    Subroutine(Vec<TypeSpecifier>),
}

/// Layout qualifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct LayoutQualifier {
    pub ids: Vec<LayoutQualifierSpec>,
}

/// Layout qualifier spec.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum LayoutQualifierSpec {
    Identifier(Identifier, Option<Box<Expr>>),
    Shared,
}

/// Precision qualifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum PrecisionQualifier {
    High,
    Medium,
    Low,
}

/// Interpolation qualifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum InterpolationQualifier {
    Smooth,
    Flat,
    NoPerspective,
}

/// Fully specified type.
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct ArraySpecifier {
    /// List of all the dimensions – possibly unsized or explicitly-sized.
    pub dimensions: Vec<ArraySpecifierDimension>,
}

/// One array specifier dimension.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum ArraySpecifierDimension {
    Unsized,
    ExplicitlySized(Box<Expr>),
}

/// A declaration.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum DeclarationData {
    FunctionPrototype(FunctionPrototype),
    InitDeclaratorList(InitDeclaratorList),
    Precision(PrecisionQualifier, TypeSpecifier),
    Block(Block),
}

/// A general purpose block, containing fields and possibly a list of declared identifiers. Semantic
/// is given with the storage qualifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct Block {
    pub qualifier: TypeQualifier,
    pub name: Identifier,
    pub fields: Vec<StructFieldSpecifier>,
    pub identifier: Option<ArrayedIdentifier>,
}

/// Function identifier.
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
}

/// Function prototype.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct FunctionPrototypeData {
    pub ty: FullySpecifiedType,
    pub name: Identifier,
    pub parameters: Vec<FunctionParameterDeclaration>,
}

/// Function parameter declaration.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum FunctionParameterDeclarationData {
    Named(Option<TypeQualifier>, FunctionParameterDeclarator),
    Unnamed(Option<TypeQualifier>, TypeSpecifier),
}

/// Function parameter declarator.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct FunctionParameterDeclarator {
    pub ty: TypeSpecifier,
    pub ident: ArrayedIdentifier,
}

/// Init declarator list.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct InitDeclaratorList {
    pub head: SingleDeclaration,
    pub tail: Vec<SingleDeclarationNoType>,
}

/// Single declaration.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct SingleDeclaration {
    pub ty: FullySpecifiedType,
    pub name: Option<Identifier>,
    pub array_specifier: Option<ArraySpecifier>,
    pub initializer: Option<Initializer>,
}

/// A single declaration with implicit, already-defined type.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct SingleDeclarationNoType {
    pub ident: ArrayedIdentifier,
    pub initializer: Option<Initializer>,
}

/// Initializer.
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
        Self::Variable(IdentifierData::from(name.into()).into())
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum UnaryOp {
    Inc,
    Dec,
    Add,
    Minus,
    Not,
    Complement,
}

/// All binary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum BinaryOp {
    Or,
    Xor,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Equal,
    NonEqual,
    LT,
    GT,
    LTE,
    GTE,
    LShift,
    RShift,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
}

/// All possible operators for assigning expressions.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum AssignmentOp {
    Equal,
    Mult,
    Div,
    Mod,
    Add,
    Sub,
    LShift,
    RShift,
    And,
    Xor,
    Or,
}

/// Starting rule.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct TranslationUnit(pub Vec<ExternalDeclaration>);

/// External declaration.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum ExternalDeclarationData {
    Preprocessor(Preprocessor),
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}

/// Function definition.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct FunctionDefinitionData {
    pub prototype: FunctionPrototype,
    pub statement: CompoundStatement,
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct ExprStatement(pub Option<Expr>);

/// Selection statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct SelectionStatement {
    pub cond: Box<Expr>,
    pub rest: SelectionRestStatement,
}

/// Condition.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum Condition {
    Expr(Box<Expr>),
    Assignment(FullySpecifiedType, Identifier, Initializer),
}

/// Selection rest statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum SelectionRestStatement {
    /// Body of the if.
    Statement(Box<Statement>),
    /// The first argument is the body of the if, the rest is the next statement.
    Else(Box<Statement>, Box<Statement>),
}

/// Switch statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct SwitchStatement {
    pub head: Box<Expr>,
    pub body: Vec<Statement>,
}

/// Case label statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum CaseLabel {
    Case(Box<Expr>),
    Def,
}

/// Iteration statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum IterationStatement {
    While(Condition, Box<Statement>),
    DoWhile(Box<Statement>, Box<Expr>),
    For(ForInitStatement, ForRestStatement, Box<Statement>),
}

/// For init statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum ForInitStatement {
    Expression(Option<Expr>),
    Declaration(Box<Declaration>),
}

/// For init statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct ForRestStatement {
    pub condition: Option<Condition>,
    pub post_expr: Option<Box<Expr>>,
}

/// Jump statement.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum JumpStatement {
    Continue,
    Break,
    Return(Option<Box<Expr>>),
    Discard,
}

/// Some basic preprocessor directives.
///
/// As it’s important to carry them around the AST because they cannot be substituted in a normal
/// preprocessor (they’re used by GPU’s compilers), those preprocessor directives are available for
/// inspection.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum PreprocessorData {
    Define(PreprocessorDefine),
    Else,
    ElseIf(PreprocessorElseIf),
    EndIf,
    Error(PreprocessorError),
    If(PreprocessorIf),
    IfDef(PreprocessorIfDef),
    IfNDef(PreprocessorIfNDef),
    Include(PreprocessorInclude),
    Line(PreprocessorLine),
    Pragma(PreprocessorPragma),
    Undef(PreprocessorUndef),
    Version(PreprocessorVersion),
    Extension(PreprocessorExtension),
}

/// A #define preprocessor directive.
///
/// Allows any expression but only Integer and Float literals make sense
#[derive(Clone, Debug, PartialEq, NodeContents)]
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
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorElseIf {
    pub condition: String,
}

/// An #error preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorError {
    pub message: String,
}

/// An #if preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorIf {
    pub condition: String,
}

/// An #ifdef preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorIfDef {
    pub ident: Identifier,
}

/// A #ifndef preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorIfNDef {
    pub ident: Identifier,
}

/// An #include name annotation.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorInclude {
    pub path: Path,
}

/// A #line preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorLine {
    pub line: u32,
    pub source_string_number: Option<u32>,
}

/// A #pragma preprocessor directive.
/// Holds compiler-specific command.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorPragma {
    pub command: String,
}

/// A #undef preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorUndef {
    pub name: Identifier,
}

/// A #version preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorVersion {
    pub version: u16,
    pub profile: Option<PreprocessorVersionProfile>,
}

/// A #version profile annotation.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum PreprocessorVersionProfile {
    Core,
    Compatibility,
    ES,
}

/// An #extension preprocessor directive.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub struct PreprocessorExtension {
    pub name: PreprocessorExtensionName,
    pub behavior: Option<PreprocessorExtensionBehavior>,
}

/// An #extension name annotation.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum PreprocessorExtensionName {
    /// All extensions you could ever imagine in your whole lifetime (how crazy is that!).
    All,
    /// A specific extension.
    Specific(String),
}

/// An #extension behavior annotation.
#[derive(Clone, Debug, PartialEq, NodeContents)]
pub enum PreprocessorExtensionBehavior {
    Require,
    Enable,
    Warn,
    Disable,
}

/// A borrowed comment
#[derive(Debug, Clone, PartialEq, NodeContents)]
pub enum Comment<'s> {
    /// Single-line comment
    Single(Cow<'s, str>),
    /// Multi-line comment
    Multi(Cow<'s, str>),
}

impl Comment<'_> {
    pub fn text(&self) -> &str {
        match self {
            Self::Single(s) => s,
            Self::Multi(s) => s,
        }
    }

    pub fn to_owned(&self) -> Comment<'static> {
        match self {
            Self::Single(s) => Comment::Single(Cow::Owned(s.clone().into_owned())),
            Self::Multi(s) => Comment::Multi(Cow::Owned(s.clone().into_owned())),
        }
    }
}
