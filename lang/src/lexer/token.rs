use logos::Logos;
use strum_macros::{EnumDiscriminants, IntoStaticStr};

use super::{
    parse_cmt, parse_f32, parse_f64, parse_ident, parse_int, parse_uint, LexerContext, TypeNames,
};

#[derive(Debug, Clone, PartialEq, Logos, EnumDiscriminants)]
#[logos(extras = LexerContext)]
#[strum_discriminants(name(TokenKind), derive(IntoStaticStr))]
pub enum Token<'i> {
    #[token("const")]
    Const,
    #[token("bool")]
    Bool,
    #[token("float")]
    Float,
    #[token("int")]
    Int,
    #[token("uint")]
    UInt,
    #[token("double")]
    Double,
    #[token("bvec2")]
    BVec2,
    #[token("bvec3")]
    BVec3,
    #[token("bvec4")]
    BVec4,
    #[token("ivec2")]
    IVec2,
    #[token("ivec3")]
    IVec3,
    #[token("ivec4")]
    IVec4,
    #[token("uvec2")]
    UVec2,
    #[token("uvec3")]
    UVec3,
    #[token("uvec4")]
    UVec4,
    #[token("vec2")]
    Vec2,
    #[token("vec3")]
    Vec3,
    #[token("vec4")]
    Vec4,
    #[token("mat2")]
    Mat2,
    #[token("mat3")]
    Mat3,
    #[token("mat4")]
    Mat4,
    #[token("mat2x2")]
    Mat2x2,
    #[token("mat2x3")]
    Mat2x3,
    #[token("mat2x4")]
    Mat2x4,
    #[token("mat3x2")]
    Mat3x2,
    #[token("mat3x3")]
    Mat3x3,
    #[token("mat3x4")]
    Mat3x4,
    #[token("mat4x2")]
    Mat4x2,
    #[token("mat4x3")]
    Mat4x3,
    #[token("mat4x4")]
    Mat4x4,
    #[token("dvec2")]
    DVec2,
    #[token("dvec3")]
    DVec3,
    #[token("dvec4")]
    DVec4,
    #[token("dmat2")]
    DMat2,
    #[token("dmat3")]
    DMat3,
    #[token("dmat4")]
    DMat4,
    #[token("dmat2x2")]
    DMat2x2,
    #[token("dmat2x3")]
    DMat2x3,
    #[token("dmat2x4")]
    DMat2x4,
    #[token("dmat3x2")]
    DMat3x2,
    #[token("dmat3x3")]
    DMat3x3,
    #[token("dmat3x4")]
    DMat3x4,
    #[token("dmat4x2")]
    DMat4x2,
    #[token("dmat4x3")]
    DMat4x3,
    #[token("dmat4x4")]
    DMat4x4,
    #[token("centroid")]
    Centroid,
    #[token("in")]
    In,
    #[token("out")]
    Out,
    #[token("inout")]
    InOut,
    #[token("uniform")]
    Uniform,
    #[token("patch")]
    Patch,
    #[token("sample")]
    Sample,
    #[token("buffer")]
    Buffer,
    #[token("shared")]
    Shared,
    #[token("coherent")]
    Coherent,
    #[token("volatile")]
    Volatile,
    #[token("restrict")]
    Restrict,
    #[token("readonly")]
    ReadOnly,
    #[token("writeonly")]
    WriteOnly,
    #[token("noperspective")]
    NoPerspective,
    #[token("flat")]
    Flat,
    #[token("smooth")]
    Smooth,
    #[token("layout")]
    Layout,
    #[token("atomic_uint")]
    AtomicUInt,
    #[token("sampler1D")]
    Sampler1D,
    #[token("sampler1DShadow")]
    Sampler1DShadow,
    #[token("sampler1DArray")]
    Sampler1DArray,
    #[token("sampler1DArrayShadow")]
    Sampler1DArrayShadow,
    #[token("isampler1D")]
    ISampler1D,
    #[token("isampler1DArray")]
    ISampler1DArray,
    #[token("usampler1D")]
    USampler1D,
    #[token("usampler1DArray")]
    USampler1DArray,
    #[token("sampler2D")]
    Sampler2D,
    #[token("sampler2DShadow")]
    Sampler2DShadow,
    #[token("sampler2DArray")]
    Sampler2DArray,
    #[token("sampler2DArrayShadow")]
    Sampler2DArrayShadow,
    #[token("isampler2D")]
    ISampler2D,
    #[token("isampler2DArray")]
    ISampler2DArray,
    #[token("usampler2D")]
    USampler2D,
    #[token("usampler2DArray")]
    USampler2DArray,
    #[token("sampler2DRect")]
    Sampler2DRect,
    #[token("sampler2DRectShadow")]
    Sampler2DRectShadow,
    #[token("isampler2DRect")]
    ISampler2DRect,
    #[token("usampler2DRect")]
    USampler2DRect,
    #[token("sampler2DMS")]
    Sampler2DMS,
    #[token("isampler2DMS")]
    ISampler2DMS,
    #[token("usampler2DMS")]
    USampler2DMS,
    #[token("sampler2DMSArray")]
    Sampler2DMSArray,
    #[token("isampler2DMSArray")]
    ISampler2DMSArray,
    #[token("usampler2DMSArray")]
    USampler2DMSArray,
    #[token("sampler3D")]
    Sampler3D,
    #[token("isampler3D")]
    ISampler3D,
    #[token("usampler3D")]
    USampler3D,
    #[token("samplerCube")]
    SamplerCube,
    #[token("samplerCubeShadow")]
    SamplerCubeShadow,
    #[token("isamplerCube")]
    ISamplerCube,
    #[token("usamplerCube")]
    USamplerCube,
    #[token("samplerCubeArray")]
    SamplerCubeArray,
    #[token("samplerCubeArrayShadow")]
    SamplerCubeArrayShadow,
    #[token("isamplerCubeArray")]
    ISamplerCubeArray,
    #[token("usamplerCubeArray")]
    USamplerCubeArray,
    #[token("samplerBuffer")]
    SamplerBuffer,
    #[token("isamplerBuffer")]
    ISamplerBuffer,
    #[token("usamplerBuffer")]
    USamplerBuffer,
    #[token("image1D")]
    Image1D,
    #[token("iimage1D")]
    IImage1D,
    #[token("uimage1D")]
    UImage1D,
    #[token("image1DArray")]
    Image1DArray,
    #[token("iimage1DArray")]
    IImage1DArray,
    #[token("uimage1DArray")]
    UImage1DArray,
    #[token("image2D")]
    Image2D,
    #[token("iimage2D")]
    IImage2D,
    #[token("uimage2D")]
    UImage2D,
    #[token("image2DArray")]
    Image2DArray,
    #[token("iimage2DArray")]
    IImage2DArray,
    #[token("uimage2DArray")]
    UImage2DArray,
    #[token("image2DRect")]
    Image2DRect,
    #[token("iimage2DRect")]
    IImage2DRect,
    #[token("uimage2DRect")]
    UImage2DRect,
    #[token("image2DMS")]
    Image2DMS,
    #[token("iimage2DMS")]
    IImage2DMS,
    #[token("uimage2DMS")]
    UImage2DMS,
    #[token("image2DMSArray")]
    Image2DMSArray,
    #[token("iimage2DMSArray")]
    IImage2DMSArray,
    #[token("uimage2DMSArray")]
    UImage2DMSArray,
    #[token("image3D")]
    Image3D,
    #[token("iimage3D")]
    IImage3D,
    #[token("uimage3D")]
    UImage3D,
    #[token("imageCube")]
    ImageCube,
    #[token("iimageCube")]
    IImageCube,
    #[token("uimageCube")]
    UImageCube,
    #[token("imageCubeArray")]
    ImageCubeArray,
    #[token("iimageCubeArray")]
    IImageCubeArray,
    #[token("uimageCubeArray")]
    UImageCubeArray,
    #[token("imageBuffer")]
    ImageBuffer,
    #[token("iimageBuffer")]
    IImageBuffer,
    #[token("uimageBuffer")]
    UImageBuffer,

    // Begin Vulkan-target keywords
    #[token("texture1D")]
    Texture1D,
    #[token("texture1DArray")]
    Texture1DArray,
    #[token("itexture1D")]
    ITexture1D,
    #[token("itexture1DArray")]
    ITexture1DArray,
    #[token("utexture1D")]
    UTexture1D,
    #[token("utexture1DArray")]
    UTexture1DArray,
    #[token("texture2D")]
    Texture2D,
    #[token("texture2DArray")]
    Texture2DArray,
    #[token("itexture2D")]
    ITexture2D,
    #[token("itexture2DArray")]
    ITexture2DArray,
    #[token("utexture2D")]
    UTexture2D,
    #[token("utexture2DArray")]
    UTexture2DArray,
    #[token("texture2DRect")]
    Texture2DRect,
    #[token("itexture2DRect")]
    ITexture2DRect,
    #[token("utexture2DRect")]
    UTexture2DRect,
    #[token("texture2DMS")]
    Texture2DMS,
    #[token("itexture2DMS")]
    ITexture2DMS,
    #[token("utexture2DMS")]
    UTexture2DMS,
    #[token("texture2DMSArray")]
    Texture2DMSArray,
    #[token("itexture2DMSArray")]
    ITexture2DMSArray,
    #[token("utexture2DMSArray")]
    UTexture2DMSArray,
    #[token("texture3D")]
    Texture3D,
    #[token("itexture3D")]
    ITexture3D,
    #[token("utexture3D")]
    UTexture3D,
    #[token("textureCube")]
    TextureCube,
    #[token("itextureCube")]
    ITextureCube,
    #[token("utextureCube")]
    UTextureCube,
    #[token("textureCubeArray")]
    TextureCubeArray,
    #[token("itextureCubeArray")]
    ITextureCubeArray,
    #[token("utextureCubeArray")]
    UTextureCubeArray,
    #[token("textureBuffer")]
    TextureBuffer,
    #[token("itextureBuffer")]
    ITextureBuffer,
    #[token("utextureBuffer")]
    UTextureBuffer,
    #[token("sampler")]
    Sampler,
    #[token("samplerShadow")]
    SamplerShadow,
    #[token("subpassInput")]
    SubpassInput,
    #[token("isubpassInput")]
    ISubpassInput,
    #[token("usubpassInput")]
    USubpassInput,
    #[token("subpassInputMS")]
    SubpassInputMS,
    #[token("isubpassInputMS")]
    ISubpassInputMS,
    #[token("usubpassInputMS")]
    USubpassInputMS,
    // End Vulkan-target keywords
    #[token("struct")]
    Struct,
    #[token("void")]
    Void,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("do")]
    Do,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("if")]
    If,
    #[token("discard")]
    Discard,
    #[token("return")]
    Return,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("default")]
    Default,
    #[token("subroutine")]
    Subroutine,
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*", parse_ident)]
    Identifier((&'i str, TypeNames)),
    TypeName(&'i str), // Cast from Identifier depending on known type names
    #[regex(
        r"([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)([eE][+-]?[0-9]+)?(f|F)?",
        parse_f32
    )]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+(f|F)?", parse_f32)]
    FloatConstant(f32),
    #[regex(r"0[0-7]*", |lex| parse_int(lex, 8))]
    #[regex(r"[1-9][0-9]*", |lex| parse_int(lex, 10))]
    #[regex(r"0[xX][0-9A-Fa-f]+", |lex| parse_int(lex, 16))]
    IntConstant(i32),
    #[regex(r"0[0-7]*[uU]", |lex| parse_uint(lex, 8))]
    #[regex(r"[1-9][0-9]*[uU]", |lex| parse_uint(lex, 10))]
    #[regex(r"0[xX][0-9A-Fa-f]+[uU]", |lex| parse_uint(lex, 16))]
    UIntConstant(u32),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    BoolConstant(bool),
    #[regex(
        r"([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)([eE][+-]?[0-9]+)?(lf|LF)",
        parse_f64
    )]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+(lf|LF)", parse_f64)]
    DoubleConstant(f64),
    #[token("<<")]
    LeftOp,
    #[token(">>")]
    RightOp,
    #[token("++")]
    IncOp,
    #[token("--")]
    DecOp,
    #[token("<=")]
    LeOp,
    #[token(">=")]
    GeOp,
    #[token("==")]
    EqOp,
    #[token("!=")]
    NeOp,
    #[token("&&")]
    AndOp,
    #[token("||")]
    OrOp,
    #[token("^^")]
    XorOp,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("+=")]
    AddAssign,
    #[token("%=")]
    ModAssign,
    #[token("<<=")]
    LeftAssign,
    #[token(">>=")]
    RightAssign,
    #[token("&=")]
    AndAssign,
    #[token("^=")]
    XorAssign,
    #[token("|=")]
    OrAssign,
    #[token("-=")]
    SubAssign,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token(";")]
    Semicolon,
    #[token("!")]
    Bang,
    #[token("-")]
    Dash,
    #[token("~")]
    Tilde,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    LeftAngle,
    #[token(">")]
    RightAngle,
    #[token("|")]
    VerticalBar,
    #[token("^")]
    Caret,
    #[token("&")]
    Ampersand,
    #[token("?")]
    Question,
    #[token("invariant")]
    Invariant,
    #[token("precise")]
    Precise,
    #[token("highp")]
    HighPrecision,
    #[token("mediump")]
    MediumPrecision,
    #[token("lowp")]
    LowPrecision,
    #[token("precision")]
    Precision,

    // TODO: Line continuation can happen inside tokens
    #[regex("([ \t\r\n]|\\\\\r?\n)+", logos::skip)]
    Whitespace,
    #[regex("//(.|\\\\\r?\n)*", |lex| { parse_cmt(lex, true); logos::Skip })]
    SingleLineComment,
    #[regex("/\\*([^*]|\\*[^/])+\\*/", |lex| { parse_cmt(lex, false); logos::Skip })]
    MultiLineComment,

    // TODO: Line continuations in preprocessor pragmas?
    #[regex("#([ \t]|\\\\\r?\n)*define")]
    PpDefine,
    #[regex("#([ \t]|\\\\\r?\n)*else")]
    PpElse,
    #[regex("#([ \t]|\\\\\r?\n)*elif")]
    PpElif,
    #[regex("#([ \t]|\\\\\r?\n)*endif")]
    PpEndIf,
    #[regex("#([ \t]|\\\\\r?\n)*error")]
    PpError,
    #[regex("#([ \t]|\\\\\r?\n)*if")]
    PpIf,
    #[regex("#([ \t]|\\\\\r?\n)*ifdef")]
    PpIfDef,
    #[regex("#([ \t]|\\\\\r?\n)*ifndef")]
    PpIfNDef,
    #[regex("#([ \t]|\\\\\r?\n)*include")]
    PpInclude,
    #[regex("#([ \t]|\\\\\r?\n)*line")]
    PpLine,
    #[regex("#([ \t]|\\\\\r?\n)*pragma")]
    PpPragma,
    #[regex("#([ \t]|\\\\\r?\n)*undef")]
    PpUndef,
    #[regex("#([ \t]|\\\\\r?\n)*version")]
    PpVersion,
    #[regex("#([ \t]|\\\\\r?\n)*extension")]
    PpExtension,

    PpRest(std::borrow::Cow<'i, str>),

    PpCore,
    PpCompatibility,
    PpEs,

    PpExtRequire,
    PpExtEnable,
    PpExtWarn,
    PpExtDisable,

    PpPathAbsolute(&'i str),
    PpPathRelative(&'i str),

    #[error]
    Error,
}

impl<'i> Token<'i> {
    pub fn is_pp(&self) -> bool {
        match self {
            Self::PpDefine => true,
            Self::PpElse => true,
            Self::PpElif => true,
            Self::PpEndIf => true,
            Self::PpError => true,
            Self::PpIf => true,
            Self::PpIfDef => true,
            Self::PpIfNDef => true,
            Self::PpInclude => true,
            Self::PpLine => true,
            Self::PpPragma => true,
            Self::PpUndef => true,
            Self::PpVersion => true,
            Self::PpExtension => true,
            _ => false,
        }
    }

    pub fn as_str(&self) -> &'i str {
        match self {
            Self::Identifier((s, _)) => s,
            Self::TypeName(s) => s,
            Self::PpPathRelative(s) => s,
            Self::PpPathAbsolute(s) => s,
            _ => panic!("cannot convert token {:?}, to str", self),
        }
    }

    pub fn type_names(&self) -> TypeNames {
        match self {
            Self::Identifier((_, n)) => n.clone(),
            _ => panic!("cannot get type_names for token {:?}", self),
        }
    }
}

macro_rules! impl_into {
    ($t:ty => $i:ident) => {
        impl<'i> Into<$t> for Token<'i> {
            fn into(self) -> $t {
                match self {
                    Self::$i(i) => i,
                    other => panic!(concat!("cannot convert {:?} into ", stringify!($i)), other),
                }
            }
        }
    };
}

impl_into!(i32 => IntConstant);
impl_into!(u32 => UIntConstant);
impl_into!(f32 => FloatConstant);
impl_into!(f64 => DoubleConstant);
impl_into!(bool => BoolConstant);

impl<'i> Into<String> for Token<'i> {
    fn into(self) -> String {
        match self {
            Self::PpRest(s) => s.to_string(),
            other => panic!("cannot convert {:?} into String", other),
        }
    }
}
