use logos::Logos;

use super::{
    parse_cmt, parse_f32, parse_f64, parse_ident, parse_int, parse_rs_ident, parse_uint,
    LexerContext,
};

#[derive(Debug, Clone, PartialEq, Logos, lang_util::Token)]
#[logos(extras = LexerContext)]
#[allow(missing_docs)]
pub enum Token<'i> {
    #[token("const")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Const,
    #[token("bool")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Bool,
    #[token("float")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Float,
    #[token("int")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Int,
    #[token("uint")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UInt,
    #[token("double")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Double,
    #[token("bvec2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    BVec2,
    #[token("bvec3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    BVec3,
    #[token("bvec4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    BVec4,
    #[token("ivec2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IVec2,
    #[token("ivec3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IVec3,
    #[token("ivec4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IVec4,
    #[token("uvec2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UVec2,
    #[token("uvec3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UVec3,
    #[token("uvec4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UVec4,
    #[token("vec2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Vec2,
    #[token("vec3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Vec3,
    #[token("vec4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Vec4,
    #[token("mat2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat2,
    #[token("mat3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat3,
    #[token("mat4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat4,
    #[token("mat2x2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat2x2,
    #[token("mat2x3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat2x3,
    #[token("mat2x4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat2x4,
    #[token("mat3x2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat3x2,
    #[token("mat3x3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat3x3,
    #[token("mat3x4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat3x4,
    #[token("mat4x2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat4x2,
    #[token("mat4x3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat4x3,
    #[token("mat4x4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Mat4x4,
    #[token("dvec2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DVec2,
    #[token("dvec3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DVec3,
    #[token("dvec4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DVec4,
    #[token("dmat2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat2,
    #[token("dmat3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat3,
    #[token("dmat4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat4,
    #[token("dmat2x2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat2x2,
    #[token("dmat2x3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat2x3,
    #[token("dmat2x4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat2x4,
    #[token("dmat3x2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat3x2,
    #[token("dmat3x3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat3x3,
    #[token("dmat3x4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat3x4,
    #[token("dmat4x2")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat4x2,
    #[token("dmat4x3")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat4x3,
    #[token("dmat4x4")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    DMat4x4,
    #[token("centroid")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Centroid,
    #[token("in")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    In,
    #[token("out")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Out,
    #[token("inout")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    InOut,
    #[token("uniform")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Uniform,
    #[token("patch")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Patch,
    #[token("sample")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Sample,
    #[token("buffer")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Buffer,
    #[token("shared")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Shared,
    #[token("coherent")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Coherent,
    #[token("volatile")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Volatile,
    #[token("restrict")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Restrict,
    #[token("readonly")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    ReadOnly,
    #[token("writeonly")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    WriteOnly,
    #[token("noperspective")]
    #[lang_util(kind = "interpolation qualifier", kind = "type qualifier")]
    NoPerspective,
    #[token("flat")]
    #[lang_util(kind = "interpolation qualifier", kind = "type qualifier")]
    Flat,
    #[token("smooth")]
    #[lang_util(kind = "interpolation qualifier", kind = "type qualifier")]
    Smooth,
    #[token("layout")]
    #[lang_util(kind = "layout qualifier", kind = "type qualifier")]
    Layout,
    #[token("atomic_uint")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    AtomicUInt,
    #[token("sampler1D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler1D,
    #[token("sampler1DShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler1DShadow,
    #[token("sampler1DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler1DArray,
    #[token("sampler1DArrayShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler1DArrayShadow,
    #[token("isampler1D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler1D,
    #[token("isampler1DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler1DArray,
    #[token("usampler1D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler1D,
    #[token("usampler1DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler1DArray,
    #[token("sampler2D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2D,
    #[token("sampler2DShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DShadow,
    #[token("sampler2DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DArray,
    #[token("sampler2DArrayShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DArrayShadow,
    #[token("isampler2D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler2D,
    #[token("isampler2DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler2DArray,
    #[token("usampler2D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler2D,
    #[token("usampler2DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler2DArray,
    #[token("sampler2DRect")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DRect,
    #[token("sampler2DRectShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DRectShadow,
    #[token("isampler2DRect")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler2DRect,
    #[token("usampler2DRect")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler2DRect,
    #[token("sampler2DMS")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DMs,
    #[token("isampler2DMS")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler2DMs,
    #[token("usampler2DMS")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler2DMs,
    #[token("sampler2DMSArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler2DMsArray,
    #[token("isampler2DMSArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler2DMsArray,
    #[token("usampler2DMSArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler2DMsArray,
    #[token("sampler3D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Sampler3D,
    #[token("isampler3D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISampler3D,
    #[token("usampler3D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USampler3D,
    #[token("samplerCube")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    SamplerCube,
    #[token("samplerCubeShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    SamplerCubeShadow,
    #[token("isamplerCube")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISamplerCube,
    #[token("usamplerCube")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USamplerCube,
    #[token("samplerCubeArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    SamplerCubeArray,
    #[token("samplerCubeArrayShadow")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    SamplerCubeArrayShadow,
    #[token("isamplerCubeArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISamplerCubeArray,
    #[token("usamplerCubeArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USamplerCubeArray,
    #[token("samplerBuffer")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    SamplerBuffer,
    #[token("isamplerBuffer")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ISamplerBuffer,
    #[token("usamplerBuffer")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    USamplerBuffer,
    #[token("image1D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image1D,
    #[token("iimage1D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage1D,
    #[token("uimage1D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage1D,
    #[token("image1DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image1DArray,
    #[token("iimage1DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage1DArray,
    #[token("uimage1DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage1DArray,
    #[token("image2D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image2D,
    #[token("iimage2D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage2D,
    #[token("uimage2D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage2D,
    #[token("image2DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image2DArray,
    #[token("iimage2DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage2DArray,
    #[token("uimage2DArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage2DArray,
    #[token("image2DRect")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image2DRect,
    #[token("iimage2DRect")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage2DRect,
    #[token("uimage2DRect")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage2DRect,
    #[token("image2DMS")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image2DMs,
    #[token("iimage2DMS")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage2DMs,
    #[token("uimage2DMS")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage2DMs,
    #[token("image2DMSArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image2DMsArray,
    #[token("iimage2DMSArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage2DMsArray,
    #[token("uimage2DMSArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage2DMsArray,
    #[token("image3D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    Image3D,
    #[token("iimage3D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImage3D,
    #[token("uimage3D")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImage3D,
    #[token("imageCube")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ImageCube,
    #[token("iimageCube")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImageCube,
    #[token("uimageCube")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImageCube,
    #[token("imageCubeArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ImageCubeArray,
    #[token("iimageCubeArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImageCubeArray,
    #[token("uimageCubeArray")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImageCubeArray,
    #[token("imageBuffer")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    ImageBuffer,
    #[token("iimageBuffer")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    IImageBuffer,
    #[token("uimageBuffer")]
    #[lang_util(kind = "type name", kind = "vulkan type name")]
    UImageBuffer,

    // Begin Vulkan-target keywords
    #[token("texture1D")]
    #[lang_util(kind = "vulkan type name")]
    Texture1D,
    #[token("texture1DArray")]
    #[lang_util(kind = "vulkan type name")]
    Texture1DArray,
    #[token("itexture1D")]
    #[lang_util(kind = "vulkan type name")]
    ITexture1D,
    #[token("itexture1DArray")]
    #[lang_util(kind = "vulkan type name")]
    ITexture1DArray,
    #[token("utexture1D")]
    #[lang_util(kind = "vulkan type name")]
    UTexture1D,
    #[token("utexture1DArray")]
    #[lang_util(kind = "vulkan type name")]
    UTexture1DArray,
    #[token("texture2D")]
    #[lang_util(kind = "vulkan type name")]
    Texture2D,
    #[token("texture2DArray")]
    #[lang_util(kind = "vulkan type name")]
    Texture2DArray,
    #[token("itexture2D")]
    #[lang_util(kind = "vulkan type name")]
    ITexture2D,
    #[token("itexture2DArray")]
    #[lang_util(kind = "vulkan type name")]
    ITexture2DArray,
    #[token("utexture2D")]
    #[lang_util(kind = "vulkan type name")]
    UTexture2D,
    #[token("utexture2DArray")]
    #[lang_util(kind = "vulkan type name")]
    UTexture2DArray,
    #[token("texture2DRect")]
    #[lang_util(kind = "vulkan type name")]
    Texture2DRect,
    #[token("itexture2DRect")]
    #[lang_util(kind = "vulkan type name")]
    ITexture2DRect,
    #[token("utexture2DRect")]
    #[lang_util(kind = "vulkan type name")]
    UTexture2DRect,
    #[token("texture2DMS")]
    #[lang_util(kind = "vulkan type name")]
    Texture2DMs,
    #[token("itexture2DMS")]
    #[lang_util(kind = "vulkan type name")]
    ITexture2DMs,
    #[token("utexture2DMS")]
    #[lang_util(kind = "vulkan type name")]
    UTexture2DMs,
    #[token("texture2DMSArray")]
    #[lang_util(kind = "vulkan type name")]
    Texture2DMsArray,
    #[token("itexture2DMSArray")]
    #[lang_util(kind = "vulkan type name")]
    ITexture2DMsArray,
    #[token("utexture2DMSArray")]
    #[lang_util(kind = "vulkan type name")]
    UTexture2DMsArray,
    #[token("texture3D")]
    #[lang_util(kind = "vulkan type name")]
    Texture3D,
    #[token("itexture3D")]
    #[lang_util(kind = "vulkan type name")]
    ITexture3D,
    #[token("utexture3D")]
    #[lang_util(kind = "vulkan type name")]
    UTexture3D,
    #[token("textureCube")]
    #[lang_util(kind = "vulkan type name")]
    TextureCube,
    #[token("itextureCube")]
    #[lang_util(kind = "vulkan type name")]
    ITextureCube,
    #[token("utextureCube")]
    #[lang_util(kind = "vulkan type name")]
    UTextureCube,
    #[token("textureCubeArray")]
    #[lang_util(kind = "vulkan type name")]
    TextureCubeArray,
    #[token("itextureCubeArray")]
    #[lang_util(kind = "vulkan type name")]
    ITextureCubeArray,
    #[token("utextureCubeArray")]
    #[lang_util(kind = "vulkan type name")]
    UTextureCubeArray,
    #[token("textureBuffer")]
    #[lang_util(kind = "vulkan type name")]
    TextureBuffer,
    #[token("itextureBuffer")]
    #[lang_util(kind = "vulkan type name")]
    ITextureBuffer,
    #[token("utextureBuffer")]
    #[lang_util(kind = "vulkan type name")]
    UTextureBuffer,
    #[token("sampler")]
    #[lang_util(kind = "vulkan type name")]
    Sampler,
    #[token("samplerShadow")]
    #[lang_util(kind = "vulkan type name")]
    SamplerShadow,
    #[token("subpassInput")]
    #[lang_util(kind = "vulkan type name")]
    SubpassInput,
    #[token("isubpassInput")]
    #[lang_util(kind = "vulkan type name")]
    ISubpassInput,
    #[token("usubpassInput")]
    #[lang_util(kind = "vulkan type name")]
    USubpassInput,
    #[token("subpassInputMS")]
    #[lang_util(kind = "vulkan type name")]
    SubpassInputMs,
    #[token("isubpassInputMS")]
    #[lang_util(kind = "vulkan type name")]
    ISubpassInputMs,
    #[token("usubpassInputMS")]
    #[lang_util(kind = "vulkan type name")]
    USubpassInputMs,
    // End Vulkan-target keywords
    #[token("struct")]
    #[lang_util(kind = "struct", kind = "keyword")]
    Struct,
    #[token("void")]
    #[lang_util(kind = "type name")]
    Void,
    #[token("while")]
    #[lang_util(kind = "keyword")]
    While,
    #[token("break")]
    #[lang_util(kind = "keyword")]
    Break,
    #[token("continue")]
    #[lang_util(kind = "keyword")]
    Continue,
    #[token("do")]
    #[lang_util(kind = "keyword")]
    Do,
    #[token("else")]
    #[lang_util(kind = "keyword")]
    Else,
    #[token("for")]
    #[lang_util(kind = "keyword")]
    For,
    #[token("if")]
    #[lang_util(kind = "keyword")]
    If,
    #[token("discard")]
    #[lang_util(kind = "keyword")]
    Discard,
    #[token("return")]
    #[lang_util(kind = "keyword")]
    Return,
    #[token("switch")]
    #[lang_util(kind = "keyword")]
    Switch,
    #[token("case")]
    #[lang_util(kind = "keyword")]
    Case,
    #[token("default")]
    #[lang_util(kind = "keyword")]
    Default,
    #[token("subroutine")]
    #[lang_util(kind = "storage qualifier", kind = "type qualifier")]
    Subroutine,
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*", parse_ident)]
    #[regex("#\\s*\\(\\s*[a-zA-Z_][a-zA-Z_0-9]*\\s*\\)", parse_rs_ident)]
    #[lang_util(display("{}", "_0.0"), as = "ident", kind = "identifier")]
    Identifier((&'i str, LexerContext)),
    #[lang_util(as = "ty_name", kind = "type name")]
    TypeName(&'i str), // Cast from Identifier depending on known type names
    #[regex(
        r"([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)([eE][+-]?[0-9]+)?(f|F)?",
        parse_f32
    )]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+(f|F)?", parse_f32)]
    #[lang_util(as = "float_constant", kind = "literal")]
    FloatConstant(f32),
    #[regex(r"0[0-7]*", |lex| parse_int(lex, 8))]
    #[regex(r"[1-9][0-9]*", |lex| parse_int(lex, 10))]
    #[regex(r"0[xX][0-9A-Fa-f]+", |lex| parse_int(lex, 16))]
    #[lang_util(as = "int_constant", kind = "literal")]
    IntConstant(i32),
    #[regex(r"0[0-7]*[uU]", |lex| parse_uint(lex, 8))]
    #[regex(r"[1-9][0-9]*[uU]", |lex| parse_uint(lex, 10))]
    #[regex(r"0[xX][0-9A-Fa-f]+[uU]", |lex| parse_uint(lex, 16))]
    #[lang_util(as = "uint_constant", kind = "literal")]
    UIntConstant(u32),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    #[lang_util(as = "bool_constant", kind = "literal")]
    BoolConstant(bool),
    #[regex(
        r"([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)([eE][+-]?[0-9]+)?(lf|LF)",
        parse_f64
    )]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+(lf|LF)", parse_f64)]
    #[lang_util(as = "double_constant", kind = "literal")]
    DoubleConstant(f64),
    #[token("<<")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    LeftOp,
    #[token(">>")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    RightOp,
    #[token("++")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    IncOp,
    #[token("--")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    DecOp,
    #[token("<=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    LeOp,
    #[token(">=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    GeOp,
    #[token("==")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    EqOp,
    #[token("!=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    NeOp,
    #[token("&&")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    AndOp,
    #[token("||")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    OrOp,
    #[token("^^")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    XorOp,
    #[token("*=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    MulAssign,
    #[token("/=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    DivAssign,
    #[token("+=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    AddAssign,
    #[token("%=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    ModAssign,
    #[token("<<=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    LeftAssign,
    #[token(">>=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    RightAssign,
    #[token("&=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    AndAssign,
    #[token("^=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    XorAssign,
    #[token("|=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    OrAssign,
    #[token("-=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
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
    #[lang_util(kind = "binary operator", kind = "operator")]
    Dot,
    #[token(",")]
    #[lang_util(kind = "operator")]
    Comma,
    #[token(":")]
    #[lang_util(kind = "operator")]
    Colon,
    #[token("=")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    Equal,
    #[token(";")]
    Semicolon,
    #[token("!")]
    #[lang_util(kind = "unary operator", kind = "operator")]
    Bang,
    #[token("-")]
    #[lang_util(kind = "operator")]
    Dash,
    #[token("~")]
    #[lang_util(kind = "unary operator", kind = "operator")]
    Tilde,
    #[token("+")]
    #[lang_util(kind = "operator")]
    Plus,
    #[token("*")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    Star,
    #[token("/")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    Slash,
    #[token("%")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    Percent,
    #[token("<")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    LeftAngle,
    #[token(">")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    RightAngle,
    #[token("|")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    VerticalBar,
    #[token("^")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    Caret,
    #[token("&")]
    #[lang_util(kind = "binary operator", kind = "operator")]
    Ampersand,
    #[token("?")]
    #[lang_util(kind = "operator")]
    Question,
    #[token("invariant")]
    #[lang_util(kind = "type qualifier")]
    Invariant,
    #[token("precise")]
    #[lang_util(kind = "type qualifier")]
    Precise,
    #[token("highp")]
    #[lang_util(kind = "precision qualifier", kind = "type qualifier")]
    HighPrecision,
    #[token("mediump")]
    #[lang_util(kind = "precision qualifier", kind = "type qualifier")]
    MediumPrecision,
    #[token("lowp")]
    #[lang_util(kind = "precision qualifier", kind = "type qualifier")]
    LowPrecision,
    #[token("precision")]
    Precision,

    // TODO: Line continuation can happen inside tokens
    #[regex("([ \t\r\n]|\\\\\r?\n)+", logos::skip)]
    #[lang_util(display = "<whitespace>", as(display), kind = "trivia")]
    Whitespace,
    #[regex("//(.|\\\\\r?\n)*", |lex| { parse_cmt(lex, true); logos::Skip })]
    #[lang_util(display = "<single line comment>", as(display), kind = "trivia")]
    SingleLineComment,
    #[regex("/\\*([^*]|\\*[^/])+\\*/", |lex| { parse_cmt(lex, false); logos::Skip })]
    #[lang_util(display = "<multi line comment>", as(display), kind = "trivia")]
    MultiLineComment,

    // TODO: Line continuations in preprocessor pragmas?
    #[regex("#([ \t]|\\\\\r?\n)*define")]
    #[lang_util(display = "#define", as(display), kind = "preprocessor directive")]
    PpDefine,
    #[regex("#([ \t]|\\\\\r?\n)*else")]
    #[lang_util(display = "#else", as(display), kind = "preprocessor directive")]
    PpElse,
    #[regex("#([ \t]|\\\\\r?\n)*elif")]
    #[lang_util(display = "#elif", as(display), kind = "preprocessor directive")]
    PpElif,
    #[regex("#([ \t]|\\\\\r?\n)*endif")]
    #[lang_util(display = "#endif", as(display), kind = "preprocessor directive")]
    PpEndIf,
    #[regex("#([ \t]|\\\\\r?\n)*error")]
    #[lang_util(display = "#error", as(display), kind = "preprocessor directive")]
    PpError,
    #[regex("#([ \t]|\\\\\r?\n)*if")]
    #[lang_util(display = "#if", as(display), kind = "preprocessor directive")]
    PpIf,
    #[regex("#([ \t]|\\\\\r?\n)*ifdef")]
    #[lang_util(display = "#ifdef", as(display), kind = "preprocessor directive")]
    PpIfDef,
    #[regex("#([ \t]|\\\\\r?\n)*ifndef")]
    #[lang_util(display = "#ifndef", as(display), kind = "preprocessor directive")]
    PpIfNDef,
    #[regex("#([ \t]|\\\\\r?\n)*include")]
    #[lang_util(display = "#include", as(display), kind = "preprocessor directive")]
    PpInclude,
    #[regex("#([ \t]|\\\\\r?\n)*line")]
    #[lang_util(display = "#line", as(display), kind = "preprocessor directive")]
    PpLine,
    #[regex("#([ \t]|\\\\\r?\n)*pragma")]
    #[lang_util(display = "#pragma", as(display), kind = "preprocessor directive")]
    PpPragma,
    #[regex("#([ \t]|\\\\\r?\n)*undef")]
    #[lang_util(display = "#undef", as(display), kind = "preprocessor directive")]
    PpUndef,
    #[regex("#([ \t]|\\\\\r?\n)*version")]
    #[lang_util(display = "#version", as(display), kind = "preprocessor directive")]
    PpVersion,
    #[regex("#([ \t]|\\\\\r?\n)*extension")]
    #[lang_util(display = "#extension", as(display), kind = "preprocessor directive")]
    PpExtension,

    #[lang_util(
        display = "<preprocessor string>",
        as(display),
        kind = "preprocessor string"
    )]
    PpRest(std::borrow::Cow<'i, str>),

    #[lang_util(display = "core", as(display), kind = "version profile")]
    PpCore,
    #[lang_util(display = "compatibility", as(display), kind = "version profile")]
    PpCompatibility,
    #[lang_util(display = "es", as(display), kind = "version profile")]
    PpEs,

    #[lang_util(display = "require", as(display), kind = "extension behavior")]
    PpExtRequire,
    #[lang_util(display = "enable", as(display), kind = "extension behavior")]
    PpExtEnable,
    #[lang_util(display = "warn", as(display), kind = "extension behavior")]
    PpExtWarn,
    #[lang_util(display = "disable", as(display), kind = "extension behavior")]
    PpExtDisable,

    #[lang_util(display = "<{}>", as(display), kind = "include path")]
    PpPathAbsolute(&'i str),
    #[lang_util(display = "\"{}\"", as(display), kind = "include path")]
    PpPathRelative(&'i str),

    #[error]
    #[lang_util(display = "<invalid token>", as(display), kind = "error")]
    Error,
}

impl<'i> Token<'i> {
    /// Return `true` if this token is a preprocessor token
    pub fn is_pp(&self) -> bool {
        matches!(
            self,
            Self::PpDefine
                | Self::PpElse
                | Self::PpElif
                | Self::PpEndIf
                | Self::PpError
                | Self::PpIf
                | Self::PpIfDef
                | Self::PpIfNDef
                | Self::PpInclude
                | Self::PpLine
                | Self::PpPragma
                | Self::PpUndef
                | Self::PpVersion
                | Self::PpExtension
        )
    }

    /// Return this token's inner text as a string slice
    pub fn as_str(&self) -> &'i str {
        match self {
            Self::Identifier((s, _)) => s,
            Self::TypeName(s) => s,
            Self::PpPathRelative(s) => s,
            Self::PpPathAbsolute(s) => s,
            _ => panic!("cannot convert token {:?}, to str", self),
        }
    }

    /// Return this token's lexer context
    pub fn context(&self) -> LexerContext {
        match self {
            Self::Identifier((_, c)) => c.clone(),
            _ => panic!("cannot get type_names for token {:?}", self),
        }
    }
}

macro_rules! impl_from {
    ($t:ty => $i:ident) => {
        impl<'i> From<Token<'i>> for $t {
            fn from(value: Token<'i>) -> Self {
                match value {
                    Token::$i(i) => i,
                    other => panic!(concat!("cannot convert {:?} into ", stringify!($i)), other),
                }
            }
        }
    };
}

impl_from!(i32 => IntConstant);
impl_from!(u32 => UIntConstant);
impl_from!(f32 => FloatConstant);
impl_from!(f64 => DoubleConstant);
impl_from!(bool => BoolConstant);

impl<'i> From<Token<'i>> for String {
    fn from(value: Token<'i>) -> Self {
        match value {
            Token::PpRest(s) => s.to_string(),
            other => panic!("cannot convert {:?} into String", other),
        }
    }
}
