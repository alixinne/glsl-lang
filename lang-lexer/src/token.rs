use lang_util::SmolStr;

#[derive(Debug, Clone, PartialEq, lang_util::Token)]
#[allow(missing_docs)]
pub enum Token {
    #[lang_util(token = "const", kind = "storage qualifier", kind = "type qualifier")]
    Const,
    #[lang_util(token = "bool", kind = "type name")]
    Bool,
    #[lang_util(token = "float", kind = "type name")]
    Float,
    #[lang_util(token = "int", kind = "type name")]
    Int,
    #[lang_util(token = "uint", kind = "type name")]
    UInt,
    #[lang_util(token = "double", kind = "type name")]
    Double,
    #[lang_util(token = "bvec2", kind = "type name")]
    BVec2,
    #[lang_util(token = "bvec3", kind = "type name")]
    BVec3,
    #[lang_util(token = "bvec4", kind = "type name")]
    BVec4,
    #[lang_util(token = "ivec2", kind = "type name")]
    IVec2,
    #[lang_util(token = "ivec3", kind = "type name")]
    IVec3,
    #[lang_util(token = "ivec4", kind = "type name")]
    IVec4,
    #[lang_util(token = "uvec2", kind = "type name")]
    UVec2,
    #[lang_util(token = "uvec3", kind = "type name")]
    UVec3,
    #[lang_util(token = "uvec4", kind = "type name")]
    UVec4,
    #[lang_util(token = "vec2", kind = "type name")]
    Vec2,
    #[lang_util(token = "vec3", kind = "type name")]
    Vec3,
    #[lang_util(token = "vec4", kind = "type name")]
    Vec4,
    #[lang_util(token = "mat2", kind = "type name")]
    Mat2,
    #[lang_util(token = "mat3", kind = "type name")]
    Mat3,
    #[lang_util(token = "mat4", kind = "type name")]
    Mat4,
    #[lang_util(token = "mat2x2", kind = "type name")]
    Mat2x2,
    #[lang_util(token = "mat2x3", kind = "type name")]
    Mat2x3,
    #[lang_util(token = "mat2x4", kind = "type name")]
    Mat2x4,
    #[lang_util(token = "mat3x2", kind = "type name")]
    Mat3x2,
    #[lang_util(token = "mat3x3", kind = "type name")]
    Mat3x3,
    #[lang_util(token = "mat3x4", kind = "type name")]
    Mat3x4,
    #[lang_util(token = "mat4x2", kind = "type name")]
    Mat4x2,
    #[lang_util(token = "mat4x3", kind = "type name")]
    Mat4x3,
    #[lang_util(token = "mat4x4", kind = "type name")]
    Mat4x4,
    #[lang_util(token = "dvec2", kind = "type name")]
    DVec2,
    #[lang_util(token = "dvec3", kind = "type name")]
    DVec3,
    #[lang_util(token = "dvec4", kind = "type name")]
    DVec4,
    #[lang_util(token = "dmat2", kind = "type name")]
    DMat2,
    #[lang_util(token = "dmat3", kind = "type name")]
    DMat3,
    #[lang_util(token = "dmat4", kind = "type name")]
    DMat4,
    #[lang_util(token = "dmat2x2", kind = "type name")]
    DMat2x2,
    #[lang_util(token = "dmat2x3", kind = "type name")]
    DMat2x3,
    #[lang_util(token = "dmat2x4", kind = "type name")]
    DMat2x4,
    #[lang_util(token = "dmat3x2", kind = "type name")]
    DMat3x2,
    #[lang_util(token = "dmat3x3", kind = "type name")]
    DMat3x3,
    #[lang_util(token = "dmat3x4", kind = "type name")]
    DMat3x4,
    #[lang_util(token = "dmat4x2", kind = "type name")]
    DMat4x2,
    #[lang_util(token = "dmat4x3", kind = "type name")]
    DMat4x3,
    #[lang_util(token = "dmat4x4", kind = "type name")]
    DMat4x4,
    #[lang_util(
        token = "centroid",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Centroid,
    #[lang_util(token = "in", kind = "storage qualifier", kind = "type qualifier")]
    In,
    #[lang_util(token = "out", kind = "storage qualifier", kind = "type qualifier")]
    Out,
    #[lang_util(token = "inout", kind = "storage qualifier", kind = "type qualifier")]
    InOut,
    #[lang_util(token = "uniform", kind = "storage qualifier", kind = "type qualifier")]
    Uniform,
    #[lang_util(token = "patch", kind = "storage qualifier", kind = "type qualifier")]
    Patch,
    #[lang_util(token = "sample", kind = "storage qualifier", kind = "type qualifier")]
    Sample,
    #[lang_util(token = "buffer", kind = "storage qualifier", kind = "type qualifier")]
    Buffer,
    #[lang_util(token = "shared", kind = "storage qualifier", kind = "type qualifier")]
    Shared,
    #[lang_util(
        token = "coherent",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Coherent,
    #[lang_util(
        token = "volatile",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Volatile,
    #[lang_util(
        token = "restrict",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Restrict,
    #[lang_util(
        token = "readonly",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    ReadOnly,
    #[lang_util(
        token = "writeonly",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    WriteOnly,
    #[lang_util(
        token = "attribute",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Attribute,
    #[lang_util(token = "varying", kind = "storage qualifier", kind = "type qualifier")]
    Varying,
    #[lang_util(
        token = "noperspective",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    NoPerspective,
    #[lang_util(
        token = "flat",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    Flat,
    #[lang_util(
        token = "smooth",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    Smooth,
    #[lang_util(token = "layout", kind = "layout qualifier", kind = "type qualifier")]
    Layout,
    #[lang_util(token = "atomic_uint", kind = "type name")]
    AtomicUInt,
    #[lang_util(token = "sampler1D", kind = "type name")]
    Sampler1D,
    #[lang_util(
        token = "sampler1DShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler1DShadow,
    #[lang_util(
        token = "sampler1DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler1DArray,
    #[lang_util(
        token = "sampler1DArrayShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler1DArrayShadow,
    #[lang_util(token = "isampler1D", kind = "type name")]
    ISampler1D,
    #[lang_util(
        token = "isampler1DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler1DArray,
    #[lang_util(token = "usampler1D", kind = "type name")]
    USampler1D,
    #[lang_util(
        token = "usampler1DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler1DArray,
    #[lang_util(token = "sampler2D", kind = "type name")]
    Sampler2D,
    #[lang_util(
        token = "sampler2DShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DShadow,
    #[lang_util(
        token = "sampler2DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DArray,
    #[lang_util(
        token = "sampler2DArrayShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DArrayShadow,
    #[lang_util(token = "isampler2D", kind = "type name")]
    ISampler2D,
    #[lang_util(
        token = "isampler2DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler2DArray,
    #[lang_util(token = "usampler2D", kind = "type name")]
    USampler2D,
    #[lang_util(
        token = "usampler2DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler2DArray,
    #[lang_util(token = "sampler2DRect", kind = "type name")]
    Sampler2DRect,
    #[lang_util(
        token = "sampler2DRectShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DRectShadow,
    #[lang_util(
        token = "isampler2DRect",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler2DRect,
    #[lang_util(
        token = "usampler2DRect",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler2DRect,
    #[lang_util(token = "sampler2DMS", kind = "type name")]
    Sampler2DMs,
    #[lang_util(token = "isampler2DMS", kind = "type name")]
    ISampler2DMs,
    #[lang_util(token = "usampler2DMS", kind = "type name")]
    USampler2DMs,
    #[lang_util(
        token = "sampler2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DMsArray,
    #[lang_util(
        token = "isampler2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler2DMsArray,
    #[lang_util(
        token = "usampler2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler2DMsArray,
    #[lang_util(token = "sampler3D", kind = "type name")]
    Sampler3D,
    #[lang_util(token = "isampler3D", kind = "type name")]
    ISampler3D,
    #[lang_util(token = "usampler3D", kind = "type name")]
    USampler3D,
    #[lang_util(token = "samplerCube", kind = "type name")]
    SamplerCube,
    #[lang_util(
        token = "samplerCubeShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    SamplerCubeShadow,
    #[lang_util(token = "isamplerCube", kind = "type name")]
    ISamplerCube,
    #[lang_util(token = "usamplerCube", kind = "type name")]
    USamplerCube,
    #[lang_util(
        token = "samplerCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    SamplerCubeArray,
    #[lang_util(
        token = "samplerCubeArrayShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    SamplerCubeArrayShadow,
    #[lang_util(
        token = "isamplerCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISamplerCubeArray,
    #[lang_util(
        token = "usamplerCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USamplerCubeArray,
    #[lang_util(token = "samplerBuffer", kind = "type name")]
    SamplerBuffer,
    #[lang_util(
        token = "isamplerBuffer",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISamplerBuffer,
    #[lang_util(
        token = "usamplerBuffer",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USamplerBuffer,
    #[lang_util(token = "image1D", kind = "type name")]
    Image1D,
    #[lang_util(token = "iimage1D", kind = "type name")]
    IImage1D,
    #[lang_util(token = "uimage1D", kind = "type name")]
    UImage1D,
    #[lang_util(token = "image1DArray", kind = "type name")]
    Image1DArray,
    #[lang_util(token = "iimage1DArray", kind = "type name")]
    IImage1DArray,
    #[lang_util(token = "uimage1DArray", kind = "type name")]
    UImage1DArray,
    #[lang_util(token = "image2D", kind = "type name")]
    Image2D,
    #[lang_util(token = "iimage2D", kind = "type name")]
    IImage2D,
    #[lang_util(token = "uimage2D", kind = "type name")]
    UImage2D,
    #[lang_util(token = "image2DArray", kind = "type name")]
    Image2DArray,
    #[lang_util(token = "iimage2DArray", kind = "type name")]
    IImage2DArray,
    #[lang_util(token = "uimage2DArray", kind = "type name")]
    UImage2DArray,
    #[lang_util(token = "image2DRect", kind = "type name")]
    Image2DRect,
    #[lang_util(token = "iimage2DRect", kind = "type name")]
    IImage2DRect,
    #[lang_util(token = "uimage2DRect", kind = "type name")]
    UImage2DRect,
    #[lang_util(token = "image2DMS", kind = "type name")]
    Image2DMs,
    #[lang_util(token = "iimage2DMS", kind = "type name")]
    IImage2DMs,
    #[lang_util(token = "uimage2DMS", kind = "type name")]
    UImage2DMs,
    #[lang_util(
        token = "image2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Image2DMsArray,
    #[lang_util(
        token = "iimage2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    IImage2DMsArray,
    #[lang_util(
        token = "uimage2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    UImage2DMsArray,
    #[lang_util(token = "image3D", kind = "type name")]
    Image3D,
    #[lang_util(token = "iimage3D", kind = "type name")]
    IImage3D,
    #[lang_util(token = "uimage3D", kind = "type name")]
    UImage3D,
    #[lang_util(token = "imageCube", kind = "type name")]
    ImageCube,
    #[lang_util(token = "iimageCube", kind = "type name")]
    IImageCube,
    #[lang_util(token = "uimageCube", kind = "type name")]
    UImageCube,
    #[lang_util(
        token = "imageCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ImageCubeArray,
    #[lang_util(
        token = "iimageCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    IImageCubeArray,
    #[lang_util(
        token = "uimageCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    UImageCubeArray,
    #[lang_util(token = "imageBuffer", kind = "type name")]
    ImageBuffer,
    #[lang_util(token = "iimageBuffer", kind = "type name")]
    IImageBuffer,
    #[lang_util(token = "uimageBuffer", kind = "type name")]
    UImageBuffer,

    // Begin Vulkan-target keywords
    #[lang_util(token = "texture1D")]
    Texture1D,
    #[lang_util(token = "texture1DArray")]
    Texture1DArray,
    #[lang_util(token = "itexture1D")]
    ITexture1D,
    #[lang_util(token = "itexture1DArray")]
    ITexture1DArray,
    #[lang_util(token = "utexture1D")]
    UTexture1D,
    #[lang_util(token = "utexture1DArray")]
    UTexture1DArray,
    #[lang_util(token = "texture2D")]
    Texture2D,
    #[lang_util(token = "texture2DArray")]
    Texture2DArray,
    #[lang_util(token = "itexture2D")]
    ITexture2D,
    #[lang_util(token = "itexture2DArray")]
    ITexture2DArray,
    #[lang_util(token = "utexture2D")]
    UTexture2D,
    #[lang_util(token = "utexture2DArray")]
    UTexture2DArray,
    #[lang_util(token = "texture2DRect")]
    Texture2DRect,
    #[lang_util(token = "itexture2DRect")]
    ITexture2DRect,
    #[lang_util(token = "utexture2DRect")]
    UTexture2DRect,
    #[lang_util(token = "texture2DMS")]
    Texture2DMs,
    #[lang_util(token = "itexture2DMS")]
    ITexture2DMs,
    #[lang_util(token = "utexture2DMS")]
    UTexture2DMs,
    #[lang_util(token = "texture2DMSArray")]
    Texture2DMsArray,
    #[lang_util(token = "itexture2DMSArray")]
    ITexture2DMsArray,
    #[lang_util(token = "utexture2DMSArray")]
    UTexture2DMsArray,
    #[lang_util(token = "texture3D")]
    Texture3D,
    #[lang_util(token = "itexture3D")]
    ITexture3D,
    #[lang_util(token = "utexture3D")]
    UTexture3D,
    #[lang_util(token = "textureCube")]
    TextureCube,
    #[lang_util(token = "itextureCube")]
    ITextureCube,
    #[lang_util(token = "utextureCube")]
    UTextureCube,
    #[lang_util(token = "textureCubeArray")]
    TextureCubeArray,
    #[lang_util(token = "itextureCubeArray")]
    ITextureCubeArray,
    #[lang_util(token = "utextureCubeArray")]
    UTextureCubeArray,
    #[lang_util(token = "textureBuffer")]
    TextureBuffer,
    #[lang_util(token = "itextureBuffer")]
    ITextureBuffer,
    #[lang_util(token = "utextureBuffer")]
    UTextureBuffer,
    #[lang_util(token = "sampler")]
    Sampler,
    #[lang_util(token = "samplerShadow")]
    SamplerShadow,
    #[lang_util(token = "subpassInput")]
    SubpassInput,
    #[lang_util(token = "isubpassInput")]
    ISubpassInput,
    #[lang_util(token = "usubpassInput")]
    USubpassInput,
    #[lang_util(token = "subpassInputMS")]
    SubpassInputMs,
    #[lang_util(token = "isubpassInputMS")]
    ISubpassInputMs,
    #[lang_util(token = "usubpassInputMS")]
    USubpassInputMs,
    // End Vulkan-target keywords
    #[lang_util(token = "struct", kind = "struct", kind = "keyword")]
    Struct,
    #[lang_util(token = "void", kind = "type name")]
    Void,
    #[lang_util(token = "while", kind = "keyword")]
    While,
    #[lang_util(token = "break", kind = "keyword")]
    Break,
    #[lang_util(token = "continue", kind = "keyword")]
    Continue,
    #[lang_util(token = "do", kind = "keyword")]
    Do,
    #[lang_util(token = "else", kind = "keyword")]
    Else,
    #[lang_util(token = "for", kind = "keyword")]
    For,
    #[lang_util(token = "if", kind = "keyword")]
    If,
    #[lang_util(token = "discard", kind = "keyword")]
    Discard,
    #[lang_util(token = "return", kind = "keyword")]
    Return,
    #[lang_util(token = "switch", kind = "keyword")]
    Switch,
    #[lang_util(token = "case", kind = "keyword")]
    Case,
    #[lang_util(token = "default", kind = "keyword")]
    Default,
    #[lang_util(
        token = "subroutine",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Subroutine,
    #[lang_util(parser = "ident", kind = "identifier")]
    Identifier(SmolStr),
    #[lang_util(parser = "ty_name", kind = "type name")]
    TypeName(SmolStr), // Cast from Identifier depending on known type names
    #[lang_util(parser = "float_constant", kind = "literal")]
    FloatConstant(f32),
    #[lang_util(parser = "int_constant", kind = "literal")]
    IntConstant(i32),
    #[lang_util(parser = "uint_constant", kind = "literal")]
    UIntConstant(u32),
    #[lang_util(parser = "bool_constant", kind = "literal")]
    BoolConstant(bool),
    #[lang_util(parser = "double_constant", kind = "literal")]
    DoubleConstant(f64),
    #[lang_util(token = "<<", kind = "binary operator", kind = "operator")]
    LeftOp,
    #[lang_util(token = ">>", kind = "binary operator", kind = "operator")]
    RightOp,
    #[lang_util(token = "++", kind = "unary operator", kind = "operator")]
    IncOp,
    #[lang_util(token = "--", kind = "unary operator", kind = "operator")]
    DecOp,
    #[lang_util(token = "<=", kind = "binary operator", kind = "operator")]
    LeOp,
    #[lang_util(token = ">=", kind = "binary operator", kind = "operator")]
    GeOp,
    #[lang_util(token = "==", kind = "binary operator", kind = "operator")]
    EqOp,
    #[lang_util(token = "!=", kind = "binary operator", kind = "operator")]
    NeOp,
    #[lang_util(token = "&&", kind = "binary operator", kind = "operator")]
    AndOp,
    #[lang_util(token = "||", kind = "binary operator", kind = "operator")]
    OrOp,
    #[lang_util(token = "^^", kind = "binary operator", kind = "operator")]
    XorOp,
    #[lang_util(token = "*=", kind = "binary operator", kind = "operator")]
    MulAssign,
    #[lang_util(token = "/=", kind = "binary operator", kind = "operator")]
    DivAssign,
    #[lang_util(token = "+=", kind = "binary operator", kind = "operator")]
    AddAssign,
    #[lang_util(token = "%=", kind = "binary operator", kind = "operator")]
    ModAssign,
    #[lang_util(token = "<<=", kind = "binary operator", kind = "operator")]
    LeftAssign,
    #[lang_util(token = ">>=", kind = "binary operator", kind = "operator")]
    RightAssign,
    #[lang_util(token = "&=", kind = "binary operator", kind = "operator")]
    AndAssign,
    #[lang_util(token = "^=", kind = "binary operator", kind = "operator")]
    XorAssign,
    #[lang_util(token = "|=", kind = "binary operator", kind = "operator")]
    OrAssign,
    #[lang_util(token = "-=", kind = "binary operator", kind = "operator")]
    SubAssign,
    #[lang_util(token = "(")]
    LeftParen,
    #[lang_util(token = ")")]
    RightParen,
    #[lang_util(token = "[")]
    LeftBracket,
    #[lang_util(token = "]")]
    RightBracket,
    #[lang_util(token = "{")]
    LeftBrace,
    #[lang_util(token = "}")]
    RightBrace,
    #[lang_util(token = ".", kind = "binary operator", kind = "operator")]
    Dot,
    #[lang_util(token = ",", kind = "operator")]
    Comma,
    #[lang_util(token = ":", kind = "operator")]
    Colon,
    #[lang_util(token = "=", kind = "binary operator", kind = "operator")]
    Equal,
    #[lang_util(token = ";")]
    Semicolon,
    #[lang_util(token = "!", kind = "unary operator", kind = "operator")]
    Bang,
    #[lang_util(
        token = "-",
        kind = "binary operator",
        kind = "unary operator",
        kind = "operator"
    )]
    Dash,
    #[lang_util(token = "~", kind = "unary operator", kind = "operator")]
    Tilde,
    #[lang_util(
        token = "+",
        kind = "binary operator",
        kind = "unary operator",
        kind = "operator"
    )]
    Plus,
    #[lang_util(token = "*", kind = "binary operator", kind = "operator")]
    Star,
    #[lang_util(token = "/", kind = "binary operator", kind = "operator")]
    Slash,
    #[lang_util(token = "%", kind = "binary operator", kind = "operator")]
    Percent,
    #[lang_util(token = "<", kind = "binary operator", kind = "operator")]
    LeftAngle,
    #[lang_util(token = ">", kind = "binary operator", kind = "operator")]
    RightAngle,
    #[lang_util(token = "|", kind = "binary operator", kind = "operator")]
    VerticalBar,
    #[lang_util(token = "^", kind = "binary operator", kind = "operator")]
    Caret,
    #[lang_util(token = "&", kind = "binary operator", kind = "operator")]
    Ampersand,
    #[lang_util(token = "?", kind = "operator")]
    Question,
    #[lang_util(token = "invariant")]
    Invariant,
    #[lang_util(token = "precise", kind = "type qualifier")]
    Precise,
    #[lang_util(token = "highp", kind = "precision qualifier", kind = "type qualifier")]
    HighPrecision,
    #[lang_util(
        token = "mediump",
        kind = "precision qualifier",
        kind = "type qualifier"
    )]
    MediumPrecision,
    #[lang_util(token = "lowp", kind = "precision qualifier", kind = "type qualifier")]
    LowPrecision,
    #[lang_util(token = "precision")]
    Precision,

    // TODO: Line continuation can happen inside tokens
    #[lang_util(display = "<whitespace>", parser(display), kind = "trivia")]
    Whitespace,
    #[lang_util(display = "<single line comment>", parser(display), kind = "trivia")]
    SingleLineComment,
    #[lang_util(display = "<multi line comment>", parser(display), kind = "trivia")]
    MultiLineComment,

    // TODO: Line continuations in preprocessor pragmas?
    #[lang_util(display = "#define", parser(display), kind = "preprocessor directive")]
    PpDefine,
    #[lang_util(display = "#else", parser(display), kind = "preprocessor directive")]
    PpElse,
    #[lang_util(display = "#elif", parser(display), kind = "preprocessor directive")]
    PpElif,
    #[lang_util(display = "#endif", parser(display), kind = "preprocessor directive")]
    PpEndIf,
    #[lang_util(display = "#error", parser(display), kind = "preprocessor directive")]
    PpError,
    #[lang_util(display = "#if", parser(display), kind = "preprocessor directive")]
    PpIf,
    #[lang_util(display = "#ifdef", parser(display), kind = "preprocessor directive")]
    PpIfDef,
    #[lang_util(display = "#ifndef", parser(display), kind = "preprocessor directive")]
    PpIfNDef,
    #[lang_util(display = "#include", parser(display), kind = "preprocessor directive")]
    PpInclude,
    #[lang_util(display = "#line", parser(display), kind = "preprocessor directive")]
    PpLine,
    #[lang_util(display = "#pragma", parser(display), kind = "preprocessor directive")]
    PpPragma,
    #[lang_util(display = "#undef", parser(display), kind = "preprocessor directive")]
    PpUndef,
    #[lang_util(display = "#version", parser(display), kind = "preprocessor directive")]
    PpVersion,
    #[lang_util(
        display = "#extension",
        parser(display),
        kind = "preprocessor directive"
    )]
    PpExtension,

    #[lang_util(
        display = "<preprocessor string>",
        parser(display),
        kind = "preprocessor string"
    )]
    PpRest(String),

    #[lang_util(display = "core", parser(display), kind = "version profile")]
    PpCore,
    #[lang_util(display = "compatibility", parser(display), kind = "version profile")]
    PpCompatibility,
    #[lang_util(display = "es", parser(display), kind = "version profile")]
    PpEs,

    #[lang_util(display = "require", parser(display), kind = "extension behavior")]
    PpExtRequire,
    #[lang_util(display = "enable", parser(display), kind = "extension behavior")]
    PpExtEnable,
    #[lang_util(display = "warn", parser(display), kind = "extension behavior")]
    PpExtWarn,
    #[lang_util(display = "disable", parser(display), kind = "extension behavior")]
    PpExtDisable,

    #[lang_util(display = "<{}>", parser(display), kind = "include path")]
    PpPathAbsolute(String),
    #[lang_util(display = "\"{}\"", parser(display), kind = "include path")]
    PpPathRelative(String),

    #[lang_util(display = "<invalid token>", parser(display), kind = "error")]
    Error,
}

impl Token {
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
    pub fn as_str(&self) -> &str {
        match self {
            Self::Identifier(s) => s,
            Self::TypeName(s) => s,
            Self::PpPathRelative(s) => s,
            Self::PpPathAbsolute(s) => s,
            _ => panic!("cannot convert token {self:?}, to str"),
        }
    }
}

macro_rules! impl_from {
    ($t:ty => $i:ident) => {
        impl From<Token> for $t {
            fn from(value: Token) -> Self {
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

impl From<Token> for String {
    fn from(value: Token) -> Self {
        match value {
            Token::PpRest(s) => s,
            other => panic!("cannot convert {other:?} into String"),
        }
    }
}
