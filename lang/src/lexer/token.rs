use smol_str::SmolStr;

use super::LexerContext;

#[cfg(feature = "lexer-v1")]
use super::v1::parsers::{
    parse_cmt, parse_f32, parse_f64, parse_ident, parse_int, parse_rs_ident, parse_uint,
};
#[cfg(feature = "lexer-v1")]
use logos::Logos;

#[derive(Debug, Clone, PartialEq, lang_util::Token)]
#[cfg_attr(feature = "lexer-v1", derive(Logos))]
#[cfg_attr(feature = "lexer-v1", logos(extras = LexerContext))]
#[allow(missing_docs)]
pub enum Token {
    #[cfg_attr(feature = "lexer-v1", token("const"))]
    #[lang_util(token = "const", kind = "storage qualifier", kind = "type qualifier")]
    Const,
    #[cfg_attr(feature = "lexer-v1", token("bool"))]
    #[lang_util(token = "bool", kind = "type name")]
    Bool,
    #[cfg_attr(feature = "lexer-v1", token("float"))]
    #[lang_util(token = "float", kind = "type name")]
    Float,
    #[cfg_attr(feature = "lexer-v1", token("int"))]
    #[lang_util(token = "int", kind = "type name")]
    Int,
    #[cfg_attr(feature = "lexer-v1", token("uint"))]
    #[lang_util(token = "uint", kind = "type name")]
    UInt,
    #[cfg_attr(feature = "lexer-v1", token("double"))]
    #[lang_util(token = "double", kind = "type name")]
    Double,
    #[cfg_attr(feature = "lexer-v1", token("bvec2"))]
    #[lang_util(token = "bvec2", kind = "type name")]
    BVec2,
    #[cfg_attr(feature = "lexer-v1", token("bvec3"))]
    #[lang_util(token = "bvec3", kind = "type name")]
    BVec3,
    #[cfg_attr(feature = "lexer-v1", token("bvec4"))]
    #[lang_util(token = "bvec4", kind = "type name")]
    BVec4,
    #[cfg_attr(feature = "lexer-v1", token("ivec2"))]
    #[lang_util(token = "ivec2", kind = "type name")]
    IVec2,
    #[cfg_attr(feature = "lexer-v1", token("ivec3"))]
    #[lang_util(token = "ivec3", kind = "type name")]
    IVec3,
    #[cfg_attr(feature = "lexer-v1", token("ivec4"))]
    #[lang_util(token = "ivec4", kind = "type name")]
    IVec4,
    #[cfg_attr(feature = "lexer-v1", token("uvec2"))]
    #[lang_util(token = "uvec2", kind = "type name")]
    UVec2,
    #[cfg_attr(feature = "lexer-v1", token("uvec3"))]
    #[lang_util(token = "uvec3", kind = "type name")]
    UVec3,
    #[cfg_attr(feature = "lexer-v1", token("uvec4"))]
    #[lang_util(token = "uvec4", kind = "type name")]
    UVec4,
    #[cfg_attr(feature = "lexer-v1", token("vec2"))]
    #[lang_util(token = "vec2", kind = "type name")]
    Vec2,
    #[cfg_attr(feature = "lexer-v1", token("vec3"))]
    #[lang_util(token = "vec3", kind = "type name")]
    Vec3,
    #[cfg_attr(feature = "lexer-v1", token("vec4"))]
    #[lang_util(token = "vec4", kind = "type name")]
    Vec4,
    #[cfg_attr(feature = "lexer-v1", token("mat2"))]
    #[lang_util(token = "mat2", kind = "type name")]
    Mat2,
    #[cfg_attr(feature = "lexer-v1", token("mat3"))]
    #[lang_util(token = "mat3", kind = "type name")]
    Mat3,
    #[cfg_attr(feature = "lexer-v1", token("mat4"))]
    #[lang_util(token = "mat4", kind = "type name")]
    Mat4,
    #[cfg_attr(feature = "lexer-v1", token("mat2x2"))]
    #[lang_util(token = "mat2x2", kind = "type name")]
    Mat2x2,
    #[cfg_attr(feature = "lexer-v1", token("mat2x3"))]
    #[lang_util(token = "mat2x3", kind = "type name")]
    Mat2x3,
    #[cfg_attr(feature = "lexer-v1", token("mat2x4"))]
    #[lang_util(token = "mat2x4", kind = "type name")]
    Mat2x4,
    #[cfg_attr(feature = "lexer-v1", token("mat3x2"))]
    #[lang_util(token = "mat3x2", kind = "type name")]
    Mat3x2,
    #[cfg_attr(feature = "lexer-v1", token("mat3x3"))]
    #[lang_util(token = "mat3x3", kind = "type name")]
    Mat3x3,
    #[cfg_attr(feature = "lexer-v1", token("mat3x4"))]
    #[lang_util(token = "mat3x4", kind = "type name")]
    Mat3x4,
    #[cfg_attr(feature = "lexer-v1", token("mat4x2"))]
    #[lang_util(token = "mat4x2", kind = "type name")]
    Mat4x2,
    #[cfg_attr(feature = "lexer-v1", token("mat4x3"))]
    #[lang_util(token = "mat4x3", kind = "type name")]
    Mat4x3,
    #[cfg_attr(feature = "lexer-v1", token("mat4x4"))]
    #[lang_util(token = "mat4x4", kind = "type name")]
    Mat4x4,
    #[cfg_attr(feature = "lexer-v1", token("dvec2"))]
    #[lang_util(token = "dvec2", kind = "type name")]
    DVec2,
    #[cfg_attr(feature = "lexer-v1", token("dvec3"))]
    #[lang_util(token = "dvec3", kind = "type name")]
    DVec3,
    #[cfg_attr(feature = "lexer-v1", token("dvec4"))]
    #[lang_util(token = "dvec4", kind = "type name")]
    DVec4,
    #[cfg_attr(feature = "lexer-v1", token("dmat2"))]
    #[lang_util(token = "dmat2", kind = "type name")]
    DMat2,
    #[cfg_attr(feature = "lexer-v1", token("dmat3"))]
    #[lang_util(token = "dmat3", kind = "type name")]
    DMat3,
    #[cfg_attr(feature = "lexer-v1", token("dmat4"))]
    #[lang_util(token = "dmat4", kind = "type name")]
    DMat4,
    #[cfg_attr(feature = "lexer-v1", token("dmat2x2"))]
    #[lang_util(token = "dmat2x2", kind = "type name")]
    DMat2x2,
    #[cfg_attr(feature = "lexer-v1", token("dmat2x3"))]
    #[lang_util(token = "dmat2x3", kind = "type name")]
    DMat2x3,
    #[cfg_attr(feature = "lexer-v1", token("dmat2x4"))]
    #[lang_util(token = "dmat2x4", kind = "type name")]
    DMat2x4,
    #[cfg_attr(feature = "lexer-v1", token("dmat3x2"))]
    #[lang_util(token = "dmat3x2", kind = "type name")]
    DMat3x2,
    #[cfg_attr(feature = "lexer-v1", token("dmat3x3"))]
    #[lang_util(token = "dmat3x3", kind = "type name")]
    DMat3x3,
    #[cfg_attr(feature = "lexer-v1", token("dmat3x4"))]
    #[lang_util(token = "dmat3x4", kind = "type name")]
    DMat3x4,
    #[cfg_attr(feature = "lexer-v1", token("dmat4x2"))]
    #[lang_util(token = "dmat4x2", kind = "type name")]
    DMat4x2,
    #[cfg_attr(feature = "lexer-v1", token("dmat4x3"))]
    #[lang_util(token = "dmat4x3", kind = "type name")]
    DMat4x3,
    #[cfg_attr(feature = "lexer-v1", token("dmat4x4"))]
    #[lang_util(token = "dmat4x4", kind = "type name")]
    DMat4x4,
    #[cfg_attr(feature = "lexer-v1", token("centroid"))]
    #[lang_util(
        token = "centroid",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Centroid,
    #[cfg_attr(feature = "lexer-v1", token("in"))]
    #[lang_util(token = "in", kind = "storage qualifier", kind = "type qualifier")]
    In,
    #[cfg_attr(feature = "lexer-v1", token("out"))]
    #[lang_util(token = "out", kind = "storage qualifier", kind = "type qualifier")]
    Out,
    #[cfg_attr(feature = "lexer-v1", token("inout"))]
    #[lang_util(token = "inout", kind = "storage qualifier", kind = "type qualifier")]
    InOut,
    #[cfg_attr(feature = "lexer-v1", token("uniform"))]
    #[lang_util(token = "uniform", kind = "storage qualifier", kind = "type qualifier")]
    Uniform,
    #[cfg_attr(feature = "lexer-v1", token("patch"))]
    #[lang_util(token = "patch", kind = "storage qualifier", kind = "type qualifier")]
    Patch,
    #[cfg_attr(feature = "lexer-v1", token("sample"))]
    #[lang_util(token = "sample", kind = "storage qualifier", kind = "type qualifier")]
    Sample,
    #[cfg_attr(feature = "lexer-v1", token("buffer"))]
    #[lang_util(token = "buffer", kind = "storage qualifier", kind = "type qualifier")]
    Buffer,
    #[cfg_attr(feature = "lexer-v1", token("shared"))]
    #[lang_util(token = "shared", kind = "storage qualifier", kind = "type qualifier")]
    Shared,
    #[cfg_attr(feature = "lexer-v1", token("coherent"))]
    #[lang_util(
        token = "coherent",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Coherent,
    #[cfg_attr(feature = "lexer-v1", token("volatile"))]
    #[lang_util(
        token = "volatile",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Volatile,
    #[cfg_attr(feature = "lexer-v1", token("restrict"))]
    #[lang_util(
        token = "restrict",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Restrict,
    #[cfg_attr(feature = "lexer-v1", token("readonly"))]
    #[lang_util(
        token = "readonly",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    ReadOnly,
    #[cfg_attr(feature = "lexer-v1", token("writeonly"))]
    #[lang_util(
        token = "writeonly",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    WriteOnly,
    #[cfg_attr(feature = "lexer-v1", token("attribute"))]
    #[lang_util(
        token = "attribute",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Attribute,
    #[cfg_attr(feature = "lexer-v1", token("varying"))]
    #[lang_util(token = "varying", kind = "storage qualifier", kind = "type qualifier")]
    Varying,
    #[cfg_attr(feature = "lexer-v1", token("noperspective"))]
    #[lang_util(
        token = "noperspective",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    NoPerspective,
    #[cfg_attr(feature = "lexer-v1", token("flat"))]
    #[lang_util(
        token = "flat",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    Flat,
    #[cfg_attr(feature = "lexer-v1", token("smooth"))]
    #[lang_util(
        token = "smooth",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    Smooth,
    #[cfg_attr(feature = "lexer-v1", token("layout"))]
    #[lang_util(token = "layout", kind = "layout qualifier", kind = "type qualifier")]
    Layout,
    #[cfg_attr(feature = "lexer-v1", token("atomic_uint"))]
    #[lang_util(token = "atomic_uint", kind = "type name")]
    AtomicUInt,
    #[cfg_attr(feature = "lexer-v1", token("sampler1D"))]
    #[lang_util(token = "sampler1D", kind = "type name")]
    Sampler1D,
    #[cfg_attr(feature = "lexer-v1", token("sampler1DShadow"))]
    #[lang_util(
        token = "sampler1DShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler1DShadow,
    #[cfg_attr(feature = "lexer-v1", token("sampler1DArray"))]
    #[lang_util(
        token = "sampler1DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler1DArray,
    #[cfg_attr(feature = "lexer-v1", token("sampler1DArrayShadow"))]
    #[lang_util(
        token = "sampler1DArrayShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler1DArrayShadow,
    #[cfg_attr(feature = "lexer-v1", token("isampler1D"))]
    #[lang_util(token = "isampler1D", kind = "type name")]
    ISampler1D,
    #[cfg_attr(feature = "lexer-v1", token("isampler1DArray"))]
    #[lang_util(
        token = "isampler1DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler1DArray,
    #[cfg_attr(feature = "lexer-v1", token("usampler1D"))]
    #[lang_util(token = "usampler1D", kind = "type name")]
    USampler1D,
    #[cfg_attr(feature = "lexer-v1", token("usampler1DArray"))]
    #[lang_util(
        token = "usampler1DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler1DArray,
    #[cfg_attr(feature = "lexer-v1", token("sampler2D"))]
    #[lang_util(token = "sampler2D", kind = "type name")]
    Sampler2D,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DShadow"))]
    #[lang_util(
        token = "sampler2DShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DShadow,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DArray"))]
    #[lang_util(
        token = "sampler2DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DArray,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DArrayShadow"))]
    #[lang_util(
        token = "sampler2DArrayShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DArrayShadow,
    #[cfg_attr(feature = "lexer-v1", token("isampler2D"))]
    #[lang_util(token = "isampler2D", kind = "type name")]
    ISampler2D,
    #[cfg_attr(feature = "lexer-v1", token("isampler2DArray"))]
    #[lang_util(
        token = "isampler2DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler2DArray,
    #[cfg_attr(feature = "lexer-v1", token("usampler2D"))]
    #[lang_util(token = "usampler2D", kind = "type name")]
    USampler2D,
    #[cfg_attr(feature = "lexer-v1", token("usampler2DArray"))]
    #[lang_util(
        token = "usampler2DArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler2DArray,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DRect"))]
    #[lang_util(token = "sampler2DRect", kind = "type name")]
    Sampler2DRect,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DRectShadow"))]
    #[lang_util(
        token = "sampler2DRectShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DRectShadow,
    #[cfg_attr(feature = "lexer-v1", token("isampler2DRect"))]
    #[lang_util(
        token = "isampler2DRect",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler2DRect,
    #[cfg_attr(feature = "lexer-v1", token("usampler2DRect"))]
    #[lang_util(
        token = "usampler2DRect",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler2DRect,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DMS"))]
    #[lang_util(token = "sampler2DMS", kind = "type name")]
    Sampler2DMs,
    #[cfg_attr(feature = "lexer-v1", token("isampler2DMS"))]
    #[lang_util(token = "isampler2DMS", kind = "type name")]
    ISampler2DMs,
    #[cfg_attr(feature = "lexer-v1", token("usampler2DMS"))]
    #[lang_util(token = "usampler2DMS", kind = "type name")]
    USampler2DMs,
    #[cfg_attr(feature = "lexer-v1", token("sampler2DMSArray"))]
    #[lang_util(
        token = "sampler2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Sampler2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("isampler2DMSArray"))]
    #[lang_util(
        token = "isampler2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISampler2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("usampler2DMSArray"))]
    #[lang_util(
        token = "usampler2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USampler2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("sampler3D"))]
    #[lang_util(token = "sampler3D", kind = "type name")]
    Sampler3D,
    #[cfg_attr(feature = "lexer-v1", token("isampler3D"))]
    #[lang_util(token = "isampler3D", kind = "type name")]
    ISampler3D,
    #[cfg_attr(feature = "lexer-v1", token("usampler3D"))]
    #[lang_util(token = "usampler3D", kind = "type name")]
    USampler3D,
    #[cfg_attr(feature = "lexer-v1", token("samplerCube"))]
    #[lang_util(token = "samplerCube", kind = "type name")]
    SamplerCube,
    #[cfg_attr(feature = "lexer-v1", token("samplerCubeShadow"))]
    #[lang_util(
        token = "samplerCubeShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    SamplerCubeShadow,
    #[cfg_attr(feature = "lexer-v1", token("isamplerCube"))]
    #[lang_util(token = "isamplerCube", kind = "type name")]
    ISamplerCube,
    #[cfg_attr(feature = "lexer-v1", token("usamplerCube"))]
    #[lang_util(token = "usamplerCube", kind = "type name")]
    USamplerCube,
    #[cfg_attr(feature = "lexer-v1", token("samplerCubeArray"))]
    #[lang_util(
        token = "samplerCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    SamplerCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("samplerCubeArrayShadow"))]
    #[lang_util(
        token = "samplerCubeArrayShadow",
        kind = "type name",
        kind = "vulkan type name"
    )]
    SamplerCubeArrayShadow,
    #[cfg_attr(feature = "lexer-v1", token("isamplerCubeArray"))]
    #[lang_util(
        token = "isamplerCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISamplerCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("usamplerCubeArray"))]
    #[lang_util(
        token = "usamplerCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USamplerCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("samplerBuffer"))]
    #[lang_util(token = "samplerBuffer", kind = "type name")]
    SamplerBuffer,
    #[cfg_attr(feature = "lexer-v1", token("isamplerBuffer"))]
    #[lang_util(
        token = "isamplerBuffer",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ISamplerBuffer,
    #[cfg_attr(feature = "lexer-v1", token("usamplerBuffer"))]
    #[lang_util(
        token = "usamplerBuffer",
        kind = "type name",
        kind = "vulkan type name"
    )]
    USamplerBuffer,
    #[cfg_attr(feature = "lexer-v1", token("image1D"))]
    #[lang_util(token = "image1D", kind = "type name")]
    Image1D,
    #[cfg_attr(feature = "lexer-v1", token("iimage1D"))]
    #[lang_util(token = "iimage1D", kind = "type name")]
    IImage1D,
    #[cfg_attr(feature = "lexer-v1", token("uimage1D"))]
    #[lang_util(token = "uimage1D", kind = "type name")]
    UImage1D,
    #[cfg_attr(feature = "lexer-v1", token("image1DArray"))]
    #[lang_util(token = "image1DArray", kind = "type name")]
    Image1DArray,
    #[cfg_attr(feature = "lexer-v1", token("iimage1DArray"))]
    #[lang_util(token = "iimage1DArray", kind = "type name")]
    IImage1DArray,
    #[cfg_attr(feature = "lexer-v1", token("uimage1DArray"))]
    #[lang_util(token = "uimage1DArray", kind = "type name")]
    UImage1DArray,
    #[cfg_attr(feature = "lexer-v1", token("image2D"))]
    #[lang_util(token = "image2D", kind = "type name")]
    Image2D,
    #[cfg_attr(feature = "lexer-v1", token("iimage2D"))]
    #[lang_util(token = "iimage2D", kind = "type name")]
    IImage2D,
    #[cfg_attr(feature = "lexer-v1", token("uimage2D"))]
    #[lang_util(token = "uimage2D", kind = "type name")]
    UImage2D,
    #[cfg_attr(feature = "lexer-v1", token("image2DArray"))]
    #[lang_util(token = "image2DArray", kind = "type name")]
    Image2DArray,
    #[cfg_attr(feature = "lexer-v1", token("iimage2DArray"))]
    #[lang_util(token = "iimage2DArray", kind = "type name")]
    IImage2DArray,
    #[cfg_attr(feature = "lexer-v1", token("uimage2DArray"))]
    #[lang_util(token = "uimage2DArray", kind = "type name")]
    UImage2DArray,
    #[cfg_attr(feature = "lexer-v1", token("image2DRect"))]
    #[lang_util(token = "image2DRect", kind = "type name")]
    Image2DRect,
    #[cfg_attr(feature = "lexer-v1", token("iimage2DRect"))]
    #[lang_util(token = "iimage2DRect", kind = "type name")]
    IImage2DRect,
    #[cfg_attr(feature = "lexer-v1", token("uimage2DRect"))]
    #[lang_util(token = "uimage2DRect", kind = "type name")]
    UImage2DRect,
    #[cfg_attr(feature = "lexer-v1", token("image2DMS"))]
    #[lang_util(token = "image2DMS", kind = "type name")]
    Image2DMs,
    #[cfg_attr(feature = "lexer-v1", token("iimage2DMS"))]
    #[lang_util(token = "iimage2DMS", kind = "type name")]
    IImage2DMs,
    #[cfg_attr(feature = "lexer-v1", token("uimage2DMS"))]
    #[lang_util(token = "uimage2DMS", kind = "type name")]
    UImage2DMs,
    #[cfg_attr(feature = "lexer-v1", token("image2DMSArray"))]
    #[lang_util(
        token = "image2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    Image2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("iimage2DMSArray"))]
    #[lang_util(
        token = "iimage2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    IImage2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("uimage2DMSArray"))]
    #[lang_util(
        token = "uimage2DMSArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    UImage2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("image3D"))]
    #[lang_util(token = "image3D", kind = "type name")]
    Image3D,
    #[cfg_attr(feature = "lexer-v1", token("iimage3D"))]
    #[lang_util(token = "iimage3D", kind = "type name")]
    IImage3D,
    #[cfg_attr(feature = "lexer-v1", token("uimage3D"))]
    #[lang_util(token = "uimage3D", kind = "type name")]
    UImage3D,
    #[cfg_attr(feature = "lexer-v1", token("imageCube"))]
    #[lang_util(token = "imageCube", kind = "type name")]
    ImageCube,
    #[cfg_attr(feature = "lexer-v1", token("iimageCube"))]
    #[lang_util(token = "iimageCube", kind = "type name")]
    IImageCube,
    #[cfg_attr(feature = "lexer-v1", token("uimageCube"))]
    #[lang_util(token = "uimageCube", kind = "type name")]
    UImageCube,
    #[cfg_attr(feature = "lexer-v1", token("imageCubeArray"))]
    #[lang_util(
        token = "imageCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    ImageCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("iimageCubeArray"))]
    #[lang_util(
        token = "iimageCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    IImageCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("uimageCubeArray"))]
    #[lang_util(
        token = "uimageCubeArray",
        kind = "type name",
        kind = "vulkan type name"
    )]
    UImageCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("imageBuffer"))]
    #[lang_util(token = "imageBuffer", kind = "type name")]
    ImageBuffer,
    #[cfg_attr(feature = "lexer-v1", token("iimageBuffer"))]
    #[lang_util(token = "iimageBuffer", kind = "type name")]
    IImageBuffer,
    #[cfg_attr(feature = "lexer-v1", token("uimageBuffer"))]
    #[lang_util(token = "uimageBuffer", kind = "type name")]
    UImageBuffer,

    // Begin Vulkan-target keywords
    #[cfg_attr(feature = "lexer-v1", token("texture1D"))]
    #[lang_util(token = "texture1D")]
    Texture1D,
    #[cfg_attr(feature = "lexer-v1", token("texture1DArray"))]
    #[lang_util(token = "texture1DArray")]
    Texture1DArray,
    #[cfg_attr(feature = "lexer-v1", token("itexture1D"))]
    #[lang_util(token = "itexture1D")]
    ITexture1D,
    #[cfg_attr(feature = "lexer-v1", token("itexture1DArray"))]
    #[lang_util(token = "itexture1DArray")]
    ITexture1DArray,
    #[cfg_attr(feature = "lexer-v1", token("utexture1D"))]
    #[lang_util(token = "utexture1D")]
    UTexture1D,
    #[cfg_attr(feature = "lexer-v1", token("utexture1DArray"))]
    #[lang_util(token = "utexture1DArray")]
    UTexture1DArray,
    #[cfg_attr(feature = "lexer-v1", token("texture2D"))]
    #[lang_util(token = "texture2D")]
    Texture2D,
    #[cfg_attr(feature = "lexer-v1", token("texture2DArray"))]
    #[lang_util(token = "texture2DArray")]
    Texture2DArray,
    #[cfg_attr(feature = "lexer-v1", token("itexture2D"))]
    #[lang_util(token = "itexture2D")]
    ITexture2D,
    #[cfg_attr(feature = "lexer-v1", token("itexture2DArray"))]
    #[lang_util(token = "itexture2DArray")]
    ITexture2DArray,
    #[cfg_attr(feature = "lexer-v1", token("utexture2D"))]
    #[lang_util(token = "utexture2D")]
    UTexture2D,
    #[cfg_attr(feature = "lexer-v1", token("utexture2DArray"))]
    #[lang_util(token = "utexture2DArray")]
    UTexture2DArray,
    #[cfg_attr(feature = "lexer-v1", token("texture2DRect"))]
    #[lang_util(token = "texture2DRect")]
    Texture2DRect,
    #[cfg_attr(feature = "lexer-v1", token("itexture2DRect"))]
    #[lang_util(token = "itexture2DRect")]
    ITexture2DRect,
    #[cfg_attr(feature = "lexer-v1", token("utexture2DRect"))]
    #[lang_util(token = "utexture2DRect")]
    UTexture2DRect,
    #[cfg_attr(feature = "lexer-v1", token("texture2DMS"))]
    #[lang_util(token = "texture2DMS")]
    Texture2DMs,
    #[cfg_attr(feature = "lexer-v1", token("itexture2DMS"))]
    #[lang_util(token = "itexture2DMS")]
    ITexture2DMs,
    #[cfg_attr(feature = "lexer-v1", token("utexture2DMS"))]
    #[lang_util(token = "utexture2DMS")]
    UTexture2DMs,
    #[cfg_attr(feature = "lexer-v1", token("texture2DMSArray"))]
    #[lang_util(token = "texture2DMSArray")]
    Texture2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("itexture2DMSArray"))]
    #[lang_util(token = "itexture2DMSArray")]
    ITexture2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("utexture2DMSArray"))]
    #[lang_util(token = "utexture2DMSArray")]
    UTexture2DMsArray,
    #[cfg_attr(feature = "lexer-v1", token("texture3D"))]
    #[lang_util(token = "texture3D")]
    Texture3D,
    #[cfg_attr(feature = "lexer-v1", token("itexture3D"))]
    #[lang_util(token = "itexture3D")]
    ITexture3D,
    #[cfg_attr(feature = "lexer-v1", token("utexture3D"))]
    #[lang_util(token = "utexture3D")]
    UTexture3D,
    #[cfg_attr(feature = "lexer-v1", token("textureCube"))]
    #[lang_util(token = "textureCube")]
    TextureCube,
    #[cfg_attr(feature = "lexer-v1", token("itextureCube"))]
    #[lang_util(token = "itextureCube")]
    ITextureCube,
    #[cfg_attr(feature = "lexer-v1", token("utextureCube"))]
    #[lang_util(token = "utextureCube")]
    UTextureCube,
    #[cfg_attr(feature = "lexer-v1", token("textureCubeArray"))]
    #[lang_util(token = "textureCubeArray")]
    TextureCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("itextureCubeArray"))]
    #[lang_util(token = "itextureCubeArray")]
    ITextureCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("utextureCubeArray"))]
    #[lang_util(token = "utextureCubeArray")]
    UTextureCubeArray,
    #[cfg_attr(feature = "lexer-v1", token("textureBuffer"))]
    #[lang_util(token = "textureBuffer")]
    TextureBuffer,
    #[cfg_attr(feature = "lexer-v1", token("itextureBuffer"))]
    #[lang_util(token = "itextureBuffer")]
    ITextureBuffer,
    #[cfg_attr(feature = "lexer-v1", token("utextureBuffer"))]
    #[lang_util(token = "utextureBuffer")]
    UTextureBuffer,
    #[cfg_attr(feature = "lexer-v1", token("sampler"))]
    #[lang_util(token = "sampler")]
    Sampler,
    #[cfg_attr(feature = "lexer-v1", token("samplerShadow"))]
    #[lang_util(token = "samplerShadow")]
    SamplerShadow,
    #[cfg_attr(feature = "lexer-v1", token("subpassInput"))]
    #[lang_util(token = "subpassInput")]
    SubpassInput,
    #[cfg_attr(feature = "lexer-v1", token("isubpassInput"))]
    #[lang_util(token = "isubpassInput")]
    ISubpassInput,
    #[cfg_attr(feature = "lexer-v1", token("usubpassInput"))]
    #[lang_util(token = "usubpassInput")]
    USubpassInput,
    #[cfg_attr(feature = "lexer-v1", token("subpassInputMS"))]
    #[lang_util(token = "subpassInputMS")]
    SubpassInputMs,
    #[cfg_attr(feature = "lexer-v1", token("isubpassInputMS"))]
    #[lang_util(token = "isubpassInputMS")]
    ISubpassInputMs,
    #[cfg_attr(feature = "lexer-v1", token("usubpassInputMS"))]
    #[lang_util(token = "usubpassInputMS")]
    USubpassInputMs,
    // End Vulkan-target keywords
    #[cfg_attr(feature = "lexer-v1", token("struct"))]
    #[lang_util(token = "struct", kind = "struct", kind = "keyword")]
    Struct,
    #[cfg_attr(feature = "lexer-v1", token("void"))]
    #[lang_util(token = "void", kind = "type name")]
    Void,
    #[cfg_attr(feature = "lexer-v1", token("while"))]
    #[lang_util(token = "while", kind = "keyword")]
    While,
    #[cfg_attr(feature = "lexer-v1", token("break"))]
    #[lang_util(token = "break", kind = "keyword")]
    Break,
    #[cfg_attr(feature = "lexer-v1", token("continue"))]
    #[lang_util(token = "continue", kind = "keyword")]
    Continue,
    #[cfg_attr(feature = "lexer-v1", token("do"))]
    #[lang_util(token = "do", kind = "keyword")]
    Do,
    #[cfg_attr(feature = "lexer-v1", token("else"))]
    #[lang_util(token = "else", kind = "keyword")]
    Else,
    #[cfg_attr(feature = "lexer-v1", token("for"))]
    #[lang_util(token = "for", kind = "keyword")]
    For,
    #[cfg_attr(feature = "lexer-v1", token("if"))]
    #[lang_util(token = "if", kind = "keyword")]
    If,
    #[cfg_attr(feature = "lexer-v1", token("discard"))]
    #[lang_util(token = "discard", kind = "keyword")]
    Discard,
    #[cfg_attr(feature = "lexer-v1", token("return"))]
    #[lang_util(token = "return", kind = "keyword")]
    Return,
    #[cfg_attr(feature = "lexer-v1", token("switch"))]
    #[lang_util(token = "switch", kind = "keyword")]
    Switch,
    #[cfg_attr(feature = "lexer-v1", token("case"))]
    #[lang_util(token = "case", kind = "keyword")]
    Case,
    #[cfg_attr(feature = "lexer-v1", token("default"))]
    #[lang_util(token = "default", kind = "keyword")]
    Default,
    #[cfg_attr(feature = "lexer-v1", token("subroutine"))]
    #[lang_util(
        token = "subroutine",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    Subroutine,
    #[cfg_attr(feature = "lexer-v1", regex("[a-zA-Z_][a-zA-Z_0-9]*", parse_ident))]
    #[cfg_attr(
        feature = "lexer-v1",
        regex("#\\s*\\(\\s*[a-zA-Z_][a-zA-Z_0-9]*\\s*\\)", parse_rs_ident)
    )]
    #[lang_util(as = "ident", kind = "identifier")]
    Identifier(SmolStr),
    #[lang_util(as = "ty_name", kind = "type name")]
    TypeName(SmolStr), // Cast from Identifier depending on known type names
    #[cfg_attr(
        feature = "lexer-v1",
        regex(
            r"([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)([eE][+-]?[0-9]+)?(f|F)?",
            parse_f32
        )
    )]
    #[cfg_attr(feature = "lexer-v1", regex(r"[0-9]+[eE][+-]?[0-9]+(f|F)?", parse_f32))]
    #[lang_util(as = "float_constant", kind = "literal")]
    FloatConstant(f32),
    #[cfg_attr(feature = "lexer-v1", regex(r"0[0-7]*", |lex| parse_int(lex, 8)))]
    #[cfg_attr(feature = "lexer-v1", regex(r"[1-9][0-9]*", |lex| parse_int(lex, 10)))]
    #[cfg_attr(feature = "lexer-v1", regex(r"0[xX][0-9A-Fa-f]+", |lex| parse_int(lex, 16)))]
    #[lang_util(as = "int_constant", kind = "literal")]
    IntConstant(i32),
    #[cfg_attr(feature = "lexer-v1", regex(r"0[0-7]*[uU]", |lex| parse_uint(lex, 8)))]
    #[cfg_attr(feature = "lexer-v1", regex(r"[1-9][0-9]*[uU]", |lex| parse_uint(lex, 10)))]
    #[cfg_attr(feature = "lexer-v1", regex(r"0[xX][0-9A-Fa-f]+[uU]", |lex| parse_uint(lex, 16)))]
    #[lang_util(as = "uint_constant", kind = "literal")]
    UIntConstant(u32),
    #[cfg_attr(feature = "lexer-v1", token("true", |_| true))]
    #[cfg_attr(feature = "lexer-v1", token("false", |_| false))]
    #[lang_util(as = "bool_constant", kind = "literal")]
    BoolConstant(bool),
    #[cfg_attr(
        feature = "lexer-v1",
        regex(
            r"([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)([eE][+-]?[0-9]+)?(lf|LF)",
            parse_f64
        )
    )]
    #[cfg_attr(
        feature = "lexer-v1",
        regex(r"[0-9]+[eE][+-]?[0-9]+(lf|LF)", parse_f64)
    )]
    #[lang_util(as = "double_constant", kind = "literal")]
    DoubleConstant(f64),
    #[cfg_attr(feature = "lexer-v1", token("<<"))]
    #[lang_util(token = "<<", kind = "binary operator", kind = "operator")]
    LeftOp,
    #[cfg_attr(feature = "lexer-v1", token(">>"))]
    #[lang_util(token = ">>", kind = "binary operator", kind = "operator")]
    RightOp,
    #[cfg_attr(feature = "lexer-v1", token("++"))]
    #[lang_util(token = "++", kind = "unary operator", kind = "operator")]
    IncOp,
    #[cfg_attr(feature = "lexer-v1", token("--"))]
    #[lang_util(token = "--", kind = "unary operator", kind = "operator")]
    DecOp,
    #[cfg_attr(feature = "lexer-v1", token("<="))]
    #[lang_util(token = "<=", kind = "binary operator", kind = "operator")]
    LeOp,
    #[cfg_attr(feature = "lexer-v1", token(">="))]
    #[lang_util(token = ">=", kind = "binary operator", kind = "operator")]
    GeOp,
    #[cfg_attr(feature = "lexer-v1", token("=="))]
    #[lang_util(token = "==", kind = "binary operator", kind = "operator")]
    EqOp,
    #[cfg_attr(feature = "lexer-v1", token("!="))]
    #[lang_util(token = "!=", kind = "binary operator", kind = "operator")]
    NeOp,
    #[cfg_attr(feature = "lexer-v1", token("&&"))]
    #[lang_util(token = "&&", kind = "binary operator", kind = "operator")]
    AndOp,
    #[cfg_attr(feature = "lexer-v1", token("||"))]
    #[lang_util(token = "||", kind = "binary operator", kind = "operator")]
    OrOp,
    #[cfg_attr(feature = "lexer-v1", token("^^"))]
    #[lang_util(token = "^^", kind = "binary operator", kind = "operator")]
    XorOp,
    #[cfg_attr(feature = "lexer-v1", token("*="))]
    #[lang_util(token = "*=", kind = "binary operator", kind = "operator")]
    MulAssign,
    #[cfg_attr(feature = "lexer-v1", token("/="))]
    #[lang_util(token = "/=", kind = "binary operator", kind = "operator")]
    DivAssign,
    #[cfg_attr(feature = "lexer-v1", token("+="))]
    #[lang_util(token = "+=", kind = "binary operator", kind = "operator")]
    AddAssign,
    #[cfg_attr(feature = "lexer-v1", token("%="))]
    #[lang_util(token = "%=", kind = "binary operator", kind = "operator")]
    ModAssign,
    #[cfg_attr(feature = "lexer-v1", token("<<="))]
    #[lang_util(token = "<<=", kind = "binary operator", kind = "operator")]
    LeftAssign,
    #[cfg_attr(feature = "lexer-v1", token(">>="))]
    #[lang_util(token = ">>=", kind = "binary operator", kind = "operator")]
    RightAssign,
    #[cfg_attr(feature = "lexer-v1", token("&="))]
    #[lang_util(token = "&=", kind = "binary operator", kind = "operator")]
    AndAssign,
    #[cfg_attr(feature = "lexer-v1", token("^="))]
    #[lang_util(token = "^=", kind = "binary operator", kind = "operator")]
    XorAssign,
    #[cfg_attr(feature = "lexer-v1", token("|="))]
    #[lang_util(token = "|=", kind = "binary operator", kind = "operator")]
    OrAssign,
    #[cfg_attr(feature = "lexer-v1", token("-="))]
    #[lang_util(token = "-=", kind = "binary operator", kind = "operator")]
    SubAssign,
    #[cfg_attr(feature = "lexer-v1", token("("))]
    #[lang_util(token = "(")]
    LeftParen,
    #[cfg_attr(feature = "lexer-v1", token(")"))]
    #[lang_util(token = ")")]
    RightParen,
    #[cfg_attr(feature = "lexer-v1", token("["))]
    #[lang_util(token = "[")]
    LeftBracket,
    #[cfg_attr(feature = "lexer-v1", token("]"))]
    #[lang_util(token = "]")]
    RightBracket,
    #[cfg_attr(feature = "lexer-v1", token("{"))]
    #[lang_util(token = "{")]
    LeftBrace,
    #[cfg_attr(feature = "lexer-v1", token("}"))]
    #[lang_util(token = "}")]
    RightBrace,
    #[cfg_attr(feature = "lexer-v1", token("."))]
    #[lang_util(token = ".", kind = "binary operator", kind = "operator")]
    Dot,
    #[cfg_attr(feature = "lexer-v1", token(","))]
    #[lang_util(token = ",", kind = "operator")]
    Comma,
    #[cfg_attr(feature = "lexer-v1", token(":"))]
    #[lang_util(token = ":", kind = "operator")]
    Colon,
    #[cfg_attr(feature = "lexer-v1", token("="))]
    #[lang_util(token = "=", kind = "binary operator", kind = "operator")]
    Equal,
    #[cfg_attr(feature = "lexer-v1", token(";"))]
    #[lang_util(token = ";")]
    Semicolon,
    #[cfg_attr(feature = "lexer-v1", token("!"))]
    #[lang_util(token = "!", kind = "unary operator", kind = "operator")]
    Bang,
    #[cfg_attr(feature = "lexer-v1", token("-"))]
    #[lang_util(
        token = "-",
        kind = "binary operator",
        kind = "unary operator",
        kind = "operator"
    )]
    Dash,
    #[cfg_attr(feature = "lexer-v1", token("~"))]
    #[lang_util(token = "~", kind = "unary operator", kind = "operator")]
    Tilde,
    #[cfg_attr(feature = "lexer-v1", token("+"))]
    #[lang_util(
        token = "+",
        kind = "binary operator",
        kind = "unary operator",
        kind = "operator"
    )]
    Plus,
    #[cfg_attr(feature = "lexer-v1", token("*"))]
    #[lang_util(token = "*", kind = "binary operator", kind = "operator")]
    Star,
    #[cfg_attr(feature = "lexer-v1", token("/"))]
    #[lang_util(token = "/", kind = "binary operator", kind = "operator")]
    Slash,
    #[cfg_attr(feature = "lexer-v1", token("%"))]
    #[lang_util(token = "%", kind = "binary operator", kind = "operator")]
    Percent,
    #[cfg_attr(feature = "lexer-v1", token("<"))]
    #[lang_util(token = "<", kind = "binary operator", kind = "operator")]
    LeftAngle,
    #[cfg_attr(feature = "lexer-v1", token(">"))]
    #[lang_util(token = ">", kind = "binary operator", kind = "operator")]
    RightAngle,
    #[cfg_attr(feature = "lexer-v1", token("|"))]
    #[lang_util(token = "|", kind = "binary operator", kind = "operator")]
    VerticalBar,
    #[cfg_attr(feature = "lexer-v1", token("^"))]
    #[lang_util(token = "^", kind = "binary operator", kind = "operator")]
    Caret,
    #[cfg_attr(feature = "lexer-v1", token("&"))]
    #[lang_util(token = "&", kind = "binary operator", kind = "operator")]
    Ampersand,
    #[cfg_attr(feature = "lexer-v1", token("?"))]
    #[lang_util(token = "?", kind = "operator")]
    Question,
    #[cfg_attr(feature = "lexer-v1", token("invariant"))]
    #[lang_util(token = "invariant")]
    Invariant,
    #[cfg_attr(feature = "lexer-v1", token("precise"))]
    #[lang_util(token = "precise", kind = "type qualifier")]
    Precise,
    #[cfg_attr(feature = "lexer-v1", token("highp"))]
    #[lang_util(token = "highp", kind = "precision qualifier", kind = "type qualifier")]
    HighPrecision,
    #[cfg_attr(feature = "lexer-v1", token("mediump"))]
    #[lang_util(
        token = "mediump",
        kind = "precision qualifier",
        kind = "type qualifier"
    )]
    MediumPrecision,
    #[cfg_attr(feature = "lexer-v1", token("lowp"))]
    #[lang_util(token = "lowp", kind = "precision qualifier", kind = "type qualifier")]
    LowPrecision,
    #[cfg_attr(feature = "lexer-v1", token("precision"))]
    #[lang_util(token = "precision")]
    Precision,

    // TODO: Line continuation can happen inside tokens
    #[cfg_attr(feature = "lexer-v1", regex("([ \t\r\n]|\\\\\r?\n)+", logos::skip))]
    #[lang_util(display = "<whitespace>", as(display), kind = "trivia")]
    Whitespace,
    #[cfg_attr(feature = "lexer-v1", regex("//(.|\\\\\r?\n)*", |lex| { parse_cmt(lex, true); logos::Skip }))]
    #[lang_util(display = "<single line comment>", as(display), kind = "trivia")]
    SingleLineComment,
    #[cfg_attr(feature = "lexer-v1", regex("/\\*([^*]|\\*[^/])+\\*/", |lex| { parse_cmt(lex, false); logos::Skip }))]
    #[lang_util(display = "<multi line comment>", as(display), kind = "trivia")]
    MultiLineComment,

    // TODO: Line continuations in preprocessor pragmas?
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*define"))]
    #[lang_util(display = "#define", as(display), kind = "preprocessor directive")]
    PpDefine,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*else"))]
    #[lang_util(display = "#else", as(display), kind = "preprocessor directive")]
    PpElse,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*elif"))]
    #[lang_util(display = "#elif", as(display), kind = "preprocessor directive")]
    PpElif,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*endif"))]
    #[lang_util(display = "#endif", as(display), kind = "preprocessor directive")]
    PpEndIf,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*error"))]
    #[lang_util(display = "#error", as(display), kind = "preprocessor directive")]
    PpError,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*if"))]
    #[lang_util(display = "#if", as(display), kind = "preprocessor directive")]
    PpIf,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*ifdef"))]
    #[lang_util(display = "#ifdef", as(display), kind = "preprocessor directive")]
    PpIfDef,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*ifndef"))]
    #[lang_util(display = "#ifndef", as(display), kind = "preprocessor directive")]
    PpIfNDef,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*include"))]
    #[lang_util(display = "#include", as(display), kind = "preprocessor directive")]
    PpInclude,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*line"))]
    #[lang_util(display = "#line", as(display), kind = "preprocessor directive")]
    PpLine,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*pragma"))]
    #[lang_util(display = "#pragma", as(display), kind = "preprocessor directive")]
    PpPragma,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*undef"))]
    #[lang_util(display = "#undef", as(display), kind = "preprocessor directive")]
    PpUndef,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*version"))]
    #[lang_util(display = "#version", as(display), kind = "preprocessor directive")]
    PpVersion,
    #[cfg_attr(feature = "lexer-v1", regex("#([ \t]|\\\\\r?\n)*extension"))]
    #[lang_util(display = "#extension", as(display), kind = "preprocessor directive")]
    PpExtension,

    #[lang_util(
        display = "<preprocessor string>",
        as(display),
        kind = "preprocessor string"
    )]
    PpRest(String),

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
    PpPathAbsolute(String),
    #[lang_util(display = "\"{}\"", as(display), kind = "include path")]
    PpPathRelative(String),

    #[cfg_attr(feature = "lexer-v1", error)]
    #[lang_util(display = "<invalid token>", as(display), kind = "error")]
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
            _ => panic!("cannot convert token {:?}, to str", self),
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
            other => panic!("cannot convert {:?} into String", other),
        }
    }
}
