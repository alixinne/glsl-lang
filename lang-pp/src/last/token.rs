use smol_str::SmolStr;

use crate::{parser::SyntaxKind, processor::event::TokenLike, Unescaped};

use super::{keywords::KeywordAtom, type_names::TypeNameAtom, TypeNameState};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display)]
#[allow(non_camel_case_types)]
pub enum TypeName {
    /// "void"
    #[display(fmt = "void")]
    VOID,
    /// "int"
    #[display(fmt = "int")]
    INT,
    /// "bool"
    #[display(fmt = "bool")]
    BOOL,
    /// "float"
    #[display(fmt = "float")]
    FLOAT,
    /// "double"
    #[display(fmt = "double")]
    DOUBLE,
    /// "vec2"
    #[display(fmt = "vec2")]
    VEC2,
    /// "vec3"
    #[display(fmt = "vec3")]
    VEC3,
    /// "vec4"
    #[display(fmt = "vec4")]
    VEC4,
    /// "ivec2"
    #[display(fmt = "ivec2")]
    IVEC2,
    /// "ivec3"
    #[display(fmt = "ivec3")]
    IVEC3,
    /// "ivec4"
    #[display(fmt = "ivec4")]
    IVEC4,
    /// "bvec2"
    #[display(fmt = "bvec2")]
    BVEC2,
    /// "bvec3"
    #[display(fmt = "bvec3")]
    BVEC3,
    /// "bvec4"
    #[display(fmt = "bvec4")]
    BVEC4,
    /// "uint"
    #[display(fmt = "uint")]
    UINT,
    /// "atomic_uint"
    #[display(fmt = "atomic_uint")]
    ATOMIC_UINT,
    /// "uvec2"
    #[display(fmt = "uvec2")]
    UVEC2,
    /// "uvec3"
    #[display(fmt = "uvec3")]
    UVEC3,
    /// "uvec4"
    #[display(fmt = "uvec4")]
    UVEC4,
    /// "dvec2"
    #[display(fmt = "dvec2")]
    DVEC2,
    /// "dvec3"
    #[display(fmt = "dvec3")]
    DVEC3,
    /// "dvec4"
    #[display(fmt = "dvec4")]
    DVEC4,
    /// "mat2"
    #[display(fmt = "mat2")]
    MAT2,
    /// "mat3"
    #[display(fmt = "mat3")]
    MAT3,
    /// "mat4"
    #[display(fmt = "mat4")]
    MAT4,
    /// "mat2x2"
    #[display(fmt = "mat2x2")]
    MAT2X2,
    /// "mat2x3"
    #[display(fmt = "mat2x3")]
    MAT2X3,
    /// "mat2x4"
    #[display(fmt = "mat2x4")]
    MAT2X4,
    /// "mat3x2"
    #[display(fmt = "mat3x2")]
    MAT3X2,
    /// "mat3x3"
    #[display(fmt = "mat3x3")]
    MAT3X3,
    /// "mat3x4"
    #[display(fmt = "mat3x4")]
    MAT3X4,
    /// "mat4x2"
    #[display(fmt = "mat4x2")]
    MAT4X2,
    /// "mat4x3"
    #[display(fmt = "mat4x3")]
    MAT4X3,
    /// "mat4x4"
    #[display(fmt = "mat4x4")]
    MAT4X4,
    /// "dmat2"
    #[display(fmt = "dmat2")]
    DMAT2,
    /// "dmat3"
    #[display(fmt = "dmat3")]
    DMAT3,
    /// "dmat4"
    #[display(fmt = "dmat4")]
    DMAT4,
    /// "dmat2x2"
    #[display(fmt = "dmat2x2")]
    DMAT2X2,
    /// "dmat2x3"
    #[display(fmt = "dmat2x3")]
    DMAT2X3,
    /// "dmat2x4"
    #[display(fmt = "dmat2x4")]
    DMAT2X4,
    /// "dmat3x2"
    #[display(fmt = "dmat3x2")]
    DMAT3X2,
    /// "dmat3x3"
    #[display(fmt = "dmat3x3")]
    DMAT3X3,
    /// "dmat3x4"
    #[display(fmt = "dmat3x4")]
    DMAT3X4,
    /// "dmat4x2"
    #[display(fmt = "dmat4x2")]
    DMAT4X2,
    /// "dmat4x3"
    #[display(fmt = "dmat4x3")]
    DMAT4X3,
    /// "dmat4x4"
    #[display(fmt = "dmat4x4")]
    DMAT4X4,
    /// "sampler1D"
    #[display(fmt = "sampler1D")]
    SAMPLER1D,
    /// "sampler1DShadow"
    #[display(fmt = "sampler1DShadow")]
    SAMPLER1DSHADOW,
    /// "sampler1DArray"
    #[display(fmt = "sampler1DArray")]
    SAMPLER1DARRAY,
    /// "sampler1DArrayShadow"
    #[display(fmt = "sampler1DArrayShadow")]
    SAMPLER1DARRAYSHADOW,
    /// "isampler1D"
    #[display(fmt = "isampler1D")]
    ISAMPLER1D,
    /// "isampler1DArray"
    #[display(fmt = "isampler1DArray")]
    ISAMPLER1DARRAY,
    /// "usampler1D"
    #[display(fmt = "usampler1D")]
    USAMPLER1D,
    /// "usampler1DArray"
    #[display(fmt = "usampler1DArray")]
    USAMPLER1DARRAY,
    /// "sampler2D"
    #[display(fmt = "sampler2D")]
    SAMPLER2D,
    /// "sampler2DShadow"
    #[display(fmt = "sampler2DShadow")]
    SAMPLER2DSHADOW,
    /// "sampler2DArray"
    #[display(fmt = "sampler2DArray")]
    SAMPLER2DARRAY,
    /// "sampler2DArrayShadow"
    #[display(fmt = "sampler2DArrayShadow")]
    SAMPLER2DARRAYSHADOW,
    /// "isampler2D"
    #[display(fmt = "isampler2D")]
    ISAMPLER2D,
    /// "isampler2DArray"
    #[display(fmt = "isampler2DArray")]
    ISAMPLER2DARRAY,
    /// "usampler2D"
    #[display(fmt = "usampler2D")]
    USAMPLER2D,
    /// "usampler2DArray"
    #[display(fmt = "usampler2DArray")]
    USAMPLER2DARRAY,
    /// "sampler2DRect"
    #[display(fmt = "sampler2DRect")]
    SAMPLER2DRECT,
    /// "sampler2DRectShadow"
    #[display(fmt = "sampler2DRectShadow")]
    SAMPLER2DRECTSHADOW,
    /// "isampler2DRect"
    #[display(fmt = "isampler2DRect")]
    ISAMPLER2DRECT,
    /// "usampler2DRect"
    #[display(fmt = "usampler2DRect")]
    USAMPLER2DRECT,
    /// "sampler2DMS"
    #[display(fmt = "sampler2DMS")]
    SAMPLER2DMS,
    /// "isampler2DMS"
    #[display(fmt = "isampler2DMS")]
    ISAMPLER2DMS,
    /// "usampler2DMS"
    #[display(fmt = "usampler2DMS")]
    USAMPLER2DMS,
    /// "sampler2DMSArray"
    #[display(fmt = "sampler2DMSArray")]
    SAMPLER2DMSARRAY,
    /// "isampler2DMSArray"
    #[display(fmt = "isampler2DMSArray")]
    ISAMPLER2DMSARRAY,
    /// "usampler2DMSArray"
    #[display(fmt = "usampler2DMSArray")]
    USAMPLER2DMSARRAY,
    /// "sampler3D"
    #[display(fmt = "sampler3D")]
    SAMPLER3D,
    /// "isampler3D"
    #[display(fmt = "isampler3D")]
    ISAMPLER3D,
    /// "usampler3D"
    #[display(fmt = "usampler3D")]
    USAMPLER3D,
    /// "samplerCube"
    #[display(fmt = "samplerCube")]
    SAMPLERCUBE,
    /// "samplerCubeShadow"
    #[display(fmt = "samplerCubeShadow")]
    SAMPLERCUBESHADOW,
    /// "isamplerCube"
    #[display(fmt = "isamplerCube")]
    ISAMPLERCUBE,
    /// "usamplerCube"
    #[display(fmt = "usamplerCube")]
    USAMPLERCUBE,
    /// "samplerCubeArray"
    #[display(fmt = "samplerCubeArray")]
    SAMPLERCUBEARRAY,
    /// "samplerCubeArrayShadow"
    #[display(fmt = "samplerCubeArrayShadow")]
    SAMPLERCUBEARRAYSHADOW,
    /// "isamplerCubeArray"
    #[display(fmt = "isamplerCubeArray")]
    ISAMPLERCUBEARRAY,
    /// "usamplerCubeArray"
    #[display(fmt = "usamplerCubeArray")]
    USAMPLERCUBEARRAY,
    /// "samplerBuffer"
    #[display(fmt = "samplerBuffer")]
    SAMPLERBUFFER,
    /// "isamplerBuffer"
    #[display(fmt = "isamplerBuffer")]
    ISAMPLERBUFFER,
    /// "usamplerBuffer"
    #[display(fmt = "usamplerBuffer")]
    USAMPLERBUFFER,
    /// "image1D"
    #[display(fmt = "image1D")]
    IMAGE1D,
    /// "iimage1D"
    #[display(fmt = "iimage1D")]
    IIMAGE1D,
    /// "uimage1D"
    #[display(fmt = "uimage1D")]
    UIMAGE1D,
    /// "image1DArray"
    #[display(fmt = "image1DArray")]
    IMAGE1DARRAY,
    /// "iimage1DArray"
    #[display(fmt = "iimage1DArray")]
    IIMAGE1DARRAY,
    /// "uimage1DArray"
    #[display(fmt = "uimage1DArray")]
    UIMAGE1DARRAY,
    /// "image2D"
    #[display(fmt = "image2D")]
    IMAGE2D,
    /// "iimage2D"
    #[display(fmt = "iimage2D")]
    IIMAGE2D,
    /// "uimage2D"
    #[display(fmt = "uimage2D")]
    UIMAGE2D,
    /// "image2DArray"
    #[display(fmt = "image2DArray")]
    IMAGE2DARRAY,
    /// "iimage2DArray"
    #[display(fmt = "iimage2DArray")]
    IIMAGE2DARRAY,
    /// "uimage2DArray"
    #[display(fmt = "uimage2DArray")]
    UIMAGE2DARRAY,
    /// "image2DRect"
    #[display(fmt = "image2DRect")]
    IMAGE2DRECT,
    /// "iimage2DRect"
    #[display(fmt = "iimage2DRect")]
    IIMAGE2DRECT,
    /// "uimage2DRect"
    #[display(fmt = "uimage2DRect")]
    UIMAGE2DRECT,
    /// "image2DMS"
    #[display(fmt = "image2DMS")]
    IMAGE2DMS,
    /// "iimage2DMS"
    #[display(fmt = "iimage2DMS")]
    IIMAGE2DMS,
    /// "uimage2DMS"
    #[display(fmt = "uimage2DMS")]
    UIMAGE2DMS,
    /// "image2DMSArray"
    #[display(fmt = "image2DMSArray")]
    IMAGE2DMSARRAY,
    /// "iimage2DMSArray"
    #[display(fmt = "iimage2DMSArray")]
    IIMAGE2DMSARRAY,
    /// "uimage2DMSArray"
    #[display(fmt = "uimage2DMSArray")]
    UIMAGE2DMSARRAY,
    /// "image3D"
    #[display(fmt = "image3D")]
    IMAGE3D,
    /// "iimage3D"
    #[display(fmt = "iimage3D")]
    IIMAGE3D,
    /// "uimage3D"
    #[display(fmt = "uimage3D")]
    UIMAGE3D,
    /// "imageCube"
    #[display(fmt = "imageCube")]
    IMAGECUBE,
    /// "iimageCube"
    #[display(fmt = "iimageCube")]
    IIMAGECUBE,
    /// "uimageCube"
    #[display(fmt = "uimageCube")]
    UIMAGECUBE,
    /// "imageCubeArray"
    #[display(fmt = "imageCubeArray")]
    IMAGECUBEARRAY,
    /// "iimageCubeArray"
    #[display(fmt = "iimageCubeArray")]
    IIMAGECUBEARRAY,
    /// "uimageCubeArray"
    #[display(fmt = "uimageCubeArray")]
    UIMAGECUBEARRAY,
    /// "imageBuffer"
    #[display(fmt = "imageBuffer")]
    IMAGEBUFFER,
    /// "iimageBuffer"
    #[display(fmt = "iimageBuffer")]
    IIMAGEBUFFER,
    /// "uimageBuffer"
    #[display(fmt = "uimageBuffer")]
    UIMAGEBUFFER,
    // Vulkan type names
    /// "texture1D"
    #[display(fmt = "texture1D")]
    TEXTURE1D,
    /// "texture1DArray"
    #[display(fmt = "texture1DArray")]
    TEXTURE1DARRAY,
    /// "itexture1D"
    #[display(fmt = "itexture1D")]
    ITEXTURE1D,
    /// "itexture1DArray"
    #[display(fmt = "itexture1DArray")]
    ITEXTURE1DARRAY,
    /// "utexture1D"
    #[display(fmt = "utexture1D")]
    UTEXTURE1D,
    /// "utexture1DArray"
    #[display(fmt = "utexture1DArray")]
    UTEXTURE1DARRAY,
    /// "texture2D"
    #[display(fmt = "texture2D")]
    TEXTURE2D,
    /// "texture2DArray"
    #[display(fmt = "texture2DArray")]
    TEXTURE2DARRAY,
    /// "itexture2D"
    #[display(fmt = "itexture2D")]
    ITEXTURE2D,
    /// "itexture2DArray"
    #[display(fmt = "itexture2DArray")]
    ITEXTURE2DARRAY,
    /// "utexture2D"
    #[display(fmt = "utexture2D")]
    UTEXTURE2D,
    /// "utexture2DArray"
    #[display(fmt = "utexture2DArray")]
    UTEXTURE2DARRAY,
    /// "texture2DRect"
    #[display(fmt = "texture2DRect")]
    TEXTURE2DRECT,
    /// "itexture2DRect"
    #[display(fmt = "itexture2DRect")]
    ITEXTURE2DRECT,
    /// "utexture2DRect"
    #[display(fmt = "utexture2DRect")]
    UTEXTURE2DRECT,
    /// "texture2DMS"
    #[display(fmt = "texture2DMS")]
    TEXTURE2DMS,
    /// "itexture2DMS"
    #[display(fmt = "itexture2DMS")]
    ITEXTURE2DMS,
    /// "utexture2DMS"
    #[display(fmt = "utexture2DMS")]
    UTEXTURE2DMS,
    /// "texture2DMSArray"
    #[display(fmt = "texture2DMSArray")]
    TEXTURE2DMSARRAY,
    /// "itexture2DMSArray"
    #[display(fmt = "itexture2DMSArray")]
    ITEXTURE2DMSARRAY,
    /// "utexture2DMSArray"
    #[display(fmt = "utexture2DMSArray")]
    UTEXTURE2DMSARRAY,
    /// "texture3D"
    #[display(fmt = "texture3D")]
    TEXTURE3D,
    /// "itexture3D"
    #[display(fmt = "itexture3D")]
    ITEXTURE3D,
    /// "utexture3D"
    #[display(fmt = "utexture3D")]
    UTEXTURE3D,
    /// "textureCube"
    #[display(fmt = "textureCube")]
    TEXTURECUBE,
    /// "itextureCube"
    #[display(fmt = "itextureCube")]
    ITEXTURECUBE,
    /// "utextureCube"
    #[display(fmt = "utextureCube")]
    UTEXTURECUBE,
    /// "textureCubeArray"
    #[display(fmt = "textureCubeArray")]
    TEXTURECUBEARRAY,
    /// "itextureCubeArray"
    #[display(fmt = "itextureCubeArray")]
    ITEXTURECUBEARRAY,
    /// "utextureCubeArray"
    #[display(fmt = "utextureCubeArray")]
    UTEXTURECUBEARRAY,
    /// "textureBuffer"
    #[display(fmt = "textureBuffer")]
    TEXTUREBUFFER,
    /// "itextureBuffer"
    #[display(fmt = "itextureBuffer")]
    ITEXTUREBUFFER,
    /// "utextureBuffer"
    #[display(fmt = "utextureBuffer")]
    UTEXTUREBUFFER,
    /// "sampler"
    #[display(fmt = "sampler")]
    SAMPLER,
    /// "samplerShadow"
    #[display(fmt = "samplerShadow")]
    SAMPLERSHADOW,
    /// "subpassInput"
    #[display(fmt = "subpassInput")]
    SUBPASSINPUT,
    /// "isubpassInput"
    #[display(fmt = "isubpassInput")]
    ISUBPASSINPUT,
    /// "usubpassInput"
    #[display(fmt = "usubpassInput")]
    USUBPASSINPUT,
    /// "subpassInputMS"
    #[display(fmt = "subpassInputMS")]
    SUBPASSINPUTMS,
    /// "isubpassInputMS"
    #[display(fmt = "isubpassInputMS")]
    ISUBPASSINPUTMS,
    /// "usubpassInputMS"
    #[display(fmt = "usubpassInputMS")]
    USUBPASSINPUTMS,
    // Reserved for future use
    /// "hvec2"
    #[display(fmt = "hvec2")]
    HVEC2,
    /// "hvec3"
    #[display(fmt = "hvec3")]
    HVEC3,
    /// "hvec4"
    #[display(fmt = "hvec4")]
    HVEC4,
    /// "fvec2"
    #[display(fmt = "fvec2")]
    FVEC2,
    /// "fvec3"
    #[display(fmt = "fvec3")]
    FVEC3,
    /// "fvec4"
    #[display(fmt = "fvec4")]
    FVEC4,
    /// "sampler3DRect"
    #[display(fmt = "sampler3DRect")]
    SAMPLER3DRECT,
    /// Generic type name
    OTHER(TypeNameAtom),
}

impl TypeName {
    fn parse(
        name: &str,
        target_vulkan: bool,
        is_type_name: impl Fn(&TypeNameAtom) -> TypeNameState,
    ) -> Option<(Self, Option<TypeNameState>)> {
        use TypeName::*;

        let type_name_atom = TypeNameAtom::from(name);

        if type_name_atom == type_name!("void") {
            return Some((VOID, None));
        } else if type_name_atom == type_name!("int") {
            return Some((INT, None));
        } else if type_name_atom == type_name!("bool") {
            return Some((BOOL, None));
        } else if type_name_atom == type_name!("float") {
            return Some((FLOAT, None));
        } else if type_name_atom == type_name!("double") {
            return Some((DOUBLE, None));
        } else if type_name_atom == type_name!("vec2") {
            return Some((VEC2, None));
        } else if type_name_atom == type_name!("vec3") {
            return Some((VEC3, None));
        } else if type_name_atom == type_name!("vec4") {
            return Some((VEC4, None));
        } else if type_name_atom == type_name!("ivec2") {
            return Some((IVEC2, None));
        } else if type_name_atom == type_name!("ivec3") {
            return Some((IVEC3, None));
        } else if type_name_atom == type_name!("ivec4") {
            return Some((IVEC4, None));
        } else if type_name_atom == type_name!("bvec2") {
            return Some((BVEC2, None));
        } else if type_name_atom == type_name!("bvec3") {
            return Some((BVEC3, None));
        } else if type_name_atom == type_name!("bvec4") {
            return Some((BVEC4, None));
        } else if type_name_atom == type_name!("uint") {
            return Some((UINT, None));
        } else if type_name_atom == type_name!("atomic_uint") {
            return Some((ATOMIC_UINT, None));
        } else if type_name_atom == type_name!("uvec2") {
            return Some((UVEC2, None));
        } else if type_name_atom == type_name!("uvec3") {
            return Some((UVEC3, None));
        } else if type_name_atom == type_name!("uvec4") {
            return Some((UVEC4, None));
        } else if type_name_atom == type_name!("dvec2") {
            return Some((DVEC2, None));
        } else if type_name_atom == type_name!("dvec3") {
            return Some((DVEC3, None));
        } else if type_name_atom == type_name!("dvec4") {
            return Some((DVEC4, None));
        } else if type_name_atom == type_name!("mat2") {
            return Some((MAT2, None));
        } else if type_name_atom == type_name!("mat3") {
            return Some((MAT3, None));
        } else if type_name_atom == type_name!("mat4") {
            return Some((MAT4, None));
        } else if type_name_atom == type_name!("mat2x2") {
            return Some((MAT2X2, None));
        } else if type_name_atom == type_name!("mat2x3") {
            return Some((MAT2X3, None));
        } else if type_name_atom == type_name!("mat2x4") {
            return Some((MAT2X4, None));
        } else if type_name_atom == type_name!("mat3x2") {
            return Some((MAT3X2, None));
        } else if type_name_atom == type_name!("mat3x3") {
            return Some((MAT3X3, None));
        } else if type_name_atom == type_name!("mat3x4") {
            return Some((MAT3X4, None));
        } else if type_name_atom == type_name!("mat4x2") {
            return Some((MAT4X2, None));
        } else if type_name_atom == type_name!("mat4x3") {
            return Some((MAT4X3, None));
        } else if type_name_atom == type_name!("mat4x4") {
            return Some((MAT4X4, None));
        } else if type_name_atom == type_name!("dmat2") {
            return Some((DMAT2, None));
        } else if type_name_atom == type_name!("dmat3") {
            return Some((DMAT3, None));
        } else if type_name_atom == type_name!("dmat4") {
            return Some((DMAT4, None));
        } else if type_name_atom == type_name!("dmat2x2") {
            return Some((DMAT2X2, None));
        } else if type_name_atom == type_name!("dmat2x3") {
            return Some((DMAT2X3, None));
        } else if type_name_atom == type_name!("dmat2x4") {
            return Some((DMAT2X4, None));
        } else if type_name_atom == type_name!("dmat3x2") {
            return Some((DMAT3X2, None));
        } else if type_name_atom == type_name!("dmat3x3") {
            return Some((DMAT3X3, None));
        } else if type_name_atom == type_name!("dmat3x4") {
            return Some((DMAT3X4, None));
        } else if type_name_atom == type_name!("dmat4x2") {
            return Some((DMAT4X2, None));
        } else if type_name_atom == type_name!("dmat4x3") {
            return Some((DMAT4X3, None));
        } else if type_name_atom == type_name!("dmat4x4") {
            return Some((DMAT4X4, None));
        } else if type_name_atom == type_name!("sampler1D") {
            return Some((SAMPLER1D, None));
        } else if type_name_atom == type_name!("sampler1DShadow") {
            return Some((SAMPLER1DSHADOW, None));
        } else if type_name_atom == type_name!("sampler1DArray") {
            return Some((SAMPLER1DARRAY, None));
        } else if type_name_atom == type_name!("sampler1DArrayShadow") {
            return Some((SAMPLER1DARRAYSHADOW, None));
        } else if type_name_atom == type_name!("isampler1D") {
            return Some((ISAMPLER1D, None));
        } else if type_name_atom == type_name!("isampler1DArray") {
            return Some((ISAMPLER1DARRAY, None));
        } else if type_name_atom == type_name!("usampler1D") {
            return Some((USAMPLER1D, None));
        } else if type_name_atom == type_name!("usampler1DArray") {
            return Some((USAMPLER1DARRAY, None));
        } else if type_name_atom == type_name!("sampler2D") {
            return Some((SAMPLER2D, None));
        } else if type_name_atom == type_name!("sampler2DShadow") {
            return Some((SAMPLER2DSHADOW, None));
        } else if type_name_atom == type_name!("sampler2DArray") {
            return Some((SAMPLER2DARRAY, None));
        } else if type_name_atom == type_name!("sampler2DArrayShadow") {
            return Some((SAMPLER2DARRAYSHADOW, None));
        } else if type_name_atom == type_name!("isampler2D") {
            return Some((ISAMPLER2D, None));
        } else if type_name_atom == type_name!("isampler2DArray") {
            return Some((ISAMPLER2DARRAY, None));
        } else if type_name_atom == type_name!("usampler2D") {
            return Some((USAMPLER2D, None));
        } else if type_name_atom == type_name!("usampler2DArray") {
            return Some((USAMPLER2DARRAY, None));
        } else if type_name_atom == type_name!("sampler2DRect") {
            return Some((SAMPLER2DRECT, None));
        } else if type_name_atom == type_name!("sampler2DRectShadow") {
            return Some((SAMPLER2DRECTSHADOW, None));
        } else if type_name_atom == type_name!("isampler2DRect") {
            return Some((ISAMPLER2DRECT, None));
        } else if type_name_atom == type_name!("usampler2DRect") {
            return Some((USAMPLER2DRECT, None));
        } else if type_name_atom == type_name!("sampler2DMS") {
            return Some((SAMPLER2DMS, None));
        } else if type_name_atom == type_name!("isampler2DMS") {
            return Some((ISAMPLER2DMS, None));
        } else if type_name_atom == type_name!("usampler2DMS") {
            return Some((USAMPLER2DMS, None));
        } else if type_name_atom == type_name!("sampler2DMSArray") {
            return Some((SAMPLER2DMSARRAY, None));
        } else if type_name_atom == type_name!("isampler2DMSArray") {
            return Some((ISAMPLER2DMSARRAY, None));
        } else if type_name_atom == type_name!("usampler2DMSArray") {
            return Some((USAMPLER2DMSARRAY, None));
        } else if type_name_atom == type_name!("sampler3D") {
            return Some((SAMPLER3D, None));
        } else if type_name_atom == type_name!("isampler3D") {
            return Some((ISAMPLER3D, None));
        } else if type_name_atom == type_name!("usampler3D") {
            return Some((USAMPLER3D, None));
        } else if type_name_atom == type_name!("samplerCube") {
            return Some((SAMPLERCUBE, None));
        } else if type_name_atom == type_name!("samplerCubeShadow") {
            return Some((SAMPLERCUBESHADOW, None));
        } else if type_name_atom == type_name!("isamplerCube") {
            return Some((ISAMPLERCUBE, None));
        } else if type_name_atom == type_name!("usamplerCube") {
            return Some((USAMPLERCUBE, None));
        } else if type_name_atom == type_name!("samplerCubeArray") {
            return Some((SAMPLERCUBEARRAY, None));
        } else if type_name_atom == type_name!("samplerCubeArrayShadow") {
            return Some((SAMPLERCUBEARRAYSHADOW, None));
        } else if type_name_atom == type_name!("isamplerCubeArray") {
            return Some((ISAMPLERCUBEARRAY, None));
        } else if type_name_atom == type_name!("usamplerCubeArray") {
            return Some((USAMPLERCUBEARRAY, None));
        } else if type_name_atom == type_name!("samplerBuffer") {
            return Some((SAMPLERBUFFER, None));
        } else if type_name_atom == type_name!("isamplerBuffer") {
            return Some((ISAMPLERBUFFER, None));
        } else if type_name_atom == type_name!("usamplerBuffer") {
            return Some((USAMPLERBUFFER, None));
        } else if type_name_atom == type_name!("image1D") {
            return Some((IMAGE1D, None));
        } else if type_name_atom == type_name!("iimage1D") {
            return Some((IIMAGE1D, None));
        } else if type_name_atom == type_name!("uimage1D") {
            return Some((UIMAGE1D, None));
        } else if type_name_atom == type_name!("image1DArray") {
            return Some((IMAGE1DARRAY, None));
        } else if type_name_atom == type_name!("iimage1DArray") {
            return Some((IIMAGE1DARRAY, None));
        } else if type_name_atom == type_name!("uimage1DArray") {
            return Some((UIMAGE1DARRAY, None));
        } else if type_name_atom == type_name!("image2D") {
            return Some((IMAGE2D, None));
        } else if type_name_atom == type_name!("iimage2D") {
            return Some((IIMAGE2D, None));
        } else if type_name_atom == type_name!("uimage2D") {
            return Some((UIMAGE2D, None));
        } else if type_name_atom == type_name!("image2DArray") {
            return Some((IMAGE2DARRAY, None));
        } else if type_name_atom == type_name!("iimage2DArray") {
            return Some((IIMAGE2DARRAY, None));
        } else if type_name_atom == type_name!("uimage2DArray") {
            return Some((UIMAGE2DARRAY, None));
        } else if type_name_atom == type_name!("image2DRect") {
            return Some((IMAGE2DRECT, None));
        } else if type_name_atom == type_name!("iimage2DRect") {
            return Some((IIMAGE2DRECT, None));
        } else if type_name_atom == type_name!("uimage2DRect") {
            return Some((UIMAGE2DRECT, None));
        } else if type_name_atom == type_name!("image2DMS") {
            return Some((IMAGE2DMS, None));
        } else if type_name_atom == type_name!("iimage2DMS") {
            return Some((IIMAGE2DMS, None));
        } else if type_name_atom == type_name!("uimage2DMS") {
            return Some((UIMAGE2DMS, None));
        } else if type_name_atom == type_name!("image2DMSArray") {
            return Some((IMAGE2DMSARRAY, None));
        } else if type_name_atom == type_name!("iimage2DMSArray") {
            return Some((IIMAGE2DMSARRAY, None));
        } else if type_name_atom == type_name!("uimage2DMSArray") {
            return Some((UIMAGE2DMSARRAY, None));
        } else if type_name_atom == type_name!("image3D") {
            return Some((IMAGE3D, None));
        } else if type_name_atom == type_name!("iimage3D") {
            return Some((IIMAGE3D, None));
        } else if type_name_atom == type_name!("uimage3D") {
            return Some((UIMAGE3D, None));
        } else if type_name_atom == type_name!("imageCube") {
            return Some((IMAGECUBE, None));
        } else if type_name_atom == type_name!("iimageCube") {
            return Some((IIMAGECUBE, None));
        } else if type_name_atom == type_name!("uimageCube") {
            return Some((UIMAGECUBE, None));
        } else if type_name_atom == type_name!("imageCubeArray") {
            return Some((IMAGECUBEARRAY, None));
        } else if type_name_atom == type_name!("iimageCubeArray") {
            return Some((IIMAGECUBEARRAY, None));
        } else if type_name_atom == type_name!("uimageCubeArray") {
            return Some((UIMAGECUBEARRAY, None));
        } else if type_name_atom == type_name!("imageBuffer") {
            return Some((IMAGEBUFFER, None));
        } else if type_name_atom == type_name!("iimageBuffer") {
            return Some((IIMAGEBUFFER, None));
        } else if type_name_atom == type_name!("uimageBuffer") {
            return Some((UIMAGEBUFFER, None));
        }

        // Vulkan type names
        if target_vulkan {
            if type_name_atom == type_name!("texture1D") {
                return Some((TEXTURE1D, None));
            } else if type_name_atom == type_name!("texture1DArray") {
                return Some((TEXTURE1DARRAY, None));
            } else if type_name_atom == type_name!("itexture1D") {
                return Some((ITEXTURE1D, None));
            } else if type_name_atom == type_name!("itexture1DArray") {
                return Some((ITEXTURE1DARRAY, None));
            } else if type_name_atom == type_name!("utexture1D") {
                return Some((UTEXTURE1D, None));
            } else if type_name_atom == type_name!("utexture1DArray") {
                return Some((UTEXTURE1DARRAY, None));
            } else if type_name_atom == type_name!("texture2D") {
                return Some((TEXTURE2D, None));
            } else if type_name_atom == type_name!("texture2DArray") {
                return Some((TEXTURE2DARRAY, None));
            } else if type_name_atom == type_name!("itexture2D") {
                return Some((ITEXTURE2D, None));
            } else if type_name_atom == type_name!("itexture2DArray") {
                return Some((ITEXTURE2DARRAY, None));
            } else if type_name_atom == type_name!("utexture2D") {
                return Some((UTEXTURE2D, None));
            } else if type_name_atom == type_name!("utexture2DArray") {
                return Some((UTEXTURE2DARRAY, None));
            } else if type_name_atom == type_name!("texture2DRect") {
                return Some((TEXTURE2DRECT, None));
            } else if type_name_atom == type_name!("itexture2DRect") {
                return Some((ITEXTURE2DRECT, None));
            } else if type_name_atom == type_name!("utexture2DRect") {
                return Some((UTEXTURE2DRECT, None));
            } else if type_name_atom == type_name!("texture2DMS") {
                return Some((TEXTURE2DMS, None));
            } else if type_name_atom == type_name!("itexture2DMS") {
                return Some((ITEXTURE2DMS, None));
            } else if type_name_atom == type_name!("utexture2DMS") {
                return Some((UTEXTURE2DMS, None));
            } else if type_name_atom == type_name!("texture2DMSArray") {
                return Some((TEXTURE2DMSARRAY, None));
            } else if type_name_atom == type_name!("itexture2DMSArray") {
                return Some((ITEXTURE2DMSARRAY, None));
            } else if type_name_atom == type_name!("utexture2DMSArray") {
                return Some((UTEXTURE2DMSARRAY, None));
            } else if type_name_atom == type_name!("texture3D") {
                return Some((TEXTURE3D, None));
            } else if type_name_atom == type_name!("itexture3D") {
                return Some((ITEXTURE3D, None));
            } else if type_name_atom == type_name!("utexture3D") {
                return Some((UTEXTURE3D, None));
            } else if type_name_atom == type_name!("textureCube") {
                return Some((TEXTURECUBE, None));
            } else if type_name_atom == type_name!("itextureCube") {
                return Some((ITEXTURECUBE, None));
            } else if type_name_atom == type_name!("utextureCube") {
                return Some((UTEXTURECUBE, None));
            } else if type_name_atom == type_name!("textureCubeArray") {
                return Some((TEXTURECUBEARRAY, None));
            } else if type_name_atom == type_name!("itextureCubeArray") {
                return Some((ITEXTURECUBEARRAY, None));
            } else if type_name_atom == type_name!("utextureCubeArray") {
                return Some((UTEXTURECUBEARRAY, None));
            } else if type_name_atom == type_name!("textureBuffer") {
                return Some((TEXTUREBUFFER, None));
            } else if type_name_atom == type_name!("itextureBuffer") {
                return Some((ITEXTUREBUFFER, None));
            } else if type_name_atom == type_name!("utextureBuffer") {
                return Some((UTEXTUREBUFFER, None));
            } else if type_name_atom == type_name!("sampler") {
                return Some((SAMPLER, None));
            } else if type_name_atom == type_name!("samplerShadow") {
                return Some((SAMPLERSHADOW, None));
            } else if type_name_atom == type_name!("subpassInput") {
                return Some((SUBPASSINPUT, None));
            } else if type_name_atom == type_name!("isubpassInput") {
                return Some((ISUBPASSINPUT, None));
            } else if type_name_atom == type_name!("usubpassInput") {
                return Some((USUBPASSINPUT, None));
            } else if type_name_atom == type_name!("subpassInputMS") {
                return Some((SUBPASSINPUTMS, None));
            } else if type_name_atom == type_name!("isubpassInputMS") {
                return Some((ISUBPASSINPUTMS, None));
            } else if type_name_atom == type_name!("usubpassInputMS") {
                return Some((USUBPASSINPUTMS, None));
            }
        }

        // Reserved for future use
        if type_name_atom == type_name!("hvec2") {
            return Some((HVEC2, None));
        } else if type_name_atom == type_name!("hvec3") {
            return Some((HVEC3, None));
        } else if type_name_atom == type_name!("hvec4") {
            return Some((HVEC4, None));
        } else if type_name_atom == type_name!("fvec2") {
            return Some((FVEC2, None));
        } else if type_name_atom == type_name!("fvec3") {
            return Some((FVEC3, None));
        } else if type_name_atom == type_name!("fvec4") {
            return Some((FVEC4, None));
        } else if type_name_atom == type_name!("sampler3DRect") {
            return Some((SAMPLER3DRECT, None));
        }

        let result = is_type_name(&type_name_atom);
        if result.is_type_name() {
            return Some((OTHER(type_name_atom), Some(result)));
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, lang_util::Token)]
#[allow(non_camel_case_types)]
pub enum Token {
    /// Identifier
    #[lang_util(as = "IDENT", kind = "identifier")]
    IDENT(SmolStr),
    /// Type name
    #[lang_util(as = "TYPE_NAME", kind = "type name")]
    TYPE_NAME(TypeName),
    /// Float constant
    #[lang_util(as = "FLOAT_CONST", kind = "float constant", kind = "literal")]
    FLOAT_CONST(f32),
    /// Int constant
    #[lang_util(as = "INT_CONST", kind = "int constant", kind = "literal")]
    INT_CONST(i32),
    /// Unsigned int constant
    #[lang_util(as = "UINT_CONST", kind = "uint constant", kind = "literal")]
    UINT_CONST(u32),
    /// Bool constant
    #[lang_util(as = "BOOL_CONST", kind = "bool constant", kind = "literal")]
    BOOL_CONST(bool),
    /// Double constant
    #[lang_util(as = "DOUBLE_CONST", kind = "double constant", kind = "literal")]
    DOUBLE_CONST(f64),
    // Multi-char tokens
    /// <<
    #[lang_util(token = "<<", kind = "binary operator", kind = "operator")]
    LEFT_OP,
    /// >>
    #[lang_util(token = ">>", kind = "binary operator", kind = "operator")]
    RIGHT_OP,
    /// ++
    #[lang_util(token = "++", kind = "unary operator", kind = "operator")]
    INC_OP,
    /// --
    #[lang_util(token = "--", kind = "unary operator", kind = "operator")]
    DEC_OP,
    /// <=
    #[lang_util(token = "<=", kind = "binary operator", kind = "operator")]
    LE_OP,
    /// >=
    #[lang_util(token = ">=", kind = "binary operator", kind = "operator")]
    GE_OP,
    /// ==
    #[lang_util(token = "==", kind = "binary operator", kind = "operator")]
    EQ_OP,
    /// !=
    #[lang_util(token = "!=", kind = "binary operator", kind = "operator")]
    NE_OP,
    /// &&
    #[lang_util(token = "&&", kind = "binary operator", kind = "operator")]
    AND_OP,
    /// ||
    #[lang_util(token = "||", kind = "binary operator", kind = "operator")]
    OR_OP,
    /// ^^
    #[lang_util(token = "^^", kind = "binary operator", kind = "operator")]
    XOR_OP,
    /// *=
    #[lang_util(token = "*=", kind = "binary operator", kind = "operator")]
    MUL_ASSIGN,
    /// /=
    #[lang_util(token = "/=", kind = "binary operator", kind = "operator")]
    DIV_ASSIGN,
    /// +=
    #[lang_util(token = "+=", kind = "binary operator", kind = "operator")]
    ADD_ASSIGN,
    /// %=
    #[lang_util(token = "%=", kind = "binary operator", kind = "operator")]
    MOD_ASSIGN,
    /// <<=
    #[lang_util(token = "<<=", kind = "binary operator", kind = "operator")]
    LEFT_ASSIGN,
    /// >>=
    #[lang_util(token = ">>=", kind = "binary operator", kind = "operator")]
    RIGHT_ASSIGN,
    /// &=
    #[lang_util(token = "&=", kind = "binary operator", kind = "operator")]
    AND_ASSIGN,
    /// ^=
    #[lang_util(token = "^=", kind = "binary operator", kind = "operator")]
    XOR_ASSIGN,
    /// |=
    #[lang_util(token = "|=", kind = "binary operator", kind = "operator")]
    OR_ASSIGN,
    /// -=
    #[lang_util(token = "-=", kind = "binary operator", kind = "operator")]
    SUB_ASSIGN,
    // Single-char tokens
    /// (
    #[lang_util(token = "(")]
    LPAREN,
    /// )
    #[lang_util(token = ")")]
    RPAREN,
    /// [
    #[lang_util(token = "[")]
    LBRACKET,
    /// ]
    #[lang_util(token = "]")]
    RBRACKET,
    /// {
    #[lang_util(token = "{")]
    LBRACE,
    /// }
    #[lang_util(token = "}")]
    RBRACE,
    /// .
    #[lang_util(token = ".", kind = "binary operator", kind = "operator")]
    PERIOD,
    /// ,
    #[lang_util(token = ",", kind = "operator")]
    COMMA,
    /// :
    #[lang_util(token = ":", kind = "operator")]
    COLON,
    /// =
    #[lang_util(token = "=", kind = "binary operator", kind = "operator")]
    EQUAL,
    /// ;
    #[lang_util(token = ";")]
    SEMICOLON,
    /// !
    #[lang_util(token = "!", kind = "unary operator", kind = "operator")]
    BANG,
    /// -
    #[lang_util(
        token = "-",
        kind = "binary operator",
        kind = "unary operator",
        kind = "operator"
    )]
    DASH,
    /// ~
    #[lang_util(token = "~", kind = "unary operator", kind = "operator")]
    TILDE,
    /// +
    #[lang_util(
        token = "+",
        kind = "binary operator",
        kind = "unary operator",
        kind = "operator"
    )]
    PLUS,
    /// *
    #[lang_util(token = "*", kind = "binary operator", kind = "operator")]
    ASTERISK,
    /// /
    #[lang_util(token = "/", kind = "binary operator", kind = "operator")]
    SLASH,
    /// %
    #[lang_util(token = "%", kind = "binary operator", kind = "operator")]
    PERCENT,
    /// <
    #[lang_util(token = "<", kind = "binary operator", kind = "operator")]
    LANGLE,
    /// >
    #[lang_util(token = ">", kind = "binary operator", kind = "operator")]
    RANGLE,
    /// |
    #[lang_util(token = "|", kind = "binary operator", kind = "operator")]
    BAR,
    /// ^
    #[lang_util(token = "^", kind = "binary operator", kind = "operator")]
    CARET,
    /// &
    #[lang_util(token = "&", kind = "binary operator", kind = "operator")]
    AMPERSAND,
    /// ?
    #[lang_util(token = "?", kind = "operator")]
    QUESTION,
    /// #
    #[lang_util(token = "#")]
    HASH,
    // Keywords
    /// "const"
    #[lang_util(token = "const", kind = "storage qualifier", kind = "type qualifier")]
    CONST,
    /// "uniform"
    #[lang_util(token = "uniform", kind = "storage qualifier", kind = "type qualifier")]
    UNIFORM,
    /// "buffer"
    #[lang_util(token = "buffer", kind = "storage qualifier", kind = "type qualifier")]
    BUFFER,
    /// "shared"
    #[lang_util(token = "shared", kind = "storage qualifier", kind = "type qualifier")]
    SHARED,
    /// "attribute"
    #[lang_util(
        token = "attribute",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    ATTRIBUTE,
    /// "varying"
    #[lang_util(token = "varying", kind = "storage qualifier", kind = "type qualifier")]
    VARYING,
    /// "coherent"
    #[lang_util(
        token = "coherent",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    COHERENT,
    /// "volatile"
    #[lang_util(
        token = "volatile",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    VOLATILE,
    /// "restrict"
    #[lang_util(
        token = "restrict",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    RESTRICT,
    /// "readonly"
    #[lang_util(
        token = "readonly",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    READONLY,
    /// "writeonly"
    #[lang_util(
        token = "writeonly",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    WRITEONLY,
    /// "layout"
    #[lang_util(token = "layout", kind = "layout qualifier", kind = "type qualifier")]
    LAYOUT,
    /// "centroid"
    #[lang_util(
        token = "centroid",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    CENTROID,
    /// "flat"
    #[lang_util(
        token = "flat",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    FLAT,
    /// "smooth"
    #[lang_util(
        token = "smooth",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    SMOOTH,
    /// "noperspective"
    #[lang_util(
        token = "noperspective",
        kind = "interpolation qualifier",
        kind = "type qualifier"
    )]
    NOPERSPECTIVE,
    /// "patch"
    #[lang_util(token = "patch", kind = "storage qualifier", kind = "type qualifier")]
    PATCH,
    /// "sample"
    #[lang_util(token = "sample", kind = "storage qualifier", kind = "type qualifier")]
    SAMPLE,
    /// "invariant"
    #[lang_util(token = "invariant", kind = "type qualifier")]
    INVARIANT,
    /// "precise"
    #[lang_util(token = "precise", kind = "type qualifier")]
    PRECISE,
    /// "break"
    #[lang_util(token = "break", kind = "keyword")]
    BREAK,
    /// "continue"
    #[lang_util(token = "continue", kind = "keyword")]
    CONTINUE,
    /// "do"
    #[lang_util(token = "do", kind = "keyword")]
    DO,
    /// "for"
    #[lang_util(token = "for", kind = "keyword")]
    FOR,
    /// "while"
    #[lang_util(token = "while", kind = "keyword")]
    WHILE,
    /// "switch"
    #[lang_util(token = "switch", kind = "keyword")]
    SWITCH,
    /// "case"
    #[lang_util(token = "case", kind = "keyword")]
    CASE,
    /// "default"
    #[lang_util(token = "default", kind = "keyword")]
    DEFAULT,
    /// "if"
    #[lang_util(token = "if", kind = "keyword")]
    IF,
    /// "else"
    #[lang_util(token = "else", kind = "keyword")]
    ELSE,
    /// "subroutine"
    #[lang_util(
        token = "subroutine",
        kind = "storage qualifier",
        kind = "type qualifier"
    )]
    SUBROUTINE,
    /// "in"
    #[lang_util(token = "in", kind = "storage qualifier", kind = "type qualifier")]
    IN,
    /// "out"
    #[lang_util(token = "out", kind = "storage qualifier", kind = "type qualifier")]
    OUT,
    /// "inout"
    #[lang_util(token = "inout", kind = "storage qualifier", kind = "type qualifier")]
    INOUT,
    /// "discard"
    #[lang_util(token = "discard", kind = "keyword")]
    DISCARD,
    /// "return"
    #[lang_util(token = "return", kind = "keyword")]
    RETURN,
    /// "lowp"
    #[lang_util(token = "lowp", kind = "precision qualifier", kind = "type qualifier")]
    LOWP,
    /// "mediump"
    #[lang_util(
        token = "mediump",
        kind = "precision qualifier",
        kind = "type qualifier"
    )]
    MEDIUMP,
    /// "highp"
    #[lang_util(token = "highp", kind = "precision qualifier", kind = "type qualifier")]
    HIGHP,
    /// "precision"
    #[lang_util(token = "precision")]
    PRECISION,
    /// "struct"
    #[lang_util(token = "struct", kind = "struct", kind = "keyword")]
    STRUCT,
    // Reserved for future use
    /// "common"
    #[lang_util(token = "common", kind = "reserved keyword")]
    COMMON,
    /// "partition"
    #[lang_util(token = "partition", kind = "reserved keyword")]
    PARTITION,
    /// "active"
    #[lang_util(token = "active", kind = "reserved keyword")]
    ACTIVE,
    /// "asm"
    #[lang_util(token = "asm", kind = "reserved keyword")]
    ASM,
    /// "class"
    #[lang_util(token = "class", kind = "reserved keyword")]
    CLASS,
    /// "union"
    #[lang_util(token = "union", kind = "reserved keyword")]
    UNION,
    /// "enum"
    #[lang_util(token = "enum", kind = "reserved keyword")]
    ENUM,
    /// "typedef"
    #[lang_util(token = "typedef", kind = "reserved keyword")]
    TYPEDEF,
    /// "template"
    #[lang_util(token = "template", kind = "reserved keyword")]
    TEMPLATE,
    /// "this"
    #[lang_util(token = "this", kind = "reserved keyword")]
    THIS,
    /// "resource"
    #[lang_util(token = "resource", kind = "reserved keyword")]
    RESOURCE,
    /// "goto"
    #[lang_util(token = "goto", kind = "reserved keyword")]
    GOTO,
    /// "inline"
    #[lang_util(token = "inline", kind = "reserved keyword")]
    INLINE,
    /// "noinline"
    #[lang_util(token = "noinline", kind = "reserved keyword")]
    NOINLINE,
    /// "public"
    #[lang_util(token = "public", kind = "reserved keyword")]
    PUBLIC,
    /// "static"
    #[lang_util(token = "static", kind = "reserved keyword")]
    STATIC,
    /// "extern"
    #[lang_util(token = "extern", kind = "reserved keyword")]
    EXTERN,
    /// "external"
    #[lang_util(token = "external", kind = "reserved keyword")]
    EXTERNAL,
    /// "interface"
    #[lang_util(token = "interface", kind = "reserved keyword")]
    INTERFACE,
    /// "long"
    #[lang_util(token = "long", kind = "reserved keyword")]
    LONG,
    /// "short"
    #[lang_util(token = "short", kind = "reserved keyword")]
    SHORT,
    /// "half"
    #[lang_util(token = "half", kind = "reserved keyword")]
    HALF,
    /// "fixed"
    #[lang_util(token = "fixed", kind = "reserved keyword")]
    FIXED,
    /// "unsigned"
    #[lang_util(token = "unsigned", kind = "reserved keyword")]
    UNSIGNED,
    /// "superp"
    #[lang_util(token = "superp", kind = "reserved keyword")]
    SUPERP,
    /// "input"
    #[lang_util(token = "input", kind = "reserved keyword")]
    INPUT,
    /// "output"
    #[lang_util(token = "output", kind = "reserved keyword")]
    OUTPUT,
    /// "filter"
    #[lang_util(token = "filter", kind = "reserved keyword")]
    FILTER,
    /// "sizeof"
    #[lang_util(token = "sizeof", kind = "reserved keyword")]
    SIZEOF,
    /// "cast"
    #[lang_util(token = "cast", kind = "reserved keyword")]
    CAST,
    /// "namespace"
    #[lang_util(token = "namespace", kind = "reserved keyword")]
    NAMESPACE,
    /// "using"
    #[lang_util(token = "using", kind = "reserved keyword")]
    USING,
    // Other
    /// Whitespaace
    #[lang_util(display = "<whitespace>", as(display), kind = "trivia")]
    WS,
    /// Comment (single-line or multi-line)
    #[lang_util(display = "<comment>", as(display), kind = "trivia")]
    COMMENT,
    /// Marker for invalid tokens
    #[lang_util(display = "<invalid token>", as(display), kind = "error")]
    ERROR(ErrorKind),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display)]
pub enum ErrorKind {
    #[display(fmt = "invalid token")]
    InvalidToken,
    #[display(fmt = "invalid int literal")]
    InvalidIntLiteral,
    #[display(fmt = "invalid uint literal")]
    InvalidUIntLiteral,
    #[display(fmt = "invalid float literal")]
    InvalidFloatLiteral,
    #[display(fmt = "invalid double literal")]
    InvalidDoubleLiteral,
}

impl Token {
    pub(super) fn from_token(
        value: &impl TokenLike,
        target_vulkan: bool,
        is_type_name: impl Fn(&TypeNameAtom) -> TypeNameState,
    ) -> (Self, Option<TypeNameState>) {
        use Token::*;
        let kind = value.kind();

        // First, try the tokens that don't require text processing
        match kind {
            SyntaxKind::PP_EMPTY
            | SyntaxKind::PP_INCLUDE
            | SyntaxKind::PP_INCLUDE_PATH
            | SyntaxKind::PP_DEFINE
            | SyntaxKind::PP_DEFINE_ARGS
            | SyntaxKind::PP_DEFINE_ARG
            | SyntaxKind::PP_DEFINE_BODY
            | SyntaxKind::PP_UNDEF
            | SyntaxKind::PP_IDENT
            | SyntaxKind::PP_IF
            | SyntaxKind::PP_IF_EXPR
            | SyntaxKind::PP_IFDEF
            | SyntaxKind::PP_IFNDEF
            | SyntaxKind::PP_ELSE
            | SyntaxKind::PP_ELIF
            | SyntaxKind::PP_ENDIF
            | SyntaxKind::PP_ERROR
            | SyntaxKind::PP_ERROR_BODY
            | SyntaxKind::PP_PRAGMA
            | SyntaxKind::PP_PRAGMA_BODY
            | SyntaxKind::PP_EXTENSION
            | SyntaxKind::PP_VERSION
            | SyntaxKind::PP_VERSION_NUMBER
            | SyntaxKind::PP_VERSION_PROFILE
            | SyntaxKind::PP_LINE
            | SyntaxKind::PP_LINE_BODY
            | SyntaxKind::ERROR
            | SyntaxKind::ROOT
            | SyntaxKind::_LAST
            | SyntaxKind::QUOTE_STRING
            | SyntaxKind::ANGLE_STRING
            | SyntaxKind::BACKSLASH
            | SyntaxKind::DEFINED
            | SyntaxKind::PP_CONCAT
            | SyntaxKind::PP_CONCAT_OP => {
                return (ERROR(ErrorKind::InvalidToken), None);
            }
            // Those need further processing
            SyntaxKind::IDENT_KW => {}
            SyntaxKind::DIGITS => {}
            SyntaxKind::LEFT_OP => {
                return (LEFT_OP, None);
            }
            SyntaxKind::RIGHT_OP => {
                return (RIGHT_OP, None);
            }
            SyntaxKind::INC_OP => {
                return (INC_OP, None);
            }
            SyntaxKind::DEC_OP => {
                return (DEC_OP, None);
            }
            SyntaxKind::LE_OP => {
                return (LE_OP, None);
            }
            SyntaxKind::GE_OP => {
                return (GE_OP, None);
            }
            SyntaxKind::EQ_OP => {
                return (EQ_OP, None);
            }
            SyntaxKind::NE_OP => {
                return (NE_OP, None);
            }
            SyntaxKind::AND_OP => {
                return (AND_OP, None);
            }
            SyntaxKind::OR_OP => {
                return (OR_OP, None);
            }
            SyntaxKind::XOR_OP => {
                return (XOR_OP, None);
            }
            SyntaxKind::MUL_ASSIGN => {
                return (MUL_ASSIGN, None);
            }
            SyntaxKind::DIV_ASSIGN => {
                return (DIV_ASSIGN, None);
            }
            SyntaxKind::ADD_ASSIGN => {
                return (ADD_ASSIGN, None);
            }
            SyntaxKind::MOD_ASSIGN => {
                return (MOD_ASSIGN, None);
            }
            SyntaxKind::LEFT_ASSIGN => {
                return (LEFT_ASSIGN, None);
            }
            SyntaxKind::RIGHT_ASSIGN => {
                return (RIGHT_ASSIGN, None);
            }
            SyntaxKind::AND_ASSIGN => {
                return (AND_ASSIGN, None);
            }
            SyntaxKind::XOR_ASSIGN => {
                return (XOR_ASSIGN, None);
            }
            SyntaxKind::OR_ASSIGN => {
                return (OR_ASSIGN, None);
            }
            SyntaxKind::SUB_ASSIGN => {
                return (SUB_ASSIGN, None);
            }
            SyntaxKind::LPAREN => {
                return (LPAREN, None);
            }
            SyntaxKind::RPAREN => {
                return (RPAREN, None);
            }
            SyntaxKind::LBRACKET => {
                return (LBRACKET, None);
            }
            SyntaxKind::RBRACKET => {
                return (RBRACKET, None);
            }
            SyntaxKind::LBRACE => {
                return (LBRACE, None);
            }
            SyntaxKind::RBRACE => {
                return (RBRACE, None);
            }
            SyntaxKind::PERIOD => {
                return (PERIOD, None);
            }
            SyntaxKind::COMMA => {
                return (COMMA, None);
            }
            SyntaxKind::COLON => {
                return (COLON, None);
            }
            SyntaxKind::EQUAL => {
                return (EQUAL, None);
            }
            SyntaxKind::SEMICOLON => {
                return (SEMICOLON, None);
            }
            SyntaxKind::BANG => {
                return (BANG, None);
            }
            SyntaxKind::DASH => {
                return (DASH, None);
            }
            SyntaxKind::TILDE => {
                return (TILDE, None);
            }
            SyntaxKind::PLUS => {
                return (PLUS, None);
            }
            SyntaxKind::ASTERISK => {
                return (ASTERISK, None);
            }
            SyntaxKind::SLASH => {
                return (SLASH, None);
            }
            SyntaxKind::PERCENT => {
                return (PERCENT, None);
            }
            SyntaxKind::LANGLE => {
                return (LANGLE, None);
            }
            SyntaxKind::RANGLE => {
                return (RANGLE, None);
            }
            SyntaxKind::BAR => {
                return (BAR, None);
            }
            SyntaxKind::CARET => {
                return (CARET, None);
            }
            SyntaxKind::AMPERSAND => {
                return (AMPERSAND, None);
            }
            SyntaxKind::QUESTION => {
                return (QUESTION, None);
            }
            SyntaxKind::HASH => {
                return (HASH, None);
            }
            SyntaxKind::WS | SyntaxKind::NEWLINE => {
                return (WS, None);
            }
            SyntaxKind::COMMENT => {
                return (COMMENT, None);
            }
        }

        // Either IDENT_KW or DIGITS, we need to examine the text to know more
        let text = Unescaped::new(value.text()).to_string();
        if kind == SyntaxKind::IDENT_KW {
            // Is this a keyword?
            let keyword_atom = KeywordAtom::from(text.as_ref());

            // Keywords
            if keyword_atom == keyword!("const") {
                return (CONST, None);
            } else if keyword_atom == keyword!("uniform") {
                return (UNIFORM, None);
            } else if keyword_atom == keyword!("buffer") {
                return (BUFFER, None);
            } else if keyword_atom == keyword!("shared") {
                return (SHARED, None);
            } else if keyword_atom == keyword!("attribute") {
                return (ATTRIBUTE, None);
            } else if keyword_atom == keyword!("varying") {
                return (VARYING, None);
            } else if keyword_atom == keyword!("coherent") {
                return (COHERENT, None);
            } else if keyword_atom == keyword!("volatile") {
                return (VOLATILE, None);
            } else if keyword_atom == keyword!("restrict") {
                return (RESTRICT, None);
            } else if keyword_atom == keyword!("readonly") {
                return (READONLY, None);
            } else if keyword_atom == keyword!("writeonly") {
                return (WRITEONLY, None);
            } else if keyword_atom == keyword!("layout") {
                return (LAYOUT, None);
            } else if keyword_atom == keyword!("centroid") {
                return (CENTROID, None);
            } else if keyword_atom == keyword!("flat") {
                return (FLAT, None);
            } else if keyword_atom == keyword!("smooth") {
                return (SMOOTH, None);
            } else if keyword_atom == keyword!("noperspective") {
                return (NOPERSPECTIVE, None);
            } else if keyword_atom == keyword!("patch") {
                return (PATCH, None);
            } else if keyword_atom == keyword!("sample") {
                return (SAMPLE, None);
            } else if keyword_atom == keyword!("invariant") {
                return (INVARIANT, None);
            } else if keyword_atom == keyword!("precise") {
                return (PRECISE, None);
            } else if keyword_atom == keyword!("break") {
                return (BREAK, None);
            } else if keyword_atom == keyword!("continue") {
                return (CONTINUE, None);
            } else if keyword_atom == keyword!("do") {
                return (DO, None);
            } else if keyword_atom == keyword!("for") {
                return (FOR, None);
            } else if keyword_atom == keyword!("while") {
                return (WHILE, None);
            } else if keyword_atom == keyword!("switch") {
                return (SWITCH, None);
            } else if keyword_atom == keyword!("case") {
                return (CASE, None);
            } else if keyword_atom == keyword!("default") {
                return (DEFAULT, None);
            } else if keyword_atom == keyword!("if") {
                return (IF, None);
            } else if keyword_atom == keyword!("else") {
                return (ELSE, None);
            } else if keyword_atom == keyword!("subroutine") {
                return (SUBROUTINE, None);
            } else if keyword_atom == keyword!("in") {
                return (IN, None);
            } else if keyword_atom == keyword!("out") {
                return (OUT, None);
            } else if keyword_atom == keyword!("inout") {
                return (INOUT, None);
            } else if keyword_atom == keyword!("true") {
                return (BOOL_CONST(true), None);
            } else if keyword_atom == keyword!("false") {
                return (BOOL_CONST(false), None);
            } else if keyword_atom == keyword!("discard") {
                return (DISCARD, None);
            } else if keyword_atom == keyword!("return") {
                return (RETURN, None);
            } else if keyword_atom == keyword!("lowp") {
                return (LOWP, None);
            } else if keyword_atom == keyword!("mediump") {
                return (MEDIUMP, None);
            } else if keyword_atom == keyword!("highp") {
                return (HIGHP, None);
            } else if keyword_atom == keyword!("precision") {
                return (PRECISION, None);
            } else if keyword_atom == keyword!("struct") {
                return (STRUCT, None);
            }
            // Reserved for future use
            else if keyword_atom == keyword!("common") {
                return (COMMON, None);
            } else if keyword_atom == keyword!("partition") {
                return (PARTITION, None);
            } else if keyword_atom == keyword!("active") {
                return (ACTIVE, None);
            } else if keyword_atom == keyword!("asm") {
                return (ASM, None);
            } else if keyword_atom == keyword!("class") {
                return (CLASS, None);
            } else if keyword_atom == keyword!("union") {
                return (UNION, None);
            } else if keyword_atom == keyword!("enum") {
                return (ENUM, None);
            } else if keyword_atom == keyword!("typedef") {
                return (TYPEDEF, None);
            } else if keyword_atom == keyword!("template") {
                return (TEMPLATE, None);
            } else if keyword_atom == keyword!("this") {
                return (THIS, None);
            } else if keyword_atom == keyword!("resource") {
                return (RESOURCE, None);
            } else if keyword_atom == keyword!("goto") {
                return (GOTO, None);
            } else if keyword_atom == keyword!("inline") {
                return (INLINE, None);
            } else if keyword_atom == keyword!("noinline") {
                return (NOINLINE, None);
            } else if keyword_atom == keyword!("public") {
                return (PUBLIC, None);
            } else if keyword_atom == keyword!("static") {
                return (STATIC, None);
            } else if keyword_atom == keyword!("extern") {
                return (EXTERN, None);
            } else if keyword_atom == keyword!("external") {
                return (EXTERNAL, None);
            } else if keyword_atom == keyword!("interface") {
                return (INTERFACE, None);
            } else if keyword_atom == keyword!("long") {
                return (LONG, None);
            } else if keyword_atom == keyword!("short") {
                return (SHORT, None);
            } else if keyword_atom == keyword!("half") {
                return (HALF, None);
            } else if keyword_atom == keyword!("fixed") {
                return (FIXED, None);
            } else if keyword_atom == keyword!("unsigned") {
                return (UNSIGNED, None);
            } else if keyword_atom == keyword!("superp") {
                return (SUPERP, None);
            } else if keyword_atom == keyword!("input") {
                return (INPUT, None);
            } else if keyword_atom == keyword!("output") {
                return (OUTPUT, None);
            } else if keyword_atom == keyword!("filter") {
                return (FILTER, None);
            } else if keyword_atom == keyword!("sizeof") {
                return (SIZEOF, None);
            } else if keyword_atom == keyword!("cast") {
                return (CAST, None);
            } else if keyword_atom == keyword!("namespace") {
                return (NAMESPACE, None);
            } else if keyword_atom == keyword!("using") {
                return (USING, None);
            }

            // Else it might be a built-in type name
            if let Some((type_name, state)) =
                TypeName::parse(text.as_ref(), target_vulkan, is_type_name)
            {
                return (TYPE_NAME(type_name), state);
            }

            // Nothing matched, it's actually an ident
            (IDENT(text.into()), None)
        } else if kind == SyntaxKind::DIGITS {
            let result = if text.ends_with('f') || text.ends_with('F') || text.contains('.') {
                // Floating-point constant

                if let Some(double) = text.strip_suffix("lf").or_else(|| text.strip_suffix("LF")) {
                    lexical::parse(double)
                        .map(DOUBLE_CONST)
                        .map_err(|_| ErrorKind::InvalidDoubleLiteral)
                } else if let Some(float) = text
                    .strip_suffix('f')
                    .or_else(|| text.strip_suffix('F').or_else(|| Some(text.as_ref())))
                {
                    lexical::parse(float)
                        .map(FLOAT_CONST)
                        .map_err(|_| ErrorKind::InvalidFloatLiteral)
                } else {
                    Err(ErrorKind::InvalidFloatLiteral)
                }
            } else {
                let (unsigned, text) = {
                    if let Some(stripped) =
                        text.strip_suffix('u').or_else(|| text.strip_suffix('U'))
                    {
                        (true, stripped)
                    } else {
                        (false, text.as_ref())
                    }
                };

                // Integer constant
                if let Some(text) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
                    // Hexadecimal constant
                    if unsigned {
                        u32::from_str_radix(text, 16)
                            .map(UINT_CONST)
                            .map_err(|_| ErrorKind::InvalidUIntLiteral)
                    } else {
                        i32::from_str_radix(text, 16)
                            .map(INT_CONST)
                            .map_err(|_| ErrorKind::InvalidIntLiteral)
                    }
                } else if let Some(text) = text.strip_prefix('0') {
                    // Octal constant
                    if unsigned {
                        u32::from_str_radix(text, 8)
                            .map(UINT_CONST)
                            .map_err(|_| ErrorKind::InvalidUIntLiteral)
                    } else {
                        i32::from_str_radix(text, 8)
                            .map(INT_CONST)
                            .map_err(|_| ErrorKind::InvalidIntLiteral)
                    }
                } else {
                    // Decimal constant
                    if unsigned {
                        text.parse()
                            .map(UINT_CONST)
                            .map_err(|_| ErrorKind::InvalidUIntLiteral)
                    } else {
                        text.parse()
                            .map(INT_CONST)
                            .map_err(|_| ErrorKind::InvalidIntLiteral)
                    }
                }
            };

            match result {
                Ok(res) => (res, None),
                Err(err) => (ERROR(err), None),
            }
        } else {
            unreachable!()
        }
    }
}
