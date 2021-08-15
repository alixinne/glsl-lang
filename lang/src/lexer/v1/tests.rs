use super::*;

use crate::parse::ParseOptions;

fn check_ident(input: &str) {
    let context: LexerContext = ParseOptions {
        target_vulkan: false,
        ..Default::default()
    }
    .into();

    let mut lexer = Lexer::new(input, context.clone());

    assert_eq!(
        lexer.next().map(|(_, n, _)| n),
        Some(Token::Identifier((input, context)))
    );
}

fn check(input: &str, token: Token) {
    let mut lexer = Lexer::new(
        input,
        ParseOptions {
            target_vulkan: false,
            ..Default::default()
        }
        .into(),
    );
    assert_eq!(lexer.next().map(|(_, n, _)| n), Some(token));
}

fn check_vulkan(input: &str, token: Token) {
    let mut lexer = Lexer::new(
        input,
        ParseOptions {
            target_vulkan: true,
            ..Default::default()
        }
        .into(),
    );
    assert_eq!(lexer.next().map(|(_, n, _)| n), Some(token));
}

#[test]
fn lex_const() {
    check("const", Token::Const);
}

#[test]
fn lex_bool() {
    check("bool", Token::Bool);
}

#[test]
fn lex_float() {
    check("float", Token::Float);
}

#[test]
fn lex_int() {
    check("int", Token::Int);
}

#[test]
fn lex_uint() {
    check("uint", Token::UInt);
}

#[test]
fn lex_double() {
    check("double", Token::Double);
}

#[test]
fn lex_bvec2() {
    check("bvec2", Token::BVec2);
}

#[test]
fn lex_bvec3() {
    check("bvec3", Token::BVec3);
}

#[test]
fn lex_bvec4() {
    check("bvec4", Token::BVec4);
}

#[test]
fn lex_ivec2() {
    check("ivec2", Token::IVec2);
}

#[test]
fn lex_ivec3() {
    check("ivec3", Token::IVec3);
}

#[test]
fn lex_ivec4() {
    check("ivec4", Token::IVec4);
}

#[test]
fn lex_uvec2() {
    check("uvec2", Token::UVec2);
}

#[test]
fn lex_uvec3() {
    check("uvec3", Token::UVec3);
}

#[test]
fn lex_uvec4() {
    check("uvec4", Token::UVec4);
}

#[test]
fn lex_vec2() {
    check("vec2", Token::Vec2);
}

#[test]
fn lex_vec3() {
    check("vec3", Token::Vec3);
}

#[test]
fn lex_vec4() {
    check("vec4", Token::Vec4);
}

#[test]
fn lex_mat2() {
    check("mat2", Token::Mat2);
}

#[test]
fn lex_mat3() {
    check("mat3", Token::Mat3);
}

#[test]
fn lex_mat4() {
    check("mat4", Token::Mat4);
}

#[test]
fn lex_mat2x2() {
    check("mat2x2", Token::Mat2x2);
}

#[test]
fn lex_mat2x3() {
    check("mat2x3", Token::Mat2x3);
}

#[test]
fn lex_mat2x4() {
    check("mat2x4", Token::Mat2x4);
}

#[test]
fn lex_mat3x2() {
    check("mat3x2", Token::Mat3x2);
}

#[test]
fn lex_mat3x3() {
    check("mat3x3", Token::Mat3x3);
}

#[test]
fn lex_mat3x4() {
    check("mat3x4", Token::Mat3x4);
}

#[test]
fn lex_mat4x2() {
    check("mat4x2", Token::Mat4x2);
}

#[test]
fn lex_mat4x3() {
    check("mat4x3", Token::Mat4x3);
}

#[test]
fn lex_mat4x4() {
    check("mat4x4", Token::Mat4x4);
}

#[test]
fn lex_d_vec2() {
    check("dvec2", Token::DVec2);
}

#[test]
fn lex_d_vec3() {
    check("dvec3", Token::DVec3);
}

#[test]
fn lex_d_vec4() {
    check("dvec4", Token::DVec4);
}

#[test]
fn lex_dmat2() {
    check("dmat2", Token::DMat2);
}

#[test]
fn lex_dmat3() {
    check("dmat3", Token::DMat3);
}

#[test]
fn lex_dmat4() {
    check("dmat4", Token::DMat4);
}

#[test]
fn lex_dmat2x2() {
    check("dmat2x2", Token::DMat2x2);
}

#[test]
fn lex_dmat2x3() {
    check("dmat2x3", Token::DMat2x3);
}

#[test]
fn lex_dmat2x4() {
    check("dmat2x4", Token::DMat2x4);
}

#[test]
fn lex_dmat3x2() {
    check("dmat3x2", Token::DMat3x2);
}

#[test]
fn lex_dmat3x3() {
    check("dmat3x3", Token::DMat3x3);
}

#[test]
fn lex_dmat3x4() {
    check("dmat3x4", Token::DMat3x4);
}

#[test]
fn lex_dmat4x2() {
    check("dmat4x2", Token::DMat4x2);
}

#[test]
fn lex_dmat4x3() {
    check("dmat4x3", Token::DMat4x3);
}

#[test]
fn lex_dmat4x4() {
    check("dmat4x4", Token::DMat4x4);
}

#[test]
fn lex_centroid() {
    check("centroid", Token::Centroid);
}

#[test]
fn lex_in() {
    check("in", Token::In);
}

#[test]
fn lex_out() {
    check("out", Token::Out);
}

#[test]
fn lex_in_out() {
    check("inout", Token::InOut);
}

#[test]
fn lex_uniform() {
    check("uniform", Token::Uniform);
}

#[test]
fn lex_patch() {
    check("patch", Token::Patch);
}

#[test]
fn lex_sample() {
    check("sample", Token::Sample);
}

#[test]
fn lex_buffer() {
    check("buffer", Token::Buffer);
}

#[test]
fn lex_shared() {
    check("shared", Token::Shared);
}

#[test]
fn lex_coherent() {
    check("coherent", Token::Coherent);
}

#[test]
fn lex_volatile() {
    check("volatile", Token::Volatile);
}

#[test]
fn lex_restrict() {
    check("restrict", Token::Restrict);
}

#[test]
fn lex_read_only() {
    check("readonly", Token::ReadOnly);
}

#[test]
fn lex_write_only() {
    check("writeonly", Token::WriteOnly);
}

#[test]
fn lex_no_perspective() {
    check("noperspective", Token::NoPerspective);
}

#[test]
fn lex_flat() {
    check("flat", Token::Flat);
}

#[test]
fn lex_smooth() {
    check("smooth", Token::Smooth);
}

#[test]
fn lex_layout() {
    check("layout", Token::Layout);
}

#[test]
fn lex_atomic_uint() {
    check("atomic_uint", Token::AtomicUInt);
}

#[test]
fn lex_sampler_1d() {
    check("sampler1D", Token::Sampler1D);
}

#[test]
fn lex_sampler_1d_shadow() {
    check("sampler1DShadow", Token::Sampler1DShadow);
}

#[test]
fn lex_sampler_1d_array() {
    check("sampler1DArray", Token::Sampler1DArray);
}

#[test]
fn lex_sampler_1d_array_shadow() {
    check("sampler1DArrayShadow", Token::Sampler1DArrayShadow);
}

#[test]
fn lex_isampler_1d() {
    check("isampler1D", Token::ISampler1D);
}

#[test]
fn lex_isampler_1d_array() {
    check("isampler1DArray", Token::ISampler1DArray);
}

#[test]
fn lex_usampler_1d() {
    check("usampler1D", Token::USampler1D);
}

#[test]
fn lex_usampler_1d_array() {
    check("usampler1DArray", Token::USampler1DArray);
}

#[test]
fn lex_sampler_2d() {
    check("sampler2D", Token::Sampler2D);
}

#[test]
fn lex_sampler_2d_shadow() {
    check("sampler2DShadow", Token::Sampler2DShadow);
}

#[test]
fn lex_sampler_2d_array() {
    check("sampler2DArray", Token::Sampler2DArray);
}

#[test]
fn lex_sampler_2d_array_shadow() {
    check("sampler2DArrayShadow", Token::Sampler2DArrayShadow);
}

#[test]
fn lex_isampler_2d() {
    check("isampler2D", Token::ISampler2D);
}

#[test]
fn lex_isampler_2d_array() {
    check("isampler2DArray", Token::ISampler2DArray);
}

#[test]
fn lex_usampler_2d() {
    check("usampler2D", Token::USampler2D);
}

#[test]
fn lex_usampler_2d_array() {
    check("usampler2DArray", Token::USampler2DArray);
}

#[test]
fn lex_sampler_2d_rect() {
    check("sampler2DRect", Token::Sampler2DRect);
}

#[test]
fn lex_sampler_2d_rect_shadow() {
    check("sampler2DRectShadow", Token::Sampler2DRectShadow);
}

#[test]
fn lex_isampler_2d_rect() {
    check("isampler2DRect", Token::ISampler2DRect);
}

#[test]
fn lex_usampler_2d_rect() {
    check("usampler2DRect", Token::USampler2DRect);
}

#[test]
fn lex_sampler_2dms() {
    check("sampler2DMS", Token::Sampler2DMs);
}

#[test]
fn lex_isampler_2dms() {
    check("isampler2DMS", Token::ISampler2DMs);
}

#[test]
fn lex_usampler_2dms() {
    check("usampler2DMS", Token::USampler2DMs);
}

#[test]
fn lex_sampler_2dms_array() {
    check("sampler2DMSArray", Token::Sampler2DMsArray);
}

#[test]
fn lex_isampler_2dms_array() {
    check("isampler2DMSArray", Token::ISampler2DMsArray);
}

#[test]
fn lex_usampler_2dms_array() {
    check("usampler2DMSArray", Token::USampler2DMsArray);
}

#[test]
fn lex_sampler_3d() {
    check("sampler3D", Token::Sampler3D);
}

#[test]
fn lex_isampler_3d() {
    check("isampler3D", Token::ISampler3D);
}

#[test]
fn lex_usampler_3d() {
    check("usampler3D", Token::USampler3D);
}

#[test]
fn lex_sampler_cube() {
    check("samplerCube", Token::SamplerCube);
}

#[test]
fn lex_sampler_cube_shadow() {
    check("samplerCubeShadow", Token::SamplerCubeShadow);
}

#[test]
fn lex_isampler_cube() {
    check("isamplerCube", Token::ISamplerCube);
}

#[test]
fn lex_usampler_cube() {
    check("usamplerCube", Token::USamplerCube);
}

#[test]
fn lex_sampler_cube_array() {
    check("samplerCubeArray", Token::SamplerCubeArray);
}

#[test]
fn lex_sampler_cube_array_shadow() {
    check("samplerCubeArrayShadow", Token::SamplerCubeArrayShadow);
}

#[test]
fn lex_isampler_cube_array() {
    check("isamplerCubeArray", Token::ISamplerCubeArray);
}

#[test]
fn lex_usampler_cube_array() {
    check("usamplerCubeArray", Token::USamplerCubeArray);
}

#[test]
fn lex_sampler_buffer() {
    check("samplerBuffer", Token::SamplerBuffer);
}

#[test]
fn lex_isampler_buffer() {
    check("isamplerBuffer", Token::ISamplerBuffer);
}

#[test]
fn lex_usampler_buffer() {
    check("usamplerBuffer", Token::USamplerBuffer);
}

#[test]
fn lex_image_1d() {
    check("image1D", Token::Image1D);
}

#[test]
fn lex_iimage_1d() {
    check("iimage1D", Token::IImage1D);
}

#[test]
fn lex_uimage_1d() {
    check("uimage1D", Token::UImage1D);
}

#[test]
fn lex_image_1d_array() {
    check("image1DArray", Token::Image1DArray);
}

#[test]
fn lex_iimage_1d_array() {
    check("iimage1DArray", Token::IImage1DArray);
}

#[test]
fn lex_uimage_1d_array() {
    check("uimage1DArray", Token::UImage1DArray);
}

#[test]
fn lex_image_2d() {
    check("image2D", Token::Image2D);
}

#[test]
fn lex_iimage_2d() {
    check("iimage2D", Token::IImage2D);
}

#[test]
fn lex_uimage_2d() {
    check("uimage2D", Token::UImage2D);
}

#[test]
fn lex_image_2d_array() {
    check("image2DArray", Token::Image2DArray);
}

#[test]
fn lex_iimage_2d_array() {
    check("iimage2DArray", Token::IImage2DArray);
}

#[test]
fn lex_uimage_2d_array() {
    check("uimage2DArray", Token::UImage2DArray);
}

#[test]
fn lex_image_2d_rect() {
    check("image2DRect", Token::Image2DRect);
}

#[test]
fn lex_iimage_2d_rect() {
    check("iimage2DRect", Token::IImage2DRect);
}

#[test]
fn lex_uimage_2d_rect() {
    check("uimage2DRect", Token::UImage2DRect);
}

#[test]
fn lex_image_2dms() {
    check("image2DMS", Token::Image2DMs);
}

#[test]
fn lex_iimage_2dms() {
    check("iimage2DMS", Token::IImage2DMs);
}

#[test]
fn lex_uimage_2dms() {
    check("uimage2DMS", Token::UImage2DMs);
}

#[test]
fn lex_image_2dms_array() {
    check("image2DMSArray", Token::Image2DMsArray);
}

#[test]
fn lex_iimage_2dms_array() {
    check("iimage2DMSArray", Token::IImage2DMsArray);
}

#[test]
fn lex_uimage_2dms_array() {
    check("uimage2DMSArray", Token::UImage2DMsArray);
}

#[test]
fn lex_image_3d() {
    check("image3D", Token::Image3D);
}

#[test]
fn lex_iimage_3d() {
    check("iimage3D", Token::IImage3D);
}

#[test]
fn lex_uimage_3d() {
    check("uimage3D", Token::UImage3D);
}

#[test]
fn lex_image_cube() {
    check("imageCube", Token::ImageCube);
}

#[test]
fn lex_iimage_cube() {
    check("iimageCube", Token::IImageCube);
}

#[test]
fn lex_uimage_cube() {
    check("uimageCube", Token::UImageCube);
}

#[test]
fn lex_image_cube_array() {
    check("imageCubeArray", Token::ImageCubeArray);
}

#[test]
fn lex_iimage_cube_array() {
    check("iimageCubeArray", Token::IImageCubeArray);
}

#[test]
fn lex_uimage_cube_array() {
    check("uimageCubeArray", Token::UImageCubeArray);
}

#[test]
fn lex_image_buffer() {
    check("imageBuffer", Token::ImageBuffer);
}

#[test]
fn lex_iimage_buffer() {
    check("iimageBuffer", Token::IImageBuffer);
}

#[test]
fn lex_uimage_buffer() {
    check("uimageBuffer", Token::UImageBuffer);
}

// Begin Vulkan-target keywords
#[test]
fn lex_texture_1d() {
    check_vulkan("texture1D", Token::Texture1D);
    check_ident("texture1D");
}

#[test]
fn lex_texture_1d_array() {
    check_vulkan("texture1DArray", Token::Texture1DArray);
    check_ident("texture1DArray");
}

#[test]
fn lex_itexture_1d() {
    check_vulkan("itexture1D", Token::ITexture1D);
    check_ident("itexture1D");
}

#[test]
fn lex_itexture_1d_array() {
    check_vulkan("itexture1DArray", Token::ITexture1DArray);
    check_ident("itexture1DArray");
}

#[test]
fn lex_utexture_1d() {
    check_vulkan("utexture1D", Token::UTexture1D);
    check_ident("utexture1D");
}

#[test]
fn lex_utexture_1d_array() {
    check_vulkan("utexture1DArray", Token::UTexture1DArray);
    check_ident("utexture1DArray");
}

#[test]
fn lex_texture_2d() {
    check_vulkan("texture2D", Token::Texture2D);
    check_ident("texture2D");
}

#[test]
fn lex_texture_2d_array() {
    check_vulkan("texture2DArray", Token::Texture2DArray);
    check_ident("texture2DArray");
}

#[test]
fn lex_itexture_2d() {
    check_vulkan("itexture2D", Token::ITexture2D);
    check_ident("itexture2D");
}

#[test]
fn lex_itexture_2d_array() {
    check_vulkan("itexture2DArray", Token::ITexture2DArray);
    check_ident("itexture2DArray");
}

#[test]
fn lex_utexture_2d() {
    check_vulkan("utexture2D", Token::UTexture2D);
    check_ident("utexture2D");
}

#[test]
fn lex_utexture_2d_array() {
    check_vulkan("utexture2DArray", Token::UTexture2DArray);
    check_ident("utexture2DArray");
}

#[test]
fn lex_texture_2d_rect() {
    check_vulkan("texture2DRect", Token::Texture2DRect);
    check_ident("texture2DRect");
}

#[test]
fn lex_itexture_2d_rect() {
    check_vulkan("itexture2DRect", Token::ITexture2DRect);
    check_ident("itexture2DRect");
}

#[test]
fn lex_utexture_2d_rect() {
    check_vulkan("utexture2DRect", Token::UTexture2DRect);
    check_ident("utexture2DRect");
}

#[test]
fn lex_texture_2dms() {
    check_vulkan("texture2DMS", Token::Texture2DMs);
    check_ident("texture2DMS");
}

#[test]
fn lex_itexture_2dms() {
    check_vulkan("itexture2DMS", Token::ITexture2DMs);
    check_ident("itexture2DMS");
}

#[test]
fn lex_utexture_2dms() {
    check_vulkan("utexture2DMS", Token::UTexture2DMs);
    check_ident("utexture2DMS");
}

#[test]
fn lex_texture_2dms_array() {
    check_vulkan("texture2DMSArray", Token::Texture2DMsArray);
    check_ident("texture2DMSArray");
}

#[test]
fn lex_itexture_2dms_array() {
    check_vulkan("itexture2DMSArray", Token::ITexture2DMsArray);
    check_ident("itexture2DMSArray");
}

#[test]
fn lex_utexture_2dms_array() {
    check_vulkan("utexture2DMSArray", Token::UTexture2DMsArray);
    check_ident("utexture2DMSArray");
}

#[test]
fn lex_texture_3d() {
    check_vulkan("texture3D", Token::Texture3D);
    check_ident("texture3D");
}

#[test]
fn lex_itexture_3d() {
    check_vulkan("itexture3D", Token::ITexture3D);
    check_ident("itexture3D");
}

#[test]
fn lex_utexture_3d() {
    check_vulkan("utexture3D", Token::UTexture3D);
    check_ident("utexture3D");
}

#[test]
fn lex_texture_cube() {
    check_vulkan("textureCube", Token::TextureCube);
    check_ident("textureCube");
}

#[test]
fn lex_itexture_cube() {
    check_vulkan("itextureCube", Token::ITextureCube);
    check_ident("itextureCube");
}

#[test]
fn lex_utexture_cube() {
    check_vulkan("utextureCube", Token::UTextureCube);
    check_ident("utextureCube");
}

#[test]
fn lex_texture_cube_array() {
    check_vulkan("textureCubeArray", Token::TextureCubeArray);
    check_ident("textureCubeArray");
}

#[test]
fn lex_itexture_cube_array() {
    check_vulkan("itextureCubeArray", Token::ITextureCubeArray);
    check_ident("itextureCubeArray");
}

#[test]
fn lex_utexture_cube_array() {
    check_vulkan("utextureCubeArray", Token::UTextureCubeArray);
    check_ident("utextureCubeArray");
}

#[test]
fn lex_texture_buffer() {
    check_vulkan("textureBuffer", Token::TextureBuffer);
    check_ident("textureBuffer");
}

#[test]
fn lex_itexture_buffer() {
    check_vulkan("itextureBuffer", Token::ITextureBuffer);
    check_ident("itextureBuffer");
}

#[test]
fn lex_utexture_buffer() {
    check_vulkan("utextureBuffer", Token::UTextureBuffer);
    check_ident("utextureBuffer");
}

#[test]
fn lex_sampler() {
    check_vulkan("sampler", Token::Sampler);
    check_ident("sampler");
}

#[test]
fn lex_sampler_shadow() {
    check_vulkan("samplerShadow", Token::SamplerShadow);
    check_ident("samplerShadow");
}

#[test]
fn lex_subpass_input() {
    check_vulkan("subpassInput", Token::SubpassInput);
    check_ident("subpassInput");
}

#[test]
fn lex_isubpass_input() {
    check_vulkan("isubpassInput", Token::ISubpassInput);
    check_ident("isubpassInput");
}

#[test]
fn lex_usubpass_input() {
    check_vulkan("usubpassInput", Token::USubpassInput);
    check_ident("usubpassInput");
}

#[test]
fn lex_subpass_input_ms() {
    check_vulkan("subpassInputMS", Token::SubpassInputMs);
    check_ident("subpassInputMS");
}

#[test]
fn lex_isubpass_input_ms() {
    check_vulkan("isubpassInputMS", Token::ISubpassInputMs);
    check_ident("isubpassInputMS");
}

#[test]
fn lex_usubpass_input_ms() {
    check_vulkan("usubpassInputMS", Token::USubpassInputMs);
    check_ident("usubpassInputMS");
}
// End Vulkan-target keywords

#[test]
fn lex_struct() {
    check("struct", Token::Struct);
}

#[test]
fn lex_void() {
    check("void", Token::Void);
}

#[test]
fn lex_while() {
    check("while", Token::While);
}

#[test]
fn lex_break() {
    check("break", Token::Break);
}

#[test]
fn lex_continue() {
    check("continue", Token::Continue);
}

#[test]
fn lex_do() {
    check("do", Token::Do);
}

#[test]
fn lex_else() {
    check("else", Token::Else);
}

#[test]
fn lex_for() {
    check("for", Token::For);
}

#[test]
fn lex_if() {
    check("if", Token::If);
}

#[test]
fn lex_discard() {
    check("discard", Token::Discard);
}

#[test]
fn lex_return() {
    check("return", Token::Return);
}

#[test]
fn lex_switch() {
    check("switch", Token::Switch);
}

#[test]
fn lex_case() {
    check("case", Token::Case);
}

#[test]
fn lex_default() {
    check("default", Token::Default);
}

#[test]
fn lex_subroutine() {
    check("subroutine", Token::Subroutine);
}

#[test]
fn lex_identifier() {
    check_ident("a");
    check_ident("ab_cd");
    check_ident("Ab_cd");
    check_ident("Ab_c8d");
    check_ident("Ab_c8d9");
}

#[test]
fn lex_float_constant() {
    check("0.", Token::FloatConstant(0.));
    check(".0", Token::FloatConstant(0.));
    check(".035", Token::FloatConstant(0.035));
    check("0.", Token::FloatConstant(0.));
    check("0.035", Token::FloatConstant(0.035));
    check(".035f", Token::FloatConstant(0.035));
    check("0.f", Token::FloatConstant(0.));
    check("314.f", Token::FloatConstant(314.));
    check("0.035f", Token::FloatConstant(0.035));
    check(".035F", Token::FloatConstant(0.035));
    check("0.F", Token::FloatConstant(0.));
    check("0.035F", Token::FloatConstant(0.035));
    check("1.03e+34", Token::FloatConstant(1.03e+34));
    check("1.03E+34", Token::FloatConstant(1.03E+34));
    check("1.03e-34", Token::FloatConstant(1.03e-34));
    check("1.03E-34", Token::FloatConstant(1.03E-34));
    check("1.03e+34f", Token::FloatConstant(1.03e+34));
    check("1.03E+34f", Token::FloatConstant(1.03E+34));
    check("1.03e-34f", Token::FloatConstant(1.03e-34));
    check("1.03E-34f", Token::FloatConstant(1.03E-34));
    check("1.03e+34F", Token::FloatConstant(1.03e+34));
    check("1.03E+34F", Token::FloatConstant(1.03E+34));
    check("1.03e-34F", Token::FloatConstant(1.03e-34));
    check("1.03E-34F", Token::FloatConstant(1.03E-34));
}

#[test]
fn lex_int_constant() {
    check("0", Token::IntConstant(0));
    check("012 ", Token::IntConstant(0o12));
    check("03", Token::IntConstant(0o3));
    check("07654321", Token::IntConstant(0o7654321));
    check("076556", Token::IntConstant(0o76556));
    check("0x0123789", Token::IntConstant(0x0123789));
    check("0x3", Token::IntConstant(0x3));
    check("0x9ABCDEF", Token::IntConstant(0x9ABCDEF));
    check("0x9abcdef", Token::IntConstant(0x9abcdef));
    check("0xABCDEF", Token::IntConstant(0xabcdef));
    check("0xabcdef", Token::IntConstant(0xabcdef));
    check("0xffffffff", Token::IntConstant(0xffffffffu32 as i32));
    check("123456", Token::IntConstant(123456));
    check("13", Token::IntConstant(13));
    check("3", Token::IntConstant(3));
    check("42", Token::IntConstant(42));
}

#[test]
fn lex_uint_constant() {
    check("0xffffffffU", Token::UIntConstant(0xffffffffu32));
}

#[test]
fn lex_double_constant() {
    check("0.lf", Token::DoubleConstant(0.));
    check("0.035lf", Token::DoubleConstant(0.035));
    check(".035lf", Token::DoubleConstant(0.035));
    check(".035LF", Token::DoubleConstant(0.035));
    check("0.LF", Token::DoubleConstant(0.));
    check("0.035LF", Token::DoubleConstant(0.035));
    check("1.03e+34lf", Token::DoubleConstant(1.03e+34));
    check("1.03E+34lf", Token::DoubleConstant(1.03E+34));
    check("1.03e-34lf", Token::DoubleConstant(1.03e-34));
    check("1.03E-34lf", Token::DoubleConstant(1.03E-34));
    check("1.03e+34LF", Token::DoubleConstant(1.03e+34));
    check("1.03E+34LF", Token::DoubleConstant(1.03E+34));
    check("1.03e-34LF", Token::DoubleConstant(1.03e-34));
    check("1.03E-34LF", Token::DoubleConstant(1.03E-34));
}

#[test]
fn lex_bool_constant() {
    check("false", Token::BoolConstant(false));
    check("true", Token::BoolConstant(true));
}

#[test]
fn lex_left_op() {
    check("<<", Token::LeftOp);
}

#[test]
fn lex_right_op() {
    check(">>", Token::RightOp);
}

#[test]
fn lex_inc_op() {
    check("++", Token::IncOp);
}

#[test]
fn lex_dec_op() {
    check("--", Token::DecOp);
}

#[test]
fn lex_le_op() {
    check("<=", Token::LeOp);
}

#[test]
fn lex_ge_op() {
    check(">=", Token::GeOp);
}

#[test]
fn lex_eq_op() {
    check("==", Token::EqOp);
}

#[test]
fn lex_ne_op() {
    check("!=", Token::NeOp);
}

#[test]
fn lex_and_op() {
    check("&&", Token::AndOp);
}

#[test]
fn lex_or_op() {
    check("||", Token::OrOp);
}

#[test]
fn lex_xor_op() {
    check("^^", Token::XorOp);
}

#[test]
fn lex_mul_assign() {
    check("*=", Token::MulAssign);
}

#[test]
fn lex_div_assign() {
    check("/=", Token::DivAssign);
}

#[test]
fn lex_add_assign() {
    check("+=", Token::AddAssign);
}

#[test]
fn lex_mod_assign() {
    check("%=", Token::ModAssign);
}

#[test]
fn lex_left_assign() {
    check("<<=", Token::LeftAssign);
}

#[test]
fn lex_right_assign() {
    check(">>=", Token::RightAssign);
}

#[test]
fn lex_and_assign() {
    check("&=", Token::AndAssign);
}

#[test]
fn lex_xor_assign() {
    check("^=", Token::XorAssign);
}

#[test]
fn lex_or_assign() {
    check("|=", Token::OrAssign);
}

#[test]
fn lex_sub_assign() {
    check("-=", Token::SubAssign);
}

#[test]
fn lex_left_paren() {
    check("(", Token::LeftParen);
}

#[test]
fn lex_right_paren() {
    check(")", Token::RightParen);
}

#[test]
fn lex_left_bracket() {
    check("[", Token::LeftBracket);
}

#[test]
fn lex_right_bracket() {
    check("]", Token::RightBracket);
}

#[test]
fn lex_left_brace() {
    check("{", Token::LeftBrace);
}

#[test]
fn lex_right_brace() {
    check("}", Token::RightBrace);
}

#[test]
fn lex_dot() {
    check(".", Token::Dot);
}

#[test]
fn lex_comma() {
    check(",", Token::Comma);
}

#[test]
fn lex_colon() {
    check(":", Token::Colon);
}

#[test]
fn lex_equal() {
    check("=", Token::Equal);
}

#[test]
fn lex_semicolon() {
    check(";", Token::Semicolon);
}

#[test]
fn lex_bang() {
    check("!", Token::Bang);
}

#[test]
fn lex_dash() {
    check("-", Token::Dash);
}

#[test]
fn lex_tilde() {
    check("~", Token::Tilde);
}

#[test]
fn lex_plus() {
    check("+", Token::Plus);
}

#[test]
fn lex_star() {
    check("*", Token::Star);
}

#[test]
fn lex_slash() {
    check("/", Token::Slash);
}

#[test]
fn lex_percent() {
    check("%", Token::Percent);
}

#[test]
fn lex_left_angle() {
    check("<", Token::LeftAngle);
}

#[test]
fn lex_right_angle() {
    check(">", Token::RightAngle);
}

#[test]
fn lex_vertical_bar() {
    check("|", Token::VerticalBar);
}

#[test]
fn lex_caret() {
    check("^", Token::Caret);
}

#[test]
fn lex_ampersand() {
    check("&", Token::Ampersand);
}

#[test]
fn lex_question() {
    check("?", Token::Question);
}

#[test]
fn lex_invariant() {
    check("invariant", Token::Invariant);
}

#[test]
fn lex_precise() {
    check("precise", Token::Precise);
}

#[test]
fn lex_high_precision() {
    check("highp", Token::HighPrecision);
}

#[test]
fn lex_medium_precision() {
    check("mediump", Token::MediumPrecision);
}

#[test]
fn lex_low_precision() {
    check("lowp", Token::LowPrecision);
}

#[test]
fn lex_precision() {
    check("precision", Token::Precision);
}
