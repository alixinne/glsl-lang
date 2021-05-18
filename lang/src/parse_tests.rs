use lang_util::node::NodeContent;

use crate::{
    ast,
    parse::{Parsable, ParseContext, ParseOptions},
};

#[test]
fn parse_comments() {
    let opts = ParseOptions::new().with_comments();

    assert_eq!(
        ast::TranslationUnit::parse_with_options("// lol", &opts).map(|(_, opts)| opts
            .into_data()
            .unwrap()
            .comments
            .unwrap()
            .into_iter()
            .next()
            .unwrap()
            .1
            .content),
        Ok(ast::CommentData::Single(" lol".to_owned())),
    );

    assert_eq!(
        ast::TranslationUnit::parse_with_options("/* Something */\nvoid main() {}", &opts).map(
            |(_, opts)| opts
                .into_data()
                .unwrap()
                .comments
                .unwrap()
                .into_iter()
                .next()
                .unwrap()
                .1
                .content
        ),
        Ok(ast::CommentData::Multi(" Something ".to_owned())),
    );
}

#[test]
fn parse_unary_op() {
    assert_eq!(ast::UnaryOp::parse("+"), Ok(ast::UnaryOp::Add));
    assert_eq!(ast::UnaryOp::parse("-"), Ok(ast::UnaryOp::Minus));
    assert_eq!(ast::UnaryOp::parse("!"), Ok(ast::UnaryOp::Not));
    assert_eq!(ast::UnaryOp::parse("~"), Ok(ast::UnaryOp::Complement));
    assert_eq!(ast::UnaryOp::parse("++"), Ok(ast::UnaryOp::Inc));
    assert_eq!(ast::UnaryOp::parse("--"), Ok(ast::UnaryOp::Dec));
}

#[test]
fn parse_array_specifier_dimension_unsized() {
    assert_eq!(
        ast::ArraySpecifierDimension::parse("[]"),
        Ok(ast::ArraySpecifierDimension::Unsized)
    );
    assert_eq!(
        ast::ArraySpecifierDimension::parse("[ ]"),
        Ok(ast::ArraySpecifierDimension::Unsized)
    );
    assert_eq!(
        ast::ArraySpecifierDimension::parse("[\n]"),
        Ok(ast::ArraySpecifierDimension::Unsized)
    );
}

#[test]
fn parse_array_specifier_dimension_sized() {
    let ix = ast::Expr::IntConst(0);

    assert_eq!(
        ast::ArraySpecifierDimension::parse("[0]"),
        Ok(ast::ArraySpecifierDimension::ExplicitlySized(Box::new(
            ix.clone()
        )))
    );
    assert_eq!(
        ast::ArraySpecifierDimension::parse("[\n0   \t]"),
        Ok(ast::ArraySpecifierDimension::ExplicitlySized(Box::new(ix)))
    );
}

#[test]
fn parse_array_specifier_unsized() {
    assert_eq!(
        ast::ArraySpecifier::parse("[]"),
        Ok(ast::ArraySpecifier {
            dimensions: vec![ast::ArraySpecifierDimension::Unsized]
        })
    )
}

#[test]
fn parse_array_specifier_sized() {
    let ix = ast::Expr::IntConst(123);

    assert_eq!(
        ast::ArraySpecifier::parse("[123]"),
        Ok(ast::ArraySpecifier {
            dimensions: vec![ast::ArraySpecifierDimension::ExplicitlySized(Box::new(ix))]
        })
    )
}

#[test]
fn parse_array_specifier_sized_multiple() {
    let a = ast::Expr::IntConst(2);
    let b = ast::Expr::IntConst(100);
    let d = ast::Expr::IntConst(5);

    assert_eq!(
        ast::ArraySpecifier::parse("[2][100][][5]"),
        Ok(ast::ArraySpecifier {
            dimensions: vec![
                ast::ArraySpecifierDimension::ExplicitlySized(Box::new(a)),
                ast::ArraySpecifierDimension::ExplicitlySized(Box::new(b)),
                ast::ArraySpecifierDimension::Unsized,
                ast::ArraySpecifierDimension::ExplicitlySized(Box::new(d)),
            ]
        })
    )
}

#[test]
fn parse_interpolation_qualifier() {
    assert_eq!(
        ast::InterpolationQualifier::parse("smooth"),
        Ok(ast::InterpolationQualifier::Smooth)
    );
    assert_eq!(
        ast::InterpolationQualifier::parse("flat"),
        Ok(ast::InterpolationQualifier::Flat)
    );
    assert_eq!(
        ast::InterpolationQualifier::parse("noperspective"),
        Ok(ast::InterpolationQualifier::NoPerspective)
    );
}

#[test]
fn parse_precision_qualifier() {
    assert_eq!(
        ast::PrecisionQualifier::parse("highp"),
        Ok(ast::PrecisionQualifier::High)
    );
    assert_eq!(
        ast::PrecisionQualifier::parse("mediump"),
        Ok(ast::PrecisionQualifier::Medium)
    );
    assert_eq!(
        ast::PrecisionQualifier::parse("lowp"),
        Ok(ast::PrecisionQualifier::Low)
    );
}

#[test]
fn parse_storage_qualifier() {
    assert_eq!(
        ast::StorageQualifier::parse("const"),
        Ok(ast::StorageQualifier::Const)
    );
    assert_eq!(
        ast::StorageQualifier::parse("inout"),
        Ok(ast::StorageQualifier::InOut)
    );
    assert_eq!(
        ast::StorageQualifier::parse("in"),
        Ok(ast::StorageQualifier::In)
    );
    assert_eq!(
        ast::StorageQualifier::parse("out"),
        Ok(ast::StorageQualifier::Out)
    );
    assert_eq!(
        ast::StorageQualifier::parse("centroid"),
        Ok(ast::StorageQualifier::Centroid)
    );
    assert_eq!(
        ast::StorageQualifier::parse("patch"),
        Ok(ast::StorageQualifier::Patch)
    );
    assert_eq!(
        ast::StorageQualifier::parse("sample"),
        Ok(ast::StorageQualifier::Sample)
    );
    assert_eq!(
        ast::StorageQualifier::parse("uniform"),
        Ok(ast::StorageQualifier::Uniform)
    );
    assert_eq!(
        ast::StorageQualifier::parse("buffer"),
        Ok(ast::StorageQualifier::Buffer)
    );
    assert_eq!(
        ast::StorageQualifier::parse("shared"),
        Ok(ast::StorageQualifier::Shared)
    );
    assert_eq!(
        ast::StorageQualifier::parse("coherent"),
        Ok(ast::StorageQualifier::Coherent)
    );
    assert_eq!(
        ast::StorageQualifier::parse("volatile"),
        Ok(ast::StorageQualifier::Volatile)
    );
    assert_eq!(
        ast::StorageQualifier::parse("restrict"),
        Ok(ast::StorageQualifier::Restrict)
    );
    assert_eq!(
        ast::StorageQualifier::parse("readonly"),
        Ok(ast::StorageQualifier::ReadOnly)
    );
    assert_eq!(
        ast::StorageQualifier::parse("writeonly"),
        Ok(ast::StorageQualifier::WriteOnly)
    );
    assert_eq!(
        ast::StorageQualifier::parse("subroutine"),
        Ok(ast::StorageQualifier::Subroutine(vec![]))
    );
}

#[test]
fn parse_layout_qualifier_std430() {
    let expected = ast::LayoutQualifier {
        ids: vec![ast::LayoutQualifierSpec::Identifier(
            "std430".into_node(),
            None,
        )],
    };

    assert_eq!(
        ast::LayoutQualifier::parse("layout (std430)"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::LayoutQualifier::parse("layout  (std430   )"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::LayoutQualifier::parse("layout \n\t (  std430  )"),
        Ok(expected.clone())
    );
    assert_eq!(ast::LayoutQualifier::parse("layout(std430)"), Ok(expected));
}

#[test]
fn parse_layout_qualifier_shared() {
    let expected = ast::LayoutQualifier {
        ids: vec![ast::LayoutQualifierSpec::Shared],
    };

    assert_eq!(
        ast::LayoutQualifier::parse("layout (shared)"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::LayoutQualifier::parse("layout ( shared )"),
        Ok(expected.clone())
    );
    assert_eq!(ast::LayoutQualifier::parse("layout(shared)"), Ok(expected));
}

#[test]
fn parse_layout_qualifier_list() {
    let id_0 = ast::LayoutQualifierSpec::Shared;
    let id_1 = ast::LayoutQualifierSpec::Identifier("std140".into_node(), None);
    let id_2 = ast::LayoutQualifierSpec::Identifier(
        "max_vertices".into_node(),
        Some(Box::new(ast::Expr::IntConst(3))),
    );
    let expected = ast::LayoutQualifier {
        ids: vec![id_0, id_1, id_2],
    };

    assert_eq!(
        ast::LayoutQualifier::parse("layout (shared, std140, max_vertices = 3)"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::LayoutQualifier::parse("layout(shared,std140,max_vertices=3)"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::LayoutQualifier::parse("layout\n\n\t (    shared , std140, max_vertices= 3)"),
        Ok(expected)
    );
}

#[test]
fn parse_type_qualifier() {
    let storage_qual = ast::TypeQualifierSpec::Storage(ast::StorageQualifier::Const);
    let id_0 = ast::LayoutQualifierSpec::Shared;
    let id_1 = ast::LayoutQualifierSpec::Identifier("std140".into_node(), None);
    let id_2 = ast::LayoutQualifierSpec::Identifier(
        "max_vertices".into_node(),
        Some(Box::new(ast::Expr::IntConst(3))),
    );
    let layout_qual = ast::TypeQualifierSpec::Layout(ast::LayoutQualifier {
        ids: vec![id_0, id_1, id_2],
    });
    let expected: ast::TypeQualifier = ast::TypeQualifierData {
        qualifiers: vec![storage_qual, layout_qual],
    }
    .into();

    assert_eq!(
        ast::TypeQualifier::parse("const layout (shared, std140, max_vertices = 3)"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::TypeQualifier::parse("const layout(shared,std140,max_vertices=3)"),
        Ok(expected)
    );
}

#[test]
fn parse_struct_field_specifier() {
    let expected: ast::StructFieldSpecifier = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["foo".into_node()],
    }
    .into();

    assert_eq!(
        ast::StructFieldSpecifier::parse("vec4 foo;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::StructFieldSpecifier::parse("vec4     foo ;"),
        Ok(expected)
    );
}

#[test]
fn parse_struct_field_specifier_type_name() {
    let expected: ast::StructFieldSpecifier = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::TypeName("S0238_3".into_node()).into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["x".into_node()],
    }
    .into();

    let opts = get_s0238_3_opts();
    assert_eq!(
        ast::StructFieldSpecifier::parse_with_options("S0238_3 x;", &opts).map(|(p, _)| p),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::StructFieldSpecifier::parse_with_options("S0238_3     x ;", &opts).map(|(p, _)| p),
        Ok(expected)
    );
}

#[test]
fn parse_struct_field_specifier_several() {
    let expected: ast::StructFieldSpecifier = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["foo".into_node(), "bar".into_node(), "zoo".into_node()],
    }
    .into();

    assert_eq!(
        ast::StructFieldSpecifier::parse("vec4 foo, bar, zoo;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::StructFieldSpecifier::parse("vec4     foo , bar  , zoo ;"),
        Ok(expected)
    );
}

#[test]
fn parse_struct_specifier_one_field() {
    let field = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["foo".into_node()],
    };
    let expected: ast::StructSpecifier = ast::StructSpecifierData {
        name: Some("TestStruct".into_node()),
        fields: vec![field.into()],
    }
    .into();

    assert_eq!(
        ast::StructSpecifier::parse("struct TestStruct { vec4 foo; }"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::StructSpecifier::parse("struct      TestStruct \n \n\n {\n    vec4   foo  ;}"),
        Ok(expected)
    );
}

fn get_s0238_3_opts() -> ParseContext {
    let opts = ParseOptions::default().build();
    opts.add_type_name(ast::IdentifierData::from("S0238_3").into());
    opts
}

#[test]
fn parse_struct_specifier_multi_fields() {
    let foo_field = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["foo".into_node()],
    };
    let bar = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Float.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["bar".into_node()],
    };
    let zoo = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::UInt.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["zoo".into_node()],
    };
    let foobar = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::BVec3.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["foo_BAR_zoo3497_34".into_node()],
    };
    let s = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::TypeName("S0238_3".into_node()).into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["x".into_node()],
    };
    let expected: ast::StructSpecifier = ast::StructSpecifierData {
        name: Some("_TestStruct_934i".into_node()),
        fields: vec![
            foo_field.into(),
            bar.into(),
            zoo.into(),
            foobar.into(),
            s.into(),
        ],
    }
    .into();

    let opts = get_s0238_3_opts();
    assert_eq!(
      ast::StructSpecifier::parse_with_options(
        "struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }",
        &opts,
      ).map(|(p, _)| p),
      Ok(expected.clone()),
    );
    assert_eq!(
      ast::StructSpecifier::parse_with_options(
        "struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}",
        &opts,
      ).map(|(p, _)| p),
      Ok(expected.clone()),
    );
    assert_eq!(
      ast::StructSpecifier::parse_with_options(
        "struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}",
        &opts,
      ).map(|(p, _)| p),
      Ok(expected),
    );
}

#[test]
fn parse_type_specifier_non_array() {
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("bool"),
        Ok(ast::TypeSpecifierNonArrayData::Bool.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("int"),
        Ok(ast::TypeSpecifierNonArrayData::Int.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uint"),
        Ok(ast::TypeSpecifierNonArrayData::UInt.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("float"),
        Ok(ast::TypeSpecifierNonArrayData::Float.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("double"),
        Ok(ast::TypeSpecifierNonArrayData::Double.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("vec2"),
        Ok(ast::TypeSpecifierNonArrayData::Vec2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("vec3"),
        Ok(ast::TypeSpecifierNonArrayData::Vec3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("vec4"),
        Ok(ast::TypeSpecifierNonArrayData::Vec4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dvec2"),
        Ok(ast::TypeSpecifierNonArrayData::DVec2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dvec3"),
        Ok(ast::TypeSpecifierNonArrayData::DVec3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dvec4"),
        Ok(ast::TypeSpecifierNonArrayData::DVec4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("bvec2"),
        Ok(ast::TypeSpecifierNonArrayData::BVec2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("bvec3"),
        Ok(ast::TypeSpecifierNonArrayData::BVec3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("bvec4"),
        Ok(ast::TypeSpecifierNonArrayData::BVec4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("ivec2"),
        Ok(ast::TypeSpecifierNonArrayData::IVec2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("ivec3"),
        Ok(ast::TypeSpecifierNonArrayData::IVec3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("ivec4"),
        Ok(ast::TypeSpecifierNonArrayData::IVec4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uvec2"),
        Ok(ast::TypeSpecifierNonArrayData::UVec2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uvec3"),
        Ok(ast::TypeSpecifierNonArrayData::UVec3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uvec4"),
        Ok(ast::TypeSpecifierNonArrayData::UVec4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat2"),
        Ok(ast::TypeSpecifierNonArrayData::Mat2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat3"),
        Ok(ast::TypeSpecifierNonArrayData::Mat3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat4"),
        Ok(ast::TypeSpecifierNonArrayData::Mat4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat2x2"),
        Ok(ast::TypeSpecifierNonArrayData::Mat22.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat2x3"),
        Ok(ast::TypeSpecifierNonArrayData::Mat23.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat2x4"),
        Ok(ast::TypeSpecifierNonArrayData::Mat24.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat3x2"),
        Ok(ast::TypeSpecifierNonArrayData::Mat32.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat3x3"),
        Ok(ast::TypeSpecifierNonArrayData::Mat33.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat3x4"),
        Ok(ast::TypeSpecifierNonArrayData::Mat34.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat4x2"),
        Ok(ast::TypeSpecifierNonArrayData::Mat42.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat4x3"),
        Ok(ast::TypeSpecifierNonArrayData::Mat43.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("mat4x4"),
        Ok(ast::TypeSpecifierNonArrayData::Mat44.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat2"),
        Ok(ast::TypeSpecifierNonArrayData::DMat2.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat3"),
        Ok(ast::TypeSpecifierNonArrayData::DMat3.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat4"),
        Ok(ast::TypeSpecifierNonArrayData::DMat4.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat2x2"),
        Ok(ast::TypeSpecifierNonArrayData::DMat22.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat2x3"),
        Ok(ast::TypeSpecifierNonArrayData::DMat23.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat2x4"),
        Ok(ast::TypeSpecifierNonArrayData::DMat24.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat3x2"),
        Ok(ast::TypeSpecifierNonArrayData::DMat32.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat3x3"),
        Ok(ast::TypeSpecifierNonArrayData::DMat33.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat3x4"),
        Ok(ast::TypeSpecifierNonArrayData::DMat34.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat4x2"),
        Ok(ast::TypeSpecifierNonArrayData::DMat42.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat4x3"),
        Ok(ast::TypeSpecifierNonArrayData::DMat43.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("dmat4x4"),
        Ok(ast::TypeSpecifierNonArrayData::DMat44.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler1D"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler1D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image1D"),
        Ok(ast::TypeSpecifierNonArrayData::Image1D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2D"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image2D"),
        Ok(ast::TypeSpecifierNonArrayData::Image2D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler3D"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler3D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image3D"),
        Ok(ast::TypeSpecifierNonArrayData::Image3D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("samplerCube"),
        Ok(ast::TypeSpecifierNonArrayData::SamplerCube.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("imageCube"),
        Ok(ast::TypeSpecifierNonArrayData::ImageCube.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DRect"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DRect.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image2DRect"),
        Ok(ast::TypeSpecifierNonArrayData::Image2DRect.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler1DArray"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler1DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image1DArray"),
        Ok(ast::TypeSpecifierNonArrayData::Image1DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DArray"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image2DArray"),
        Ok(ast::TypeSpecifierNonArrayData::Image2DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("samplerBuffer"),
        Ok(ast::TypeSpecifierNonArrayData::SamplerBuffer.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("imageBuffer"),
        Ok(ast::TypeSpecifierNonArrayData::ImageBuffer.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DMS"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DMs.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image2DMS"),
        Ok(ast::TypeSpecifierNonArrayData::Image2DMs.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DMSArray"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DMsArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("image2DMSArray"),
        Ok(ast::TypeSpecifierNonArrayData::Image2DMsArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("samplerCubeArray"),
        Ok(ast::TypeSpecifierNonArrayData::SamplerCubeArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("imageCubeArray"),
        Ok(ast::TypeSpecifierNonArrayData::ImageCubeArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler1DShadow"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler1DShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DShadow"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DRectShadow"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DRectShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler1DArrayShadow"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler1DArrayShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("sampler2DArrayShadow"),
        Ok(ast::TypeSpecifierNonArrayData::Sampler2DArrayShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("samplerCubeShadow"),
        Ok(ast::TypeSpecifierNonArrayData::SamplerCubeShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("samplerCubeArrayShadow"),
        Ok(ast::TypeSpecifierNonArrayData::SamplerCubeArrayShadow.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler1D"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler1D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage1D"),
        Ok(ast::TypeSpecifierNonArrayData::IImage1D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler2D"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler2D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage2D"),
        Ok(ast::TypeSpecifierNonArrayData::IImage2D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler3D"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler3D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage3D"),
        Ok(ast::TypeSpecifierNonArrayData::IImage3D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isamplerCube"),
        Ok(ast::TypeSpecifierNonArrayData::ISamplerCube.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimageCube"),
        Ok(ast::TypeSpecifierNonArrayData::IImageCube.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler2DRect"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler2DRect.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage2DRect"),
        Ok(ast::TypeSpecifierNonArrayData::IImage2DRect.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler1DArray"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler1DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage1DArray"),
        Ok(ast::TypeSpecifierNonArrayData::IImage1DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler2DArray"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler2DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage2DArray"),
        Ok(ast::TypeSpecifierNonArrayData::IImage2DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isamplerBuffer"),
        Ok(ast::TypeSpecifierNonArrayData::ISamplerBuffer.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimageBuffer"),
        Ok(ast::TypeSpecifierNonArrayData::IImageBuffer.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler2DMS"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler2DMs.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage2DMS"),
        Ok(ast::TypeSpecifierNonArrayData::IImage2DMs.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isampler2DMSArray"),
        Ok(ast::TypeSpecifierNonArrayData::ISampler2DMsArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimage2DMSArray"),
        Ok(ast::TypeSpecifierNonArrayData::IImage2DMsArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("isamplerCubeArray"),
        Ok(ast::TypeSpecifierNonArrayData::ISamplerCubeArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("iimageCubeArray"),
        Ok(ast::TypeSpecifierNonArrayData::IImageCubeArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("atomic_uint"),
        Ok(ast::TypeSpecifierNonArrayData::AtomicUInt.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler1D"),
        Ok(ast::TypeSpecifierNonArrayData::USampler1D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage1D"),
        Ok(ast::TypeSpecifierNonArrayData::UImage1D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler2D"),
        Ok(ast::TypeSpecifierNonArrayData::USampler2D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage2D"),
        Ok(ast::TypeSpecifierNonArrayData::UImage2D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler3D"),
        Ok(ast::TypeSpecifierNonArrayData::USampler3D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage3D"),
        Ok(ast::TypeSpecifierNonArrayData::UImage3D.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usamplerCube"),
        Ok(ast::TypeSpecifierNonArrayData::USamplerCube.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimageCube"),
        Ok(ast::TypeSpecifierNonArrayData::UImageCube.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler2DRect"),
        Ok(ast::TypeSpecifierNonArrayData::USampler2DRect.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage2DRect"),
        Ok(ast::TypeSpecifierNonArrayData::UImage2DRect.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler1DArray"),
        Ok(ast::TypeSpecifierNonArrayData::USampler1DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage1DArray"),
        Ok(ast::TypeSpecifierNonArrayData::UImage1DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler2DArray"),
        Ok(ast::TypeSpecifierNonArrayData::USampler2DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage2DArray"),
        Ok(ast::TypeSpecifierNonArrayData::UImage2DArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usamplerBuffer"),
        Ok(ast::TypeSpecifierNonArrayData::USamplerBuffer.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimageBuffer"),
        Ok(ast::TypeSpecifierNonArrayData::UImageBuffer.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler2DMS"),
        Ok(ast::TypeSpecifierNonArrayData::USampler2DMs.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage2DMS"),
        Ok(ast::TypeSpecifierNonArrayData::UImage2DMs.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usampler2DMSArray"),
        Ok(ast::TypeSpecifierNonArrayData::USampler2DMsArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimage2DMSArray"),
        Ok(ast::TypeSpecifierNonArrayData::UImage2DMsArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("usamplerCubeArray"),
        Ok(ast::TypeSpecifierNonArrayData::USamplerCubeArray.into())
    );
    assert_eq!(
        ast::TypeSpecifierNonArray::parse("uimageCubeArray"),
        Ok(ast::TypeSpecifierNonArrayData::UImageCubeArray.into())
    );

    let opts = ParseOptions::default().build();
    opts.add_type_name(ast::IdentifierData::from("ReturnType").into());
    assert_eq!(
        ast::TypeSpecifierNonArray::parse_with_options("ReturnType", &opts).map(|(p, _)| p),
        Ok(
            ast::TypeSpecifierNonArrayData::TypeName(ast::TypeNameData::from("ReturnType").into())
                .into()
        )
    );
}

#[test]
fn parse_type_specifier() {
    assert_eq!(
        ast::TypeSpecifier::parse("uint"),
        Ok(ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::UInt.into(),
            array_specifier: None
        }
        .into())
    );
    assert_eq!(
        ast::TypeSpecifier::parse("iimage2DMSArray[35]"),
        Ok(ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::IImage2DMsArray.into(),
            array_specifier: Some(ast::ArraySpecifier {
                dimensions: vec![ast::ArraySpecifierDimension::ExplicitlySized(Box::new(
                    ast::Expr::IntConst(35)
                ))]
            })
        }
        .into())
    );
}

#[test]
fn parse_fully_specified_type() {
    let ty = ast::TypeSpecifierData {
        ty: ast::TypeSpecifierNonArrayData::IImage2DMsArray.into(),
        array_specifier: None,
    };
    let expected = ast::FullySpecifiedType {
        qualifier: None,
        ty: ty.into(),
    };

    assert_eq!(
        ast::FullySpecifiedType::parse("iimage2DMSArray"),
        Ok(expected)
    );
}

#[test]
fn parse_fully_specified_type_with_qualifier() {
    let opts = ParseOptions::new().build();
    let tn = opts.add_type_name(ast::IdentifierData::from("S032_29k").into());

    let qual_spec = ast::TypeQualifierSpec::Storage(ast::StorageQualifier::Subroutine(vec![
        ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec2).into(),
        ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::from(tn)).into(),
    ]));
    let qual = ast::TypeQualifierData {
        qualifiers: vec![qual_spec],
    }
    .into();
    let ty = ast::TypeSpecifierData {
        ty: ast::TypeSpecifierNonArrayData::IImage2DMsArray.into(),
        array_specifier: None,
    };
    let expected = ast::FullySpecifiedType {
        qualifier: Some(qual),
        ty: ty.into(),
    };

    assert_eq!(
        ast::FullySpecifiedType::parse_with_options(
            "subroutine (vec2, S032_29k) iimage2DMSArray",
            &opts,
        )
        .map(|(p, _)| p),
        Ok(expected.clone()),
    );
    assert_eq!(
        ast::FullySpecifiedType::parse_with_options(
            "subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray ",
            &opts,
        )
        .map(|(p, _)| p),
        Ok(expected.clone()),
    );
    assert_eq!(
        ast::FullySpecifiedType::parse_with_options(
            "subroutine(vec2,S032_29k)iimage2DMSArray",
            &opts,
        )
        .map(|(p, _)| p),
        Ok(expected),
    );
}

#[test]
fn parse_primary_expr_intconst() {
    assert_eq!(ast::Expr::parse("0 "), Ok(ast::Expr::IntConst(0)));
    assert_eq!(ast::Expr::parse("1 "), Ok(ast::Expr::IntConst(1)));
}

#[test]
fn parse_primary_expr_uintconst() {
    assert_eq!(ast::Expr::parse("0u "), Ok(ast::Expr::UIntConst(0)));
    assert_eq!(ast::Expr::parse("1u "), Ok(ast::Expr::UIntConst(1)));
}

#[test]
fn parse_primary_expr_floatconst() {
    assert_eq!(ast::Expr::parse("0.f "), Ok(ast::Expr::FloatConst(0.)));
    assert_eq!(ast::Expr::parse("1.f "), Ok(ast::Expr::FloatConst(1.)));
    assert_eq!(ast::Expr::parse("0.F "), Ok(ast::Expr::FloatConst(0.)));
    assert_eq!(ast::Expr::parse("1.F "), Ok(ast::Expr::FloatConst(1.)));
}

#[test]
fn parse_primary_expr_doubleconst() {
    assert_eq!(ast::Expr::parse("0. "), Ok(ast::Expr::FloatConst(0.)));
    assert_eq!(ast::Expr::parse("1. "), Ok(ast::Expr::FloatConst(1.)));
    assert_eq!(ast::Expr::parse("0.lf "), Ok(ast::Expr::DoubleConst(0.)));
    assert_eq!(ast::Expr::parse("1.lf "), Ok(ast::Expr::DoubleConst(1.)));
    assert_eq!(ast::Expr::parse("0.LF "), Ok(ast::Expr::DoubleConst(0.)));
    assert_eq!(ast::Expr::parse("1.LF "), Ok(ast::Expr::DoubleConst(1.)));
}

#[test]
fn parse_primary_expr_boolconst() {
    assert_eq!(
        ast::Expr::parse("false"),
        Ok(ast::Expr::BoolConst(false.to_owned()))
    );
    assert_eq!(
        ast::Expr::parse("true"),
        Ok(ast::Expr::BoolConst(true.to_owned()))
    );
}

#[test]
fn parse_primary_expr_parens() {
    assert_eq!(ast::Expr::parse("(0)"), Ok(ast::Expr::IntConst(0)));
    assert_eq!(ast::Expr::parse("(  0 )"), Ok(ast::Expr::IntConst(0)));
    assert_eq!(ast::Expr::parse("(  .0 )"), Ok(ast::Expr::FloatConst(0.)));
    assert_eq!(ast::Expr::parse("(  (.0) )"), Ok(ast::Expr::FloatConst(0.)));
    assert_eq!(ast::Expr::parse("(true) "), Ok(ast::Expr::BoolConst(true)));
}

#[test]
fn parse_postfix_function_call_no_args() {
    let fun = ast::FunIdentifier::TypeSpecifier(
        ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec3).into(),
    );
    let args = Vec::new();
    let expected = ast::Expr::FunCall(fun, args);

    assert_eq!(ast::Expr::parse("vec3()"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("vec3   (  ) "), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("vec3   (\nvoid\n) "), Ok(expected));
}

#[test]
fn parse_postfix_function_call_one_arg() {
    let fun = ast::FunIdentifier::ident("foo");
    let args = vec![ast::Expr::IntConst(0)];
    let expected = ast::Expr::FunCall(fun, args);

    assert_eq!(ast::Expr::parse("foo(0)"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("foo   ( 0 ) "), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("foo   (\n0\t\n) "), Ok(expected));
}

#[test]
fn parse_postfix_function_call_multi_arg() {
    let fun = ast::FunIdentifier::ident("foo");
    let args = vec![
        ast::Expr::IntConst(0),
        ast::Expr::BoolConst(false),
        ast::Expr::Variable("bar".into_node()),
    ];
    let expected = ast::Expr::FunCall(fun, args);

    assert_eq!(ast::Expr::parse("foo(0, false, bar)"), Ok(expected.clone()));
    assert_eq!(
        ast::Expr::parse("foo   ( 0\t, false    ,\t\tbar) "),
        Ok(expected)
    );
}

#[test]
fn parse_postfix_expr_bracket() {
    let id = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Bracket(Box::new(id), Box::new(ast::Expr::IntConst(7354)));

    assert_eq!(ast::Expr::parse("foo[7354]"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("foo[\n  7354    ]"), Ok(expected));
}

#[test]
fn parse_postfix_expr_dot() {
    let foo_var = Box::new(ast::Expr::Variable("foo".into_node()));
    let expected = ast::Expr::Dot(foo_var, "bar".into_node());

    assert_eq!(ast::Expr::parse("foo.bar"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("(foo).bar"), Ok(expected));
}

#[test]
fn parse_postfix_expr_dot_several() {
    let foo_var = Box::new(ast::Expr::Variable("foo".into_node()));
    let expected = ast::Expr::Dot(
        Box::new(ast::Expr::Dot(foo_var, "bar".into_node())),
        "zoo".into_node(),
    );

    assert_eq!(ast::Expr::parse("foo.bar.zoo"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("(foo).bar.zoo"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("(foo.bar).zoo"), Ok(expected));
}

#[test]
fn parse_postfix_postinc() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::PostInc(Box::new(foo_var));

    assert_eq!(ast::Expr::parse("foo++"), Ok(expected));
}

#[test]
fn parse_postfix_postdec() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::PostDec(Box::new(foo_var));

    assert_eq!(ast::Expr::parse("foo--"), Ok(expected));
}

#[test]
fn parse_unary_add() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Unary(ast::UnaryOp::Add, Box::new(foo_var));

    assert_eq!(ast::Expr::parse("+foo"), Ok(expected));
}

#[test]
fn parse_unary_minus() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Unary(ast::UnaryOp::Minus, Box::new(foo_var));

    assert_eq!(ast::Expr::parse("-foo"), Ok(expected));
}

#[test]
fn parse_unary_not() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Unary(ast::UnaryOp::Not, Box::new(foo_var));

    assert_eq!(ast::Expr::parse("!foo"), Ok(expected));
}

#[test]
fn parse_unary_complement() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Unary(ast::UnaryOp::Complement, Box::new(foo_var));

    assert_eq!(ast::Expr::parse("~foo"), Ok(expected));
}

#[test]
fn parse_unary_inc() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Unary(ast::UnaryOp::Inc, Box::new(foo_var));

    assert_eq!(ast::Expr::parse("++foo"), Ok(expected));
}

#[test]
fn parse_unary_dec() {
    let foo_var = ast::Expr::Variable("foo".into_node());
    let expected = ast::Expr::Unary(ast::UnaryOp::Dec, Box::new(foo_var));

    assert_eq!(ast::Expr::parse("--foo"), Ok(expected));
}

#[test]
fn parse_expr_float() {
    assert_eq!(ast::Expr::parse("314."), Ok(ast::Expr::FloatConst(314.)));
    assert_eq!(ast::Expr::parse("314.f"), Ok(ast::Expr::FloatConst(314.)));
    assert_eq!(ast::Expr::parse("314.LF"), Ok(ast::Expr::DoubleConst(314.)));
}

#[test]
fn parse_expr_add_2() {
    let one = Box::new(ast::Expr::IntConst(1));
    let expected = ast::Expr::Binary(ast::BinaryOp::Add, one.clone(), one);

    assert_eq!(ast::Expr::parse("1 + 1"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("1+1"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("(1 + 1)"), Ok(expected));
}

#[test]
fn parse_expr_add_3() {
    let one = Box::new(ast::Expr::UIntConst(1));
    let two = Box::new(ast::Expr::UIntConst(2));
    let three = Box::new(ast::Expr::UIntConst(3));
    let expected = ast::Expr::Binary(
        ast::BinaryOp::Add,
        Box::new(ast::Expr::Binary(ast::BinaryOp::Add, one, two)),
        three,
    );

    assert_eq!(ast::Expr::parse("1u + 2u + 3u"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("1u+2u+3u"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("((1u + 2u) + 3u)"), Ok(expected));
}

#[test]
fn parse_expr_add_mult_3() {
    let one = Box::new(ast::Expr::UIntConst(1));
    let two = Box::new(ast::Expr::UIntConst(2));
    let three = Box::new(ast::Expr::UIntConst(3));
    let expected = ast::Expr::Binary(
        ast::BinaryOp::Add,
        Box::new(ast::Expr::Binary(ast::BinaryOp::Mult, one, two)),
        three,
    );

    assert_eq!(ast::Expr::parse("1u * 2u + 3u"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("1u*2u+3u"), Ok(expected.clone()));
    assert_eq!(ast::Expr::parse("(1u * 2u) + 3u"), Ok(expected));
}

#[test]
fn parse_expr_add_sub_mult_div() {
    let one = Box::new(ast::Expr::IntConst(1));
    let two = Box::new(ast::Expr::IntConst(2));
    let three = Box::new(ast::Expr::IntConst(3));
    let four = Box::new(ast::Expr::IntConst(4));
    let five = Box::new(ast::Expr::IntConst(5));
    let six = Box::new(ast::Expr::IntConst(6));
    let expected = ast::Expr::Binary(
        ast::BinaryOp::Add,
        Box::new(ast::Expr::Binary(
            ast::BinaryOp::Mult,
            one,
            Box::new(ast::Expr::Binary(ast::BinaryOp::Add, two, three)),
        )),
        Box::new(ast::Expr::Binary(
            ast::BinaryOp::Div,
            four,
            Box::new(ast::Expr::Binary(ast::BinaryOp::Add, five, six)),
        )),
    );

    assert_eq!(ast::Expr::parse("1 * (2 + 3) + 4 / (5 + 6)"), Ok(expected));
}

#[test]
fn parse_complex_expr() {
    let input = "normalize((inverse(view) * vec4(ray.dir, 0.)).xyz)";
    let zero = ast::Expr::FloatConst(0.);
    let ray = ast::Expr::Variable("ray".into_node());
    let raydir = ast::Expr::Dot(Box::new(ray), "dir".into_node());
    let vec4 = ast::Expr::FunCall(
        ast::FunIdentifier::TypeSpecifier(
            ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec4).into(),
        ),
        vec![raydir, zero],
    );
    let view = ast::Expr::Variable("view".into_node());
    let iview = ast::Expr::FunCall(ast::FunIdentifier::ident("inverse"), vec![view]);
    let mul = ast::Expr::Binary(ast::BinaryOp::Mult, Box::new(iview), Box::new(vec4));
    let xyz = ast::Expr::Dot(Box::new(mul), "xyz".into_node());
    let normalize = ast::Expr::FunCall(ast::FunIdentifier::ident("normalize"), vec![xyz]);
    let expected = normalize;

    assert_eq!(ast::Expr::parse(&input), Ok(expected));
}

#[test]
fn parse_function_identifier_typename() {
    let expected = ast::FunIdentifier::ident("foo");
    assert_eq!(ast::FunIdentifier::parse("foo"), Ok(expected.clone()));
    assert_eq!(ast::FunIdentifier::parse("foo\n\t"), Ok(expected.clone()));
    assert_eq!(ast::FunIdentifier::parse("foo\n "), Ok(expected));
}

#[test]
fn parse_function_identifier_cast() {
    let expected = ast::FunIdentifier::TypeSpecifier(
        ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec3).into(),
    );
    assert_eq!(ast::FunIdentifier::parse("vec3"), Ok(expected.clone()));
    assert_eq!(ast::FunIdentifier::parse("vec3\t\n\n \t"), Ok(expected));
}

#[test]
fn parse_function_identifier_cast_array_unsized() {
    let expected = ast::FunIdentifier::TypeSpecifier(
        ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec3.into(),
            array_specifier: Some(ast::ArraySpecifier {
                dimensions: vec![ast::ArraySpecifierDimension::Unsized],
            }),
        }
        .into(),
    );

    assert_eq!(ast::FunIdentifier::parse("vec3[]"), Ok(expected.clone()));
    assert_eq!(ast::FunIdentifier::parse("vec3  [\t\n]"), Ok(expected));
}

#[test]
fn parse_function_identifier_cast_array_sized() {
    let expected = ast::FunIdentifier::TypeSpecifier(
        ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec3.into(),
            array_specifier: Some(ast::ArraySpecifier {
                dimensions: vec![ast::ArraySpecifierDimension::ExplicitlySized(Box::new(
                    ast::Expr::IntConst(12),
                ))],
            }),
        }
        .into(),
    );

    assert_eq!(ast::FunIdentifier::parse("vec3[12]"), Ok(expected.clone()));
    assert_eq!(ast::FunIdentifier::parse("vec3  [\t 12\n]"), Ok(expected));
}

#[test]
fn parse_assignment_op() {
    assert_eq!(ast::AssignmentOp::parse("="), Ok(ast::AssignmentOp::Equal));
    assert_eq!(ast::AssignmentOp::parse("*="), Ok(ast::AssignmentOp::Mult));
    assert_eq!(ast::AssignmentOp::parse("/="), Ok(ast::AssignmentOp::Div));
    assert_eq!(ast::AssignmentOp::parse("%="), Ok(ast::AssignmentOp::Mod));
    assert_eq!(ast::AssignmentOp::parse("+="), Ok(ast::AssignmentOp::Add));
    assert_eq!(ast::AssignmentOp::parse("-="), Ok(ast::AssignmentOp::Sub));
    assert_eq!(
        ast::AssignmentOp::parse("<<="),
        Ok(ast::AssignmentOp::LShift)
    );
    assert_eq!(
        ast::AssignmentOp::parse(">>="),
        Ok(ast::AssignmentOp::RShift)
    );
    assert_eq!(ast::AssignmentOp::parse("&="), Ok(ast::AssignmentOp::And));
    assert_eq!(ast::AssignmentOp::parse("^="), Ok(ast::AssignmentOp::Xor));
    assert_eq!(ast::AssignmentOp::parse("|="), Ok(ast::AssignmentOp::Or));
}

#[test]
fn parse_expr_statement() {
    let expected = ast::ExprStatement(Some(ast::Expr::Assignment(
        Box::new(ast::Expr::Variable("foo".into_node())),
        ast::AssignmentOp::Equal,
        Box::new(ast::Expr::FloatConst(314.)),
    )));

    assert_eq!(
        ast::ExprStatement::parse("foo = 314.f;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::ExprStatement::parse("foo=314.f;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::ExprStatement::parse("foo\n\t=  \n314.f;"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_function_prototype() {
    let rt = ast::FullySpecifiedType {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec3.into(),
            array_specifier: None,
        }
        .into(),
    };
    let arg0_ty = ast::TypeSpecifierData {
        ty: ast::TypeSpecifierNonArrayData::Vec2.into(),
        array_specifier: None,
    };
    let arg0 = ast::FunctionParameterDeclarationData::Unnamed(None, arg0_ty.into());
    let qual_spec = ast::TypeQualifierSpec::Storage(ast::StorageQualifier::Out);
    let qual = ast::TypeQualifierData {
        qualifiers: vec![qual_spec],
    }
    .into();
    let arg1 = ast::FunctionParameterDeclarationData::Named(
        Some(qual),
        ast::FunctionParameterDeclarator {
            ty: ast::TypeSpecifierData {
                ty: ast::TypeSpecifierNonArrayData::Float.into(),
                array_specifier: None,
            }
            .into(),
            ident: "the_arg".into_node(),
        },
    );
    let fp = ast::FunctionPrototypeData {
        ty: rt,
        name: "foo".into_node(),
        parameters: vec![arg0.into(), arg1.into()],
    };
    let expected: ast::Declaration = ast::DeclarationData::FunctionPrototype(fp.into()).into();

    assert_eq!(
        ast::Declaration::parse("vec3 foo(vec2, out float the_arg);"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::Declaration::parse("vec3 \nfoo ( vec2\n, out float \n\tthe_arg )\n;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::Declaration::parse("vec3 foo(vec2,out float the_arg);"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_init_declarator_list_single() {
    let ty = ast::FullySpecifiedType {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Int.into(),
            array_specifier: None,
        }
        .into(),
    };
    let sd = ast::SingleDeclaration {
        ty,
        name: Some("foo".into_node()),
        array_specifier: None,
        initializer: Some(ast::Initializer::Simple(Box::new(ast::Expr::IntConst(34)))),
    };
    let idl = ast::InitDeclaratorList {
        head: sd,
        tail: Vec::new(),
    };
    let expected: ast::Declaration = ast::DeclarationData::InitDeclaratorList(idl).into();

    assert_eq!(
        ast::Declaration::parse("int foo = 34;"),
        Ok(expected.clone())
    );
    assert_eq!(ast::Declaration::parse("int foo=34;"), Ok(expected.clone()));
    assert_eq!(
        ast::Declaration::parse("int    \t  \nfoo =\t34  ;"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_init_declarator_list_complex() {
    let ty = ast::FullySpecifiedType {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Int.into(),
            array_specifier: None,
        }
        .into(),
    };
    let sd = ast::SingleDeclaration {
        ty,
        name: Some("foo".into_node()),
        array_specifier: None,
        initializer: Some(ast::Initializer::Simple(Box::new(ast::Expr::IntConst(34)))),
    };
    let sdnt = ast::SingleDeclarationNoType {
        ident: "bar".into_node(),
        initializer: Some(ast::Initializer::Simple(Box::new(ast::Expr::IntConst(12)))),
    };
    let expected: ast::Declaration =
        ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
            head: sd,
            tail: vec![sdnt],
        })
        .into();

    assert_eq!(
        ast::Declaration::parse("int foo = 34, bar = 12;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::Declaration::parse("int foo=34,bar=12;"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::Declaration::parse("int    \t  \nfoo =\t34 \n,\tbar=      12\n ;"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_precision_low() {
    let qual = ast::PrecisionQualifier::Low;
    let ty = ast::TypeSpecifierData {
        ty: ast::TypeSpecifierNonArrayData::Float.into(),
        array_specifier: None,
    };
    let expected: ast::Declaration = ast::DeclarationData::Precision(qual, ty.into()).into();

    assert_eq!(
        ast::Declaration::parse("precision lowp float;"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_precision_medium() {
    let qual = ast::PrecisionQualifier::Medium;
    let ty = ast::TypeSpecifierData {
        ty: ast::TypeSpecifierNonArrayData::Float.into(),
        array_specifier: None,
    };
    let expected: ast::Declaration = ast::DeclarationData::Precision(qual, ty.into()).into();

    assert_eq!(
        ast::Declaration::parse("precision mediump float;"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_precision_high() {
    let qual = ast::PrecisionQualifier::High;
    let ty = ast::TypeSpecifierData {
        ty: ast::TypeSpecifierNonArrayData::Float.into(),
        array_specifier: None,
    };
    let expected: ast::Declaration = ast::DeclarationData::Precision(qual, ty.into()).into();

    assert_eq!(
        ast::Declaration::parse("precision highp float;"),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_uniform_block() {
    let opts = ParseOptions::new().build();
    let foo_var = opts.add_type_name(ast::IdentifierData::from("foo").into());

    let qual_spec = ast::TypeQualifierSpec::Storage(ast::StorageQualifier::Uniform);
    let qual = ast::TypeQualifierData {
        qualifiers: vec![qual_spec],
    }
    .into();
    let f0 = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Float.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["a".into_node()],
    };
    let f1 = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec3.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["b".into_node()],
    };
    let f2 = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::TypeName(foo_var).into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["c".into_node(), "d".into_node()],
    };
    let expected: ast::Declaration = ast::DeclarationData::Block(ast::Block {
        qualifier: qual,
        name: "UniformBlockTest".into_node(),
        fields: vec![f0.into(), f1.into(), f2.into()],
        identifier: None,
    })
    .into();

    assert_eq!(
        ast::Declaration::parse_with_options(
            "uniform UniformBlockTest { float a; vec3 b; foo c, d; };",
            &opts
        )
        .map(|(p, _)| p),
        Ok(expected.clone()),
    );

    assert_eq!(
        ast::Declaration::parse_with_options(
            "uniform   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;",
            &opts,
        ).map(|(p, _)| p),
        Ok(expected)
    );
}

#[test]
fn parse_declaration_buffer_block() {
    let opts = ParseOptions::new().build();
    let foo_var = opts.add_type_name(ast::IdentifierData::from("foo").into());

    let qual_spec = ast::TypeQualifierSpec::Storage(ast::StorageQualifier::Buffer);
    let qual = ast::TypeQualifierData {
        qualifiers: vec![qual_spec],
    }
    .into();
    let f0 = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Float.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["a".into_node()],
    };
    let f1 = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::Vec3.into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec![ast::ArrayedIdentifierData::new(
            "b".into_node(),
            Some(ast::ArraySpecifier {
                dimensions: vec![ast::ArraySpecifierDimension::Unsized],
            }),
        )
        .into()],
    };
    let f2 = ast::StructFieldSpecifierData {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::TypeName(foo_var).into(),
            array_specifier: None,
        }
        .into(),
        identifiers: vec!["c".into_node(), "d".into_node()],
    };
    let expected: ast::Declaration = ast::DeclarationData::Block(ast::Block {
        qualifier: qual,
        name: "UniformBlockTest".into_node(),
        fields: vec![f0.into(), f1.into(), f2.into()],
        identifier: None,
    })
    .into();

    assert_eq!(
        ast::Declaration::parse_with_options(
            "buffer UniformBlockTest { float a; vec3 b[]; foo c, d; };",
            &opts,
        )
        .map(|(p, _)| p),
        Ok(expected.clone()),
    );

    assert_eq!(
        ast::Declaration::parse_with_options(
            "buffer   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b   [   ]\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;",
            &opts,
        ).map(|(p, _)| p),
        Ok(expected),
    );
}

#[test]
fn parse_selection_statement_if() {
    let cond = ast::Expr::Binary(
        ast::BinaryOp::Lt,
        Box::new(ast::Expr::Variable("foo".into_node())),
        Box::new(ast::Expr::IntConst(10)),
    );
    let ret = Box::new(ast::Expr::BoolConst(false));
    let st = ast::StatementData::Jump(ast::JumpStatement::Return(Some(ret)));
    let body = ast::StatementData::Compound(
        ast::CompoundStatementData {
            statement_list: vec![st.into()],
        }
        .into(),
    );
    let rest = ast::SelectionRestStatement::Statement(Box::new(body.into()));
    let expected = ast::SelectionStatement {
        cond: Box::new(cond),
        rest,
    };

    assert_eq!(
        ast::SelectionStatement::parse("if (foo < 10) { return false; }"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::SelectionStatement::parse("if \n(foo<10\n) \t{return false;}"),
        Ok(expected)
    );
}

#[test]
fn parse_selection_statement_if_else() {
    let cond = ast::Expr::Binary(
        ast::BinaryOp::Lt,
        Box::new(ast::Expr::Variable("foo".into_node())),
        Box::new(ast::Expr::IntConst(10)),
    );
    let if_ret = Box::new(ast::Expr::FloatConst(0.));
    let if_st = ast::StatementData::Jump(ast::JumpStatement::Return(Some(if_ret)));
    let if_body = ast::StatementData::Compound(
        ast::CompoundStatementData {
            statement_list: vec![if_st.into()],
        }
        .into(),
    );
    let else_ret = Box::new(ast::Expr::Variable("foo".into_node()));
    let else_st = ast::StatementData::Jump(ast::JumpStatement::Return(Some(else_ret)));
    let else_body = ast::StatementData::Compound(
        ast::CompoundStatementData {
            statement_list: vec![else_st.into()],
        }
        .into(),
    );
    let rest =
        ast::SelectionRestStatement::Else(Box::new(if_body.into()), Box::new(else_body.into()));
    let expected = ast::SelectionStatement {
        cond: Box::new(cond),
        rest,
    };

    assert_eq!(
        ast::SelectionStatement::parse("if (foo < 10) { return 0.f; } else { return foo; }"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::SelectionStatement::parse(
            "if \n(foo<10\n) \t{return 0.f\t;\n\n}\n else{\n\t return foo   ;}"
        ),
        Ok(expected)
    );
}

#[test]
fn parse_switch_statement_empty() {
    let head = Box::new(ast::Expr::Variable("foo".into_node()));
    let expected = ast::SwitchStatement {
        head,
        body: Vec::new(),
    };

    assert_eq!(
        ast::SwitchStatement::parse("switch (foo) {}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::SwitchStatement::parse("switch(foo){}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::SwitchStatement::parse("switch\n\n (  foo  \t   \n) { \n\n   }"),
        Ok(expected)
    );
}

#[test]
fn parse_switch_statement_cases() {
    let head = Box::new(ast::Expr::Variable("foo".into_node()));
    let case0 =
        ast::StatementData::CaseLabel(ast::CaseLabel::Case(Box::new(ast::Expr::IntConst(0))));
    let case1 =
        ast::StatementData::CaseLabel(ast::CaseLabel::Case(Box::new(ast::Expr::IntConst(1))));
    let ret = ast::StatementData::Jump(ast::JumpStatement::Return(Some(Box::new(
        ast::Expr::UIntConst(12),
    ))));
    let expected = ast::SwitchStatement {
        head,
        body: vec![case0.into(), case1.into(), ret.into()],
    };

    assert_eq!(
        ast::SwitchStatement::parse("switch (foo) { case 0: case 1: return 12u; }"),
        Ok(expected)
    );
}

#[test]
fn parse_case_label_def() {
    assert_eq!(ast::CaseLabel::parse("default:"), Ok(ast::CaseLabel::Def));
    assert_eq!(
        ast::CaseLabel::parse("default   :"),
        Ok(ast::CaseLabel::Def)
    );
}

#[test]
fn parse_case_label() {
    let expected = ast::CaseLabel::Case(Box::new(ast::Expr::IntConst(3)));

    assert_eq!(ast::CaseLabel::parse("case 3:"), Ok(expected.clone()));
    assert_eq!(ast::CaseLabel::parse("case\n\t 3   :"), Ok(expected));
}

#[test]
fn parse_iteration_statement_while_empty() {
    let cond = ast::Condition::Expr(ast::Expr::Binary(
        ast::BinaryOp::Gte,
        Box::new(ast::Expr::Variable("a".into_node())),
        Box::new(ast::Expr::Variable("b".into_node())),
    ));
    let st = ast::StatementData::Compound(
        ast::CompoundStatementData {
            statement_list: Vec::new(),
        }
        .into(),
    );
    let expected = ast::IterationStatement::While(cond, Box::new(st.into()));

    assert_eq!(
        ast::IterationStatement::parse("while (a >= b) {}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::IterationStatement::parse("while(a>=b){}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::IterationStatement::parse("while (  a >=\n\tb  )\t  {   \n}"),
        Ok(expected)
    );
}

#[test]
fn parse_iteration_statement_do_while_empty() {
    let st = ast::StatementData::Compound(
        ast::CompoundStatementData {
            statement_list: Vec::new(),
        }
        .into(),
    );
    let cond = Box::new(ast::Expr::Binary(
        ast::BinaryOp::Gte,
        Box::new(ast::Expr::Variable("a".into_node())),
        Box::new(ast::Expr::Variable("b".into_node())),
    ));
    let expected = ast::IterationStatement::DoWhile(Box::new(st.into()), cond);

    assert_eq!(
        ast::IterationStatement::parse("do {} while (a >= b);"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::IterationStatement::parse("do{}while(a>=b);"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::IterationStatement::parse("do \n {\n} while (  a >=\n\tb  )\t  \n;"),
        Ok(expected)
    );
}

#[test]
fn parse_iteration_statement_for_empty() {
    let init = ast::ForInitStatement::Declaration(Box::new(
        ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
            head: ast::SingleDeclaration {
                ty: ast::FullySpecifiedType {
                    qualifier: None,
                    ty: ast::TypeSpecifierData {
                        ty: ast::TypeSpecifierNonArrayData::Float.into(),
                        array_specifier: None,
                    }
                    .into(),
                },
                name: Some("i".into_node()),
                array_specifier: None,
                initializer: Some(ast::Initializer::Simple(Box::new(ast::Expr::FloatConst(
                    0.,
                )))),
            },
            tail: Vec::new(),
        })
        .into(),
    ));
    let rest = ast::ForRestStatement {
        condition: Some(ast::Condition::Expr(ast::Expr::Binary(
            ast::BinaryOp::Lte,
            Box::new(ast::Expr::Variable("i".into_node())),
            Box::new(ast::Expr::FloatConst(10.)),
        ))),
        post_expr: Some(Box::new(ast::Expr::Unary(
            ast::UnaryOp::Inc,
            Box::new(ast::Expr::Variable("i".into_node())),
        ))),
    };
    let st = ast::StatementData::Compound(
        ast::CompoundStatementData {
            statement_list: Vec::new(),
        }
        .into(),
    );
    let expected = ast::IterationStatement::For(init, rest, Box::new(st.into()));

    assert_eq!(
        ast::IterationStatement::parse("for (float i = 0.f; i <= 10.f; ++i) {}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::IterationStatement::parse("for(float i=0.f;i<=10.f;++i){}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::IterationStatement::parse(
            "for\n\t (  \t\n\nfloat \ni \t=\n0.f\n;\ni\t<=  10.f; \n++i\n)\n{\n}"
        ),
        Ok(expected)
    );
}

#[test]
fn parse_jump_continue() {
    assert_eq!(
        ast::JumpStatement::parse("continue;"),
        Ok(ast::JumpStatement::Continue)
    );
}

#[test]
fn parse_jump_break() {
    assert_eq!(
        ast::JumpStatement::parse("break;"),
        Ok(ast::JumpStatement::Break)
    );
}

#[test]
fn parse_jump_return() {
    let expected = ast::JumpStatement::Return(Some(Box::new(ast::Expr::IntConst(3))));
    assert_eq!(ast::JumpStatement::parse("return 3;"), Ok(expected));
}

#[test]
fn parse_jump_empty_return() {
    let expected: ast::Statement =
        ast::StatementData::Jump(ast::JumpStatement::Return(None)).into();
    assert_eq!(ast::Statement::parse("return;"), Ok(expected));
}

#[test]
fn parse_jump_discard() {
    assert_eq!(
        ast::JumpStatement::parse("discard;"),
        Ok(ast::JumpStatement::Discard)
    );
}

#[test]
fn parse_simple_statement_return() {
    let e = ast::Expr::BoolConst(false);
    let expected: ast::Statement =
        ast::StatementData::Jump(ast::JumpStatement::Return(Some(Box::new(e)))).into();

    assert_eq!(ast::Statement::parse("return false;"), Ok(expected));
}

#[test]
fn parse_compound_statement_empty() {
    let expected = ast::CompoundStatementData {
        statement_list: Vec::new(),
    }
    .into();

    assert_eq!(ast::CompoundStatement::parse("{}"), Ok(expected));
}

#[test]
fn parse_compound_statement() {
    let st0 = ast::StatementData::Selection(ast::SelectionStatement {
        cond: Box::new(ast::Expr::BoolConst(true)),
        rest: ast::SelectionRestStatement::Statement(Box::new(
            ast::StatementData::Compound(
                ast::CompoundStatementData {
                    statement_list: Vec::new(),
                }
                .into(),
            )
            .into(),
        )),
    });
    let st1 = ast::StatementData::Declaration(
        ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
            head: ast::SingleDeclaration {
                ty: ast::FullySpecifiedType {
                    qualifier: None,
                    ty: ast::TypeSpecifierData {
                        ty: ast::TypeSpecifierNonArrayData::ISampler3D.into(),
                        array_specifier: None,
                    }
                    .into(),
                },
                name: Some("x".into_node()),
                array_specifier: None,
                initializer: None,
            },
            tail: Vec::new(),
        })
        .into(),
    );
    let st2 = ast::StatementData::Jump(ast::JumpStatement::Return(Some(Box::new(
        ast::Expr::IntConst(42),
    ))));
    let expected: ast::CompoundStatement = ast::CompoundStatementData {
        statement_list: vec![st0.into(), st1.into(), st2.into()],
    }
    .into();

    assert_eq!(
        ast::CompoundStatement::parse("{ if (true) {} isampler3D x; return 42 ; }"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::CompoundStatement::parse("{if(true){}isampler3D x;return 42;}"),
        Ok(expected)
    );
}

#[test]
fn parse_function_definition() {
    let rt = ast::FullySpecifiedType {
        qualifier: None,
        ty: ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::IImage2DArray.into(),
            array_specifier: None,
        }
        .into(),
    };
    let fp = ast::FunctionPrototypeData {
        ty: rt,
        name: "foo".into_node(),
        parameters: Vec::new(),
    }
    .into();
    let st0 = ast::StatementData::Jump(ast::JumpStatement::Return(Some(Box::new(
        ast::Expr::Variable("bar".into_node()),
    ))));
    let expected: ast::FunctionDefinition = ast::FunctionDefinitionData {
        prototype: fp,
        statement: ast::CompoundStatementData {
            statement_list: vec![st0.into()],
        }
        .into(),
    }
    .into();

    assert_eq!(
        ast::FunctionDefinition::parse("iimage2DArray foo() { return bar; }"),
        Ok(expected.clone()),
    );
    assert_eq!(
        ast::FunctionDefinition::parse("iimage2DArray \tfoo\n()\n \n{\n return \nbar\n;}"),
        Ok(expected.clone())
    );
    assert_eq!(
        ast::FunctionDefinition::parse("iimage2DArray foo(){return bar;}"),
        Ok(expected)
    );
}

#[test]
fn parse_buffer_block_0() {
    let src = include_str!("../data/tests/buffer_block_0.glsl");
    let main_fn = ast::ExternalDeclarationData::FunctionDefinition(
        ast::FunctionDefinitionData {
            prototype: ast::FunctionPrototypeData {
                ty: ast::FullySpecifiedType {
                    qualifier: None,
                    ty: ast::TypeSpecifierData {
                        ty: ast::TypeSpecifierNonArrayData::Void.into(),
                        array_specifier: None,
                    }
                    .into(),
                },
                name: "main".into_node(),
                parameters: Vec::new(),
            }
            .into(),
            statement: ast::CompoundStatementData {
                statement_list: Vec::new(),
            }
            .into(),
        }
        .into(),
    );

    let buffer_block = ast::ExternalDeclarationData::Declaration(
        ast::DeclarationData::Block(ast::Block {
            qualifier: ast::TypeQualifierData {
                qualifiers: vec![ast::TypeQualifierSpec::Storage(
                    ast::StorageQualifier::Buffer,
                )],
            }
            .into(),
            name: "Foo".into_node(),
            fields: vec![ast::StructFieldSpecifierData {
                qualifier: None,
                ty: ast::TypeSpecifierData {
                    ty: ast::TypeSpecifierNonArrayData::Float.into(),
                    array_specifier: None,
                }
                .into(),
                identifiers: vec![ast::ArrayedIdentifierData::new(
                    "tiles".into_node(),
                    Some(ast::ArraySpecifier {
                        dimensions: vec![ast::ArraySpecifierDimension::Unsized],
                    }),
                )
                .into()],
            }
            .into()],
            identifier: Some("main_tiles".into_node()),
        })
        .into(),
    );

    let expected = ast::TranslationUnit(vec![buffer_block.into(), main_fn.into()]);

    assert_eq!(ast::TranslationUnit::parse(src), Ok(expected));
}

#[test]
fn parse_layout_buffer_block_0() {
    let src = include_str!("../data/tests/layout_buffer_block_0.glsl");
    let layout = ast::LayoutQualifier {
        ids: vec![
            ast::LayoutQualifierSpec::Identifier(
                "set".into_node(),
                Some(Box::new(ast::Expr::IntConst(0))),
            ),
            ast::LayoutQualifierSpec::Identifier(
                "binding".into_node(),
                Some(Box::new(ast::Expr::IntConst(0))),
            ),
        ],
    };
    let type_qual = ast::TypeQualifierData {
        qualifiers: vec![
            ast::TypeQualifierSpec::Layout(layout),
            ast::TypeQualifierSpec::Storage(ast::StorageQualifier::Buffer),
        ],
    }
    .into();
    let block = ast::ExternalDeclarationData::Declaration(
        ast::DeclarationData::Block(ast::Block {
            qualifier: type_qual,
            name: "Foo".into_node(),
            fields: vec![ast::StructFieldSpecifierData {
                qualifier: None,
                ty: ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Float).into(),
                identifiers: vec!["a".into_node()],
            }
            .into()],
            identifier: Some("foo".into_node()),
        })
        .into(),
    );

    let expected = ast::TranslationUnit(vec![block.into()]);

    assert_eq!(ast::TranslationUnit::parse(src), Ok(expected));
}

#[test]
fn parse_pp_version() {
    assert_eq!(
        ast::Preprocessor::parse("#version 450\n"),
        Ok(ast::PreprocessorData::Version(ast::PreprocessorVersion {
            version: 450,
            profile: None,
        })
        .into())
    );

    assert_eq!(
        ast::Preprocessor::parse("#version 450 core\n"),
        Ok(ast::PreprocessorData::Version(ast::PreprocessorVersion {
            version: 450,
            profile: Some(ast::PreprocessorVersionProfile::Core)
        })
        .into())
    );
}

#[test]
fn parse_pp_version_newline() {
    assert_eq!(
        ast::Preprocessor::parse("#version 450\n"),
        Ok(ast::PreprocessorData::Version(ast::PreprocessorVersion {
            version: 450,
            profile: None,
        })
        .into())
    );

    assert_eq!(
        ast::Preprocessor::parse("#version 450 core\n"),
        Ok(ast::PreprocessorData::Version(ast::PreprocessorVersion {
            version: 450,
            profile: Some(ast::PreprocessorVersionProfile::Core)
        })
        .into())
    );
}

#[test]
fn parse_pp_define() {
    let expect = |v: &str| {
        Ok(
            ast::PreprocessorData::Define(ast::PreprocessorDefine::ObjectLike {
                ident: "test".into_node(),
                value: v.to_owned(),
            })
            .into(),
        )
    };

    assert_eq!(ast::Preprocessor::parse("#define test 1.0"), expect("1.0"));
    assert_eq!(
        ast::Preprocessor::parse("#define test \\\n   1.0"),
        expect("1.0")
    );
    assert_eq!(
        ast::Preprocessor::parse("#define test 1.0\n"),
        expect("1.0")
    );

    assert_eq!(
        ast::Preprocessor::parse("#define test123 .0f\n"),
        Ok(
            ast::PreprocessorData::Define(ast::PreprocessorDefine::ObjectLike {
                ident: "test123".into_node(),
                value: ".0f".to_owned()
            })
            .into()
        )
    );

    assert_eq!(
        ast::Preprocessor::parse("#define test 1\n"),
        Ok(
            ast::PreprocessorData::Define(ast::PreprocessorDefine::ObjectLike {
                ident: "test".into_node(),
                value: "1".to_owned()
            })
            .into()
        )
    );

    let a = ast::PreprocessorData::Define(ast::PreprocessorDefine::ObjectLike {
        ident: "M_PI".into_node(),
        value: "3.14".to_owned(),
    })
    .into();
    let b = ast::PreprocessorData::Define(ast::PreprocessorDefine::ObjectLike {
        ident: "M_2PI".into_node(),
        value: "(2. * M_PI)".to_owned(),
    })
    .into();

    assert_eq!(
        ast::TranslationUnit::parse("#define M_PI 3.14\n#define M_2PI (2. * M_PI)\n"),
        Ok(ast::TranslationUnit(vec![
            ast::ExternalDeclarationData::Preprocessor(a).into(),
            ast::ExternalDeclarationData::Preprocessor(b).into()
        ]))
    );
}

#[test]
fn parse_pp_define_with_args() {
    let expected: ast::Preprocessor =
        ast::PreprocessorData::Define(ast::PreprocessorDefine::FunctionLike {
            ident: "add".into_node(),
            args: vec![
                ast::IdentifierData::from("x").into(),
                ast::IdentifierData::from("y").into(),
            ],
            value: "(x + y)".to_owned(),
        })
        .into();

    assert_eq!(
        ast::Preprocessor::parse("#define \\\n add(x, y) \\\n (x + y)"),
        Ok(expected.clone())
    );

    assert_eq!(
        ast::Preprocessor::parse("#define \\\n add(  x, y  ) \\\n (x + y)"),
        Ok(expected)
    );
}

#[test]
fn parse_pp_define_multiline() {
    assert_eq!(
        ast::Preprocessor::parse(
            r#"#define foo \
       32"#
        ),
        Ok(
            ast::PreprocessorData::Define(ast::PreprocessorDefine::ObjectLike {
                ident: "foo".into_node(),
                value: "32".to_owned(),
            })
            .into()
        )
    );
}

#[test]
fn parse_pp_else() {
    assert_eq!(
        ast::Preprocessor::parse("#    else\n"),
        Ok(ast::PreprocessorData::Else.into())
    );
}

#[test]
fn parse_pp_elif() {
    assert_eq!(
        ast::Preprocessor::parse("#   elif \\\n42\n"),
        Ok(ast::PreprocessorData::ElseIf(ast::PreprocessorElseIf {
            condition: "42".to_owned()
        })
        .into())
    );
}

#[test]
fn parse_pp_endif() {
    assert_eq!(
        ast::Preprocessor::parse("#\\\nendif"),
        Ok(ast::PreprocessorData::EndIf.into())
    );
}

#[test]
fn parse_pp_error() {
    assert_eq!(
        ast::Preprocessor::parse("#error \\\n     some message"),
        Ok(ast::PreprocessorData::Error(ast::PreprocessorError {
            message: "some message".to_owned()
        })
        .into())
    );
}

#[test]
fn parse_pp_if() {
    assert_eq!(
        ast::Preprocessor::parse("# \\\nif 42"),
        Ok(ast::PreprocessorData::If(ast::PreprocessorIf {
            condition: "42".to_owned()
        })
        .into())
    );
}

#[test]
fn parse_pp_ifdef() {
    assert_eq!(
        ast::Preprocessor::parse("#ifdef       FOO\n"),
        Ok(ast::PreprocessorData::IfDef(ast::PreprocessorIfDef {
            ident: ast::IdentifierData("FOO".into()).into()
        })
        .into())
    );
}

#[test]
fn parse_pp_ifndef() {
    assert_eq!(
        ast::Preprocessor::parse("#\\\nifndef \\\n   FOO\n"),
        Ok(ast::PreprocessorData::IfNDef(ast::PreprocessorIfNDef {
            ident: ast::IdentifierData("FOO".into()).into()
        })
        .into())
    );
}

#[test]
fn parse_pp_include() {
    assert_eq!(
        ast::Preprocessor::parse("#include <filename>\n"),
        Ok(ast::PreprocessorData::Include(ast::PreprocessorInclude {
            path: ast::PathData::Absolute("filename".to_owned()).into()
        })
        .into())
    );

    assert_eq!(
        ast::Preprocessor::parse("#include \\\n\"filename\"\n"),
        Ok(ast::PreprocessorData::Include(ast::PreprocessorInclude {
            path: ast::PathData::Relative("filename".to_owned()).into()
        })
        .into())
    );
}

#[test]
fn parse_pp_line() {
    assert_eq!(
        ast::Preprocessor::parse("#   line \\\n2\n"),
        Ok(ast::PreprocessorData::Line(ast::PreprocessorLine {
            line: 2,
            source_string_number: None,
        })
        .into())
    );

    assert_eq!(
        ast::Preprocessor::parse("#line 2 \\\n 4\n"),
        Ok(ast::PreprocessorData::Line(ast::PreprocessorLine {
            line: 2,
            source_string_number: Some(4),
        })
        .into())
    );
}

#[test]
fn parse_pp_pragma() {
    assert_eq!(
        ast::Preprocessor::parse("#\\\npragma  some   flag"),
        Ok(ast::PreprocessorData::Pragma(ast::PreprocessorPragma {
            command: "some   flag".to_owned()
        })
        .into())
    );
}

#[test]
fn parse_pp_undef() {
    assert_eq!(
        ast::Preprocessor::parse("# undef \\\n FOO"),
        Ok(ast::PreprocessorData::Undef(ast::PreprocessorUndef {
            name: ast::IdentifierData("FOO".into()).into()
        })
        .into())
    );
}

#[test]
fn parse_pp_extension() {
    assert_eq!(
        ast::Preprocessor::parse("#extension all: require\n"),
        Ok(
            ast::PreprocessorData::Extension(ast::PreprocessorExtension {
                name: ast::PreprocessorExtensionName::All,
                behavior: Some(ast::PreprocessorExtensionBehavior::Require)
            })
            .into()
        )
    );

    assert_eq!(
        ast::Preprocessor::parse("#extension GL_foobar: warn\n"),
        Ok(
            ast::PreprocessorData::Extension(ast::PreprocessorExtension {
                name: ast::PreprocessorExtensionName::Specific("GL_foobar".into()),
                behavior: Some(ast::PreprocessorExtensionBehavior::Warn)
            })
            .into()
        )
    );
}

#[test]
fn parse_dos_crlf() {
    assert!(ast::TranslationUnit::parse("#version 460 core\r\nvoid main(){}\r\n").is_ok());
}

#[test]
fn parse_dot_field_expr_array() {
    let src = "a[0].xyz";
    let expected = ast::Expr::Dot(
        Box::new(ast::Expr::Bracket(
            Box::new(ast::Expr::Variable("a".into_node())),
            Box::new(ast::Expr::IntConst(0)),
        )),
        "xyz".into_node(),
    );

    assert_eq!(ast::Expr::parse(src), Ok(expected));
}

#[test]
fn parse_dot_field_expr_statement() {
    let src = "vec3 v = smoothstep(vec3(border_width), vec3(0.0), v_barycenter).zyx;";
    let fun = ast::FunIdentifier::ident("smoothstep");
    let args = vec![
        ast::Expr::FunCall(
            ast::FunIdentifier::TypeSpecifier(
                ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec3).into(),
            ),
            vec![ast::Expr::Variable("border_width".into_node())],
        ),
        ast::Expr::FunCall(
            ast::FunIdentifier::TypeSpecifier(
                ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec3).into(),
            ),
            vec![ast::Expr::FloatConst(0.)],
        ),
        ast::Expr::Variable("v_barycenter".into_node()),
    ];
    let ini = ast::Initializer::Simple(Box::new(ast::Expr::Dot(
        Box::new(ast::Expr::FunCall(fun, args)),
        "zyx".into_node(),
    )));
    let sd = ast::SingleDeclaration {
        ty: ast::FullySpecifiedType {
            qualifier: None,
            ty: ast::TypeSpecifierData {
                ty: ast::TypeSpecifierNonArrayData::Vec3.into(),
                array_specifier: None,
            }
            .into(),
        },
        name: Some("v".into_node()),
        array_specifier: None,
        initializer: Some(ini),
    };
    let expected: ast::Statement = ast::StatementData::Declaration(
        ast::DeclarationData::InitDeclaratorList(ast::InitDeclaratorList {
            head: sd,
            tail: Vec::new(),
        })
        .into(),
    )
    .into();

    assert_eq!(ast::Statement::parse(src), Ok(expected));
}

#[test]
fn parse_arrayed_identifier() {
    let expected: ast::ArrayedIdentifier = ast::ArrayedIdentifierData::new(
        "foo".into_node(),
        ast::ArraySpecifier {
            dimensions: vec![ast::ArraySpecifierDimension::Unsized],
        },
    )
    .into();

    assert_eq!(ast::ArrayedIdentifier::parse("foo[]"), Ok(expected.clone()));
    assert_eq!(
        ast::ArrayedIdentifier::parse("foo \t\n  [\n\t ]"),
        Ok(expected)
    );
}

#[test]
fn parse_dangling_else() {
    // Checks that the else is attached to the closest if statement, following disambiguation rules
    // for C compilers.
    //
    // See https://www.cs.cornell.edu/andru/javaspec/19.doc.html for how the grammar was modeled for
    // a LALR parser.

    assert_eq!(
        ast::Statement::parse("if (ca) if (cb) ab(); else c();"),
        Ok(ast::StatementData::Selection(ast::SelectionStatement {
            cond: Box::new(ast::Expr::variable("ca")),
            rest: ast::SelectionRestStatement::Statement(Box::new(
                ast::StatementData::Selection(ast::SelectionStatement {
                    cond: Box::new(ast::Expr::variable("cb")),
                    rest: ast::SelectionRestStatement::Else(
                        Box::new(
                            ast::StatementData::Expression(ast::ExprStatement(Some(
                                ast::Expr::FunCall(ast::FunIdentifier::ident("ab"), vec![])
                            )))
                            .into()
                        ),
                        Box::new(
                            ast::StatementData::Expression(ast::ExprStatement(Some(
                                ast::Expr::FunCall(ast::FunIdentifier::ident("c"), vec![])
                            )))
                            .into()
                        ),
                    ),
                })
                .into()
            ))
        })
        .into())
    );
}
