#[test]
fn dependent_defines() {
    use glsl_lang::{ast, parse::DefaultParse};

    let src = include_str!("../data/tests/range_order.glsl");

    assert!(ast::TranslationUnit::parse(src).is_ok());
}
