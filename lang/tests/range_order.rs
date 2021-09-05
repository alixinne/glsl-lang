#[cfg(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
))]
fn test_inner() {
    use glsl_lang::{ast, parse::DefaultParse};

    let src = include_str!("../data/tests/range_order.glsl");

    assert!(ast::TranslationUnit::parse(src).is_ok());
}

#[cfg(not(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
)))]
fn test_inner() {}

#[test]
#[cfg_attr(
    not(any(
        feature = "lexer-v1",
        feature = "lexer-v2-min",
        feature = "lexer-v2-full"
    )),
    ignore = "a lexer is required for this test"
)]
fn dependent_defines() {
    test_inner();
}
