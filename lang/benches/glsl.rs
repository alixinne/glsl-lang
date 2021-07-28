use criterion::{criterion_group, criterion_main, Criterion};

pub fn parse_impl<L, G>(c: &mut Criterion, input: &str, name: &str)
where
    L: glsl_lang::parse::Parse,
    G: glsl::parser::Parse,
{
    let mut group = c.benchmark_group(&if input.contains('\n') {
        name.to_owned()
    } else {
        format!("{}: {}", name, input)
    });

    group.bench_function("lalrpop", |b| {
        use glsl_lang::parse::LangParser;

        let parser = L::Parser::new();
        let opts = glsl_lang::parse::ParseOptions::new().build();

        b.iter(|| {
            L::parse_with_parser(input, &opts, &parser).ok();
        })
    });

    group.bench_function("glsl", |b| {
        b.iter(|| {
            G::parse(input).ok();
        })
    });

    group.finish();
}

#[cfg(feature = "parse-expr")]
pub fn parse_expr(c: &mut Criterion) {
    let input = "((((((((1.0f))))))))";
    parse_impl::<glsl_lang::ast::Expr, glsl::syntax::Expr>(c, input, "Expr")
}

pub fn parse_tu(c: &mut Criterion) {
    let input = "void main() { ((((((((1.0f)))))))); }";
    parse_impl::<glsl_lang::ast::TranslationUnit, glsl::syntax::TranslationUnit>(
        c,
        input,
        "TranslationUnit",
    )
}

pub fn parse_big_tu(c: &mut Criterion) {
    let input = include_str!("../../data/spv.400.frag");
    parse_impl::<glsl_lang::ast::TranslationUnit, glsl::syntax::TranslationUnit>(
        c,
        input,
        "spv.400.frag",
    )
}

#[cfg(feature = "parse-expr")]
criterion_group!(glsl, parse_expr, parse_tu, parse_big_tu);
#[cfg(not(feature = "parse-expr"))]
criterion_group!(glsl, parse_tu, parse_big_tu);

criterion_main!(glsl);
