use super::*;
use expect_test::expect;

fn check(actual: Ast, expect: expect_test::Expect) {
    expect.assert_eq(&format!("{:#?}", actual.into_inner().0));
}

fn parse(input: &str) -> crate::parser::Ast {
    crate::parser::Parser::new(input).parse()
}

#[test]
fn test_basic() {
    let src = "#define A\\\n H 2.32323e8 /* Some comment */
                A;\n";

    check(
        parse(src),
        expect![[r##"
            ROOT@0..62
              PP_DEFINE@0..43
                HASH@0..1 "#"
                IDENT_KW@1..7 "define"
                WS@7..8 " "
                IDENT_KW@8..11 "A\\\n"
                WS@11..12 " "
                PP_DEFINE_BODY@12..23
                  IDENT_KW@12..13 "H"
                  WS@13..14 " "
                  DIGITS@14..23 "2.32323e8"
                WS@23..24 " "
                COMMENT@24..42 "/* Some comment */"
                NEWLINE@42..43 "\n"
              WS@43..59 "                "
              IDENT_KW@59..60 "A"
              SEMICOLON@60..61 ";"
              NEWLINE@61..62 "\n"
        "##]],
    );
}

#[test]
fn test_include() {
    check(
        parse("#include <x/a.glsl>\n"),
        expect![[r##"
            ROOT@0..20
              PP_INCLUDE@0..20
                HASH@0..1 "#"
                IDENT_KW@1..8 "include"
                WS@8..9 " "
                PP_INCLUDE_PATH@9..19
                  ANGLE_STRING@9..19 "<x/a.glsl>"
                NEWLINE@19..20 "\n"
        "##]],
    );
    check(
        parse("#include \"y/b.glsl\"\n"),
        expect![[r##"
            ROOT@0..20
              PP_INCLUDE@0..20
                HASH@0..1 "#"
                IDENT_KW@1..8 "include"
                WS@8..9 " "
                PP_INCLUDE_PATH@9..19
                  QUOTE_STRING@9..19 "\"y/b.glsl\""
                NEWLINE@19..20 "\n"
        "##]],
    );
}

#[test]
fn test_define() {
    check(
        parse("#define A B\n"),
        expect![[r##"
            ROOT@0..12
              PP_DEFINE@0..12
                HASH@0..1 "#"
                IDENT_KW@1..7 "define"
                WS@7..8 " "
                IDENT_KW@8..9 "A"
                WS@9..10 " "
                PP_DEFINE_BODY@10..11
                  IDENT_KW@10..11 "B"
                NEWLINE@11..12 "\n"
        "##]],
    );
    check(
        parse("#define C(x) x\n"),
        expect![[r##"
            ROOT@0..15
              PP_DEFINE@0..15
                HASH@0..1 "#"
                IDENT_KW@1..7 "define"
                WS@7..8 " "
                IDENT_KW@8..9 "C"
                PP_DEFINE_ARGS@9..12
                  LPAREN@9..10 "("
                  PP_DEFINE_ARG@10..11
                    IDENT_KW@10..11 "x"
                  RPAREN@11..12 ")"
                WS@12..13 " "
                PP_DEFINE_BODY@13..14
                  IDENT_KW@13..14 "x"
                NEWLINE@14..15 "\n"
        "##]],
    );
}

#[test]
fn test_undef() {
    check(
        parse("#undef Y\n"),
        expect![[r##"
            ROOT@0..9
              PP_UNDEF@0..9
                HASH@0..1 "#"
                IDENT_KW@1..6 "undef"
                WS@6..7 " "
                PP_IDENT@7..8
                  IDENT_KW@7..8 "Y"
                NEWLINE@8..9 "\n"
        "##]],
    );
}

#[test]
fn test_line() {
    check(
        parse("#line 2 5\n"),
        expect![[r##"
            ROOT@0..10
              PP_LINE@0..10
                HASH@0..1 "#"
                IDENT_KW@1..5 "line"
                WS@5..6 " "
                PP_LINE_BODY@6..9
                  DIGITS@6..7 "2"
                  WS@7..8 " "
                  DIGITS@8..9 "5"
                NEWLINE@9..10 "\n"
        "##]],
    );
}

#[test]
fn test_error() {
    check(
        parse("#error hello\n"),
        expect![[r##"
            ROOT@0..13
              PP_ERROR@0..13
                HASH@0..1 "#"
                IDENT_KW@1..6 "error"
                WS@6..7 " "
                PP_ERROR_BODY@7..12
                  IDENT_KW@7..12 "hello"
                NEWLINE@12..13 "\n"
        "##]],
    );
}

#[test]
fn test_pragma() {
    check(
        parse("#pragma debug(on)\n"),
        expect![[r##"
            ROOT@0..18
              PP_PRAGMA@0..18
                HASH@0..1 "#"
                IDENT_KW@1..7 "pragma"
                WS@7..8 " "
                PP_PRAGMA_BODY@8..17
                  IDENT_KW@8..13 "debug"
                  LPAREN@13..14 "("
                  IDENT_KW@14..16 "on"
                  RPAREN@16..17 ")"
                NEWLINE@17..18 "\n"
        "##]],
    );

    check(
        parse("#pra\\\ngma debug(on)\n"),
        expect![[r##"
            ROOT@0..20
              PP_PRAGMA@0..20
                HASH@0..1 "#"
                IDENT_KW@1..9 "pra\\\ngma"
                WS@9..10 " "
                PP_PRAGMA_BODY@10..19
                  IDENT_KW@10..15 "debug"
                  LPAREN@15..16 "("
                  IDENT_KW@16..18 "on"
                  RPAREN@18..19 ")"
                NEWLINE@19..20 "\n"
        "##]],
    );
}

#[test]
fn test_empty() {
    check(
        parse("#   \n"),
        expect![[r##"
            ROOT@0..5
              PP_EMPTY@0..5
                HASH@0..1 "#"
                WS@1..4 "   "
                NEWLINE@4..5 "\n"
        "##]],
    );
}
