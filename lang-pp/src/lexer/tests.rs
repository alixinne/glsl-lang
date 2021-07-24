use std::borrow::Cow;

use super::*;
use crate::Unescaped;
use Token::*;

fn tokenize(src: &str) -> Vec<Token> {
    let pp = Lexer::new(src);
    pp.map(|tk| tk.token).collect()
}

fn tokenize_str(src: &str) -> Vec<(Token, Cow<'_, str>)> {
    let pp = Lexer::new(src);
    pp.map(|tk| (tk.token, Unescaped::new(tk.raw(src)).to_string()))
        .collect()
}

#[test]
fn test_ident() {
    assert_eq!(
        &tokenize_str("gl_FragColor")[..],
        &[(IDENT_KW, Cow::Borrowed("gl_FragColor"))]
    );
    assert_eq!(
        &tokenize_str("gl_Frag\\\nColor")[..],
        &[(IDENT_KW, Cow::Borrowed("gl_FragColor"))]
    );
    assert_eq!(
        &tokenize_str("gl_Frag\\\r\nColor")[..],
        &[(IDENT_KW, Cow::Borrowed("gl_FragColor"))]
    );
}

#[test]
fn test_digits() {
    assert_eq!(
        &tokenize_str("01214")[..],
        &[(DIGITS, Cow::Borrowed("01214"))]
    );
    assert_eq!(
        &tokenize_str("012\\\n14")[..],
        &[(DIGITS, Cow::Borrowed("01214"))]
    );
    assert_eq!(
        &tokenize_str("012\\\r\n14")[..],
        &[(DIGITS, Cow::Borrowed("01214"))]
    );
    assert_eq!(&tokenize("3.2324e2")[..], &[DIGITS]);
}

#[test]
fn test_single_comment() {
    assert_eq!(&tokenize("// Hello\n")[..], &[COMMENT, NEWLINE]);
    assert_eq!(&tokenize("// Hel\\\nlo\n")[..], &[COMMENT, NEWLINE]);
    assert_eq!(&tokenize("// Hel\\\r\nlo\n")[..], &[COMMENT, NEWLINE]);
}

#[test]
fn test_multi_comment() {
    assert_eq!(&tokenize("/* Hello\n */\n")[..], &[COMMENT, NEWLINE]);
    assert_eq!(&tokenize("/* Hel\\\nlo\n */\n")[..], &[COMMENT, NEWLINE]);
    assert_eq!(&tokenize("/* Hel\\\r\nlo\n */\n")[..], &[COMMENT, NEWLINE]);
}

#[test]
fn test_whitespace_sequence() {
    assert_eq!(
        &tokenize("  b \r\n c \n d  ")[..],
        &[WS, IDENT_KW, WS, NEWLINE, WS, IDENT_KW, WS, NEWLINE, WS, IDENT_KW, WS]
    );
}

#[test]
fn test_glued_tokens() {
    assert_eq!(
        &tokenize("<<>>++--<=>===!=&&||^^*=/=+=%=<<=>>=&=^=|=-=##")[..],
        &[
            LEFT_OP,
            RIGHT_OP,
            INC_OP,
            DEC_OP,
            LE_OP,
            GE_OP,
            EQ_OP,
            NE_OP,
            AND_OP,
            OR_OP,
            XOR_OP,
            MUL_ASSIGN,
            DIV_ASSIGN,
            ADD_ASSIGN,
            MOD_ASSIGN,
            LEFT_ASSIGN,
            RIGHT_ASSIGN,
            AND_ASSIGN,
            XOR_ASSIGN,
            OR_ASSIGN,
            SUB_ASSIGN,
            PP_CONCAT
        ]
    );
}
