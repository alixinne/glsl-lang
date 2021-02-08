use std::str::FromStr;

use super::{LexicalError, PreprocessorToken, Token, TypeNames};

pub fn parse_int<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
    radix: u32,
) -> Result<i32, LexicalError> {
    let mut slice = lex.slice();
    let fb = slice.bytes().nth(0);
    let sgn = if fb == Some(b'-') {
        slice = &slice[1..];
        0xFFFFFFFFu32
    } else if fb == Some(b'+') {
        slice = &slice[1..];
        1u32
    } else {
        1u32
    };

    Ok(sgn.wrapping_mul(u32::from_str_radix(
        &slice[match radix {
            16 => 2,
            _ => 0,
        }..],
        radix,
    )?) as i32)
}

pub fn parse_uint<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
    radix: u32,
) -> Result<u32, LexicalError> {
    let mut slice = lex.slice();
    let fb = slice.bytes().nth(0);
    let sgn = if fb == Some(b'-') {
        slice = &slice[1..];
        0xFFFFFFFFu32
    } else if fb == Some(b'+') {
        slice = &slice[1..];
        1u32
    } else {
        1u32
    };

    Ok(sgn.wrapping_mul(u32::from_str_radix(
        &slice[match radix {
            16 => 2,
            _ => 0,
        }..slice.len() - 1],
        radix,
    )?))
}

pub fn parse_f32<'i>(lex: &mut logos::Lexer<'i, Token<'i>>) -> Result<f32, LexicalError> {
    let s = lex.slice();
    Ok(f32::from_str(
        s.strip_suffix(|c| c == 'f' || c == 'F').unwrap_or(s),
    )?)
}

pub fn parse_f64<'i>(lex: &mut logos::Lexer<'i, Token<'i>>) -> Result<f64, LexicalError> {
    let s = lex.slice();
    Ok(f64::from_str(
        s.strip_suffix(|c| c == 'f' || c == 'F')
            .and_then(|s| s.strip_suffix(|c| c == 'l' || c == 'L'))
            .unwrap_or(s),
    )?)
}

pub fn parse_pp_int<'i>(
    lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>,
) -> Result<i32, LexicalError> {
    Ok(i32::from_str(lex.slice())?)
}

pub fn parse_pp_path<'i>(lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>) -> &'i str {
    &lex.slice()[1..lex.slice().len() - 1]
}

pub fn parse_pp_ident<'i>(
    lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>,
) -> Result<(&'i str, TypeNames), LexicalError> {
    Ok((lex.slice(), lex.extras.type_names.clone()))
}

pub fn parse_ident<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
) -> Result<(&'i str, TypeNames), LexicalError> {
    Ok((lex.slice(), lex.extras.type_names.clone()))
}
