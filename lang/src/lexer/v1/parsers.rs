use std::str::FromStr;

use super::{LexerContext, LexerPosition, LexicalError, PreprocessorToken, Token};

pub fn parse_int<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
    radix: u32,
) -> Result<i32, LexicalError> {
    let mut slice = lex.slice();
    let fb = slice.bytes().next();
    let sgn = if fb == Some(b'-') {
        slice = &slice[1..];
        0xFFFFFFFFu32
    } else if fb == Some(b'+') {
        slice = &slice[1..];
        1u32
    } else {
        1u32
    };

    Ok(sgn.wrapping_mul(
        u32::from_str_radix(
            &slice[match radix {
                16 => 2,
                _ => 0,
            }..],
            radix,
        )
        .map_err(|source| LexicalError::InvalidIntLiteral {
            location: LexerPosition::new(lex.extras.opts.source_id, lex.span().start),
            source,
        })?,
    ) as i32)
}

pub fn parse_uint<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
    radix: u32,
) -> Result<u32, LexicalError> {
    let mut slice = lex.slice();
    let fb = slice.bytes().next();
    let sgn = if fb == Some(b'-') {
        slice = &slice[1..];
        0xFFFFFFFFu32
    } else if fb == Some(b'+') {
        slice = &slice[1..];
        1u32
    } else {
        1u32
    };

    Ok(sgn.wrapping_mul(
        u32::from_str_radix(
            &slice[match radix {
                16 => 2,
                _ => 0,
            }..slice.len() - 1],
            radix,
        )
        .map_err(|source| LexicalError::InvalidIntLiteral {
            location: LexerPosition::new(lex.extras.opts.source_id, lex.span().start),
            source,
        })?,
    ))
}

pub fn parse_f32<'i>(lex: &mut logos::Lexer<'i, Token<'i>>) -> Result<f32, LexicalError> {
    let s = lex.slice();
    f32::from_str(s.strip_suffix(|c| c == 'f' || c == 'F').unwrap_or(s)).map_err(|source| {
        LexicalError::InvalidFloatLiteral {
            location: LexerPosition::new(lex.extras.opts.source_id, lex.span().start),
            source,
        }
    })
}

pub fn parse_f64<'i>(lex: &mut logos::Lexer<'i, Token<'i>>) -> Result<f64, LexicalError> {
    let s = lex.slice();
    f64::from_str(
        s.strip_suffix(|c| c == 'f' || c == 'F')
            .and_then(|s| s.strip_suffix(|c| c == 'l' || c == 'L'))
            .unwrap_or(s),
    )
    .map_err(|source| LexicalError::InvalidFloatLiteral {
        location: LexerPosition::new(lex.extras.opts.source_id, lex.span().start),
        source,
    })
}

pub fn parse_pp_int<'i>(
    lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>,
) -> Result<i32, LexicalError> {
    i32::from_str(lex.slice()).map_err(|source| LexicalError::InvalidIntLiteral {
        location: LexerPosition::new(lex.extras.opts.source_id, lex.span().start),
        source,
    })
}

pub fn parse_pp_path<'i>(lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>) -> &'i str {
    &lex.slice()[1..lex.slice().len() - 1]
}

pub fn parse_pp_ident<'i>(
    lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>,
) -> Result<(&'i str, LexerContext), LexicalError> {
    Ok((lex.slice(), lex.extras.clone()))
}

pub fn parse_ident<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
) -> Result<(&'i str, LexerContext), LexicalError> {
    Ok((lex.slice(), lex.extras.clone()))
}

pub fn parse_rs_ident<'i>(
    lex: &mut logos::Lexer<'i, Token<'i>>,
) -> Result<(&'i str, LexerContext), LexicalError> {
    if lex.extras.opts.allow_rs_ident {
        Ok((lex.slice(), lex.extras.clone()))
    } else {
        Err(LexicalError::ForbiddenRsQuote {
            location: LexerPosition::new(lex.extras.opts.source_id, lex.span().start),
        })
    }
}

fn parse_cmt_int(
    extras: &super::LexerContext,
    slice: &str,
    span: std::ops::Range<usize>,
    is_single: bool,
) {
    use crate::ast::NodeContent;

    if extras.has_comments() {
        let source_id = extras.opts.source_id;

        let comment = if is_single {
            crate::ast::CommentData::Single(slice[2..].to_owned())
        } else {
            crate::ast::CommentData::Multi(slice[2..slice.len() - 2].to_owned())
        }
        .spanned(
            LexerPosition::new(source_id, span.start),
            LexerPosition::new(source_id, span.end),
        );

        extras.add_comment(comment);
    }
}

pub fn parse_cmt<'i>(lex: &mut logos::Lexer<'i, Token<'i>>, is_single: bool) {
    parse_cmt_int(&lex.extras, lex.slice(), lex.span(), is_single)
}

pub fn parse_pp_cmt<'i>(lex: &mut logos::Lexer<'i, PreprocessorToken<'i>>, is_single: bool) {
    parse_cmt_int(&lex.extras, lex.slice(), lex.span(), is_single)
}
