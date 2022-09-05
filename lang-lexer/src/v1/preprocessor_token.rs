use logos::Logos;

use super::{
    parsers::{parse_pp_cmt, parse_pp_int, parse_pp_path},
    ParseContext, ParseOptions, Token,
};

#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[logos(extras = (ParseContext, ParseOptions))]
pub enum PreprocessorToken<'i> {
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier(&'i str),

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    Rest(std::borrow::Cow<'i, str>),

    #[regex(r"[0-9]*", parse_pp_int)]
    IntConstant(i32),

    PpCore,
    PpCompatibility,
    PpEs,

    PpExtRequire,
    PpExtEnable,
    PpExtWarn,
    PpExtDisable,

    #[regex("<[^>]*>", parse_pp_path)]
    PathAbsolute(&'i str),
    #[regex(r#""[^"]*""#, parse_pp_path)]
    PathRelative(&'i str),

    #[regex("([ \t]|\\\\\r?\n)+", logos::skip)]
    Whitespace,
    #[regex("//(.|\\\\\r?\n)*", |lex| { parse_pp_cmt(lex, true); logos::Skip })]
    SingleLineComment,
    #[regex("/\\*([^*]|\\*[^/])+\\*/", |lex| { parse_pp_cmt(lex, false); logos::Skip })]
    MultiLineComment,

    #[regex("\r?\n")]
    Newline,

    #[error]
    Error,
}

impl<'i> From<PreprocessorToken<'i>> for Token {
    fn from(pp: PreprocessorToken<'i>) -> Self {
        match pp {
            PreprocessorToken::Identifier(s) => Self::Identifier(s.into()),
            PreprocessorToken::Error => Self::Error,
            PreprocessorToken::LeftParen => Self::LeftParen,
            PreprocessorToken::RightParen => Self::RightParen,
            PreprocessorToken::Comma => Self::Comma,
            PreprocessorToken::Whitespace => Self::Whitespace,
            PreprocessorToken::SingleLineComment => Self::SingleLineComment,
            PreprocessorToken::MultiLineComment => Self::MultiLineComment,
            PreprocessorToken::IntConstant(inner) => Self::IntConstant(inner),
            PreprocessorToken::Newline => Self::Whitespace,
            PreprocessorToken::Rest(rest) => Self::PpRest(rest.to_string()),
            PreprocessorToken::PpCore => Self::PpCore,
            PreprocessorToken::PpCompatibility => Self::PpCompatibility,
            PreprocessorToken::PpEs => Self::PpEs,
            PreprocessorToken::PpExtRequire => Self::PpExtRequire,
            PreprocessorToken::PpExtEnable => Self::PpExtEnable,
            PreprocessorToken::PpExtWarn => Self::PpExtWarn,
            PreprocessorToken::PpExtDisable => Self::PpExtDisable,
            PreprocessorToken::Colon => Self::Colon,
            PreprocessorToken::PathAbsolute(inner) => Self::PpPathAbsolute(inner.to_string()),
            PreprocessorToken::PathRelative(inner) => Self::PpPathRelative(inner.to_string()),
        }
    }
}
