use smol_str::SmolStr;

use crate::lexer;

pub type Error = lang_util::located::Located<ErrorKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    UnknownPreprocessorDirective {
        name: SmolStr,
    },
    ExtraTokensInPreprocessorDirective {
        name: SmolStr,
    },
    UnexpectedTokensInDefineArgs,
    Unexpected {
        actual: lexer::Token,
        expected: Box<[lexer::Token]>,
    },
    EndOfInput {
        expected: Box<[lexer::Token]>,
    },
}

impl std::error::Error for ErrorKind {}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnknownPreprocessorDirective { name } => {
                write!(f, "unknown preprocessor directive `#{}`", name)
            }
            ErrorKind::ExtraTokensInPreprocessorDirective { name } => {
                write!(f, "'#{}' : extra tokens in preprocessor directive", name)
            }
            ErrorKind::UnexpectedTokensInDefineArgs => {
                write!(f, "unexpected tokens in #define function arguments")
            }
            ErrorKind::EndOfInput { expected } => {
                if expected.len() == 0 {
                    write!(f, "unexpected end of input")
                } else {
                    // TODO: Proper display
                    write!(f, "unexpected end of input: {:?}", expected)
                }
            }
            ErrorKind::Unexpected { actual, expected } => {
                if expected.len() == 0 {
                    write!(f, "unexpected {:?}", actual)
                } else {
                    // TODO: Proper display
                    write!(f, "unexpected {:?}, expected {:?}", actual, expected)
                }
            }
        }
    }
}
