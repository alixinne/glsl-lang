use rowan::TextRange;
use smol_str::SmolStr;

use crate::lexer;

#[derive(Debug, Clone)]
pub struct Error {
    kind: ErrorKind,
    pos: TextRange,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}..{}: {}",
            u32::from(self.pos.start()),
            u32::from(self.pos.end()),
            self.kind
        )
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnknownPreprocessorDirective {
        name: SmolStr,
    },
    ExtraTokensInPreprocessorDirective,
    UnexpectedTokensInDefineArgs,
    Unexpected {
        actual: lexer::Token,
        expected: Box<[lexer::Token]>,
    },
    EndOfInput {
        expected: Box<[lexer::Token]>,
    },
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnknownPreprocessorDirective { name } => {
                write!(f, "unknown preprocessor directive `#{}`", name)
            }
            ErrorKind::ExtraTokensInPreprocessorDirective => {
                write!(f, "extra tokens in preprocessor directive")
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

impl Error {
    pub fn new(kind: ErrorKind, pos: TextRange) -> Self {
        Self { kind, pos }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn pos(&self) -> TextRange {
        self.pos
    }
}
