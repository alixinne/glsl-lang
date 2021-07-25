use std::path::PathBuf;

use derive_more::From;
use rowan::NodeOrToken;
use smol_str::SmolStr;
use thiserror::Error;

use crate::{
    parser::{self, SyntaxNode, SyntaxToken},
    FileId,
};

use super::nodes::{self, DirectiveResult};

#[derive(Debug)]
pub struct ProcessingError {
    node: SyntaxNode,
    kind: ProcessingErrorKind,
}

impl ProcessingError {
    pub fn new(node: SyntaxNode, kind: ProcessingErrorKind) -> Self {
        Self { node, kind }
    }

    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }

    pub fn kind(&self) -> &ProcessingErrorKind {
        &self.kind
    }
}

impl std::fmt::Display for ProcessingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for ProcessingError {}

#[derive(Debug)]
pub enum ProcessingErrorKind {
    ExtraEndIf,
    ExtraElse,
    ProtectedDefine { ident: SmolStr, is_undef: bool },
    ErrorDirective { message: String },
}

impl ProcessingErrorKind {
    pub fn with_node(self, node: SyntaxNode) -> ProcessingError {
        ProcessingError::new(node, self)
    }
}

impl std::fmt::Display for ProcessingErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcessingErrorKind::ExtraEndIf => {
                write!(f, "unmatched #endif")
            }
            ProcessingErrorKind::ExtraElse => {
                write!(f, "unmatched #else")
            }
            ProcessingErrorKind::ProtectedDefine { ident, is_undef } => {
                let directive = if *is_undef { "undef" } else { "define" };

                if ident.starts_with("GL_") {
                    write!(
                        f,
                        "'#{}' : names beginning with \"GL_\" can't be (un)defined: {}",
                        directive, ident
                    )
                } else {
                    write!(
                        f,
                        "'#{}' : predefined names can't be (un)defined: {}",
                        directive, ident
                    )
                }
            }
            ProcessingErrorKind::ErrorDirective { message } => {
                write!(f, "'#error' : {}", message)
            }
        }
    }
}

#[derive(Debug)]
pub struct Error<E: std::error::Error + 'static> {
    kind: ErrorKind<E>,
}

impl<E: std::error::Error> Error<E> {
    pub fn kind(&self) -> &ErrorKind<E> {
        &self.kind
    }
}

impl<E: std::error::Error> std::fmt::Display for Error<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<E: std::error::Error> std::error::Error for Error<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}

impl<E: std::error::Error> From<ErrorKind<E>> for Error<E> {
    fn from(kind: ErrorKind<E>) -> Self {
        Self { kind }
    }
}

impl<E: std::error::Error> From<parser::Error> for Error<E> {
    fn from(error: parser::Error) -> Self {
        Self {
            kind: ErrorKind::Parse(error),
        }
    }
}

impl<E: std::error::Error> From<ProcessingError> for Error<E> {
    fn from(error: ProcessingError) -> Self {
        Self {
            kind: ErrorKind::Processing(error),
        }
    }
}

#[derive(Debug, Error)]
pub enum ErrorKind<E: std::error::Error + 'static> {
    #[error("i/o error: {0}")]
    Io(#[source] E),
    #[error(transparent)]
    Parse(#[from] parser::Error),
    #[error(transparent)]
    Processing(#[from] ProcessingError),
    #[error("unhandled directive or substitution: \"{}\"", .0.to_string().trim())]
    Unhandled(NodeOrToken<SyntaxNode, SyntaxToken>),
}

#[derive(Debug, From)]
pub enum DirectiveKind {
    Version(DirectiveResult<nodes::Version>),
    Extension(DirectiveResult<nodes::Extension>),
    Define(DirectiveResult<nodes::Define>),
    IfDef(DirectiveResult<nodes::IfDef>),
    IfNDef(DirectiveResult<nodes::IfNDef>),
    Else,
    EndIf,
    Undef(DirectiveResult<nodes::Undef>),
    Error(DirectiveResult<nodes::Error>),
}

#[derive(Debug, From)]
pub enum Event<E: std::error::Error + 'static> {
    Error(Error<E>),
    EnterFile { file_id: FileId, path: PathBuf },
    Token(SyntaxToken),
    Directive(DirectiveKind),
}

impl<E: std::error::Error> Event<E> {
    pub fn directive<D: Into<DirectiveKind>>(d: D) -> Self {
        Self::Directive(d.into())
    }

    pub fn error<T: Into<Error<E>>>(e: T) -> Self {
        Self::Error(e.into())
    }
}
