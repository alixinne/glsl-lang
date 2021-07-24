use std::path::PathBuf;

use smol_str::SmolStr;
use thiserror::Error;

use crate::{
    parser::{SyntaxNode, SyntaxToken},
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

#[derive(Debug, Error)]
pub enum ProcessingErrorKind {
    #[error("unmatched #endif")]
    ExtraEndIf,
    #[error("unmatched #else")]
    ExtraElse,
    #[error("protected definition cannot be undefined")]
    ProtectedDefine { ident: SmolStr },
    #[error("#error : {message}")]
    ErrorDirective { message: String },
}

impl ProcessingErrorKind {
    pub fn with_node(self, node: SyntaxNode) -> ProcessingError {
        ProcessingError::new(node, self)
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

impl<E: std::error::Error> From<crate::parser::Error> for Error<E> {
    fn from(error: crate::parser::Error) -> Self {
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
    Parse(#[from] crate::parser::Error),
    #[error(transparent)]
    Processing(#[from] ProcessingError),
    #[error("unhandled directive: {0:?}")]
    Unhandled(SyntaxNode),
}

#[derive(Debug)]
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

impl From<DirectiveResult<nodes::Version>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::Version>) -> Self {
        Self::Version(d)
    }
}

impl From<DirectiveResult<nodes::Extension>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::Extension>) -> Self {
        Self::Extension(d)
    }
}

impl From<DirectiveResult<nodes::Define>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::Define>) -> Self {
        Self::Define(d)
    }
}

impl From<DirectiveResult<nodes::IfDef>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::IfDef>) -> Self {
        Self::IfDef(d)
    }
}

impl From<DirectiveResult<nodes::IfNDef>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::IfNDef>) -> Self {
        Self::IfNDef(d)
    }
}

impl From<DirectiveResult<nodes::Undef>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::Undef>) -> Self {
        Self::Undef(d)
    }
}

impl From<DirectiveResult<nodes::Error>> for DirectiveKind {
    fn from(d: DirectiveResult<nodes::Error>) -> Self {
        Self::Error(d)
    }
}

#[derive(Debug)]
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

impl<E: std::error::Error> From<DirectiveKind> for Event<E> {
    fn from(directive_kind: DirectiveKind) -> Self {
        Self::Directive(directive_kind)
    }
}
