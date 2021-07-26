use std::{convert::TryFrom, path::PathBuf};

use derive_more::From;
use rowan::{NodeOrToken, TextRange};
use smol_str::SmolStr;
use thiserror::Error;

use crate::{
    lexer::LineMap,
    parser::{self, SyntaxKind, SyntaxNode, SyntaxToken},
    FileId,
};

use super::nodes::{self, DirectiveResult};

#[derive(Debug)]
pub struct ProcessingError {
    node: NodeOrToken<SyntaxNode, SyntaxToken>,
    kind: ProcessingErrorKind,
    pos: TextRange,
    user_pos: (u32, u32),
}

impl ProcessingError {
    pub fn new(
        node: NodeOrToken<SyntaxNode, SyntaxToken>,
        kind: ProcessingErrorKind,
        pos: TextRange,
        user_pos: (u32, u32),
    ) -> Self {
        Self {
            node,
            kind,
            pos,
            user_pos,
        }
    }

    pub fn node_or_token(&self) -> &NodeOrToken<SyntaxNode, SyntaxToken> {
        &self.node
    }

    pub fn kind(&self) -> &ProcessingErrorKind {
        &self.kind
    }

    pub fn pos(&self) -> TextRange {
        self.pos
    }

    pub fn line(&self) -> u32 {
        self.user_pos.0
    }

    pub fn col(&self) -> u32 {
        self.user_pos.1
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
    ProtectedDefine {
        ident: SmolStr,
        is_undef: bool,
    },
    ErrorDirective {
        message: String,
    },
    UnterminatedMacroInvocation {
        ident: SmolStr,
    },
    UnexpectedDirective {
        ident: SmolStr,
        node: SyntaxNode,
    },
    MismatchedArguments {
        ident: SmolStr,
        expected: usize,
        actual: usize,
    },
}

impl ProcessingErrorKind {
    pub fn with_node(
        self,
        node: NodeOrToken<SyntaxNode, SyntaxToken>,
        line_map: &LineMap,
    ) -> ProcessingError {
        let pos = node.text_range();
        let start = pos.start();
        ProcessingError::new(node, self, pos, line_map.get_line_and_col(start.into()))
    }

    pub fn with_token(self, token: impl TokenLike, line_map: &LineMap) -> ProcessingError {
        let pos = token.text_range();
        let start = pos.start();
        ProcessingError::new(
            token.into(),
            self,
            pos,
            line_map.get_line_and_col(start.into()),
        )
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
            ProcessingErrorKind::UnterminatedMacroInvocation { ident } => {
                write!(f, "'macro expansion' : end of input in macro {}", ident)
            }
            ProcessingErrorKind::UnexpectedDirective { ident, node } => {
                write!(f, "'macro expansion' : unexpected directive while scanning for macro invocation {} argument list: \"{}\"", ident, node.text())
            }
            ProcessingErrorKind::MismatchedArguments {
                ident,
                expected,
                actual,
            } => {
                write!(f, "'macro expansion' : wrong number of arguments in input of macro {} : expected {}, got {}", ident, expected, actual)
            }
        }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    current_file: FileId,
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn pos(&self) -> TextRange {
        match &self.kind {
            ErrorKind::Parse(err) => err.pos(),
            ErrorKind::Processing(err) => err.pos(),
            ErrorKind::Unhandled(node, _) => node.text_range(),
        }
    }

    pub fn with_current_file(self, current_file: FileId) -> Self {
        Self {
            current_file,
            ..self
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::Parse(err) => write!(f, "{}:{}: ", self.current_file, err.line() + 1)?,
            ErrorKind::Processing(err) => write!(f, "{}:{}: ", self.current_file, err.line() + 1)?,
            ErrorKind::Unhandled(_, pos) => write!(f, "{}:{}: ", self.current_file, pos.0 + 1)?,
        }

        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self {
            kind,
            current_file: FileId::default(),
        }
    }
}

impl From<parser::Error> for Error {
    fn from(error: parser::Error) -> Self {
        Self {
            kind: ErrorKind::Parse(error),
            current_file: FileId::default(),
        }
    }
}

impl From<ProcessingError> for Error {
    fn from(error: ProcessingError) -> Self {
        Self {
            kind: ErrorKind::Processing(error),
            current_file: FileId::default(),
        }
    }
}

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    Parse(#[from] parser::Error),
    #[error(transparent)]
    Processing(#[from] ProcessingError),
    #[error("unhandled directive or substitution: \"{}\"", .0.to_string().trim())]
    Unhandled(NodeOrToken<SyntaxNode, SyntaxToken>, (u32, u32)),
}

impl ErrorKind {
    pub fn unhandled(
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
        line_map: &LineMap,
    ) -> Self {
        let user_pos = line_map.get_line_and_col(node_or_token.text_range().start().into());
        Self::Unhandled(node_or_token, user_pos)
    }
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

pub trait TokenLike: Into<NodeOrToken<SyntaxNode, SyntaxToken>> {
    fn kind(&self) -> SyntaxKind;
    fn text(&self) -> &str;
    fn text_range(&self) -> TextRange;
}

impl TokenLike for SyntaxToken {
    fn kind(&self) -> SyntaxKind {
        self.kind()
    }

    fn text(&self) -> &str {
        self.text()
    }

    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}

#[derive(Clone)]
pub struct OutputToken {
    inner: SyntaxToken,
    source_range: Option<TextRange>,
}

impl OutputToken {
    pub fn new(token: SyntaxToken, source_range: TextRange) -> Self {
        Self {
            inner: token,
            source_range: Some(source_range),
        }
    }

    pub fn source_range(&self) -> TextRange {
        if let Some(range) = self.source_range {
            range
        } else {
            self.inner.text_range()
        }
    }

    pub fn generated(&self) -> bool {
        self.source_range.is_some()
    }
}

impl TokenLike for OutputToken {
    fn kind(&self) -> SyntaxKind {
        self.inner.kind()
    }

    fn text(&self) -> &str {
        self.inner.text()
    }

    fn text_range(&self) -> TextRange {
        self.source_range()
    }
}

// TODO: This loses location information but should only be used in errors anyways
impl From<OutputToken> for NodeOrToken<SyntaxNode, SyntaxToken> {
    fn from(out: OutputToken) -> Self {
        NodeOrToken::Token(out.inner)
    }
}

impl From<SyntaxToken> for OutputToken {
    fn from(token: SyntaxToken) -> Self {
        Self {
            inner: token,
            source_range: None,
        }
    }
}

impl std::fmt::Debug for OutputToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}@{:?}", self.inner.kind(), self.source_range())?;

        if self.text().len() < 25 {
            return write!(f, " {:?}", self.text());
        }
        let text = self.text();
        for idx in 21..25 {
            if text.is_char_boundary(idx) {
                let text = format!("{} ...", &text[..idx]);
                return write!(f, " {:?}", text);
            }
        }

        unreachable!()
    }
}

#[derive(Debug, From)]
pub enum Event {
    Error(Error),
    EnterFile { file_id: FileId, path: PathBuf },
    Token(OutputToken),
    Directive(DirectiveKind),
}

impl Event {
    pub fn directive<D: Into<DirectiveKind>>(d: D) -> Self {
        Self::Directive(d.into())
    }

    pub fn error<T: Into<Error>>(e: T, current_file: FileId) -> Self {
        Self::Error(e.into().with_current_file(current_file))
    }
}

impl<E: std::error::Error + 'static> TryFrom<IoEvent<E>> for Event {
    type Error = E;

    fn try_from(value: IoEvent<E>) -> Result<Self, E> {
        Ok(match value {
            IoEvent::IoError(err) => {
                return Err(err);
            }
            IoEvent::Error(err) => Self::Error(err),
            IoEvent::EnterFile { file_id, path } => Self::EnterFile { file_id, path },
            IoEvent::Token(token) => Self::Token(token),
            IoEvent::Directive(directive) => Self::Directive(directive),
        })
    }
}

#[derive(Debug)]
pub enum IoEvent<E: std::error::Error + 'static> {
    IoError(E),
    Error(Error),
    EnterFile { file_id: FileId, path: PathBuf },
    Token(OutputToken),
    Directive(DirectiveKind),
}

impl<E: std::error::Error + 'static> From<Event> for IoEvent<E> {
    fn from(e: Event) -> Self {
        match e {
            Event::Error(err) => Self::Error(err),
            Event::EnterFile { file_id, path } => Self::EnterFile { file_id, path },
            Event::Token(token) => Self::Token(token),
            Event::Directive(directive) => Self::Directive(directive),
        }
    }
}
