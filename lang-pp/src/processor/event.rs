use std::path::PathBuf;

use derive_more::From;
use rowan::{NodeOrToken, TextRange};
use smol_str::SmolStr;
use thiserror::Error;

use lang_util::FileId;

use crate::{
    exts::names::ExtNameAtom,
    last::type_names::TypeNameAtom,
    parser::{self, SyntaxKind},
};

use super::{
    expand::ExpandLocation,
    nodes::{self, Directive, ExtensionName, ParsedLine, ParsedPath},
};

pub use crate::parser::{SyntaxNode, SyntaxToken};

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq, From)]
pub enum ProcessingErrorKind {
    ExtraEndIf,
    ExtraElse,
    ExtraElif,
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
    IncludeNotSupported,
    IncludeNotFound {
        path: ParsedPath,
    },
    InvalidTokenPaste {
        token: Option<SmolStr>,
    },
    CppStyleLineNotSupported,
    DirectiveVersion(nodes::VersionError),
    DirectiveExtension(nodes::ExtensionError),
    DirectiveDefine(nodes::DefineError),
    DirectiveIfDef(nodes::IfDefError),
    #[from(ignore)]
    DirectiveIfNDef(nodes::IfDefError),
    DirectiveIf(nodes::IfError),
    DirectiveElif(nodes::ElifError),
    DirectiveElse(nodes::ElseError),
    DirectiveEndIf(nodes::EndIfError),
    #[from(ignore)]
    DirectiveUndef(nodes::IfDefError),
    DirectiveError(nodes::ErrorError),
    DirectiveInclude(nodes::IncludeError),
    DirectiveLine(nodes::LineError),
    DirectivePragma(nodes::PragmaError),
}

impl ProcessingErrorKind {
    pub fn with_node(
        self,
        node: NodeOrToken<SyntaxNode, SyntaxToken>,
        location: &ExpandLocation,
    ) -> ProcessingError {
        let pos = node.text_range();

        ProcessingError::new(
            node,
            self,
            pos,
            location.offset_to_raw_line_and_col(pos.start()),
        )
    }

    pub fn with_node_and_range(
        self,
        node: NodeOrToken<SyntaxNode, SyntaxToken>,
        pos: TextRange,
        location: &ExpandLocation,
    ) -> ProcessingError {
        ProcessingError::new(
            node,
            self,
            pos,
            location.offset_to_raw_line_and_col(pos.start()),
        )
    }

    pub fn with_token(self, token: impl TokenLike, location: &ExpandLocation) -> ProcessingError {
        let pos = token.text_range();

        ProcessingError::new(
            token.into(),
            self,
            pos,
            location.offset_to_raw_line_and_col(pos.start()),
        )
    }

    pub fn with_token_and_range(
        self,
        token: impl TokenLike,
        pos: TextRange,
        location: &ExpandLocation,
    ) -> ProcessingError {
        ProcessingError::new(
            token.into(),
            self,
            pos,
            location.offset_to_raw_line_and_col(pos.start()),
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
            ProcessingErrorKind::ExtraElif => {
                write!(f, "unmatched #elif")
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
            ProcessingErrorKind::IncludeNotSupported => {
                write!(f, "'#include' : required extension not requested: GL_GOOGLE_include_directive or GL_ARB_shading_language_include")
            }
            ProcessingErrorKind::IncludeNotFound { path } => {
                write!(f, "'#include' : could not find file for {}", path)
            }
            ProcessingErrorKind::InvalidTokenPaste { token } => {
                if let Some(token) = token {
                    if token.ends_with(" ##") {
                        write!(f, "'##' : invalid use of paste operator")
                    } else {
                        write!(f, "'##' : invalid pasted token : {}", token)
                    }
                } else {
                    write!(f, "'##' : invalid use of paste operator")
                }
            }
            ProcessingErrorKind::CppStyleLineNotSupported => {
                write!(f, "'#line' : required extension not requested: GL_GOOGLE_cpp_style_line_directive")
            }
            ProcessingErrorKind::DirectiveVersion(inner) => {
                write!(f, "'#version' : {}", inner)
            }
            ProcessingErrorKind::DirectiveExtension(inner) => {
                write!(f, "'#extension' : {}", inner)
            }
            ProcessingErrorKind::DirectiveDefine(inner) => {
                write!(f, "'#define' : {}", inner)
            }
            ProcessingErrorKind::DirectiveIfDef(inner) => {
                write!(f, "'#ifdef' : {}", inner)
            }
            ProcessingErrorKind::DirectiveIfNDef(inner) => {
                write!(f, "'#ifndef' : {}", inner)
            }
            ProcessingErrorKind::DirectiveIf(inner) => {
                write!(f, "'#if' : {}", inner)
            }
            ProcessingErrorKind::DirectiveElif(inner) => {
                write!(f, "'#elif' : {}", inner)
            }
            ProcessingErrorKind::DirectiveElse(inner) => {
                write!(f, "'#else' : {}", inner)
            }
            ProcessingErrorKind::DirectiveEndIf(inner) => {
                write!(f, "'#endif' : {}", inner)
            }
            ProcessingErrorKind::DirectiveUndef(inner) => {
                write!(f, "'#undef' : {}", inner)
            }
            ProcessingErrorKind::DirectiveError(inner) => {
                write!(f, "'#error' : {}", inner)
            }
            ProcessingErrorKind::DirectiveInclude(inner) => {
                write!(f, "'#include' : {}", inner)
            }
            ProcessingErrorKind::DirectiveLine(inner) => {
                write!(f, "'#line' : {}", inner)
            }
            ProcessingErrorKind::DirectivePragma(inner) => {
                write!(f, "'#pragma' : {}", inner)
            }
        }
    }
}

#[derive(Debug)]
pub struct Located<E: std::error::Error + 'static> {
    inner: E,
    path: PathBuf,
    pos: TextRange,
    current_file: FileId,
    line: u32,
    line_override: Option<(u32, ParsedLine)>,
}

impl<E: std::error::Error> Located<E> {
    pub fn new(inner: E, path: PathBuf, pos: TextRange, location: &ExpandLocation) -> Self {
        let line = location.offset_to_line_and_col(pos.start()).0;

        Self {
            inner,
            path,
            pos,
            current_file: location.current_file(),
            line,
            line_override: location.line_override().cloned(),
        }
    }

    pub fn into_inner(self) -> E {
        self.inner
    }

    pub fn inner(&self) -> &E {
        &self.inner
    }

    pub fn current_file(&self) -> FileId {
        self.current_file
    }

    pub fn pos(&self) -> &TextRange {
        &self.pos
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn string(&self) -> &dyn std::fmt::Display {
        Error::override_string(&self.current_file, self.line_override.as_ref())
    }
}

impl<E: std::error::Error> std::error::Error for Located<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

impl<E: std::error::Error> std::fmt::Display for Located<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}: '#include' : {} : {}",
            self.string(),
            self.line() + 1,
            self.path.display(),
            self.inner
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    kind: ErrorKind,
    current_file: FileId,
    line: u32,
    line_override: Option<(u32, ParsedLine)>,
}

impl Error {
    pub fn new(kind: impl Into<ErrorKind>, location: &ExpandLocation) -> Self {
        let kind = kind.into();
        let line = location.line_to_line_number(kind.raw_line());

        Self {
            kind,
            current_file: location.current_file(),
            line,
            line_override: location.line_override().cloned(),
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn current_file(&self) -> FileId {
        self.current_file
    }

    pub fn pos(&self) -> TextRange {
        match &self.kind {
            ErrorKind::Parse(err) => err.pos(),
            ErrorKind::Processing(err) => err.pos(),
            ErrorKind::WarnExtUse { pos, .. } => *pos,
            ErrorKind::UnsupportedExt { pos, .. } => *pos,
        }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    fn override_string<'a>(
        current_file: &'a FileId,
        line_override: Option<&'a (u32, ParsedLine)>,
    ) -> &'a dyn std::fmt::Display {
        if let Some((_, line_override)) = line_override {
            match line_override {
                ParsedLine::LineAndFileNumber(_, ref file_number) => {
                    return file_number;
                }
                ParsedLine::LineAndPath(_, ref path) => {
                    return path;
                }
                _ => {}
            }
        }

        current_file
    }

    pub fn string(&self) -> &dyn std::fmt::Display {
        Self::override_string(&self.current_file, self.line_override.as_ref())
    }

    pub fn with_file_id(self, file_id: FileId) -> Self {
        Self {
            current_file: file_id,
            ..self
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: ", self.string(), self.line() + 1,)?;

        match &self.kind {
            ErrorKind::Parse(err) => {
                write!(f, "{}", err.kind())
            }
            ErrorKind::Processing(err) => {
                write!(f, "{}", err.kind())
            }
            _ => {
                write!(f, "{}", &self.kind)
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}

#[derive(Debug, PartialEq, Eq, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    Parse(#[from] parser::Error),
    #[error(transparent)]
    Processing(#[from] ProcessingError),
    #[error("warning use of '{extension}'")]
    WarnExtUse {
        extension: ExtNameAtom,
        name: Option<TypeNameAtom>,
        raw_line: u32,
        pos: TextRange,
    },
    #[error("extension not supported: {extension}")]
    UnsupportedExt {
        extension: ExtensionName,
        raw_line: u32,
        pos: TextRange,
    },
}

impl ErrorKind {
    pub fn unsupported_ext(
        extension: ExtensionName,
        pos: TextRange,
        location: &ExpandLocation,
    ) -> Self {
        let raw_line = location.offset_to_raw_line_and_col(pos.start()).0;
        Self::UnsupportedExt {
            extension,
            raw_line,
            pos,
        }
    }

    pub fn warn_ext_use(
        extension: ExtNameAtom,
        name: Option<TypeNameAtom>,
        pos: TextRange,
        location: &ExpandLocation,
    ) -> Self {
        let raw_line = location.offset_to_raw_line_and_col(pos.start()).0;
        Self::WarnExtUse {
            extension,
            name,
            raw_line,
            pos,
        }
    }

    fn raw_line(&self) -> u32 {
        match self {
            ErrorKind::Parse(err) => err.line(),
            ErrorKind::Processing(err) => err.line(),
            ErrorKind::WarnExtUse { raw_line, .. } => *raw_line,
            ErrorKind::UnsupportedExt { raw_line, .. } => *raw_line,
        }
    }
}

#[derive(Debug, PartialEq, Eq, From)]
pub enum DirectiveKind {
    Empty(nodes::Empty),
    Version(nodes::Version),
    Extension(nodes::Extension),
    Define(nodes::Define),
    IfDef(nodes::IfDef),
    IfNDef(nodes::IfNDef),
    If(nodes::If),
    Elif(nodes::Elif),
    Else(nodes::Else),
    EndIf(nodes::EndIf),
    Undef(nodes::Undef),
    Error(nodes::Error),
    Include(nodes::Include),
    Line(nodes::Line),
    Pragma(nodes::Pragma),
    Invalid(nodes::Invalid),
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

#[derive(Clone, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq, From)]
pub enum Event {
    Error {
        error: Error,
        masked: bool,
    },
    EnterFile {
        file_id: FileId,
        path: PathBuf,
        canonical_path: PathBuf,
    },
    Token {
        token: OutputToken,
        masked: bool,
    },
    Directive {
        node: SyntaxNode,
        kind: DirectiveKind,
        masked: bool,
        errors: Vec<Error>,
    },
}

impl Event {
    pub fn enter_file(file_id: FileId) -> Self {
        Self::EnterFile {
            file_id,
            path: Default::default(),
            canonical_path: Default::default(),
        }
    }

    pub fn token<T: Into<OutputToken>>(token: T, masked: bool) -> Self {
        Self::Token {
            token: token.into(),
            masked,
        }
    }

    pub fn directive<D: Into<DirectiveKind>>(d: Directive<D>, masked: bool) -> Self {
        let (inner, node) = d.into_inner();
        Self::Directive {
            node,
            kind: inner.into(),
            masked,
            errors: vec![],
        }
    }

    pub fn directive_errors<D: Into<DirectiveKind>>(
        d: Directive<D>,
        masked: bool,
        errors: impl Iterator<Item = impl Into<ErrorKind>>,
        location: &ExpandLocation,
    ) -> Self {
        let (inner, node) = d.into_inner();
        Self::Directive {
            node,
            kind: inner.into(),
            masked,
            errors: errors.map(|error| Error::new(error, location)).collect(),
        }
    }

    pub fn directive_error<E: Into<ProcessingErrorKind>>(
        (error, node): (E, SyntaxNode),
        location: &ExpandLocation,
        masked: bool,
    ) -> Self {
        Self::error(
            error.into().with_node(node.into(), location),
            location,
            masked,
        )
    }

    pub fn error<T: Into<ErrorKind>>(e: T, location: &ExpandLocation, masked: bool) -> Self {
        Self::Error {
            error: Error::new(e, location),
            masked,
        }
    }

    pub fn is_token(&self) -> bool {
        matches!(self, Event::Token { .. })
    }

    pub fn as_token(&self) -> Option<&OutputToken> {
        if let Event::Token { token, .. } = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn into_token(self) -> Option<OutputToken> {
        if let Event::Token { token, .. } = self {
            Some(token)
        } else {
            None
        }
    }
}

impl From<OutputToken> for Event {
    fn from(token: OutputToken) -> Self {
        Self::Token {
            token,
            masked: false,
        }
    }
}
