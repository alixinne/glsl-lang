use std::path::PathBuf;

use derive_more::From;
use rowan::TextRange;
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
    nodes::{self, Directive, ExtensionName, ParsedPath},
};

mod send;
pub use send::*;

pub use crate::parser::{SyntaxNode, SyntaxToken};

pub type ProcessingError = lang_util::located::Located<ProcessingErrorKind>;

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
        node: SendNode,
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

impl std::error::Error for ProcessingErrorKind {}

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

pub type Error = lang_util::located::Located<ErrorKind>;

#[derive(Debug, PartialEq, Eq, derive_more::From)]
pub enum ErrorKind {
    Parse(parser::ErrorKind),
    Processing(ProcessingErrorKind),
    WarnExtUse {
        extension: ExtNameAtom,
        name: Option<TypeNameAtom>,
        raw_line: u32,
        pos: TextRange,
    },
    UnsupportedExt {
        extension: ExtensionName,
        raw_line: u32,
        pos: TextRange,
    },
}

impl std::error::Error for ErrorKind {}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Parse(parse) => write!(f, "{}", parse),
            ErrorKind::Processing(processing) => write!(f, "{}", processing),
            ErrorKind::WarnExtUse { extension, .. } => write!(f, "warning use of '{}'", extension),
            ErrorKind::UnsupportedExt { extension, .. } => {
                write!(f, "extension not supported: {}", extension)
            }
        }
    }
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

pub trait TokenLike {
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
        errors: impl IntoIterator<Item = impl Into<ErrorKind>>,
        location: &ExpandLocation,
    ) -> Self {
        let (inner, node) = d.into_inner();
        let pos = node.text_range();

        Self::Directive {
            node,
            kind: inner.into(),
            masked,
            errors: errors
                .into_iter()
                .map(|error| {
                    Error::builder()
                        .pos(pos)
                        .resolve_file(location)
                        .finish(error.into())
                })
                .collect(),
        }
    }

    pub fn directive_error<E: Into<ProcessingErrorKind>>(
        (error, node): (E, SyntaxNode),
        location: &ExpandLocation,
        masked: bool,
    ) -> Self {
        Self::error(error.into(), node.text_range(), location, masked)
    }

    pub fn error<T: Into<ErrorKind>>(
        e: T,
        pos: impl Into<TextRange>,
        location: &ExpandLocation,
        masked: bool,
    ) -> Self {
        Self::Error {
            error: Error::builder()
                .pos(pos)
                .resolve_file(location)
                .finish(e.into()),
            masked,
        }
    }

    pub fn map_error<T: Into<ErrorKind>>(e: lang_util::located::Located<T>, masked: bool) -> Self {
        Self::Error {
            error: e.map(Into::into),
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

#[cfg(test)]
mod tests {
    fn assert_send<T: Send>() {}

    #[test]
    fn test_error_send() {
        assert_send::<super::SendEvent>();
        assert_send::<super::Error>();
    }
}
