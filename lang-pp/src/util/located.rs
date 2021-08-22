use std::{fmt, path::PathBuf};

use rowan::{TextRange, TextSize};

use lang_util::FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::From)]
pub enum PointOrRange {
    /// Offset into the source string
    Point(TextSize),
    /// Range of the source string
    Range(TextRange),
}

impl PointOrRange {
    pub fn start(&self) -> TextSize {
        match self {
            PointOrRange::Point(point) => *point,
            PointOrRange::Range(range) => range.start(),
        }
    }
}

impl Default for PointOrRange {
    fn default() -> Self {
        Self::Point(TextSize::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::From)]
pub enum FileOverride {
    /// No override
    None,
    /// Override with a raw file number
    Number(u32),
    /// Override with a path
    Path(String),
}

impl FileOverride {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl Default for FileOverride {
    fn default() -> Self {
        Self::None
    }
}

impl fmt::Display for FileOverride {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FileOverride::None => Ok(()),
            FileOverride::Number(number) => write!(f, "{}", number),
            FileOverride::Path(path) => write!(f, "{}", path),
        }
    }
}

/// Trait for objects that can resolve offsets to line and column numbers
pub trait Resolver {
    fn resolve(&self, offset: TextSize) -> (u32, u32);
}

/// Trait for objects that can return the current file number
pub trait HasFileNumber {
    fn current_file(&self) -> FileId;
}

#[derive(Default)]
pub struct LocatedBuilder {
    /// Position at which the error occurred
    pos: PointOrRange,
    /// File identifier for the error
    current_file: Option<FileId>,
    /// Path corresponding to the file identifier
    path: Option<PathBuf>,
    /// Overriden file location
    file_override: FileOverride,
    /// Resolved line number
    line_number: u32,
    /// Resolved column number
    column: u32,
}

impl LocatedBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pos(self, pos: impl Into<PointOrRange>) -> Self {
        Self {
            pos: pos.into(),
            ..self
        }
    }

    pub fn current_file(self, file: impl Into<FileId>) -> Self {
        Self {
            current_file: Some(file.into()),
            ..self
        }
    }

    pub fn path(self, path: impl Into<PathBuf>) -> Self {
        Self {
            path: Some(path.into()),
            ..self
        }
    }

    pub fn file_override(self, file_override: impl Into<FileOverride>) -> Self {
        Self {
            file_override: file_override.into(),
            ..self
        }
    }

    pub fn line_number(self, line_number: u32) -> Self {
        Self {
            line_number,
            ..self
        }
    }

    pub fn column(self, column: u32) -> Self {
        Self { column, ..self }
    }

    pub fn resolve(self, resolver: &impl Resolver) -> Self {
        let (line, col) = resolver.resolve(self.pos.start());
        Self {
            line_number: line,
            column: col,
            ..self
        }
    }

    pub fn resolve_file(self, resolver: &(impl Resolver + HasFileNumber)) -> Self {
        self.resolve(resolver).current_file(resolver.current_file())
    }

    pub fn finish<E>(self, inner: E) -> Located<E> {
        Located {
            inner,
            pos: self.pos,
            current_file: self.current_file,
            path: self.path,
            file_override: self.file_override,
            line_number: self.line_number,
            column: self.column,
        }
    }
}

#[derive(Debug)]
pub struct Located<E> {
    /// Inner error, without location information
    inner: E,
    /// Position at which the error occurred
    pos: PointOrRange,
    /// File identifier for the error
    current_file: Option<FileId>,
    /// Path corresponding to the file identifier
    path: Option<PathBuf>,
    /// Overriden file location
    file_override: FileOverride,
    /// Resolved line number
    line_number: u32,
    /// Resolved column number
    column: u32,
}

impl<E> Located<E> {
    pub fn builder() -> LocatedBuilder {
        LocatedBuilder::default()
    }

    pub fn map<F>(self, f: impl FnOnce(E) -> F) -> Located<F> {
        Located {
            inner: f(self.inner),
            pos: self.pos,
            current_file: self.current_file,
            path: self.path,
            file_override: self.file_override,
            line_number: self.line_number,
            column: self.column,
        }
    }

    pub fn inner(&self) -> &E {
        &self.inner
    }

    pub fn into_inner(self) -> E {
        self.inner
    }

    pub fn current_file(&self) -> Option<FileId> {
        self.current_file
    }

    pub fn set_current_file(&mut self, current_file: FileId) {
        self.current_file = Some(current_file);
    }

    pub fn pos(&self) -> TextSize {
        self.pos.start()
    }

    pub fn line(&self) -> u32 {
        self.line_number
    }
}

impl<E: Clone> Clone for Located<E> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            pos: self.pos,
            current_file: self.current_file,
            path: self.path.clone(),
            file_override: self.file_override.clone(),
            line_number: self.line_number,
            column: self.column,
        }
    }
}

impl<E: PartialEq> PartialEq for Located<E> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
            && self.pos == other.pos
            && self.current_file == other.current_file
            && self.path == other.path
            && self.file_override == other.file_override
            && self.line_number == other.line_number
            && self.column == other.column
    }
}

impl<E: Eq> Eq for Located<E> {}

impl<E: std::error::Error> std::error::Error for Located<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

impl<E: std::fmt::Display> std::fmt::Display for Located<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.file_override.is_none() {
            if let Some(path) = self.path.as_ref() {
                write!(f, "{}:", path.display())?;
            } else if let Some(current_file) = self.current_file {
                write!(f, "{}:", current_file)?;
            }
        } else {
            write!(f, "{}:", self.file_override)?;
        }

        write!(
            f,
            "{}:{}: {}",
            self.line_number + 1,
            self.column + 1,
            self.inner
        )
    }
}
