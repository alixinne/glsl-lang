//! Located type definition

use std::{
    fmt,
    path::{Path, PathBuf},
};

use text_size::{TextRange, TextSize};

use super::FileId;

/// Represents a file location override
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
    /// Return true if this file override is empty
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
            FileOverride::Number(number) => write!(f, "{number}"),
            FileOverride::Path(path) => write!(f, "{path}"),
        }
    }
}

/// Trait for objects that can resolve offsets to line and column numbers
pub trait Resolver {
    /// Resolve the raw offset into a (line, column) tuple
    ///
    /// # Parameters
    ///
    /// * `offset`: raw offset into the source string
    fn resolve(&self, offset: TextSize) -> (u32, u32);
}

impl Resolver for &str {
    fn resolve(&self, offset: TextSize) -> (u32, u32) {
        let offset: usize = offset.into();
        let offset = if offset >= self.len() {
            self.len().max(1) - 1
        } else {
            offset
        };

        // Find line start offset
        let line_start = line_span::find_line_start(self, offset);

        // Count newlines
        let line_index = self
            .bytes()
            .take(line_start)
            .filter(|c| *c == b'\n')
            .count();

        // Find column
        let pos_index = offset - line_start;

        (line_index as _, pos_index as _)
    }
}

/// Trait for objects that can return the current file number
pub trait HasFileNumber {
    /// Return the current file identifier
    fn current_file(&self) -> FileId;
}

/// Trait for resolving file identifiers to file names
pub trait FileIdResolver {
    /// Return the path corresponding to the FileId, if known
    fn resolve(&self, file_id: FileId) -> Option<&Path>;
}

/// Builder for a [Located] struct
#[derive(Default)]
pub struct LocatedBuilder {
    /// Position at which the error occurred
    pos: TextRange,
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
    /// Create a new builder for [Located] with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the raw position
    pub fn pos(self, pos: impl Into<TextRange>) -> Self {
        Self {
            pos: pos.into(),
            ..self
        }
    }

    /// Set the file identifier
    pub fn current_file(self, file: impl Into<FileId>) -> Self {
        Self {
            current_file: Some(file.into()),
            ..self
        }
    }

    /// Set the source path
    pub fn path(self, path: impl Into<PathBuf>) -> Self {
        Self {
            path: Some(path.into()),
            ..self
        }
    }

    /// Set the source file override
    pub fn file_override(self, file_override: impl Into<FileOverride>) -> Self {
        Self {
            file_override: file_override.into(),
            ..self
        }
    }

    /// Set the resolved line number
    pub fn line_number(self, line_number: u32) -> Self {
        Self {
            line_number,
            ..self
        }
    }

    /// Set the resolved column number
    pub fn column(self, column: u32) -> Self {
        Self { column, ..self }
    }

    /// Resolve the raw offset (see [LocatedBuilder::pos]) to line and column information
    pub fn resolve(self, resolver: &impl Resolver) -> Self {
        let (line, col) = resolver.resolve(self.pos.start());
        Self {
            line_number: line,
            column: col,
            ..self
        }
    }

    /// Resolve the raw offset (see [LocatedBuilder::pos]) to line and column information, and set
    /// the current file information
    pub fn resolve_file(self, resolver: &(impl Resolver + HasFileNumber)) -> Self {
        self.resolve(resolver).current_file(resolver.current_file())
    }

    /// Resolve the given file id into a path name, and store it in this builder
    pub fn resolve_path(self, resolver: &impl FileIdResolver) -> Self {
        Self {
            path: self
                .current_file
                .and_then(|current_file| resolver.resolve(current_file).map(Path::to_owned)),
            ..self
        }
    }

    /// Build the final [Located] object from the given inner object
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

/// Wraps an object with location data
#[derive(Debug)]
pub struct Located<E> {
    /// Inner error, without location information
    inner: E,
    /// Position at which the error occurred
    pos: TextRange,
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
    /// Create a builder for this located type
    pub fn builder() -> LocatedBuilder {
        LocatedBuilder::default()
    }

    /// Transform the inner value wrapped by this instance
    ///
    /// # Parameters
    ///
    /// * `f`: function to apply to the inner value
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

    /// Return a reference to the inner value
    pub fn inner(&self) -> &E {
        &self.inner
    }

    /// Return the inner value
    pub fn into_inner(self) -> E {
        self.inner
    }

    /// Get the current file identifier
    pub fn current_file(&self) -> Option<FileId> {
        self.current_file
    }

    /// Set the current file identifier
    pub fn set_current_file(&mut self, current_file: FileId) {
        self.current_file = Some(current_file);
    }

    /// Get the raw position into the source
    pub fn pos(&self) -> TextRange {
        self.pos
    }

    /// Get the line number for this location
    pub fn line(&self) -> u32 {
        self.line_number
    }

    /// Get the column number for this location
    pub fn col(&self) -> u32 {
        self.column
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

impl<E: std::error::Error> std::error::Error for Located<E> {}

impl<E: std::fmt::Display> std::fmt::Display for Located<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.file_override.is_none() {
            if let Some(path) = self.path.as_ref() {
                write!(f, "{}:", path.display())?;
            } else if let Some(current_file) = self.current_file {
                write!(f, "{current_file}:")?;
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

#[cfg(test)]
mod tests {
    use super::Resolver;
    use std::convert::TryInto;

    #[test]
    fn resolved_position() {
        let s = r#"
Hello,
World"#;

        let offset = s.find('r').unwrap().try_into().unwrap();
        let resolved = s.resolve(offset);

        assert_eq!(resolved.0, 2);
        assert_eq!(resolved.1, 2);
    }

    #[test]
    fn resolved_position_last_char() {
        let s = r#"
Hello,
World"#;

        let offset = s.find('d').unwrap().try_into().unwrap();
        let resolved = s.resolve(offset);

        assert_eq!(resolved.0, 2);
        assert_eq!(resolved.1, 4);
    }

    #[test]
    fn resolved_position_out_of_bounds() {
        let offset = 1.into();
        assert_eq!("".resolve(offset).0, 0);
    }
}
