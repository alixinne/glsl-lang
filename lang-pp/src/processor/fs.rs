use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};

use encoding_rs::Encoding;

use lang_util::FileId;

use crate::parser::{Ast, Parser};

use super::{Expand, ProcessorState};

pub trait FileSystem {
    type Error: std::error::Error + 'static;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error>;
    fn read(&self, path: &Path, encoding: Option<&'static Encoding>)
        -> Result<String, Self::Error>;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Std;

impl FileSystem for Std {
    type Error = std::io::Error;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error> {
        std::fs::canonicalize(path)
    }

    fn read(
        &self,
        path: &Path,
        encoding: Option<&'static Encoding>,
    ) -> Result<String, Self::Error> {
        if let Some(encoding) = encoding {
            let bytes = std::fs::read(path)?;
            Ok(encoding.decode(&bytes).0.to_string())
        } else {
            std::fs::read_to_string(path)
        }
    }
}

pub type StdProcessor = Processor<Std>;

#[derive(Debug, Clone, Copy)]
pub struct ParsedFile<'a> {
    file_id: FileId,
    ast: &'a Ast,
}

impl<'a> ParsedFile<'a> {
    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn ast(&self) -> &Ast {
        self.ast
    }

    pub fn process(self, initial_state: ProcessorState) -> Expand {
        Expand::new(self, initial_state)
    }
}

impl<'a> From<ParsedFile<'a>> for (FileId, Ast) {
    fn from(parsed_file: ParsedFile<'a>) -> Self {
        (parsed_file.file_id, parsed_file.ast.clone())
    }
}

/// Preprocessor based on a filesystem
#[derive(Debug)]
pub struct Processor<F: FileSystem> {
    /// Cache of parsed files (preprocessor token sequences)
    file_cache: HashMap<FileId, Ast>,
    /// Mapping from canonical paths to FileIds
    file_ids: HashMap<PathBuf, FileId>,
    /// Mapping from #include/input paths to canonical paths
    canonical_paths: HashMap<PathBuf, PathBuf>,
    /// Filesystem abstraction
    fs: F,
}

impl<F: FileSystem> Processor<F> {
    pub fn new_with_fs(fs: F) -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: HashMap::with_capacity(1),
            canonical_paths: HashMap::with_capacity(1),
            fs,
        }
    }

    pub fn parse(
        &mut self,
        path: &Path,
        encoding: Option<&'static Encoding>,
    ) -> Result<ParsedFile, F::Error> {
        // Find the canonical path. Not using the entry API because cloning a path is expensive.
        let canonical_path = if let Some(canonical_path) = self.canonical_paths.get(path) {
            canonical_path
        } else {
            let canonical_path = self.fs.canonicalize(&path)?;
            // Using entry allows inserting and returning a reference
            self.canonical_paths
                .entry(path.to_owned())
                .or_insert(canonical_path)
        };

        // Allocate a file id. Not using the entry API because cloning a path is expensive.
        let file_id = if let Some(file_id) = self.file_ids.get(canonical_path.as_path()) {
            *file_id
        } else {
            let file_id = FileId::new(self.file_ids.len() as _);
            self.file_ids.insert(canonical_path.to_owned(), file_id);
            file_id
        };

        match self.file_cache.entry(file_id) {
            Entry::Occupied(entry) => Ok(ParsedFile {
                file_id,
                ast: entry.into_mut(),
            }),
            Entry::Vacant(entry) => {
                // Read the file
                let input = self.fs.read(&canonical_path, encoding)?;
                // Parse it
                let ast = Parser::new(&input).parse();
                // Check that the root node covers the entire range
                debug_assert_eq!(u32::from(ast.green_node().text_len()), input.len() as u32);

                Ok(ParsedFile {
                    file_id,
                    ast: entry.insert(ast),
                })
            }
        }
    }
}

impl<F: FileSystem + Default> Processor<F> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<F: FileSystem + Default> Default for Processor<F> {
    fn default() -> Self {
        Self::new_with_fs(F::default())
    }
}
