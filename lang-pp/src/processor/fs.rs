use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};

use bimap::BiHashMap;
use encoding_rs::Encoding;

use lang_util::FileId;

use crate::parser::{Ast, Parser};

use super::{
    event::{Event, IoEvent, Located, ProcessingErrorKind},
    expand::{ExpandEvent, ExpandOne},
    nodes::{ParsedPath, PathType},
    ProcessorState,
};

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

pub struct ExpandStack<'p, F: FileSystem> {
    processor: &'p mut Processor<F>,
    stack: Vec<ExpandOne>,
    state: Option<ProcessorState>,
}

impl<'p, F: FileSystem> ExpandStack<'p, F> {
    pub fn into_state(self) -> Option<ProcessorState> {
        self.state
    }
}

impl<'p, F: FileSystem> Iterator for ExpandStack<'p, F> {
    type Item = IoEvent<F::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(mut expand) = self.stack.pop() {
                let result = expand.next();

                if let Some(event) = result {
                    match event {
                        ExpandEvent::Event(event) => {
                            // Put it back on the stack
                            self.stack.push(expand);

                            return Some(match event {
                                Event::EnterFile(file_id) => {
                                    let (canonical_path, input_path) =
                                        self.processor.get_paths(file_id).unwrap();

                                    IoEvent::EnterFile {
                                        file_id,
                                        path: input_path.to_owned(),
                                        canonical_path: canonical_path.to_owned(),
                                    }
                                }
                                Event::Error { error, masked } => IoEvent::Error { error, masked },
                                Event::Token { token, masked } => IoEvent::Token { token, masked },
                                Event::Directive { node, kind, masked } => {
                                    IoEvent::Directive { node, kind, masked }
                                }
                            });
                        }
                        ExpandEvent::EnterFile(current_state, node, path) => {
                            // Put it back on the stack
                            self.stack.push(expand);

                            // Get the location
                            let location = self.stack.last().unwrap().location();

                            // We are supposed to enter a new file
                            // First, parse it using the preprocessor
                            if let Some(resolved_path) =
                                self.processor.resolve(location.current_file(), &path)
                            {
                                // TODO: Allow passing an encoding from somewhere
                                match self.processor.parse(&resolved_path, None) {
                                    Ok(parsed) => {
                                        self.stack.push(parsed.expand_one(current_state));
                                    }
                                    Err(error) => {
                                        // Just return the error, we'll keep iterating on the lower
                                        // file by looping
                                        return Some(IoEvent::IoError(Located::new(
                                            error,
                                            resolved_path,
                                            node.text_range(),
                                            location,
                                        )));
                                    }
                                }
                            } else {
                                // Resolving the path failed, throw an error located at the
                                // right place
                                return Some(IoEvent::error(
                                    ProcessingErrorKind::IncludeNotFound { path }
                                        .with_node(node.into(), &location),
                                    location,
                                    false,
                                ));
                            }
                        }
                        ExpandEvent::Completed(state) => {
                            if let Some(last) = self.stack.last_mut() {
                                // Propagate the updated state upwards in the stack
                                last.set_state(state);
                            } else {
                                // No more, store the final state
                                self.state = Some(state);
                            }
                        }
                    }
                }
            } else {
                return None;
            }
        }
    }
}

#[derive(Debug)]
pub struct ParsedFile<'p, F: FileSystem> {
    processor: &'p mut Processor<F>,
    file_id: FileId,
}

impl<'p, F: FileSystem> ParsedFile<'p, F> {
    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn process(self, initial_state: ProcessorState) -> ExpandStack<'p, F> {
        let ast = self.ast();

        ExpandStack {
            processor: self.processor,
            stack: vec![ExpandOne::new((self.file_id, ast), initial_state)],
            state: None,
        }
    }

    pub fn ast(&self) -> Ast {
        self.processor
            .file_cache
            .get(&self.file_id)
            .unwrap()
            .clone()
    }

    fn expand_one(self, initial_state: ProcessorState) -> ExpandOne {
        ExpandOne::new(self, initial_state)
    }
}

impl<'p, F: FileSystem> From<ParsedFile<'p, F>> for (FileId, Ast) {
    fn from(parsed_file: ParsedFile<'p, F>) -> Self {
        let ast = parsed_file.ast();
        (parsed_file.file_id, ast)
    }
}

/// Preprocessor based on a filesystem
#[derive(Debug)]
pub struct Processor<F: FileSystem> {
    /// Cache of parsed files (preprocessor token sequences)
    file_cache: HashMap<FileId, Ast>,
    /// Mapping from canonical paths to FileIds
    file_ids: BiHashMap<PathBuf, FileId>,
    /// Mapping from #include/input paths to canonical paths
    canonical_paths: BiHashMap<PathBuf, PathBuf>,
    /// Filesystem abstraction
    fs: F,
}

impl<F: FileSystem> Processor<F> {
    pub fn new_with_fs(fs: F) -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: BiHashMap::with_capacity(1),
            canonical_paths: BiHashMap::with_capacity(1),
            fs,
        }
    }

    pub fn get_paths(&self, file_id: FileId) -> Option<(&PathBuf, &PathBuf)> {
        // Find the canonical path for the current file identifier
        let canonical_path = self.file_ids.get_by_right(&file_id)?;

        // Transform that back into a non-canonical path
        let input_path = self.canonical_paths.get_by_right(canonical_path)?;

        Some((canonical_path, input_path))
    }

    pub fn resolve(&self, relative_to: FileId, path: &ParsedPath) -> Option<PathBuf> {
        let (_, input_path) = self.get_paths(relative_to)?;

        match path.ty {
            PathType::Angle => {
                // TODO: Handle angle-quoted paths
                None
            }
            PathType::Quote => Some(input_path.parent()?.join(&path.path)),
        }
    }

    pub fn parse(
        &mut self,
        path: &Path,
        encoding: Option<&'static Encoding>,
    ) -> Result<ParsedFile<F>, F::Error> {
        // Find the canonical path. Not using the entry API because cloning a path is expensive.
        let canonical_path = if let Some(canonical_path) = self.canonical_paths.get_by_left(path) {
            canonical_path
        } else {
            let canonical_path = self.fs.canonicalize(&path)?;
            self.canonical_paths.insert(path.to_owned(), canonical_path);
            self.canonical_paths.get_by_left(path).unwrap()
        };

        // Allocate a file id. Not using the entry API because cloning a path is expensive.
        let file_id = if let Some(file_id) = self.file_ids.get_by_left(canonical_path.as_path()) {
            *file_id
        } else {
            let file_id = FileId::new(self.file_ids.len() as _);
            self.file_ids.insert(canonical_path.to_owned(), file_id);
            file_id
        };

        match self.file_cache.entry(file_id) {
            Entry::Occupied(_) => Ok(ParsedFile {
                processor: self,
                file_id,
            }),
            Entry::Vacant(entry) => {
                // Read the file
                let input = self.fs.read(&canonical_path, encoding)?;
                // Parse it
                let ast = Parser::new(&input).parse();
                // Check that the root node covers the entire range
                debug_assert_eq!(u32::from(ast.green_node().text_len()), input.len() as u32);
                // Insert it
                entry.insert(ast);

                Ok(ParsedFile {
                    processor: self,
                    file_id,
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
