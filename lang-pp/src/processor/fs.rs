use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};

use bimap::BiHashMap;
use encoding_rs::Encoding;

use lang_util::{
    located::{FileIdResolver, Located, LocatedBuilder},
    FileId,
};

use crate::{
    last::LocatedIterator,
    parser::{Ast, Parser},
};

use super::{
    event::{Event, ProcessingErrorKind},
    expand::{ExpandEvent, ExpandOne},
    nodes::{ParsedPath, PathType},
    ProcessorState,
};

pub trait FileSystem {
    type Error: std::error::Error + 'static;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error>;
    fn exists(&self, path: &Path) -> bool;
    fn read(
        &self,
        path: &Path,
        encoding: Option<&'static Encoding>,
    ) -> Result<std::borrow::Cow<'_, str>, Self::Error>;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Std;

impl FileSystem for Std {
    type Error = std::io::Error;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error> {
        std::fs::canonicalize(path)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn read(
        &self,
        path: &Path,
        encoding: Option<&'static Encoding>,
    ) -> Result<std::borrow::Cow<'_, str>, Self::Error> {
        if let Some(encoding) = encoding {
            let bytes = std::fs::read(path)?;
            Ok(encoding.decode(&bytes).0.to_string().into())
        } else {
            std::fs::read_to_string(path).map(Into::into)
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
    pub fn tokenize(
        self,
        current_version: u16,
        target_vulkan: bool,
        registry: &crate::exts::Registry,
    ) -> crate::last::Tokenizer<'_, Self> {
        crate::last::Tokenizer::new(self, current_version, target_vulkan, registry)
    }

    pub fn into_state(self) -> Option<ProcessorState> {
        self.state
    }
}

impl<'p, F: FileSystem> Iterator for ExpandStack<'p, F> {
    type Item = Result<Event, Located<F::Error>>;

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
                                Event::EnterFile { file_id, .. } => {
                                    if let Some((canonical_path, input_path)) =
                                        self.processor.get_paths(file_id)
                                    {
                                        Ok(Event::EnterFile {
                                            file_id,
                                            path: input_path.to_owned(),
                                            canonical_path: canonical_path.to_owned(),
                                        })
                                    } else {
                                        // Source block, no file path available
                                        Ok(Event::EnterFile {
                                            file_id,
                                            path: Default::default(),
                                            canonical_path: Default::default(),
                                        })
                                    }
                                }
                                other => Ok(other),
                            });
                        }
                        ExpandEvent::EnterFile(node, path) => {
                            let state = expand.state().unwrap().clone();

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
                                        self.stack.push(parsed.expand_one(state));
                                    }
                                    Err(error) => {
                                        // Just return the error, we'll keep iterating on the lower
                                        // file by looping
                                        return Some(Err(LocatedBuilder::new()
                                            .pos(node.text_range())
                                            .path(resolved_path)
                                            .resolve_file(location)
                                            .finish(error)));
                                    }
                                }
                            } else {
                                // Resolving the path failed, throw an error located at the
                                // right place
                                return Some(Ok(Event::error(
                                    ProcessingErrorKind::IncludeNotFound { path },
                                    node.text_range(),
                                    location,
                                    false,
                                )));
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

impl<'p, F: FileSystem> FileIdResolver for ExpandStack<'p, F> {
    fn resolve(&self, file_id: FileId) -> Option<&Path> {
        <Processor<F> as FileIdResolver>::resolve(self.processor, file_id)
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

impl<'p, F: FileSystem> IntoIterator for ParsedFile<'p, F> {
    type Item = <ExpandStack<'p, F> as Iterator>::Item;
    type IntoIter = ExpandStack<'p, F>;

    fn into_iter(self) -> Self::IntoIter {
        self.process(ProcessorState::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
enum PathOrSource {
    Source(usize, PathBuf),
    Path(PathBuf),
}

impl PathOrSource {
    fn as_path(&self) -> Option<&PathBuf> {
        match self {
            PathOrSource::Source(_, _) => None,
            PathOrSource::Path(path) => Some(path),
        }
    }
}

/// Preprocessor based on a filesystem
#[derive(Debug)]
pub struct Processor<F: FileSystem> {
    /// Cache of parsed files (preprocessor token sequences)
    file_cache: HashMap<FileId, Ast>,
    /// Mapping from canonical paths to FileIds
    file_ids: BiHashMap<PathOrSource, FileId>,
    /// Mapping from #include/input paths to canonical paths
    canonical_paths: BiHashMap<PathBuf, PathBuf>,
    /// List of include paths in resolution order
    system_paths: Vec<PathBuf>,
    /// Filesystem abstraction
    fs: F,
}

impl<F: FileSystem> Processor<F> {
    pub fn new_with_fs(fs: F) -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: BiHashMap::with_capacity(1),
            canonical_paths: BiHashMap::with_capacity(1),
            system_paths: Vec::new(),
            fs,
        }
    }

    fn get_paths(&self, file_id: FileId) -> Option<(&PathBuf, &PathBuf)> {
        // Find the canonical path for the current file identifier
        let canonical_path = self.file_ids.get_by_right(&file_id)?;

        let canonical_path = match canonical_path {
            PathOrSource::Source(_, _) => {
                return None;
            }
            PathOrSource::Path(path) => path,
        };

        // Transform that back into a non-canonical path
        let input_path = self.canonical_paths.get_by_right(canonical_path)?;

        Some((canonical_path, input_path))
    }

    pub fn system_paths(&self) -> &Vec<PathBuf> {
        &self.system_paths
    }

    pub fn system_paths_mut(&mut self) -> &mut Vec<PathBuf> {
        &mut self.system_paths
    }

    fn resolve(&self, relative_to: FileId, path: &ParsedPath) -> Option<PathBuf> {
        let parent = self
            .file_ids
            .get_by_right(&relative_to)
            .and_then(|key| match key {
                PathOrSource::Source(_, dir) => Some(dir.as_path()),
                PathOrSource::Path(path) => self
                    .canonical_paths
                    .get_by_right(path)
                    .and_then(|path| path.parent()),
            })?;

        match path.ty {
            PathType::Angle => self.system_paths.iter().find_map(|system_path| {
                let full_path = system_path.join(&path.path);
                self.fs.exists(&full_path).then(|| full_path)
            }),
            PathType::Quote => Some(parent.join(&path.path)),
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
            let canonical_path = self.fs.canonicalize(path)?;
            self.canonical_paths.insert(path.to_owned(), canonical_path);
            self.canonical_paths.get_by_left(path).unwrap()
        };

        // Allocate a file id. Not using the entry API because cloning a path is expensive.
        let key: PathOrSource = canonical_path.to_owned().into();
        let file_id = if let Some(file_id) = self.file_ids.get_by_left(&key) {
            *file_id
        } else {
            let file_id = FileId::new(self.file_ids.len() as _);
            self.file_ids.insert(key, file_id);
            file_id
        };

        match self.file_cache.entry(file_id) {
            Entry::Occupied(_) => Ok(ParsedFile {
                processor: self,
                file_id,
            }),
            Entry::Vacant(entry) => {
                // Read the file
                let input = self.fs.read(canonical_path, encoding)?;
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

    /// Parse a given source block as if it belonged in a specific directory
    ///
    /// # Parameters
    ///
    /// * `source`: GLSL source block to parse
    /// * `path`: path to the directory that (virtually) contains this GLSL source block
    pub fn parse_source(&mut self, source: &str, path: &Path) -> ParsedFile<F> {
        // Create key for this source block
        let key = PathOrSource::Source(self.file_ids.len(), path.to_owned());

        // Register file id
        let file_id = FileId::new(self.file_ids.len() as _);
        self.file_ids.insert(key, file_id);

        // Parse the source
        let ast = Parser::new(source).parse();
        // Check that the root node covers the entire range
        debug_assert_eq!(u32::from(ast.green_node().text_len()), source.len() as u32);
        // Insert into the cache
        self.file_cache.insert(file_id, ast);

        ParsedFile {
            processor: self,
            file_id,
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

impl<F: FileSystem> FileIdResolver for Processor<F> {
    fn resolve(&self, file_id: FileId) -> Option<&Path> {
        self.file_ids
            .get_by_right(&file_id)
            .and_then(PathOrSource::as_path)
            .and_then(|canonical_path| self.canonical_paths.get_by_right(canonical_path))
            .map(|pathbuf| pathbuf.as_path())
    }
}

impl<'p, F: FileSystem> LocatedIterator for ExpandStack<'p, F> {
    fn location(&self) -> &crate::processor::expand::ExpandLocation {
        self.stack.last().unwrap().location()
    }
}
