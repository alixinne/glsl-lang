use std::{
    collections::{HashMap, VecDeque},
    convert::TryInto,
    iter::{FromIterator, FusedIterator},
    path::{Path, PathBuf},
    rc::Rc,
};

use smol_str::SmolStr;

use crate::{
    parser::{Parser, SyntaxKind::*, SyntaxNode, SyntaxToken},
    Ast, FileId,
};

#[macro_use]
pub mod exts;

mod fs;
pub use fs::*;

pub mod nodes;
use nodes::{
    Define, DefineObject, DirectiveResult, Extension, ExtensionName, Version,
    GL_ARB_SHADING_LANGUAGE_INCLUDE, GL_GOOGLE_INCLUDE_DIRECTIVE,
};

/// Operating mode for #include directives
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeMode {
    /// No #include directives are allowed
    None,
    /// GL_ARB_shading_language_include runtime includes
    ArbInclude,
    /// GL_GOOGLE_include_directive compile-time includes
    GoogleInclude,
}

impl Default for IncludeMode {
    fn default() -> Self {
        Self::None
    }
}

/// Current state of the preprocessor
#[derive(Debug, Clone)]
pub struct ProcessorState {
    extension_stack: Vec<Extension>,
    include_mode: IncludeMode,
    // use Rc to make cloning the whole struct cheaper
    definitions: HashMap<SmolStr, Rc<Define>>,
    version: Version,
}

impl Default for ProcessorState {
    fn default() -> Self {
        Self {
            // Spec 3.3, "The initial state of the compiler is as if the directive
            // `#extension all : disable` was issued
            extension_stack: vec![Extension::disable(ExtensionName::All)],
            // No #include extensions enabled
            include_mode: IncludeMode::None,
            // Spec 3.3, "There is a built-in macro definition for each profile the implementation
            // supports. All implementations provide the following macro:
            // `#define GL_core_profile 1`
            definitions: HashMap::from_iter([(
                "GL_core_profile".into(),
                Rc::new(Define::object(
                    "GL_core_profile".into(),
                    DefineObject::from_str("1").unwrap(),
                    true,
                )),
            )]),
            version: Version::default(),
        }
    }
}

/// Preprocessor
#[derive(Debug)]
pub struct Processor<F: FileSystem> {
    /// Cache of parsed files (preprocessor token sequences)
    file_cache: HashMap<FileId, Ast>,
    /// Mapping from canonical paths to FileIds
    file_ids: HashMap<PathBuf, FileId>,
    /// Mapping from #include/input paths to canonical paths
    canonical_paths: HashMap<PathBuf, PathBuf>,
    /// Current state of the preprocessor
    current_state: ProcessorState,
    /// Filesystem abstraction
    fs: F,
}

impl<F: FileSystem + Default> Processor<F> {
    pub fn new(initial_state: ProcessorState) -> Self {
        Self {
            current_state: initial_state,
            ..Default::default()
        }
    }
}

impl<F: FileSystem> Processor<F> {
    pub fn new_with_fs(initial_state: ProcessorState, fs: F) -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: HashMap::with_capacity(1),
            canonical_paths: HashMap::with_capacity(1),
            current_state: initial_state,
            fs,
        }
    }

    pub fn process(&mut self, entry: &Path) -> ProcessorEvents<F> {
        ProcessorEvents {
            processor: Some(self),
            file_stack: vec![entry.to_owned()],
            event_buf: Default::default(),
        }
    }

    fn parse(&mut self, path: &Path) -> Result<(FileId, &Ast), F::Error> {
        // Find the canonical path
        let canonical_path = if let Some(canonical_path) = self.canonical_paths.get(path) {
            canonical_path
        } else {
            let canonical_path = self.fs.canonicalize(&path)?;
            self.canonical_paths
                .entry(path.to_owned())
                .or_insert(canonical_path)
        };

        // Allocate a file id
        let file_id = if let Some(file_id) = self.file_ids.get(canonical_path.as_path()) {
            file_id
        } else {
            let file_id = FileId::new(self.file_ids.len() as _);
            self.file_ids
                .entry(canonical_path.to_owned())
                .or_insert(file_id)
        };

        if !self.file_cache.contains_key(file_id) {
            // Read the file
            let input = self.fs.read(&canonical_path)?;
            // Parse it
            let ast = Parser::new(&input).parse();
            self.file_cache.insert(*file_id, ast);
        }

        // Return the parsed result
        Ok(self
            .file_cache
            .get_key_value(&file_id)
            .map(|(&k, v)| (k, v))
            .unwrap())
    }

    fn expand(&mut self, ast: Ast) -> Vec<Event<F::Error>> {
        let (root, mut errors) = ast.into_inner();

        // TODO: Smarter capacity calculation
        let mut result = Vec::with_capacity(1024);

        for node_or_token in root.children_with_tokens() {
            if let Some(first) = errors.first() {
                if node_or_token.text_range().end() >= first.pos().start() {
                    result.push(Event::ParseError(errors.pop().unwrap()));
                }
            }

            match node_or_token {
                rowan::NodeOrToken::Node(node) => {
                    match node.kind() {
                        PP_EMPTY => {
                            // Discard
                        }
                        PP_VERSION => {
                            // TODO: Check that the version is the first thing in the file?

                            let directive: DirectiveResult<Version> = node.try_into();

                            if let Ok(version) = &directive {
                                self.current_state.version = **version;
                            }

                            result.push(Event::Version { directive });
                        }
                        PP_EXTENSION => {
                            let directive: DirectiveResult<Extension> = node.try_into();

                            if let Ok(extension) = &directive {
                                // Push onto the stack
                                self.current_state
                                    .extension_stack
                                    .push((**extension).clone());

                                let target_include_mode =
                                    if extension.name == *GL_ARB_SHADING_LANGUAGE_INCLUDE {
                                        Some(IncludeMode::ArbInclude)
                                    } else if extension.name == *GL_GOOGLE_INCLUDE_DIRECTIVE {
                                        Some(IncludeMode::GoogleInclude)
                                    } else {
                                        None
                                    };

                                if let Some(target) = target_include_mode {
                                    if extension.behavior.is_active() {
                                        self.current_state.include_mode = target;
                                    } else {
                                        // TODO: Implement current mode as a stack?
                                        self.current_state.include_mode = IncludeMode::None;
                                    }
                                }
                            }

                            result.push(Event::Extension { directive });
                        }
                        _ => {
                            // Handle node, this is a preprocessor directive
                            result.push(Event::Unhandled(node));
                        }
                    }
                }
                rowan::NodeOrToken::Token(token) => {
                    result.push(Event::Token(token));
                }
            }
        }

        result
    }
}

#[derive(Debug)]
pub enum Event<E: std::error::Error> {
    IoError(E),
    EnterFile {
        file_id: FileId,
        path: PathBuf,
    },
    ParseError(crate::parser::Error),
    Token(SyntaxToken),
    Version {
        directive: DirectiveResult<Version>,
    },
    Extension {
        directive: DirectiveResult<Extension>,
    },
    Unhandled(SyntaxNode),
}

pub struct ProcessorEvents<'p, F: FileSystem> {
    processor: Option<&'p mut Processor<F>>,
    file_stack: Vec<PathBuf>,
    event_buf: VecDeque<Event<F::Error>>,
}

impl<'p, F: FileSystem> Iterator for ProcessorEvents<'p, F> {
    type Item = Event<F::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(processor) = &mut self.processor {
                // First, check if we have buffered any events
                if let Some(event) = self.event_buf.pop_front() {
                    return Some(event);
                }

                // Then, check how we can generate more events
                if let Some(file) = self.file_stack.pop() {
                    // An unprocessed file
                    match processor.parse(&file) {
                        Ok((file_id, ast)) => {
                            let ast = ast.clone();

                            // We entered a file
                            self.event_buf.push_back(Event::EnterFile {
                                file_id,
                                path: file.to_owned(),
                            });

                            // Add all preprocessor events
                            self.event_buf.extend(processor.expand(ast));

                            continue;
                        }
                        Err(err) => {
                            // Failed reading the file
                            return Some(Event::IoError(err));
                        }
                    }
                }

                // If we get here, there are no more events we can generate
                self.processor.take();
                return None;
            } else {
                return None;
            }
        }
    }
}

impl<F: FileSystem> FusedIterator for ProcessorEvents<'_, F> {}

impl<F: FileSystem + Default> Default for Processor<F> {
    fn default() -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: HashMap::with_capacity(1),
            canonical_paths: HashMap::with_capacity(1),
            current_state: Default::default(),
            fs: F::default(),
        }
    }
}
