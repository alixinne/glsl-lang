use std::{
    collections::{HashMap, VecDeque},
    convert::TryInto,
    iter::{FromIterator, FusedIterator},
    path::{Path, PathBuf},
    rc::Rc,
};

use smol_str::SmolStr;
use thiserror::Error;

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
    Define, DefineObject, DirectiveResult, Extension, ExtensionName, IfDef, IfNDef, Undef, Version,
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

        #[derive(Clone, Copy, PartialEq, Eq)]
        enum IfState {
            /// No #if group of this level was included
            None,
            /// The current #if group of this level is included
            Active { else_seen: bool },
            /// One past #if group of this level was included, but not the current one
            One { else_seen: bool },
        }

        // TODO: Smarter capacity calculation
        let mut result = Vec::with_capacity(1024);
        let mut mask_stack = Vec::with_capacity(4);
        let mut mask_active = true;

        for node_or_token in root.children_with_tokens() {
            if let Some(first) = errors.first() {
                if node_or_token.text_range().end() >= first.pos().start() {
                    let error = errors.pop().unwrap();

                    // Parse errors in non-included blocks are ignored
                    if mask_active {
                        result.push(Event::ParseError(error));
                    }
                }
            }

            match node_or_token {
                rowan::NodeOrToken::Node(node) => {
                    match node.kind() {
                        PP_EMPTY => {
                            // Discard
                        }
                        PP_VERSION => {
                            if mask_active {
                                // TODO: Check that the version is the first thing in the file?

                                let directive: DirectiveResult<Version> = node.try_into();

                                if let Ok(version) = &directive {
                                    self.current_state.version = **version;
                                }

                                result.push(Event::Version { directive });
                            }
                        }
                        PP_EXTENSION => {
                            if mask_active {
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
                        }
                        PP_IFDEF => {
                            if mask_active {
                                let directive: DirectiveResult<IfDef> = node.try_into();

                                if let Ok(ifdef) = &directive {
                                    // Update masking state
                                    mask_active =
                                        self.current_state.definitions.contains_key(&ifdef.ident);
                                    mask_stack.push(IfState::Active { else_seen: false });
                                }

                                result.push(Event::IfDef { directive });
                            } else {
                                // Record the #ifdef in the stack to support nesting
                                mask_stack.push(IfState::None);
                            }
                        }
                        PP_IFNDEF => {
                            if mask_active {
                                let directive: DirectiveResult<IfNDef> = node.try_into();

                                if let Ok(ifdef) = &directive {
                                    // Update masking state
                                    mask_active =
                                        !self.current_state.definitions.contains_key(&ifdef.ident);
                                    mask_stack.push(IfState::Active { else_seen: false });
                                }

                                result.push(Event::IfNDef { directive });
                            } else {
                                // Record the #ifdef in the stack to support nesting
                                mask_stack.push(IfState::None);
                            }
                        }
                        PP_ELSE => {
                            if let Some(top) = mask_stack.pop() {
                                match top {
                                    IfState::None => {
                                        mask_active = mask_stack
                                            .last()
                                            .map(|top| matches!(*top, IfState::Active { .. }))
                                            .unwrap_or(true);

                                        mask_stack.push(IfState::Active { else_seen: true });
                                    }
                                    IfState::Active { else_seen } | IfState::One { else_seen } => {
                                        if else_seen {
                                            // Extra #else
                                            result.push(Event::ProcessorError(
                                                ProcessorError::ExtraElse { node },
                                            ));

                                            continue;
                                        } else {
                                            mask_active = false;
                                            mask_stack.push(IfState::One { else_seen: true });
                                        }
                                    }
                                }

                                result.push(Event::Else);
                            } else {
                                // Stray #else
                                result.push(Event::ProcessorError(ProcessorError::ExtraElse {
                                    node,
                                }));
                            }
                        }
                        PP_ENDIF => {
                            if let Some(_) = mask_stack.pop() {
                                mask_active = mask_stack
                                    .last()
                                    .map(|top| matches!(*top, IfState::Active { .. }))
                                    .unwrap_or(true);

                                // TODO: Return syntax node?
                                if mask_active {
                                    result.push(Event::EndIf);
                                }
                            } else {
                                // Stray #endif
                                result.push(Event::ProcessorError(ProcessorError::ExtraEndIf {
                                    node,
                                }));
                            }
                        }
                        PP_UNDEF => {
                            if mask_active {
                                let directive: DirectiveResult<Undef> = node.clone().try_into();

                                let protected_ident = if let Ok(ifdef) = &directive {
                                    if let Some(def) =
                                        self.current_state.definitions.get(&ifdef.ident)
                                    {
                                        if def.protected() {
                                            Some(ifdef.ident.clone())
                                        } else {
                                            self.current_state.definitions.remove(&ifdef.ident);
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };

                                result.push(Event::Undef { directive });

                                if let Some(ident) = protected_ident {
                                    result.push(Event::ProcessorError(
                                        ProcessorError::ProtectedDefine { node, ident },
                                    ));
                                }
                            }
                        }
                        _ => {
                            // Handle node, this is a preprocessor directive
                            result.push(Event::Unhandled(node));
                        }
                    }
                }
                rowan::NodeOrToken::Token(token) => {
                    if mask_active {
                        result.push(Event::Token(token));
                    }
                }
            }
        }

        result
    }
}

#[derive(Debug, Error)]
pub enum ProcessorError {
    #[error("unmatched #endif")]
    ExtraEndIf { node: SyntaxNode },
    #[error("unmatched #else")]
    ExtraElse { node: SyntaxNode },
    #[error("protected definition cannot be undefined")]
    ProtectedDefine { node: SyntaxNode, ident: SmolStr },
}

#[derive(Debug)]
pub enum Event<E: std::error::Error> {
    IoError(E),
    ParseError(crate::parser::Error),
    ProcessorError(ProcessorError),
    EnterFile {
        file_id: FileId,
        path: PathBuf,
    },
    Token(SyntaxToken),
    Version {
        directive: DirectiveResult<Version>,
    },
    Extension {
        directive: DirectiveResult<Extension>,
    },
    IfDef {
        directive: DirectiveResult<IfDef>,
    },
    IfNDef {
        directive: DirectiveResult<IfNDef>,
    },
    Else,
    EndIf,
    Undef {
        directive: DirectiveResult<Undef>,
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
