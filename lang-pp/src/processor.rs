use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    convert::TryInto,
    iter::{FromIterator, FusedIterator},
    num::NonZeroU32,
    path::{Path, PathBuf},
    rc::Rc,
};

use arrayvec::ArrayVec;
use rowan::{GreenNodeBuilder, NodeOrToken, SyntaxElementChildren, TextRange};
use smol_str::SmolStr;

use crate::{
    lexer::LineMap,
    parser::{
        self, Parser, PreprocessorLang,
        SyntaxKind::{self, *},
        SyntaxNode, SyntaxToken,
    },
    Ast, FileId, Unescaped,
};

#[macro_use]
pub mod exts;

mod event;
pub use event::*;

mod fs;
pub use fs::*;

pub mod nodes;
use nodes::{
    Define, DefineKind, DefineObject, DirectiveResult, Error, Extension, ExtensionName, IfDef,
    IfNDef, Undef, Version, GL_ARB_SHADING_LANGUAGE_INCLUDE, GL_GOOGLE_INCLUDE_DIRECTIVE,
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
    definitions: HashMap<SmolStr, Definition>,
    version: Version,
}

#[derive(Debug, Clone)]
enum Definition {
    Regular(Rc<Define>, FileId),
    Line,
    File,
    Version,
}

impl Definition {
    pub fn protected(&self) -> bool {
        match self {
            Definition::Regular(d, _) => d.protected(),
            Definition::Line => true,
            Definition::File => true,
            Definition::Version => true,
        }
    }

    pub fn object_like(&self) -> bool {
        match self {
            Definition::Regular(d, _) => matches!(d.kind(), DefineKind::Object(_)),
            Definition::Line => true,
            Definition::File => true,
            Definition::Version => true,
        }
    }

    fn substitute_string(src: &str, kind: SyntaxKind) -> impl Iterator<Item = SyntaxToken> {
        let replaced = {
            // TODO: Reuse node cache
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(ROOT.into());
            builder.token(kind.into(), src);
            builder.finish_node();
            builder.finish()
        };

        SyntaxNode::new_root(replaced)
            .children_with_tokens()
            .filter_map(|node_or_token| {
                if let NodeOrToken::Token(token) = node_or_token {
                    Some(token)
                } else {
                    None
                }
            })
    }

    pub fn substitute(
        &self,
        source_token_sequence: &[&SyntaxToken],
        current_state: &ProcessorState,
        line_map: &LineMap,
    ) -> Option<Vec<OutputToken>> {
        let entire_range = TextRange::new(
            source_token_sequence.first().unwrap().text_range().start(),
            source_token_sequence.last().unwrap().text_range().end(),
        );

        match self {
            Definition::Line => Some(
                Self::substitute_string(
                    &format!(
                        "{}",
                        line_map
                            .get_line_and_col(source_token_sequence[0].text_range().start().into())
                            .0
                            + 1
                    ),
                    DIGITS,
                )
                .map(|token| OutputToken::new(token, entire_range))
                .collect(),
            ),
            Definition::Version => Some(
                Self::substitute_string(&format!("{}", current_state.version.number), DIGITS)
                    .map(|token| OutputToken::new(token, entire_range))
                    .collect(),
            ),
            _ => None,
        }
    }
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
            definitions: HashMap::from_iter([
                (
                    "GL_core_profile".into(),
                    Definition::Regular(
                        Rc::new(Define::object(
                            "GL_core_profile".into(),
                            DefineObject::from_str("1").unwrap(),
                            true,
                        )),
                        FileId::default(),
                    ),
                ),
                ("__LINE__".into(), Definition::Line),
                ("__FILE__".into(), Definition::File),
                ("__VERSION__".into(), Definition::Version),
            ]),
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

    pub fn process(&mut self, entry: &Path) -> Expand<F> {
        Expand::new(self, entry.to_owned())
    }

    fn parse(&mut self, path: &Path) -> Result<(FileId, &Ast), F::Error> {
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
            // SAFETY: n + 1 > 0
            let file_id =
                FileId::new(unsafe { NonZeroU32::new_unchecked(self.file_ids.len() as u32 + 1) });
            self.file_ids.insert(canonical_path.to_owned(), file_id);
            file_id
        };

        match self.file_cache.entry(file_id) {
            Entry::Occupied(entry) => Ok((file_id, entry.into_mut())),
            Entry::Vacant(entry) => {
                // Read the file
                let input = self.fs.read(&canonical_path)?;
                // Parse it
                let ast = Parser::new(&input).parse();
                Ok((file_id, entry.insert(ast)))
            }
        }
    }

    fn handle_node(
        &mut self,
        node: SyntaxNode,
        mask_active: &mut bool,
        mask_stack: &mut Vec<IfState>,
        current_file: FileId,
        line_map: &LineMap,
    ) -> ArrayVec<Event<F::Error>, 2> {
        let mut result = ArrayVec::new();

        match node.kind() {
            PP_EMPTY => {
                // Discard
            }
            PP_VERSION => {
                if *mask_active {
                    // TODO: Check that the version is the first thing in the file?

                    let directive: DirectiveResult<Version> = node.try_into();

                    if let Ok(version) = &directive {
                        self.current_state.version = **version;
                    }

                    result.push(Event::directive(directive));
                }
            }
            PP_EXTENSION => {
                if *mask_active {
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

                    result.push(Event::directive(directive));
                }
            }
            PP_DEFINE => {
                if *mask_active {
                    let directive: DirectiveResult<Define> = node.clone().try_into();

                    let error = if let Ok(define) = &directive {
                        if define.name().starts_with("GL_") {
                            Some(
                                ProcessingErrorKind::ProtectedDefine {
                                    ident: define.name().into(),
                                    is_undef: false,
                                }
                                .with_node(node, line_map),
                            )
                        } else {
                            let definition =
                                Definition::Regular(Rc::new((**define).clone()), current_file);

                            match self.current_state.definitions.entry(define.name().into()) {
                                Entry::Occupied(mut entry) => {
                                    if entry.get().protected() {
                                        Some(
                                            ProcessingErrorKind::ProtectedDefine {
                                                ident: define.name().into(),
                                                is_undef: false,
                                            }
                                            .with_node(node, line_map),
                                        )
                                    } else {
                                        // TODO: Check that we are not overwriting an incompatible definition
                                        *entry.get_mut() = definition;

                                        None
                                    }
                                }
                                Entry::Vacant(entry) => {
                                    entry.insert(definition);
                                    None
                                }
                            }
                        }
                    } else {
                        None
                    };

                    result.push(Event::directive(directive));

                    if let Some(error) = error {
                        result.push(Event::error(error, current_file));
                    }
                }
            }
            PP_IFDEF => {
                if *mask_active {
                    let directive: DirectiveResult<IfDef> = node.try_into();

                    if let Ok(ifdef) = &directive {
                        // Update masking state
                        *mask_active = self.current_state.definitions.contains_key(&ifdef.ident);
                        mask_stack.push(IfState::Active { else_seen: false });
                    }

                    result.push(Event::directive(directive));
                } else {
                    // Record the #ifdef in the stack to support nesting
                    mask_stack.push(IfState::None);
                }
            }
            PP_IFNDEF => {
                if *mask_active {
                    let directive: DirectiveResult<IfNDef> = node.try_into();

                    if let Ok(ifdef) = &directive {
                        // Update masking state
                        *mask_active = !self.current_state.definitions.contains_key(&ifdef.ident);
                        mask_stack.push(IfState::Active { else_seen: false });
                    }

                    result.push(Event::directive(directive));
                } else {
                    // Record the #ifdef in the stack to support nesting
                    mask_stack.push(IfState::None);
                }
            }
            PP_ELSE => {
                if let Some(top) = mask_stack.pop() {
                    match top {
                        IfState::None => {
                            *mask_active = mask_stack
                                .last()
                                .map(|top| matches!(*top, IfState::Active { .. }))
                                .unwrap_or(true);

                            mask_stack.push(IfState::Active { else_seen: true });
                        }
                        IfState::Active { else_seen } | IfState::One { else_seen } => {
                            if else_seen {
                                // Extra #else
                                result.push(Event::error(
                                    ProcessingErrorKind::ExtraElse.with_node(node, line_map),
                                    current_file,
                                ));

                                return result;
                            } else {
                                *mask_active = false;
                                mask_stack.push(IfState::One { else_seen: true });
                            }
                        }
                    }

                    result.push(Event::directive(DirectiveKind::Else));
                } else {
                    // Stray #else
                    result.push(Event::error(
                        ProcessingErrorKind::ExtraElse.with_node(node, line_map),
                        current_file,
                    ));
                }
            }
            PP_ENDIF => {
                if let Some(_) = mask_stack.pop() {
                    *mask_active = mask_stack
                        .last()
                        .map(|top| matches!(*top, IfState::Active { .. }))
                        .unwrap_or(true);

                    // TODO: Return syntax node?
                    if *mask_active {
                        result.push(Event::directive(DirectiveKind::EndIf));
                    }
                } else {
                    // Stray #endif
                    result.push(Event::error(
                        ProcessingErrorKind::ExtraEndIf.with_node(node, line_map),
                        current_file,
                    ));
                }
            }
            PP_UNDEF => {
                if *mask_active {
                    let directive: DirectiveResult<Undef> = node.clone().try_into();

                    let protected_ident = if let Ok(ifdef) = &directive {
                        if ifdef.ident.starts_with("GL_") {
                            Some(ifdef.ident.clone())
                        } else {
                            if let Some(def) = self.current_state.definitions.get(&ifdef.ident) {
                                if def.protected() {
                                    Some(ifdef.ident.clone())
                                } else {
                                    self.current_state.definitions.remove(&ifdef.ident);
                                    None
                                }
                            } else {
                                None
                            }
                        }
                    } else {
                        None
                    };

                    result.push(Event::directive(directive));

                    if let Some(ident) = protected_ident {
                        result.push(Event::error(
                            ProcessingErrorKind::ProtectedDefine {
                                ident,
                                is_undef: true,
                            }
                            .with_node(node, line_map),
                            current_file,
                        ));
                    }
                }
            }
            PP_ERROR => {
                if *mask_active {
                    let directive: DirectiveResult<Error> = node.clone().try_into();

                    let error = if let Ok(error) = &directive {
                        Some(Event::error(
                            ProcessingErrorKind::ErrorDirective {
                                message: error.message.clone(),
                            }
                            .with_node(node, line_map),
                            current_file,
                        ))
                    } else {
                        None
                    };

                    result.push(Event::directive(directive));

                    if let Some(error_event) = error {
                        result.push(error_event);
                    }
                }
            }
            _ => {
                // Handle node, this is a preprocessor directive
                result.push(Event::error(
                    ErrorKind::unhandled(NodeOrToken::Node(node), line_map),
                    current_file,
                ));
            }
        }

        result
    }
}

pub struct Expand<'p, F: FileSystem> {
    processor: &'p mut Processor<F>,
    mask_stack: Vec<IfState>,
    mask_active: bool,
    line_map: LineMap,
    state: ExpandState<F>,
}

enum ExpandState<F: FileSystem> {
    Init {
        path: PathBuf,
    },
    Iterate {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
    },
    PendingOne {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
    },
    PendingEvents {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        events: ArrayVec<Event<F::Error>, 2>,
    },
    ExpandedTokens {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        tokens: VecDeque<OutputToken>,
    },
    Complete,
}

impl<'p, F: FileSystem> Expand<'p, F> {
    pub fn new(processor: &'p mut Processor<F>, path: PathBuf) -> Self {
        Self {
            processor,
            mask_stack: Vec::with_capacity(4),
            mask_active: true,
            line_map: LineMap::default(),
            state: ExpandState::Init { path },
        }
    }

    fn handle_node_or_token(
        &mut self,
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
    ) -> Option<Event<F::Error>> {
        match node_or_token {
            rowan::NodeOrToken::Node(node) => {
                self.state = ExpandState::PendingEvents {
                    current_file,
                    iterator,
                    errors,
                    events: self.processor.handle_node(
                        node,
                        &mut self.mask_active,
                        &mut self.mask_stack,
                        current_file,
                        &self.line_map,
                    ),
                };
            }
            rowan::NodeOrToken::Token(token) => {
                if self.mask_active {
                    // Look for macro substitutions
                    if let Some(definition) = (if token.kind() == IDENT_KW {
                        Some(Unescaped::new(token.text()).to_string())
                    } else {
                        None
                    })
                    .and_then(|ident| self.processor.current_state.definitions.get(ident.as_ref()))
                    {
                        // We matched a defined identifier
                        // TODO: Handle macro substitutions

                        if definition.object_like() {
                            if let Some(result) = definition.substitute(
                                &[&token],
                                &self.processor.current_state,
                                &self.line_map,
                            ) {
                                // We handled this definition
                                self.state = ExpandState::ExpandedTokens {
                                    current_file,
                                    iterator,
                                    errors,
                                    tokens: result.into(),
                                };

                                // Return tokens in the next iterations
                                return None;
                            }
                        }

                        let mut events = ArrayVec::new();
                        events.push(Event::error(
                            ErrorKind::unhandled(NodeOrToken::Token(token.clone()), &self.line_map),
                            current_file,
                        ));
                        events.push(Event::Token(token.into()));

                        self.state = ExpandState::PendingEvents {
                            current_file,
                            iterator,
                            errors,
                            events,
                        };
                    } else {
                        // No matching definition for this identifier, just keep iterating
                        self.state = ExpandState::Iterate {
                            current_file,
                            iterator,
                            errors,
                        };

                        return Some(Event::Token(token.into()));
                    }
                } else {
                    // Just keep iterating
                    self.state = ExpandState::Iterate {
                        current_file,
                        iterator,
                        errors,
                    };
                }
            }
        }

        None
    }
}

impl<'p, F: FileSystem> Iterator for Expand<'p, F> {
    type Item = Event<F::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match std::mem::replace(&mut self.state, ExpandState::Complete) {
                ExpandState::Init { path } => match self.processor.parse(&path) {
                    Ok((file_id, ast)) => {
                        let (root, errors, line_map) = ast.clone().into_inner();

                        // Store the current line map
                        self.line_map = line_map;

                        self.state = ExpandState::Iterate {
                            current_file: file_id,
                            iterator: root.children_with_tokens(),
                            errors,
                        };

                        return Some(Event::EnterFile { file_id, path });
                    }
                    Err(err) => {
                        return Some(Event::Error(ErrorKind::Io(err).into()));
                    }
                },
                ExpandState::Iterate {
                    current_file,
                    mut iterator,
                    mut errors,
                } => {
                    if let Some(node_or_token) = iterator.next() {
                        if let Some(first) = errors.first() {
                            if node_or_token.text_range().end() >= first.pos().start() {
                                let error = errors.pop().unwrap();

                                // Parse errors in non-included blocks are ignored
                                if self.mask_active {
                                    self.state = ExpandState::PendingOne {
                                        current_file,
                                        iterator,
                                        errors,
                                        node_or_token,
                                    };

                                    return Some(Event::error(error, current_file));
                                }
                            }
                        }

                        if let Some(result) =
                            self.handle_node_or_token(current_file, iterator, errors, node_or_token)
                        {
                            return Some(result);
                        }
                    } else {
                        // Iteration completed
                        return None;
                    }
                }
                ExpandState::PendingOne {
                    current_file,
                    iterator,
                    errors,
                    node_or_token,
                } => {
                    if let Some(result) =
                        self.handle_node_or_token(current_file, iterator, errors, node_or_token)
                    {
                        return Some(result);
                    }
                }
                ExpandState::PendingEvents {
                    current_file,
                    iterator,
                    errors,
                    mut events,
                } => {
                    if let Some(event) = events.swap_pop(0) {
                        self.state = ExpandState::PendingEvents {
                            current_file,
                            iterator,
                            errors,
                            events,
                        };

                        return Some(event);
                    } else {
                        self.state = ExpandState::Iterate {
                            current_file,
                            iterator,
                            errors,
                        };
                    }
                }

                ExpandState::ExpandedTokens {
                    current_file,
                    iterator,
                    errors,
                    mut tokens,
                } => {
                    if let Some(token) = tokens.pop_front() {
                        self.state = ExpandState::ExpandedTokens {
                            current_file,
                            iterator,
                            errors,
                            tokens,
                        };

                        return Some(Event::Token(token));
                    } else {
                        self.state = ExpandState::Iterate {
                            current_file,
                            iterator,
                            errors,
                        };
                    }
                }

                ExpandState::Complete => {
                    return None;
                }
            }
        }
    }
}

impl<'p, F: FileSystem> FusedIterator for Expand<'p, F> {}

#[derive(Clone, Copy, PartialEq, Eq)]
enum IfState {
    /// No #if group of this level was included
    None,
    /// The current #if group of this level is included
    Active { else_seen: bool },
    /// One past #if group of this level was included, but not the current one
    One { else_seen: bool },
}

impl<F: FileSystem + Default> Default for Processor<F> {
    fn default() -> Self {
        Self::new_with_fs(Default::default(), F::default())
    }
}
