use std::{
    array::IntoIter,
    collections::{hash_map::Entry, HashMap, VecDeque},
    convert::TryInto,
    iter::{FromIterator, FusedIterator},
    rc::Rc,
};

use arrayvec::ArrayVec;
use rowan::{NodeOrToken, SyntaxElementChildren};
use smol_str::SmolStr;

use lang_util::FileId;

use crate::{
    lexer::LineMap,
    parser::{self, Ast, PreprocessorLang, SyntaxKind::*, SyntaxNode, SyntaxToken},
    Unescaped,
};

#[macro_use]
pub mod exts;

mod definition;
use definition::{Definition, MacroInvocation};

pub mod event;
use event::{DirectiveKind, ErrorKind, Event, ProcessingErrorKind};

pub mod fs;

pub mod nodes;
use nodes::{
    Define, DefineObject, DirectiveResult, Error, Extension, ExtensionName, IfDef, IfNDef, Undef,
    Version, GL_ARB_SHADING_LANGUAGE_INCLUDE, GL_GOOGLE_INCLUDE_DIRECTIVE,
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
            definitions: HashMap::from_iter(
                IntoIter::new([
                    Definition::Regular(
                        Rc::new(Define::object(
                            "GL_core_profile".into(),
                            DefineObject::from_str("1").unwrap(),
                            true,
                        )),
                        FileId::default(),
                    ),
                    Definition::Line,
                    Definition::File,
                    Definition::Version,
                ])
                .map(|definition| (definition.name().into(), definition)),
            ),
            version: Version::default(),
        }
    }
}

pub struct Expand {
    mask_stack: Vec<IfState>,
    mask_active: bool,
    line_map: LineMap,
    state: ExpandState,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum IfState {
    /// No #if group of this level was included
    None,
    /// The current #if group of this level is included
    Active { else_seen: bool },
    /// One past #if group of this level was included, but not the current one
    One { else_seen: bool },
}

enum ExpandState {
    Init {
        current_file: FileId,
        ast: Ast,
        current_state: ProcessorState,
    },
    Iterate {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        current_state: ProcessorState,
    },
    PendingOne {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
        current_state: ProcessorState,
    },
    PendingEvents {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        events: ArrayVec<Event, 2>,
        current_state: ProcessorState,
    },
    ExpandedTokens {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        events: VecDeque<Event>,
        current_state: ProcessorState,
    },
    Complete,
}

impl ExpandState {
    pub fn error_token(
        e: impl Into<event::Error>,
        token: SyntaxToken,
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        current_state: ProcessorState,
    ) -> Self {
        let mut events = ArrayVec::new();
        events.push(Event::error(e.into(), current_file));
        events.push(Event::Token(token.clone().into()));

        Self::PendingEvents {
            current_file,
            iterator,
            errors,
            events,
            current_state,
        }
    }
}

impl Expand {
    fn new(parsed_file: impl Into<(FileId, Ast)>, current_state: ProcessorState) -> Self {
        let (file_id, ast) = parsed_file.into();

        Self {
            mask_stack: Vec::with_capacity(4),
            mask_active: true,
            line_map: LineMap::default(),
            state: ExpandState::Init {
                current_file: file_id,
                ast,
                current_state,
            },
        }
    }

    fn handle_node(
        &mut self,
        current_state: &mut ProcessorState,
        node: SyntaxNode,
        current_file: FileId,
    ) -> ArrayVec<Event, 2> {
        let mut result = ArrayVec::new();

        match node.kind() {
            PP_EMPTY => {
                // Discard
            }
            PP_VERSION => {
                if self.mask_active {
                    // TODO: Check that the version is the first thing in the file?

                    let directive: DirectiveResult<Version> = node.try_into();

                    if let Ok(version) = &directive {
                        current_state.version = **version;
                    }

                    result.push(Event::directive(directive));
                }
            }
            PP_EXTENSION => {
                if self.mask_active {
                    let directive: DirectiveResult<Extension> = node.try_into();

                    if let Ok(extension) = &directive {
                        // Push onto the stack
                        current_state.extension_stack.push((**extension).clone());

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
                                current_state.include_mode = target;
                            } else {
                                // TODO: Implement current mode as a stack?
                                current_state.include_mode = IncludeMode::None;
                            }
                        }
                    }

                    result.push(Event::directive(directive));
                }
            }
            PP_DEFINE => {
                if self.mask_active {
                    let directive: DirectiveResult<Define> = node.clone().try_into();

                    let error = if let Ok(define) = &directive {
                        if define.name().starts_with("GL_") {
                            Some(
                                ProcessingErrorKind::ProtectedDefine {
                                    ident: define.name().into(),
                                    is_undef: false,
                                }
                                .with_node(node.into(), &self.line_map),
                            )
                        } else {
                            let definition =
                                Definition::Regular(Rc::new((**define).clone()), current_file);

                            match current_state.definitions.entry(define.name().into()) {
                                Entry::Occupied(mut entry) => {
                                    if entry.get().protected() {
                                        Some(
                                            ProcessingErrorKind::ProtectedDefine {
                                                ident: define.name().into(),
                                                is_undef: false,
                                            }
                                            .with_node(node.into(), &self.line_map),
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
                if self.mask_active {
                    let directive: DirectiveResult<IfDef> = node.try_into();

                    if let Ok(ifdef) = &directive {
                        // Update masking state
                        self.mask_active = current_state.definitions.contains_key(&ifdef.ident);
                        self.mask_stack.push(IfState::Active { else_seen: false });
                    }

                    result.push(Event::directive(directive));
                } else {
                    // Record the #ifdef in the stack to support nesting
                    self.mask_stack.push(IfState::None);
                }
            }
            PP_IFNDEF => {
                if self.mask_active {
                    let directive: DirectiveResult<IfNDef> = node.try_into();

                    if let Ok(ifdef) = &directive {
                        // Update masking state
                        self.mask_active = !current_state.definitions.contains_key(&ifdef.ident);
                        self.mask_stack.push(IfState::Active { else_seen: false });
                    }

                    result.push(Event::directive(directive));
                } else {
                    // Record the #ifdef in the stack to support nesting
                    self.mask_stack.push(IfState::None);
                }
            }
            PP_ELSE => {
                if let Some(top) = self.mask_stack.pop() {
                    match top {
                        IfState::None => {
                            self.mask_active = self
                                .mask_stack
                                .last()
                                .map(|top| matches!(*top, IfState::Active { .. }))
                                .unwrap_or(true);

                            self.mask_stack.push(IfState::Active { else_seen: true });
                        }
                        IfState::Active { else_seen } | IfState::One { else_seen } => {
                            if else_seen {
                                // Extra #else
                                result.push(Event::error(
                                    ProcessingErrorKind::ExtraElse
                                        .with_node(node.into(), &self.line_map),
                                    current_file,
                                ));

                                return result;
                            } else {
                                self.mask_active = false;
                                self.mask_stack.push(IfState::One { else_seen: true });
                            }
                        }
                    }

                    result.push(Event::directive(DirectiveKind::Else));
                } else {
                    // Stray #else
                    result.push(Event::error(
                        ProcessingErrorKind::ExtraElse.with_node(node.into(), &self.line_map),
                        current_file,
                    ));
                }
            }
            PP_ENDIF => {
                if let Some(_) = self.mask_stack.pop() {
                    self.mask_active = self
                        .mask_stack
                        .last()
                        .map(|top| matches!(*top, IfState::Active { .. }))
                        .unwrap_or(true);

                    // TODO: Return syntax node?
                    if self.mask_active {
                        result.push(Event::directive(DirectiveKind::EndIf));
                    }
                } else {
                    // Stray #endif
                    result.push(Event::error(
                        ProcessingErrorKind::ExtraEndIf.with_node(node.into(), &self.line_map),
                        current_file,
                    ));
                }
            }
            PP_UNDEF => {
                if self.mask_active {
                    let directive: DirectiveResult<Undef> = node.clone().try_into();

                    let protected_ident = if let Ok(ifdef) = &directive {
                        if ifdef.ident.starts_with("GL_") {
                            Some(ifdef.ident.clone())
                        } else {
                            if let Some(def) = current_state.definitions.get(&ifdef.ident) {
                                if def.protected() {
                                    Some(ifdef.ident.clone())
                                } else {
                                    current_state.definitions.remove(&ifdef.ident);
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
                            .with_node(node.into(), &self.line_map),
                            current_file,
                        ));
                    }
                }
            }
            PP_ERROR => {
                if self.mask_active {
                    let directive: DirectiveResult<Error> = node.clone().try_into();

                    let error = if let Ok(error) = &directive {
                        Some(Event::error(
                            ProcessingErrorKind::ErrorDirective {
                                message: error.message.clone(),
                            }
                            .with_node(node.into(), &self.line_map),
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
                // Special case for PP_IF so #endif stack correctly
                if node.kind() == PP_IF {
                    self.mask_active = false;
                    self.mask_stack.push(IfState::None);
                }

                if self.mask_active {
                    result.push(Event::error(
                        ErrorKind::unhandled(NodeOrToken::Node(node), &self.line_map),
                        current_file,
                    ));
                }
            }
        }

        result
    }

    fn handle_token(
        &mut self,
        current_state: ProcessorState,
        token: SyntaxToken,
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
    ) -> Option<Event> {
        // Look for macro substitutions
        if let Some(definition) = (if token.kind() == IDENT_KW {
            Some(Unescaped::new(token.text()).to_string())
        } else {
            None
        })
        .and_then(|ident| current_state.definitions.get(ident.as_ref()))
        {
            // We matched a defined identifier

            match MacroInvocation::parse(
                definition,
                token.clone(),
                iterator.clone(),
                &self.line_map,
            ) {
                Ok(Some((invocation, new_iterator))) => {
                    // We successfully parsed a macro invocation
                    match invocation.substitute(&current_state, &self.line_map) {
                        Ok(result) => {
                            // We handled this definition
                            self.state = ExpandState::ExpandedTokens {
                                current_file,
                                iterator: new_iterator,
                                errors,
                                events: VecDeque::from_iter(result),
                                current_state,
                            };
                        }
                        Err(err) => {
                            // Definition not handled yet
                            // TODO: Remove this once substitute never fails
                            self.state = ExpandState::error_token(
                                err,
                                token.into(),
                                current_file,
                                iterator,
                                errors,
                                current_state,
                            );
                        }
                    }
                }
                Ok(None) => {
                    // Could not parse a macro invocation starting at the current token, so just
                    // resume iterating normally
                    self.state = ExpandState::Iterate {
                        current_file,
                        iterator,
                        errors,
                        current_state,
                    };

                    return Some(Event::Token(token.into()));
                }
                Err(err) => {
                    self.state = ExpandState::error_token(
                        err,
                        token,
                        current_file,
                        iterator,
                        errors,
                        current_state,
                    );
                }
            }

            None
        } else {
            // No matching definition for this identifier, just keep iterating
            self.state = ExpandState::Iterate {
                current_file,
                iterator,
                errors,
                current_state,
            };

            Some(Event::Token(token.into()))
        }
    }

    fn handle_node_or_token(
        &mut self,
        mut current_state: ProcessorState,
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
    ) -> Option<Event> {
        match node_or_token {
            rowan::NodeOrToken::Node(node) => {
                self.state = ExpandState::PendingEvents {
                    current_file,
                    iterator,
                    errors,
                    events: self.handle_node(&mut current_state, node, current_file),
                    current_state,
                };

                None
            }
            rowan::NodeOrToken::Token(token) => {
                if self.mask_active {
                    self.handle_token(current_state, token, current_file, iterator, errors)
                } else {
                    // Just keep iterating
                    self.state = ExpandState::Iterate {
                        current_file,
                        iterator,
                        errors,
                        current_state,
                    };

                    None
                }
            }
        }
    }
}

impl Iterator for Expand {
    type Item = Event;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match std::mem::replace(&mut self.state, ExpandState::Complete) {
                ExpandState::Init {
                    current_file,
                    ast,
                    current_state,
                } => {
                    let (root, errors, line_map) = ast.into_inner();

                    // Store the current line map
                    self.line_map = line_map;

                    self.state = ExpandState::Iterate {
                        current_file,
                        iterator: root.children_with_tokens(),
                        errors,
                        current_state,
                    };

                    return Some(Event::EnterFile(current_file).into());
                }
                ExpandState::Iterate {
                    current_file,
                    mut iterator,
                    mut errors,
                    current_state,
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
                                        current_state,
                                    };

                                    return Some(Event::error(error, current_file).into());
                                }
                            }
                        }

                        if let Some(result) = self.handle_node_or_token(
                            current_state,
                            current_file,
                            iterator,
                            errors,
                            node_or_token,
                        ) {
                            return Some(result.into());
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
                    current_state,
                } => {
                    if let Some(result) = self.handle_node_or_token(
                        current_state,
                        current_file,
                        iterator,
                        errors,
                        node_or_token,
                    ) {
                        return Some(result.into());
                    }
                }
                ExpandState::PendingEvents {
                    current_file,
                    iterator,
                    errors,
                    mut events,
                    current_state,
                } => {
                    if let Some(event) = events.swap_pop(0) {
                        self.state = ExpandState::PendingEvents {
                            current_file,
                            iterator,
                            errors,
                            events,
                            current_state,
                        };

                        return Some(event.into());
                    } else {
                        self.state = ExpandState::Iterate {
                            current_file,
                            iterator,
                            errors,
                            current_state,
                        };
                    }
                }

                ExpandState::ExpandedTokens {
                    current_file,
                    iterator,
                    errors,
                    mut events,
                    current_state,
                } => {
                    if let Some(event) = events.pop_front() {
                        self.state = ExpandState::ExpandedTokens {
                            current_file,
                            iterator,
                            errors,
                            events,
                            current_state,
                        };

                        return Some(event.into());
                    } else {
                        self.state = ExpandState::Iterate {
                            current_file,
                            iterator,
                            errors,
                            current_state,
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

impl FusedIterator for Expand {}
