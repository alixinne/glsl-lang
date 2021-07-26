use std::{
    collections::{hash_map::Entry, VecDeque},
    convert::TryInto,
    iter::{FromIterator, FusedIterator},
    rc::Rc,
};

use arrayvec::ArrayVec;
use rowan::{NodeOrToken, SyntaxElementChildren};

use lang_util::FileId;

use crate::{
    lexer::LineMap,
    parser::{self, Ast, PreprocessorLang, SyntaxKind::*, SyntaxNode, SyntaxToken},
    Unescaped,
};

use super::{
    definition::{Definition, MacroInvocation},
    event::{self, DirectiveKind, ErrorKind, Event, ProcessingErrorKind},
    nodes::{
        Define, DirectiveResult, Error, Extension, IfDef, IfNDef, Include, Line, ParsedLine,
        ParsedPath, Undef, Version,
    },
    IncludeMode, ProcessorState,
};

pub struct ExpandOne {
    mask_stack: Vec<IfState>,
    mask_active: bool,
    line_map: LineMap,
    state: ExpandState,
    last_line_override: Option<(u32, ParsedLine)>,
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
    EnterNewFile {
        current_file: FileId,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        current_state: ProcessorState,
        path: ParsedPath,
        node: SyntaxNode,
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
        line_override: Option<&(u32, ParsedLine)>,
    ) -> Self {
        let mut events = ArrayVec::new();
        events.push(Event::error(e.into(), current_file, line_override));
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

enum HandleNodeResult {
    Events(ArrayVec<Event, 2>),
    EnterFile(Event, SyntaxNode, ParsedPath),
}

impl From<ArrayVec<Event, 2>> for HandleNodeResult {
    fn from(events: ArrayVec<Event, 2>) -> Self {
        Self::Events(events)
    }
}

impl ExpandOne {
    pub fn new(parsed_file: impl Into<(FileId, Ast)>, current_state: ProcessorState) -> Self {
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
            last_line_override: None,
        }
    }

    pub fn set_state(&mut self, new_state: ProcessorState) {
        match &mut self.state {
            ExpandState::Init { current_state, .. }
            | ExpandState::Iterate { current_state, .. }
            | ExpandState::EnterNewFile { current_state, .. }
            | ExpandState::PendingOne { current_state, .. }
            | ExpandState::PendingEvents { current_state, .. }
            | ExpandState::ExpandedTokens { current_state, .. } => {
                *current_state = new_state;
            }
            ExpandState::Complete => {
                panic!("cannot update the state on a completed expand");
            }
        }
    }

    pub fn line_map(&self) -> &LineMap {
        &self.line_map
    }

    pub fn line_override(&self) -> Option<&(u32, ParsedLine)> {
        self.last_line_override.as_ref()
    }

    fn handle_node(
        &mut self,
        current_state: &mut ProcessorState,
        node: SyntaxNode,
        current_file: FileId,
    ) -> HandleNodeResult {
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
                        current_state.extension(&**&extension);
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
                        result.push(Event::error(error, current_file, self.line_override()));
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
                                    self.line_override(),
                                ));

                                return result.into();
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
                        self.line_override(),
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
                        self.line_override(),
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
                            self.line_override(),
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
                            self.line_override(),
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
            PP_INCLUDE => {
                if self.mask_active {
                    if current_state.include_mode == IncludeMode::None {
                        // No include mode requested, thus we are not expecting include
                        // directives and this is a parsing error
                        result.push(Event::error(
                            parser::Error::new(
                                parser::ErrorKind::UnknownPreprocessorDirective {
                                    name: "include".into(),
                                },
                                node.text_range(),
                                &self.line_map,
                            ),
                            current_file,
                            self.line_override(),
                        ));

                        return result.into();
                    }

                    // Parse the directive itself
                    let directive: DirectiveResult<Include> = node.clone().try_into();

                    // Perform macro substitution to get the path. If this fails, the directive is
                    // malformed and shouldn't be processed.
                    let (directive, path) = match directive {
                        Ok(directive) => match directive.path(
                            &current_state,
                            &self.line_map,
                            current_file,
                            self.line_override(),
                        ) {
                            Ok(path) => (Ok(directive), Some(path)),
                            Err(err) => (Err((err, node.clone())), None),
                        },
                        err => (err, None),
                    };

                    if let Some(path) = path {
                        match current_state.include_mode {
                            IncludeMode::None => {
                                // Already handled previously
                            }
                            IncludeMode::ArbInclude => {
                                // Run-time include, also just forward the include through
                            }
                            IncludeMode::GoogleInclude => {
                                // Compile-time include, enter nested file
                                return HandleNodeResult::EnterFile(
                                    Event::directive(directive),
                                    node,
                                    path,
                                );
                            }
                        }
                    }

                    // Forward the directive
                    result.push(Event::directive(directive));
                }
            }
            PP_LINE => {
                if self.mask_active {
                    // Parse the directive itself
                    let directive: DirectiveResult<Line> = node.clone().try_into();

                    // Perform macro substitution to get the path. If this fails, the directive is
                    // malformed and shouldn't be processed.
                    let (directive, line) = match directive {
                        Ok(directive) => match directive.parse(
                            &current_state,
                            &self.line_map,
                            current_file,
                            self.line_override(),
                        ) {
                            Ok(path) => (Ok(directive), Some(path)),
                            Err(err) => (Err((err, node.clone())), None),
                        },
                        err => (err, None),
                    };

                    if let Some(line) = line {
                        let raw_line = self
                            .line_map
                            .get_line_and_col(node.text_range().start().into())
                            .0;

                        // Check that we support cpp style line
                        let line = match line {
                            ParsedLine::Line(_) | ParsedLine::LineAndFileNumber(_, _) => line,
                            ParsedLine::LineAndPath(_, _) if current_state.cpp_style_line() => line,
                            ParsedLine::LineAndPath(line, _) => {
                                result.push(Event::error(
                                    ProcessingErrorKind::CppStyleLineNotSupported
                                        .with_node(node.into(), &self.line_map),
                                    current_file,
                                    self.line_override(),
                                ));

                                ParsedLine::Line(line)
                            }
                        };

                        // Combine the previous and the new override
                        self.last_line_override = match self.last_line_override.take() {
                            Some((_, prev_override)) => match line {
                                ParsedLine::Line(line_number) => Some(match prev_override {
                                    ParsedLine::Line(_) => line,
                                    ParsedLine::LineAndFileNumber(_, file_number) => {
                                        ParsedLine::LineAndFileNumber(line_number, file_number)
                                    }
                                    ParsedLine::LineAndPath(_, path) => {
                                        ParsedLine::LineAndPath(line_number, path)
                                    }
                                }),
                                ParsedLine::LineAndFileNumber(_, _)
                                | ParsedLine::LineAndPath(_, _) => Some(line),
                            },
                            None => Some(line),
                        }
                        .map(|ov| (raw_line, ov));
                    }

                    result.push(Event::directive(directive));
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
                        self.line_override(),
                    ));
                }
            }
        }

        result.into()
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
                    match invocation.substitute(
                        &current_state,
                        &self.line_map,
                        current_file,
                        self.line_override(),
                    ) {
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
                                err.with(current_file, self.line_override()),
                                token.into(),
                                current_file,
                                iterator,
                                errors,
                                current_state,
                                self.line_override(),
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
                        self.line_override(),
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
                match self.handle_node(&mut current_state, node, current_file) {
                    HandleNodeResult::Events(events) => {
                        self.state = ExpandState::PendingEvents {
                            current_file,
                            iterator,
                            errors,
                            events,
                            current_state,
                        };

                        None
                    }

                    HandleNodeResult::EnterFile(event, node, path) => {
                        self.state = ExpandState::EnterNewFile {
                            current_file,
                            iterator,
                            errors,
                            current_state,
                            path,
                            node,
                        };

                        Some(event)
                    }
                }
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

pub enum ExpandEvent {
    Event(Event),
    EnterFile(FileId, ProcessorState, SyntaxNode, ParsedPath),
    Completed(ProcessorState),
}

impl From<Event> for ExpandEvent {
    fn from(event: Event) -> Self {
        Self::Event(event)
    }
}

impl Iterator for ExpandOne {
    type Item = ExpandEvent;

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

                                    return Some(
                                        Event::error(error, current_file, self.line_override())
                                            .into(),
                                    );
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
                        // Iteration completed, return the updated state
                        return Some(ExpandEvent::Completed(current_state));
                    }
                }

                ExpandState::EnterNewFile {
                    current_file,
                    iterator,
                    errors,
                    current_state,
                    path,
                    node,
                } => {
                    self.state = ExpandState::Iterate {
                        current_file,
                        iterator,
                        errors,
                        current_state: current_state.clone(),
                    };

                    return Some(ExpandEvent::EnterFile(
                        current_file,
                        current_state,
                        node,
                        path.into(),
                    ));
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

impl FusedIterator for ExpandOne {}
