use std::{
    collections::{hash_map::Entry, VecDeque},
    convert::TryInto,
    iter::{FromIterator, FusedIterator},
    rc::Rc,
};

use arrayvec::ArrayVec;
use rowan::{NodeOrToken, SyntaxElementChildren, TextSize};

use lang_util::FileId;

use crate::{
    lexer::LineMap,
    parser::{self, Ast, PreprocessorLang, SyntaxKind::*, SyntaxNode, SyntaxToken},
    Unescaped,
};

use super::{
    definition::{Definition, MacroInvocation},
    event::{self, ErrorKind, Event, ProcessingErrorKind},
    nodes::{
        Define, DirectiveResult, Else, EndIf, Error, Extension, IfDef, IfNDef, Include, Line,
        ParsedLine, ParsedPath, Undef, Version,
    },
    IncludeMode, ProcessorState,
};

pub struct ExpandLocation {
    current_file: FileId,
    line_map: LineMap,
    line_override: Option<(u32, ParsedLine)>,
}

impl ExpandLocation {
    pub fn new(current_file: FileId) -> Self {
        Self {
            current_file,
            line_map: Default::default(),
            line_override: Default::default(),
        }
    }

    pub fn line_map(&self) -> &LineMap {
        &self.line_map
    }

    pub fn current_file(&self) -> FileId {
        self.current_file
    }

    pub fn line_override(&self) -> Option<&(u32, ParsedLine)> {
        self.line_override.as_ref()
    }

    pub fn offset_to_line_number(&self, offset: TextSize) -> u32 {
        let raw_line = self.line_map().get_line_and_col(offset.into()).0;
        self.line_to_line_number(raw_line)
    }

    pub fn line_to_line_number(&self, raw_line: u32) -> u32 {
        if let Some((origin, line_override)) = &self.line_override {
            let offset = line_override.line_number() as i64 - *origin as i64 - 2;
            (raw_line as i64 + offset) as u32
        } else {
            raw_line
        }
    }

    pub fn add_override(&mut self, current_offset: TextSize, line: ParsedLine) {
        let raw_line = self.line_map.get_line_and_col(current_offset.into()).0;

        // Combine the previous and the new override
        self.line_override = match self.line_override.take() {
            Some((_, prev_override)) => match line {
                ParsedLine::Line(line_number) => Some(match prev_override {
                    ParsedLine::Line(_) => line,
                    ParsedLine::LineAndFileNumber(_, file_number) => {
                        ParsedLine::LineAndFileNumber(line_number, file_number)
                    }
                    ParsedLine::LineAndPath(_, path) => ParsedLine::LineAndPath(line_number, path),
                }),
                ParsedLine::LineAndFileNumber(_, _) | ParsedLine::LineAndPath(_, _) => Some(line),
            },
            None => Some(line),
        }
        .map(|ov| (raw_line, ov));
    }
}

pub struct ExpandOne {
    mask_stack: Vec<IfState>,
    mask_active: bool,
    location: ExpandLocation,
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
        ast: Ast,
        current_state: ProcessorState,
    },
    Iterate {
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        current_state: ProcessorState,
    },
    EnterNewFile {
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        current_state: ProcessorState,
        path: ParsedPath,
        node: SyntaxNode,
    },
    PendingOne {
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
        current_state: ProcessorState,
    },
    PendingEvents {
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        events: ArrayVec<Event, 2>,
        current_state: ProcessorState,
    },
    ExpandedTokens {
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
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        current_state: ProcessorState,
    ) -> Self {
        let mut events = ArrayVec::new();
        events.push(Event::Error(e.into()));
        events.push(Event::Token(token.clone().into()));

        Self::PendingEvents {
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
            location: ExpandLocation::new(file_id),
            state: ExpandState::Init { ast, current_state },
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

    pub fn location(&self) -> &ExpandLocation {
        &self.location
    }

    fn handle_node(
        &mut self,
        current_state: &mut ProcessorState,
        node: SyntaxNode,
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

                    match directive {
                        Ok(directive) => {
                            current_state.version = *directive;
                            result.push(Event::directive(directive));
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
                    }
                }
            }
            PP_EXTENSION => {
                if self.mask_active {
                    let directive: DirectiveResult<Extension> = node.try_into();

                    match directive {
                        Ok(directive) => {
                            current_state.extension(&*directive);
                            result.push(Event::directive(directive));
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
                    }
                }
            }
            PP_DEFINE => {
                if self.mask_active {
                    let directive: DirectiveResult<Define> = node.try_into();

                    match directive {
                        Ok(define) => {
                            let error = if define.name().starts_with("GL_") {
                                Some(
                                    ProcessingErrorKind::ProtectedDefine {
                                        ident: define.name().into(),
                                        is_undef: false,
                                    }
                                    .with_node(define.node().clone().into(), &self.location),
                                )
                            } else {
                                let definition = Definition::Regular(
                                    Rc::new((*define).clone()),
                                    self.location.current_file(),
                                );

                                match current_state.definitions.entry(define.name().into()) {
                                    Entry::Occupied(mut entry) => {
                                        if entry.get().protected() {
                                            Some(
                                                ProcessingErrorKind::ProtectedDefine {
                                                    ident: define.name().into(),
                                                    is_undef: false,
                                                }
                                                .with_node(
                                                    define.node().clone().into(),
                                                    &self.location,
                                                ),
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
                            };

                            result.push(Event::directive(define));

                            if let Some(error) = error {
                                result.push(Event::error(error, &self.location));
                            }
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
                    }
                }
            }
            PP_IFDEF => {
                if self.mask_active {
                    let directive: DirectiveResult<IfDef> = node.try_into();

                    match directive {
                        Ok(ifdef) => {
                            // Update masking state
                            self.mask_active = current_state.definitions.contains_key(&ifdef.ident);
                            self.mask_stack.push(IfState::Active { else_seen: false });

                            result.push(Event::directive(ifdef));
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
                    }
                } else {
                    // Record the #ifdef in the stack to support nesting
                    self.mask_stack.push(IfState::None);
                }
            }
            PP_IFNDEF => {
                if self.mask_active {
                    let directive: DirectiveResult<IfNDef> = node.try_into();

                    match directive {
                        Ok(ifndef) => {
                            // Update masking state
                            self.mask_active =
                                !current_state.definitions.contains_key(&ifndef.ident);
                            self.mask_stack.push(IfState::Active { else_seen: false });

                            result.push(Event::directive(ifndef));
                        }
                        Err((error, node)) => {
                            result.push(Event::directive_error(
                                (ProcessingErrorKind::DirectiveIfNDef(error), node),
                                &self.location,
                            ));
                        }
                    }
                } else {
                    // Record the #ifdef in the stack to support nesting
                    self.mask_stack.push(IfState::None);
                }
            }
            PP_ELSE => {
                let directive: DirectiveResult<Else> = node.clone().try_into();

                match directive {
                    Ok(else_) => {
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
                                                .with_node(node.into(), &self.location),
                                            &self.location,
                                        ));

                                        return result.into();
                                    } else {
                                        self.mask_active = false;
                                        self.mask_stack.push(IfState::One { else_seen: true });
                                    }
                                }
                            }

                            result.push(Event::directive(else_));
                        } else {
                            // Stray #else
                            result.push(Event::error(
                                ProcessingErrorKind::ExtraElse
                                    .with_node(node.into(), &self.location),
                                &self.location,
                            ));
                        }
                    }
                    Err(error) => {
                        result.push(Event::directive_error(error, &self.location));
                    }
                }
            }
            PP_ENDIF => {
                let directive: DirectiveResult<EndIf> = node.clone().try_into();

                match directive {
                    Ok(endif) => {
                        if let Some(_) = self.mask_stack.pop() {
                            self.mask_active = self
                                .mask_stack
                                .last()
                                .map(|top| matches!(*top, IfState::Active { .. }))
                                .unwrap_or(true);

                            // TODO: Return syntax node?
                            if self.mask_active {
                                result.push(Event::directive(endif));
                            }
                        } else {
                            // Stray #endif
                            result.push(Event::error(
                                ProcessingErrorKind::ExtraEndIf
                                    .with_node(node.into(), &self.location),
                                &self.location,
                            ));
                        }
                    }
                    Err(error) => {
                        result.push(Event::directive_error(error, &self.location));
                    }
                }
            }
            PP_UNDEF => {
                if self.mask_active {
                    let directive: DirectiveResult<Undef> = node.clone().try_into();

                    match directive {
                        Ok(undef) => {
                            let protected_ident = {
                                if undef.ident.starts_with("GL_") {
                                    Some(undef.ident.clone())
                                } else {
                                    if let Some(def) = current_state.definitions.get(&undef.ident) {
                                        if def.protected() {
                                            Some(undef.ident.clone())
                                        } else {
                                            current_state.definitions.remove(&undef.ident);
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }
                            };

                            result.push(Event::directive(undef));

                            if let Some(ident) = protected_ident {
                                result.push(Event::error(
                                    ProcessingErrorKind::ProtectedDefine {
                                        ident,
                                        is_undef: true,
                                    }
                                    .with_node(node.into(), &self.location),
                                    &self.location,
                                ));
                            }
                        }
                        Err((error, node)) => {
                            result.push(Event::directive_error(
                                (ProcessingErrorKind::DirectiveUndef(error), node),
                                &self.location,
                            ));
                        }
                    }
                }
            }
            PP_ERROR => {
                if self.mask_active {
                    let directive: DirectiveResult<Error> = node.try_into();

                    match directive {
                        Ok(error) => {
                            let user_error = {
                                Some(Event::error(
                                    ProcessingErrorKind::ErrorDirective {
                                        message: error.message.clone(),
                                    }
                                    .with_node(error.node().clone().into(), &self.location),
                                    &self.location,
                                ))
                            };

                            result.push(Event::directive(error));

                            if let Some(error_event) = user_error {
                                result.push(error_event);
                            }
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
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
                                self.location.line_map(),
                            ),
                            &self.location,
                        ));

                        return result.into();
                    }

                    // Parse the directive itself
                    let directive: DirectiveResult<Include> = node.try_into();

                    // Perform macro substitution to get the path. If this fails, the directive is
                    // malformed and shouldn't be processed.
                    let (directive, path) = match directive {
                        Ok(directive) => match directive.path(&current_state, &self.location) {
                            Ok(path) => (Ok(directive), Some(path)),
                            Err(err) => (Err((err, directive.node().clone())), None),
                        },
                        err => (err, None),
                    };

                    match directive {
                        Ok(include) => {
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
                                        let node = include.node().clone();
                                        return HandleNodeResult::EnterFile(
                                            Event::directive(include),
                                            node,
                                            path,
                                        );
                                    }
                                }
                            }

                            // Forward the directive
                            result.push(Event::directive(include));
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
                    }
                }
            }
            PP_LINE => {
                if self.mask_active {
                    // Parse the directive itself
                    let directive: DirectiveResult<Line> = node.try_into();

                    // Perform macro substitution to get the path. If this fails, the directive is
                    // malformed and shouldn't be processed.
                    let (directive, line) = match directive {
                        Ok(directive) => match directive.parse(&current_state, &self.location) {
                            Ok(path) => (Ok(directive), Some(path)),
                            Err(err) => (Err((err, directive.node().clone())), None),
                        },
                        err => (err, None),
                    };

                    match directive {
                        Ok(ld) => {
                            if let Some(line) = line {
                                // Check that we support cpp style line
                                let line = match line {
                                    ParsedLine::Line(_) | ParsedLine::LineAndFileNumber(_, _) => {
                                        line
                                    }
                                    ParsedLine::LineAndPath(_, _)
                                        if current_state.cpp_style_line() =>
                                    {
                                        line
                                    }
                                    ParsedLine::LineAndPath(line, _) => {
                                        result.push(Event::error(
                                            ProcessingErrorKind::CppStyleLineNotSupported
                                                .with_node(
                                                    ld.node().clone().into(),
                                                    &self.location,
                                                ),
                                            &self.location,
                                        ));

                                        ParsedLine::Line(line)
                                    }
                                };

                                self.location
                                    .add_override(ld.node().text_range().start(), line);
                            }

                            result.push(Event::directive(ld));
                        }
                        Err(error) => {
                            result.push(Event::directive_error(error, &self.location));
                        }
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
                        ErrorKind::unhandled(NodeOrToken::Node(node), self.location.line_map()),
                        &self.location,
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
                &self.location,
            ) {
                Ok(Some((invocation, new_iterator))) => {
                    // We successfully parsed a macro invocation
                    match invocation.substitute(&current_state, &self.location) {
                        Ok(result) => {
                            // We handled this definition
                            self.state = ExpandState::ExpandedTokens {
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
                        iterator,
                        errors,
                        current_state,
                    };

                    return Some(Event::Token(token.into()));
                }
                Err(err) => {
                    self.state = ExpandState::error_token(
                        event::Error::new(err, &self.location),
                        token,
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
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
        node_or_token: NodeOrToken<SyntaxNode, SyntaxToken>,
    ) -> Option<Event> {
        match node_or_token {
            rowan::NodeOrToken::Node(node) => match self.handle_node(&mut current_state, node) {
                HandleNodeResult::Events(events) => {
                    self.state = ExpandState::PendingEvents {
                        iterator,
                        errors,
                        events,
                        current_state,
                    };

                    None
                }

                HandleNodeResult::EnterFile(event, node, path) => {
                    self.state = ExpandState::EnterNewFile {
                        iterator,
                        errors,
                        current_state,
                        path,
                        node,
                    };

                    Some(event)
                }
            },
            rowan::NodeOrToken::Token(token) => {
                if self.mask_active {
                    self.handle_token(current_state, token, iterator, errors)
                } else {
                    // Just keep iterating
                    self.state = ExpandState::Iterate {
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
    EnterFile(ProcessorState, SyntaxNode, ParsedPath),
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
                ExpandState::Init { ast, current_state } => {
                    let (root, errors, line_map) = ast.into_inner();

                    // Store the current line map
                    self.location.line_map = line_map;

                    self.state = ExpandState::Iterate {
                        iterator: root.children_with_tokens(),
                        errors,
                        current_state,
                    };

                    return Some(Event::EnterFile(self.location.current_file).into());
                }
                ExpandState::Iterate {
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
                                        iterator,
                                        errors,
                                        node_or_token,
                                        current_state,
                                    };

                                    return Some(Event::error(error, &self.location).into());
                                }
                            }
                        }

                        if let Some(result) = self.handle_node_or_token(
                            current_state,
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
                    iterator,
                    errors,
                    current_state,
                    path,
                    node,
                } => {
                    self.state = ExpandState::Iterate {
                        iterator,
                        errors,
                        current_state: current_state.clone(),
                    };

                    return Some(ExpandEvent::EnterFile(current_state, node, path.into()));
                }

                ExpandState::PendingOne {
                    iterator,
                    errors,
                    node_or_token,
                    current_state,
                } => {
                    if let Some(result) =
                        self.handle_node_or_token(current_state, iterator, errors, node_or_token)
                    {
                        return Some(result.into());
                    }
                }
                ExpandState::PendingEvents {
                    iterator,
                    errors,
                    mut events,
                    current_state,
                } => {
                    if let Some(event) = events.swap_pop(0) {
                        self.state = ExpandState::PendingEvents {
                            iterator,
                            errors,
                            events,
                            current_state,
                        };

                        return Some(event.into());
                    } else {
                        self.state = ExpandState::Iterate {
                            iterator,
                            errors,
                            current_state,
                        };
                    }
                }

                ExpandState::ExpandedTokens {
                    iterator,
                    errors,
                    mut events,
                    current_state,
                } => {
                    if let Some(event) = events.pop_front() {
                        self.state = ExpandState::ExpandedTokens {
                            iterator,
                            errors,
                            events,
                            current_state,
                        };

                        return Some(event.into());
                    } else {
                        self.state = ExpandState::Iterate {
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
