use std::{
    collections::{hash_map::Entry, VecDeque},
    convert::TryInto,
    iter::FusedIterator,
    rc::Rc,
};

use arrayvec::ArrayVec;
use derive_more::From;
use rowan::{NodeOrToken, SyntaxElementChildren, TextSize};

use lang_util::FileId;

use crate::{
    lexer::LineMap,
    parser::{self, Ast, PreprocessorLang, SyntaxKind::*, SyntaxNode, SyntaxToken},
    Unescaped,
};

use super::{
    definition::{Definition, MacroInvocation},
    event::{ErrorKind, Event, ProcessingErrorKind},
    nodes::{
        Define, Directive, DirectiveResult, Elif, Else, Empty, EndIf, Error, Extension, If, IfDef,
        IfNDef, Include, Invalid, Line, ParsedLine, ParsedPath, Pragma, Undef, Version,
    },
    IncludeMode, ProcessorState,
};

mod if_stack;
use if_stack::IfStack;

pub struct ExpandLocation {
    current_file: FileId,
    line_map: LineMap,
    line_override: Option<(u32, ParsedLine)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocationString<'p> {
    Number(u32),
    String(&'p str),
}

impl LocationString<'_> {
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }
}

impl std::fmt::Display for LocationString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocationString::Number(num) => write!(f, "{}", num),
            LocationString::String(path) => write!(f, "{}", path),
        }
    }
}

impl ExpandLocation {
    pub fn new(current_file: FileId) -> Self {
        Self {
            current_file,
            line_map: Default::default(),
            line_override: Default::default(),
        }
    }

    pub fn current_file(&self) -> FileId {
        self.current_file
    }

    pub fn line_override(&self) -> Option<&(u32, ParsedLine)> {
        self.line_override.as_ref()
    }

    pub fn offset_to_raw_line_and_col(&self, offset: TextSize) -> (u32, u32) {
        self.line_map.get_line_and_col(offset.into())
    }

    pub fn offset_to_line_and_col(&self, offset: TextSize) -> (u32, u32) {
        let line_and_col = self.offset_to_raw_line_and_col(offset);
        (self.line_to_line_number(line_and_col.0), line_and_col.1)
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

    pub fn string(&self) -> LocationString {
        if let Some(line_override) = &self.line_override {
            match &line_override.1 {
                ParsedLine::LineAndFileNumber(_, number) => {
                    return LocationString::Number(*number);
                }
                ParsedLine::LineAndPath(_, path) => {
                    return LocationString::String(path.as_str());
                }
                _ => {}
            }
        }

        LocationString::Number(self.current_file.number())
    }
}

pub(crate) struct ExpandOne {
    if_stack: IfStack,
    location: ExpandLocation,
    state: ExpandState,
}

#[allow(clippy::large_enum_variant)]
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
        events: ArrayVec<Event, 3>,
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

#[derive(From)]
enum HandleNodeResult {
    Event(Event),
    EnterFile(Event, SyntaxNode, ParsedPath),
}

impl ExpandOne {
    pub fn new(parsed_file: impl Into<(FileId, Ast)>, current_state: ProcessorState) -> Self {
        let (file_id, ast) = parsed_file.into();

        Self {
            if_stack: IfStack::new(),
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
        match node.kind() {
            PP_EMPTY => Event::directive(Directive::new(node, Empty), !self.if_stack.active()),
            PP_VERSION => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<Version> = node.try_into();

                match directive {
                    Ok(directive) => {
                        // TODO: Check that the version is the first thing in the file?
                        if active {
                            current_state.version = *directive;
                        }

                        Event::directive(directive, !active)
                    }
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_EXTENSION => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<Extension> = node.try_into();

                match directive {
                    Ok(directive) => {
                        if active {
                            current_state.extension(&*directive);
                        }

                        Event::directive(directive, !active)
                    }
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_DEFINE => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<Define> = node.try_into();

                match directive {
                    Ok(define) => {
                        let error = if active {
                            if define.name().starts_with("GL_") {
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
                            }
                        } else {
                            None
                        };

                        Event::directive_errors(define, !active, error, &self.location)
                    }
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_IFDEF => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<IfDef> = node.try_into();

                let (result, ret) = match directive {
                    Ok(ifdef) => {
                        let is_defined = current_state.definitions.contains_key(&ifdef.ident);
                        (is_defined, Event::directive(ifdef, !active))
                    }
                    Err(error) => (true, Event::directive_error(error, &self.location, !active)),
                };

                self.if_stack.on_if_like(result);
                ret
            }
            PP_IFNDEF => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<IfNDef> = node.try_into();

                let (result, ret) = match directive {
                    Ok(ifndef) => {
                        // Update masking state
                        let is_defined = current_state.definitions.contains_key(&ifndef.ident);
                        (!is_defined, Event::directive(ifndef, !active))
                    }
                    Err((error, node)) => (
                        true,
                        Event::directive_error(
                            (ProcessingErrorKind::DirectiveIfNDef(error), node),
                            &self.location,
                            !active,
                        ),
                    ),
                };

                self.if_stack.on_if_like(result);
                ret
            }
            PP_IF => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<If> = node.clone().try_into();

                let (result, ret) = match directive {
                    Ok(if_) => {
                        let (value, error) = if active {
                            if_.eval(&current_state, &self.location)
                        } else {
                            (true, None)
                        };

                        (
                            value,
                            Event::directive_errors(
                                if_,
                                !active,
                                error.map(ProcessingErrorKind::DirectiveIf).map(|kind| {
                                    kind.with_node(node.clone().into(), &self.location)
                                }),
                                &self.location,
                            ),
                        )
                    }
                    Err(error) => (true, Event::directive_error(error, &self.location, !active)),
                };

                self.if_stack.on_if_like(result);
                ret
            }
            PP_ELIF => {
                let active = self.if_stack.if_group_active();
                let directive: DirectiveResult<Elif> = node.clone().try_into();
                let mut errors: ArrayVec<_, 2> = ArrayVec::new();

                let expr = match &directive {
                    Ok(elif_) => {
                        if self.if_stack.if_group_active() {
                            let (value, error) = if active {
                                elif_.eval(&current_state, &self.location)
                            } else {
                                (true, None)
                            };

                            if let Some(error) = error {
                                errors.push(
                                    ProcessingErrorKind::DirectiveElif(error)
                                        .with_node(node.clone().into(), &self.location),
                                );
                            }

                            value
                        } else {
                            // Do not evaluate if the group is not active
                            true
                        }
                    }
                    Err(_) => true,
                };

                // Update the if stack, which may fail
                if let Err(error) = self.if_stack.on_elif(expr) {
                    errors.push(
                        ProcessingErrorKind::from(error).with_node(node.into(), &self.location),
                    );
                }

                match directive {
                    Ok(elif_) => Event::directive_errors(elif_, !active, errors, &self.location),
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_ELSE => {
                let active = self.if_stack.if_group_active();
                let directive: DirectiveResult<Else> = node.clone().try_into();

                // Update the if stack, which may fail
                let error = self.if_stack.on_else().err().map(|kind| {
                    ProcessingErrorKind::from(kind).with_node(node.into(), &self.location)
                });

                match directive {
                    Ok(else_) => Event::directive_errors(else_, !active, error, &self.location),
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_ENDIF => {
                let active = self.if_stack.if_group_active();
                let directive: DirectiveResult<EndIf> = node.clone().try_into();

                // Update the if stack, which may fail
                let error = self.if_stack.on_endif().err().map(|kind| {
                    ProcessingErrorKind::from(kind).with_node(node.into(), &self.location)
                });

                match directive {
                    Ok(endif) => Event::directive_errors(endif, !active, error, &self.location),
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_UNDEF => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<Undef> = node.clone().try_into();

                match directive {
                    Ok(undef) => {
                        let protected_ident = if active {
                            if undef.ident.starts_with("GL_") {
                                Some(undef.ident.clone())
                            } else if let Some(def) = current_state.definitions.get(&undef.ident) {
                                if def.protected() {
                                    Some(undef.ident.clone())
                                } else {
                                    current_state.definitions.remove(&undef.ident);
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        Event::directive_errors(
                            undef,
                            !active,
                            protected_ident.map(|ident| {
                                ProcessingErrorKind::ProtectedDefine {
                                    ident,
                                    is_undef: true,
                                }
                                .with_node(node.into(), &self.location)
                            }),
                            &self.location,
                        )
                    }
                    Err((error, node)) => Event::directive_error(
                        (ProcessingErrorKind::DirectiveUndef(error), node),
                        &self.location,
                        !active,
                    ),
                }
            }
            PP_ERROR => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<Error> = node.try_into();

                match directive {
                    Ok(error) => {
                        let user_error = ProcessingErrorKind::ErrorDirective {
                            message: error.message.clone(),
                        }
                        .with_node(error.node().clone().into(), &self.location);

                        Event::directive_errors(
                            error,
                            !active,
                            std::iter::once(user_error),
                            &self.location,
                        )
                    }
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_INCLUDE => {
                let active = self.if_stack.active();
                // Parse the directive itself
                let directive: DirectiveResult<Include> = node.clone().try_into();

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
                        let error = match (path, current_state.include_mode) {
                            (_, IncludeMode::None) if active => {
                                // No include mode requested, thus we are not expecting include
                                // directives and this is a parsing error
                                Some(
                                    ProcessingErrorKind::IncludeNotSupported
                                        .with_node(node.into(), &self.location)
                                        .into(),
                                )
                            }
                            (Some(path), IncludeMode::GoogleInclude { warn }) if active => {
                                // Compile-time include, enter nested file
                                let node = include.node().clone();
                                return HandleNodeResult::EnterFile(
                                    Event::directive_errors(
                                        include,
                                        !active,
                                        if warn {
                                            Some(ErrorKind::warn_ext_use(
                                                ext_name!("GL_GOOGLE_include_directive"),
                                                None,
                                                node.text_range(),
                                                &self.location,
                                            ))
                                        } else {
                                            None
                                        },
                                        &self.location,
                                    ),
                                    node,
                                    path,
                                );
                            }
                            // Run-time ArbInclude or inactive if group
                            (_, other) => {
                                if other.warn() {
                                    if matches!(other, IncludeMode::GoogleInclude { .. }) {
                                        Some(ErrorKind::warn_ext_use(
                                            ext_name!("GL_GOOGLE_include_directive"),
                                            None,
                                            node.text_range(),
                                            &self.location,
                                        ))
                                    } else if matches!(other, IncludeMode::ArbInclude { .. }) {
                                        Some(ErrorKind::warn_ext_use(
                                            ext_name!("GL_ARB_shading_language_include"),
                                            None,
                                            node.text_range(),
                                            &self.location,
                                        ))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }
                        };

                        // Forward the directive
                        Event::directive_errors(include, !active, error, &self.location)
                    }
                    Err(error) => {
                        if current_state.include_mode == IncludeMode::None {
                            // If includes are not enabled, the proper error we need to report is
                            // IncludeNotSupported, ignoring any errors inside the directive body
                            Event::directive_error(
                                (ProcessingErrorKind::IncludeNotSupported, node),
                                &self.location,
                                !active,
                            )
                        } else {
                            Event::directive_error(error, &self.location, !active)
                        }
                    }
                }
            }
            PP_LINE => {
                let active = self.if_stack.active();
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
                        let error = if active {
                            if let Some(line) = line {
                                // Check that we support cpp style line
                                let (line, error) = match line {
                                    ParsedLine::Line(_) | ParsedLine::LineAndFileNumber(_, _) => {
                                        (line, None)
                                    }
                                    ParsedLine::LineAndPath(_, _)
                                        if current_state.cpp_style_line() =>
                                    {
                                        (line, None)
                                    }
                                    ParsedLine::LineAndPath(line, _) => (
                                        ParsedLine::Line(line),
                                        Some(
                                            ProcessingErrorKind::CppStyleLineNotSupported
                                                .with_node(
                                                    ld.node().clone().into(),
                                                    &self.location,
                                                ),
                                        ),
                                    ),
                                };

                                self.location
                                    .add_override(ld.node().text_range().start(), line);
                                error
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        Event::directive_errors(ld, !active, error, &self.location)
                    }
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            PP_PRAGMA => {
                let active = self.if_stack.active();
                let directive: DirectiveResult<Pragma> = node.try_into();

                match directive {
                    Ok(pragma) => Event::directive(pragma, !active),
                    Err(error) => Event::directive_error(error, &self.location, !active),
                }
            }
            ERROR => {
                // Unknown preprocessor directive, these are already reported as parse errors
                Event::directive(Directive::new(node, Invalid), !self.if_stack.active())
            }
            _ => {
                // Should never happen if all preprocessor directives are implemented
                panic!("unhandled node type: {:?}", node.kind());
            }
        }
        .into()
    }

    fn handle_token(
        &mut self,
        current_state: ProcessorState,
        token: SyntaxToken,
        iterator: SyntaxElementChildren<PreprocessorLang>,
        errors: Vec<parser::Error>,
    ) -> Option<Event> {
        // Look for macro substitutions unless the current group is masked
        if let Some(definition) = (if self.if_stack.active() && token.kind() == IDENT_KW {
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
                    self.state = ExpandState::ExpandedTokens {
                        iterator: new_iterator,
                        errors,
                        events: invocation.substitute(&current_state, &self.location).into(),
                        current_state,
                    };
                }
                Ok(None) => {
                    // Could not parse a macro invocation starting at the current token, so just
                    // resume iterating normally
                    self.state = ExpandState::Iterate {
                        iterator,
                        errors,
                        current_state,
                    };

                    return Some(Event::token(token, false));
                }
                Err(err) => {
                    let mut events = ArrayVec::new();
                    events.push(Event::error(err, &self.location, false));
                    events.push(Event::token(token, false));

                    self.state = ExpandState::PendingEvents {
                        iterator,
                        errors,
                        events,
                        current_state,
                    };
                }
            }

            None
        } else {
            // No matching definition for this identifier or current if group is inactive, just keep iterating
            self.state = ExpandState::Iterate {
                iterator,
                errors,
                current_state,
            };

            Some(Event::token(token, !self.if_stack.active()))
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
                HandleNodeResult::Event(event) => {
                    self.state = ExpandState::Iterate {
                        iterator,
                        errors,
                        current_state,
                    };

                    Some(event)
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
                self.handle_token(current_state, token, iterator, errors)
            }
        }
    }
}

pub(crate) enum ExpandEvent {
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

                    return Some(Event::enter_file(self.location.current_file).into());
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

                                self.state = ExpandState::PendingOne {
                                    iterator,
                                    errors,
                                    node_or_token,
                                    current_state,
                                };

                                return Some(
                                    Event::error(error, &self.location, !self.if_stack.active())
                                        .into(),
                                );
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

                    return Some(ExpandEvent::EnterFile(current_state, node, path));
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
