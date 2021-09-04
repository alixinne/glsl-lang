use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use itertools::Itertools;
use rowan::NodeOrToken;

use lang_util::{position::NodeSpan, FileId, SmolStr, TextRange};

use crate::{
    parser::{
        SyntaxKind::{self, *},
        SyntaxNode, SyntaxToken,
    },
    util::Unescaped,
};

use super::{
    event::{Event, OutputToken, ProcessingError, ProcessingErrorKind, TokenLike},
    expand::ExpandLocation,
    nodes::{Define, DefineFunction, DefineKind, DefineObject},
    ProcessorState,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition {
    Regular(Rc<Define>, FileId),
    Line,
    File,
    Version,
}

impl Definition {
    pub fn file_id(&self) -> FileId {
        match self {
            Definition::Regular(_, file_id) => *file_id,
            _ => FileId::builtin(0),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Definition::Regular(d, _) => d.name(),
            Definition::Line => "__LINE__",
            Definition::File => "__FILE__",
            Definition::Version => "__VERSION__",
        }
    }

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

    pub fn arg_count(&self) -> usize {
        match self {
            Definition::Regular(d, _) => match d.kind() {
                DefineKind::Object(_) => 0,
                DefineKind::Function(f) => f.arg_names().len(),
            },
            _ => 0,
        }
    }

    fn substitute_string(
        src: &str,
        kind: SyntaxKind,
        range: NodeSpan,
    ) -> impl IntoIterator<Item = OutputToken> {
        Some(OutputToken::new(kind, src, range))
    }

    fn concat_node_to_tokens<T: TokenLike>(
        definition_file_id: FileId,
        node: SyntaxNode,
        args: Option<&HashMap<&str, &[T]>>,
        entire_range: NodeSpan,
    ) -> impl IntoIterator<Item = OutputToken> {
        // Find the first non-trivial token
        #[derive(Debug)]
        enum State {
            Init,
            Lhs {
                kind: SyntaxKind,
                text: String,
                pos: NodeSpan,
                trivia_buffer: VecDeque<(SyntaxKind, SmolStr, NodeSpan)>,
            },
            ExpectRhs {
                kind: SyntaxKind,
                text: String,
                pos: NodeSpan,
                trivia_buffer: VecDeque<(SyntaxKind, SmolStr, NodeSpan)>,
            },
        }

        impl Default for State {
            fn default() -> Self {
                State::Init
            }
        }

        let mut state = State::default();

        let mut input_tokens = node
            .descendants_with_tokens()
            .filter_map(NodeOrToken::into_token);

        let mut output_tokens = Vec::new();

        let mut current_arg: Option<std::slice::Iter<'_, T>> = None;

        loop {
            let input_token;
            let (current_kind, current_text, current_span) = if let Some(iter) = &mut current_arg {
                if let Some(token) = iter.next() {
                    (token.kind(), token.text(), token.text_range())
                } else {
                    current_arg.take();
                    continue;
                }
            } else if let Some(token) = input_tokens.next() {
                if token.kind() == IDENT_KW {
                    if let Some(value) = args.and_then(|args| {
                        args.get(Unescaped::new(token.text()).to_string().as_ref())
                    }) {
                        current_arg = Some(value.iter());
                        continue;
                    }
                }

                input_token = token;
                (
                    input_token.kind(),
                    input_token.text(),
                    NodeSpan::new(definition_file_id, input_token.text_range()),
                )
            } else {
                // No more input tokens
                break;
            };

            match std::mem::take(&mut state) {
                State::Init => {
                    // Initial state, we should start with a token that can be pasted
                    if current_kind == PP_CONCAT_OP {
                        output_tokens.push(OutputToken::new_error(entire_range));
                        return output_tokens;
                    }

                    if current_kind.is_trivia() {
                        // Forward leading trivia
                        output_tokens.push(OutputToken::new(
                            current_kind,
                            current_text,
                            current_span,
                        ));
                    } else {
                        // Non-trivia is the LHS
                        state = State::Lhs {
                            kind: current_kind,
                            text: current_text.to_owned(),
                            pos: current_span,
                            trivia_buffer: VecDeque::with_capacity(1),
                        };
                    }
                }
                State::Lhs {
                    kind,
                    text,
                    pos,
                    mut trivia_buffer,
                } => {
                    // We saw the LHS of a concat
                    if current_kind == PP_CONCAT_OP {
                        // And then we saw the ##, so we're expecting the RHS
                        state = State::ExpectRhs {
                            kind,
                            text,
                            pos,
                            trivia_buffer: {
                                trivia_buffer.clear();
                                trivia_buffer
                            },
                        };
                    } else if current_kind.is_trivia() {
                        // Just buffer trivia between LHS and ##
                        state = State::Lhs {
                            kind,
                            text,
                            pos,
                            trivia_buffer: {
                                trivia_buffer.push_back((
                                    current_kind,
                                    current_text.into(),
                                    current_span,
                                ));
                                trivia_buffer
                            },
                        };
                    } else {
                        // Non-trivia instead of ##, so just bump the LHS and restart

                        output_tokens.push(OutputToken::new(kind, text, pos));

                        // Bump trivia
                        while let Some((kind, text, pos)) = trivia_buffer.pop_front() {
                            output_tokens.push(OutputToken::new(kind, text, pos));
                        }

                        // Restart with the new token as LHS
                        state = State::Lhs {
                            kind: current_kind,
                            text: current_text.to_owned(),
                            pos: current_span,
                            trivia_buffer,
                        };
                    }
                }

                State::ExpectRhs {
                    kind,
                    mut text,
                    pos,
                    mut trivia_buffer,
                } => {
                    // We are expecting the RHS
                    if current_kind == PP_CONCAT_OP {
                        // Can't concat with a ##
                        output_tokens.push(OutputToken::new_error(pos));
                        return output_tokens;
                    } else if current_kind.is_trivia() {
                        // Just buffer trivia between ## and RHS
                        state = State::ExpectRhs {
                            kind,
                            text,
                            pos,
                            trivia_buffer: {
                                trivia_buffer.push_back((
                                    current_kind,
                                    current_text.into(),
                                    current_span,
                                ));
                                trivia_buffer
                            },
                        };
                    } else {
                        // Non-trivia, build resulting token
                        state = State::Lhs {
                            kind: SyntaxKind::paste(kind, current_kind),
                            text: {
                                text.push_str(current_text);
                                text
                            },
                            pos,
                            trivia_buffer: {
                                // Discard trivia
                                trivia_buffer.clear();
                                trivia_buffer
                            },
                        };
                    }
                }
            }
        }

        match state {
            State::Init => {}
            State::Lhs {
                kind,
                text,
                pos,
                trivia_buffer: _,
            } => {
                output_tokens.push(OutputToken::new(kind, text, pos));
            }
            State::ExpectRhs {
                kind: _,
                mut text,
                pos,
                trivia_buffer: _,
            } => {
                // We were expecting a RHS
                text.push_str(" ##");
                output_tokens.push(OutputToken::new(ERROR, text, pos));
            }
        }

        output_tokens
    }

    fn substitute_define_object(
        definition_file_id: FileId,
        object: &DefineObject,
        entire_range: NodeSpan,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        let mut tokens = Vec::new();
        for node_or_token in object.body().children_with_tokens() {
            match node_or_token {
                NodeOrToken::Node(node) => {
                    debug_assert!(node.kind() == PP_CONCAT);
                    tokens.extend(Self::concat_node_to_tokens::<OutputToken>(
                        definition_file_id,
                        node,
                        None,
                        entire_range,
                    ));
                }
                NodeOrToken::Token(token) => {
                    tokens.push(OutputToken::new(
                        token.kind(),
                        token.text(),
                        NodeSpan::new(definition_file_id, token.text_range()),
                    ));
                }
            }
        }
        Self::subs_tokens(tokens, entire_range, location)
    }

    fn substitute_define_function(
        definition_file_id: FileId,
        function: &DefineFunction,
        args: &[Vec<impl TokenLike>],
        entire_range: NodeSpan,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        // Put the arguments into a hashmap
        let args: HashMap<_, _> = args
            .iter()
            .zip(function.arg_names())
            .map(|(tokens, arg_name)| {
                (
                    arg_name.as_str(),
                    // TODO: Should we trim whitespace out of macro arguments?
                    trim_ws(tokens),
                )
            })
            .collect();

        let mut tokens = Vec::new();
        for node_or_token in function.body().children_with_tokens() {
            match node_or_token {
                NodeOrToken::Node(node) => {
                    debug_assert!(node.kind() == PP_CONCAT);
                    tokens.extend(Self::concat_node_to_tokens(
                        definition_file_id,
                        node,
                        Some(&args),
                        entire_range,
                    ));
                }
                NodeOrToken::Token(token) => {
                    let kind = token.kind();

                    if kind == IDENT_KW {
                        if let Some(value) =
                            args.get(Unescaped::new(token.text()).to_string().as_ref())
                        {
                            // There is an argument with those tokens
                            for subs_token in value.iter() {
                                tokens.push(OutputToken::from_token(subs_token));
                            }

                            continue;
                        }
                    }

                    tokens.push(OutputToken::new(
                        kind,
                        token.text(),
                        NodeSpan::new(definition_file_id, token.text_range()),
                    ));
                }
            }
        }

        Self::subs_tokens(tokens, entire_range, location)
    }

    fn subs_tokens(
        tokens: impl IntoIterator<Item = OutputToken>,
        entire_range: NodeSpan,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        tokens
            .into_iter()
            .map(|token| {
                if token.kind() == ERROR {
                    Event::error(
                        ProcessingErrorKind::InvalidTokenPaste {
                            token: {
                                let text = token.text();
                                if text.is_empty() {
                                    None
                                } else {
                                    Some(text.into())
                                }
                            },
                        },
                        entire_range,
                        location,
                        false,
                    )
                } else {
                    token.into()
                }
            })
            .collect()
    }

    fn substitute_object(
        &self,
        entire_range: NodeSpan,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        match self {
            Definition::Line => Self::substitute_string(
                &location
                    .offset_to_line_and_col(entire_range.start().offset)
                    .0
                    .to_string(),
                DIGITS,
                entire_range,
            )
            .into_iter()
            .map(Into::into)
            .collect(),

            Definition::File => {
                let string = location.string();
                let (string, kind) = if string.is_number() {
                    (string.to_string(), DIGITS)
                } else {
                    (format!("\"{}\"", string), QUOTE_STRING)
                };

                Self::substitute_string(&string, kind, entire_range)
                    .into_iter()
                    .map(Into::into)
                    .collect()
            }
            Definition::Version => Self::substitute_string(
                &format!("{}", current_state.version.number),
                DIGITS,
                entire_range,
            )
            .into_iter()
            .map(Into::into)
            .collect(),

            Definition::Regular(define, _) => {
                if let DefineKind::Object(object) = define.kind() {
                    Self::substitute_define_object(self.file_id(), object, entire_range, location)
                } else {
                    panic!("expected object define")
                }
            }
        }
    }

    fn substitute_function(
        &self,
        args: &[Vec<impl TokenLike>],
        entire_range: NodeSpan,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        match self {
            Definition::Regular(define, _) => {
                if let DefineKind::Function(function) = define.kind() {
                    Self::substitute_define_function(
                        self.file_id(),
                        function,
                        args,
                        entire_range,
                        location,
                    )
                } else {
                    panic!("expected function define");
                }
            }
            _ => {
                panic!("expected function define");
            }
        }
    }
}

pub(crate) fn trim_ws<T: TokenLike>(tokens: &[T]) -> &[T] {
    let leading_ws = tokens
        .iter()
        .take_while(|token| token.kind().is_whitespace())
        .count();
    let trailing_ws = tokens
        .iter()
        .rev()
        .take_while(|token| token.kind().is_whitespace())
        .count();

    let end = tokens.len() - trailing_ws;
    if end < leading_ws {
        // The slice is only whitespace
        &[]
    } else {
        &tokens[leading_ws..(tokens.len() - trailing_ws)]
    }
}

pub struct MacroInvocation<'d> {
    definition: &'d Definition,
    tokens: MacroCall,
    range: NodeSpan,
}

enum MacroCall {
    Object,
    Function(Vec<Vec<OutputToken>>),
}

impl<'d> MacroInvocation<'d> {
    pub fn parse_raw<I>(
        definition: &'d Definition,
        first_token: SyntaxToken,
        iterator: I,
        location: &ExpandLocation,
    ) -> Result<Option<(MacroInvocation<'d>, I)>, ProcessingError>
    where
        I: Iterator<Item = NodeOrToken<SyntaxNode, SyntaxToken>>,
    {
        Self::parse_nested(definition, first_token, iterator, location, None, |token| {
            (token, location.current_file())
        })
    }

    fn parse_nested<I, Q, P>(
        definition: &'d Definition,
        first_token: Q,
        mut iterator: I,
        location: &ExpandLocation,
        text_range: Option<NodeSpan>,
        token_fn: impl Fn(Q) -> P,
    ) -> Result<Option<(Self, I)>, ProcessingError>
    where
        I: Iterator<Item = NodeOrToken<SyntaxNode, Q>>,
        P: TokenLike,
    {
        let first_token = token_fn(first_token);

        let (tokens, computed_range) = if definition.object_like() {
            (MacroCall::Object, first_token.text_range())
        } else {
            // A function-like, we need to parse arguments
            let mut args = Vec::new();
            let mut seen_comma = false;
            let mut nesting_level = 0;

            let token_start = first_token.text_range().start();
            let mut last_end = token_start;
            let token_end = loop {
                match iterator.next() {
                    Some(node_or_token) => match node_or_token {
                        NodeOrToken::Node(node) => {
                            // Node, i.e. a processing directive. unexpected here
                            return Err(ProcessingError::builder()
                                .pos(first_token.text_range())
                                .resolve_file(location)
                                .finish(ProcessingErrorKind::UnexpectedDirective {
                                    ident: definition.name().into(),
                                    node: (&node).into(),
                                }));
                        }
                        NodeOrToken::Token(inner_token) => {
                            let inner_token = token_fn(inner_token);

                            // A token
                            let kind = inner_token.kind();
                            let end = inner_token.text_range().end();
                            last_end = end;

                            if nesting_level == 0 {
                                if kind.is_whitespace() {
                                    // Just ignore it, it's whitespace before the
                                    // first lparen
                                } else if kind == LPAREN {
                                    nesting_level += 1;

                                    // Create space for first argument
                                    args.push(Vec::new());
                                } else {
                                    // Unexpected garbage. Note that this is not fatal to the
                                    // compiler, the identifier will just get ignored
                                    return Ok(None);
                                }
                            } else if kind == COMMA && nesting_level == 1 {
                                // Create space for next argument
                                args.push(Vec::new());
                                seen_comma = true;
                            } else {
                                if kind == LPAREN {
                                    nesting_level += 1;
                                } else if kind == RPAREN {
                                    nesting_level -= 1;
                                }

                                if nesting_level > 0 {
                                    args.last_mut()
                                        .unwrap()
                                        .push(OutputToken::from_token(&inner_token));
                                }
                            }

                            if kind == RPAREN && nesting_level == 0 {
                                break end;
                            }
                        }
                    },
                    None => {
                        // End-of-file. Not that we haven't consumed any nodes yet
                        // so we just need to return the events via the state
                        return Err(ProcessingError::builder()
                            .pos(text_range.unwrap_or_else(|| {
                                NodeSpan::new(
                                    token_start.source_id,
                                    TextRange::new(token_start.offset, last_end.offset),
                                )
                            }))
                            .resolve_file(location)
                            .finish(ProcessingErrorKind::UnterminatedMacroInvocation {
                                ident: definition.name().into(),
                            }));
                    }
                }
            };

            // If we haven't seen a comma, this ambiguous: it could either be a 0-arguments
            // macro or one argument which happens to be empty. To fix this, we pop the empty
            // argument we may have added, but only if it's trivia
            if !seen_comma
                && definition.arg_count() == 0
                && args.len() == 1
                && args
                    .first()
                    .unwrap()
                    .iter()
                    .all(|token| token.kind().is_whitespace())
            {
                args.pop();
            }

            if args.len() != definition.arg_count() {
                return Err(ProcessingError::builder()
                    .pos(first_token.text_range())
                    .resolve_file(location)
                    .finish(ProcessingErrorKind::MismatchedArguments {
                        ident: definition.name().into(),
                        expected: definition.arg_count(),
                        actual: args.len(),
                    }));
            }

            (
                MacroCall::Function(args),
                NodeSpan::new(
                    token_start.source_id,
                    TextRange::new(token_start.offset, token_end.offset),
                ),
            )
        };

        Ok(Some((
            Self {
                definition,
                tokens,
                range: text_range.unwrap_or(computed_range),
            },
            iterator,
        )))
    }

    pub fn substitute_vec(
        current_state: &ProcessorState,
        tokens: Vec<impl TokenLike>,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        let mut subs_stack = HashSet::new();
        Self::substitute_vec_inner(current_state, tokens, location, &mut subs_stack, None)
    }

    fn substitute_vec_inner(
        current_state: &ProcessorState,
        tokens: Vec<impl TokenLike>,
        location: &ExpandLocation,
        subs_stack: &mut HashSet<SmolStr>,
        range: Option<NodeSpan>,
    ) -> Vec<Event> {
        // Macros are recursive, so we need to scan again for further substitutions
        let mut result = Vec::with_capacity(tokens.len());
        let mut iterator = tokens.into_iter().map(NodeOrToken::Token);
        let mut seen_defined_recently = false;

        while let Some(node_or_token) = iterator.next() {
            // Just a regular token
            match node_or_token {
                NodeOrToken::Node(_) => unreachable!(),
                NodeOrToken::Token(token) => {
                    let kind = token.kind();

                    if let Some(definition) = (if kind == IDENT_KW && !seen_defined_recently {
                        Some(Unescaped::new(token.text()).to_string())
                    } else {
                        None
                    })
                    .and_then(|ident| {
                        if subs_stack.contains(ident.as_ref()) {
                            None
                        } else {
                            Some(ident)
                        }
                    })
                    .and_then(|ident| current_state.definitions.get(ident.as_ref()))
                    {
                        match MacroInvocation::parse_nested(
                            definition,
                            token.clone(),
                            iterator.clone(),
                            location,
                            range,
                            |token| token,
                        ) {
                            Ok(Some((invocation, new_iterator))) => {
                                result.extend(invocation.substitute_inner(
                                    current_state,
                                    location,
                                    subs_stack,
                                ));

                                iterator = new_iterator;
                            }
                            Ok(None) => {
                                result.push(Event::token(token, false));
                            }
                            Err(err) => {
                                result.push(Event::map_error(err, false));
                            }
                        }
                    } else {
                        result.push(Event::token(token, false));
                    }

                    if seen_defined_recently {
                        if !kind.is_trivia() {
                            if kind == LPAREN {
                                // Wait for (maybe) an IDENT_KW
                            } else {
                                // IDENT_KW, RPAREN, anything else: done
                                seen_defined_recently = false;
                            }
                        }
                    } else if kind == DEFINED {
                        seen_defined_recently = true;
                    }
                }
            }
        }

        result
    }

    pub fn substitute(
        self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> Vec<Event> {
        let mut subs_stack = HashSet::new();
        self.substitute_inner(current_state, location, &mut subs_stack)
    }

    fn substitute_inner(
        self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
        subs_stack: &mut HashSet<SmolStr>,
    ) -> Vec<Event> {
        let events = match self.tokens {
            MacroCall::Object => {
                self.definition
                    .substitute_object(self.range, current_state, location)
            }
            MacroCall::Function(function) => self
                .definition
                .substitute_function(&function, self.range, location),
        };

        // Disable recursion for the current name
        subs_stack.insert(self.definition.name().into());

        let range = Some(self.range);

        // We use itertools group_by to insert the error events in the right locations in the
        // output sequence. This means we split the token sequence at errors and this wouldn't
        // return the "right" result accross errors, but since there's an error, there is no
        // spec-defined expected result.
        let result: Vec<_> = events
            .into_iter()
            .group_by(Event::is_token)
            .into_iter()
            .map(|(is_token, events)| {
                if is_token {
                    // A token sequence
                    // TODO: Prevent re-allocation
                    Self::substitute_vec_inner(
                        current_state,
                        events
                            .into_iter()
                            .filter_map(Event::into_token)
                            .collect::<Vec<_>>(),
                        location,
                        subs_stack,
                        range,
                    )
                } else {
                    events.collect()
                }
            })
            .flatten()
            .collect();

        subs_stack.remove(self.definition.name());

        result
    }
}
