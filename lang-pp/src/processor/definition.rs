use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use rowan::{GreenNodeBuilder, NodeOrToken, TextRange};
use smol_str::SmolStr;

use lang_util::FileId;

use crate::{
    parser::{
        SyntaxKind::{self, *},
        SyntaxNode, SyntaxToken,
    },
    Unescaped,
};

use super::{
    event::{
        Error, ErrorKind, Event, OutputToken, ProcessingError, ProcessingErrorKind, TokenLike,
    },
    expand::ExpandLocation,
    nodes::{Define, DefineFunction, DefineKind, DefineObject},
    ProcessorState,
};

#[derive(Debug, Clone)]
pub enum Definition {
    Regular(Rc<Define>, FileId),
    Line,
    File,
    Version,
}

impl Definition {
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

    fn substitute_string(src: &str, kind: SyntaxKind) -> impl Iterator<Item = SyntaxToken> {
        let replaced = {
            // TODO: Reuse node cache
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(ROOT.into());
            builder.token(kind.into(), src);
            builder.finish_node();
            builder.finish()
        };

        Self::tokens(replaced)
    }

    fn tokens(replaced: rowan::GreenNode) -> impl Iterator<Item = SyntaxToken> {
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

    fn substitute_define_object(
        object: &DefineObject,
        entire_range: TextRange,
    ) -> Option<Vec<OutputToken>> {
        let replaced = {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(ROOT.into());

            for node_or_token in object.body().children_with_tokens() {
                if let Some(token) = node_or_token.as_token() {
                    let kind = token.kind();

                    // TODO: Handle PP_CONCAT
                    if kind == PP_CONCAT {
                        return None;
                    }

                    builder.token(kind.into(), token.text());
                }
            }

            builder.finish_node();
            builder.finish()
        };

        Some(
            Self::tokens(replaced)
                .map(|token| OutputToken::new(token, entire_range))
                .collect(),
        )
    }

    fn substitute_define_function(
        function: &DefineFunction,
        args: Vec<Vec<impl TokenLike>>,
        entire_range: TextRange,
    ) -> Option<Vec<OutputToken>> {
        // Put the arguments into a hashmap
        let args: HashMap<_, _> = args
            .into_iter()
            .zip(function.arg_names())
            .map(|(tokens, arg_name)| (arg_name.clone(), tokens))
            .collect();

        let replaced = {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(ROOT.into());

            for node_or_token in function.body().children_with_tokens() {
                if let Some(token) = node_or_token.as_token() {
                    let kind = token.kind();

                    // TODO: Handle PP_CONCAT
                    if kind == PP_CONCAT {
                        return None;
                    } else if kind == IDENT_KW {
                        if let Some(value) = args.get(token.text()) {
                            // There is an argument with those tokens
                            // TODO: Should we trim whitespace out of macro arguments?
                            let value = trim_ws(&value);

                            for subs_token in value.iter() {
                                builder.token(subs_token.kind().into(), subs_token.text());
                            }

                            continue;
                        }
                    }

                    builder.token(kind.into(), token.text());
                }
            }

            builder.finish_node();
            builder.finish()
        };

        Some(
            Self::tokens(replaced)
                .map(|token| OutputToken::new(token, entire_range))
                .collect(),
        )
    }

    fn substitute_object(
        &self,
        entire_range: TextRange,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> Option<Vec<OutputToken>> {
        match self {
            Definition::Line => Some(
                Self::substitute_string(
                    &location
                        .offset_to_line_number(entire_range.start())
                        .to_string(),
                    DIGITS,
                )
                .map(|token| OutputToken::new(token, entire_range))
                .collect(),
            ),
            Definition::File => {
                let string = location.string();
                let (string, kind) = if string.is_number() {
                    (string.to_string(), DIGITS)
                } else {
                    (format!("\"{}\"", string), QUOTE_STRING)
                };

                Some(
                    Self::substitute_string(&string, kind)
                        .map(|token| OutputToken::new(token, entire_range))
                        .collect(),
                )
            }
            Definition::Version => Some(
                Self::substitute_string(&format!("{}", current_state.version.number), DIGITS)
                    .map(|token| OutputToken::new(token, entire_range))
                    .collect(),
            ),
            Definition::Regular(define, _) => {
                if let DefineKind::Object(object) = define.kind() {
                    Self::substitute_define_object(object, entire_range)
                } else {
                    None
                }
            }
        }
    }

    fn substitute_function(
        &self,
        args: Vec<Vec<impl TokenLike>>,
        entire_range: TextRange,
    ) -> Option<Vec<OutputToken>> {
        match self {
            Definition::Regular(define, _) => {
                if let DefineKind::Function(function) = define.kind() {
                    Self::substitute_define_function(function, args, entire_range)
                } else {
                    None
                }
            }
            _ => None,
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

pub struct MacroInvocation<'d, T> {
    definition: &'d Definition,
    tokens: MacroCall<T>,
    range: TextRange,
    first_token: T,
}

enum MacroCall<T> {
    Object,
    Function(Vec<Vec<T>>),
}

impl<'d, T: TokenLike + Clone + Into<OutputToken>> MacroInvocation<'d, T> {
    pub fn parse<I>(
        definition: &'d Definition,
        first_token: T,
        iterator: I,
        location: &ExpandLocation,
    ) -> Result<Option<(Self, I)>, ProcessingError>
    where
        I: Iterator<Item = NodeOrToken<SyntaxNode, T>>,
    {
        Self::parse_nested(definition, first_token, iterator, location, None)
    }

    fn parse_nested<I>(
        definition: &'d Definition,
        first_token: T,
        mut iterator: I,
        location: &ExpandLocation,
        text_range: Option<TextRange>,
    ) -> Result<Option<(Self, I)>, ProcessingError>
    where
        I: Iterator<Item = NodeOrToken<SyntaxNode, T>>,
    {
        let (tokens, computed_range) = if definition.object_like() {
            (MacroCall::Object, first_token.text_range())
        } else {
            // A function-like, we need to parse arguments
            let mut args = Vec::new();
            let mut seen_comma = false;
            let mut nesting_level = 0;

            let token_start = first_token.text_range().start();
            let token_end = loop {
                match iterator.next() {
                    Some(node_or_token) => match node_or_token {
                        NodeOrToken::Node(node) => {
                            // Node, i.e. a processing directive. unexpected here
                            return Err(ProcessingErrorKind::UnexpectedDirective {
                                ident: definition.name().into(),
                                node,
                            }
                            .with_node(first_token.into(), location));
                        }
                        NodeOrToken::Token(inner_token) => {
                            // A token
                            let kind = inner_token.kind();
                            let end = inner_token.text_range().end();

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
                            } else {
                                if kind == COMMA && nesting_level == 1 {
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
                                        args.last_mut().unwrap().push(inner_token);
                                    }
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
                        return Err(ProcessingErrorKind::UnterminatedMacroInvocation {
                            ident: definition.name().into(),
                        }
                        .with_node(first_token.into(), location));
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
                return Err(ProcessingErrorKind::MismatchedArguments {
                    ident: definition.name().into(),
                    expected: definition.arg_count(),
                    actual: args.len(),
                }
                .with_node(first_token.into(), location));
            }

            (
                MacroCall::Function(args),
                TextRange::new(token_start, token_end),
            )
        };

        Ok(Some((
            Self {
                definition,
                tokens,
                range: text_range.unwrap_or(computed_range),
                first_token,
            },
            iterator,
        )))
    }

    pub fn substitute_vec(
        current_state: &ProcessorState,
        tokens: Vec<T>,
        location: &ExpandLocation,
    ) -> impl Iterator<Item = Event> {
        let mut subs_stack = HashSet::new();
        Self::substitute_vec_inner(current_state, tokens, location, &mut subs_stack, None)
    }

    fn substitute_vec_inner(
        current_state: &ProcessorState,
        tokens: Vec<T>,
        location: &ExpandLocation,
        subs_stack: &mut HashSet<SmolStr>,
        range: Option<TextRange>,
    ) -> impl Iterator<Item = Event> {
        // Macros are recursive, so we need to scan again for further substitutions
        let mut result = Vec::with_capacity(tokens.len());
        let mut iterator = tokens.into_iter().map(|item| NodeOrToken::Token(item));

        while let Some(node_or_token) = iterator.next() {
            // Just a regular token
            match node_or_token {
                NodeOrToken::Node(_) => unreachable!(),
                NodeOrToken::Token(token) => {
                    if let Some(definition) = (if token.kind() == IDENT_KW {
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
                        ) {
                            Ok(Some((invocation, new_iterator))) => {
                                match invocation.substitute_inner(
                                    current_state,
                                    location,
                                    subs_stack,
                                ) {
                                    Ok(events) => {
                                        result.extend(events);
                                        iterator = new_iterator;
                                    }
                                    Err(err) => {
                                        result.push(Event::Error(err.into()));
                                        result.push(Event::Token(token.into()));
                                    }
                                }
                            }
                            Ok(None) => {
                                result.push(Event::Token(token.into()));
                            }
                            Err(err) => {
                                result.push(Event::error(err, location));
                            }
                        }
                    } else {
                        result.push(Event::Token(token.into()));
                    }
                }
            }
        }

        result.into_iter()
    }

    pub fn substitute(
        self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> Result<impl Iterator<Item = Event>, Error> {
        let mut subs_stack = HashSet::new();
        self.substitute_inner(current_state, location, &mut subs_stack)
    }

    fn substitute_inner(
        self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
        subs_stack: &mut HashSet<SmolStr>,
    ) -> Result<impl Iterator<Item = Event>, Error> {
        let result = match self.tokens {
            MacroCall::Object => {
                self.definition
                    .substitute_object(self.range, current_state, location)
            }
            MacroCall::Function(function) => {
                self.definition.substitute_function(function, self.range)
            }
        };

        if let Some(tokens) = result {
            // Disable recursion for the current name
            subs_stack.insert(self.definition.name().into());

            let result = MacroInvocation::substitute_vec_inner(
                current_state,
                tokens,
                location,
                subs_stack,
                Some(self.range),
            );

            subs_stack.remove(self.definition.name());

            Ok(result)
        } else {
            Err(Error::new(
                ErrorKind::unhandled(self.first_token.into(), location.line_map()),
                location,
            ))
        }
    }
}
