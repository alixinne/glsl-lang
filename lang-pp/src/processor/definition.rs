use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use rowan::{GreenNodeBuilder, NodeOrToken, TextRange};
use smol_str::SmolStr;

use crate::{
    lexer::LineMap,
    parser::{
        SyntaxKind::{self, *},
        SyntaxNode, SyntaxToken,
    },
    FileId, Unescaped,
};

use super::{
    event::{Error, ErrorKind},
    nodes::{Define, DefineFunction, DefineKind, DefineObject},
    Event, FileSystem, OutputToken, ProcessingError, ProcessingErrorKind, ProcessorState,
    TokenLike,
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
                            let value = {
                                let leading_ws = value
                                    .iter()
                                    .take_while(|token| token.kind().is_whitespace())
                                    .count();
                                let trailing_ws = value
                                    .iter()
                                    .rev()
                                    .take_while(|token| token.kind().is_whitespace())
                                    .count();

                                &value[leading_ws..(value.len() - trailing_ws)]
                            };

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
        line_map: &LineMap,
    ) -> Option<Vec<OutputToken>> {
        match self {
            Definition::Line => Some(
                Self::substitute_string(
                    &format!(
                        "{}",
                        line_map.get_line_and_col(entire_range.start().into()).0 + 1
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
            Definition::Regular(define, _) => {
                if let DefineKind::Object(object) = define.kind() {
                    Self::substitute_define_object(object, entire_range)
                } else {
                    None
                }
            }
            _ => None,
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

impl<'d, T: TokenLike> MacroInvocation<'d, T> {
    pub fn parse<I>(
        definition: &'d Definition,
        first_token: T,
        iterator: I,
        line_map: &LineMap,
    ) -> Result<Option<(Self, I)>, ProcessingError>
    where
        I: Iterator<Item = NodeOrToken<SyntaxNode, T>>,
    {
        Self::parse_nested(definition, first_token, iterator, line_map, None)
    }

    fn parse_nested<I>(
        definition: &'d Definition,
        first_token: T,
        mut iterator: I,
        line_map: &LineMap,
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
                            .with_node(first_token.into(), line_map));
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
                        .with_node(first_token.into(), line_map));
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
                .with_node(first_token.into(), line_map));
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

    pub fn substitute<F: FileSystem>(
        self,
        current_state: &ProcessorState,
        line_map: &LineMap,
    ) -> Result<impl Iterator<Item = Event<F::Error>>, Error<F::Error>> {
        let mut subs_stack = HashSet::new();
        self.substitute_inner::<F>(current_state, line_map, &mut subs_stack)
    }

    fn substitute_inner<F: FileSystem>(
        self,
        current_state: &ProcessorState,
        line_map: &LineMap,
        subs_stack: &mut HashSet<SmolStr>,
    ) -> Result<impl Iterator<Item = Event<F::Error>>, Error<F::Error>> {
        let result = match self.tokens {
            MacroCall::Object => {
                self.definition
                    .substitute_object(self.range, current_state, line_map)
            }
            MacroCall::Function(function) => {
                self.definition.substitute_function(function, self.range)
            }
        };

        if let Some(tokens) = result {
            // Disable recursion for the current name
            subs_stack.insert(self.definition.name().into());

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
                                line_map,
                                Some(self.range),
                            ) {
                                Ok(Some((invocation, new_iterator))) => {
                                    match invocation.substitute_inner::<F>(
                                        current_state,
                                        line_map,
                                        subs_stack,
                                    ) {
                                        Ok(events) => {
                                            result.extend(events);
                                            iterator = new_iterator;
                                        }
                                        Err(err) => {
                                            result.push(Event::Error(err.into()));
                                            result.push(Event::Token(token));
                                        }
                                    }
                                }
                                Ok(None) => {
                                    result.push(Event::Token(token));
                                }
                                Err(err) => {
                                    result.push(Event::Error(err.into()));
                                }
                            }
                        } else {
                            result.push(Event::Token(token));
                        }
                    }
                }
            }

            subs_stack.remove(self.definition.name());

            Ok(result.into_iter())
        } else {
            Err(ErrorKind::unhandled(self.first_token.into(), line_map).into())
        }
    }
}
