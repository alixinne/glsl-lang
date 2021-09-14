use std::collections::VecDeque;

use glsl_lang_pp::{
    last::{self, LocatedIterator, MaybeToken, TokenState, Tokenizer},
    processor::event::{self, Error, EventDirective, OutputToken},
    types,
};

use glsl_lang_types::ast;

use lang_util::{located::Located, position::NodeSpan, FileId, NodeContent, TextRange};

use crate::{ParseContext, ParseOptions};

use super::{Directives, LexerPosition, LexicalError, Token};

pub type Item<E> = Result<(LexerPosition, Token, LexerPosition), LexicalError<E>>;

pub struct LexerCore {
    pub ctx: ParseContext,
    file_id: FileId,
    opts: ParseOptions,
    directives: Vec<EventDirective>,
}

pub enum HandleTokenResult<E: std::error::Error + 'static> {
    None,
    Item(Item<E>),
    Pending(VecDeque<Item<E>>, VecDeque<Result<last::Event, Located<E>>>),
}

impl<E: std::error::Error + 'static> HandleTokenResult<E> {
    pub fn push_item(&mut self, item: Item<E>) {
        match std::mem::take(self) {
            HandleTokenResult::None => {
                *self = HandleTokenResult::Item(item);
            }
            HandleTokenResult::Item(old_item) => {
                let mut pending_items = VecDeque::with_capacity(2);
                pending_items.push_back(old_item);
                pending_items.push_back(item);
                *self = HandleTokenResult::Pending(pending_items, VecDeque::new());
            }
            HandleTokenResult::Pending(mut items, events) => {
                items.push_back(item);
                *self = HandleTokenResult::Pending(items, events);
            }
        }
    }

    pub fn pop_item(&mut self) -> Option<Item<E>> {
        match std::mem::take(self) {
            HandleTokenResult::None => None,
            HandleTokenResult::Item(item) => Some(item),
            HandleTokenResult::Pending(mut items, events) => {
                let item = items.pop_front();
                if !items.is_empty() || !events.is_empty() {
                    *self = Self::Pending(items, events);
                }
                item
            }
        }
    }

    pub fn pop_event(&mut self) -> Option<Result<last::Event, Located<E>>> {
        match std::mem::take(self) {
            HandleTokenResult::None => None,
            HandleTokenResult::Item(item) => {
                *self = Self::Item(item);
                None
            }
            HandleTokenResult::Pending(items, mut events) => {
                let event = events.pop_front();
                if !items.is_empty() || !events.is_empty() {
                    *self = Self::Pending(items, events);
                }
                event
            }
        }
    }

    pub fn push_errors(&mut self, errors: impl IntoIterator<Item = Error>) {
        let items = errors.into_iter().map(|error| Err(error.into()));

        match std::mem::take(self) {
            HandleTokenResult::None => {
                *self = HandleTokenResult::Pending(items.collect(), Default::default());
            }
            HandleTokenResult::Item(old_item) => {
                let mut pending_items = VecDeque::with_capacity(items.size_hint().0 + 1);
                pending_items.push_back(old_item);
                pending_items.extend(items);
                *self = HandleTokenResult::Pending(pending_items, Default::default());
            }
            HandleTokenResult::Pending(mut old_items, events) => {
                old_items.extend(items);
                *self = HandleTokenResult::Pending(old_items, events);
            }
        }
    }
}

impl<E: std::error::Error + 'static> Default for HandleTokenResult<E> {
    fn default() -> Self {
        Self::None
    }
}

impl LexerCore {
    pub fn new(opts: &ParseOptions, ctx: ParseContext) -> Self {
        let file_id = opts.source_id;
        Self {
            ctx,
            file_id,
            opts: *opts,
            directives: Vec::with_capacity(2),
        }
    }

    fn lang_token(
        &self,
        source_token: &OutputToken,
        token_kind: types::Token,
    ) -> Result<(LexerPosition, Token, LexerPosition), (types::Token, types::token::ErrorKind)>
    {
        crate::v2::lang_token(
            &self.ctx,
            source_token.text(),
            source_token.text_range(),
            token_kind,
        )
    }

    pub fn handle_file_id(&mut self, file_id: FileId) {
        self.file_id = file_id;
    }

    pub fn handle_token<'r, I, E>(
        &self,
        source_token: OutputToken,
        token_kind: types::Token,
        state: TokenState,
        tokenizer: &mut Tokenizer<'r, I>,
        token_state: &mut HandleTokenResult<E>,
    ) where
        E: std::error::Error + 'static,
        I: Iterator<Item = Result<event::Event, Located<E>>> + LocatedIterator,
        <Tokenizer<'r, I> as Iterator>::Item: MaybeToken,
    {
        if state.active() {
            match self.lang_token(&source_token, token_kind) {
                Ok(token) => {
                    // Try to get the next token when we encounter trivia
                    match token.1 {
                        Token::Whitespace => {}
                        Token::SingleLineComment | Token::MultiLineComment => {
                            if self.ctx.has_comments() {
                                let mut text = source_token.text().split_at(2).1.to_string();

                                let comment = match token.1 {
                                    Token::SingleLineComment => ast::CommentData::Single(text),
                                    Token::MultiLineComment => {
                                        text.pop();
                                        text.pop();
                                        ast::CommentData::Multi(text)
                                    }
                                    _ => unreachable!(),
                                }
                                .spanned(token.0, token.2);

                                self.ctx.add_comment(comment);
                            }
                        }
                        _ => {
                            if token.1 == Token::LeftBrace {
                                self.ctx.push_scope();
                            } else if token.1 == Token::RightBrace {
                                self.ctx.pop_scope();
                            }

                            token_state.push_item(Ok(token));
                        }
                    }
                }

                Err((token_kind, error)) => {
                    if !(self.opts.allow_rs_ident
                        && error == types::token::ErrorKind::InvalidToken
                        && token_kind == types::Token::HASH)
                    {
                        token_state.push_item(Err(LexicalError::Token {
                            kind: error,
                            pos: source_token.text_range(),
                        }));
                    } else {
                        // Try to detect #(...)
                        let start = source_token.text_range().start();
                        let mut end = start;

                        let mut pending_items = VecDeque::new();
                        let mut pending_events = VecDeque::new();

                        pending_items.push_back(Err(LexicalError::Token {
                            kind: types::token::ErrorKind::InvalidToken,
                            pos: NodeSpan::new(
                                start.source_id,
                                TextRange::new(start.offset, end.offset),
                            ),
                        }));

                        while let Some(maybe_lparen_result) = tokenizer.next() {
                            // Skip whitespace
                            if let Some(types::Token::WS) = maybe_lparen_result.as_token_kind() {
                                pending_events.push_back(maybe_lparen_result);
                                continue;
                            }

                            if let Some(types::Token::LPAREN) = maybe_lparen_result.as_token_kind()
                            {
                                // We have seen a left parenthesis
                                // Now, consume everything until the
                                // matching rparen
                                let mut level = 1;
                                let mut quoted = "#(".to_owned();

                                while level > 0 {
                                    match tokenizer.next() {
                                        Some(result) => {
                                            if let Some((source_token, token_kind, _)) =
                                                result.as_token()
                                            {
                                                match token_kind {
                                                    types::Token::LPAREN => level += 1,
                                                    types::Token::RPAREN => level -= 1,
                                                    _ => {}
                                                }

                                                if level > 0 {
                                                    quoted.push_str(source_token.text());
                                                } else {
                                                    end = source_token.text_range().start();
                                                }
                                            }
                                        }
                                        None => {
                                            // End of file: return an error. Since the same error
                                            // we would return has been added for the # token, just
                                            // exit the loop
                                            *token_state = HandleTokenResult::Pending(
                                                pending_items,
                                                pending_events,
                                            );
                                            return;
                                        }
                                    }
                                }

                                quoted.push(')');
                                token_state.push_item(Ok((
                                    start,
                                    Token::Identifier(quoted.into()),
                                    end,
                                )));
                                return;
                            } else {
                                pending_events.push_back(maybe_lparen_result);
                                *token_state =
                                    HandleTokenResult::Pending(pending_items, pending_events);
                                return;
                            }
                        }
                    }
                }
            };
        }
    }

    pub fn into_directives(self) -> Directives {
        self.directives.into()
    }

    pub fn handle_directive(
        &mut self,
        directive: EventDirective,
        masked: bool,
    ) -> Result<(), Vec<Error>> {
        if masked {
            return Ok(());
        }

        let errors = directive.errors().to_vec();

        self.directives.push(directive);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
