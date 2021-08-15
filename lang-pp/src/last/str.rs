use std::iter::FusedIterator;

use lang_util::FileId;

use crate::{
    exts::Registry,
    parser::SyntaxNode,
    processor::{
        event::{self, DirectiveKind, Error, ErrorKind, OutputToken, TokenLike},
        str::ProcessStrError,
    },
};

use super::{
    type_names::TypeNameAtom, LocatedIterator, Token, TokenState, TypeNameState, TypeTable,
};

#[derive(Debug)]
pub enum Event {
    Error {
        error: Error,
        masked: bool,
    },
    EnterFile(FileId),
    Token {
        source_token: OutputToken,
        token_kind: Token,
        state: TokenState,
    },
    Directive {
        node: SyntaxNode,
        kind: DirectiveKind,
        masked: bool,
        errors: Vec<Error>,
    },
}

pub struct Tokenizer<'r, I> {
    inner: I,
    type_table: TypeTable<'r>,
    pending_error: Option<Error>,
}

impl<'r, I: LocatedIterator> Tokenizer<'r, I> {
    pub fn new(inner: I, target_vulkan: bool, registry: &'r Registry) -> Self {
        Self {
            inner,
            type_table: TypeTable::new(registry, target_vulkan),
            pending_error: None,
        }
    }

    pub fn tokenize_single(
        &self,
        token: &impl TokenLike,
    ) -> (Token, Option<TypeNameState>, Option<Error>) {
        self.type_table
            .tokenize_single(token, self.inner.location())
    }
}

impl<'r, I> super::Tokenizer for Tokenizer<'r, I> {
    fn promote_type_name(&mut self, name: TypeNameAtom) -> bool {
        self.type_table.promote_type_name(name)
    }
}

impl<'r, I: Iterator<Item = Result<event::Event, ProcessStrError>> + LocatedIterator> Iterator
    for Tokenizer<'r, I>
{
    type Item = Result<Event, ProcessStrError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(error) = self.pending_error.take() {
            return Some(Ok(Event::Error {
                error,
                masked: false,
            }));
        }

        self.inner.next().map(|result| match result {
            Ok(event) => Ok(match event {
                event::Event::Error { error, masked } => Event::Error { error, masked },
                event::Event::EnterFile(file_id) => Event::EnterFile(file_id),
                event::Event::Token { token, masked } => {
                    let (token_kind, state, error) = self.tokenize_single(&token);

                    if !masked {
                        self.pending_error = error;
                    }

                    Event::Token {
                        source_token: token,
                        token_kind,
                        state: TokenState::new(state, masked),
                    }
                }
                event::Event::Directive {
                    node,
                    kind,
                    masked,
                    errors,
                } => {
                    if !masked {
                        if let DirectiveKind::Extension(extension) = &kind {
                            if !self.type_table.handle_extension(extension) {
                                self.pending_error = Some(Error::new(
                                    ErrorKind::unsupported_ext(
                                        extension.name.clone(),
                                        node.text_range(),
                                        self.inner.location(),
                                    ),
                                    self.inner.location(),
                                ));
                            }
                        }
                    }

                    Event::Directive {
                        node,
                        kind,
                        masked,
                        errors,
                    }
                }
            }),
            Err(err) => Err(err),
        })
    }
}

impl<'r, I: Iterator<Item = Result<event::Event, ProcessStrError>> + LocatedIterator> FusedIterator
    for Tokenizer<'r, I>
{
}
