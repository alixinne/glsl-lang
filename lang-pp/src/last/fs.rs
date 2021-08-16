use std::{convert::TryFrom, iter::FusedIterator, path::PathBuf};

use lang_util::FileId;

use crate::{
    exts::Registry,
    parser::SyntaxNode,
    processor::{
        event::{self, DirectiveKind, Error, ErrorKind, Located, OutputToken, TokenLike},
        expand::ExpandLocation,
    },
};

use super::{
    type_names::TypeNameAtom, LocatedIterator, MaybeToken, Token, TokenState, TypeNameState,
    TypeTable,
};

#[derive(Debug)]
pub enum Event<E: std::error::Error + 'static> {
    IoError(event::Located<E>),
    Error {
        error: Error,
        masked: bool,
    },
    EnterFile {
        file_id: FileId,
        path: PathBuf,
        canonical_path: PathBuf,
    },
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

impl<E: std::error::Error + 'static> TryFrom<Event<E>> for super::str::Event {
    type Error = Located<E>;

    fn try_from(value: Event<E>) -> Result<Self, Located<E>> {
        match value {
            Event::IoError(err) => Err(err),
            Event::Error { error, masked } => Ok(super::str::Event::Error { error, masked }),
            Event::EnterFile { file_id, .. } => Ok(super::str::Event::EnterFile(file_id)),
            Event::Token {
                source_token,
                token_kind,
                state,
            } => Ok(super::str::Event::Token {
                source_token,
                token_kind,
                state,
            }),
            Event::Directive {
                node,
                kind,
                masked,
                errors,
            } => Ok(super::str::Event::Directive {
                node,
                kind,
                masked,
                errors,
            }),
        }
    }
}

impl<E: std::error::Error + 'static> MaybeToken for Event<E> {
    fn token(&self) -> Option<(&OutputToken, &Token, &TokenState)> {
        match self {
            Event::Token {
                source_token,
                token_kind,
                state,
            } => Some((source_token, token_kind, state)),
            _ => None,
        }
    }
}

pub struct Tokenizer<'r, I> {
    inner: I,
    type_table: TypeTable<'r>,
    pending_error: Option<Error>,
}

impl<
        'r,
        E: std::error::Error + 'static,
        I: Iterator<Item = event::IoEvent<E>> + LocatedIterator,
    > super::Tokenizer for Tokenizer<'r, I>
{
    type Item = Event<E>;
    type Error = Located<E>;

    fn promote_type_name(&mut self, name: TypeNameAtom) -> bool {
        self.type_table.promote_type_name(name)
    }

    fn next_event(&mut self) -> Option<Self::Item> {
        self.next()
    }

    fn location(&self) -> &ExpandLocation {
        self.inner.location()
    }
}

impl<
        'r,
        E: std::error::Error + 'static,
        I: Iterator<Item = event::IoEvent<E>> + LocatedIterator,
    > Tokenizer<'r, I>
{
    pub fn new(inner: I, target_vulkan: bool, registry: &'r Registry) -> Self {
        Self {
            inner,
            type_table: TypeTable::new(registry, target_vulkan),
            pending_error: None,
        }
    }

    pub fn inner(&self) -> &I {
        &self.inner
    }

    pub fn tokenize_single(
        &self,
        token: &impl TokenLike,
    ) -> (Token, Option<TypeNameState>, Option<Error>) {
        self.type_table
            .tokenize_single(token, self.inner.location())
    }
}

impl<
        'r,
        E: std::error::Error + 'static,
        I: Iterator<Item = event::IoEvent<E>> + LocatedIterator,
    > Iterator for Tokenizer<'r, I>
{
    type Item = Event<E>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(error) = self.pending_error.take() {
            return Some(Event::Error {
                error,
                masked: false,
            });
        }

        self.inner.next().map(|event| match event {
            event::IoEvent::IoError(error) => Event::IoError(error),
            event::IoEvent::Error { error, masked } => Event::Error { error, masked },
            event::IoEvent::EnterFile {
                file_id,
                path,
                canonical_path,
            } => Event::EnterFile {
                file_id,
                path,
                canonical_path,
            },
            event::IoEvent::Token { token, masked } => {
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
            event::IoEvent::Directive {
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
        })
    }
}

impl<
        'r,
        E: std::error::Error + 'static,
        I: Iterator<Item = event::IoEvent<E>> + LocatedIterator,
    > FusedIterator for Tokenizer<'r, I>
{
}
