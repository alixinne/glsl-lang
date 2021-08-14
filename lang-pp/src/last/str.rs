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
    type_names::TypeNameAtom, LocatedIterator, Token, TokenState, TypeName, TypeNameState,
    TypeTable,
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

impl<'r, I> Tokenizer<'r, I> {
    pub fn new(inner: I, registry: &'r Registry) -> Self {
        Self {
            inner,
            type_table: TypeTable::new(registry),
            pending_error: None,
        }
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
                    let (token_kind, state) =
                        Token::from_token(&token, |tn| self.type_table.is_type_name(tn));

                    if !masked {
                        if let Some(TypeNameState::WarnType(extension)) = &state {
                            self.pending_error = Some(Error::new(
                                ErrorKind::warn_ext_use(
                                    extension.clone(),
                                    match &token_kind {
                                        Token::TYPE_NAME(TypeName::OTHER(type_name)) => {
                                            Some(type_name.clone())
                                        }
                                        _ => unreachable!(),
                                    },
                                    token.text_range(),
                                    self.inner.location(),
                                ),
                                self.inner.location(),
                            ));
                        }
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
                            self.type_table.handle_extension(extension);
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
