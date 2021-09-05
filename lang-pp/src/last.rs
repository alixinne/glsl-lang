//! Last preprocessing stage definitions

use std::{collections::HashMap, iter::FusedIterator, path::PathBuf};

use lang_util::{located::FileIdResolver, FileId};

use crate::{
    exts::{names::ExtNameAtom, ExtensionSpec, Registry},
    processor::{
        event::{self, DirectiveKind, Error, ErrorKind, EventDirective, OutputToken, TokenLike},
        expand::ExpandLocation,
        nodes::{Extension, ExtensionBehavior, ExtensionName},
    },
    types::{
        type_names::{TypeNameAtom, TypeNameState},
        Token, TypeName,
    },
};

mod token;

#[derive(Debug, PartialEq)]
pub enum Event {
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
        directive: EventDirective,
        masked: bool,
    },
}

pub trait LocatedIterator {
    fn location(&self) -> &ExpandLocation;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenState {
    Masked,
    Active,
    Warn(ExtNameAtom),
}

impl TokenState {
    fn new(state: Option<TypeNameState>, masked: bool) -> Self {
        if masked {
            Self::Masked
        } else if let Some(state) = state {
            match state {
                TypeNameState::WarnType(name) => Self::Warn(name),
                _ => Self::Active,
            }
        } else {
            Self::Active
        }
    }

    pub fn active(self) -> bool {
        matches!(self, Self::Active | Self::Warn(_))
    }

    pub fn warn(self) -> bool {
        matches!(self, Self::Warn(_))
    }
}

struct TypeTable<'r> {
    type_names: HashMap<TypeNameAtom, Option<(ExtNameAtom, ExtensionBehavior)>>,
    extensions: HashMap<ExtNameAtom, ExtensionBehavior>,
    registry: &'r Registry,
    target_vulkan: bool,
    current_version: u16,
}

impl<'r> TypeTable<'r> {
    fn new(registry: &'r Registry, current_version: u16, target_vulkan: bool) -> Self {
        Self {
            type_names: Default::default(),
            extensions: Default::default(),
            registry,
            target_vulkan,
            current_version,
        }
    }

    fn is_type_name(&self, name: &TypeNameAtom) -> TypeNameState {
        if let Some(type_def) = self.type_names.get(name) {
            match type_def {
                Some((name, behavior)) if *behavior == ExtensionBehavior::Warn => {
                    TypeNameState::WarnType(name.clone())
                }
                _ => TypeNameState::Type,
            }
        } else {
            TypeNameState::Ident
        }
    }

    fn promote_type_name(&mut self, name: TypeNameAtom) -> bool {
        self.type_names.insert(name, None).is_some()
    }

    fn set_extension_behavior(&mut self, spec: &ExtensionSpec, behavior: ExtensionBehavior) {
        if behavior == ExtensionBehavior::Disable {
            // Disable the extension
            for type_name in spec.type_names() {
                self.type_names.remove(type_name);
            }

            self.extensions.remove(spec.name());
        } else {
            // Enable the extension
            for type_name in spec.type_names() {
                self.type_names
                    .insert(type_name.clone(), Some((spec.name().clone(), behavior)));
            }

            self.extensions.insert(spec.name().clone(), behavior);
        }
    }

    fn handle_extension(&mut self, extension: &Extension) -> bool {
        match &extension.name {
            ExtensionName::All => {
                if extension.behavior == ExtensionBehavior::Disable
                    || extension.behavior == ExtensionBehavior::Warn
                {
                    for ext in self.registry.all() {
                        self.set_extension_behavior(ext, extension.behavior);
                    }
                } else {
                    // TODO: Handle invalid behavior for all
                }
            }
            ExtensionName::Specific(name) => {
                if let Some(spec) = self.registry.get(name) {
                    self.set_extension_behavior(spec, extension.behavior);
                } else {
                    return false;
                }
            }
        }

        true
    }

    fn tokenize_single(
        &self,
        token: &impl TokenLike,
        location: &ExpandLocation,
    ) -> (Token, Option<TypeNameState>, Option<Error>) {
        let (token_kind, state) =
            token::token_from_syntax_kind(token, self.current_version, self.target_vulkan, |tn| {
                self.is_type_name(tn)
            });

        let error = if let Some(TypeNameState::WarnType(extension)) = &state {
            Some(
                Error::builder()
                    .pos(token.text_range())
                    .resolve_file(location)
                    .finish(ErrorKind::warn_ext_use(
                        extension.clone(),
                        match &token_kind {
                            Token::TYPE_NAME(TypeName::OTHER(type_name)) => Some(type_name.clone()),
                            _ => unreachable!(),
                        },
                        token.text_range(),
                        location,
                    )),
            )
        } else {
            None
        };

        (token_kind, state, error)
    }
}

pub trait MaybeToken {
    fn as_token(&self) -> Option<(&OutputToken, &Token, &TokenState)>;

    fn as_token_kind(&self) -> Option<&Token> {
        self.as_token().map(|(_, kind, _)| kind)
    }
}

impl<T: MaybeToken, E> MaybeToken for Result<T, E> {
    fn as_token(&self) -> Option<(&OutputToken, &Token, &TokenState)> {
        self.as_ref().ok().and_then(MaybeToken::as_token)
    }
}

impl MaybeToken for Event {
    fn as_token(&self) -> Option<(&OutputToken, &Token, &TokenState)> {
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

impl<'r, I: LocatedIterator> Tokenizer<'r, I> {
    pub fn new(
        inner: impl IntoIterator<IntoIter = I>,
        current_version: u16,
        target_vulkan: bool,
        registry: &'r Registry,
    ) -> Self {
        Self {
            inner: inner.into_iter(),
            type_table: TypeTable::new(registry, current_version, target_vulkan),
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

    pub fn promote_type_name(&mut self, name: TypeNameAtom) -> bool {
        self.type_table.promote_type_name(name)
    }

    pub fn location(&self) -> &crate::processor::expand::ExpandLocation {
        self.inner.location()
    }
}

impl<'r, E, I: Iterator<Item = Result<event::Event, E>> + LocatedIterator> Iterator
    for Tokenizer<'r, I>
{
    type Item = Result<Event, E>;

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
                event::Event::EnterFile {
                    file_id,
                    path,
                    canonical_path,
                } => Event::EnterFile {
                    file_id,
                    path,
                    canonical_path,
                },
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
                event::Event::Directive { directive, masked } => {
                    if !masked {
                        match directive.kind() {
                            DirectiveKind::Version(version) => {
                                self.type_table.current_version = version.number;
                            }
                            DirectiveKind::Extension(extension) => {
                                if !self.type_table.handle_extension(extension) {
                                    self.pending_error = Some(
                                        Error::builder()
                                            .pos(directive.text_range())
                                            .resolve_file(self.inner.location())
                                            .finish(ErrorKind::unsupported_ext(
                                                extension.name.clone(),
                                                directive.text_range(),
                                                self.inner.location(),
                                            )),
                                    );
                                }
                            }
                            _ => {}
                        }
                    }

                    Event::Directive { directive, masked }
                }
            }),
            Err(err) => Err(err),
        })
    }
}

impl<'r, E, I: Iterator<Item = Result<event::Event, E>> + LocatedIterator> FusedIterator
    for Tokenizer<'r, I>
{
}

impl<'r, I: FileIdResolver> FileIdResolver for Tokenizer<'r, I> {
    fn resolve(&self, file_id: FileId) -> Option<&std::path::Path> {
        self.inner.resolve(file_id)
    }
}

#[cfg(test)]
mod tests {
    use lang_util::FileId;
    use rowan::NodeOrToken;

    use crate::processor::event::DirectiveKind;

    use super::{Event, MaybeToken};

    #[test]
    fn test_float_constant() {
        use super::Token::*;

        fn parse(src: &str) -> Vec<super::Token> {
            crate::processor::str::process(src, crate::processor::ProcessorState::default())
                .tokenize(100, false, &crate::exts::DEFAULT_REGISTRY)
                .filter_map(|evt| evt.as_token().map(|(_, kind, _)| kind.clone()))
                .collect()
        }

        assert_eq!(parse("1e-34"), &[FLOAT_CONST(1E-34)]);
        assert_eq!(parse("1e-34f"), &[FLOAT_CONST(1E-34)]);
        assert_eq!(parse("1E-34f"), &[FLOAT_CONST(1E-34)]);
        assert_eq!(parse("1e-34F"), &[FLOAT_CONST(1E-34)]);
        assert_eq!(parse("1E-34F"), &[FLOAT_CONST(1E-34)]);
    }

    #[test]
    /// Ensure that we can extract #(...) for glsl-lang-quote. This is not part of the spec so this
    /// explains why we need to re-examine tokens using Tokenizer::tokenize_single.
    ///
    /// The default behavior is spec-compliant (#( is an invalid preprocessor directive), but we
    /// can still get this alternative behavior where #(...) is interpreted as a quoting
    /// interpolation.
    fn test_hash_ident() {
        use super::Token::*;

        let mut tokens = Vec::new();

        let mut tokenizer = crate::processor::str::process(
            "#(ident) = hello",
            crate::processor::ProcessorState::default(),
        )
        .tokenize(100, false, &crate::exts::DEFAULT_REGISTRY);

        #[allow(clippy::while_let_on_iterator)]
        while let Some(result) = tokenizer.next() {
            if let Ok(event) = result {
                match event {
                    Event::Token { token_kind, .. } => {
                        tokens.push(token_kind);
                    }
                    Event::Directive { directive, .. }
                        if matches!(directive.kind(), DirectiveKind::Invalid(_)) =>
                    {
                        // Extract the tokens from the directive parse tree
                        tokens.extend(
                            directive
                                .node
                                .descendants_with_tokens()
                                .filter_map(NodeOrToken::into_token)
                                .map(|token| {
                                    tokenizer.tokenize_single(&(&token, FileId::default())).0
                                }),
                        );
                    }
                    _ => {}
                }
            }
        }

        assert_eq!(
            &tokens,
            &[
                HASH,
                LPAREN,
                IDENT("ident".into()),
                RPAREN,
                WS,
                EQUAL,
                WS,
                IDENT("hello".into())
            ]
        );
    }
}
