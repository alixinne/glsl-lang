//! Last preprocessing stage definitions

#[macro_use]
pub mod keywords;
#[macro_use]
pub mod type_names;

pub mod fs;

pub mod str;

pub mod token;
use std::collections::HashMap;

pub use token::{Token, TypeName};

use type_names::TypeNameAtom;

use crate::{
    exts::{names::ExtNameAtom, ExtensionSpec, Registry},
    processor::{
        event::{Error, ErrorKind, TokenLike},
        expand::ExpandLocation,
        nodes::{Extension, ExtensionBehavior, ExtensionName},
    },
};

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
}

#[derive(Debug, Clone)]
pub enum TypeNameState {
    Ident,
    Type,
    WarnType(ExtNameAtom),
}

impl TypeNameState {
    pub fn is_type_name(&self) -> bool {
        matches!(self, Self::Type | Self::WarnType(_))
    }
}

impl<'r> TypeTable<'r> {
    fn new(registry: &'r Registry, target_vulkan: bool) -> Self {
        Self {
            type_names: Default::default(),
            extensions: Default::default(),
            registry,
            target_vulkan,
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
                if let Some(spec) = self.registry.get(&name) {
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
            Token::from_token(token, self.target_vulkan, |tn| self.is_type_name(tn));

        let error = if let Some(TypeNameState::WarnType(extension)) = &state {
            Some(Error::new(
                ErrorKind::warn_ext_use(
                    extension.clone(),
                    match &token_kind {
                        Token::TYPE_NAME(TypeName::OTHER(type_name)) => Some(type_name.clone()),
                        _ => unreachable!(),
                    },
                    token.text_range(),
                    location,
                ),
                location,
            ))
        } else {
            None
        };

        (token_kind, state, error)
    }
}

pub trait Tokenizer {
    fn promote_type_name(&mut self, name: TypeNameAtom) -> bool;
}

#[cfg(test)]
mod tests {
    use rowan::NodeOrToken;

    use crate::processor::event::DirectiveKind;

    use super::str::Event;

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
        .tokenize(false, &crate::exts::DEFAULT_REGISTRY);

        #[allow(clippy::while_let_on_iterator)]
        while let Some(result) = tokenizer.next() {
            if let Ok(event) = result {
                match event {
                    Event::Token { token_kind, .. } => {
                        tokens.push(token_kind);
                    }
                    Event::Directive {
                        node,
                        kind: DirectiveKind::Invalid(_),
                        ..
                    } => {
                        // Extract the tokens from the directive parse tree
                        tokens.extend(
                            node.descendants_with_tokens()
                                .filter_map(NodeOrToken::into_token)
                                .map(|token| tokenizer.tokenize_single(&token).0),
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
