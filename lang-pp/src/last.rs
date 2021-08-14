//! Last preprocessing stage definitions

#[macro_use]
pub mod keywords;
#[macro_use]
pub mod type_names;

pub mod fs;

pub mod str;

mod token;
use std::collections::HashMap;

pub use token::{Token, TypeName};

use type_names::TypeNameAtom;

use crate::{
    exts::{names::ExtNameAtom, ExtensionSpec, Registry},
    processor::{
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
}

#[derive(Debug, Clone)]
enum TypeNameState {
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
    fn new(registry: &'r Registry) -> Self {
        Self {
            type_names: Default::default(),
            extensions: Default::default(),
            registry,
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

    fn handle_extension(&mut self, extension: &Extension) {
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
                    // TODO: Handle extension not found for require
                }
            }
        }
    }
}

pub trait Tokenizer {
    fn promote_type_name(&mut self, name: TypeNameAtom) -> bool;
}
