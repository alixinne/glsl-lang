use std::collections::HashMap;

use crate::last::type_names::TypeNameAtom;

#[macro_use]
pub mod names;

use names::ExtNameAtom;

pub struct ExtensionSpec {
    name: ExtNameAtom,
    type_names: Vec<TypeNameAtom>,
}

impl ExtensionSpec {
    pub fn name(&self) -> &ExtNameAtom {
        &self.name
    }

    pub fn type_names(&self) -> &[TypeNameAtom] {
        &self.type_names
    }
}

// TODO: Fill registry with extension data from Khronos

#[derive(Default)]
pub struct Registry {
    extensions: HashMap<ExtNameAtom, ExtensionSpec>,
}

impl Registry {
    pub fn all(&self) -> impl Iterator<Item = &ExtensionSpec> {
        self.extensions.values()
    }

    pub fn get(&self, name: &ExtNameAtom) -> Option<&ExtensionSpec> {
        self.extensions.get(name)
    }
}

lazy_static::lazy_static! {
    pub static ref DEFAULT_REGISTRY: Registry = Registry::default();
}
