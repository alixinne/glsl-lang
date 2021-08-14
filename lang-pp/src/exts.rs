use std::{array::IntoIter, collections::HashMap};

use crate::last::type_names::TypeNameAtom;

#[macro_use]
pub mod names;

use names::ExtNameAtom;

pub struct ExtensionSpec {
    name: ExtNameAtom,
    type_names: Vec<TypeNameAtom>,
}

impl ExtensionSpec {
    pub fn new(name: ExtNameAtom, type_names: Vec<TypeNameAtom>) -> Self {
        Self { name, type_names }
    }

    pub fn name(&self) -> &ExtNameAtom {
        &self.name
    }

    pub fn type_names(&self) -> &[TypeNameAtom] {
        &self.type_names
    }
}

pub struct Registry {
    extensions: HashMap<ExtNameAtom, ExtensionSpec>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            extensions: Default::default(),
        }
    }

    pub fn all(&self) -> impl Iterator<Item = &ExtensionSpec> {
        self.extensions.values()
    }

    pub fn get(&self, name: &ExtNameAtom) -> Option<&ExtensionSpec> {
        self.extensions.get(name)
    }
}

// TODO: Fill registry with extension data from Khronos

impl Default for Registry {
    fn default() -> Self {
        Self {
            extensions: IntoIter::new([
                ExtensionSpec::new(
                    ExtNameAtom::from("GL_OES_EGL_image_external"),
                    vec![TypeNameAtom::from("samplerExternalOES")],
                ),
                ExtensionSpec::new(
                    ExtNameAtom::from("GL_OES_texture_buffer"),
                    vec![
                        TypeNameAtom::from("samplerBuffer"),
                        TypeNameAtom::from("isamplerBuffer"),
                        TypeNameAtom::from("usamplerBuffer"),
                        TypeNameAtom::from("imageBuffer"),
                        TypeNameAtom::from("iimageBuffer"),
                        TypeNameAtom::from("uimageBuffer"),
                    ],
                ),
            ])
            .map(|spec| (spec.name.clone(), spec))
            .collect(),
        }
    }
}

lazy_static::lazy_static! {
    pub static ref DEFAULT_REGISTRY: Registry = Registry::default();
    pub static ref EMPTY_REGISTRY: Registry = Registry::new();
}
