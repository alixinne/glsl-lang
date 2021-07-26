use std::{array::IntoIter, collections::HashMap, iter::FromIterator, rc::Rc};

use smol_str::SmolStr;

use lang_util::FileId;

#[macro_use]
pub mod exts;

mod definition;
use definition::Definition;

pub mod event;

pub mod expand;

pub mod fs;

pub mod nodes;
use nodes::{Define, DefineObject, Extension, ExtensionName, Version};

/// Operating mode for #include directives
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeMode {
    /// No #include directives are allowed
    None,
    /// GL_ARB_shading_language_include runtime includes
    ArbInclude,
    /// GL_GOOGLE_include_directive compile-time includes
    GoogleInclude,
}

impl Default for IncludeMode {
    fn default() -> Self {
        Self::None
    }
}

/// Current state of the preprocessor
#[derive(Debug, Clone)]
pub struct ProcessorState {
    extension_stack: Vec<Extension>,
    include_mode: IncludeMode,
    // use Rc to make cloning the whole struct cheaper
    definitions: HashMap<SmolStr, Definition>,
    version: Version,
}

impl Default for ProcessorState {
    fn default() -> Self {
        Self {
            // Spec 3.3, "The initial state of the compiler is as if the directive
            // `#extension all : disable` was issued
            extension_stack: vec![Extension::disable(ExtensionName::All)],
            // No #include extensions enabled
            include_mode: IncludeMode::None,
            // Spec 3.3, "There is a built-in macro definition for each profile the implementation
            // supports. All implementations provide the following macro:
            // `#define GL_core_profile 1`
            definitions: HashMap::from_iter(
                IntoIter::new([
                    Definition::Regular(
                        Rc::new(Define::object(
                            "GL_core_profile".into(),
                            DefineObject::from_str("1").unwrap(),
                            true,
                        )),
                        FileId::default(),
                    ),
                    Definition::Line,
                    Definition::File,
                    Definition::Version,
                ])
                .map(|definition| (definition.name().into(), definition)),
            ),
            version: Version::default(),
        }
    }
}
