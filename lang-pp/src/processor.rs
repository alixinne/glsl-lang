use std::{array::IntoIter, collections::HashMap, rc::Rc};

use smol_str::SmolStr;

use lang_util::FileId;

mod definition;
use definition::Definition;

pub mod event;

pub mod expand;

mod expr;

pub mod fs;

pub mod nodes;
use nodes::{Define, DefineObject, Extension, ExtensionName, Version};

use crate::processor::nodes::ExtensionBehavior;

pub mod str;

/// Operating mode for #include directives
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeMode {
    /// No #include directives are allowed
    None,
    /// GL_ARB_shading_language_include runtime includes
    ArbInclude { warn: bool },
    /// GL_GOOGLE_include_directive compile-time includes
    GoogleInclude { warn: bool },
}

impl IncludeMode {
    pub fn warn(self) -> bool {
        match self {
            IncludeMode::None => false,
            IncludeMode::ArbInclude { warn } | IncludeMode::GoogleInclude { warn } => warn,
        }
    }
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
    cpp_style_line: bool,
}

// TODO: Add builder/constructor

impl ProcessorState {
    pub fn get_definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }

    // TODO: Return a proper error type?
    pub fn definition(&mut self, definition: Define, file_id: FileId) -> bool {
        let entry = self.definitions.entry(definition.name().into());

        match entry {
            std::collections::hash_map::Entry::Occupied(mut occupied) => {
                if occupied.get().protected() {
                    false
                } else {
                    occupied.insert(Definition::Regular(Rc::new(definition), file_id));
                    true
                }
            }
            std::collections::hash_map::Entry::Vacant(vacant) => {
                vacant.insert(Definition::Regular(Rc::new(definition), file_id));
                true
            }
        }
    }

    pub fn extension(&mut self, extension: &nodes::Extension) {
        // Push onto the stack
        self.extension_stack.push(extension.clone());

        // Process include extensions
        let target_include_mode = if extension.name == ext_name!("GL_ARB_shading_language_include")
        {
            Some(IncludeMode::ArbInclude {
                warn: extension.behavior == ExtensionBehavior::Warn,
            })
        } else if extension.name == ext_name!("GL_GOOGLE_include_directive") {
            Some(IncludeMode::GoogleInclude {
                warn: extension.behavior == ExtensionBehavior::Warn,
            })
        } else {
            None
        };

        if let Some(target) = target_include_mode {
            if extension.behavior.is_active() {
                self.include_mode = target;

                // GL_GOOGLE_include_directive enable GL_GOOGLE_cpp_style_line
                if let IncludeMode::GoogleInclude { .. } = target {
                    self.cpp_style_line = true;
                }
            } else {
                // TODO: Implement current mode as a stack?
                self.include_mode = IncludeMode::None;
            }
        }

        // Process others
        if extension.name == ext_name!("GL_GOOGLE_cpp_style_line_directive") {
            if extension.behavior.is_active() {
                self.cpp_style_line = true;
            } else {
                // TODO: Notify instead of silently ignoring?
                if !matches!(self.include_mode, IncludeMode::GoogleInclude { .. }) {
                    self.cpp_style_line = false;
                }
            }
        }
    }

    pub fn cpp_style_line(&self) -> bool {
        self.cpp_style_line
    }
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
            definitions: IntoIter::new([
                Definition::Regular(
                    Rc::new(Define::object(
                        "GL_core_profile".into(),
                        DefineObject::one(),
                        true,
                    )),
                    FileId::default(),
                ),
                Definition::Line,
                Definition::File,
                Definition::Version,
            ])
            .chain(crate::exts::DEFAULT_REGISTRY.all().map(|spec| {
                Definition::Regular(
                    Rc::new(Define::object(
                        spec.name().as_ref().into(),
                        DefineObject::one(),
                        true,
                    )),
                    FileId::default(),
                )
            }))
            .map(|definition| (definition.name().into(), definition))
            .collect(),
            version: Version::default(),
            cpp_style_line: false,
        }
    }
}
