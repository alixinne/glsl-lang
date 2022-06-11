use std::collections::HashMap;

use lang_util::{FileId, SmolStr};

mod definition;
use definition::Definition;

pub mod event;

pub mod expand;

mod expr;

pub mod fs;

pub mod nodes;
use nodes::{Define, DefineObject, Version};

use crate::{
    exts::Registry,
    processor::nodes::{ExtensionBehavior, ExtensionName},
};

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessorState {
    include_mode: IncludeMode,
    definitions: HashMap<SmolStr, Definition>,
    version: Version,
    cpp_style_line: bool,
}

impl ProcessorState {
    pub fn builder() -> ProcessorStateBuilder<'static> {
        ProcessorStateBuilder::default()
    }

    fn get_definition(&self, name: &str) -> Option<&Definition> {
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
                    occupied.insert(Definition::Regular(definition.into(), file_id));
                    true
                }
            }
            std::collections::hash_map::Entry::Vacant(vacant) => {
                vacant.insert(Definition::Regular(definition.into(), file_id));
                true
            }
        }
    }

    fn add_extension(&mut self, name: &ExtensionName, behavior: ExtensionBehavior) {
        // Process include extensions
        let target_include_mode = if *name == ext_name!("GL_ARB_shading_language_include") {
            Some(IncludeMode::ArbInclude {
                warn: behavior == ExtensionBehavior::Warn,
            })
        } else if *name == ext_name!("GL_GOOGLE_include_directive") {
            Some(IncludeMode::GoogleInclude {
                warn: behavior == ExtensionBehavior::Warn,
            })
        } else {
            None
        };

        if let Some(target) = target_include_mode {
            if behavior.is_active() {
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
        if *name == ext_name!("GL_GOOGLE_cpp_style_line_directive") {
            if behavior.is_active() {
                self.cpp_style_line = true;
            } else {
                // TODO: Notify instead of silently ignoring?
                if !matches!(self.include_mode, IncludeMode::GoogleInclude { .. }) {
                    self.cpp_style_line = false;
                }
            }
        }
    }

    fn extension(&mut self, extension: &nodes::Extension) {
        self.add_extension(&extension.name, extension.behavior);
    }

    fn cpp_style_line(&self) -> bool {
        self.cpp_style_line
    }
}

impl Default for ProcessorState {
    fn default() -> Self {
        ProcessorStateBuilder::default().finish()
    }
}

#[derive(Clone)]
pub struct ProcessorStateBuilder<'r> {
    core_profile: bool,
    compatibility_profile: bool,
    es_profile: bool,
    extensions: Vec<(ExtensionName, ExtensionBehavior)>,
    definitions: Vec<Define>,
    registry: &'r Registry,
}

impl<'r> ProcessorStateBuilder<'r> {
    pub fn new(registry: &'r Registry) -> Self {
        let default = ProcessorStateBuilder::default();
        Self {
            registry,
            ..default
        }
    }

    pub fn registry<'s>(self, registry: &'s Registry) -> ProcessorStateBuilder<'s> {
        ProcessorStateBuilder::<'s> {
            registry,
            core_profile: self.core_profile,
            compatibility_profile: self.compatibility_profile,
            es_profile: self.es_profile,
            extensions: self.extensions,
            definitions: self.definitions,
        }
    }

    pub fn core_profile(self, core_profile: bool) -> Self {
        Self {
            core_profile,
            ..self
        }
    }

    pub fn compatibility_profile(self, compatibility_profile: bool) -> Self {
        Self {
            compatibility_profile,
            ..self
        }
    }

    pub fn es_profile(self, es_profile: bool) -> Self {
        Self { es_profile, ..self }
    }

    pub fn extension(
        mut self,
        name: impl Into<ExtensionName>,
        behavior: impl Into<ExtensionBehavior>,
    ) -> Self {
        self.extensions.push((name.into(), behavior.into()));
        self
    }

    pub fn definition(mut self, definition: impl Into<Define>) -> Self {
        self.definitions.push(definition.into());
        self
    }

    pub fn finish(self) -> ProcessorState {
        let one = DefineObject::one();

        let mut state = ProcessorState {
            // No #include extensions enabled
            include_mode: IncludeMode::None,
            // Spec 3.3, "There is a built-in macro definition for each profile the implementation
            // supports. All implementations provide the following macro:
            // `#define GL_core_profile 1`
            definitions: self
                .core_profile
                .then(|| Define::object("GL_core_profile".into(), one.clone(), true))
                .into_iter()
                .chain(
                    self.compatibility_profile
                        .then(|| {
                            Define::object("GL_compatibility_profile".into(), one.clone(), true)
                        })
                        .into_iter(),
                )
                .chain(
                    self.es_profile
                        .then(|| Define::object("GL_es_profile".into(), one.clone(), true))
                        .into_iter(),
                )
                .chain(self.definitions.into_iter())
                .map(|definition| Definition::Regular(definition.into(), FileId::default()))
                .chain([Definition::Line, Definition::File, Definition::Version].into_iter())
                .chain(self.registry.all().map(|spec| {
                    Definition::Regular(
                        Define::object(spec.name().as_ref().into(), one.clone(), true).into(),
                        FileId::default(),
                    )
                }))
                .map(|definition| (definition.name().into(), definition))
                .collect(),
            version: Version::default(),
            cpp_style_line: false,
        };

        for (name, behavior) in self.extensions {
            state.add_extension(&name, behavior);
        }

        state
    }
}

impl From<ProcessorStateBuilder<'_>> for ProcessorState {
    fn from(builder: ProcessorStateBuilder<'_>) -> Self {
        builder.finish()
    }
}

impl Default for ProcessorStateBuilder<'static> {
    fn default() -> Self {
        Self {
            core_profile: true,
            compatibility_profile: false,
            es_profile: false,
            extensions: Default::default(),
            definitions: Default::default(),
            registry: &*crate::exts::DEFAULT_REGISTRY,
        }
    }
}
