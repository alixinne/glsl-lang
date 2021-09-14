use glsl_lang_pp::processor::event::{DirectiveKind, EventDirective};
use glsl_lang_types::ast::{
    self, PreprocessorExtensionBehaviorData, PreprocessorExtensionNameData,
    PreprocessorVersionProfileData,
};
use lang_util::{position::LexerPosition, NodeContent, TextSize};

#[derive(Default, Debug, Clone)]
pub struct Directives {
    directives: Vec<EventDirective>,
}

impl Directives {
    pub fn directives(&self) -> &[EventDirective] {
        &self.directives
    }

    fn get_declaration(directive: &EventDirective) -> Option<ast::ExternalDeclaration> {
        let start = directive.text_range().start();
        let end = directive.text_range().end();

        match directive.kind() {
            DirectiveKind::Version(version) => Some(
                ast::ExternalDeclarationData::Preprocessor(
                    ast::PreprocessorData::Version(
                        ast::PreprocessorVersionData {
                            version: version.number,
                            profile: match version.parsed_profile {
                                None => None,
                                Some(glsl_lang_pp::processor::nodes::VersionProfile::None) => None,
                                Some(glsl_lang_pp::processor::nodes::VersionProfile::Core) => {
                                    Some(PreprocessorVersionProfileData::Core.into())
                                }
                                Some(
                                    glsl_lang_pp::processor::nodes::VersionProfile::Compatibility,
                                ) => Some(PreprocessorVersionProfileData::Compatibility.into()),
                                Some(glsl_lang_pp::processor::nodes::VersionProfile::Es) => {
                                    Some(PreprocessorVersionProfileData::Es.into())
                                }
                            },
                        }
                        .into(),
                    )
                    .spanned(start, end),
                )
                .spanned(start, end),
            ),

            DirectiveKind::Pragma(pragma) => Some(
                ast::ExternalDeclarationData::Preprocessor(
                    ast::PreprocessorData::Pragma(
                        ast::PreprocessorPragmaData {
                            command: pragma.raw().to_owned(),
                        }
                        .into(),
                    )
                    .spanned(start, end),
                )
                .spanned(start, end),
            ),

            DirectiveKind::Extension(extension) => Some(
                ast::ExternalDeclarationData::Preprocessor(
                    ast::PreprocessorData::Extension(
                        ast::PreprocessorExtensionData {
                            name: match extension.name {
                                glsl_lang_pp::processor::nodes::ExtensionName::All => {
                                    PreprocessorExtensionNameData::All
                                }
                                glsl_lang_pp::processor::nodes::ExtensionName::Specific(
                                    ref name,
                                ) => PreprocessorExtensionNameData::Specific(name.as_ref().into()),
                            }
                            .into(),
                            behavior: Some(
                                match extension.behavior {
                                    glsl_lang_pp::processor::nodes::ExtensionBehavior::Require => {
                                        PreprocessorExtensionBehaviorData::Require
                                    }
                                    glsl_lang_pp::processor::nodes::ExtensionBehavior::Enable => {
                                        PreprocessorExtensionBehaviorData::Enable
                                    }
                                    glsl_lang_pp::processor::nodes::ExtensionBehavior::Warn => {
                                        PreprocessorExtensionBehaviorData::Warn
                                    }
                                    glsl_lang_pp::processor::nodes::ExtensionBehavior::Disable => {
                                        PreprocessorExtensionBehaviorData::Disable
                                    }
                                }
                                .into(),
                            ),
                        }
                        .into(),
                    )
                    .spanned(start, end),
                )
                .spanned(start, end),
            ),
            _ => None,
        }
    }

    pub fn inject(mut self, root: &mut ast::TranslationUnit) -> Directives {
        let mut directive_idx = 0;
        let mut declaration_idx = 0;

        // Insert directives
        let mut start = None;
        while declaration_idx < root.0.len() && directive_idx < self.directives.len() {
            // The current declaration. We want to insert directives before this declaration.
            let current_declaration = &root.0[declaration_idx];

            // Find where the range starts
            let actual_start = if let Some(start) = start {
                // This is the end of the previous declaration
                start
            } else if let Some(current_start) = current_declaration.span.map(|span| span.start()) {
                // Start from the beginning of the file
                LexerPosition::new(current_start.source_id, TextSize::default())
            } else {
                // No span information, keep looking
                declaration_idx += 1;
                continue;
            };

            // Find where the current declaration ends
            let end = if let Some(current_end) = current_declaration.span.map(|span| span.end()) {
                current_end
            } else {
                // The current node has no span information, so use the current start
                actual_start
            };

            // Are there any directives to insert before the current declaration_idx?
            while directive_idx < self.directives.len() {
                // The current directive
                let current_directive = &self.directives[directive_idx];
                let span = current_directive.text_range();

                // TODO: What happends to directives from multiple files?
                if span.source_id() == actual_start.source_id {
                    if span.end().offset <= actual_start.offset {
                        if let Some(declaration) = Self::get_declaration(current_directive) {
                            // Add to ast
                            root.0.insert(declaration_idx, declaration);
                            declaration_idx += 1;
                            // Processed, remove from directive list
                            self.directives.remove(directive_idx);
                        } else {
                            // Can't be processed, skip it
                            directive_idx += 1;
                        }
                    } else {
                        // This directive comes later
                        break;
                    }
                }
            }

            // Advance the current declaration
            declaration_idx += 1;

            // Advance the directive range
            start = Some(end);
        }

        // TODO: Check source_id?
        while directive_idx < self.directives.len() {
            if let Some(declaration) = Self::get_declaration(&self.directives[directive_idx]) {
                root.0.push(declaration);
                self.directives.remove(directive_idx);
            } else {
                directive_idx += 1;
            }
        }

        self
    }
}

impl From<Vec<EventDirective>> for Directives {
    fn from(directives: Vec<EventDirective>) -> Self {
        Self { directives }
    }
}

impl From<Directives> for Vec<EventDirective> {
    fn from(directives: Directives) -> Self {
        directives.directives
    }
}
