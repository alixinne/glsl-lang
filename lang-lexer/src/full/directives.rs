use std::cmp;

use glsl_lang_pp::processor::{
    event::{DirectiveKind, EventDirective},
    nodes::VersionProfile,
};
use glsl_lang_types::ast::{
    self, PreprocessorExtensionBehaviorData, PreprocessorExtensionNameData,
    PreprocessorVersionProfileData,
};
use lang_util::NodeContent;

#[derive(Default, Debug, Clone)]
pub struct Directives {
    directives: Vec<EventDirective>,
}

impl Directives {
    pub fn directives(&self) -> &[EventDirective] {
        &self.directives
    }

    fn get_declaration(
        directive: &EventDirective,
        highest_version: &mut Option<(u16, Option<VersionProfile>)>,
    ) -> Option<ast::ExternalDeclaration> {
        let start = directive.text_range().start();
        let end = directive.text_range().end();

        match directive.kind() {
            DirectiveKind::Version(version) => {
                let version_number = version.number;
                let profile = version.parsed_profile;

                let had_highest_version;
                let (highest_version_number, highest_profile) = match highest_version {
                    Some(version_data) => {
                        had_highest_version = true;
                        version_data
                    }
                    None => {
                        had_highest_version = false;
                        highest_version.insert((version_number, profile))
                    }
                };

                *highest_version_number = cmp::max(version_number, *highest_version_number);
                *highest_profile = cmp::max(
                    // Wrap in Some to avoid OpenGL ES being considered higher than no profile (i.e., core profile)
                    Some(profile.unwrap_or(VersionProfile::None)),
                    *highest_profile,
                );

                (!had_highest_version).then_some(
                    ast::ExternalDeclarationData::Preprocessor(
                        ast::PreprocessorData::Version(
                            ast::PreprocessorVersionData {
                                version: version_number,
                                profile: match profile {
                                    None | Some(VersionProfile::None) => None,
                                    Some(VersionProfile::Core) => {
                                        Some(PreprocessorVersionProfileData::Core.into())
                                    }
                                    Some(VersionProfile::Compatibility) => {
                                        Some(PreprocessorVersionProfileData::Compatibility.into())
                                    }
                                    Some(VersionProfile::Es) => {
                                        Some(PreprocessorVersionProfileData::Es.into())
                                    }
                                },
                            }
                            .into(),
                        )
                        .spanned(start, end),
                    )
                    .spanned(start, end),
                )
            }

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
        let mut highest_version = None;
        let mut version_directive_declaration_idx = None;

        // Insert directives
        let mut start = None;
        while declaration_idx < root.0.len() && directive_idx < self.directives.len() {
            // The current declaration. We want to insert directives before this declaration.
            let current_declaration = &root.0[declaration_idx];

            // Find where the range starts
            let actual_start =
                if let Some(current_start) = current_declaration.span.map(|span| span.start()) {
                    current_start
                } else if let Some(start) = start {
                    // This is the end of the previous declaration
                    start
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

                // For directives in #include'd files, a previous #include directive event prevents
                // inserting them too soon in the top-level file
                if span.end().offset <= actual_start.offset
                    || actual_start.source_id != span.source_id()
                {
                    if let Some(declaration) =
                        Self::get_declaration(current_directive, &mut highest_version)
                    {
                        // Add to ast
                        root.0.insert(declaration_idx, declaration);

                        if matches!(current_directive.kind(), DirectiveKind::Version(_)) {
                            version_directive_declaration_idx.get_or_insert(declaration_idx);
                        }

                        declaration_idx += 1;

                        // Processed, remove from directive list
                        self.directives.remove(directive_idx);
                    } else {
                        // Can't/shouldn't be processed, skip it
                        directive_idx += 1;
                    }
                } else {
                    // This directive comes later
                    break;
                }
            }

            // Advance the current declaration
            declaration_idx += 1;

            // Advance the directive range
            start = Some(end);
        }

        // Append any remaining directives
        while directive_idx < self.directives.len() {
            let current_directive = &self.directives[directive_idx];

            if let Some(declaration) =
                Self::get_declaration(current_directive, &mut highest_version)
            {
                root.0.push(declaration);

                if matches!(current_directive.kind(), DirectiveKind::Version(_)) {
                    version_directive_declaration_idx.get_or_insert(root.0.len() - 1);
                }

                self.directives.remove(directive_idx);
            } else {
                directive_idx += 1;
            }
        }

        // Set the version directive value to the highest (i.e., the one that requires the most
        // OpenGL features) that was found
        if let (
            Some(version_directive_declaration_idx),
            Some((highest_version_number, highest_profile)),
        ) = (version_directive_declaration_idx, highest_version)
        {
            if let ast::ExternalDeclarationData::Preprocessor(preprocessor_data) =
                &mut root.0[version_directive_declaration_idx].content
            {
                if let ast::PreprocessorData::Version(preprocessor_version) =
                    &mut preprocessor_data.content
                {
                    preprocessor_version.version = highest_version_number;
                    preprocessor_version.profile = match highest_profile {
                        None | Some(VersionProfile::None) => None,
                        Some(VersionProfile::Core) => {
                            Some(PreprocessorVersionProfileData::Core.into())
                        }
                        Some(VersionProfile::Compatibility) => {
                            Some(PreprocessorVersionProfileData::Compatibility.into())
                        }
                        Some(VersionProfile::Es) => Some(PreprocessorVersionProfileData::Es.into()),
                    };
                }
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
