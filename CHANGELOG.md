## [Unreleased]

- - -
## v0.6.1 - 2024-10-01
#### Bug Fixes
- **(transpiler)** append "precision" for precision decl (#53) - (1596970) - Yorkie Makoto
#### Miscellaneous Chores
- **(version)** v0.6.0 [skip ci] - (222af29) - glsl-lang

- - -

## v0.6.0 - 2024-08-19
#### Continuous Integration
- **(docs)** add permissions for github_token - (2e9f4ca) - *alixinne*
#### Miscellaneous Chores
- manually bump dev-dependencies - (7d74c9b) - alixinne
- add deprecation notice for v1 lexer - (653ffd9) - alixinne

- - -

## v0.6.0-beta.1 - 2024-08-19
#### Bug Fixes
- **(deps)** update rust crate derive_more to v1 - (0315528) - renovate[bot]
- **(deps)** update all non-major dependencies - (65b144f) - renovate[bot]
#### Features
- rename lang_util(as) to lang_util(parser) - (1902047) - alixinne
#### Miscellaneous Chores
- **(cog)** allow releases on next branch - (8a47fde) - *alixinne*
- **(deps)** update softprops/action-gh-release action to v2 - (2c27d68) - renovate[bot]
- **(deps)** update peaceiris/actions-gh-pages action to v4 - (37d2da5) - renovate[bot]
- **(version)** v0.6.0-beta.1 [skip ci] - (9cbd402) - *alixinne*
- **(version)** v0.5.3 [skip ci] - (ef30eef) - glsl-lang
- fix script headers - (d746dcf) - alixinne
- fix clippy warnings - (b0ccc3f) - alixinne

- - -

## v0.6.0-beta.1 - 2024-08-19
#### Bug Fixes
- **(deps)** update rust crate derive_more to v1 - (0315528) - renovate[bot]
- **(deps)** update all non-major dependencies - (65b144f) - renovate[bot]
- **(glsl-lang-quote)** correctly parse #(ident) at line start - (c47f7fc) - *alixinne*
- update github profile information - (2beea1f) - Alix Tavernier
#### Build system
- set resolver to 2 - (73544c2) - alixinne
#### Features
- rename lang_util(as) to lang_util(parser) - (1902047) - alixinne
#### Miscellaneous Chores
- **(cog)** allow releases on next branch - (8a47fde) - *alixinne*
- **(deps)** update softprops/action-gh-release action to v2 - (2c27d68) - renovate[bot]
- **(deps)** update peaceiris/actions-gh-pages action to v4 - (37d2da5) - renovate[bot]
- **(deps)** update rust crate glsl to v7 - (e8782ee) - renovate[bot]
- **(deps)** update actions/cache action to v4 - (76c7bd2) - renovate[bot]
- **(deps)** update actions/checkout action to v4 - (be49ea6) - renovate[bot]
- **(deps)** update actions/cache action to v3 - (b159bfb) - renovate[bot]
- **(version)** v0.5.3 [skip ci] - (ef30eef) - glsl-lang
- **(version)** v0.5.2 [skip ci] - (c0ada55) - glsl-lang
- fix script headers - (d746dcf) - alixinne
- fix clippy warnings - (b0ccc3f) - alixinne
- fix clippy warnings - (c28f583) - alixinne
- bump rust version to 1.74.0 - (23a3c9b) - alixinne

- - -

## v0.5.3 - 2024-07-23
#### Bug Fixes
- **(glsl-lang-quote)** correctly parse #(ident) at line start - (c47f7fc) - *alixinne*
- update github profile information - (2beea1f) - Alix Tavernier
#### Build system
- set resolver to 2 - (73544c2) - alixinne
#### Miscellaneous Chores
- **(deps)** update rust crate glsl to v7 - (e8782ee) - renovate[bot]
- **(deps)** update actions/cache action to v4 - (76c7bd2) - renovate[bot]
- **(deps)** update actions/checkout action to v4 - (be49ea6) - renovate[bot]
- **(deps)** update actions/cache action to v3 - (b159bfb) - renovate[bot]
- **(version)** v0.5.2 [skip ci] - (c0ada55) - glsl-lang
- fix clippy warnings - (c28f583) - alixinne
- bump rust version to 1.74.0 - (23a3c9b) - alixinne

- - -

## v0.5.2 - 2023-11-13
#### Bug Fixes
- **(cli)** actually setup fs preprocessor - (ca89a01) - *alixinne*
- **(lang-lexer/inject)** prevent some #include'd pp. dirs. from being injected at wrong positions - (cbd63d0) - Alejandro González
- **(lang-lexer/inject)** better support injection of pp. dirs. from multiple files - (556fd4b) - Alejandro González
#### Continuous Integration
- **(bumpver)** use --exact - (7b81cdb) - *alixinne*
#### Miscellaneous Chores
- **(deps)** update rust crate lalrpop-util to 0.20.0 - (43c7226) - renovate[bot]
- **(deps)** update rust crate lalrpop to 0.20.0 - (b1801d9) - renovate[bot]
- **(deps)** update cocogitto/cocogitto-action action to v3.5 - (6728065) - renovate[bot]
- **(deps)** drop dependency on `lexical` - (5dc7eb2) - Alejandro González
- **(deps)** add renovate.json - (d73718f) - renovate[bot]
- **(version)** v0.5.1 [skip ci] - (3198b9d) - glsl-lang
- fix dependency updates - (e525715) - alixinne
- fix in-workspace package versions - (254a042) - alixinne
- silence a new Clippy lint, run `cargo fmt` - (8a34d08) - Alejandro González
- fix new Clippy unnecessary allocs lint in `lang-quote` - (50f509d) - Alejandro González
- bump Rust toolchain to 1.70.0 - (db51697) - Alejandro González
#### Tests
- **(multifile-pp)** add tests for #24 - (fba1694) - *alixinne*

- - -

## v0.5.1 - 2023-04-27
#### Bug Fixes
- **(transpiler)** add missing brackets for array expressions - (51a7729) - Alejandro González
#### Miscellaneous Chores
- **(version)** v0.5.0 [skip ci] - (881c0e4) - glsl-lang

- - -

## v0.5.0 - 2023-04-03
#### Bug Fixes
- **(lexer/v2-full)** fix injecting pp. directives after ext. declarations - (a68c630) - Alejandro González
- **(transpiler)** put pp. dirs on their own line no matter what the terminators are - (bb30674) - Alejandro González
- **(transpiler)** correct transpilation of precision qualifiers - (4b059a6) - Alejandro González
#### Build system
- pin rust-toolchain version to 1.68.1 - (d79a36f) - alixinne
#### Continuous Integration
- add skip ci to release commits - (eb57693) - alixinne
- enable build on next branch - (9cb8c4d) - alixinne
- allow clippy::result_large_err - (b8facaa) - alixinne
#### Features
- **(transpiler)** overhaul formatting and customizability - (be86326) - Alejandro González
#### Miscellaneous Chores
- **(deps)** update lalrpop to 0.19.9 - (580cdb1) - *alixinne*
- **(deps)** replace dependency on `lazy_static` by `once_cell` - (7aa176e) - Alejandro González
- add CODEOWNERS file - (3f3fad8) - alixinne
- fix clippy warnings - (ecf75a8) - alixinne
#### Tests
- fix test build failure when not using the v2-full parser - (5b5092a) - Alejandro González

- - -

## v0.4.1 - 2022-09-05
#### Bug Fixes
- **(glsl-lang-types)** do not generate node type aliases via a derive macro (#9) - (b4c75a1) - Alexandre Bléron
#### Continuous Integration
- retry cargo publish (#14) - (3cd8895) - alixinne

- - -

## v0.4.0 - 2022-09-05
#### Bug Fixes
- **(lang-util-dev)** update similar_asserts - (b3e5728) - *alixinne*
- implement Eq when applicable - (f7df6d0) - alixinne
- re-export FileId from glsl_lang::ast (#11) - (7138ef8) - Alexandre Bléron
#### Features
- add GL_KHR_vulkan_glsl types to parser - (12f9515) - Alexandre Bléron

- - -

## v0.3.1 - 2022-07-03
#### Bug Fixes
- **(transpiler)** allow !Sized sinks for transpiler functions - (0385b7e) - Alexandre Bléron
#### Continuous Integration
- use cargo-workspaces - (52f54da) - alixinne

- - -

## v0.3.0 - 2022-06-15
#### Bug Fixes
- update all dependencies to 0.2.1 - (bea4a64) - alixinne
#### Build system
- fix ci/bumpver.sh - (10c6aca) - alixinne
- configure cog - (0aed54d) - alixinne
#### Continuous Integration
- configure cocogitto - (ea369e4) - alixinne
#### Features
- update dependencies - (16020e4) - alixinne
#### Miscellaneous Chores
- update CHANGELOG for cocogitto - (23bcf06) - alixinne

- - -


## [0.2.1] - 2022-06-11
### Changed
- Updated to Rust 2021 edition

### Fixed
- `glsl-lang`: Fix documentation example for v0.2.0

## [0.2.0] - 2022-02-07
### Added
- `lang-util`: Add lang_util::error::ResolvedPosition::without_source_number
- `lang-util`: Add support for serde serializing of node contents
- `lang-util`: Implement AsRef for Node (contents)
- `lang-util`: Implement PartialOrd/Ord for Node (contents)
- `lang-util`: Implement Hash for Node (contents)
- `glsl-lang`: Add `serde` feature for AST serialization
- `glsl-lang-cli`: Add JSON output format
- `glsl-lang-cli`: Add input file argument
- `glsl-lang-cli`: Add GLSL output format
- `glsl-lang`: Implement PartialOrd and Hash for Identifier and TypeName
- `lang-util`: Add NodeSpan::start() and NodeSpan::end()
- `lang-util`: Generate display impl for derive(Token)
- `lang-util`: Add lang_util(as = "...") for mapping to lalrpop parser tokens
- `lang-util`: Add TokenDescription metadata for lexer tokens
- `glsl-lang`: Add location data to all AST nodes

### Changed
- PartialEq for Node now compares the contents of the node by default
- `glsl-lang`: Box large enum variants (FunIdentifierData::TypeSpecifier, ConditionData::Assignment) to reduce size differences

### Removed
- NodeContentEq trait in lang-util
- `lang-util`: dependency on parse-display
- `glsl-lang`: dependency on parse-display

### Fixed
- `#[derive(NodeContent)]` doesn't require importing NodeDisplay anymore

## [0.1.3] - 2021-04-16
### Added
- Add ParseContext::with_policy

## [0.1.2] - 2021-04-06
### Changed
- Updated landing pages for crates in this workspace

### Fixed
- Fix CI scripts behavior
- Fix crates.io categories for `glsl-lang-quote`

## [0.1.1] - 2021-04-05
### Fixed
- Fix READMEs for crates.io

## [0.1.0] - 2021-04-05
- Initial release

[Unreleased]: https://github.com/alixinne/glsl-lang/compare/v0.2.1...HEAD
[0.2.1]: https://github.com/alixinne/glsl-lang/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/alixinne/glsl-lang/compare/v0.1.3...v0.2.0
[0.1.3]: https://github.com/alixinne/glsl-lang/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/alixinne/glsl-lang/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/alixinne/glsl-lang/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/alixinne/glsl-lang/releases/tag/v0.1.0
