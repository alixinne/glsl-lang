# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `lang-util`: Add lang_util::error::ResolvedPosition::without_source_number
- `lang-util`: Add support for serde serializing of node contents
- `lang-util`: Implement AsRef for Node (contents)
- `lang-util`: Implement PartialOrd/Ord for Node (contents)
- `lang-util`: Implement Hash for Node (contents)

### Changed
- PartialEq for Node now compares the contents of the node by default

### Removed
- NodeContentEq trait in lang-util

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

[Unreleased]: https://github.com/vtavernier/glsl-lang/compare/v0.1.3...HEAD
[0.1.3]: https://github.com/vtavernier/glsl-lang/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/vtavernier/glsl-lang/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/vtavernier/glsl-lang/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/vtavernier/glsl-lang/releases/tag/v0.1.0
