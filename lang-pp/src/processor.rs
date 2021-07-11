use std::{
    collections::{HashMap, VecDeque},
    convert::{TryFrom, TryInto},
    iter::{FromIterator, FusedIterator},
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use arrayvec::ArrayVec;
use rowan::NodeOrToken;
use smol_str::SmolStr;
use string_cache::Atom;
use thiserror::Error;

use crate::{
    parser::{
        Parser,
        SyntaxKind::{self, *},
        SyntaxNode, SyntaxToken,
    },
    unescape_line_continuations, Ast, FileId,
};

#[allow(dead_code)]
#[macro_use]
pub mod exts {
    include!(concat!(env!("OUT_DIR"), "/ext_names.rs"));
}

#[derive(Debug, Clone)]
pub struct Directive<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> {
    node: SyntaxNode,
    inner: I,
}

impl<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> Directive<I> {
    pub fn into_inner(self) -> (I, SyntaxNode) {
        (self.inner, self.node)
    }
}

impl<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> TryFrom<SyntaxNode> for Directive<I> {
    type Error = (I::Error, SyntaxNode);

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        match I::try_from(value.clone()) {
            Ok(inner) => Ok(Self { inner, node: value }),
            Err(err) => Err((err, value)),
        }
    }
}

impl<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> std::ops::Deref for Directive<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub type DirectiveResult<I> = Result<Directive<I>, <Directive<I> as TryFrom<SyntaxNode>>::Error>;

pub trait DirectiveExt: Sized {
    fn into_node(self) -> SyntaxNode;
}

impl<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> DirectiveExt for DirectiveResult<I> {
    fn into_node(self) -> SyntaxNode {
        match self {
            Ok(directive) => directive.into_inner().1,
            Err(error) => error.1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version {
    pub number: u16,
    pub profile: VersionProfile,
}

impl Default for Version {
    fn default() -> Self {
        // Spec 3.3: shaders that do not include a #version directive will be treated as targeting
        // version 1.10
        Self {
            number: 110,
            profile: VersionProfile::None,
        }
    }
}

#[derive(Debug, Error)]
pub enum VersionError {
    #[error("missing version number in #version directive")]
    MissingVersionNumber,
    #[error("invalid version number in #version directive")]
    InvalidVersionNumber(#[from] lexical::Error),
    #[error("unsupported version number in #version directive")]
    UnsupportedVersionNumber,
    #[error("invalid version profile")]
    InvalidVersionProfile { version_number: u16 },
    #[error("cannot specify a profile")]
    ProfileUnsupported { version_number: u16 },
    #[error("es profile is required")]
    EsProfileRequired { version_number: u16 },
}

const VALID_VERSION_NUMBERS: [u16; 17] = [
    100, 110, 120, 130, 140, 150, 300, 310, 320, 330, 400, 410, 420, 430, 440, 450, 460,
];

impl TryFrom<SyntaxNode> for Version {
    type Error = VersionError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        // Parse version number
        // TODO: Replace use of lexical with glsl parser
        let version_number: u16 = value
            .children()
            .find_map(|token| {
                if token.kind() == PP_VERSION_NUMBER {
                    token.first_token()
                } else {
                    None
                }
            })
            .ok_or_else(|| Self::Error::MissingVersionNumber)
            .and_then(|token| {
                lexical::parse(unescape_line_continuations(token.text()).as_ref())
                    .map_err(Into::into)
            })?;

        // Check the version number is supported
        VALID_VERSION_NUMBERS
            .binary_search(&version_number)
            .map_err(|_| Self::Error::UnsupportedVersionNumber)?;

        // Find the profile
        let profile = if let Some(profile) = value.children().find_map(|token| {
            if token.kind() == PP_VERSION_PROFILE {
                token.first_token()
            } else {
                None
            }
        }) {
            unescape_line_continuations(profile.text())
                .parse()
                .map_err(|_| Self::Error::InvalidVersionProfile { version_number })?
        } else {
            VersionProfile::None
        };

        // A profile argument can only be used with version 150 or greater.
        if version_number < 150 && profile != VersionProfile::None {
            return Err(Self::Error::ProfileUnsupported { version_number });
        }

        // If version 300 or 310 is specified, the profile argument is not optional and must be es,
        // or a compile-time error results
        if (version_number == 300 || version_number == 310) && profile != VersionProfile::Es {
            return Err(Self::Error::EsProfileRequired { version_number });
        }

        if version_number == 100 || version_number == 300 || version_number == 310 {
            // Shaders that specify #version 100 will be treated as targeting version 1.00 of the
            // OpenGL ES Shading Language. Shaders that specify #version 300 will be treated as
            // targeting version 3.00 of the OpenGL ES Shading Language. Shaders that specify
            // #version 310 will be treated as targeting version 3.10 of the OpenGL ES Shading
            // Language.

            Ok(Self {
                number: version_number,
                profile: VersionProfile::Es,
            })
        } else if version_number >= 150 {
            // If no profile argument is provided and the version is 150 or greater, the default is
            // core.

            Ok(Self {
                number: version_number,
                profile: if profile == VersionProfile::None {
                    VersionProfile::Core
                } else {
                    profile
                },
            })
        } else {
            Ok(Self {
                number: version_number,
                profile,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VersionProfile {
    None,
    Core,
    Compatibility,
    Es,
}

impl FromStr for VersionProfile {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "core" => Self::Core,
            "compatibility" => Self::Compatibility,
            "es" => Self::Es,
            _ => {
                return Err(());
            }
        })
    }
}

/// Extension behavior
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionBehavior {
    Require,
    Enable,
    Warn,
    Disable,
}

impl ExtensionBehavior {
    pub fn is_active(&self) -> bool {
        matches!(
            self,
            ExtensionBehavior::Require | ExtensionBehavior::Enable | ExtensionBehavior::Warn
        )
    }
}

impl FromStr for ExtensionBehavior {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "require" => Self::Require,
            "enable" => Self::Enable,
            "warn" => Self::Warn,
            "disable" => Self::Disable,
            _ => {
                return Err(());
            }
        })
    }
}

/// Extension name
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtensionName {
    /// All extensions
    All,
    /// Specific extension
    Specific(Atom<exts::ExtNameAtomStaticSet>),
}

impl ExtensionName {
    pub fn new(name: &str) -> Self {
        if name == "all" {
            Self::All
        } else {
            Self::Specific(Atom::from(name))
        }
    }
}

lazy_static::lazy_static! {
    static ref GL_ARB_SHADING_LANGUAGE_INCLUDE: ExtensionName = ExtensionName::Specific(ext_name!("GL_ARB_shading_language_include"));
    static ref GL_GOOGLE_INCLUDE_DIRECTIVE: ExtensionName = ExtensionName::Specific(ext_name!("GL_GOOGLE_include_directive"));
}

#[derive(Debug, Clone)]
pub struct Extension {
    pub name: ExtensionName,
    pub behavior: ExtensionBehavior,
}

impl Extension {
    pub fn require(name: ExtensionName) -> Self {
        Self {
            name,
            behavior: ExtensionBehavior::Require,
        }
    }

    pub fn enable(name: ExtensionName) -> Self {
        Self {
            name,
            behavior: ExtensionBehavior::Enable,
        }
    }

    pub fn warn(name: ExtensionName) -> Self {
        Self {
            name,
            behavior: ExtensionBehavior::Warn,
        }
    }

    pub fn disable(name: ExtensionName) -> Self {
        Self {
            name,
            behavior: ExtensionBehavior::Disable,
        }
    }
}

#[derive(Debug, Error)]
pub enum ExtensionError {
    #[error("missing extension name in #extension directive")]
    MissingExtensionName,
    #[error("missing extension behavior in #extension directive")]
    MissingExtensionBehavior { name: ExtensionName },
    #[error("invalid extension behavior in #extension directive")]
    InvalidExtensionBehavior { name: ExtensionName },
    #[error("invalid behavior in #extension all directive")]
    InvalidAllBehavior { behavior: ExtensionBehavior },
}

impl TryFrom<SyntaxNode> for Extension {
    type Error = ExtensionError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        // Collect identifiers
        let idents: ArrayVec<_, 2> = value
            .children_with_tokens()
            .filter_map(|node_or_token| {
                if let NodeOrToken::Token(token) = node_or_token {
                    if token.kind() == IDENT_KW {
                        Some(token)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .skip(1)
            .take(2)
            .collect();

        let name = idents
            .get(0)
            .ok_or_else(|| Self::Error::MissingExtensionName)
            .map(|name| ExtensionName::new(unescape_line_continuations(name.text()).as_ref()))?;

        let behavior = idents
            .get(1)
            .ok_or_else(|| Self::Error::MissingExtensionBehavior { name: name.clone() })
            .and_then(|behavior| {
                ExtensionBehavior::from_str(unescape_line_continuations(behavior.text()).as_ref())
                    .map_err(|_| Self::Error::InvalidExtensionBehavior { name: name.clone() })
            })?;

        if name == ExtensionName::All {
            if behavior != ExtensionBehavior::Warn && behavior != ExtensionBehavior::Disable {
                return Err(Self::Error::InvalidAllBehavior { behavior });
            }
        }

        Ok(Self { name, behavior })
    }
}

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

#[derive(Debug, Clone)]
pub struct DefineObject {
    tokens: SyntaxNode,
}

impl DefineObject {
    pub fn from_str(input: &str) -> Option<Self> {
        Some(Self {
            tokens: crate::parser::Parser::new(input).parse_define_body()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct DefineFunction {
    args: Vec<SmolStr>,
    tokens: SyntaxNode,
}

#[derive(Debug, Clone)]
pub enum DefineKind {
    Object(DefineObject),
    Function(DefineFunction),
}

/// A preprocessor definition
#[derive(Debug, Clone)]
pub struct Define {
    /// Name of this definition
    name: SmolStr,
    /// Type of this definition
    kind: DefineKind,
    /// true if this definition can't be #undef-ined
    protected: bool,
}

impl Define {
    pub fn object(name: SmolStr, object: DefineObject, protected: bool) -> Self {
        Self {
            name,
            kind: DefineKind::Object(object),
            protected,
        }
    }

    pub fn function(name: SmolStr, function: DefineFunction, protected: bool) -> Self {
        Self {
            name,
            kind: DefineKind::Function(function),
            protected,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn kind(&self) -> &DefineKind {
        &self.kind
    }

    pub fn protected(&self) -> bool {
        self.protected
    }
}

/// Current state of the preprocessor
#[derive(Debug, Clone)]
pub struct ProcessorState {
    extension_stack: Vec<Extension>,
    include_mode: IncludeMode,
    // use Rc to make cloning the whole struct cheaper
    definitions: HashMap<SmolStr, Rc<Define>>,
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
            definitions: HashMap::from_iter([(
                "GL_core_profile".into(),
                Rc::new(Define::object(
                    "GL_core_profile".into(),
                    DefineObject::from_str("1").unwrap(),
                    true,
                )),
            )]),
            version: Version::default(),
        }
    }
}

pub trait FileSystem {
    type Error: std::error::Error;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error>;
    fn read(&self, path: &Path) -> Result<String, Self::Error>;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Std;

impl FileSystem for Std {
    type Error = std::io::Error;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error> {
        std::fs::canonicalize(path)
    }

    fn read(&self, path: &Path) -> Result<String, Self::Error> {
        std::fs::read_to_string(path)
    }
}

pub type StdProcessor = Processor<Std>;

/// Preprocessor
#[derive(Debug)]
pub struct Processor<F: FileSystem> {
    /// Cache of parsed files (preprocessor token sequences)
    file_cache: HashMap<FileId, Ast>,
    /// Mapping from canonical paths to FileIds
    file_ids: HashMap<PathBuf, FileId>,
    /// Mapping from #include/input paths to canonical paths
    canonical_paths: HashMap<PathBuf, PathBuf>,
    /// Current state of the preprocessor
    current_state: ProcessorState,
    /// Filesystem abstraction
    fs: F,
}

impl<F: FileSystem + Default> Processor<F> {
    pub fn new(initial_state: ProcessorState) -> Self {
        Self {
            current_state: initial_state,
            ..Default::default()
        }
    }
}

impl<F: FileSystem> Processor<F> {
    pub fn new_with_fs(initial_state: ProcessorState, fs: F) -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: HashMap::with_capacity(1),
            canonical_paths: HashMap::with_capacity(1),
            current_state: initial_state,
            fs,
        }
    }

    pub fn process(&mut self, entry: &Path) -> ProcessorEvents<F> {
        ProcessorEvents {
            processor: Some(self),
            file_stack: vec![entry.to_owned()],
            event_buf: Default::default(),
        }
    }

    fn parse(&mut self, path: &Path) -> Result<(FileId, &Ast), F::Error> {
        // Find the canonical path
        let canonical_path = if let Some(canonical_path) = self.canonical_paths.get(path) {
            canonical_path
        } else {
            let canonical_path = self.fs.canonicalize(&path)?;
            self.canonical_paths
                .entry(path.to_owned())
                .or_insert(canonical_path)
        };

        // Allocate a file id
        let file_id = if let Some(file_id) = self.file_ids.get(canonical_path.as_path()) {
            file_id
        } else {
            let file_id = FileId::new(self.file_ids.len() as _);
            self.file_ids
                .entry(canonical_path.to_owned())
                .or_insert(file_id)
        };

        if !self.file_cache.contains_key(file_id) {
            // Read the file
            let input = self.fs.read(&canonical_path)?;
            // Parse it
            let ast = Parser::new(&input).parse();
            self.file_cache.insert(*file_id, ast);
        }

        // Return the parsed result
        Ok(self
            .file_cache
            .get_key_value(&file_id)
            .map(|(&k, v)| (k, v))
            .unwrap())
    }

    fn expand(&mut self, ast: Ast) -> Vec<Event<F::Error>> {
        let (root, mut errors) = ast.into_inner();

        // TODO: Smarter capacity calculation
        let mut result = Vec::with_capacity(1024);

        for node_or_token in root.children_with_tokens() {
            if let Some(first) = errors.first() {
                if node_or_token.text_range().end() >= first.pos().start() {
                    result.push(Event::ParseError(errors.pop().unwrap()));
                }
            }

            match node_or_token {
                rowan::NodeOrToken::Node(node) => {
                    match node.kind() {
                        PP_EMPTY => {
                            // Discard
                        }
                        PP_VERSION => {
                            // TODO: Check that the version is the first thing in the file?

                            let directive: DirectiveResult<Version> = node.try_into();

                            if let Ok(version) = &directive {
                                self.current_state.version = **version;
                            }

                            result.push(Event::Version { directive });
                        }
                        PP_EXTENSION => {
                            let directive: DirectiveResult<Extension> = node.try_into();

                            if let Ok(extension) = &directive {
                                // Push onto the stack
                                self.current_state
                                    .extension_stack
                                    .push((**extension).clone());

                                let target_include_mode =
                                    if extension.name == *GL_ARB_SHADING_LANGUAGE_INCLUDE {
                                        Some(IncludeMode::ArbInclude)
                                    } else if extension.name == *GL_GOOGLE_INCLUDE_DIRECTIVE {
                                        Some(IncludeMode::GoogleInclude)
                                    } else {
                                        None
                                    };

                                if let Some(target) = target_include_mode {
                                    if extension.behavior.is_active() {
                                        self.current_state.include_mode = target;
                                    } else {
                                        // TODO: Implement current mode as a stack?
                                        self.current_state.include_mode = IncludeMode::None;
                                    }
                                }
                            }

                            result.push(Event::Extension { directive });
                        }
                        _ => {
                            // Handle node, this is a preprocessor directive
                            result.push(Event::Unhandled(node));
                        }
                    }
                }
                rowan::NodeOrToken::Token(token) => {
                    result.push(Event::Token(token));
                }
            }
        }

        result
    }
}

#[derive(Debug)]
pub enum Event<E: std::error::Error> {
    IoError(E),
    EnterFile {
        file_id: FileId,
        path: PathBuf,
    },
    ParseError(crate::parser::Error),
    Token(SyntaxToken),
    Version {
        directive: DirectiveResult<Version>,
    },
    Extension {
        directive: DirectiveResult<Extension>,
    },
    Unhandled(SyntaxNode),
}

pub struct ProcessorEvents<'p, F: FileSystem> {
    processor: Option<&'p mut Processor<F>>,
    file_stack: Vec<PathBuf>,
    event_buf: VecDeque<Event<F::Error>>,
}

impl<'p, F: FileSystem> Iterator for ProcessorEvents<'p, F> {
    type Item = Event<F::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(processor) = &mut self.processor {
                // First, check if we have buffered any events
                if let Some(event) = self.event_buf.pop_front() {
                    return Some(event);
                }

                // Then, check how we can generate more events
                if let Some(file) = self.file_stack.pop() {
                    // An unprocessed file
                    match processor.parse(&file) {
                        Ok((file_id, ast)) => {
                            let ast = ast.clone();

                            // We entered a file
                            self.event_buf.push_back(Event::EnterFile {
                                file_id,
                                path: file.to_owned(),
                            });

                            // Add all preprocessor events
                            self.event_buf.extend(processor.expand(ast));

                            continue;
                        }
                        Err(err) => {
                            // Failed reading the file
                            return Some(Event::IoError(err));
                        }
                    }
                }

                // If we get here, there are no more events we can generate
                self.processor.take();
                return None;
            } else {
                return None;
            }
        }
    }
}

impl<F: FileSystem> FusedIterator for ProcessorEvents<'_, F> {}

impl<F: FileSystem + Default> Default for Processor<F> {
    fn default() -> Self {
        Self {
            file_cache: HashMap::with_capacity(1),
            file_ids: HashMap::with_capacity(1),
            canonical_paths: HashMap::with_capacity(1),
            current_state: Default::default(),
            fs: F::default(),
        }
    }
}
