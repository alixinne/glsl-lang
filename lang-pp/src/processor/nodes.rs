use std::{borrow::Cow, cmp::Ordering, convert::TryFrom, str::FromStr};

use arrayvec::ArrayVec;
use rowan::NodeOrToken;
use thiserror::Error;

use lang_util::{FileId, SmolStr};

use crate::{
    exts::names::ExtNameAtom,
    parser::{SyntaxKind::*, SyntaxNode, SyntaxToken},
    processor::expr::{EvalResult, ExprEvaluator},
    types::{
        path::{ParsedPath, PathType},
        Token,
    },
    util::Unescaped,
};

use super::{
    definition::{trim_ws, MacroInvocation},
    event::{Event, OutputToken, SendEvent},
    expand::ExpandLocation,
    ProcessorState,
};

#[derive(Debug, Clone)]
pub struct Directive<I> {
    file_id: FileId,
    node: SyntaxNode,
    inner: I,
}

impl<I> Directive<I> {
    pub fn new(file_id: FileId, node: SyntaxNode, inner: I) -> Self {
        Self {
            file_id,
            node,
            inner,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }

    pub fn into_inner(self) -> (FileId, I, SyntaxNode) {
        (self.file_id, self.inner, self.node)
    }
}

impl<I: TryFrom<(FileId, SyntaxNode)> + std::fmt::Debug + Clone> TryFrom<(FileId, SyntaxNode)>
    for Directive<I>
{
    type Error = (I::Error, SyntaxNode);

    fn try_from((file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        match I::try_from((file_id, value.clone())) {
            Ok(inner) => Ok(Self {
                file_id,
                inner,
                node: value,
            }),
            Err(err) => Err((err, value)),
        }
    }
}

impl<I> std::ops::Deref for Directive<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub type DirectiveResult<I> =
    Result<Directive<I>, <Directive<I> as TryFrom<(FileId, SyntaxNode)>>::Error>;

pub trait DirectiveExt: Sized {
    fn into_node(self) -> SyntaxNode;
}

impl<I: TryFrom<(FileId, SyntaxNode)> + std::fmt::Debug + Clone> DirectiveExt
    for DirectiveResult<I>
{
    fn into_node(self) -> SyntaxNode {
        match self {
            Ok(directive) => directive.into_inner().2,
            Err(error) => error.1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Invalid;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Empty;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version {
    pub number: u16,
    pub profile: VersionProfile,
    pub parsed_profile: Option<VersionProfile>,
}

impl Default for Version {
    fn default() -> Self {
        // Spec 3.3: shaders that do not include a #version directive will be treated as targeting
        // version 1.10
        Self {
            number: 110,
            profile: VersionProfile::None,
            parsed_profile: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum VersionError {
    #[error("missing version number in #version directive")]
    MissingVersionNumber,
    #[error("invalid version number in #version directive")]
    InvalidVersionNumber { version_number: String },
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

impl TryFrom<(FileId, SyntaxNode)> for Version {
    type Error = VersionError;

    fn try_from((_file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        // Parse version number
        // The GLSL spec refers to the ISO/IEC 14882:1998 (C++) standard for preprocessor
        // directives, and mentions that #version, due to it "following the same convention"
        // as __VERSION__, is "a decimal integer". The cited standard, section 2.9
        // "Preprocessing numbers", says that such numbers "lexically include all integral
        // literal tokens". These correspond to the "integer-constant" (for the GLSL grammar)
        // or "integer-literal" (for the C++ grammar) grammar symbols. But, because #version
        // is a decimal integer, it is understood that the only "integer-constant" production
        // rule we have to accept is the one for decimal constants, which are sequences of
        // decimal digits beginning with a nonzero digit and followed by an optional
        // "integer-suffix" (u or U character). This is a subset of what we can parse as
        // (U)INT_CONST tokens, but other implementations are likely to observe Postel's law
        let version_number: u16 = value
            .children()
            .find_map(|token| {
                if token.kind() == PP_VERSION_NUMBER {
                    token.first_token()
                } else {
                    None
                }
            })
            .ok_or(Self::Error::MissingVersionNumber)
            .and_then(|token| {
                let token_string = Unescaped::new(token.text()).to_string();
                match Token::parse_digits(&token_string) {
                    Token::INT_CONST(version_number)
                        if version_number >= 0 && version_number <= u16::MAX as i32 =>
                    {
                        Ok(version_number as u16)
                    }
                    Token::UINT_CONST(version_number) if version_number <= u16::MAX as u32 => {
                        Ok(version_number as u16)
                    }
                    _ => Err(Self::Error::InvalidVersionNumber {
                        version_number: token_string.into_owned(),
                    }),
                }
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
            Unescaped::new(profile.text())
                .to_string()
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
                parsed_profile: Some(profile),
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
                parsed_profile: Some(profile),
            })
        } else {
            Ok(Self {
                number: version_number,
                profile,
                parsed_profile: Some(profile),
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

impl VersionProfile {
    /// Returns an integer representing the relative size of the feature set of an OpenGL profile.
    /// Profiles with a higher index are assumed to offer a superset of the features of profiles
    /// with a lower index.
    fn as_feature_set_size_index(&self) -> usize {
        // OpenGL ES offers a feature set smaller than the core profile.
        // Conversely, the core profile has less features than the compatibility profile.
        // When no profile is specified, the profile defaults to core
        match self {
            Self::None | Self::Core => 1,
            Self::Compatibility => 2,
            Self::Es => 0,
        }
    }
}

impl PartialOrd for VersionProfile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_feature_set_size_index()
            .partial_cmp(&other.as_feature_set_size_index())
    }
}

impl Ord for VersionProfile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_feature_set_size_index()
            .cmp(&other.as_feature_set_size_index())
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
    Specific(ExtNameAtom),
}

impl ExtensionName {
    pub fn new(name: Cow<'_, str>) -> Self {
        Self::from(name.as_ref())
    }
}

impl PartialEq<ExtNameAtom> for ExtensionName {
    fn eq(&self, other: &ExtNameAtom) -> bool {
        match self {
            ExtensionName::All => false,
            ExtensionName::Specific(this) => this == other,
        }
    }
}

impl std::fmt::Display for ExtensionName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExtensionName::All => write!(f, "all"),
            ExtensionName::Specific(name) => write!(f, "{}", name),
        }
    }
}

impl From<&str> for ExtensionName {
    fn from(name: &str) -> Self {
        if name == "all" {
            Self::All
        } else {
            Self::Specific(ExtNameAtom::from(name))
        }
    }
}

impl From<ExtNameAtom> for ExtensionName {
    fn from(name: ExtNameAtom) -> Self {
        if name.as_ref() == "all" {
            Self::All
        } else {
            Self::Specific(name)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq, Error)]
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

impl TryFrom<(FileId, SyntaxNode)> for Extension {
    type Error = ExtensionError;

    fn try_from((_file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
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
            .ok_or(Self::Error::MissingExtensionName)
            .map(|name| ExtensionName::new(Unescaped::new(name.text()).to_string()))?;

        let behavior = idents
            .get(1)
            .ok_or_else(|| Self::Error::MissingExtensionBehavior { name: name.clone() })
            .and_then(|behavior| {
                ExtensionBehavior::from_str(&Unescaped::new(behavior.text()).to_string())
                    .map_err(|_| Self::Error::InvalidExtensionBehavior { name: name.clone() })
            })?;

        if name == ExtensionName::All
            && behavior != ExtensionBehavior::Warn
            && behavior != ExtensionBehavior::Disable
        {
            return Err(Self::Error::InvalidAllBehavior { behavior });
        }

        Ok(Self { name, behavior })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefineObject {
    tokens: SyntaxNode,
}

impl DefineObject {
    pub fn new(tokens: SyntaxNode) -> Self {
        Self { tokens }
    }

    pub fn one() -> Self {
        Self::from_str("1").unwrap()
    }

    pub fn body(&self) -> &SyntaxNode {
        &self.tokens
    }
}

impl FromStr for DefineObject {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            tokens: crate::parser::Parser::new(s)
                .parse_define_body()
                .ok_or(())?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefineFunction {
    args: Vec<SmolStr>,
    tokens: SyntaxNode,
}

impl DefineFunction {
    pub fn new(args: Vec<SmolStr>, tokens: SyntaxNode) -> Self {
        Self { args, tokens }
    }

    pub fn arg_names(&self) -> &[SmolStr] {
        &self.args
    }

    pub fn body(&self) -> &SyntaxNode {
        &self.tokens
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefineKind {
    Object(DefineObject),
    Function(DefineFunction),
}

/// A preprocessor definition
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum DefineError {
    #[error("missing name for #define")]
    MissingName,
    #[error("missing body for #define")]
    MissingBody { name: SmolStr },
}

impl TryFrom<(FileId, SyntaxNode)> for Define {
    type Error = DefineError;

    fn try_from((_file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        // Find out define name
        let name = value
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
            .nth(1)
            .ok_or(Self::Error::MissingName)?;

        let name = Unescaped::new(name.text());

        // Find the body
        let body = value
            .children()
            .find(|node| node.kind() == PP_DEFINE_BODY)
            .ok_or_else(|| Self::Error::MissingBody { name: name.into() })?;

        // Find the arguments
        if let Some(args) = value.children().find(|node| node.kind() == PP_DEFINE_ARGS) {
            let args = args
                .children()
                .filter_map(|arg| {
                    if arg.kind() == PP_DEFINE_ARG {
                        arg.first_token()
                            .map(|token| Unescaped::new(token.text()).into())
                    } else {
                        None
                    }
                })
                .collect();

            Ok(Self::function(
                name.into(),
                DefineFunction::new(args, body),
                false,
            ))
        } else {
            Ok(Self::object(name.into(), DefineObject::new(body), false))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfDef {
    pub ident: SmolStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum IfDefError {
    #[error("identifier for #ifdef is missing")]
    MissingIdentifier,
}

impl TryFrom<(FileId, SyntaxNode)> for IfDef {
    type Error = IfDefError;

    fn try_from((_file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        let pp_ident = value
            .children()
            .find_map(|node| {
                if node.kind() == PP_IDENT {
                    node.first_child_or_token()
                        .and_then(|node_or_token| node_or_token.into_token())
                } else {
                    None
                }
            })
            .ok_or(Self::Error::MissingIdentifier)?;

        Ok(Self {
            ident: Unescaped::new(pp_ident.text()).into(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfNDef {
    pub ident: SmolStr,
}

impl TryFrom<(FileId, SyntaxNode)> for IfNDef {
    type Error = <IfDef as TryFrom<(FileId, SyntaxNode)>>::Error;

    fn try_from(value: (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self {
            ident: IfDef::try_from(value)?.ident,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Undef {
    pub ident: SmolStr,
}

impl TryFrom<(FileId, SyntaxNode)> for Undef {
    type Error = <IfDef as TryFrom<(FileId, SyntaxNode)>>::Error;

    fn try_from(value: (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self {
            ident: IfDef::try_from(value)?.ident,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ErrorError {
    #[error("missing body for #error")]
    MissingBody,
}

impl TryFrom<(FileId, SyntaxNode)> for Error {
    type Error = ErrorError;

    fn try_from((_file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        let body = value
            .children()
            .find(|node| node.kind() == PP_ERROR_BODY)
            .ok_or(Self::Error::MissingBody)?;

        // Unescape line continuations
        let raw_message = body.text();
        let mut message = String::with_capacity(raw_message.len().into());

        raw_message.for_each_chunk(|chunk| {
            message.extend(Unescaped::new(chunk).chars());
        });

        Ok(Self { message })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Include {
    file_id: FileId,
    path: SyntaxNode,
}

impl Include {
    pub fn path(
        &self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> Result<ParsedPath, IncludeError> {
        // Perform macro substitution
        let tokens = self
            .path
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .map(|token| (token, self.file_id))
            .collect();
        let subs_events = MacroInvocation::substitute_vec(current_state, tokens, location);

        // Make sure they are all tokens
        if !subs_events.iter().all(Event::is_token) {
            return Err(IncludeError::MalformedPath {
                tokens: subs_events.into_iter().map(Into::into).collect(),
            });
        }

        let subs_tokens: Vec<_> = subs_events
            .into_iter()
            .filter_map(Event::into_token)
            .collect();

        // Discard trivial tokens
        let subs_tokens = trim_ws(&subs_tokens);

        // By now, the include should either be a quote string or an angle string, and this should
        // be the only token
        if subs_tokens.is_empty() {
            return Err(IncludeError::MissingPath);
        } else if subs_tokens.len() > 1 {
            return Err(IncludeError::ExtraTokens {
                tokens: subs_tokens.to_vec(),
            });
        }

        // unwrap: we just checked there is one
        let first_token = subs_tokens.first().unwrap();
        let ty = match first_token.kind() {
            ANGLE_STRING => PathType::Angle,
            QUOTE_STRING => PathType::Quote,
            _ => {
                return Err(IncludeError::InvalidPathLiteral {
                    token: first_token.clone(),
                });
            }
        };

        let text = first_token.text();
        let text = &text[1..text.len() - 1];

        Ok(ParsedPath {
            path: text.to_string(),
            ty,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum IncludeError {
    #[error("missing path for #include directive")]
    MissingPath,
    #[error("malformed path")]
    MalformedPath { tokens: Vec<SendEvent> },
    #[error("extra tokens in #include path")]
    ExtraTokens { tokens: Vec<OutputToken> },
    #[error("invalid path literal")]
    InvalidPathLiteral { token: OutputToken },
}

impl TryFrom<(FileId, SyntaxNode)> for Include {
    type Error = IncludeError;

    fn try_from((file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self {
            file_id,
            path: value
                .children()
                .find(|node| node.kind() == PP_INCLUDE_PATH)
                .ok_or(Self::Error::MissingPath)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Line {
    file_id: FileId,
    body: SyntaxNode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedLine {
    Line(u32),
    LineAndFileNumber(u32, u32),
    LineAndPath(u32, String),
}

impl ParsedLine {
    pub fn line_number(&self) -> u32 {
        match self {
            ParsedLine::Line(line)
            | ParsedLine::LineAndFileNumber(line, _)
            | ParsedLine::LineAndPath(line, _) => *line,
        }
    }
}

impl From<ParsedLine> for lang_util::located::FileOverride {
    fn from(value: ParsedLine) -> Self {
        match value {
            ParsedLine::Line(_) => Self::None,
            ParsedLine::LineAndFileNumber(_, file) => Self::Number(file),
            ParsedLine::LineAndPath(_, path) => Self::Path(path),
        }
    }
}

impl Line {
    pub fn parse(
        &self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> Result<ParsedLine, LineError> {
        // Perform macro substitution
        let tokens = self
            .body
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .map(|token| (token, self.file_id))
            .collect();
        let subs_events = MacroInvocation::substitute_vec(current_state, tokens, location);

        // Make sure they are all tokens
        if !subs_events.iter().all(Event::is_token) {
            return Err(LineError::MalformedLine {
                tokens: subs_events.into_iter().map(Into::into).collect(),
            });
        }

        // Evalute the expressions in the line directive
        let eval_results: Vec<_> = ExprEvaluator::new(
            subs_events.iter().filter_map(Event::as_token),
            current_state,
        )
        .collect();

        // By now, the include should either be a quote string or an angle string, and this should
        // be the only token
        if !eval_results.is_empty() && eval_results.len() <= 2 {
            let token = &eval_results[0];
            // Line number
            let line_number: u32 = if let EvalResult::Constant(Ok(value)) = token {
                if *value >= 0 {
                    Some(*value as _)
                } else {
                    None
                }
            } else {
                None
            }
            .ok_or_else(|| LineError::InvalidLineNumber {
                token: token.clone(),
            })?;

            if eval_results.len() > 1 {
                let token = &eval_results[1];
                match token {
                    EvalResult::Constant(value) => Ok(ParsedLine::LineAndFileNumber(
                        line_number,
                        if let Ok(value) =
                            value.and_then(|val| if val >= 0 { Ok(val as _) } else { Err(()) })
                        {
                            Ok(value)
                        } else {
                            Err(LineError::InvalidPath {
                                token: token.clone(),
                            })
                        }?,
                    )),
                    EvalResult::Token(token) => {
                        if token.kind() == QUOTE_STRING {
                            let text = token.text();
                            Ok(ParsedLine::LineAndPath(
                                line_number,
                                text[1..text.len() - 1].to_string(),
                            ))
                        } else {
                            Err(LineError::InvalidPath {
                                token: EvalResult::Token(token.clone()),
                            })
                        }
                    }
                }
            } else {
                Ok(ParsedLine::Line(line_number))
            }
        } else if eval_results.is_empty() {
            Err(LineError::MissingLineNumber)
        } else {
            Err(LineError::ExtraTokens {
                tokens: eval_results,
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LineError {
    #[error("missing body for #line directive")]
    MissingBody,
    #[error("malformed line")]
    MalformedLine { tokens: Vec<SendEvent> },
    #[error("missing line number")]
    MissingLineNumber,
    #[error("extra tokens in #line path")]
    ExtraTokens { tokens: Vec<EvalResult> },
    #[error("invalid line number")]
    InvalidLineNumber { token: EvalResult },
    #[error("invalid path")]
    InvalidPath { token: EvalResult },
}

impl TryFrom<(FileId, SyntaxNode)> for Line {
    type Error = LineError;

    fn try_from((file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self {
            file_id,
            body: value
                .children()
                .find(|node| node.kind() == PP_LINE_BODY)
                .ok_or(Self::Error::MissingBody)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum IfEvalError {
    #[error("malformed expression")]
    MalformedExpr { tokens: Vec<SendEvent> },
    #[error("missing expression")]
    MissingExpr,
    #[error("extra tokens at end of expression")]
    ExtraTokens { tokens: Vec<EvalResult> },
    #[error("invalid constant expression")]
    InvalidExpr { token: EvalResult },
}

fn eval_inner(
    definition_file_id: FileId,
    body: &SyntaxNode,
    current_state: &ProcessorState,
    location: &ExpandLocation,
) -> (bool, Option<IfEvalError>) {
    // Perform macro substitution
    let tokens = body
        .children_with_tokens()
        .filter_map(NodeOrToken::into_token)
        .map(|token| (token, definition_file_id))
        .collect();
    let subs_events = MacroInvocation::substitute_vec(current_state, tokens, location);

    // Make sure they are all tokens
    if !subs_events.iter().all(Event::is_token) {
        return (
            true,
            Some(IfEvalError::MalformedExpr {
                tokens: subs_events.into_iter().map(Into::into).collect(),
            }),
        );
    }

    // Evalute the expressions in the line directive
    let eval_results: Vec<_> = ExprEvaluator::new(
        subs_events.iter().filter_map(Event::as_token),
        current_state,
    )
    .collect();

    // Check that we have at least "something" to evaluate
    if eval_results.is_empty() {
        return (true, Some(IfEvalError::MissingExpr));
    }

    // Split the result and the rest
    let (first, rest): (_, Vec<_>) = {
        let mut iter = eval_results.into_iter();
        let first = iter.next().unwrap();
        (first, iter.collect())
    };

    // Do we have extra tokens?
    let error = if !rest.is_empty() {
        Some(IfEvalError::ExtraTokens { tokens: rest })
    } else {
        None
    };

    // Is the result an int?
    match first {
        EvalResult::Constant(Ok(value)) => (value != 0, error),
        other => (true, Some(IfEvalError::InvalidExpr { token: other })),
    }
}

fn eval_if<E: From<IfEvalError>>(
    definition_file_id: FileId,
    body: &SyntaxNode,
    current_state: &ProcessorState,
    location: &ExpandLocation,
) -> (bool, Option<E>) {
    let (result, e) = eval_inner(definition_file_id, body, current_state, location);
    (result, e.map(E::from))
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum IfError {
    #[error("missing body for #if directive")]
    MissingBody,
    #[error(transparent)]
    Eval(#[from] IfEvalError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    file_id: FileId,
    body: SyntaxNode,
}

impl If {
    pub fn eval(
        &self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> (bool, Option<IfError>) {
        eval_if(self.file_id, &self.body, current_state, location)
    }
}

impl TryFrom<(FileId, SyntaxNode)> for If {
    type Error = IfError;

    fn try_from((file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self {
            file_id,
            body: value
                .children()
                .find(|node| node.kind() == PP_IF_EXPR)
                .ok_or(Self::Error::MissingBody)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Elif {
    file_id: FileId,
    body: SyntaxNode,
}

impl Elif {
    pub fn eval(
        &self,
        current_state: &ProcessorState,
        location: &ExpandLocation,
    ) -> (bool, Option<ElifError>) {
        eval_if(self.file_id, &self.body, current_state, location)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ElifError {
    #[error("missing body for #elif directive")]
    MissingBody,
    #[error(transparent)]
    Eval(#[from] IfEvalError),
}

impl TryFrom<(FileId, SyntaxNode)> for Elif {
    type Error = ElifError;

    fn try_from((file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self {
            file_id,
            body: value
                .children()
                .find(|node| node.kind() == PP_IF_EXPR)
                .ok_or(Self::Error::MissingBody)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Else;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ElseError {}

impl TryFrom<(FileId, SyntaxNode)> for Else {
    type Error = ElseError;

    fn try_from(_: (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EndIf;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum EndIfError {}

impl TryFrom<(FileId, SyntaxNode)> for EndIf {
    type Error = EndIfError;

    fn try_from(_: (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        Ok(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pragma {
    value: ParsedPragma,
    raw: String,
}

impl Pragma {
    pub fn value(&self) -> &ParsedPragma {
        &self.value
    }

    pub fn raw(&self) -> &str {
        self.raw.as_str()
    }

    fn parse_function_pragma(tokens: &[SyntaxToken]) -> Option<bool> {
        if tokens.len() == 4 {
            let value = Unescaped::new(tokens[2].text()).to_string();

            if tokens[1].kind() == LPAREN && tokens[3].kind() == RPAREN {
                if value == "on" {
                    return Some(true);
                } else if value == "off" {
                    return Some(false);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedPragma {
    StdGl(SyntaxNode),
    Optimize(bool),
    Debug(bool),
    Unknown(SyntaxNode),
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum PragmaError {
    #[error("missing body")]
    MissingBody,
    #[error("{name} pragma syntax is incorrect")]
    IncorrectSyntax { name: SmolStr },
}

impl TryFrom<(FileId, SyntaxNode)> for Pragma {
    type Error = PragmaError;

    fn try_from((_file_id, value): (FileId, SyntaxNode)) -> Result<Self, Self::Error> {
        let body = value
            .children()
            .find(|node| node.kind() == PP_PRAGMA_BODY)
            .ok_or(Self::Error::MissingBody)?;

        let raw = body.text().to_string();

        // Collect non-trivial tokens
        let tokens: Vec<_> = body
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .filter(|token| !token.kind().is_whitespace())
            .collect();

        if let Some(first_token) = tokens.first() {
            if first_token.kind() == IDENT_KW {
                let name = SmolStr::from(Unescaped::new(first_token.text()));
                match name.as_str() {
                    "STDGL" => {
                        return Ok(Self {
                            value: ParsedPragma::StdGl(body),
                            raw,
                        });
                    }
                    "optimize" => {
                        return if let Some(value) = Self::parse_function_pragma(&tokens) {
                            Ok(Self {
                                value: ParsedPragma::Optimize(value),
                                raw,
                            })
                        } else {
                            Err(Self::Error::IncorrectSyntax { name })
                        };
                    }
                    "debug" => {
                        return if let Some(value) = Self::parse_function_pragma(&tokens) {
                            Ok(Self {
                                value: ParsedPragma::Debug(value),
                                raw,
                            })
                        } else {
                            Err(Self::Error::IncorrectSyntax { name })
                        };
                    }
                    _ => {}
                }
            }
        }

        Ok(Self {
            value: ParsedPragma::Unknown(body),
            raw,
        })
    }
}
