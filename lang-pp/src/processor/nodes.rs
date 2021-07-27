use std::{borrow::Cow, convert::TryFrom, str::FromStr};

use arrayvec::ArrayVec;
use rowan::NodeOrToken;
use smol_str::SmolStr;
use string_cache::Atom;
use thiserror::Error;

use crate::{
    parser::{SyntaxKind::*, SyntaxNode},
    processor::event::TokenLike,
    Unescaped,
};

use super::{
    definition::{trim_ws, MacroInvocation},
    event::{Event, OutputToken},
    expand::ExpandLocation,
    exts, ProcessorState,
};

#[derive(Debug, Clone)]
pub struct Directive<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> {
    node: SyntaxNode,
    inner: I,
}

impl<I: TryFrom<SyntaxNode> + std::fmt::Debug + Clone> Directive<I> {
    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }

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
                lexical::parse(Unescaped::new(token.text()).to_string().as_ref())
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
    pub fn new(name: Cow<'_, str>) -> Self {
        if name == "all" {
            Self::All
        } else {
            Self::Specific(Atom::from(name))
        }
    }
}

lazy_static::lazy_static! {
    pub static ref GL_ARB_SHADING_LANGUAGE_INCLUDE: ExtensionName = ExtensionName::Specific(ext_name!("GL_ARB_shading_language_include"));
    pub static ref GL_GOOGLE_INCLUDE_DIRECTIVE: ExtensionName = ExtensionName::Specific(ext_name!("GL_GOOGLE_include_directive"));
    pub static ref GL_GOOGLE_CPP_STYLE_LINE_DIRECTIVE: ExtensionName = ExtensionName::Specific(ext_name!("GL_GOOGLE_cpp_style_line_directive"));
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
            .map(|name| ExtensionName::new(Unescaped::new(name.text()).to_string()))?;

        let behavior = idents
            .get(1)
            .ok_or_else(|| Self::Error::MissingExtensionBehavior { name: name.clone() })
            .and_then(|behavior| {
                ExtensionBehavior::from_str(&Unescaped::new(behavior.text()).to_string())
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

#[derive(Debug, Clone)]
pub struct DefineObject {
    tokens: SyntaxNode,
}

impl DefineObject {
    pub fn new(tokens: SyntaxNode) -> Self {
        Self { tokens }
    }

    pub fn from_str(input: &str) -> Option<Self> {
        Some(Self {
            tokens: crate::parser::Parser::new(input).parse_define_body()?,
        })
    }

    pub fn body(&self) -> &SyntaxNode {
        &self.tokens
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Error)]
pub enum DefineError {
    #[error("missing name for #define")]
    MissingName,
    #[error("missing body for #define")]
    MissingBody { name: SmolStr },
}

impl TryFrom<SyntaxNode> for Define {
    type Error = DefineError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
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
            .skip(1)
            .next()
            .ok_or_else(|| Self::Error::MissingName)?;

        let name = Unescaped::new(name.text());

        // Find the body
        let body = value
            .children()
            .find(|node| node.kind() == PP_DEFINE_BODY)
            .ok_or_else(|| Self::Error::MissingBody {
                name: name.clone().into(),
            })?;

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

#[derive(Debug, Clone)]
pub struct IfDef {
    pub ident: SmolStr,
}

#[derive(Debug, Error)]
pub enum IfDefError {
    #[error("identifier for #ifdef is missing")]
    MissingIdentifier,
}

impl TryFrom<SyntaxNode> for IfDef {
    type Error = IfDefError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
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
            .ok_or_else(|| Self::Error::MissingIdentifier)?;

        Ok(Self {
            ident: Unescaped::new(pp_ident.text()).into(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct IfNDef {
    pub ident: SmolStr,
}

impl TryFrom<SyntaxNode> for IfNDef {
    type Error = <IfDef as TryFrom<SyntaxNode>>::Error;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        Ok(Self {
            ident: IfDef::try_from(value)?.ident,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Undef {
    pub ident: SmolStr,
}

impl TryFrom<SyntaxNode> for Undef {
    type Error = <IfDef as TryFrom<SyntaxNode>>::Error;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        Ok(Self {
            ident: IfDef::try_from(value)?.ident,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
}

#[derive(Debug, Error)]
pub enum ErrorError {
    #[error("missing body for #error")]
    MissingBody,
}

impl TryFrom<SyntaxNode> for Error {
    type Error = ErrorError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        let body = value
            .children()
            .find(|node| node.kind() == PP_ERROR_BODY)
            .ok_or_else(|| Self::Error::MissingBody)?;

        // Unescape line continuations
        let raw_message = body.text();
        let mut message = String::with_capacity(raw_message.len().into());

        raw_message.for_each_chunk(|chunk| {
            message.extend(Unescaped::new(chunk).chars());
        });

        Ok(Self { message })
    }
}

#[derive(Debug, Clone)]
pub struct Include {
    path: SyntaxNode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedPath {
    pub path: String,
    pub ty: PathType,
}

impl std::fmt::Display for ParsedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            PathType::Angle => write!(f, "<{}>", self.path),
            PathType::Quote => write!(f, "\"{}\"", self.path),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathType {
    Angle,
    Quote,
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
            .collect();
        let subs_events: Vec<_> =
            MacroInvocation::substitute_vec(current_state, tokens, location).collect();

        // Make sure they are all tokens
        if !subs_events.iter().all(|evt| matches!(evt, Event::Token(_))) {
            return Err(IncludeError::MalformedPath {
                tokens: subs_events,
            });
        }

        let subs_tokens: Vec<_> = subs_events
            .into_iter()
            .filter_map(|event| match event {
                Event::Token(token) => Some(token),
                _ => None,
            })
            .collect();

        // Discard trivial tokens
        let subs_tokens = trim_ws(&subs_tokens);

        // By now, the include should either be a quote string or an angle string, and this should
        // be the only token
        if subs_tokens.len() == 0 {
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

#[derive(Debug, Error)]
pub enum IncludeError {
    #[error("missing path for #include directive")]
    MissingPath,
    #[error("malformed path")]
    MalformedPath { tokens: Vec<Event> },
    #[error("extra tokens in #include path")]
    ExtraTokens { tokens: Vec<OutputToken> },
    #[error("invalid path literal")]
    InvalidPathLiteral { token: OutputToken },
}

impl TryFrom<SyntaxNode> for Include {
    type Error = IncludeError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        Ok(Self {
            path: value
                .children()
                .find(|node| node.kind() == PP_INCLUDE_PATH)
                .ok_or_else(|| Self::Error::MissingPath)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Line {
    body: SyntaxNode,
}

#[derive(Debug, Clone)]
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
            .collect();
        let subs_events: Vec<_> =
            MacroInvocation::substitute_vec(current_state, tokens, location).collect();

        // Make sure they are all tokens
        if !subs_events.iter().all(|evt| matches!(evt, Event::Token(_))) {
            return Err(LineError::MalformedLine {
                tokens: subs_events,
            });
        }

        // Discard all trivial tokens
        let subs_tokens: Vec<_> = subs_events
            .into_iter()
            .filter_map(|event| match event {
                Event::Token(token) if !token.kind().is_trivia() => Some(token),
                _ => None,
            })
            .collect();

        // By now, the include should either be a quote string or an angle string, and this should
        // be the only token
        if subs_tokens.len() >= 1 && subs_tokens.len() <= 2 {
            // Line number
            let token = &subs_tokens[0];
            let line_number: u32 = if token.kind() == DIGITS {
                Some(token)
            } else {
                None
            }
            .and_then(|token| {
                lexical::parse(Unescaped::new(token.text()).to_string().as_ref()).ok()
            })
            .ok_or_else(|| LineError::InvalidLineNumber {
                token: token.clone(),
            })?;

            if subs_tokens.len() > 1 {
                let token = &subs_tokens[1];
                if token.kind() == QUOTE_STRING {
                    let text = token.text();
                    Ok(ParsedLine::LineAndPath(
                        line_number,
                        text[1..text.len() - 1].to_string(),
                    ))
                } else if token.kind() == DIGITS {
                    Ok(ParsedLine::LineAndFileNumber(
                        line_number,
                        lexical::parse(Unescaped::new(token.text()).to_string().as_ref()).map_err(
                            |_| LineError::InvalidPath {
                                token: token.clone(),
                            },
                        )?,
                    ))
                } else {
                    Err(LineError::InvalidPath {
                        token: token.clone(),
                    })
                }
            } else {
                Ok(ParsedLine::Line(line_number))
            }
        } else {
            Err(LineError::ExtraTokens {
                tokens: subs_tokens.to_vec(),
            })
        }
    }
}

#[derive(Debug, Error)]
pub enum LineError {
    #[error("missing body for #line directive")]
    MissingBody,
    #[error("malformed line")]
    MalformedLine { tokens: Vec<Event> },
    #[error("extra tokens in #line path")]
    ExtraTokens { tokens: Vec<OutputToken> },
    #[error("invalid line number")]
    InvalidLineNumber { token: OutputToken },
    #[error("invalid path")]
    InvalidPath { token: OutputToken },
}

impl TryFrom<SyntaxNode> for Line {
    type Error = LineError;

    fn try_from(value: SyntaxNode) -> Result<Self, Self::Error> {
        Ok(Self {
            body: value
                .children()
                .find(|node| node.kind() == PP_LINE_BODY)
                .ok_or_else(|| Self::Error::MissingBody)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Else;

#[derive(Debug, Error)]
pub enum ElseError {}

impl TryFrom<SyntaxNode> for Else {
    type Error = ElseError;

    fn try_from(_: SyntaxNode) -> Result<Self, Self::Error> {
        // TODO: Handle extra tokens?
        Ok(Self)
    }
}

#[derive(Debug, Clone)]
pub struct EndIf;

#[derive(Debug, Error)]
pub enum EndIfError {}

impl TryFrom<SyntaxNode> for EndIf {
    type Error = EndIfError;

    fn try_from(_: SyntaxNode) -> Result<Self, Self::Error> {
        // TODO: Handle extra tokens?
        Ok(Self)
    }
}
