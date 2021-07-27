use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::rc::Rc;

use lang_util::FileId;

use crate::ast;

/// Parsing options
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParseOptions {
    /// `true` if the GLSL target should be Vulkan instead of OpenGL
    pub target_vulkan: bool,
    /// Unique source identifier for token positions
    pub source_id: FileId,
    /// Allow Rust quoting identifiers (`#(ident)`) in the source
    pub allow_rs_ident: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            target_vulkan: false,
            source_id: FileId::new(0),
            allow_rs_ident: false,
        }
    }
}

impl ParseOptions {
    /// Create new parsing options using default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new parsing context from this options object
    pub fn build(self) -> ParseContext {
        ParseContext {
            opts: self,
            ..Default::default()
        }
    }

    /// Create a new parsing context from this options object, with comment parsing enabled
    pub fn with_comments(self) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(ParseContextData::with_comments())),
        }
    }

    /// Create a new parsing context from this options object, with a custom type table policy
    pub fn with_policy(self, policy: impl TypeTablePolicy + 'static) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(ParseContextData::with_policy(policy))),
        }
    }

    /// Create a new parsing context from this options object, with a custom type table policy and
    /// comment parsing enabled
    pub fn with_comments_and_policy(self, policy: impl TypeTablePolicy + 'static) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(ParseContextData::with_comments_and_policy(
                policy,
            ))),
        }
    }

    /// Create a new parsing context from this options object and pre-existing context data
    pub fn with_context(self, context: ParseContextData) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(context)),
        }
    }
}

/// Parsing context
#[derive(Default, Debug, Clone, PartialEq)]
pub struct ParseContext {
    /// Parsing options
    pub opts: ParseOptions,
    /// Parsing data
    pub data: Rc<RefCell<ParseContextData>>,
}

impl ParseContext {
    /// Clone the parsing data and return the cloned context
    pub fn clone_inner(&self) -> Self {
        Self {
            opts: self.opts,
            data: Rc::new(RefCell::new(self.data.borrow().clone())),
        }
    }

    /// Consume this [ParseContext] and return its data. Will fail if there are multiple references
    /// to this context's data.
    pub fn into_data(self) -> Option<ParseContextData> {
        Rc::try_unwrap(self.data).ok().map(RefCell::into_inner)
    }

    /// Obtain a reference to the context's data
    pub fn data(&self) -> std::cell::Ref<'_, ParseContextData> {
        self.data.borrow()
    }

    /// Obtain an exclusive reference to the context's data
    pub fn data_mut(&self) -> std::cell::RefMut<'_, ParseContextData> {
        self.data.borrow_mut()
    }

    /// Create a new parse context cloning the given one's data, but applies the given policy
    pub fn with_policy(&self, policy: impl TypeTablePolicy + 'static) -> ParseContext {
        Self {
            opts: self.opts,
            data: {
                let mut data = self.data().clone();
                data.policy = Rc::new(policy);
                Rc::new(RefCell::new(data))
            },
        }
    }
}

impl From<ParseOptions> for ParseContext {
    fn from(opts: ParseOptions) -> Self {
        Self {
            opts,
            data: Default::default(),
        }
    }
}

/// Parsing context data
#[derive(Debug, Clone)]
pub struct ParseContextData {
    /// List of known type names
    pub names: HashSet<String>,
    /// List of parsed comments (or `None` to disable comment parsing)
    pub comments: Option<CommentList>,

    policy: Rc<dyn TypeTablePolicy>,
}

impl ParseContextData {
    /// Create a new [ParseContextData] object
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new [ParseContextData] object with the given type table policy
    ///
    /// # Parameters
    ///
    /// * `policy`: policy object managing the type table
    pub fn with_policy(policy: impl TypeTablePolicy + 'static) -> Self {
        Self {
            policy: Rc::new(policy),
            ..Default::default()
        }
    }

    /// Create a new [ParseContextData] object with comments parsing enabled
    pub fn with_comments() -> Self {
        Self {
            comments: Some(Default::default()),
            ..Default::default()
        }
    }

    /// Create a new [ParseContextData] object with comments parsing enabled and the given type
    /// table policy
    ///
    /// # Parameters
    ///
    /// * `policy`: policy object managing the type table
    pub fn with_comments_and_policy(policy: impl TypeTablePolicy + 'static) -> Self {
        Self {
            comments: Some(Default::default()),
            policy: Rc::new(policy),
            ..Default::default()
        }
    }
}

impl Default for ParseContextData {
    fn default() -> Self {
        Self {
            names: Default::default(),
            comments: Default::default(),
            policy: Rc::new(GlslTypeTablePolicy),
        }
    }
}

impl PartialEq for ParseContextData {
    fn eq(&self, other: &Self) -> bool {
        self.names.eq(&other.names)
    }
}

// Begin type name stuff

/// Context in which an identifier is seen for the first time
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentifierContext {
    /// The identifier is a function declaration name
    FunctionPrototype,
}

/// A policy to dictate which identifiers should be seen as type names or not
pub trait TypeTablePolicy: std::fmt::Debug {
    /// Return `true` if the given identifier (in its context) should now be considered a type name
    /// or not.
    fn promote_to_type_name(&self, name: &ast::Identifier, ctx: IdentifierContext) -> bool;
}

/// Default GLSL type table policy: only struct declarations create new type names
#[derive(Debug, Clone, Copy)]
pub struct GlslTypeTablePolicy;

impl TypeTablePolicy for GlslTypeTablePolicy {
    fn promote_to_type_name(&self, _: &ast::Identifier, _: IdentifierContext) -> bool {
        false
    }
}

impl ParseContext {
    /// Return `true` if the given name is a type name
    pub fn is_type_name(&self, name: &str) -> bool {
        self.data.borrow().is_type_name(name)
    }

    /// Register `name` as a new type name
    pub fn add_type_name(&self, name: ast::Identifier) -> ast::TypeName {
        self.data.borrow_mut().add_type_name(name)
    }

    /// Update the context data with a new identifier in a given context
    pub fn new_identifier(&self, name: &ast::Identifier, ctx: IdentifierContext) {
        self.data.borrow_mut().new_identifier(name, ctx)
    }
}

impl ParseContextData {
    /// Return `true` if the given name is a type name
    pub fn is_type_name(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    /// Register `name` as a new type name
    pub fn add_type_name(&mut self, name: ast::Identifier) -> ast::TypeName {
        let name_string = name.0.to_string();
        self.names.insert(name_string);
        name.map(ast::TypeNameData::from)
    }

    /// Update the context data with a new identifier in a given context
    pub fn new_identifier(&mut self, name: &ast::Identifier, ctx: IdentifierContext) {
        if self.policy.promote_to_type_name(name, ctx) {
            self.add_type_name(name.clone());
        }
    }
}

// End type name stuff

// Begin comment stuff

/// A list of comments indexed by their position
pub type CommentList = BTreeMap<ast::NodeSpan, ast::Comment>;

impl ParseContext {
    /// Return `true` if this parsing context supports comments
    pub fn has_comments(&self) -> bool {
        self.data.borrow().comments.is_some()
    }

    /// Add a new comment to the parsed comments list
    pub fn add_comment(&self, comment: ast::Comment) {
        self.data.borrow_mut().add_comment(comment)
    }
}

impl ParseContextData {
    /// Add a new comment to the parsed comments list
    pub fn add_comment(&mut self, comment: ast::Comment) {
        if let Some(comments) = self.comments.as_mut() {
            let span = comment.span;
            comments.insert(span.unwrap(), comment);
        }
    }
}
