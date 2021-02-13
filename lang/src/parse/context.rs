use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::rc::Rc;

use crate::ast;

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct ParseOptions {
    pub target_vulkan: bool,
    pub source_id: usize,
    pub allow_rs_ident: bool,
}

impl ParseOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn build(self) -> ParseContext {
        ParseContext {
            opts: self,
            ..Default::default()
        }
    }

    pub fn with_comments(self) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(ParseContextData::with_comments())),
        }
    }

    pub fn with_policy(self, policy: impl TypeTablePolicy + 'static) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(ParseContextData::with_policy(policy))),
        }
    }

    pub fn with_comments_and_policy(self, policy: impl TypeTablePolicy + 'static) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(ParseContextData::with_comments_and_policy(
                policy,
            ))),
        }
    }

    pub fn with_context(self, context: ParseContextData) -> ParseContext {
        ParseContext {
            opts: self,
            data: Rc::new(RefCell::new(context)),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ParseContext {
    pub opts: ParseOptions,
    pub data: Rc<RefCell<ParseContextData>>,
}

impl ParseContext {
    pub fn clone_inner(&self) -> Self {
        Self {
            opts: self.opts,
            data: Rc::new(RefCell::new(self.data.borrow().clone())),
        }
    }

    pub fn into_data(self) -> Option<ParseContextData> {
        Rc::try_unwrap(self.data).ok().map(RefCell::into_inner)
    }

    pub fn data(&self) -> std::cell::Ref<'_, ParseContextData> {
        self.data.borrow()
    }

    pub fn data_mut(&self) -> std::cell::RefMut<'_, ParseContextData> {
        self.data.borrow_mut()
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

#[derive(Debug, Clone)]
pub struct ParseContextData {
    pub names: HashSet<String>,
    pub comments: Option<CommentList>,

    policy: Rc<dyn TypeTablePolicy>,
}

impl ParseContextData {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_policy(policy: impl TypeTablePolicy + 'static) -> Self {
        Self {
            policy: Rc::new(policy),
            ..Default::default()
        }
    }

    pub fn with_comments() -> Self {
        Self {
            comments: Some(Default::default()),
            ..Default::default()
        }
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentifierContext {
    FunctionPrototype,
}

pub trait TypeTablePolicy: std::fmt::Debug {
    fn promote_to_type_name(&self, name: &ast::Identifier, ctx: IdentifierContext) -> bool;
}

#[derive(Debug, Clone, Copy)]
pub struct GlslTypeTablePolicy;

impl TypeTablePolicy for GlslTypeTablePolicy {
    fn promote_to_type_name(&self, _: &ast::Identifier, _: IdentifierContext) -> bool {
        false
    }
}

impl ParseContext {
    pub fn is_type_name(&self, name: &str) -> bool {
        self.data.borrow().is_type_name(name)
    }

    pub fn add_type_name(&self, name: ast::Identifier) -> ast::TypeName {
        self.data.borrow_mut().add_type_name(name)
    }

    pub fn new_identifier(&self, name: &ast::Identifier, ctx: IdentifierContext) {
        self.data.borrow_mut().new_identifier(name, ctx)
    }
}

impl ParseContextData {
    pub fn is_type_name(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    pub fn add_type_name(&mut self, name: ast::Identifier) -> ast::TypeName {
        let name_string = name.0.to_string();
        self.names.insert(name_string);
        name.map(ast::TypeNameData::from)
    }

    pub fn new_identifier(&mut self, name: &ast::Identifier, ctx: IdentifierContext) {
        if self.policy.promote_to_type_name(name, ctx) {
            self.add_type_name(name.clone());
        }
    }
}

// End type name stuff

// Begin comment stuff

pub type CommentList = BTreeMap<ast::NodeSpan, ast::Comment>;

impl ParseContext {
    pub fn has_comments(&self) -> bool {
        self.data.borrow().comments.is_some()
    }

    pub fn add_comment(&self, comment: ast::Comment) {
        self.data.borrow_mut().add_comment(comment)
    }
}

impl ParseContextData {
    pub fn add_comment(&mut self, comment: ast::Comment) {
        if let Some(comments) = self.comments.as_mut() {
            let span = comment.span;
            comments.insert(span.unwrap(), comment);
        }
    }
}
