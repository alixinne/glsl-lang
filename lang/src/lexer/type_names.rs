use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::ast;

#[derive(Debug, Clone, Copy)]
pub enum IdentifierContext {
    FunctionPrototype,
}

pub trait TypeTablePolicy {
    fn promote_to_type_name(&self, name: &ast::Identifier, ctx: IdentifierContext) -> bool;
}

pub struct GlslTypeTablePolicy {}

impl TypeTablePolicy for GlslTypeTablePolicy {
    fn promote_to_type_name(&self, _: &ast::Identifier, _: IdentifierContext) -> bool {
        false
    }
}

#[derive(Clone)]
pub struct TypeNames {
    names: Rc<RefCell<HashSet<String>>>,
    policy: Rc<dyn TypeTablePolicy>,
}

impl TypeNames {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_policy(policy: impl TypeTablePolicy + 'static) -> Self {
        Self {
            policy: Rc::new(policy),
            ..Default::default()
        }
    }

    pub fn clone_inner(&self) -> Self {
        Self {
            names: Rc::new(RefCell::new(self.names.borrow().clone())),
            policy: self.policy.clone(),
        }
    }

    pub fn is_type_name(&self, name: &str) -> bool {
        self.names.borrow().contains(name)
    }

    pub fn add_type_name(&self, name: ast::Identifier) -> ast::TypeName {
        let name_string = name.0.to_string();
        self.names.borrow_mut().insert(name_string);
        name.map(|id| ast::TypeNameData::from(id))
    }

    pub fn new_identifier(&self, name: &ast::Identifier, ctx: IdentifierContext) {
        if self.policy.promote_to_type_name(name, ctx) {
            self.add_type_name(name.clone());
        }
    }
}

impl Default for TypeNames {
    fn default() -> Self {
        Self {
            names: Default::default(),
            policy: Rc::new(GlslTypeTablePolicy {}),
        }
    }
}

impl std::fmt::Debug for TypeNames {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeNames")
            .field("names", &self.names)
            .finish()
    }
}

impl PartialEq for TypeNames {
    fn eq(&self, other: &Self) -> bool {
        self.names.eq(&other.names)
    }
}
