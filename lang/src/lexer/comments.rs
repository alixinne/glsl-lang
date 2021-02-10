use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast;

pub type CommentList = BTreeMap<ast::NodeSpan, ast::Comment>;

#[derive(Clone)]
pub struct Comments {
    comments: Rc<RefCell<CommentList>>,
}

impl Comments {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_inner(self) -> Option<CommentList> {
        Rc::try_unwrap(self.comments).ok().map(|rc| rc.into_inner())
    }

    pub fn clone_inner(&self) -> Self {
        Self {
            comments: Rc::new(RefCell::new(self.comments.borrow().clone())),
        }
    }

    pub fn add_comment(&self, comment: ast::Comment) {
        let span = comment.span;
        self.comments.borrow_mut().insert(span.unwrap(), comment);
    }
}

impl Default for Comments {
    fn default() -> Self {
        Self {
            comments: Default::default(),
        }
    }
}

impl std::fmt::Debug for Comments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Comments")
            .field("comments", &self.comments)
            .finish()
    }
}

impl PartialEq for Comments {
    fn eq(&self, other: &Self) -> bool {
        self.comments.eq(&other.comments)
    }
}
