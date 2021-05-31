use rowan::GreenNode;

use super::{Error, SyntaxNode};

pub struct Ast {
    green_node: GreenNode,
    errors: Vec<Error>,
}

impl Ast {
    pub fn new(green_node: GreenNode, errors: Vec<Error>) -> Self {
        Self { green_node, errors }
    }

    pub fn into_inner(self) -> (SyntaxNode, Vec<Error>) {
        (SyntaxNode::new_root(self.green_node), self.errors)
    }

    pub fn root(self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node)
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}
