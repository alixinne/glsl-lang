use rowan::GreenNode;

use crate::util::LineMap;

use super::{Error, SyntaxNode};

#[derive(Debug, Clone)]
pub struct Ast {
    green_node: GreenNode,
    errors: Vec<Error>,
    line_map: LineMap,
}

impl Ast {
    pub fn new(green_node: GreenNode, errors: Vec<Error>, line_map: LineMap) -> Self {
        Self {
            green_node,
            errors,
            line_map,
        }
    }

    pub fn into_inner(self) -> (SyntaxNode, Vec<Error>, LineMap) {
        (
            SyntaxNode::new_root(self.green_node),
            self.errors,
            self.line_map,
        )
    }

    pub fn green_node(&self) -> &GreenNode {
        &self.green_node
    }
}
