use std::path::PathBuf;

use rowan::TextRange;

use lang_util::FileId;

use super::{parser::SyntaxNode, Error, Event, OutputToken};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendNode {
    root: rowan::GreenNode,
    range: TextRange,
}

impl SendNode {
    pub fn text(&self) -> String {
        SyntaxNode::from(self.clone()).text().to_string()
    }
}

impl From<&SyntaxNode> for SendNode {
    fn from(node: &SyntaxNode) -> Self {
        let range = node.text_range();
        Self {
            root: node
                .ancestors()
                .last()
                .unwrap_or_else(|| node.clone())
                .green()
                .to_owned(),
            range,
        }
    }
}

impl From<SendNode> for SyntaxNode {
    fn from(node: SendNode) -> Self {
        let range = node.range;
        SyntaxNode::new_root(node.root)
            .descendants()
            .find(|descendant| descendant.text_range() == range)
            .unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendEvent {
    Error {
        error: Error,
        masked: bool,
    },
    EnterFile {
        file_id: FileId,
        path: PathBuf,
        canonical_path: PathBuf,
    },
    Token {
        token: OutputToken,
        masked: bool,
    },
    Directive {
        node: SendNode,
        masked: bool,
        errors: Vec<Error>,
    },
}

impl From<Event> for SendEvent {
    fn from(value: Event) -> Self {
        match value {
            Event::Error { error, masked } => Self::Error { error, masked },
            Event::EnterFile {
                file_id,
                path,
                canonical_path,
            } => Self::EnterFile {
                file_id,
                path,
                canonical_path,
            },
            Event::Token { token, masked } => Self::Token { token, masked },
            Event::Directive { directive, masked } => Self::Directive {
                node: (&directive.node).into(),
                masked,
                errors: directive.errors,
            },
        }
    }
}
