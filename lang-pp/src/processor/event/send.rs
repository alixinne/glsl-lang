use std::path::PathBuf;

use rowan::TextRange;

use lang_util::FileId;

use crate::processor::expr::EvalResult;

use super::{Error, Event, OutputToken, SyntaxNode, TokenLike};

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
pub struct SendToken {
    root: rowan::GreenNode,
    range: TextRange,
    source_range: Option<TextRange>,
}

impl SendToken {
    pub fn text(&self) -> String {
        OutputToken::from(self.clone()).text().to_string()
    }
}

impl From<&OutputToken> for SendToken {
    fn from(node: &OutputToken) -> Self {
        Self {
            root: node.inner.ancestors().last().unwrap().green().to_owned(),
            range: node.text_range(),
            source_range: node.source_range,
        }
    }
}

impl From<SendToken> for OutputToken {
    fn from(node: SendToken) -> Self {
        let range = node.range;
        let inner = SyntaxNode::new_root(node.root)
            .descendants_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|descendant| descendant.text_range() == range)
            .unwrap();
        Self {
            inner,
            source_range: node.source_range,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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
        token: SendToken,
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
            Event::Token { token, masked } => Self::Token {
                token: (&token).into(),
                masked,
            },
            Event::Directive {
                node,
                masked,
                errors,
                ..
            } => Self::Directive {
                node: (&node).into(),
                masked,
                errors,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SendEvalResult {
    Constant(Result<i32, ()>),
    Token(SendToken),
}

impl From<&EvalResult> for SendEvalResult {
    fn from(value: &EvalResult) -> Self {
        match value {
            EvalResult::Constant(constant) => Self::Constant(*constant),
            EvalResult::Token(token) => Self::Token(token.into()),
        }
    }
}
