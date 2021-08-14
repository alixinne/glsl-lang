use thiserror::Error;

use lang_util::FileId;

use crate::{
    last::LocatedIterator,
    parser::{self, SyntaxNode},
};

use super::{
    event::Event,
    expand::{ExpandEvent, ExpandOne},
    nodes::ParsedPath,
    ProcessorState,
};

pub fn parse(input: &str) -> parser::Ast {
    parser::Parser::new(input).parse()
}

#[derive(Debug, Error)]
pub enum ProcessStrError {
    #[error("an include was requested without a filesystem context")]
    IncludeRequested(ProcessorState, SyntaxNode, ParsedPath),
}

pub fn process(input: &str, state: ProcessorState) -> ExpandStr {
    let file_id = FileId::new(0);
    let ast = parser::Parser::new(input).parse();
    ExpandStr {
        inner: ExpandOne::new((file_id, ast), state),
        final_state: None,
    }
}

pub struct ExpandStr {
    inner: ExpandOne,
    final_state: Option<ProcessorState>,
}

impl ExpandStr {
    pub fn into_state(mut self) -> Option<ProcessorState> {
        self.final_state.take()
    }
}

impl Iterator for ExpandStr {
    type Item = Result<Event, ProcessStrError>;

    fn next(&mut self) -> Option<Self::Item> {
        let event = self.inner.next()?;
        match event {
            ExpandEvent::Event(event) => Some(Ok(event)),
            ExpandEvent::EnterFile(state, node, path) => {
                Some(Err(ProcessStrError::IncludeRequested(state, node, path)))
            }
            ExpandEvent::Completed(state) => {
                self.final_state = Some(state);
                None
            }
        }
    }
}

impl LocatedIterator for ExpandStr {
    fn location(&self) -> &crate::processor::expand::ExpandLocation {
        self.inner.location()
    }
}
