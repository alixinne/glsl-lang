use thiserror::Error;

use crate::processor::event::ProcessingErrorKind;

#[derive(Clone, Copy, PartialEq, Eq)]
enum IfState {
    /// No #if group of this level was included
    None,
    /// The current #if group of this level is included
    Active { else_seen: bool },
    /// One past #if group of this level was included, but not the current one
    One { else_seen: bool },
}

impl IfState {
    pub fn else_seen(&self) -> bool {
        match self {
            Self::None => false,
            Self::Active { else_seen } | Self::One { else_seen } => *else_seen,
        }
    }

    pub fn active(&self) -> bool {
        matches!(self, Self::Active { .. })
    }

    pub fn activate(self) -> Self {
        match self {
            Self::None => Self::Active { else_seen: false },
            Self::Active { else_seen } | Self::One { else_seen } => Self::Active { else_seen },
        }
    }

    pub fn deactivate(self) -> Self {
        match self {
            Self::Active { else_seen } => Self::One { else_seen },
            other => other,
        }
    }

    pub fn with_else_seen(self, active: bool) -> Self {
        if active {
            match self {
                Self::None => Self::Active { else_seen: true },
                Self::Active { .. } | Self::One { .. } => Self::One { else_seen: true },
            }
        } else {
            Self::One { else_seen: true }
        }
    }
}

#[derive(Debug, Error)]
#[allow(clippy::enum_variant_names)]
pub enum IfError {
    #[error("unmatched #elif directive")]
    ExtraElif,
    #[error("unmatched #else directive")]
    ExtraElse,
    #[error("unmatched #endif directive")]
    ExtraEndIf,
}

impl From<IfError> for ProcessingErrorKind {
    fn from(value: IfError) -> Self {
        match value {
            IfError::ExtraElif => ProcessingErrorKind::ExtraElif,
            IfError::ExtraElse => ProcessingErrorKind::ExtraElse,
            IfError::ExtraEndIf => ProcessingErrorKind::ExtraEndIf,
        }
    }
}

pub struct IfStack {
    stack: Vec<IfState>,
}

impl IfStack {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(4),
        }
    }

    pub fn if_group_active(&self) -> bool {
        let len = self.stack.len();
        if len >= 2 {
            unsafe { self.stack.get_unchecked(len - 2) }.active()
        } else {
            true
        }
    }

    pub fn active(&self) -> bool {
        self.stack.last().map(|top| top.active()).unwrap_or(true)
    }

    pub fn on_if_like(&mut self, expr: bool) {
        if self.active() && expr {
            self.stack.push(IfState::Active { else_seen: false });
        } else {
            self.stack.push(IfState::None);
        }
    }

    pub fn on_elif(&mut self, expr: bool) -> Result<(), IfError> {
        // Pop the current state
        let top = if let Some(top) = self.stack.pop() {
            top
        } else {
            return Err(IfError::ExtraElif);
        };

        // Check that we haven't already seen an else
        if top.else_seen() {
            // If that's the case, just ignore the next block
            self.stack.push(IfState::One { else_seen: true });
            return Err(IfError::ExtraElif);
        }

        // Is the next block active?
        // Note that we use active, since we popped the current level off
        if self.active() && expr {
            self.stack.push(top.activate());
        } else {
            self.stack.push(top.deactivate());
        }

        Ok(())
    }

    pub fn on_else(&mut self) -> Result<(), IfError> {
        // Pop the current state
        let top = if let Some(top) = self.stack.pop() {
            top
        } else {
            return Err(IfError::ExtraElse);
        };

        // Check that we haven't already seen an else
        if top.else_seen() {
            // If that's the case, just ignore the next block
            self.stack.push(IfState::One { else_seen: true });
            return Err(IfError::ExtraElif);
        }

        // The next block will be active if no other block before was active
        self.stack.push(top.with_else_seen(self.active()));

        Ok(())
    }

    pub fn on_endif(&mut self) -> Result<(), IfError> {
        // Pop the current state
        let _top = if let Some(top) = self.stack.pop() {
            top
        } else {
            return Err(IfError::ExtraEndIf);
        };

        // Nothing more to do

        Ok(())
    }
}
