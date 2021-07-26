//! File identifier definition

use std::num::NonZeroU32;

/// Unique file identifier
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(Option<NonZeroU32>);

impl FileId {
    /// Create a new file identifier
    pub fn new(raw: NonZeroU32) -> Self {
        Self(Some(raw))
    }
}

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(raw) = self.0 {
            write!(f, "{}", u32::from(raw) - 1)
        } else {
            write!(f, "builtin")
        }
    }
}
