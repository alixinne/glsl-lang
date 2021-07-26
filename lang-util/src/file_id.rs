//! File identifier definition

/// Unique file identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

const MAX_VALUE: u32 = 0x7FFFFFFF;
const BUILTIN_BIT: u32 = 0x80000000;

impl FileId {
    /// Create a new file identifier
    ///
    /// # Panics
    ///
    /// panics if raw is greater than 0x7FFFFFFF
    pub fn new(raw: u32) -> Self {
        if raw > MAX_VALUE {
            panic!("file identifier is too large");
        }

        Self(raw)
    }

    /// Create a new file identifier for a built-in string
    ///
    /// # Panics
    ///
    /// panics if raw is greater than 0x7FFFFFFE
    pub fn builtin(raw: u32) -> Self {
        if raw > (MAX_VALUE - 1) {
            panic!("file identifier is too large");
        }

        Self(BUILTIN_BIT | raw + 1)
    }
}

impl Default for FileId {
    fn default() -> Self {
        Self(BUILTIN_BIT)
    }
}

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if (self.0 & BUILTIN_BIT) == BUILTIN_BIT {
            let raw = self.0 & !BUILTIN_BIT;

            if raw == 0 {
                write!(f, "internal")
            } else {
                write!(f, "builtin-{}", raw - 1)
            }
        } else {
            write!(f, "{}", self.0)
        }
    }
}
