use std::path::{Path, PathBuf};

use super::Processor;

pub trait FileSystem {
    type Error: std::error::Error + 'static;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error>;
    fn read(&self, path: &Path) -> Result<String, Self::Error>;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Std;

impl FileSystem for Std {
    type Error = std::io::Error;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error> {
        std::fs::canonicalize(path)
    }

    fn read(&self, path: &Path) -> Result<String, Self::Error> {
        std::fs::read_to_string(path)
    }
}

pub type StdProcessor = Processor<Std>;
