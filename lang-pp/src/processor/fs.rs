use std::path::{Path, PathBuf};

use encoding_rs::Encoding;

use super::Processor;

pub trait FileSystem {
    type Error: std::error::Error + 'static;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error>;
    fn read(&self, path: &Path, encoding: Option<&'static Encoding>)
        -> Result<String, Self::Error>;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Std;

impl FileSystem for Std {
    type Error = std::io::Error;

    fn canonicalize(&self, path: &Path) -> Result<PathBuf, Self::Error> {
        std::fs::canonicalize(path)
    }

    fn read(
        &self,
        path: &Path,
        encoding: Option<&'static Encoding>,
    ) -> Result<String, Self::Error> {
        if let Some(encoding) = encoding {
            let bytes = std::fs::read(path)?;
            Ok(encoding.decode(&bytes).0.to_string())
        } else {
            std::fs::read_to_string(path)
        }
    }
}

pub type StdProcessor = Processor<Std>;
