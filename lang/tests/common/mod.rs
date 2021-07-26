use std::path::Path;
use std::{
    fs::{self, File},
    io::prelude::*,
    path::PathBuf,
};

use glsl_lang::{ast::NodeDisplay, parse::Parse};

struct Paths {
    ast: PathBuf,
}

impl Paths {
    fn path(base: &Path, file_name: &str, ext: &str) -> PathBuf {
        let mut base = base.to_owned();
        let mut file_name = file_name.to_owned();
        file_name.push_str(ext);
        base.push(file_name);
        base
    }

    pub fn new(path: &Path) -> Self {
        let dir_name = path.parent().unwrap();
        let mut result = dir_name.to_owned();
        result.push("localRsResults");

        fs::create_dir_all(&result).unwrap();

        let file_name = path.file_name().unwrap().to_string_lossy().to_string();

        Self {
            ast: Self::path(&result, &file_name, ".ast"),
        }
    }
}

pub fn test_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let paths = Paths::new(&path);

    let source = std::fs::read_to_string(&path).expect("failed to parse file");
    let result = glsl_lang::ast::TranslationUnit::parse(&source);

    // Write .ast file
    {
        let mut f = File::create(&paths.ast).unwrap();
        match &result {
            Ok(result) => {
                write!(f, "{}", result.0.display()).unwrap();
            }

            Err(err) => {
                write!(f, "{}", err).unwrap();
            }
        }
    }

    assert!(result.is_ok());
}
