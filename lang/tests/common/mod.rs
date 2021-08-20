use std::path::Path;
use std::{
    fs::{self, File},
    io::prelude::*,
    path::PathBuf,
};

use glsl_lang::{
    ast::{self, NodeDisplay},
    parse::ParseError,
};

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

#[cfg(all(feature = "lexer-v1", not(feature = "lexer-v2")))]
fn parse_tu(path: &Path) -> Result<ast::TranslationUnit, ParseError<glsl_lang::lexer::v1::Lexer>> {
    use glsl_lang::parse::Parse;

    let source = std::fs::read_to_string(&path).expect("failed to parse file");
    glsl_lang::ast::TranslationUnit::parse(&source)
}

#[cfg(feature = "lexer-v2")]
fn parse_tu(
    path: &Path,
) -> Result<
    ast::TranslationUnit,
    ParseError<glsl_lang::lexer::v2::fs::Lexer<glsl_lang_pp::processor::fs::Std>>,
> {
    use glsl_lang::{lexer::v2::fs::PreprocessorExt, parse::IntoLexerExt};

    let mut processor = glsl_lang_pp::processor::fs::StdProcessor::new();
    processor
        .open(path, None)
        .expect("failed to open file")
        .builder()
        .parse()
        .map(|(tu, _, _)| tu)
}

#[cfg(not(any(feature = "lexer-v1", feature = "lexer-v2")))]
fn parse_tu(path: &Path) -> Result<ast::TranslationUnit, &'static str> {
    panic!("no lexer selected")
}

pub fn test_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let paths = Paths::new(path);

    let result = parse_tu(path);

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
