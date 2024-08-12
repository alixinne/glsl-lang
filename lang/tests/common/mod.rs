use std::path::Path;
use std::{fs::File, io::prelude::*};

use glsl_lang::ast::{self, NodeDisplay};
use lang_util_dev::test_util::PathKey;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, lang_util_dev::Display)]
enum Output {
    #[cfg_attr(
        all(
            feature = "lexer-v1",
            not(feature = "lexer-v2-min"),
            not(feature = "lexer-v2-full")
        ),
        display("ast-v1")
    )]
    #[cfg_attr(
        all(feature = "lexer-v2-min", not(feature = "lexer-v2-full")),
        display("ast-v1")
    )]
    #[cfg_attr(feature = "lexer-v2-full", display("ast-v2-full"))]
    Ast,
}

const ALL_OUTPUTS: &[Output] = &[Output::Ast];

impl PathKey for Output {
    fn all() -> &'static [Self] {
        ALL_OUTPUTS
    }
}

type Paths = lang_util_dev::test_util::Paths<Output>;

#[cfg(all(
    any(feature = "lexer-v1", feature = "lexer-v2-min"),
    not(feature = "lexer-v2-full")
))]
fn parse_tu<'i>(
    path: &Path,
) -> Result<
    ast::TranslationUnit,
    glsl_lang::parse::ParseError<
        <glsl_lang::parse::DefaultLexer<'i> as glsl_lang::lexer::HasLexerError>::Error,
    >,
> {
    use glsl_lang::parse::DefaultParse;

    let source = std::fs::read_to_string(&path).expect("failed to parse file");
    glsl_lang::ast::TranslationUnit::parse(&source)
}

#[cfg(feature = "lexer-v2-full")]
fn parse_tu(
    path: &Path,
) -> Result<
    ast::TranslationUnit,
    glsl_lang::parse::ParseError<<glsl_lang::lexer::v2_full::fs::Lexer<glsl_lang_pp::processor::fs::Std> as glsl_lang::lexer::HasLexerError>::Error>,
>{
    use glsl_lang::{
        lexer::v2_full::fs::PreprocessorExt,
        parse::{IntoParseBuilderExt, ParseOptions},
    };

    let mut processor = glsl_lang_pp::processor::fs::StdProcessor::new();
    processor
        .open(path)
        .expect("failed to open file")
        .builder()
        .opts(&ParseOptions {
            default_version: 100,
            ..Default::default()
        })
        .parse()
        .map(|(tu, _, _)| tu)
}

#[cfg(not(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
)))]
fn parse_tu(_path: &Path) -> Result<ast::TranslationUnit, &'static str> {
    panic!("no lexer selected")
}

pub fn test_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let paths = Paths::new(path).unwrap();

    let result = parse_tu(path);

    // Write .ast file
    {
        let mut f = File::create(paths.path(Output::Ast)).unwrap();
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

    paths.finish();
}
