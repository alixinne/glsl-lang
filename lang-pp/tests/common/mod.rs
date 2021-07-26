use std::path::Path;
use std::{
    fs::{self, File},
    io::prelude::*,
    path::PathBuf,
};

use glsl_lang_pp::processor::{
    event::{DirectiveKind, ErrorKind, IoEvent, TokenLike},
    nodes::DirectiveExt,
};

use rowan::NodeOrToken;

struct Paths {
    parsed: PathBuf,
    events: PathBuf,
    pp: PathBuf,
    errors: PathBuf,
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
            parsed: Self::path(&result, &file_name, ".parsed"),
            events: Self::path(&result, &file_name, ".events"),
            pp: Self::path(&result, &file_name, ".pp"),
            errors: Self::path(&result, &file_name, ".errors"),
        }
    }
}

pub fn test_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let mut pp = glsl_lang_pp::processor::fs::StdProcessor::default();

    let paths = Paths::new(&path);
    let (_, ast) = match pp.parse(&path, None) {
        Ok(inner) => Ok(inner),
        Err(err) => {
            if err.kind() == std::io::ErrorKind::InvalidData {
                pp.parse(&path, Some(encoding_rs::WINDOWS_1252))
            } else {
                Err(err)
            }
        }
    }
    .expect("failed to open file");

    // Write the resulting tree, even if there are errors
    {
        let mut f = File::create(&paths.parsed).unwrap();
        write!(f, "{:#?}", ast.clone().into_inner().0).unwrap();
    }

    let mut eventsf = File::create(&paths.events).unwrap();
    let mut ppf = File::create(&paths.pp).unwrap();
    let mut errorsf = File::create(&paths.errors).unwrap();

    let mut unhandled_count = 0;
    let mut error_count = 0;

    for event in pp.process(path) {
        writeln!(eventsf, "{:?}", event).unwrap();

        match event {
            IoEvent::IoError(_) => {}

            IoEvent::Error(error) => {
                match error.kind() {
                    ErrorKind::Parse(_) => {}
                    ErrorKind::Processing(_) => {}
                    ErrorKind::Unhandled(node_or_token, _) => {
                        unhandled_count += 1;

                        if let NodeOrToken::Node(node) = node_or_token {
                            write!(ppf, "{}", node.text()).unwrap();
                        }
                    }
                }

                error_count += 1;
                writeln!(errorsf, "{}", error).unwrap();
            }

            IoEvent::EnterFile { .. } => {}

            IoEvent::Token(token) => {
                write!(ppf, "{}", token.text()).unwrap();
            }

            IoEvent::Directive(directive) => match directive {
                DirectiveKind::Version(directive) => {
                    write!(ppf, "{}", directive.into_node()).unwrap();
                }
                DirectiveKind::Extension(directive) => {
                    write!(ppf, "{}", directive.into_node()).unwrap();
                }
                DirectiveKind::Define(_) => {}
                DirectiveKind::IfDef(_) => {}
                DirectiveKind::IfNDef(_) => {}
                DirectiveKind::Else => {}
                DirectiveKind::EndIf => {}
                DirectiveKind::Undef(_) => {}
                DirectiveKind::Error(_) => {}
            },
        }
    }

    if error_count == 0 {
        drop(errorsf);
        fs::remove_file(&paths.errors).unwrap();
    }

    assert_eq!(unhandled_count, 0, "number of unhandled events should be 0");
}
