use std::path::Path;
use std::{
    fs::{self, File},
    io::prelude::*,
    path::PathBuf,
};

use glsl_lang_pp::{
    parse,
    processor::{nodes::DirectiveExt, DirectiveKind, ErrorKind, Event},
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
    let input = {
        let raw_bytes = fs::read(&path).expect("failed to read input file");
        match String::from_utf8(raw_bytes) {
            Ok(string) => string,
            Err(error) => encoding_rs::WINDOWS_1252
                .decode(error.as_bytes())
                .0
                .to_string(),
        }
    };

    let paths = Paths::new(&path);
    let result = parse(&input);

    // Get the syntax tree
    let (root, _, _) = result.into_inner();

    // Check that we parsed the file with exact positions. If stages 0 or 1 fail, this should break
    assert_eq!(u32::from(root.text_range().end()), input.len() as u32);

    // Write the resulting tree, even if there are errors
    fs::write(&paths.parsed, format!("{:#?}", root)).expect("failed to write .parsed");

    // Write the result
    let mut pp = glsl_lang_pp::processor::StdProcessor::default();

    let mut eventsf = File::create(&paths.events).unwrap();
    let mut ppf = File::create(&paths.pp).unwrap();
    let mut errorsf = File::create(&paths.errors).unwrap();

    let mut unhandled_count = 0;
    let mut error_count = 0;

    for event in pp.process(path) {
        writeln!(eventsf, "{:?}", event).unwrap();

        match event {
            Event::Error(error) => {
                match error.kind() {
                    ErrorKind::Io(_) => {}
                    ErrorKind::Parse(_) => {}
                    ErrorKind::Processing(_) => {}
                    ErrorKind::Unhandled(node_or_token) => {
                        unhandled_count += 1;

                        if let NodeOrToken::Node(node) = node_or_token {
                            write!(ppf, "{}", node.text()).unwrap();
                        }
                    }
                }

                error_count += 1;
                writeln!(errorsf, "{}", error).unwrap();
            }

            Event::EnterFile { .. } => {}

            Event::Token(token) => {
                write!(ppf, "{}", token.text()).unwrap();
            }

            Event::Directive(directive) => match directive {
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
