use std::path::Path;
use std::{io::prelude::*, path::PathBuf};

use glsl_lang_pp::{
    parse,
    processor::{nodes::DirectiveExt, DirectiveKind, ErrorKind, Event},
};

fn out_path(path: &Path, ext: &str) -> PathBuf {
    let file_name = path.file_name().unwrap().to_string_lossy().to_string() + ext;
    let dir_name = path.parent().unwrap();
    let mut result = dir_name.to_owned();
    result.push("localRsResults");

    std::fs::create_dir_all(&result).unwrap();

    result.push(file_name);
    result
}

pub fn test_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let input = {
        let raw_bytes = std::fs::read(&path).expect("failed to read input file");
        match String::from_utf8(raw_bytes) {
            Ok(string) => string,
            Err(error) => encoding_rs::WINDOWS_1252
                .decode(error.as_bytes())
                .0
                .to_string(),
        }
    };

    let result = parse(&input);

    // Get the syntax tree
    let (root, _) = result.into_inner();

    // Check that we parsed the file with exact positions. If stages 0 or 1 fail, this should break
    assert_eq!(u32::from(root.text_range().end()), input.len() as u32);

    // Write the resulting tree, even if there are errors
    std::fs::write(out_path(path, ".parsed"), format!("{:#?}", root))
        .expect("failed to write .parsed");

    // Write the result
    let mut pp = glsl_lang_pp::processor::StdProcessor::default();

    let events_file = out_path(path, ".events");
    let mut eventsf = std::fs::File::create(events_file).unwrap();
    let pp_file = out_path(path, ".pp");
    let mut ppf = std::fs::File::create(pp_file).unwrap();
    let errors_file = out_path(path, ".errors");
    let mut errorsf = std::fs::File::create(&errors_file).unwrap();

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
                    ErrorKind::Unhandled(node) => {
                        unhandled_count += 1;
                        write!(ppf, "{}", node.text()).unwrap();
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
        std::fs::remove_file(&errors_file).unwrap();
    }

    assert_eq!(unhandled_count, 0, "number of unhandled events should be 0");
}
