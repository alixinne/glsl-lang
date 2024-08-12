use std::{fs::File, io::prelude::*, path::Path};

use glsl_lang_pp::{
    exts::{names::ExtNameAtom, DEFAULT_REGISTRY},
    last::Event,
    processor::{event::DirectiveKind, nodes::ExtensionName},
};
use lang_util_dev::test_util::PathKey;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, lang_util_dev::Display)]
enum Output {
    #[display("parsed")]
    Parsed,
    #[display("events")]
    Events,
    #[display("pp")]
    Preprocessed,
    #[display("errors")]
    Errors,
}

const ALL_OUTPUTS: &[Output] = &[
    Output::Parsed,
    Output::Events,
    Output::Preprocessed,
    Output::Errors,
];

impl PathKey for Output {
    fn all() -> &'static [Self] {
        ALL_OUTPUTS
    }
}

type Paths = lang_util_dev::test_util::Paths<Output>;

fn inspect_extension(name: &str, critical_error_count: &mut usize) {
    if name.starts_with("GL_")
        && !["GL_ES", "GL_SPIRV"].contains(&name)
        && DEFAULT_REGISTRY.get(&ExtNameAtom::from(name)).is_none()
    {
        eprintln!("unsupported extension: {}", name);
        *critical_error_count += 1;
    }
}

pub fn test_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let mut pp = glsl_lang_pp::processor::fs::StdProcessor::default();

    let paths = Paths::new(path).unwrap();

    let parsed = match pp.parse(path) {
        Ok(inner) => Ok(inner),
        Err(err) => {
            if err.kind() == std::io::ErrorKind::InvalidData {
                std::fs::read(path).map(|value| {
                    pp.parse_source(
                        encoding_rs::WINDOWS_1252.decode(&value).0.as_ref(),
                        path.parent().unwrap(),
                    )
                })
            } else {
                Err(err)
            }
        }
    }
    .expect("failed to open file");

    // Write the resulting tree, even if there are errors
    {
        let mut f = File::create(paths.path(Output::Parsed)).unwrap();
        write!(f, "{:#?}", parsed.ast().into_inner().0).unwrap();
    }

    let mut eventsf = File::create(paths.path(Output::Events)).unwrap();
    let mut ppf = File::create(paths.path(Output::Preprocessed)).unwrap();
    let mut errorsf = File::create(paths.path(Output::Errors)).unwrap();

    // Redaction regex
    // Replace Os specific info in errors
    let err_os_redactor = regex::Regex::new("Os \\{ [^\\}]* \\}").unwrap();
    // Replace Os specific messages in stdout
    let stdout_matchers = [
        (
            regex::Regex::new(r#"The system cannot find the (path|file) specified. \(os error \d+\)"#)
                .unwrap(),
            "No such file or directory (os error 2)",
        ),
        (
            regex::Regex::new(
                r#"The filename, directory name, or volume label syntax is incorrect. \(os error \d+\)"#,
            )
            .unwrap(),
            "No such file or directory (os error 2)",
        ),
    ];

    // Error writer that does the replacements
    let mut write_error = |err: &dyn std::fmt::Display| {
        let mut formatted = err.to_string();

        for (regex, replacement) in &stdout_matchers {
            formatted = regex.replace(&formatted, *replacement).to_string();
        }

        if let Some(idx) = formatted.find(": ") {
            formatted = formatted[..idx].replace('\\', "/") + &formatted[idx..];
        }

        writeln!(errorsf, "{}", formatted).unwrap();
    };

    let mut critical_error_count = 0;

    for result in parsed.into_iter().tokenize(100, false, &DEFAULT_REGISTRY) {
        // Redact repository path from EnterFile events
        let debug_formatted = format!("{:?}", result);

        // TODO: Make this more robust
        let redacted = if debug_formatted.contains("EnterFile") {
            let marker = ", canonical_path: ";
            let idx = debug_formatted.find(marker).unwrap();
            let without_canonical_path = String::from(&debug_formatted[..idx]) + " })";
            if without_canonical_path.contains("\\\"") {
                without_canonical_path
                    .replace("\\\\", "/")
                    .replace("\\\"", "\"")
            } else {
                without_canonical_path.replace("\\\\", "/")
            }
        } else if debug_formatted.contains("inner: Os") {
            err_os_redactor
                .replace(&debug_formatted, "Os")
                .replace("\\\\", "/")
                .to_string()
        } else {
            debug_formatted
        };

        writeln!(eventsf, "{}", redacted).unwrap();

        match result {
            Ok(event) => match event {
                Event::Error { error, masked } => {
                    if !masked {
                        write_error(&error);
                    }
                }

                Event::EnterFile { .. } => {}

                Event::Token {
                    source_token,
                    token_kind: _,
                    state,
                } => {
                    if state.active() {
                        write!(ppf, "{}", source_token.text()).unwrap();
                    }
                }

                Event::Directive { directive, masked } => {
                    if !masked {
                        for error in directive.errors() {
                            write_error(&error);
                        }

                        match directive.kind() {
                            DirectiveKind::Version(_) | DirectiveKind::Pragma(_) => {
                                write!(ppf, "{}", directive).unwrap();
                            }
                            DirectiveKind::Extension(ext) => {
                                write!(ppf, "{}", directive).unwrap();

                                if let ExtensionName::Specific(name) = &ext.name {
                                    inspect_extension(name, &mut critical_error_count);
                                }
                            }
                            DirectiveKind::IfDef(ifdef) => {
                                let name = &ifdef.ident;
                                inspect_extension(name, &mut critical_error_count);
                            }
                            _ => {}
                        }
                    }
                }
            },

            Err(err) => {
                write_error(&err);
            }
        }
    }

    // Drop everything to flush output to disk
    drop(eventsf);
    drop(ppf);
    drop(errorsf);

    // Check there are no critical preprocessor errors
    assert_eq!(critical_error_count, 0);

    // Check outputs
    paths.finish();
}
