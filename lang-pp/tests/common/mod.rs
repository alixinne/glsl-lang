use std::path::Path;
use std::{
    fs::{self, File},
    io::prelude::*,
    path::PathBuf,
};

use glsl_lang_pp::{
    exts::DEFAULT_REGISTRY,
    last::fs::Event,
    processor::{
        event::{DirectiveKind, TokenLike},
        ProcessorState,
    },
};

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
    let parsed = match pp.parse(&path, None) {
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
        write!(f, "{:#?}", parsed.ast().into_inner().0).unwrap();
    }

    let mut eventsf = File::create(&paths.events).unwrap();
    let mut ppf = File::create(&paths.pp).unwrap();
    let mut errorsf = File::create(&paths.errors).unwrap();

    let mut error_count = 0;

    for event in parsed
        .process(ProcessorState::default())
        .tokenize(false, &DEFAULT_REGISTRY)
    {
        writeln!(eventsf, "{:?}", event).unwrap();

        match event {
            Event::IoError(err) => {
                error_count += 1;
                writeln!(errorsf, "{}", err).unwrap();
            }

            Event::Error { error, masked } => {
                if !masked {
                    error_count += 1;
                    writeln!(errorsf, "{}", error).unwrap();
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

            Event::Directive {
                node,
                kind,
                masked,
                errors,
            } => {
                if !masked {
                    for error in errors {
                        error_count += 1;
                        writeln!(errorsf, "{}", error).unwrap();
                    }

                    match kind {
                        DirectiveKind::Version(_)
                        | DirectiveKind::Pragma(_)
                        | DirectiveKind::Extension(_) => {
                            write!(ppf, "{}", node).unwrap();
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    if error_count == 0 {
        drop(errorsf);
        fs::remove_file(&paths.errors).unwrap();
    }
}
