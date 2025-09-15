#![cfg(feature = "full")]

use std::path::PathBuf;

use glsl_lang_pp::{
    exts::DEFAULT_REGISTRY,
    last::{Event, TokenState},
    types::Token,
};

#[test]
fn test_issue_61() {
    // See https://github.com/alixinne/glsl-lang/pull/61
    let path: PathBuf = "../lang-pp/tests/issue_61.glsl".into();
    let mut pp = glsl_lang_pp::processor::fs::StdProcessor::default();

    let parsed = match pp.parse(&path) {
        Ok(inner) => Ok(inner),
        Err(err) => {
            if err.kind() == std::io::ErrorKind::InvalidData {
                std::fs::read(&path).map(|value| {
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

    assert_eq!(
        parsed
            .into_iter()
            .tokenize(300, false, &DEFAULT_REGISTRY)
            .filter_map(|token| {
                // Extract float constants which are not excluded by the preprocessor
                if let Ok(Event::Token {
                    state: TokenState::Active,
                    token_kind: Token::FLOAT_CONST(val),
                    ..
                }) = token
                {
                    return Some(val);
                }

                return None;
            })
            .collect::<Vec<_>>(),
        // Only one pp branch should be active, so only one constant is produced
        vec![1.0]
    );
}
