use std::io::prelude::*;
use std::path::Path;

use glsl_lang_pp::parse;

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
    let (root, errors) = result.into_inner();

    // Check that we parsed the file with exact positions. If stages 0 or 1 fail, this should break
    assert_eq!(u32::from(root.text_range().end()), input.len() as u32);

    // Write the resulting tree, even if there are errors
    std::fs::write(
        path.with_file_name(path.file_name().unwrap().to_string_lossy().to_string() + ".parsed"),
        format!("{:#?}", root),
    )
    .expect("failed to write .parsed");

    // Check that there are no errors
    let errors_file =
        path.with_file_name(path.file_name().unwrap().to_string_lossy().to_string() + ".errors");
    if !errors.is_empty() {
        let mut f = std::fs::File::create(errors_file).unwrap();

        for error in &errors {
            writeln!(f, "{}", error).unwrap();
        }
    } else {
        std::fs::remove_file(&errors_file).ok();
    }

    assert!(errors.is_empty());
}
