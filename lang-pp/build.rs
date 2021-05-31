use std::env;
use std::fs;
use std::io::prelude::*;
use std::path::PathBuf;

use heck::SnakeCase;

const EXCLUDE_PREFIXES: &[&str] = &[
    "hlsl.", "spv.", /* TODO: Remove this when we support attributes */
];

const SHADER_EXTS: &[&str] = &[
    "mesh", "tese", "rgen", "tesc", "geom", "comp", "vert", "frag",
];

fn main() {
    let current_dir = env::current_dir().expect("failed to read current dir");
    let files: Vec<PathBuf> = {
        fs::read_dir(current_dir.join("../glslang/Test"))
            .map(|dir| {
                dir.into_iter()
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| {
                        entry
                            .file_name()
                            .to_str()
                            .map(|file_name| {
                                !EXCLUDE_PREFIXES
                                    .iter()
                                    .any(|prefix| file_name.starts_with(prefix))
                                    && SHADER_EXTS.iter().any(|ext| file_name.ends_with(ext))
                            })
                            .unwrap_or(false)
                    })
                    .map(|entry| {
                        entry
                            .path()
                            .strip_prefix(&current_dir)
                            .expect("failed to strip current dir")
                            .to_owned()
                    })
                    .collect()
            })
            .unwrap_or_else(|_| vec![])
    };

    let mut f =
        fs::File::create(PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("glslang_tests.rs"))
            .expect("failed to open output file");

    for test_case in files.into_iter() {
        writeln!(
            f,
            "#[test]
fn test_{test_name}() {{
    common::test_file(r#\"{test_path}\"#);
}}",
            test_name = test_case
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_snake_case()
                .replace(".", "_")
                .replace("__", "_"),
            test_path = test_case.to_string_lossy()
        )
        .unwrap();
    }
}
