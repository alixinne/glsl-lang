use std::collections::HashSet;
use std::fs;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use heck::ToSnakeCase;

const EXCLUDE_PREFIXES: &[&str] = &[
    "hlsl.", "spv.", /* TODO: Remove this when we support attributes */
];

const SHADER_EXTS: &[&str] = &[
    "mesh", "tese", "rgen", "tesc", "geom", "comp", "vert", "frag",
];

pub struct DiscoveredTests {
    paths: Vec<PathBuf>,
    ignore_tests: HashSet<String>,
}

impl DiscoveredTests {
    pub fn new(paths: Vec<PathBuf>) -> Self {
        Self {
            paths,
            ignore_tests: std::env::var("IGNORE_TESTS")
                .ok()
                .map(|vars| vars.split(',').map(String::from).collect())
                .unwrap_or_default(),
        }
    }

    pub fn write_entry(&self, target: &Path, header: &str) -> anyhow::Result<()> {
        let mut f = fs::File::create(target)?;

        write!(f, "{}", header)?;
        writeln!(f, "mod common;")?;

        for test_case in &self.paths {
            let test_name = test_case
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_snake_case()
                .replace(".", "_")
                .replace("__", "_");

            writeln!(
                f,
                "#[test]{ignore_attribute}
fn test_{test_name}() {{
    common::test_file(r#\"{test_path}\"#);
}}",
                test_name = test_name,
                test_path = test_case.to_string_lossy(),
                ignore_attribute = if self.ignore_tests.contains(&test_name) {
                    "\n#[ignore]"
                } else {
                    ""
                }
            )?;
        }

        eprintln!("Wrote {}", target.display());

        Ok(())
    }
}

impl From<Vec<PathBuf>> for DiscoveredTests {
    fn from(paths: Vec<PathBuf>) -> Self {
        Self::new(paths)
    }
}

pub fn discover_tests(current_dir: &Path) -> DiscoveredTests {
    fs::read_dir(current_dir.join("data"))
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
                    PathBuf::from("..").join(
                        entry
                            .path()
                            .strip_prefix(&current_dir)
                            .expect("failed to strip current dir"),
                    )
                })
                .collect()
        })
        .unwrap_or_else(|_| Vec::new())
        .into()
}
