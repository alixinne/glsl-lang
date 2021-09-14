//! Testing utilities

use std::collections::HashMap;
use std::path::Path;
use std::{fs, path::PathBuf};

use similar_asserts::assert_str_eq;

/// Key for differentiating between kinds of test outputs
pub trait PathKey: std::fmt::Display + PartialEq + Eq + std::hash::Hash + Clone + Sized {
    /// Return the list of all possible test output kinds
    fn all() -> &'static [Self];
    /// Return the directory name for the local results
    fn local_results_prefix() -> &'static str {
        "localRsResults"
    }
    /// Return the directory name for the public results
    fn public_results_prefix() -> &'static str {
        "rsResults"
    }
}

/// Test output manager
pub struct Paths<K: PathKey> {
    /// Local (generated) test results
    local_results: PathBuf,
    /// Valid (bumped) test results
    public_results: PathBuf,
    /// Computed file names
    paths: HashMap<K, PathBuf>,
}

impl<K: PathKey + 'static> Paths<K> {
    /// Create a new output manager for the given input path
    ///
    /// # Parameters
    ///
    /// * `input_path`: input file for the current test case
    pub fn new(input_path: &Path) -> std::io::Result<Self> {
        // Create the results directory
        let dir_name = input_path.parent().unwrap();

        let local_results = dir_name.join(K::local_results_prefix());
        let public_results = dir_name.join(K::public_results_prefix());

        for result in &[&local_results, &public_results] {
            fs::create_dir_all(&result)?;
        }

        let file_name = input_path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();

        let paths: HashMap<_, _> = K::all()
            .iter()
            .map(|key| (key.clone(), PathBuf::from(format!("{}.{}", file_name, key))))
            .collect();

        // Pre-run cleanup
        for path in paths.values() {
            let path = local_results.join(path);
            if path.exists() {
                fs::remove_file(path).expect("failed to cleanup result path");
            }
        }

        Ok(Self {
            local_results,
            public_results,
            paths,
        })
    }

    /// Obtain a path for a given output kind
    pub fn path(&self, key: K) -> PathBuf {
        self.local_results.join(self.paths.get(&key).unwrap())
    }

    /// Complete the testing process for this test case
    ///
    /// This will check the local result against the public results to find discrepancies, or
    /// bump the local results if requested.
    ///
    /// To bump results, set LANG_UTIL_TEST=bump before running cargo test.
    pub fn finish(self) {
        #[derive(PartialEq, Eq)]
        enum Mode {
            Check,
            Bump,
        }

        let mode = std::env::var("LANG_UTIL_TEST")
            .ok()
            .map(|value| {
                if value == "bump" {
                    Mode::Bump
                } else {
                    Mode::Check
                }
            })
            .unwrap_or(Mode::Check);

        for (_k, name) in self.paths {
            // Compute full result paths
            let local_result = self.local_results.join(&name);
            let public_result = self.public_results.join(&name);

            if local_result.exists() {
                match mode {
                    Mode::Check => {
                        assert!(public_result.exists(), "missing snapshot");

                        let local = std::fs::read_to_string(&local_result)
                            .expect("failed to read local result");
                        let public = std::fs::read_to_string(&public_result)
                            .expect("failed to read snapshot");

                        assert_str_eq!(local, public, "snapshot mismatch");
                    }
                    Mode::Bump => {
                        std::fs::copy(local_result, public_result)
                            .expect("failed to bump result to snapshot");
                    }
                }
            } else {
                assert!(!public_result.exists(), "missing local result");
            }
        }
    }
}
