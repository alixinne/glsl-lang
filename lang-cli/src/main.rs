//! [`glsl-lang`](https://crates.io/crates/glsl-lang) debugging CLI.
//!
//! *This is only a prototype for debugging, more options will be added in later updates.*
//!
//! # Usage
//!
//! Print GLSL AST to the standard output:
//! ```bash
//! $ cargo run < source.glsl
//! TranslationUnit
//!   ExternalDeclaration@0:0..45 `Declaration`
//!     Declaration@0:0..45 `Block`
//!       [...]
//! ```

#![deny(missing_docs)]

use std::io::prelude::*;

use glsl_lang::ast::NodeDisplay;
use glsl_lang::parse::Parse;

/// CLI entry point
fn main() -> anyhow::Result<()> {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s)?;
    match glsl_lang::ast::TranslationUnit::parse(s.as_str()) {
        Ok(tu) => {
            println!("{}", tu.display());
        }
        Err(error) => {
            eprintln!("error: {:?}", error);
        }
    }
    Ok(())
}
