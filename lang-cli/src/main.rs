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

use anyhow::bail;

use glsl_lang::ast::{NodeDisplay, TranslationUnit};
use glsl_lang::parse::Parse;

fn output_text(output: &mut dyn std::io::Write, tu: TranslationUnit) -> anyhow::Result<()> {
    writeln!(output, "{}", tu.display())?;
    Ok(())
}

#[cfg(feature = "json")]
fn output_json(output: &mut dyn std::io::Write, tu: TranslationUnit) -> anyhow::Result<()> {
    serde_json::to_writer(output, &tu)?;
    Ok(())
}

/// CLI entry point
fn main() -> anyhow::Result<()> {
    let mut args = pico_args::Arguments::from_env();

    // Figure out output format
    let output_fn = match args
        .opt_value_from_str("--format")?
        .unwrap_or("text".to_owned())
        .as_str()
    {
        "text" => output_text,
        #[cfg(feature = "json")]
        "json" => output_json,
        other => bail!("unknown output format: {}", other),
    };

    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s)?;

    match glsl_lang::ast::TranslationUnit::parse(s.as_str()) {
        Ok(tu) => {
            output_fn(&mut std::io::stdout(), tu)?;
        }
        Err(error) => {
            eprintln!("error: {:?}", error);
        }
    }

    Ok(())
}
