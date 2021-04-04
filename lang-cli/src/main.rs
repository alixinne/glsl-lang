//! `glsl-lang` debugging CLI. Prints GLSL ASTs to the standard output.

#![deny(missing_docs)]

use std::io::prelude::*;

use glsl_lang::ast::NodeDisplay;
use glsl_lang::parse::Parse;

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
