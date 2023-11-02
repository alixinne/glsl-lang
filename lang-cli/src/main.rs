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

use std::{io::prelude::*, path::Path};

use argh::FromArgs;

use glsl_lang::{
    ast::{NodeDisplay, TranslationUnit},
    lexer::v2_full::fs::PreprocessorExt,
    parse::IntoParseBuilderExt,
};

fn output_text(output: &mut dyn std::io::Write, tu: TranslationUnit) -> std::io::Result<()> {
    writeln!(output, "{}", tu.display())?;
    Ok(())
}

#[cfg(feature = "json")]
fn output_json(output: &mut dyn std::io::Write, tu: TranslationUnit) -> std::io::Result<()> {
    serde_json::to_writer(output, &tu)?;
    Ok(())
}

fn output_glsl(output: &mut dyn std::io::Write, tu: TranslationUnit) -> std::io::Result<()> {
    let mut s = String::new();

    glsl_lang::transpiler::glsl::show_translation_unit(
        &mut s,
        &tu,
        glsl_lang::transpiler::glsl::FormattingState::default(),
    )
    .unwrap();

    write!(output, "{}", s)?;

    Ok(())
}

#[derive(Debug, FromArgs)]
/// glsl-lang command-line interface
struct Opts {
    #[argh(option, default = "\"text\".to_owned()")]
    /// output format (text, json or glsl)
    format: String,

    #[argh(positional)]
    /// input file path
    path: Option<String>,
}

use miette::{Diagnostic, SourceSpan};

#[derive(Debug, Diagnostic)]
#[diagnostic(code(glsl_lang::parse::error))]
struct ParseError<I: std::error::Error + 'static> {
    inner: lang_util::located::Located<I>,
    #[source_code]
    src: NamedSource,
    snip: SourceSpan,
    #[label = "Error occurred here."]
    bad_bit: SourceSpan,
}

impl<I: std::error::Error + 'static> std::error::Error for ParseError<I> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.inner.inner())
    }
}

impl<I: std::error::Error> std::fmt::Display for ParseError<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Failed to parse input GLSL at line {} column {}.",
            self.inner.line() + 1,
            self.inner.col() + 1
        )
    }
}

use miette::{NamedSource, Result};
fn parse_tu(source: &str, path: &str) -> Result<glsl_lang::ast::TranslationUnit> {
    let mut processor = glsl_lang_pp::processor::fs::StdProcessor::new();
    let tu: Result<glsl_lang::ast::TranslationUnit, _> = processor
        .open_source(
            source,
            Path::new(path).parent().unwrap_or_else(|| Path::new(".")),
        )
        .builder()
        .parse()
        .map(|(mut tu, _, iter)| {
            iter.into_directives().inject(&mut tu);
            tu
        });

    match tu {
        Ok(tu) => Ok(tu),
        Err(err) => {
            let pos = err.pos();

            // Find 2 lines before and after
            let start = usize::from(pos.start());
            let end = usize::from(pos.end());

            // TODO: '\n' isn't what GLSL calls a line
            let before = source
                .rmatch_indices('\n')
                .filter(|(i, _ch)| *i < start)
                .map(|(i, _ch)| i + 1)
                .nth(2)
                .unwrap_or(0);

            let after = source
                .match_indices('\n')
                .filter(|(i, _ch)| *i > end)
                .map(|(i, _ch)| i)
                .nth(2)
                .unwrap_or(source.len());

            Err(ParseError {
                inner: err,
                src: NamedSource::new(path, source.to_string()),
                snip: (before, after.saturating_sub(before)).into(),
                bad_bit: (usize::from(pos.start()), usize::from(pos.len())).into(),
            }
            .into())
        }
    }
}

/// CLI entry point
fn main() -> Result<(), std::io::Error> {
    let args: Opts = argh::from_env();

    // Figure out output format
    let output_fn = match args.format.as_str() {
        "text" => output_text,
        #[cfg(feature = "json")]
        "json" => output_json,
        "glsl" => output_glsl,
        other => panic!("unknown output format: {}", other),
    };

    let mut s = String::new();

    // Read input from argument or stdin
    if let Some(path) = args.path.as_deref() {
        s = std::fs::read_to_string(path)?;
    } else {
        std::io::stdin().read_to_string(&mut s)?;
    }

    match parse_tu(
        s.as_str(),
        &args
            .path
            .as_ref()
            .map(String::to_owned)
            .unwrap_or_else(|| "standard input".to_owned()),
    ) {
        Ok(tu) => {
            output_fn(&mut std::io::stdout(), tu)?;
        }
        Err(diag) => {
            eprintln!("{:?}", diag);
            std::process::exit(1);
        }
    }

    Ok(())
}
