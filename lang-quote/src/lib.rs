//! `glsl-lang-quote` offers proc-macros to quote GLSL syntax in Rust.

#![deny(missing_docs)]

use glsl_lang::{
    ast,
    parse::{Parsable, ParseOptions},
};
use proc_macro2::TokenStream;

use crate::tokenize::Tokenize;

mod quoted;
mod tokenize;

fn glsl_internal<F>(input: proc_macro::TokenStream) -> proc_macro::TokenStream
where
    F: Parsable + Tokenize + std::fmt::Debug,
{
    let s = format!("{}", &input);
    let opts = ParseOptions {
        allow_rs_ident: true,
        ..Default::default()
    }
    .build();
    let parsed: Result<(F, _), _> = Parsable::parse_with_options(&s, &opts);

    if let Ok((tu, _)) = parsed {
        // create the stream and return it
        let mut stream = TokenStream::new();
        tu.tokenize(&mut stream);

        stream.into()
    } else {
        panic!("GLSL error: {:?}", parsed);
    }
}

/// Parse a translation unit at compile time
#[proc_macro]
pub fn glsl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::TranslationUnit>(input)
}

/// Parse a statement at compile time
#[cfg(any(feature = "quote-statement", feature = "quote-parsable"))]
#[proc_macro]
pub fn glsl_statement(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::Statement>(input)
}

/// Parse an expression at compile time
#[cfg(any(feature = "quote-expr", feature = "quote-parsable"))]
#[proc_macro]
pub fn glsl_expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::Expr>(input)
}
