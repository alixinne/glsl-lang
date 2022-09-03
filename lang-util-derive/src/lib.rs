//! `lang-util-derive` is a proc-macro crate providing automatically derived implementations of the
//! traits provided by [`lang-util`](https://crates.io/crates/lang-util).
//!
//! These macros are re-exported by the `lang-util` crate, which you should depend on instead.

#![deny(missing_docs)]

#[macro_use]
extern crate darling;

mod node_display;

/// Derives an implementation of `NodeContentDisplay` for the given type.
#[proc_macro_derive(NodeContentDisplay, attributes(lang_util))]
pub fn node_display(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    node_display::node_display(input)
}

mod token;

/// Derive `lang_util::error::Token` for an enum usable with Logos
#[proc_macro_derive(Token, attributes(lang_util))]
pub fn token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    token::token(input)
}
