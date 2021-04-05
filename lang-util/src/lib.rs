//! `lang-util` is a crate that implements utilities to parse and represent syntax trees using
//! [`lalrpop`](https://crates.io/crates/lalrpop) and [`logos`](https://crates.io/crates/logos).
//!
//! This crate is tailored for use in the [`glsl-lang`](https://crates.io/crates/glsl-lang) crate,
//! but you may use its utilities for implementing your own language parsers:
//! - [error]: parsing error reporting module, with user-readable location information
//! - [node]: AST node structure and display
//! - [position]: utilities for working with positions in strings

#![deny(missing_docs)]

pub use lang_util_derive::{NodeContent, Token};

pub mod error;
pub use error::Token;

pub mod node;
pub use node::NodeContent;

pub mod position;
