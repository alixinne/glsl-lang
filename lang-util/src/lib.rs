//! `lang-util` is a crate that implements utilities to parse and represent syntax trees.
//! It also provides error formatting facilities for parsers using
//! [`lalrpop`](https://crates.io/crates/lalrpop) and [`logos`](https://crates.io/crates/logos).
//!
//! This crate is tailored for use in the [`glsl-lang`](https://crates.io/crates/glsl-lang) crate,
//! but you may use its utilities for implementing your own language parsers:
//! - [error]: parsing error reporting module, with user-readable location information. Only
//!   available with the `lalrpop` feature enabled.
//! - [node]: AST node structure and display
//! - [position]: utilities for working with positions in strings

#![deny(missing_docs)]

pub use lang_util_derive::{NodeContentDisplay, Token};

#[cfg(feature = "lalrpop")]
pub mod error;

mod file_id;
pub use file_id::FileId;

pub mod located;

pub mod node;
pub use node::NodeContent;

pub mod position;

pub mod token;
pub use token::Token;

// Re-exports
pub use smol_str::SmolStr;
pub use text_size::{TextRange, TextSize};
