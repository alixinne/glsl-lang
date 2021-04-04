//! [crate] is a crate that implements utilities to parse and represent syntax trees using
//! `lalrpop` and `logos`.

#![deny(missing_docs)]

pub use lang_util_derive::{NodeContent, Token};

pub mod error;
pub use error::Token;

pub mod node;
pub use node::NodeContent;

pub mod position;
