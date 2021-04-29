//! `lang-util-derive` is a proc-macro crate providing automatically derived implementations of the
//! traits provided by [`lang-util`](https://crates.io/crates/lang-util).
//!
//! These macros are re-exported by the `lang-util` crate, which you should depend on instead.

#![deny(missing_docs)]

#[macro_use]
extern crate darling;

mod node_content;

/// Mark a type as representing data in an AST node
///
/// # Examples
///
/// If the name of the data structure doesn't end with `Data`, the `lang_util::NodeContent` trait
/// will just be implemented for that type.
/// ```ignore
/// use lang_util_derive::NodeContent;
///
/// #[derive(NodeContent)]
/// pub struct Declaration {
///     pub name: String,
/// }
/// ```
///
/// If the name of the data structure ends with `Data`, the `lang_util::NodeContent` trait will be
/// implemented for that type, and a type alias for `Node<T>` will be declared for the suffix-less
/// name.
/// ```ignore
/// use lang_util_derive::NodeContent;
///
/// #[derive(NodeContent)]
/// pub struct DeclarationData {
///     pub name: String,
/// }
///
/// // Will declare pub type Declaration = ::lang_util::node::Node<DeclarationData>;
/// ```
#[proc_macro_derive(NodeContent, attributes(lang_util))]
pub fn node_content(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    node_content::node_content(input)
}

mod token;

/// Derive `lang_util::error::Token` for an enum usable with Logos
#[proc_macro_derive(Token, attributes(lang_util))]
pub fn token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    token::token(input)
}
