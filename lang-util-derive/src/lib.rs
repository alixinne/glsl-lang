//! `lang-util-derive` is a proc-macro crate providing automatically derived implementations of the
//! traits provided by [`lang-util`](https://crates.io/crates/lang-util).
//!
//! These macros are re-exported by the `lang-util` crate, which you should depend on instead.

#![deny(missing_docs)]

#[macro_use]
extern crate darling;

use darling::FromDeriveInput;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

mod content_eq;
use content_eq::node_content_eq;

mod content_display;
use content_display::node_content_display;

#[derive(Default, FromMeta)]
#[darling(default)]
struct NodeDisplay {
    leaf: bool,
}

#[derive(FromDeriveInput)]
#[darling(attributes(lang_util), forward_attrs(allow, doc, cfg))]
struct NodeContentOpts {
    ident: syn::Ident,
    generics: syn::Generics,
    #[darling(default)]
    display: NodeDisplay,
}

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
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    // Find out struct-level options
    let opts = NodeContentOpts::from_derive_input(&input).expect("failed to parse options");

    // Add anonymous lifetimes as needed
    let lifetimes: Vec<_> = opts.generics.lifetimes().map(|_| quote! { '_ }).collect();

    // Build the content_eq method
    let content_eq_body = node_content_eq(&input);

    // Generate the name of the target for usage in impl targets
    let base_ident = &opts.ident;
    let struct_name = if lifetimes.is_empty() {
        quote! { #base_ident }
    } else {
        quote! { #base_ident<#(#lifetimes),*> }
    };

    // Build the output, possibly using quasi-quotation
    let mut expanded = quote! {
      #[automatically_derived]
      impl ::lang_util::node::NodeContent for #struct_name {}
      #[automatically_derived]
      impl ::lang_util::node::NodeContentEq for #struct_name {
        fn content_eq(&self, other: &Self) -> bool {
          #content_eq_body
        }
      }
    };

    // Is this a "Data" node?
    let raw_name = base_ident
        .to_string()
        .strip_suffix("Data")
        .map(|id| format_ident!("{}", id));

    // Add node wrapper
    if let Some(raw_name) = &raw_name {
        let lifetimes: Vec<_> = input.generics.lifetimes().collect();
        let type_name = if lifetimes.is_empty() {
            quote! { #raw_name }
        } else {
            quote! { #raw_name<#(#lifetimes),*> }
        };

        let doc = format!("Type alias for `Node<{}>`", struct_name);
        let quoted = quote! {
          #[doc = #doc]
          pub type #type_name = ::lang_util::node::Node<#struct_name>;
        };

        expanded.extend(quoted);
    };

    // The node name for the NodeDisplay impl
    let node_name = raw_name.unwrap_or_else(|| base_ident.clone()).to_string();

    let display_quoted = node_content_display(&input, &opts, &struct_name, &node_name);

    expanded.extend(display_quoted);

    proc_macro::TokenStream::from(expanded)
}

/// Derive `lang_util::error::Token` for an enum usable with Logos
#[proc_macro_derive(Token, attributes(lang_util))]
pub fn token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    // Add anonymous lifetimes as needed
    let lifetimes: Vec<_> = input.generics.lifetimes().map(|_| quote! { '_ }).collect();

    let base_ident = &input.ident;
    let enum_name = if lifetimes.is_empty() {
        quote! { #base_ident }
    } else {
        quote! { #base_ident<#(#lifetimes),*> }
    };

    let quoted = quote! {
        impl ::lang_util::error::Token  for #enum_name {
        }
    };

    proc_macro::TokenStream::from(quoted)
}
