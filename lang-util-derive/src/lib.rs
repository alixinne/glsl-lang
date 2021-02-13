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

        let quoted = quote! {
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
