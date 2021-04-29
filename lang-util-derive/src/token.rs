use quote::quote;
use syn::{parse_macro_input, DeriveInput};

pub(crate) fn token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
