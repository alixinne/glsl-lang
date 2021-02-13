use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data, DeriveInput};

pub(crate) fn node_content_eq(input: &DeriveInput) -> TokenStream {
    match &input.data {
        Data::Struct(ds) => {
            let mut current_expr = None;

            for (id, field) in ds.fields.iter().enumerate() {
                let field_id = field
                    .ident
                    .as_ref()
                    .map(|id| quote! { #id })
                    .unwrap_or_else(|| {
                        let id = syn::Index::from(id);
                        quote! { #id }
                    });

                let this_field_expr = quote! { self.#field_id.content_eq(&other.#field_id) };

                current_expr = Some(match current_expr {
                    Some(before) => quote! { #before && #this_field_expr },
                    None => this_field_expr,
                });
            }

            current_expr.unwrap_or_else(|| quote! { true })
        }
        Data::Enum(de) => {
            let mut arms = Vec::new();

            for variant in &de.variants {
                let variant_name = &variant.ident;

                // Build the result expression
                let mut expr = None;
                // The left variant binding
                let mut bind_left = Vec::new();
                // The right variant binding
                let mut bind_right = Vec::new();

                let (bind_left, bind_right) = match &variant.fields {
                    // Enum variant with named fields: Enum::X { .. }
                    syn::Fields::Named(named) => {
                        for (id, field) in named.named.iter().enumerate() {
                            let field_id = &field.ident;
                            let left_field_id = format_ident!("a{}", id + 1);
                            let right_field_id = format_ident!("b{}", id + 1);

                            bind_left.push(quote! {
                              #field_id: #left_field_id
                            });
                            bind_right.push(quote! {
                              #field_id: #right_field_id
                            });

                            let this_field_expr =
                                quote! { #left_field_id.content_eq(&#right_field_id) };

                            expr = Some(match expr {
                                Some(before) => quote! { #before && #this_field_expr },
                                None => this_field_expr,
                            });
                        }

                        (
                            quote! { { #(#bind_left),* } },
                            quote! { { #(#bind_right),* } },
                        )
                    }
                    // Enum variant with unnamed fields: Enum::X(..)
                    syn::Fields::Unnamed(unnamed) => {
                        for (id, _) in unnamed.unnamed.iter().enumerate() {
                            let left_field_id = format_ident!("a{}", id + 1);
                            let right_field_id = format_ident!("b{}", id + 1);

                            bind_left.push(quote! { #left_field_id });
                            bind_right.push(quote! { #right_field_id });

                            let this_field_expr =
                                quote! { #left_field_id.content_eq(&#right_field_id) };

                            expr = Some(match expr {
                                Some(before) => quote! { #before && #this_field_expr },
                                None => this_field_expr,
                            });
                        }

                        (quote! { (#(#bind_left),*) }, quote! { (#(#bind_right),*) })
                    }
                    // Enum with no fields
                    syn::Fields::Unit => {
                        arms.push(quote! { (Self::#variant_name, Self::#variant_name) => true });
                        continue;
                    }
                };

                let expr = expr.unwrap_or_else(|| quote! { true });
                arms.push(
                    quote! { (Self::#variant_name #bind_left, Self::#variant_name #bind_right) => #expr },
                );
            }

            quote! { match (self, other) {
              #(#arms),*,
              _ => false
            } }
        }
        _ => quote_spanned! {
            input.span() => compile_error!("unsupported type for NodeContent derive");
        },
    }
}
