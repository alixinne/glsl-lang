use darling::{FromField, FromVariant};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data, DeriveInput};

#[derive(Default, FromMeta)]
struct DisplayFieldOpts {
    /// Skip formatting this field or variant
    #[darling(default)]
    skip: bool,
    /// Use this field when formatting extra data
    #[darling(default)]
    extra: bool,
}

#[derive(Default, FromMeta)]
struct DisplayVariantOpts {
    /// Value to use as extra format instead of the variant name
    #[darling(default)]
    extra: Option<String>,
}

#[derive(FromField)]
#[darling(attributes(lang_util))]
struct NodeDisplayField {
    ident: Option<syn::Ident>,
    #[darling(default)]
    display: DisplayFieldOpts,
}

#[derive(FromVariant)]
#[darling(attributes(lang_util))]
struct NodeDisplayVariant {
    ident: syn::Ident,
    fields: darling::ast::Fields<NodeDisplayField>,
    #[darling(default)]
    display: DisplayVariantOpts,
}

fn is_unit_enum(en: &syn::DataEnum) -> bool {
    en.variants.iter().all(|variant| {
        NodeDisplayVariant::from_variant(variant)
            .map(|dv| dv.fields.style == darling::ast::Style::Unit)
            .unwrap_or(false)
    })
}

pub(crate) fn node_content_display(
    input: &DeriveInput,
    opts: &super::NodeContentOpts,
    struct_name: &TokenStream,
    node_name: &str,
) -> TokenStream {
    let display_extra_impl = {
        let mut ts = TokenStream::new();

        // TODO: Support `extra` on variant struct fields

        match &input.data {
            Data::Struct(st) => {
                for (i, field) in st.fields.iter().enumerate() {
                    let df = NodeDisplayField::from_field(&field)
                        .expect("failed to parse field attributes");

                    let ident = if let Some(id) = &df.ident {
                        quote! { #id }
                    } else {
                        let i = syn::Index::from(i);
                        quote! { #i }
                    };

                    if df.display.extra {
                        ts.extend(quote_spanned! {
                            field.span() =>
                                write!(f, " `{}`", self.#ident)?;
                        });
                    }
                }
            }
            Data::Enum(en) => {
                let mut match_body = TokenStream::new();

                // Are the variants all units?
                for variant in &en.variants {
                    let dv = NodeDisplayVariant::from_variant(&variant)
                        .expect("failed to parse variant attributes");

                    let name = &dv.ident;

                    // Find out how to display the enum variant
                    let vs = dv
                        .display
                        .extra
                        .clone()
                        .unwrap_or_else(|| dv.ident.to_string());

                    // Fields pattern
                    let fields = match dv.fields.style {
                        darling::ast::Style::Unit => None,
                        darling::ast::Style::Tuple => {
                            let mut v = Vec::with_capacity(dv.fields.fields.len());
                            let ident = format_ident!("_");
                            for _ in 0..dv.fields.fields.len() {
                                v.push(ident.clone());
                            }
                            Some(quote! { (#(#v),*) })
                        }
                        darling::ast::Style::Struct => Some(quote! { { .. } }),
                    };

                    let quoted = quote_spanned! {
                        variant.span() =>
                            Self::#name#fields=> {
                                write!(f, " `{}`", #vs)?;
                            }
                    };

                    match_body.extend(quoted);
                }

                ts.extend(quote_spanned! {
                    input.span() => match self {
                        #match_body
                    };
                });
            }
            Data::Union(_) => ts.extend(quote_spanned! {
                input.span() =>
                    compile_error!("Unions are not supported");
            }),
        };

        ts.extend(quote! { Ok(()) });
        ts
    };

    let display_children_impl = if opts.display.leaf {
        quote! { Ok(()) }
    } else {
        let mut ts = TokenStream::new();
        ts.extend(quote! { use ::lang_util::node::NodeDisplay; });

        match &input.data {
            Data::Struct(st) => {
                for (i, field) in st.fields.iter().enumerate() {
                    let df = NodeDisplayField::from_field(&field)
                        .expect("failed to parse field attributes");

                    let ident = if let Some(id) = &df.ident {
                        quote! { #id }
                    } else {
                        let i = syn::Index::from(i);
                        quote! { #i }
                    };

                    ts.extend(quote_spanned! {
                        field.span() =>
                            write!(f, "{}", self.#ident.display().set_level(level))?;
                    });
                }
            }
            Data::Enum(en) => {
                let mut match_body = TokenStream::new();

                if !is_unit_enum(&en) {
                    for variant in &en.variants {
                        let dv = NodeDisplayVariant::from_variant(&variant)
                            .expect("failed to parse variant attributes");

                        let name = &dv.ident;

                        if let darling::ast::Style::Unit = dv.fields.style {
                            let vs = dv.ident.to_string();
                            let quoted = quote_spanned! {
                                variant.span() =>
                                    Self::#name => {
                                        write!(f, "{}", ::lang_util::node::NodeDisplayWrapper::new(#vs, level))?;
                                    }
                            };

                            match_body.extend(quoted);
                        } else if let darling::ast::Style::Tuple = dv.fields.style {
                            let mut variant_body = TokenStream::new();
                            let mut field_names = Vec::new();

                            for (i, field) in dv.fields.fields.iter().enumerate() {
                                // Add tuple identifier
                                let ff = if field.display.skip {
                                    format_ident!("_")
                                } else {
                                    format_ident!("tuple{}", i)
                                };

                                field_names.push(ff.clone());

                                // Add code
                                if !field.display.skip {
                                    variant_body.extend(quote! {
                                        write!(f, "{}", #ff.display().set_level(level))?;
                                    });
                                }
                            }

                            match_body.extend(quote_spanned! {
                                variant.span() =>
                                    Self::#name(#(#field_names),*) => {
                                        #variant_body
                                    }
                            });
                        } else {
                            let mut variant_body = TokenStream::new();
                            let mut field_names = Vec::new();

                            for field in &dv.fields.fields {
                                // Add tuple identifier
                                let ff = field.ident.as_ref().unwrap();
                                field_names.push(ff.clone());

                                // Add code
                                if !field.display.skip {
                                    variant_body.extend(quote! {
                                        write!(f, "{}", #ff.display().set_level(level))?;
                                    });
                                }
                            }

                            match_body.extend(quote_spanned! {
                                variant.span() =>
                                    Self::#name { #(#field_names),* } => {
                                        #variant_body
                                    }
                            });
                        }
                    }

                    ts.extend(quote_spanned! {
                        input.span() => match self {
                            #match_body
                        };
                    });
                }
            }
            Data::Union(_) => ts.extend(quote_spanned! {
                input.span() =>
                    compile_error!("Unions are not supported");
            }),
        };

        ts.extend(quote! { Ok(()) });
        ts
    };

    quote! {
        #[automatically_derived]
        impl ::lang_util::node::NodeContentDisplay for #struct_name {
            fn name() -> Option<&'static str> {
               Some(#node_name)
            }

            fn display_extra(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                #display_extra_impl
            }

            fn display_children(&self, level: usize, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                #display_children_impl
            }
        }
    }
}
