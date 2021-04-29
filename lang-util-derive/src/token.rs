use darling::{FromDeriveInput, FromField, FromMeta, FromVariant};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, DeriveInput};

#[derive(FromField)]
struct TokenVariantField {
    ident: Option<syn::Ident>,
}

struct TokenDisplay {
    format: String,
    args: Vec<String>,
}

impl FromMeta for TokenDisplay {
    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(Self {
            format: value.to_owned(),
            args: vec![],
        })
    }

    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        Ok(Self {
            format: String::from_nested_meta(
                items
                    .first()
                    .ok_or_else(|| darling::Error::custom("missing format string"))?,
            )?,
            args: items
                .iter()
                .skip(1)
                .map(String::from_nested_meta)
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

struct TokenAttr {
    token: String,
}

impl FromMeta for TokenAttr {
    fn from_list(items: &[syn::NestedMeta]) -> darling::Result<Self> {
        Ok(Self {
            token: String::from_nested_meta(
                items
                    .first()
                    .ok_or_else(|| darling::Error::custom("missing token literal"))?,
            )?,
        })
    }
}

#[derive(FromVariant)]
#[darling(attributes(lang_util), forward_attrs(token))]
struct TokenVariant {
    ident: syn::Ident,
    fields: darling::ast::Fields<TokenVariantField>,
    attrs: Vec<syn::Attribute>,
    #[darling(default)]
    display: Option<TokenDisplay>,
}

impl TokenVariant {
    fn display_from(&self, variant: &TokenVariant, declared_fields: &[syn::Ident]) -> TokenStream {
        match variant.fields.style {
            darling::ast::Style::Tuple => {
                if let Some(display) = &variant.display {
                    let fmt = &display.format;

                    // TODO: Better than this ugly format string parsing?
                    let args = if display.args.is_empty() {
                        if declared_fields.len() == 1 && display.format.contains("{}") {
                            let f = &declared_fields[0];
                            vec![quote_spanned! { variant.ident.span() => #f }]
                        } else {
                            declared_fields
                                .iter()
                                .enumerate()
                                .filter_map(|(i, df)| {
                                    let ph = format!("_{}", i);

                                    if fmt.contains(&ph) {
                                        let ph = format_ident!("{}", ph);
                                        Some(quote_spanned! { variant.ident.span() => #ph = #df })
                                    } else {
                                        None
                                    }
                                })
                                .collect()
                        }
                    } else {
                        let mut repl = Vec::new();
                        for (i, df) in declared_fields.iter().enumerate() {
                            repl.push((format!("_{}", i), df.to_string()));
                        }

                        display
                            .args
                            .iter()
                            .map(|arg| {
                                let mut arg = arg.clone();
                                for (src, dst) in &repl {
                                    arg = arg.replace(src, dst);
                                }

                                syn::parse_str(&arg).expect("parsing error")
                            })
                            .collect()
                    };

                    return quote_spanned! {
                        variant.ident.span() =>
                            write!(f, #fmt, #(#args),*)
                    };
                } else {
                    // No display attribute, check if we can have a sensible default
                    if declared_fields.len() == 1 {
                        let v1 = &declared_fields[0];
                        return quote_spanned! {
                            variant.ident.span() =>
                                write!(f, "{}", #v1)
                        };
                    }
                }
            }
            darling::ast::Style::Struct => {
                return quote_spanned! {
                    variant.ident.span() =>
                        compile_error!("struct variants are not supported yet by derive(Token)")
                };
            }
            darling::ast::Style::Unit => {
                // Unit struct, is there a token impl?
                if let Some(token) = variant.attrs.iter().find(|attr| {
                    attr.path
                        .get_ident()
                        .map(|ident| ident == "token")
                        .unwrap_or(false)
                }) {
                    return match token
                        .parse_meta()
                        .map_err(|err| darling::Error::custom(err))
                        .and_then(|meta| TokenAttr::from_meta(&meta))
                    {
                        Ok(value) => {
                            let value = value.token;
                            quote_spanned! { variant.ident.span() => write!(f, "{}", #value) }
                        }
                        Err(error) => {
                            let s = format!("invalid token attribute: {}", error);
                            quote_spanned! {
                                token.path.span() =>
                                    compile_error!(#s)
                            }
                        }
                    };
                } else if let Some(display) = &variant.display {
                    let value = &display.format;
                    return quote_spanned! { variant.ident.span() => write!(f, "{}", #value) };
                }
            }
        }

        quote_spanned! {
            variant.ident.span() =>
                compile_error!("missing token or lang_util(display(...)) attributes")
        }
    }
}

#[derive(FromDeriveInput)]
#[darling(attributes(lang_util))]
struct TokenOpts {
    ident: syn::Ident,
    generics: syn::Generics,
    data: darling::ast::Data<TokenVariant, ()>,
}

fn display_impl(
    base_ident: &syn::Ident,
    enum_name: &TokenStream,
    variants: &[TokenVariant],
) -> TokenStream {
    let mut arms = Vec::with_capacity(variants.len());

    for variant in variants {
        let variant_name = &variant.ident;
        let (variant_header, fields) = match &variant.fields.style {
            darling::ast::Style::Tuple => {
                let fields: Vec<_> = variant
                    .fields
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, _field)| format_ident!("value_{}", i))
                    .collect();

                (
                    quote_spanned! { variant.ident.span() => #variant_name ( #(#fields),* ) },
                    fields,
                )
            }
            darling::ast::Style::Struct => {
                let fields: Vec<_> = variant
                    .fields
                    .fields
                    .iter()
                    .map(|field| field.ident.as_ref().unwrap().clone())
                    .collect();

                (
                    quote_spanned! { variant.ident.span() => #variant_name { #(#fields),* } },
                    fields,
                )
            }
            darling::ast::Style::Unit => (quote! { #variant_name }, vec![]),
        };

        let body = variant.display_from(variant, &fields);

        arms.push(quote_spanned! {
            variant.ident.span() =>
                #base_ident :: #variant_header => { #body }
        });
    }

    quote_spanned! {
        base_ident.span() =>
            impl ::std::fmt::Display for #enum_name {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    match self {
                        #(#arms),*
                    }
                }
            }
    }
}

pub(crate) fn token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let opts = {
        // Parse the input tokens into a syntax tree
        let input = parse_macro_input!(input as DeriveInput);
        // Find out enum-level options
        TokenOpts::from_derive_input(&input).expect("failed to parse options")
    };

    // Extract enum fields
    let fields = match &opts.data {
        darling::ast::Data::Enum(fields) => fields,
        darling::ast::Data::Struct(_) => {
            return proc_macro::TokenStream::from(quote_spanned! {
                opts.ident.span() =>
                    compile_error!("only enums can be used as tokens with derive(Token)")
            })
        }
    };

    // Compute enum name
    let base_ident = &opts.ident;
    let enum_name = {
        // Add anonymous lifetimes as needed
        let lifetimes: Vec<_> = opts.generics.lifetimes().map(|_| quote! { '_ }).collect();

        if lifetimes.is_empty() {
            quote! { #base_ident }
        } else {
            quote! { #base_ident<#(#lifetimes),*> }
        }
    };

    let display_impl = display_impl(&base_ident, &enum_name, &fields);

    proc_macro::TokenStream::from(quote! {
        #display_impl

        impl ::lang_util::error::Token for #enum_name {
        }
    })
}
