//! A set of small traits that enable tokenizing some common types that get tokenizing erased
//! normally, such as `Option<T>` as `Some(_)` or `None`, `Box<T>` as `Box::new(_)`, etc.

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

// Quoted type.
pub trait Quoted {
    fn quote(&self) -> TokenStream;
}

impl Quoted for String {
    fn quote(&self) -> TokenStream {
        quote! { #self.to_owned() }
    }
}

impl Quoted for smol_str::SmolStr {
    fn quote(&self) -> TokenStream {
        let s = self.as_str();
        quote! { #s.into() }
    }
}

impl<T> Quoted for Option<T>
where
    T: ToTokens,
{
    fn quote(&self) -> TokenStream {
        if let Some(ref x) = *self {
            quote! { Some(#x) }
        } else {
            quote! { None }
        }
    }
}

impl<T> Quoted for Box<T>
where
    T: ToTokens,
{
    fn quote(&self) -> TokenStream {
        quote! { Box::new(#self) }
    }
}
