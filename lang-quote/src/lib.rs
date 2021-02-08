use glsl_lang::{ast, parse::Parse};
use proc_macro2::TokenStream;

use crate::tokenize::Tokenize;

mod quoted;
mod tokenize;

fn glsl_internal<F>(input: proc_macro::TokenStream) -> proc_macro::TokenStream
where
    F: Parse + Tokenize + std::fmt::Debug,
{
    let s = format!("{}", &input);
    let parsed: Result<F, _> = Parse::parse(&s);

    if let Ok(tu) = parsed {
        // create the stream and return it
        let mut stream = TokenStream::new();
        tu.tokenize(&mut stream);

        stream.into()
    } else {
        panic!("GLSL error: {:?}", parsed);
    }
}

#[proc_macro]
pub fn glsl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::TranslationUnit>(input)
}

#[proc_macro]
pub fn glsl_statement(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::Statement>(input)
}

#[proc_macro]
pub fn glsl_statements(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::CompoundStatement>(input)
}

#[proc_macro]
pub fn glsl_expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    glsl_internal::<ast::Expr>(input)
}
