use lalrpop_util::lalrpop_mod;

pub mod ast;
mod lexer;
lalrpop_mod!(
    #[allow(clippy::all)]
    parser
);
pub mod parse;
pub mod transpiler;
pub mod visitor;

#[cfg(test)]
mod parse_tests;
