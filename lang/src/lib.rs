use lalrpop_util::lalrpop_mod;

pub(crate) mod ast;
mod lexer;
lalrpop_mod!(parser);
pub mod parse;

#[cfg(test)]
mod parse_tests;
