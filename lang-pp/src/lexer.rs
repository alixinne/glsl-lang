mod newline;
use newline::*;

mod pre;
use pre::{PreLexer, TextToken as PreTextToken, Token as PreToken};

mod glue;
pub use glue::{Lexer, TextToken, Token};

#[cfg(test)]
mod tests;
