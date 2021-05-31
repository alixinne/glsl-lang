use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use rowan::TextRange;

mod input;
pub use input::*;

mod lexer;

mod parser;

/// Unique file identifier
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileId(u32);

impl FileId {
    pub fn new(raw: u32) -> Self {
        Self(raw)
    }
}

// A token somewhere in a source file
#[derive(Debug, Clone, Copy)]
pub struct TextToken<T> {
    pub token: T,
    pub range: TextRange,
}

impl<T> TextToken<T> {
    pub fn new(token: T, range: TextRange) -> Self {
        Self { token, range }
    }

    pub fn raw<'s>(&self, input: &'s str) -> &'s str {
        &input[self.range]
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> TextToken<U> {
        TextToken {
            token: f(self.token),
            range: self.range,
        }
    }

    pub fn transmute<U: From<T>>(self) -> TextToken<U> {
        TextToken {
            token: self.token.into(),
            range: self.range,
        }
    }
}

impl<T> Deref for TextToken<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<T> DerefMut for TextToken<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

pub fn unescape_line_continuations(raw: &str) -> Cow<str> {
    // TODO: More efficient?
    if let Some(_) = raw.find('\\') {
        let mut s = String::with_capacity(raw.len());
        for ch in raw.chars() {
            s.push(ch);
            if ch == '\n' {
                if s.ends_with("\\\r\n") {
                    s.pop();
                    s.pop();
                    s.pop();
                } else if s.ends_with("\\\n") {
                    s.pop();
                    s.pop();
                }
            }
        }

        Cow::Owned(s)
    } else {
        Cow::Borrowed(raw)
    }
}

// TODO: Remove this
pub use parser::Ast;
pub fn parse(input: &str) -> Ast {
    parser::Parser::new(StringInput::from(input)).parse()
}
