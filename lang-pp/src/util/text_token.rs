use std::ops::{Deref, DerefMut};

use lang_util::TextRange;

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
