use std::{
    borrow::Cow,
    fmt::Write,
    iter::Peekable,
    str::{CharIndices, MatchIndices},
};

use arrayvec::ArrayVec;

use lang_util::SmolStr;

#[derive(Debug, Clone, Copy)]
pub struct Unescaped<'s> {
    src: &'s str,
}

impl<'s> Unescaped<'s> {
    pub fn new(src: &'s str) -> Self {
        Self { src }
    }

    fn backslashes(&self) -> MatchIndices<'s, char> {
        self.src.match_indices('\\')
    }

    pub fn chars(&self) -> UnescapeIter<'s> {
        UnescapeIter {
            chars: self.src.char_indices(),
            backslashes: self.backslashes().peekable(),
        }
    }

    pub fn to_string(self) -> Cow<'s, str> {
        if self.backslashes().next().is_none() {
            Cow::Borrowed(self.src)
        } else {
            Cow::Owned(self.chars().collect::<String>())
        }
    }
}

impl<'s> From<&'s str> for Unescaped<'s> {
    fn from(value: &'s str) -> Self {
        Self::new(value)
    }
}

impl<'s> From<Unescaped<'s>> for SmolStr {
    fn from(src: Unescaped<'s>) -> Self {
        src.chars().collect()
    }
}

impl<'s> PartialEq<&str> for Unescaped<'s> {
    fn eq(&self, other: &&str) -> bool {
        self.chars().eq(other.chars())
    }
}

impl<'s> std::fmt::Display for Unescaped<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ch in self.chars() {
            f.write_char(ch)?;
        }

        Ok(())
    }
}

pub struct UnescapeIter<'s> {
    chars: CharIndices<'s>,
    backslashes: Peekable<MatchIndices<'s, char>>,
}

impl<'s> Iterator for UnescapeIter<'s> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((cont, _)) = self.backslashes.peek() {
                // There is a continuation coming

                // Create a peekable chars iterator
                let mut chars_copy = self.chars.clone().peekable();

                if let Some((i, _)) = chars_copy.peek() {
                    if *i == *cont {
                        // Consume this backslash match
                        self.backslashes.next();

                        // We are at the start of a potential continuation
                        // Skip 1 char (the backslash character)
                        // Collect 2 chars (worst case for a Windows CRLF)
                        let chars: ArrayVec<_, 2> =
                            chars_copy.map(|(_, ch)| ch).skip(1).take(2).collect();

                        // Consume the backslash char
                        self.chars.next();

                        if chars.starts_with(&['\r', '\n']) || chars.starts_with(&['\n', '\r']) {
                            // CRLF, advance thrice, loop again
                            self.chars.next(); // \r
                            self.chars.next(); // \n
                        } else if chars.starts_with(&['\n']) || chars.starts_with(&['\r']) {
                            // LF, advance twice, loop again
                            self.chars.next(); // \n
                        } else {
                            // Stray backslash, just return as-is
                            return Some('\\');
                        }
                    } else {
                        // We haven't reached the continuation yet
                        return self.chars.next().map(|(_, ch)| ch);
                    }
                } else {
                    // Nothing left
                    return None;
                }
            } else {
                // No continuation, i.e. happy path
                return self.chars.next().map(|(_, ch)| ch);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenText<'s>(TokenTextRepr<'s>);

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenTextRepr<'s> {
    Raw(&'s str),
    Unescaped(&'s str),
    JustUnescaped(Cow<'s, str>),
}

impl<'s> TokenText<'s> {
    pub fn push_str(&mut self, rest: TokenText<'_>) {
        let mut self_text = self.to_owned_string();
        self_text.push_str(rest.to_string().as_ref());
        self.0 = TokenTextRepr::JustUnescaped(self_text.into());
    }

    pub fn raw(s: &'s str) -> Self {
        Self(TokenTextRepr::Raw(s))
    }

    pub fn to_owned(&self) -> TokenText<'static> {
        TokenText(TokenTextRepr::JustUnescaped(Cow::Owned(
            self.to_owned_string(),
        )))
    }

    fn to_owned_string(&self) -> String {
        match &self.0 {
            TokenTextRepr::Raw(s) => Unescaped::from(*s).chars().collect(),
            TokenTextRepr::Unescaped(s) => s.to_owned().into(),
            TokenTextRepr::JustUnescaped(s) => (**s).to_owned(),
        }
    }

    pub fn to_string(&self) -> Cow<'s, str> {
        match &self.0 {
            TokenTextRepr::Raw(s) => Cow::Owned(Unescaped::from(*s).chars().collect()),
            TokenTextRepr::Unescaped(s) => (*s).into(),
            TokenTextRepr::JustUnescaped(s) => s.clone(),
        }
    }

    pub fn into_unescaped(self) -> Self {
        Self(match self.0 {
            TokenTextRepr::Raw(s) => {
                TokenTextRepr::JustUnescaped(Cow::Owned(Unescaped::from(s).chars().collect()))
            }
            TokenTextRepr::Unescaped(s) => TokenTextRepr::JustUnescaped(s.into()),
            TokenTextRepr::JustUnescaped(s) => TokenTextRepr::JustUnescaped(s),
        })
    }

    pub fn try_as_str(&'s self) -> Option<&'s str> {
        match &self.0 {
            TokenTextRepr::Raw(_) => None,
            TokenTextRepr::Unescaped(s) => Some(*s),
            TokenTextRepr::JustUnescaped(s) => Some((*s).as_ref()),
        }
    }

    pub unsafe fn unescaped(s: &'s str) -> Self {
        Self(TokenTextRepr::Unescaped(s))
    }
}

impl<'s> std::fmt::Display for TokenText<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            TokenTextRepr::Raw(s) => write!(f, "{}", Unescaped::from(*s)),
            TokenTextRepr::Unescaped(s) => write!(f, "{s}"),
            TokenTextRepr::JustUnescaped(s) => write!(f, "{s}"),
        }
    }
}

impl<'s> From<TokenText<'s>> for SmolStr {
    fn from(value: TokenText<'s>) -> Self {
        match value.0 {
            TokenTextRepr::Raw(raw) => Unescaped::from(raw).into(),
            TokenTextRepr::Unescaped(unescaped) => unescaped.into(),
            TokenTextRepr::JustUnescaped(unescaped) => unescaped.into(),
        }
    }
}
