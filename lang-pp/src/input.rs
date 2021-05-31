use std::{iter::Peekable, str::CharIndices};

use rowan::{TextRange, TextSize};

// A preprocessor input
pub trait Input {
    fn next_char(&mut self) -> Option<char>;
    fn peek_char(&mut self) -> Option<char>;
    fn current_pos(&self) -> TextRange;
    fn slice(&self) -> &str;
    fn end(&self) -> TextRange;
}

pub struct StringInput<'s> {
    src: &'s str,
    chars: Peekable<CharIndices<'s>>,
    current_pos: TextRange,
}

impl<'s> From<&'s str> for StringInput<'s> {
    fn from(src: &'s str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
            current_pos: Default::default(),
        }
    }
}

impl<'s> Input for StringInput<'s> {
    fn next_char(&mut self) -> Option<char> {
        if let Some((pos, ch)) = self.chars.next() {
            self.current_pos = TextRange::new(
                TextSize::from(pos as u32),
                TextSize::from(
                    self.chars
                        .peek()
                        .map(|(pos, _)| *pos as u32)
                        .unwrap_or_else(|| self.src.len() as u32),
                ),
            );

            Some(ch)
        } else {
            self.current_pos = self.end();
            None
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, ch)| *ch)
    }

    fn current_pos(&self) -> TextRange {
        self.current_pos
    }

    fn slice(&self) -> &str {
        self.src
    }

    fn end(&self) -> TextRange {
        TextRange::new(TextSize::of(self.src), TextSize::of(self.src))
    }
}
