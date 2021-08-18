//! Memory based glsl-lang-pp preprocessing lexer

use std::convert::TryInto;

use glsl_lang_pp::{
    exts::DEFAULT_REGISTRY,
    last::{self, Event},
    processor::{
        self,
        str::{ExpandStr, ProcessStrError},
        ProcessorState,
    },
};
use lang_util::error::ResolvedPosition;

use crate::parse::{LangLexer, ParseContext};

use super::{
    core::{self, HandleTokenResult, LexerCore},
    LexicalError,
};

/// glsl-lang-pp memory lexer
pub struct Lexer<'i> {
    inner: last::Tokenizer<'i, ExpandStr>,
    source: &'i str,
    core: LexerCore,
    handle_token: HandleTokenResult<ProcessStrError>,
}

impl<'i> Iterator for Lexer<'i> {
    type Item = core::Item<ProcessStrError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Pop pending events
            if let Some(item) = self.handle_token.pop_item() {
                return Some(item);
            }

            if let Some(result) = self.handle_token.pop_event().or_else(|| self.inner.next()) {
                match result {
                    Ok(event) => match event {
                        Event::Error { error, masked } => {
                            if let Some(result) = self.core.handle_error(error, masked) {
                                return Some(result);
                            }
                        }

                        Event::EnterFile { .. } => {
                            // Ignore
                        }

                        Event::Token {
                            source_token,
                            token_kind,
                            state,
                        } => {
                            self.core.handle_token(
                                source_token,
                                token_kind,
                                state,
                                &mut self.inner,
                                &mut self.handle_token,
                            );
                        }

                        Event::Directive {
                            node,
                            kind,
                            masked,
                            errors,
                        } => {
                            self.core.handle_directive(node, kind, masked, errors);
                        }
                    },

                    Err(err) => {
                        return Some(self.core.handle_str_err(err, self.inner.location()));
                    }
                }
            } else {
                return None;
            }
        }
    }
}

impl<'i> LangLexer for Lexer<'i> {
    type Input = &'i str;
    type Error = LexicalError<ProcessStrError>;

    fn new(source: Self::Input, opts: ParseContext) -> Self {
        Self {
            inner: processor::str::process(source, ProcessorState::default())
                .tokenize(opts.opts.target_vulkan, &DEFAULT_REGISTRY),
            source,
            core: LexerCore::new(opts),
            handle_token: Default::default(),
        }
    }

    fn chain<P: crate::parse::LangParser<Self>>(
        &mut self,
        parser: &P,
    ) -> Result<P::Item, crate::parse::ParseError<Self>> {
        let source = self.source;
        parser.parse(source, self).map_err(|err| {
            let location = self.inner.location();
            let lexer = lang_util::error::error_location(&err);
            let (line, col) = location.offset_to_line_and_col(
                lexer.offset.try_into().expect("input length out of range"),
            );
            let position = ResolvedPosition::new_resolved(lexer, line as _, col as _);
            lang_util::error::ParseError::new_resolved(err, position)
        })
    }
}
