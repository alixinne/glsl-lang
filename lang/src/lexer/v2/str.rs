//! Memory based glsl-lang-pp preprocessing lexer

use glsl_lang_pp::{
    exts::DEFAULT_REGISTRY,
    last::{self, Event},
    processor::{
        self,
        str::{ExpandStr, ProcessStrError},
        ProcessorState,
    },
};

use crate::parse::{LangLexer, ParseContext};

use super::{
    core::{self, HandleTokenResult, LexerCore},
    LexicalError,
};

/// glsl-lang-pp memory lexer
pub struct Lexer<'i> {
    inner: last::Tokenizer<'i, ExpandStr>,
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
                        return Some(Err(LexicalError::Io(err)));
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
                .tokenize(100, opts.opts.target_vulkan, &DEFAULT_REGISTRY),
            core: LexerCore::new(opts),
            handle_token: Default::default(),
        }
    }

    fn chain<P: crate::parse::LangParser<Self>>(
        &mut self,
        parser: &P,
    ) -> Result<P::Item, crate::parse::ParseError<Self>> {
        parser.parse(self).map_err(|err| {
            let location = self.inner.location();
            let lexer = lang_util::error::error_location(&err);

            lang_util::error::ParseError::<Self::Error>::builder()
                .pos(lexer)
                .resolve_file(location)
                .finish(err.into())
        })
    }
}
