//! Memory based glsl-lang-pp preprocessing lexer

use glsl_lang_pp::{
    exts::{Registry, DEFAULT_REGISTRY},
    last::{self, Event},
    processor::{
        self,
        str::{ExpandStr, ProcessStrError},
        ProcessorState,
    },
};
use lang_util::FileId;

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
    source_id: FileId,
}

impl<'i> Lexer<'i> {
    pub(crate) fn new_with_state(
        source: &'i str,
        registry: &'i Registry,
        opts: ParseContext,
        state: ProcessorState,
    ) -> Self {
        let source_id = opts.opts.source_id;

        Self {
            inner: processor::str::process(source, state).tokenize(
                opts.opts.default_version,
                opts.opts.target_vulkan,
                registry,
            ),
            core: LexerCore::new(opts),
            handle_token: Default::default(),
            source_id,
        }
    }
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
                        Event::Error { mut error, masked } => {
                            if !masked {
                                error.set_current_file(self.source_id);
                                return Some(Err(error.into()));
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
                            if let Err(errors) =
                                self.core.handle_directive(node, kind, masked, errors)
                            {
                                self.handle_token.push_errors(errors);
                            }
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
        Self::new_with_state(source, &DEFAULT_REGISTRY, opts, ProcessorState::default())
    }

    fn chain<P: crate::parse::LangParser<Self>>(
        &mut self,
        parser: &P,
    ) -> Result<P::Item, crate::parse::ParseError<Self>> {
        parser.parse(self).map_err(|err| {
            let location = self.inner.location();
            let (_file_id, lexer) = lang_util::error::error_location(&err);

            lang_util::error::ParseError::<Self::Error>::builder()
                .pos(lexer)
                .current_file(self.source_id)
                .resolve(location)
                .finish(err.into())
        })
    }
}
