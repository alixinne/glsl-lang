//! Memory based glsl-lang-pp preprocessing lexer

use lang_util::{position::LexerPosition, FileId};

use glsl_lang_pp::{
    exts::{Registry, DEFAULT_REGISTRY},
    last::{self, Event},
    processor::{
        self,
        str::{ExpandStr, ProcessStrError},
        ProcessorState,
    },
};

use crate::{HasLexerError, LangLexer, LangLexerIterator, ParseContext, ParseOptions, Token};

use super::{
    core::{self, HandleTokenResult, LexerCore},
    LexicalError,
};

/// glsl-lang-pp memory lexer
pub struct Lexer<'i> {
    inner: last::Tokenizer<'i, ExpandStr>,
    handle_token: HandleTokenResult<ProcessStrError>,
    opts: ParseOptions,
}

impl<'i> Lexer<'i> {
    pub(crate) fn new_with_state(
        source: &'i str,
        registry: &'i Registry,
        opts: &ParseOptions,
        state: ProcessorState,
    ) -> Self {
        Self {
            inner: processor::str::process(source, state).tokenize(
                opts.default_version,
                opts.target_vulkan,
                registry,
            ),
            handle_token: Default::default(),
            opts: *opts,
        }
    }

    fn with_context(self, ctx: ParseContext) -> LexerIterator<'i> {
        LexerIterator {
            inner: self.inner,
            core: LexerCore::new(&self.opts, ctx),
            handle_token: self.handle_token,
            source_id: self.opts.source_id,
        }
    }
}

/// glsl-lang-pp memory lexer iterator
pub struct LexerIterator<'i> {
    inner: last::Tokenizer<'i, ExpandStr>,
    core: LexerCore,
    handle_token: HandleTokenResult<ProcessStrError>,
    source_id: FileId,
}

impl<'i> Iterator for LexerIterator<'i> {
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

impl HasLexerError for Lexer<'_> {
    type Error = LexicalError<ProcessStrError>;
}

impl<'i> LangLexer<'i> for Lexer<'i> {
    type Input = &'i str;
    type Iter = LexerIterator<'i>;

    fn new(source: Self::Input, opts: &ParseOptions) -> Self {
        Self::new_with_state(source, &DEFAULT_REGISTRY, opts, ProcessorState::default())
    }

    fn run(self, ctx: ParseContext) -> Self::Iter {
        self.with_context(ctx)
    }
}

impl HasLexerError for LexerIterator<'_> {
    type Error = LexicalError<ProcessStrError>;
}

impl<'i> LangLexerIterator for LexerIterator<'i> {
    #[cfg(feature = "lalrpop")]
    fn resolve_err(
        &self,
        err: lalrpop_util::ParseError<LexerPosition, Token, Self::Error>,
    ) -> lang_util::error::ParseError<Self::Error> {
        let location = self.inner.location();
        let (_file_id, lexer) = lang_util::error::error_location(&err);

        lang_util::error::ParseError::<Self::Error>::builder()
            .pos(lexer)
            .current_file(self.source_id)
            .resolve(location)
            .finish(err.into())
    }
}
