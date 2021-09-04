//! Filesystem based glsl-lang-pp preprocessing lexer

use std::path::{Path, PathBuf};

use lang_util::position::LexerPosition;

use glsl_lang_pp::{
    exts::{Registry, DEFAULT_REGISTRY},
    last::{self, Event},
    processor::{
        fs::{ExpandStack, ParsedFile, Processor},
        ProcessorState,
    },
};

use crate::{HasLexerError, LangLexer, LangLexerIterator, ParseContext, ParseOptions, Token};

use super::{
    core::{self, HandleTokenResult, LexerCore},
    LexicalError,
};

pub use glsl_lang_pp::processor::fs::FileSystem;

/// glsl-lang-pp filesystem lexer
pub struct Lexer<'r, 'p, F: FileSystem> {
    inner: last::Tokenizer<'r, ExpandStack<'p, F>>,
    current_file: PathBuf,
    handle_token: HandleTokenResult<F::Error>,
    opts: ParseOptions,
}

impl<'r, 'p, F: FileSystem> Lexer<'r, 'p, F> {
    fn new(inner: ExpandStack<'p, F>, registry: &'r Registry, opts: &ParseOptions) -> Self {
        Self {
            inner: inner.tokenize(opts.default_version, opts.target_vulkan, registry),
            current_file: Default::default(),
            handle_token: Default::default(),
            opts: *opts,
        }
    }

    fn with_context(self, ctx: ParseContext) -> LexerIterator<'r, 'p, F> {
        LexerIterator {
            inner: self.inner,
            core: LexerCore::new(&self.opts, ctx),
            current_file: self.current_file,
            handle_token: self.handle_token,
        }
    }
}

/// glsl-lang-pp filesystem lexer iterator
pub struct LexerIterator<'r, 'p, F: FileSystem> {
    inner: last::Tokenizer<'r, ExpandStack<'p, F>>,
    core: LexerCore,
    current_file: PathBuf,
    handle_token: HandleTokenResult<F::Error>,
}

impl<'r, 'p, F: FileSystem> Iterator for LexerIterator<'r, 'p, F> {
    type Item = core::Item<F::Error>;

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
                            if !masked {
                                return Some(Err(error.into()));
                            }
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

                        Event::Directive { directive, masked } => {
                            if let Err(errors) = self.core.handle_directive(directive, masked) {
                                self.handle_token.push_errors(errors);
                            }
                        }

                        Event::EnterFile {
                            file_id,
                            path,
                            canonical_path: _,
                        } => {
                            self.current_file = path;
                            self.core.handle_file_id(file_id);
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

impl<F: FileSystem> HasLexerError for Lexer<'_, '_, F> {
    type Error = LexicalError<F::Error>;
}

impl<'r, 'p, F: FileSystem> LangLexer<'p> for Lexer<'r, 'p, F>
where
    File<'r, 'p, F>: 'p,
{
    type Input = File<'r, 'p, F>;
    type Iter = LexerIterator<'r, 'p, F>;

    fn new(source: Self::Input, opts: &ParseOptions) -> Self {
        Lexer::new(
            source.inner.process(source.state.unwrap_or_default()),
            source.registry.unwrap_or(&DEFAULT_REGISTRY),
            opts,
        )
    }

    fn run(self, ctx: ParseContext) -> Self::Iter {
        self.with_context(ctx)
    }
}

impl<F: FileSystem> HasLexerError for LexerIterator<'_, '_, F> {
    type Error = LexicalError<F::Error>;
}

impl<'r, 'p, F: FileSystem> LangLexerIterator for LexerIterator<'r, 'p, F> {
    fn resolve_err(
        &self,
        err: lalrpop_util::ParseError<LexerPosition, Token, Self::Error>,
    ) -> lang_util::error::ParseError<Self::Error> {
        let location = self.inner.location();
        let (file_id, lexer) = lang_util::error::error_location(&err);

        lang_util::error::ParseError::<Self::Error>::builder()
            .pos(lexer)
            .current_file(file_id)
            .resolve(location)
            .resolve_path(&self.inner)
            .finish(err.into())
    }
}

/// glsl-lang-pp preprocessor extensions
pub trait PreprocessorExt<F: FileSystem> {
    /// Open the given file for lexing
    ///
    /// # Parameters
    ///
    /// * `path`: path to the file to open
    fn open(&mut self, path: impl AsRef<Path>) -> Result<File<'_, '_, F>, F::Error>;

    /// Open the given source block for lexing
    ///
    /// # Parameters
    ///
    /// * `source`: source string to parse
    /// * `path`: path to the directory that contains this source
    fn open_source(&mut self, source: &str, path: impl AsRef<Path>) -> File<'_, '_, F>;
}

impl<F: FileSystem> PreprocessorExt<F> for Processor<F> {
    fn open(&mut self, path: impl AsRef<Path>) -> Result<File<'_, '_, F>, F::Error> {
        self.parse(path.as_ref()).map(|parsed_file| File {
            inner: parsed_file,
            state: None,
            registry: None,
        })
    }

    fn open_source(&mut self, source: &str, path: impl AsRef<Path>) -> File<'_, '_, F> {
        File {
            inner: self.parse_source(source, path.as_ref()),
            state: None,
            registry: None,
        }
    }
}

/// A preprocessor parsed file ready for lexing
pub struct File<'r, 'p, F: FileSystem> {
    inner: ParsedFile<'p, F>,
    state: Option<ProcessorState>,
    registry: Option<&'r Registry>,
}

impl<'r, 'p, F: FileSystem> File<'r, 'p, F> {
    /// Set the default processor state for processing this file
    pub fn with_state(self, state: impl Into<ProcessorState>) -> Self {
        Self {
            state: Some(state.into()),
            ..self
        }
    }

    /// Set the extension registry to use for this file
    pub fn with_registry(self, registry: impl Into<&'r Registry>) -> Self {
        Self {
            registry: Some(registry.into()),
            ..self
        }
    }
}
