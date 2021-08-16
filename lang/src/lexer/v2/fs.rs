//! Filesystem based glsl-lang-pp preprocessing lexer

use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

use glsl_lang_pp::{
    exts::{Registry, DEFAULT_REGISTRY},
    last::{self, fs::Event},
    processor::{
        fs::{ExpandStack, FileSystem, ParsedFile, Processor},
        ProcessorState,
    },
};

use crate::parse::{IntoLexer, LangLexer, ParseContext};

use super::{
    core::{self, HandleTokenResult, LexerCore},
    LexicalError,
};

/// glsl-lang-pp filesystem lexer
pub struct Lexer<'r, 'p, F: FileSystem + 'p> {
    inner: last::fs::Tokenizer<'r, ExpandStack<'p, F>>,
    source: Rc<String>,
    core: LexerCore,
    current_file: PathBuf,
    handle_token: HandleTokenResult<last::fs::Tokenizer<'r, ExpandStack<'p, F>>>,
}

impl<'r, 'p, F: FileSystem> Lexer<'r, 'p, F> {
    fn new(
        inner: ExpandStack<'p, F>,
        source: Rc<String>,
        registry: &'r Registry,
        opts: ParseContext,
    ) -> Self {
        Self {
            inner: inner.tokenize(opts.opts.target_vulkan, registry),
            source,
            core: LexerCore::new(opts),
            current_file: Default::default(),
            handle_token: Default::default(),
        }
    }
}

impl<'r, 'p, F: FileSystem> Iterator for Lexer<'r, 'p, F> {
    type Item = core::Item<F::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Pop pending events
            if let Some(item) = self.handle_token.pop_item() {
                return Some(item.map_err(|err| match err {
                    LexicalError::Token { kind, pos } => LexicalError::Token { kind, pos },
                    LexicalError::ProcessStrError { error, pos } => {
                        LexicalError::ProcessStrError { error, pos }
                    }
                    LexicalError::Processor(err) => LexicalError::Processor(err),
                    LexicalError::Io(io) => LexicalError::Io(io.into_inner()),
                }));
            }

            if let Some(event) = self.handle_token.pop_event().or_else(|| self.inner.next()) {
                match event {
                    Event::Error { error, masked } => {
                        if let Some(result) = self.core.handle_error(error, masked) {
                            return Some(result);
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
                    Event::Directive {
                        node,
                        kind,
                        masked,
                        errors,
                    } => {
                        self.core.handle_directive(node, kind, masked, errors);
                    }

                    Event::IoError(err) => {
                        return Some(Err(LexicalError::Io(err)));
                    }

                    Event::EnterFile {
                        file_id,
                        path,
                        canonical_path: _,
                    } => {
                        self.current_file = path;
                        self.core.handle_file_id(file_id);
                    }
                }
            } else {
                return None;
            }
        }
    }
}

impl<'r, 'p, F: FileSystem> LangLexer for Lexer<'r, 'p, F> {
    type Input = File<'p, F>;
    type Error = LexicalError<F::Error>;

    fn new(source: Self::Input, opts: ParseContext) -> Self {
        let source_string = source.inner.source();
        Lexer::new(
            source.inner.process(ProcessorState::default()),
            source_string,
            &DEFAULT_REGISTRY,
            opts,
        )
    }

    fn chain<'i, P: crate::parse::LangParser<Self>>(
        self,
        parser: &P,
    ) -> Result<P::Item, crate::parse::ParseError<Self>> {
        // TODO: Use line map to resolve the line numbers instead of lang_util::error::ParseError
        let src = self.source.clone();
        parser
            .parse(src.as_str(), self)
            .map_err(|err| lang_util::error::ParseError::new(err, src.as_str()))
    }
}

/// Preprocessor wrapper for lexing
#[derive(Debug)]
pub struct Preprocessor<'p, F: FileSystem> {
    processor: &'p mut Processor<F>,
}

/// A preprocessor parsed file ready for lexing
pub struct File<'p, F: FileSystem> {
    inner: ParsedFile<'p, F>,
}

impl<'p, F: FileSystem> IntoLexer for File<'p, F> {
    type Lexer = Lexer<'static, 'p, F>;

    fn into_lexer(
        self,
        source: <Self::Lexer as LangLexer>::Input,
        opts: ParseContext,
    ) -> Self::Lexer {
        <Self::Lexer as LangLexer>::new(source, opts)
    }
}

impl<'p, F: FileSystem> Preprocessor<'p, F> {
    /// Wrap a preprocessor instance for lexing
    pub fn new(processor: &'p mut Processor<F>) -> Self {
        Self { processor }
    }

    /// Open the given file for lexing
    ///
    /// # Parameters
    ///
    /// * `path`: path to the file to open
    /// * `encoding`: encoding to use for decoding the file
    pub fn open(
        &'p mut self,
        path: impl AsRef<Path>,
        encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<File<'p, F>, F::Error> {
        self.processor
            .parse(path.as_ref(), encoding)
            .map(|parsed_file| File { inner: parsed_file })
    }
}