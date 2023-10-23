//! Parse operation builder definition

use glsl_lang_lexer::{HasLexerError, LangLexerIterator, ParseOptions};

use super::{Extractable, HasParser, LangLexer, LangParser, ParseContext, ParseResult};

/// Builder structure for a parsing operation
pub struct ParseBuilder<'i, 'o, 'c, 'p, L: LangLexer<'i>, T: HasParser> {
    source: L::Input,
    opts: Option<&'o ParseOptions>,
    context: Option<&'c ParseContext>,
    lexer: Option<L>,
    parser: Option<&'p T::Parser>,
}

impl<'i, 'o, 'c, 'p, L: LangLexer<'i>, T: HasParser> ParseBuilder<'i, 'o, 'c, 'p, L, T> {
    /// Create a new parse builder from the given input string
    pub fn new(source: L::Input) -> Self {
        Self {
            source,
            opts: None,
            context: None,
            lexer: None,
            parser: None,
        }
    }

    /// Set the parse options for this parse
    pub fn opts(self, opts: &'o ParseOptions) -> Self {
        Self {
            opts: Some(opts),
            ..self
        }
    }

    /// Set the parse options for this parse
    pub fn context(self, ctx: &'c ParseContext) -> Self {
        Self {
            context: Some(ctx),
            ..self
        }
    }

    /// Set the parser instance to use for this parse
    pub fn parser(self, parser: &'p T::Parser) -> Self {
        Self {
            parser: Some(parser),
            ..self
        }
    }

    /// Execute the parsing operation
    #[allow(clippy::result_large_err)]
    fn parse_source(
        source: L::Input,
        opts: Option<&'o ParseOptions>,
        mut context: Option<&'c ParseContext>,
        mut lexer: Option<L>,
        mut parser: Option<&'p T::Parser>,
    ) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, T> {
        // Get parse options
        let default_opts = Default::default();
        let opts = opts.unwrap_or(&default_opts);

        // Clone the input context, or create a new one
        let cloned_context = if let Some(existing) = context.take() {
            existing.clone_inner()
        } else {
            Default::default()
        };

        // Create the lexer
        let lexer = if let Some(lexer) = lexer.take() {
            lexer
        } else {
            L::new(source, opts)
        };

        // Create the parser
        let created_parser;
        let parser = if let Some(parser) = parser.take() {
            parser
        } else {
            created_parser = <T as HasParser>::Parser::new();
            &created_parser
        };

        // Invoke the parser
        let mut iter = lexer.run(cloned_context.clone());
        match parser.parse(cloned_context.clone(), &mut iter) {
            Ok(t) => Ok((t, cloned_context, iter)),
            Err(err) => Err(iter.resolve_err(err)),
        }
    }

    /// Execute the parsing operation
    #[allow(clippy::result_large_err)]
    pub fn parse(self) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, T> {
        Self::parse_source(
            self.source,
            self.opts,
            self.context,
            self.lexer,
            self.parser,
        )
    }

    /// Execute the parsing operation, and extract the wanted node
    #[allow(clippy::result_large_err)]
    pub fn extract<U: Extractable<T>>(
        self,
    ) -> ParseResult<L::Iter, <L::Iter as HasLexerError>::Error, Option<U>> {
        Self::parse_source(
            self.source,
            self.opts,
            self.context,
            self.lexer,
            self.parser,
        )
        .map(|(root, ctx, l)| (U::extract(root), ctx, l))
    }
}

#[cfg(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
))]
impl<'i, 'o, 'c, 'p, T: HasParser> ParseBuilder<'i, 'o, 'c, 'p, super::DefaultLexer<'i>, T> {
    /// Create a new parse builder from the given input string
    pub fn default(source: <super::DefaultLexer<'i> as LangLexer<'i>>::Input) -> Self {
        Self {
            source,
            opts: None,
            context: None,
            lexer: None,
            parser: None,
        }
    }
}

/// Trait for creating parse builders from lexer inputs
pub trait IntoParseBuilderExt<'i> {
    /// Type of the lexer associated with this input
    type Lexer: LangLexer<'i>;

    /// Create a builder for this lexer input
    fn builder<'o, 'c, 'p, T>(self) -> ParseBuilder<'i, 'o, 'c, 'p, Self::Lexer, T>
    where
        T: HasParser;
}

#[cfg(any(
    feature = "lexer-v1",
    feature = "lexer-v2-min",
    feature = "lexer-v2-full"
))]
impl<'i> IntoParseBuilderExt<'i> for &'i str {
    type Lexer = super::DefaultLexer<'i>;

    fn builder<'o, 'c, 'p, T>(self) -> ParseBuilder<'i, 'o, 'c, 'p, Self::Lexer, T>
    where
        T: HasParser,
    {
        ParseBuilder::default(self)
    }
}

#[cfg(feature = "lexer-v2-full")]
impl<'r, 'p, F: glsl_lang_lexer::v2_full::fs::FileSystem> IntoParseBuilderExt<'p>
    for glsl_lang_lexer::v2_full::fs::File<'r, 'p, F>
where
    glsl_lang_lexer::v2_full::fs::File<'r, 'p, F>: 'p,
{
    type Lexer = glsl_lang_lexer::v2_full::fs::Lexer<'r, 'p, F>;

    fn builder<'o, 'c, 'q, T>(self) -> ParseBuilder<'p, 'o, 'c, 'q, Self::Lexer, T>
    where
        T: HasParser,
    {
        ParseBuilder::new(self)
    }
}
