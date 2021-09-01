//! Parse operation builder definition

use super::{HasParser, IntoLexer, LangLexer, LangParser, ParseContext, ParseResult};

/// Builder structure for a parsing operation
pub struct ParseBuilder<'o, 'p, L: IntoLexer, T: HasParser<L::Lexer>> {
    source: <L::Lexer as LangLexer>::Input,
    opts: Option<&'o ParseContext>,
    lexer: Option<L>,
    parser: Option<&'p T::Parser>,
}

impl<'o, 'p, L: IntoLexer, T: HasParser<L::Lexer>> ParseBuilder<'o, 'p, L, T> {
    /// Create a new parse builder from the given input string
    pub fn new(source: <L::Lexer as LangLexer>::Input) -> Self {
        Self {
            source,
            opts: None,
            lexer: None,
            parser: None,
        }
    }

    /// Set the parse context for this parse
    pub fn opts(self, opts: &'o ParseContext) -> Self {
        Self {
            opts: Some(opts),
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
    pub fn parse(mut self) -> ParseResult<L, T> {
        let source = self.source;

        // Clone the input context, or create a new one
        let cloned_opts = if let Some(existing) = self.opts.take() {
            existing.clone_inner()
        } else {
            Default::default()
        };

        // Create the lexer
        let mut lexer = if let Some(lexer) = self.lexer.take() {
            lexer.into_lexer(source, cloned_opts.clone())
        } else {
            L::Lexer::new(source, cloned_opts.clone())
        };

        // Create the parser
        let created_parser;
        let parser = if let Some(parser) = self.parser.take() {
            parser
        } else {
            created_parser = <T as HasParser<L::Lexer>>::Parser::new();
            &created_parser
        };

        // Invoke the parser
        lexer
            .chain(parser)
            .map(|parsed| (parsed, cloned_opts, lexer))
    }
}
