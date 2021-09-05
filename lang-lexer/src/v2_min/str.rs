//! Memory based glsl-lang-pp preprocessing lexer

use lang_util::{
    position::{LexerPosition, NodeSpan},
    NodeContent,
};

use glsl_lang_pp::types::type_names::TypeNameState;
use glsl_lang_types::ast;

use crate::{HasLexerError, LangLexer, LangLexerIterator, ParseContext, ParseOptions, Token};

use super::LexicalError;

/// glsl-lang-pp memory lexer
pub struct Lexer<'i> {
    inner: glsl_lang_pp::lexer::Lexer<'i>,
    opts: ParseOptions,
}

impl<'i> Lexer<'i> {
    pub(crate) fn new(source: &'i str, opts: &ParseOptions) -> Self {
        Self {
            inner: glsl_lang_pp::lexer::Lexer::new(source),
            opts: *opts,
        }
    }

    fn with_context(self, ctx: ParseContext) -> LexerIterator<'i> {
        LexerIterator {
            inner: self.inner,
            ctx,
            opts: self.opts,
        }
    }
}

/// glsl-lang-pp memory lexer iterator
pub struct LexerIterator<'i> {
    inner: glsl_lang_pp::lexer::Lexer<'i>,
    ctx: ParseContext,
    opts: ParseOptions,
}

impl<'i> Iterator for LexerIterator<'i> {
    type Item = Result<(LexerPosition, Token, LexerPosition), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let token = self.inner.next()?;

            let pos = NodeSpan::new(self.opts.source_id, token.range);
            let text = token.text(self.inner.input());

            let (token, _type_name_state) = glsl_lang_pp::types::Token::from_token(
                token,
                self.inner.input(),
                self.opts.default_version,
                self.opts.target_vulkan,
                |name| {
                    if self.ctx.is_type_name(name.as_ref()) {
                        TypeNameState::Type
                    } else {
                        TypeNameState::Ident
                    }
                },
            );

            let text = text.into_unescaped();
            let text = text.try_as_str().unwrap();

            match crate::v2::lang_token(&self.ctx, text, pos, token) {
                Ok(token) => {
                    // Try to get the next token when we encounter trivia
                    match token.1 {
                        Token::Whitespace => {}
                        Token::SingleLineComment | Token::MultiLineComment => {
                            if self.ctx.has_comments() {
                                let mut text = text.split_at(2).1.to_string();

                                let comment = match token.1 {
                                    Token::SingleLineComment => ast::CommentData::Single(text),
                                    Token::MultiLineComment => {
                                        text.pop();
                                        text.pop();
                                        ast::CommentData::Multi(text)
                                    }
                                    _ => unreachable!(),
                                }
                                .spanned(token.0, token.2);

                                self.ctx.add_comment(comment);
                            }
                        }
                        _ => {
                            if token.1 == Token::LeftBrace {
                                self.ctx.push_scope();
                            } else if token.1 == Token::RightBrace {
                                self.ctx.pop_scope();
                            }

                            return Some(Ok(token));
                        }
                    }
                }

                Err((_token, kind)) => {
                    return Some(Err(LexicalError::Token { kind, pos }));
                }
            }
        }
    }
}

impl HasLexerError for Lexer<'_> {
    type Error = LexicalError;
}

impl<'i> LangLexer<'i> for Lexer<'i> {
    type Input = &'i str;
    type Iter = LexerIterator<'i>;

    fn new(source: Self::Input, opts: &ParseOptions) -> Self {
        Self::new(source, opts)
    }

    fn run(self, ctx: ParseContext) -> Self::Iter {
        self.with_context(ctx)
    }
}

impl HasLexerError for LexerIterator<'_> {
    type Error = LexicalError;
}

impl<'i> LangLexerIterator for LexerIterator<'i> {
    #[cfg(feature = "lalrpop")]
    fn resolve_err(
        &self,
        err: lalrpop_util::ParseError<LexerPosition, Token, Self::Error>,
    ) -> lang_util::error::ParseError<Self::Error> {
        let location = self.inner.line_map();
        let (_file_id, lexer) = lang_util::error::error_location(&err);

        lang_util::error::ParseError::<Self::Error>::builder()
            .pos(lexer)
            .current_file(self.opts.source_id)
            .resolve(location)
            .finish(err.into())
    }
}
