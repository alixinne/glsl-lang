//! Memory based glsl-lang-pp preprocessing lexer

use std::collections::VecDeque;

use lang_util::{
    position::{LexerPosition, NodeSpan},
    NodeContent, TextRange,
};

use glsl_lang_pp::{lexer::TextToken, types::type_names::TypeNameState};
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
            pending_tokens: Default::default(),
            flags: PpFlags::None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PpFlags {
    None,
    Rest,
    Include,
    Version,
    Extension,
    DefineName,
    DefineStart,
    DefineArgs,
}

/// glsl-lang-pp memory lexer iterator
pub struct LexerIterator<'i> {
    inner: glsl_lang_pp::lexer::Lexer<'i>,
    ctx: ParseContext,
    opts: ParseOptions,
    pending_tokens: VecDeque<TextToken>,
    flags: PpFlags,
}

impl<'i> LexerIterator<'i> {
    fn consume_pp_rest(
        &mut self,
        source_token: TextToken,
        pos: NodeSpan,
        text: impl AsRef<str>,
    ) -> (NodeSpan, String) {
        // Collect all tokens until the end of the string
        let (mut rest, mut start_trivia) = if source_token.is_trivia() {
            (String::new(), true)
        } else {
            (text.as_ref().to_string(), false)
        };

        let mut last = pos.range();
        let mut trivia_buffer = Vec::new();

        // allow: we need to release the borrow on self.inner to call input
        #[allow(clippy::while_let_on_iterator)]
        while let Some(token) = self.inner.next() {
            if token.token == glsl_lang_pp::lexer::Token::NEWLINE {
                self.pending_tokens.push_back(token);
                break;
            } else {
                last = token.range;

                if !(start_trivia && token.is_trivia()) {
                    start_trivia = false;

                    if token.is_trivia() {
                        trivia_buffer.push(token);
                    } else {
                        for token in trivia_buffer.drain(..).chain(std::iter::once(token)) {
                            rest.push_str(token.text(self.inner.input()).to_string().as_ref());
                        }
                    }
                }
            }
        }

        // Reset flags
        self.flags = PpFlags::None;

        // Emit token
        let range = NodeSpan::new(
            self.opts.source_id,
            TextRange::new(pos.start().offset, last.end()),
        );

        (range, rest)
    }
}

impl<'i> Iterator for LexerIterator<'i> {
    type Item = Result<(LexerPosition, Token, LexerPosition), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        'outer: loop {
            let mut buffered = false;
            let source_token = self
                .pending_tokens
                .pop_front()
                .inspect(|_| {
                    buffered = true;
                })
                .or_else(|| self.inner.next())?;

            let pos = NodeSpan::new(self.opts.source_id, source_token.range);
            let text = source_token.text(self.inner.input());

            match self.flags {
                PpFlags::None => {
                    // No flags, parse as usual
                }

                PpFlags::Rest => {
                    // Merge all tokens into a single PpRest token
                    // This can only happen after a preprocessor directive, so the tokens will not
                    // be buffered.
                    debug_assert!(self.pending_tokens.is_empty());

                    let (range, rest) = self.consume_pp_rest(source_token, pos, text.to_string());
                    return Some(Ok((range.start(), Token::PpRest(rest), range.end())));
                }

                PpFlags::Include => {
                    // Expect an include path
                    debug_assert!(self.pending_tokens.is_empty());

                    let (range, rest) = self.consume_pp_rest(source_token, pos, text.to_string());
                    return Some(Ok((
                        range.start(),
                        if rest.starts_with('"') {
                            Token::PpPathRelative(
                                rest.strip_prefix('"')
                                    .and_then(|rest| rest.strip_suffix('"'))
                                    .map(str::to_owned)
                                    .unwrap_or(rest),
                            )
                        } else if rest.starts_with('<') {
                            Token::PpPathAbsolute(
                                rest.strip_prefix('<')
                                    .and_then(|rest| rest.strip_suffix('>'))
                                    .map(str::to_owned)
                                    .unwrap_or(rest),
                            )
                        } else {
                            Token::Error
                        },
                        range.end(),
                    )));
                }

                PpFlags::Version | PpFlags::Extension => {
                    // Handled later

                    // On newline, return to normal parsing
                    if source_token.token == glsl_lang_pp::lexer::Token::NEWLINE {
                        self.flags = PpFlags::None;
                    }
                }

                PpFlags::DefineName => {
                    // Start of a preprocessor define
                    match source_token.token {
                        glsl_lang_pp::lexer::Token::IDENT_KW => {
                            // We've seen the name, advance
                            self.flags = PpFlags::DefineStart;
                        }
                        glsl_lang_pp::lexer::Token::NEWLINE => {
                            // Done with preprocessing directive
                            self.flags = PpFlags::None;
                        }
                        _ => {
                            // Other tokens, just advance
                        }
                    }
                }

                PpFlags::DefineStart => {
                    match source_token.token {
                        glsl_lang_pp::lexer::Token::LPAREN => {
                            // Arguments
                            self.flags = PpFlags::DefineArgs;
                        }
                        glsl_lang_pp::lexer::Token::NEWLINE => {
                            // Done with preprocessing directive
                            self.flags = PpFlags::None;
                        }
                        _ => {
                            // Anything else, this is the body of an object define
                            self.pending_tokens.push_back(source_token);
                            self.flags = PpFlags::Rest;
                            continue 'outer;
                        }
                    }
                }

                PpFlags::DefineArgs => {
                    // Inside preprocessor arguments
                    match source_token.token {
                        glsl_lang_pp::lexer::Token::RPAREN => {
                            // Done with arguments
                            self.flags = PpFlags::Rest;
                        }
                        glsl_lang_pp::lexer::Token::NEWLINE => {
                            // Done with preprocessing directive
                            self.flags = PpFlags::None;
                        }
                        _ => {
                            // Other token, just advance
                        }
                    }
                }
            }

            let (token, _type_name_state) = glsl_lang_pp::types::Token::from_token(
                source_token,
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

            match crate::lang_token::lang_token(&self.ctx, text, pos, token) {
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
                        Token::Identifier(ref ident) | Token::TypeName(ref ident)
                            if self.flags == PpFlags::Version =>
                        {
                            return Some(Ok(match ident.as_str() {
                                "core" => (token.0, Token::PpCore, token.2),
                                "compatibility" => (token.0, Token::PpCompatibility, token.2),
                                "es" => (token.0, Token::PpEs, token.2),
                                _ => token,
                            }))
                        }
                        Token::Identifier(ref ident) | Token::TypeName(ref ident)
                            if self.flags == PpFlags::Extension =>
                        {
                            return Some(Ok(match ident.as_str() {
                                "require" => (token.0, Token::PpExtRequire, token.2),
                                "enable" => (token.0, Token::PpExtEnable, token.2),
                                "warn" => (token.0, Token::PpExtWarn, token.2),
                                "disable" => (token.0, Token::PpExtDisable, token.2),
                                _ => token,
                            }))
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

                Err((token, kind)) => match kind {
                    glsl_lang_pp::types::token::ErrorKind::InvalidToken
                        if !buffered && token == glsl_lang_pp::types::Token::HASH =>
                    {
                        self.pending_tokens.push_back(source_token);

                        for token in &mut self.inner {
                            self.pending_tokens.push_back(token);

                            match token.token {
                                glsl_lang_pp::lexer::Token::IDENT_KW => {
                                    // IDENT_KW, this is the name of the preprocessing directive
                                    break;
                                }
                                glsl_lang_pp::lexer::Token::WS
                                | glsl_lang_pp::lexer::Token::COMMENT
                                | glsl_lang_pp::lexer::Token::LINECONT => {
                                    // Whitespace, continue
                                }
                                _ => {
                                    // Unexpected token, stop scanning and return this sequence as
                                    // an error followed by the raw tokens
                                    continue 'outer;
                                }
                            }

                            if token.token == glsl_lang_pp::lexer::Token::IDENT_KW {
                                break;
                            }
                        }

                        // unwrap: there is at least one token, the #
                        let first_token = self.pending_tokens.front().unwrap();
                        let last_token = self.pending_tokens.back().unwrap();

                        let text = last_token.text(self.inner.input()).to_string();
                        let result = match text.as_ref() {
                            "define" => {
                                self.flags = PpFlags::DefineName;
                                Token::PpDefine
                            }
                            "elif" => {
                                self.flags = PpFlags::Rest;
                                Token::PpElif
                            }
                            "else" => Token::PpElse,
                            "endif" => Token::PpEndIf,
                            "error" => {
                                self.flags = PpFlags::Rest;
                                Token::PpError
                            }
                            "extension" => {
                                self.flags = PpFlags::Extension;
                                Token::PpExtension
                            }
                            "if" => {
                                self.flags = PpFlags::Rest;
                                Token::PpIf
                            }
                            "ifdef" => Token::PpIfDef,
                            "ifndef" => Token::PpIfNDef,
                            "include" => {
                                self.flags = PpFlags::Include;
                                Token::PpInclude
                            }
                            "line" => Token::PpLine,
                            "pragma" => {
                                self.flags = PpFlags::Rest;
                                Token::PpPragma
                            }
                            "undef" => Token::PpUndef,
                            "version" => {
                                self.flags = PpFlags::Version;
                                Token::PpVersion
                            }
                            _ => {
                                // Invalid preprocessing directive, just process tokens normally
                                continue 'outer;
                            }
                        };

                        let pos = NodeSpan::new(
                            self.opts.source_id,
                            TextRange::new(first_token.range.start(), last_token.range.end()),
                        );

                        // Drop the buffered tokens
                        self.pending_tokens.clear();

                        return Some(Ok((pos.start(), result, pos.end())));
                    }
                    _ => {
                        return Some(Err(LexicalError::Token { kind, pos }));
                    }
                },
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
