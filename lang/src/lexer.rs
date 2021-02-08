use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use logos::Logos;
use thiserror::Error;

use crate::{ast, parse::ParseOptions};

mod parsers;
use parsers::*;

mod preprocessor_token;
pub use preprocessor_token::*;

mod token;
pub use token::*;

#[cfg(test)]
mod tests;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct TypeNames {
    names: Rc<RefCell<HashSet<String>>>,
}

impl TypeNames {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clone_inner(&self) -> Self {
        Self {
            names: Rc::new(RefCell::new(self.names.borrow().clone())),
        }
    }

    pub fn is_type_name(&self, name: &str) -> bool {
        self.names.borrow().contains(name)
    }

    pub fn add_type_name(&self, name: ast::Identifier) -> ast::TypeName {
        let name_string = name.0.to_string();
        self.names.borrow_mut().insert(name_string);
        name.map(|id| ast::TypeNameData::from(id))
    }
}

pub type LexerContext = ParseOptions;

enum LexerStage<'i> {
    Source(logos::Lexer<'i, Token<'i>>),
    Preprocessor(
        logos::Lexer<'i, PreprocessorToken<'i>>,
        Token<'i>,
        bool,
        bool,
    ),
}

pub struct Lexer<'i> {
    inner: LexerStage<'i>,
    last_token: Option<Token<'i>>,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str, opts: ParseOptions) -> Self {
        Self {
            inner: LexerStage::Source(Token::lexer_with_extras(input, opts)),
            last_token: None,
        }
    }

    fn consume(src: &mut logos::Lexer<'i, Token<'i>>) -> Option<<Self as Iterator>::Item> {
        let token = src.next()?;
        let span = src.span();
        let source_id = src.extras.source_id;

        Some(((source_id, span.start), token, (source_id, span.end)))
    }

    fn consume_pp(
        pp: &mut logos::Lexer<'i, PreprocessorToken<'i>>,
    ) -> Option<((usize, usize), PreprocessorToken<'i>, (usize, usize))> {
        let token = pp.next()?;
        let span = pp.span();
        let source_id = pp.extras.source_id;

        Some(((source_id, span.start), token, (source_id, span.end)))
    }

    fn consume_pp_rest(
        pp: &mut logos::Lexer<'i, PreprocessorToken<'i>>,
    ) -> Option<((usize, usize), PreprocessorToken<'i>, (usize, usize))> {
        let mut ch1;
        let mut ch2 = None;
        let mut ch3 = None;

        // First, consume whitespace
        let mut whitespace_chars = 0;
        for b in pp.remainder().bytes() {
            ch1 = ch2;
            ch2 = ch3;
            ch3 = Some(b);

            if ch3 == Some(b'\t') || ch3 == Some(b' ') {
                // Regular whitespace
                whitespace_chars += 1;
            } else if ch3 == Some(b'\\') {
                // Could be a line continuation, wait
            } else if ch3 == Some(b'\r') && ch2 == Some(b'\\') {
                // ditto
            } else if ch3 == Some(b'\n') && ch2 == Some(b'\\') {
                // Unix line continuation
                whitespace_chars += 2;
            } else if ch3 == Some(b'\n') && ch2 == Some(b'\r') && ch1 == Some(b'\\') {
                // Windows line continuation
                whitespace_chars += 3;
            } else {
                // Not whitespace
                break;
            }
        }

        pp.bump(whitespace_chars);

        // Then consume the rest of the "line"
        let mut ch1;
        let mut ch2 = None;
        let mut ch3 = None;

        // TODO: Do not allocate if it's not needed
        let mut res = Vec::new();

        let start = pp.span().end;

        let mut consumed_chars = 0;
        for b in pp.remainder().bytes() {
            ch1 = ch2;
            ch2 = ch3;
            ch3 = Some(b);

            if ch3 == Some(b'\\') {
                // Could be a line continuation, wait
            } else if ch3 == Some(b'\r') && ch2 == Some(b'\\') {
                // ditto
            } else if ch3 == Some(b'\n') && ch2 == Some(b'\\') {
                // Unix line continuation
                consumed_chars += 2;
            } else if ch3 == Some(b'\n') && ch2 == Some(b'\r') && ch1 == Some(b'\\') {
                // Windows line continuation
                consumed_chars += 3;
            } else if ch3 == Some(b'\r') {
                // Possible Windows line ending
            } else if ch3 == Some(b'\n') && ch2 == Some(b'\r') {
                // Windows line ending
                break;
            } else if ch3 == Some(b'\n') {
                // Unix line ending
                break;
            } else if ch1 == Some(b'\\') {
                // Not a Windows line continuation
                consumed_chars += 3;
                res.push(ch1.unwrap());
                res.push(ch2.unwrap());
                res.push(ch3.unwrap());
            } else if ch2 == Some(b'\\') {
                // Not a Unix line continuation
                consumed_chars += 2;
                res.push(ch2.unwrap());
                res.push(ch3.unwrap());
            } else if ch2 == Some(b'\r') {
                // Unterminated Windows line ending
                consumed_chars += 2;
                res.push(b'\r');
                res.push(ch3.unwrap());
            } else {
                consumed_chars += 1;
                res.push(ch3.unwrap());
            }
        }

        pp.bump(consumed_chars);

        let source_id = pp.extras.source_id;
        Some((
            (source_id, start),
            PreprocessorToken::Rest(std::borrow::Cow::Owned(
                String::from_utf8(res).expect("invalid utf-8"),
            )),
            (source_id, start + consumed_chars),
        ))
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = ((usize, usize), Token<'i>, (usize, usize));

    fn next(&mut self) -> Option<Self::Item> {
        let result = match &mut self.inner {
            LexerStage::Source(src) => {
                let token = src.next()?;
                let span = src.span();
                let source_id = src.extras.source_id;

                let mut token = if src.extras.target_vulkan {
                    // Targetting Vulkan, nothing to change
                    ((source_id, span.start), token, (source_id, span.end))
                } else {
                    use Token::*;

                    // Replace Vulkan keywords with identifiers
                    (
                        (source_id, span.start),
                        match token {
                            Texture1D | Texture1DArray | ITexture1D | ITexture1DArray
                            | UTexture1D | UTexture1DArray | Texture2D | Texture2DArray
                            | ITexture2D | ITexture2DArray | UTexture2D | UTexture2DArray
                            | Texture2DRect | ITexture2DRect | UTexture2DRect | Texture2DMS
                            | ITexture2DMS | UTexture2DMS | Texture2DMSArray
                            | ITexture2DMSArray | UTexture2DMSArray | Texture3D | ITexture3D
                            | UTexture3D | TextureCube | ITextureCube | UTextureCube
                            | TextureCubeArray | ITextureCubeArray | UTextureCubeArray
                            | TextureBuffer | ITextureBuffer | UTextureBuffer | Sampler
                            | SamplerShadow | SubpassInput | ISubpassInput | USubpassInput
                            | SubpassInputMS | ISubpassInputMS | USubpassInputMS => {
                                Identifier((src.slice(), src.extras.type_names.clone()))
                            }
                            other => other,
                        },
                        (source_id, span.end),
                    )
                };

                // Transform the ident into a type name if needed
                if let Token::Identifier((ident, ref tn)) = token.1 {
                    if tn.is_type_name(ident) {
                        token.1 = Token::TypeName(ident);
                    }
                }

                // Switch to preprocessor mode when encountering a preprocessor token
                if token.1.is_pp() {
                    let consumed_rest = if let Token::PpElse | Token::PpEndIf = token.1 {
                        // These tokens have nothing to parse following them
                        true
                    } else {
                        // Other tokens will be processed by the preprocessor lexer
                        false
                    };

                    let new_lexer = src.clone().morph();
                    self.inner =
                        LexerStage::Preprocessor(new_lexer, token.1.clone(), false, consumed_rest);
                }

                Some(token)
            }
            LexerStage::Preprocessor(pp, ty, waiting_for_rparen, consumed_rest) => {
                let read_token = if *consumed_rest {
                    None
                } else {
                    match ty {
                        Token::PpDefine => {
                            Some(if let Some(Token::Identifier(_)) = &self.last_token {
                                // The last token was an identifier

                                // Is it (immediately) followed by a left paren?
                                if pp.remainder().bytes().next() == Some(b'(')
                                    || *waiting_for_rparen
                                {
                                    // We will now wait for a right parenthesis to close the identifier
                                    // list
                                    *waiting_for_rparen = true;

                                    // Then, consume it regularly so the parser can detect the identifiers
                                    Self::consume_pp(pp)
                                } else {
                                    // Otherwise, it's the define value, so consume the remainder, return
                                    // it as PpRest and return back to regular parsing
                                    *consumed_rest = true;
                                    Self::consume_pp_rest(pp)
                                }
                            } else if let Some(Token::RightParen) = &self.last_token {
                                // This is the closing parenthesis of a define
                                *consumed_rest = true;
                                Self::consume_pp_rest(pp)
                            } else {
                                // Other tokens (identifier, comma), just consume regularly
                                Self::consume_pp(pp)
                            })
                        }
                        Token::PpElif | Token::PpError | Token::PpIf | Token::PpPragma => {
                            // Consume the rest of the line
                            *consumed_rest = true;
                            Some(Self::consume_pp_rest(pp))
                        }
                        Token::PpVersion => {
                            Some(Self::consume_pp(pp).map(|(start, token, end)| {
                                (
                                    start,
                                    if let PreprocessorToken::Identifier((name, tn)) = token {
                                        if name == "core" {
                                            PreprocessorToken::PpCore
                                        } else if name == "compatibility" {
                                            PreprocessorToken::PpCompatibility
                                        } else if name == "es" {
                                            PreprocessorToken::PpEs
                                        } else {
                                            PreprocessorToken::Identifier((name, tn))
                                        }
                                    } else {
                                        token
                                    },
                                    end,
                                )
                            }))
                        }
                        Token::PpExtension => {
                            Some(Self::consume_pp(pp).map(|(start, token, end)| {
                                (
                                    start,
                                    if let PreprocessorToken::Identifier((name, tn)) = token {
                                        if name == "require" {
                                            PreprocessorToken::PpExtRequire
                                        } else if name == "enable" {
                                            PreprocessorToken::PpExtEnable
                                        } else if name == "warn" {
                                            PreprocessorToken::PpExtWarn
                                        } else if name == "disable" {
                                            PreprocessorToken::PpExtDisable
                                        } else {
                                            PreprocessorToken::Identifier((name, tn))
                                        }
                                    } else {
                                        token
                                    },
                                    end,
                                )
                            }))
                        }
                        _ => None,
                    }
                };

                // If no token was read (unexpected state), read a new one
                let token = read_token.unwrap_or_else(|| Self::consume_pp(pp));
                if let Some((_, PreprocessorToken::Newline, _)) = token {
                    // Directive completed, process next token as regular source
                    let mut new_lexer = pp.clone().morph();
                    let result = Self::consume(&mut new_lexer);
                    self.inner = LexerStage::Source(new_lexer);
                    result
                } else {
                    // Map into Token
                    token.map(|(s, t, e)| (s, t.into(), e))
                }
            }
        };

        self.last_token = result.as_ref().map(|s| s.1.clone());
        result
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum LexicalError {
    #[error("invalid int literal: {0}")]
    InvalidIntLiteral(#[from] std::num::ParseIntError),
    #[error("invalid float literal: {0}")]
    InvalidFloatLiteral(#[from] std::num::ParseFloatError),
}
