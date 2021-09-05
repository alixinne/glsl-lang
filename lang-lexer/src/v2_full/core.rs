use std::collections::VecDeque;

use glsl_lang_pp::{
    last::{self, LocatedIterator, MaybeToken, TokenState, Tokenizer},
    processor::event::{self, Error, EventDirective, OutputToken},
    types,
};

use glsl_lang_types::ast;

use lang_util::{located::Located, position::NodeSpan, FileId, NodeContent, TextRange, TextSize};

use crate::{ParseContext, ParseOptions};

use super::{LexerPosition, LexicalError, Token};

pub type Item<E> = Result<(LexerPosition, Token, LexerPosition), LexicalError<E>>;

pub struct LexerCore {
    pub ctx: ParseContext,
    file_id: FileId,
    opts: ParseOptions,
}

pub enum HandleTokenResult<E: std::error::Error + 'static> {
    None,
    Item(Item<E>),
    Pending(VecDeque<Item<E>>, VecDeque<Result<last::Event, Located<E>>>),
}

impl<E: std::error::Error + 'static> HandleTokenResult<E> {
    pub fn push_item(&mut self, item: Item<E>) {
        match std::mem::take(self) {
            HandleTokenResult::None => {
                *self = HandleTokenResult::Item(item);
            }
            HandleTokenResult::Item(old_item) => {
                let mut pending_items = VecDeque::with_capacity(2);
                pending_items.push_back(old_item);
                pending_items.push_back(item);
                *self = HandleTokenResult::Pending(pending_items, VecDeque::new());
            }
            HandleTokenResult::Pending(mut items, events) => {
                items.push_back(item);
                *self = HandleTokenResult::Pending(items, events);
            }
        }
    }

    pub fn pop_item(&mut self) -> Option<Item<E>> {
        match std::mem::take(self) {
            HandleTokenResult::None => None,
            HandleTokenResult::Item(item) => Some(item),
            HandleTokenResult::Pending(mut items, events) => {
                let item = items.pop_front();
                if !items.is_empty() || !events.is_empty() {
                    *self = Self::Pending(items, events);
                }
                item
            }
        }
    }

    pub fn pop_event(&mut self) -> Option<Result<last::Event, Located<E>>> {
        match std::mem::take(self) {
            HandleTokenResult::None => None,
            HandleTokenResult::Item(item) => {
                *self = Self::Item(item);
                None
            }
            HandleTokenResult::Pending(items, mut events) => {
                let event = events.pop_front();
                if !items.is_empty() || !events.is_empty() {
                    *self = Self::Pending(items, events);
                }
                event
            }
        }
    }

    pub fn push_errors(&mut self, errors: impl IntoIterator<Item = Error>) {
        let items = errors.into_iter().map(|error| Err(error.into()));

        match std::mem::take(self) {
            HandleTokenResult::None => {
                *self = HandleTokenResult::Pending(items.collect(), Default::default());
            }
            HandleTokenResult::Item(old_item) => {
                let mut pending_items = VecDeque::with_capacity(items.size_hint().0 + 1);
                pending_items.push_back(old_item);
                pending_items.extend(items);
                *self = HandleTokenResult::Pending(pending_items, Default::default());
            }
            HandleTokenResult::Pending(mut old_items, events) => {
                old_items.extend(items);
                *self = HandleTokenResult::Pending(old_items, events);
            }
        }
    }
}

impl<E: std::error::Error + 'static> Default for HandleTokenResult<E> {
    fn default() -> Self {
        Self::None
    }
}

impl LexerCore {
    pub fn new(opts: &ParseOptions, ctx: ParseContext) -> Self {
        let file_id = opts.source_id;
        Self {
            ctx,
            file_id,
            opts: *opts,
        }
    }

    fn lang_token(
        &self,
        source_token: &OutputToken,
        token_kind: types::Token,
    ) -> Result<(LexerPosition, Token, LexerPosition), Option<types::token::ErrorKind>> {
        let start = self.position(source_token.text_range().start());
        let end = self.position(source_token.text_range().end());

        Ok((
            start,
            match token_kind {
                types::Token::IDENT(ident) => {
                    if self.ctx.is_type_name(&ident) {
                        Token::TypeName(ident)
                    } else {
                        // It is an identifier
                        Token::Identifier(ident)
                    }
                }
                types::Token::TYPE_NAME(type_name) => match type_name {
                    types::TypeName::VOID => Token::Void,
                    types::TypeName::INT => Token::Int,
                    types::TypeName::BOOL => Token::Bool,
                    types::TypeName::FLOAT => Token::Float,
                    types::TypeName::DOUBLE => Token::Double,
                    types::TypeName::VEC2 => Token::Vec2,
                    types::TypeName::VEC3 => Token::Vec3,
                    types::TypeName::VEC4 => Token::Vec4,
                    types::TypeName::IVEC2 => Token::IVec2,
                    types::TypeName::IVEC3 => Token::IVec3,
                    types::TypeName::IVEC4 => Token::IVec4,
                    types::TypeName::BVEC2 => Token::BVec2,
                    types::TypeName::BVEC3 => Token::BVec3,
                    types::TypeName::BVEC4 => Token::BVec4,
                    types::TypeName::UINT => Token::UInt,
                    types::TypeName::ATOMIC_UINT => Token::AtomicUInt,
                    types::TypeName::UVEC2 => Token::UVec2,
                    types::TypeName::UVEC3 => Token::UVec3,
                    types::TypeName::UVEC4 => Token::UVec4,
                    types::TypeName::DVEC2 => Token::DVec2,
                    types::TypeName::DVEC3 => Token::DVec3,
                    types::TypeName::DVEC4 => Token::DVec4,
                    types::TypeName::MAT2 => Token::Mat2,
                    types::TypeName::MAT3 => Token::Mat3,
                    types::TypeName::MAT4 => Token::Mat4,
                    types::TypeName::MAT2X2 => Token::Mat2x2,
                    types::TypeName::MAT2X3 => Token::Mat2x3,
                    types::TypeName::MAT2X4 => Token::Mat2x4,
                    types::TypeName::MAT3X2 => Token::Mat3x2,
                    types::TypeName::MAT3X3 => Token::Mat3x3,
                    types::TypeName::MAT3X4 => Token::Mat3x4,
                    types::TypeName::MAT4X2 => Token::Mat4x2,
                    types::TypeName::MAT4X3 => Token::Mat4x3,
                    types::TypeName::MAT4X4 => Token::Mat4x4,
                    types::TypeName::DMAT2 => Token::DMat2,
                    types::TypeName::DMAT3 => Token::DMat3,
                    types::TypeName::DMAT4 => Token::DMat4,
                    types::TypeName::DMAT2X2 => Token::DMat2x2,
                    types::TypeName::DMAT2X3 => Token::DMat2x3,
                    types::TypeName::DMAT2X4 => Token::DMat2x4,
                    types::TypeName::DMAT3X2 => Token::DMat3x2,
                    types::TypeName::DMAT3X3 => Token::DMat3x3,
                    types::TypeName::DMAT3X4 => Token::DMat3x4,
                    types::TypeName::DMAT4X2 => Token::DMat4x2,
                    types::TypeName::DMAT4X3 => Token::DMat4x3,
                    types::TypeName::DMAT4X4 => Token::DMat4x4,
                    types::TypeName::SAMPLER1D => Token::Sampler1D,
                    types::TypeName::SAMPLER1DSHADOW => Token::Sampler1DShadow,
                    types::TypeName::SAMPLER1DARRAY => Token::Sampler1DArray,
                    types::TypeName::SAMPLER1DARRAYSHADOW => Token::Sampler1DArrayShadow,
                    types::TypeName::ISAMPLER1D => Token::ISampler1D,
                    types::TypeName::ISAMPLER1DARRAY => Token::ISampler1DArray,
                    types::TypeName::USAMPLER1D => Token::USampler1D,
                    types::TypeName::USAMPLER1DARRAY => Token::USampler1DArray,
                    types::TypeName::SAMPLER2D => Token::Sampler2D,
                    types::TypeName::SAMPLER2DSHADOW => Token::Sampler2DShadow,
                    types::TypeName::SAMPLER2DARRAY => Token::Sampler2DArray,
                    types::TypeName::SAMPLER2DARRAYSHADOW => Token::Sampler2DArrayShadow,
                    types::TypeName::ISAMPLER2D => Token::ISampler2D,
                    types::TypeName::ISAMPLER2DARRAY => Token::ISampler2DArray,
                    types::TypeName::USAMPLER2D => Token::USampler2D,
                    types::TypeName::USAMPLER2DARRAY => Token::USampler2DArray,
                    types::TypeName::SAMPLER2DRECT => Token::Sampler2DRect,
                    types::TypeName::SAMPLER2DRECTSHADOW => Token::Sampler2DRectShadow,
                    types::TypeName::ISAMPLER2DRECT => Token::ISampler2DRect,
                    types::TypeName::USAMPLER2DRECT => Token::USampler2DRect,
                    types::TypeName::SAMPLER2DMS => Token::Sampler2DMs,
                    types::TypeName::ISAMPLER2DMS => Token::ISampler2DMs,
                    types::TypeName::USAMPLER2DMS => Token::USampler2DMs,
                    types::TypeName::SAMPLER2DMSARRAY => Token::Sampler2DMsArray,
                    types::TypeName::ISAMPLER2DMSARRAY => Token::ISampler2DMsArray,
                    types::TypeName::USAMPLER2DMSARRAY => Token::USampler2DMsArray,
                    types::TypeName::SAMPLER3D => Token::Sampler3D,
                    types::TypeName::ISAMPLER3D => Token::ISampler3D,
                    types::TypeName::USAMPLER3D => Token::USampler3D,
                    types::TypeName::SAMPLERCUBE => Token::SamplerCube,
                    types::TypeName::SAMPLERCUBESHADOW => Token::SamplerCubeShadow,
                    types::TypeName::ISAMPLERCUBE => Token::ISamplerCube,
                    types::TypeName::USAMPLERCUBE => Token::USamplerCube,
                    types::TypeName::SAMPLERCUBEARRAY => Token::SamplerCubeArray,
                    types::TypeName::SAMPLERCUBEARRAYSHADOW => Token::SamplerCubeArrayShadow,
                    types::TypeName::ISAMPLERCUBEARRAY => Token::ISamplerCubeArray,
                    types::TypeName::USAMPLERCUBEARRAY => Token::USamplerCubeArray,
                    types::TypeName::SAMPLERBUFFER => Token::SamplerBuffer,
                    types::TypeName::ISAMPLERBUFFER => Token::ISamplerBuffer,
                    types::TypeName::USAMPLERBUFFER => Token::USamplerBuffer,
                    types::TypeName::IMAGE1D => Token::Image1D,
                    types::TypeName::IIMAGE1D => Token::IImage1D,
                    types::TypeName::UIMAGE1D => Token::UImage1D,
                    types::TypeName::IMAGE1DARRAY => Token::Image1DArray,
                    types::TypeName::IIMAGE1DARRAY => Token::IImage1DArray,
                    types::TypeName::UIMAGE1DARRAY => Token::UImage1DArray,
                    types::TypeName::IMAGE2D => Token::Image2D,
                    types::TypeName::IIMAGE2D => Token::IImage2D,
                    types::TypeName::UIMAGE2D => Token::UImage2D,
                    types::TypeName::IMAGE2DARRAY => Token::Image2DArray,
                    types::TypeName::IIMAGE2DARRAY => Token::IImage2DArray,
                    types::TypeName::UIMAGE2DARRAY => Token::UImage2DArray,
                    types::TypeName::IMAGE2DRECT => Token::Image2DRect,
                    types::TypeName::IIMAGE2DRECT => Token::IImage2DRect,
                    types::TypeName::UIMAGE2DRECT => Token::UImage2DRect,
                    types::TypeName::IMAGE2DMS => Token::Image2DMs,
                    types::TypeName::IIMAGE2DMS => Token::IImage2DMs,
                    types::TypeName::UIMAGE2DMS => Token::UImage2DMs,
                    types::TypeName::IMAGE2DMSARRAY => Token::Image2DMsArray,
                    types::TypeName::IIMAGE2DMSARRAY => Token::IImage2DMsArray,
                    types::TypeName::UIMAGE2DMSARRAY => Token::UImage2DMsArray,
                    types::TypeName::IMAGE3D => Token::Image3D,
                    types::TypeName::IIMAGE3D => Token::IImage3D,
                    types::TypeName::UIMAGE3D => Token::UImage3D,
                    types::TypeName::IMAGECUBE => Token::ImageCube,
                    types::TypeName::IIMAGECUBE => Token::IImageCube,
                    types::TypeName::UIMAGECUBE => Token::UImageCube,
                    types::TypeName::IMAGECUBEARRAY => Token::ImageCubeArray,
                    types::TypeName::IIMAGECUBEARRAY => Token::IImageCubeArray,
                    types::TypeName::UIMAGECUBEARRAY => Token::UImageCubeArray,
                    types::TypeName::IMAGEBUFFER => Token::ImageBuffer,
                    types::TypeName::IIMAGEBUFFER => Token::IImageBuffer,
                    types::TypeName::UIMAGEBUFFER => Token::UImageBuffer,
                    types::TypeName::TEXTURE1D => Token::Texture1D,
                    types::TypeName::TEXTURE1DARRAY => Token::Texture1DArray,
                    types::TypeName::ITEXTURE1D => Token::ITexture1D,
                    types::TypeName::ITEXTURE1DARRAY => Token::ITexture1DArray,
                    types::TypeName::UTEXTURE1D => Token::UTexture1D,
                    types::TypeName::UTEXTURE1DARRAY => Token::UTexture1DArray,
                    types::TypeName::TEXTURE2D => Token::Texture2D,
                    types::TypeName::TEXTURE2DARRAY => Token::Texture2DArray,
                    types::TypeName::ITEXTURE2D => Token::ITexture2D,
                    types::TypeName::ITEXTURE2DARRAY => Token::ITexture2DArray,
                    types::TypeName::UTEXTURE2D => Token::UTexture2D,
                    types::TypeName::UTEXTURE2DARRAY => Token::UTexture2DArray,
                    types::TypeName::TEXTURE2DRECT => Token::Texture2DRect,
                    types::TypeName::ITEXTURE2DRECT => Token::ITexture2DRect,
                    types::TypeName::UTEXTURE2DRECT => Token::UTexture2DRect,
                    types::TypeName::TEXTURE2DMS => Token::Texture2DMs,
                    types::TypeName::ITEXTURE2DMS => Token::ITexture2DMs,
                    types::TypeName::UTEXTURE2DMS => Token::UTexture2DMs,
                    types::TypeName::TEXTURE2DMSARRAY => Token::Texture2DMsArray,
                    types::TypeName::ITEXTURE2DMSARRAY => Token::ITexture2DMsArray,
                    types::TypeName::UTEXTURE2DMSARRAY => Token::UTexture2DMsArray,
                    types::TypeName::TEXTURE3D => Token::Texture3D,
                    types::TypeName::ITEXTURE3D => Token::ITexture3D,
                    types::TypeName::UTEXTURE3D => Token::UTexture3D,
                    types::TypeName::TEXTURECUBE => Token::TextureCube,
                    types::TypeName::ITEXTURECUBE => Token::ITextureCube,
                    types::TypeName::UTEXTURECUBE => Token::UTextureCube,
                    types::TypeName::TEXTURECUBEARRAY => Token::TextureCubeArray,
                    types::TypeName::ITEXTURECUBEARRAY => Token::ITextureCubeArray,
                    types::TypeName::UTEXTURECUBEARRAY => Token::UTextureCubeArray,
                    types::TypeName::TEXTUREBUFFER => Token::TextureBuffer,
                    types::TypeName::ITEXTUREBUFFER => Token::ITextureBuffer,
                    types::TypeName::UTEXTUREBUFFER => Token::UTextureBuffer,
                    types::TypeName::SAMPLER => Token::Sampler,
                    types::TypeName::SAMPLERSHADOW => Token::SamplerShadow,
                    types::TypeName::SUBPASSINPUT => Token::SubpassInput,
                    types::TypeName::ISUBPASSINPUT => Token::ISubpassInput,
                    types::TypeName::USUBPASSINPUT => Token::USubpassInput,
                    types::TypeName::SUBPASSINPUTMS => Token::SubpassInputMs,
                    types::TypeName::ISUBPASSINPUTMS => Token::ISubpassInputMs,
                    types::TypeName::USUBPASSINPUTMS => Token::USubpassInputMs,
                    other => Token::TypeName(other.to_string().into()),
                },
                types::Token::FLOAT_CONST(val) => Token::FloatConstant(val),
                types::Token::INT_CONST(val) => Token::IntConstant(val),
                types::Token::UINT_CONST(val) => Token::UIntConstant(val),
                types::Token::BOOL_CONST(val) => Token::BoolConstant(val),
                types::Token::DOUBLE_CONST(val) => Token::DoubleConstant(val),
                types::Token::LEFT_OP => Token::LeftOp,
                types::Token::RIGHT_OP => Token::RightOp,
                types::Token::INC_OP => Token::IncOp,
                types::Token::DEC_OP => Token::DecOp,
                types::Token::LE_OP => Token::LeOp,
                types::Token::GE_OP => Token::GeOp,
                types::Token::EQ_OP => Token::EqOp,
                types::Token::NE_OP => Token::NeOp,
                types::Token::AND_OP => Token::AndOp,
                types::Token::OR_OP => Token::OrOp,
                types::Token::XOR_OP => Token::XorOp,
                types::Token::MUL_ASSIGN => Token::MulAssign,
                types::Token::DIV_ASSIGN => Token::DivAssign,
                types::Token::ADD_ASSIGN => Token::AddAssign,
                types::Token::MOD_ASSIGN => Token::ModAssign,
                types::Token::LEFT_ASSIGN => Token::LeftAssign,
                types::Token::RIGHT_ASSIGN => Token::RightAssign,
                types::Token::AND_ASSIGN => Token::AndAssign,
                types::Token::XOR_ASSIGN => Token::XorAssign,
                types::Token::OR_ASSIGN => Token::OrAssign,
                types::Token::SUB_ASSIGN => Token::SubAssign,
                types::Token::LPAREN => Token::LeftParen,
                types::Token::RPAREN => Token::RightParen,
                types::Token::LBRACKET => Token::LeftBracket,
                types::Token::RBRACKET => Token::RightBracket,
                types::Token::LBRACE => Token::LeftBrace,
                types::Token::RBRACE => Token::RightBrace,
                types::Token::PERIOD => Token::Dot,
                types::Token::COMMA => Token::Comma,
                types::Token::COLON => Token::Colon,
                types::Token::EQUAL => Token::Equal,
                types::Token::SEMICOLON => Token::Semicolon,
                types::Token::BANG => Token::Bang,
                types::Token::DASH => Token::Dash,
                types::Token::TILDE => Token::Tilde,
                types::Token::PLUS => Token::Plus,
                types::Token::ASTERISK => Token::Star,
                types::Token::SLASH => Token::Slash,
                types::Token::PERCENT => Token::Percent,
                types::Token::LANGLE => Token::LeftAngle,
                types::Token::RANGLE => Token::RightAngle,
                types::Token::BAR => Token::VerticalBar,
                types::Token::CARET => Token::Caret,
                types::Token::AMPERSAND => Token::Ampersand,
                types::Token::QUESTION => Token::Question,
                types::Token::HASH => {
                    return Err(None);
                }
                types::Token::CONST => Token::Const,
                types::Token::UNIFORM => Token::Uniform,
                types::Token::BUFFER => Token::Buffer,
                types::Token::SHARED => Token::Shared,
                types::Token::COHERENT => Token::Coherent,
                types::Token::VOLATILE => Token::Volatile,
                types::Token::RESTRICT => Token::Restrict,
                types::Token::READONLY => Token::ReadOnly,
                types::Token::WRITEONLY => Token::WriteOnly,
                types::Token::LAYOUT => Token::Layout,
                types::Token::CENTROID => Token::Centroid,
                types::Token::FLAT => Token::Flat,
                types::Token::SMOOTH => Token::Smooth,
                types::Token::NOPERSPECTIVE => Token::NoPerspective,
                types::Token::PATCH => Token::Patch,
                types::Token::SAMPLE => Token::Sample,
                types::Token::INVARIANT => Token::Invariant,
                types::Token::PRECISE => Token::Precise,
                types::Token::BREAK => Token::Break,
                types::Token::CONTINUE => Token::Continue,
                types::Token::DO => Token::Do,
                types::Token::FOR => Token::For,
                types::Token::WHILE => Token::While,
                types::Token::SWITCH => Token::Switch,
                types::Token::CASE => Token::Case,
                types::Token::DEFAULT => Token::Default,
                types::Token::IF => Token::If,
                types::Token::ELSE => Token::Else,
                types::Token::SUBROUTINE => Token::Subroutine,
                types::Token::IN => Token::In,
                types::Token::OUT => Token::Out,
                types::Token::INOUT => Token::InOut,
                types::Token::DISCARD => Token::Discard,
                types::Token::RETURN => Token::Return,
                types::Token::LOWP => Token::LowPrecision,
                types::Token::MEDIUMP => Token::MediumPrecision,
                types::Token::HIGHP => Token::HighPrecision,
                types::Token::PRECISION => Token::Precision,
                types::Token::STRUCT => Token::Struct,
                types::Token::ATTRIBUTE => Token::Attribute,
                types::Token::VARYING => Token::Varying,
                types::Token::COMMON
                | types::Token::PARTITION
                | types::Token::ACTIVE
                | types::Token::ASM
                | types::Token::CLASS
                | types::Token::UNION
                | types::Token::ENUM
                | types::Token::TYPEDEF
                | types::Token::TEMPLATE
                | types::Token::THIS
                | types::Token::RESOURCE
                | types::Token::GOTO
                | types::Token::INLINE
                | types::Token::NOINLINE
                | types::Token::PUBLIC
                | types::Token::STATIC
                | types::Token::EXTERN
                | types::Token::EXTERNAL
                | types::Token::INTERFACE
                | types::Token::LONG
                | types::Token::SHORT
                | types::Token::HALF
                | types::Token::FIXED
                | types::Token::UNSIGNED
                | types::Token::SUPERP
                | types::Token::INPUT
                | types::Token::OUTPUT
                | types::Token::FILTER
                | types::Token::SIZEOF
                | types::Token::CAST
                | types::Token::NAMESPACE
                | types::Token::USING => {
                    return Err(Some(types::token::ErrorKind::InvalidToken));
                }
                types::Token::WS => Token::Whitespace,
                types::Token::COMMENT => {
                    if source_token.text().starts_with("//") {
                        Token::SingleLineComment
                    } else {
                        Token::MultiLineComment
                    }
                }
                types::Token::ERROR(kind) => {
                    return Err(Some(kind));
                }
            },
            end,
        ))
    }

    pub fn handle_file_id(&mut self, file_id: FileId) {
        self.file_id = file_id;
    }

    pub fn handle_token<'r, I, E>(
        &self,
        source_token: OutputToken,
        token_kind: types::Token,
        state: TokenState,
        tokenizer: &mut Tokenizer<'r, I>,
        token_state: &mut HandleTokenResult<E>,
    ) where
        E: std::error::Error + 'static,
        I: Iterator<Item = Result<event::Event, Located<E>>> + LocatedIterator,
        <Tokenizer<'r, I> as Iterator>::Item: MaybeToken,
    {
        if state.active() {
            match self.lang_token(&source_token, token_kind) {
                Ok(token) => {
                    // Try to get the next token when we encounter trivia
                    match token.1 {
                        Token::Whitespace => {}
                        Token::SingleLineComment | Token::MultiLineComment => {
                            if self.ctx.has_comments() {
                                let mut text = source_token.text().split_at(2).1.to_string();

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

                            token_state.push_item(Ok(token));
                        }
                    }
                }

                Err(error) => {
                    if !self.opts.allow_rs_ident || error.is_some() {
                        token_state.push_item(Err(LexicalError::Token {
                            kind: error.unwrap_or(types::token::ErrorKind::InvalidToken),
                            pos: source_token.text_range(),
                        }));
                    } else {
                        // Try to detect #(...)
                        let start = self.position(source_token.text_range().start());
                        let mut end = start;

                        let mut pending_items = VecDeque::new();
                        let mut pending_events = VecDeque::new();

                        pending_items.push_back(Err(LexicalError::Token {
                            kind: types::token::ErrorKind::InvalidToken,
                            pos: NodeSpan::new(
                                start.source_id,
                                TextRange::new(start.offset, end.offset),
                            ),
                        }));

                        while let Some(maybe_lparen_result) = tokenizer.next() {
                            // Skip whitespace
                            if let Some(types::Token::WS) = maybe_lparen_result.as_token_kind() {
                                pending_events.push_back(maybe_lparen_result);
                                continue;
                            }

                            if let Some(types::Token::LPAREN) = maybe_lparen_result.as_token_kind()
                            {
                                // We have seen a left parenthesis
                                // Now, consume everything until the
                                // matching rparen
                                let mut level = 1;
                                let mut quoted = "#(".to_owned();

                                while level > 0 {
                                    match tokenizer.next() {
                                        Some(result) => {
                                            if let Some((source_token, token_kind, _)) =
                                                result.as_token()
                                            {
                                                match token_kind {
                                                    types::Token::LPAREN => level += 1,
                                                    types::Token::RPAREN => level -= 1,
                                                    _ => {}
                                                }

                                                if level > 0 {
                                                    quoted.push_str(source_token.text());
                                                } else {
                                                    end = source_token.text_range().start();
                                                }
                                            }
                                        }
                                        None => {
                                            // End of file: return an error. Since the same error
                                            // we would return has been added for the # token, just
                                            // exit the loop
                                            *token_state = HandleTokenResult::Pending(
                                                pending_items,
                                                pending_events,
                                            );
                                            return;
                                        }
                                    }
                                }

                                quoted.push(')');
                                token_state.push_item(Ok((
                                    start,
                                    Token::Identifier(quoted.into()),
                                    end,
                                )));
                                return;
                            } else {
                                pending_events.push_back(maybe_lparen_result);
                                *token_state =
                                    HandleTokenResult::Pending(pending_items, pending_events);
                                return;
                            }
                        }
                    }
                }
            };
        }
    }

    pub fn handle_directive(
        &mut self,
        directive: EventDirective,
        masked: bool,
    ) -> Result<(), Vec<Error>> {
        if masked {
            return Ok(());
        }

        if directive.errors().is_empty() {
            Ok(())
        } else {
            Err(directive.into_errors())
        }
    }

    pub fn position(&self, offset: impl Into<TextSize>) -> LexerPosition {
        LexerPosition::new(self.file_id, offset.into())
    }
}
