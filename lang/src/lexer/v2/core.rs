use std::collections::VecDeque;

use glsl_lang_pp::{
    last::{self, type_names::TypeNameAtom, LocatedIterator, MaybeToken, TokenState, Tokenizer},
    processor::{
        event::{self, DirectiveKind, Error, Located, OutputToken, SyntaxNode, TokenLike},
        expand::ExpandLocation,
        str::ProcessStrError,
    },
};

use lang_util::{FileId, NodeContent};

use crate::parse::ParseContext;

use super::{LexerPosition, LexicalError, Token};

pub type Item<E> = Result<(LexerPosition, Token, LexerPosition), LexicalError<E>>;

pub struct LexerCore {
    pub ctx: ParseContext,
    file_id: FileId,
}

pub enum HandleTokenResult<E: std::error::Error + 'static> {
    None,
    Item(Item<E>),
    Pending(VecDeque<Item<E>>, VecDeque<Result<last::Event, E>>),
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

    pub fn pop_event(&mut self) -> Option<Result<last::Event, E>> {
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
}

impl<E: std::error::Error + 'static> Default for HandleTokenResult<E> {
    fn default() -> Self {
        Self::None
    }
}

impl LexerCore {
    pub fn new(ctx: ParseContext) -> Self {
        let file_id = ctx.opts.source_id;
        Self { ctx, file_id }
    }

    fn lang_token<'r, I, T, E>(
        &self,
        source_token: &OutputToken,
        token_kind: last::Token,
        tokenizer: &mut Tokenizer<'r, I>,
    ) -> Result<(LexerPosition, Token, LexerPosition), Option<last::token::ErrorKind>>
    where
        I: Iterator<Item = Result<T, E>> + LocatedIterator,
    {
        let start = self.position(source_token.text_range().start());
        let end = self.position(source_token.text_range().end());

        Ok((
            start,
            match token_kind {
                last::Token::IDENT(ident) => {
                    if self.ctx.is_type_name(&ident) {
                        // It was promoted to a type name downstream
                        tokenizer
                            .promote_type_name(TypeNameAtom::from(ident.as_str()));
                        Token::TypeName(ident)
                    } else {
                        // It is an identifier
                        Token::Identifier((ident, self.ctx.clone()))
                    }
                }
                last::Token::TYPE_NAME(type_name) => match type_name {
                    last::TypeName::VOID => Token::Void,
                    last::TypeName::INT => Token::Int,
                    last::TypeName::BOOL => Token::Bool,
                    last::TypeName::FLOAT => Token::Float,
                    last::TypeName::DOUBLE => Token::Double,
                    last::TypeName::VEC2 => Token::Vec2,
                    last::TypeName::VEC3 => Token::Vec3,
                    last::TypeName::VEC4 => Token::Vec4,
                    last::TypeName::IVEC2 => Token::IVec2,
                    last::TypeName::IVEC3 => Token::IVec3,
                    last::TypeName::IVEC4 => Token::IVec4,
                    last::TypeName::BVEC2 => Token::BVec2,
                    last::TypeName::BVEC3 => Token::BVec3,
                    last::TypeName::BVEC4 => Token::BVec4,
                    last::TypeName::UINT => Token::UInt,
                    last::TypeName::ATOMIC_UINT => Token::AtomicUInt,
                    last::TypeName::UVEC2 => Token::UVec2,
                    last::TypeName::UVEC3 => Token::UVec3,
                    last::TypeName::UVEC4 => Token::UVec4,
                    last::TypeName::DVEC2 => Token::DVec2,
                    last::TypeName::DVEC3 => Token::DVec3,
                    last::TypeName::DVEC4 => Token::DVec4,
                    last::TypeName::MAT2 => Token::Mat2,
                    last::TypeName::MAT3 => Token::Mat3,
                    last::TypeName::MAT4 => Token::Mat4,
                    last::TypeName::MAT2X2 => Token::Mat2x2,
                    last::TypeName::MAT2X3 => Token::Mat2x3,
                    last::TypeName::MAT2X4 => Token::Mat2x4,
                    last::TypeName::MAT3X2 => Token::Mat3x2,
                    last::TypeName::MAT3X3 => Token::Mat3x3,
                    last::TypeName::MAT3X4 => Token::Mat3x4,
                    last::TypeName::MAT4X2 => Token::Mat4x2,
                    last::TypeName::MAT4X3 => Token::Mat4x3,
                    last::TypeName::MAT4X4 => Token::Mat4x4,
                    last::TypeName::DMAT2 => Token::DMat2,
                    last::TypeName::DMAT3 => Token::DMat3,
                    last::TypeName::DMAT4 => Token::DMat4,
                    last::TypeName::DMAT2X2 => Token::DMat2x2,
                    last::TypeName::DMAT2X3 => Token::DMat2x3,
                    last::TypeName::DMAT2X4 => Token::DMat2x4,
                    last::TypeName::DMAT3X2 => Token::DMat3x2,
                    last::TypeName::DMAT3X3 => Token::DMat3x3,
                    last::TypeName::DMAT3X4 => Token::DMat3x4,
                    last::TypeName::DMAT4X2 => Token::DMat4x2,
                    last::TypeName::DMAT4X3 => Token::DMat4x3,
                    last::TypeName::DMAT4X4 => Token::DMat4x4,
                    last::TypeName::SAMPLER1D => Token::Sampler1D,
                    last::TypeName::SAMPLER1DSHADOW => Token::Sampler1DShadow,
                    last::TypeName::SAMPLER1DARRAY => Token::Sampler1DArray,
                    last::TypeName::SAMPLER1DARRAYSHADOW => Token::Sampler1DArrayShadow,
                    last::TypeName::ISAMPLER1D => Token::ISampler1D,
                    last::TypeName::ISAMPLER1DARRAY => Token::ISampler1DArray,
                    last::TypeName::USAMPLER1D => Token::USampler1D,
                    last::TypeName::USAMPLER1DARRAY => Token::USampler1DArray,
                    last::TypeName::SAMPLER2D => Token::Sampler2D,
                    last::TypeName::SAMPLER2DSHADOW => Token::Sampler2DShadow,
                    last::TypeName::SAMPLER2DARRAY => Token::Sampler2DArray,
                    last::TypeName::SAMPLER2DARRAYSHADOW => Token::Sampler2DArrayShadow,
                    last::TypeName::ISAMPLER2D => Token::ISampler2D,
                    last::TypeName::ISAMPLER2DARRAY => Token::ISampler2DArray,
                    last::TypeName::USAMPLER2D => Token::USampler2D,
                    last::TypeName::USAMPLER2DARRAY => Token::USampler2DArray,
                    last::TypeName::SAMPLER2DRECT => Token::Sampler2DRect,
                    last::TypeName::SAMPLER2DRECTSHADOW => Token::Sampler2DRectShadow,
                    last::TypeName::ISAMPLER2DRECT => Token::ISampler2DRect,
                    last::TypeName::USAMPLER2DRECT => Token::USampler2DRect,
                    last::TypeName::SAMPLER2DMS => Token::Sampler2DMs,
                    last::TypeName::ISAMPLER2DMS => Token::ISampler2DMs,
                    last::TypeName::USAMPLER2DMS => Token::USampler2DMs,
                    last::TypeName::SAMPLER2DMSARRAY => Token::Sampler2DMsArray,
                    last::TypeName::ISAMPLER2DMSARRAY => Token::ISampler2DMsArray,
                    last::TypeName::USAMPLER2DMSARRAY => Token::USampler2DMsArray,
                    last::TypeName::SAMPLER3D => Token::Sampler3D,
                    last::TypeName::ISAMPLER3D => Token::ISampler3D,
                    last::TypeName::USAMPLER3D => Token::USampler3D,
                    last::TypeName::SAMPLERCUBE => Token::SamplerCube,
                    last::TypeName::SAMPLERCUBESHADOW => Token::SamplerCubeShadow,
                    last::TypeName::ISAMPLERCUBE => Token::ISamplerCube,
                    last::TypeName::USAMPLERCUBE => Token::USamplerCube,
                    last::TypeName::SAMPLERCUBEARRAY => Token::SamplerCubeArray,
                    last::TypeName::SAMPLERCUBEARRAYSHADOW => Token::SamplerCubeArrayShadow,
                    last::TypeName::ISAMPLERCUBEARRAY => Token::ISamplerCubeArray,
                    last::TypeName::USAMPLERCUBEARRAY => Token::USamplerCubeArray,
                    last::TypeName::SAMPLERBUFFER => Token::SamplerBuffer,
                    last::TypeName::ISAMPLERBUFFER => Token::ISamplerBuffer,
                    last::TypeName::USAMPLERBUFFER => Token::USamplerBuffer,
                    last::TypeName::IMAGE1D => Token::Image1D,
                    last::TypeName::IIMAGE1D => Token::IImage1D,
                    last::TypeName::UIMAGE1D => Token::UImage1D,
                    last::TypeName::IMAGE1DARRAY => Token::Image1DArray,
                    last::TypeName::IIMAGE1DARRAY => Token::IImage1DArray,
                    last::TypeName::UIMAGE1DARRAY => Token::UImage1DArray,
                    last::TypeName::IMAGE2D => Token::Image2D,
                    last::TypeName::IIMAGE2D => Token::IImage2D,
                    last::TypeName::UIMAGE2D => Token::UImage2D,
                    last::TypeName::IMAGE2DARRAY => Token::Image2DArray,
                    last::TypeName::IIMAGE2DARRAY => Token::IImage2DArray,
                    last::TypeName::UIMAGE2DARRAY => Token::UImage2DArray,
                    last::TypeName::IMAGE2DRECT => Token::Image2DRect,
                    last::TypeName::IIMAGE2DRECT => Token::IImage2DRect,
                    last::TypeName::UIMAGE2DRECT => Token::UImage2DRect,
                    last::TypeName::IMAGE2DMS => Token::Image2DMs,
                    last::TypeName::IIMAGE2DMS => Token::IImage2DMs,
                    last::TypeName::UIMAGE2DMS => Token::UImage2DMs,
                    last::TypeName::IMAGE2DMSARRAY => Token::Image2DMsArray,
                    last::TypeName::IIMAGE2DMSARRAY => Token::IImage2DMsArray,
                    last::TypeName::UIMAGE2DMSARRAY => Token::UImage2DMsArray,
                    last::TypeName::IMAGE3D => Token::Image3D,
                    last::TypeName::IIMAGE3D => Token::IImage3D,
                    last::TypeName::UIMAGE3D => Token::UImage3D,
                    last::TypeName::IMAGECUBE => Token::ImageCube,
                    last::TypeName::IIMAGECUBE => Token::IImageCube,
                    last::TypeName::UIMAGECUBE => Token::UImageCube,
                    last::TypeName::IMAGECUBEARRAY => Token::ImageCubeArray,
                    last::TypeName::IIMAGECUBEARRAY => Token::IImageCubeArray,
                    last::TypeName::UIMAGECUBEARRAY => Token::UImageCubeArray,
                    last::TypeName::IMAGEBUFFER => Token::ImageBuffer,
                    last::TypeName::IIMAGEBUFFER => Token::IImageBuffer,
                    last::TypeName::UIMAGEBUFFER => Token::UImageBuffer,
                    last::TypeName::TEXTURE1D => Token::Texture1D,
                    last::TypeName::TEXTURE1DARRAY => Token::Texture1DArray,
                    last::TypeName::ITEXTURE1D => Token::ITexture1D,
                    last::TypeName::ITEXTURE1DARRAY => Token::ITexture1DArray,
                    last::TypeName::UTEXTURE1D => Token::UTexture1D,
                    last::TypeName::UTEXTURE1DARRAY => Token::UTexture1DArray,
                    last::TypeName::TEXTURE2D => Token::Texture2D,
                    last::TypeName::TEXTURE2DARRAY => Token::Texture2DArray,
                    last::TypeName::ITEXTURE2D => Token::ITexture2D,
                    last::TypeName::ITEXTURE2DARRAY => Token::ITexture2DArray,
                    last::TypeName::UTEXTURE2D => Token::UTexture2D,
                    last::TypeName::UTEXTURE2DARRAY => Token::UTexture2DArray,
                    last::TypeName::TEXTURE2DRECT => Token::Texture2DRect,
                    last::TypeName::ITEXTURE2DRECT => Token::ITexture2DRect,
                    last::TypeName::UTEXTURE2DRECT => Token::UTexture2DRect,
                    last::TypeName::TEXTURE2DMS => Token::Texture2DMs,
                    last::TypeName::ITEXTURE2DMS => Token::ITexture2DMs,
                    last::TypeName::UTEXTURE2DMS => Token::UTexture2DMs,
                    last::TypeName::TEXTURE2DMSARRAY => Token::Texture2DMsArray,
                    last::TypeName::ITEXTURE2DMSARRAY => Token::ITexture2DMsArray,
                    last::TypeName::UTEXTURE2DMSARRAY => Token::UTexture2DMsArray,
                    last::TypeName::TEXTURE3D => Token::Texture3D,
                    last::TypeName::ITEXTURE3D => Token::ITexture3D,
                    last::TypeName::UTEXTURE3D => Token::UTexture3D,
                    last::TypeName::TEXTURECUBE => Token::TextureCube,
                    last::TypeName::ITEXTURECUBE => Token::ITextureCube,
                    last::TypeName::UTEXTURECUBE => Token::UTextureCube,
                    last::TypeName::TEXTURECUBEARRAY => Token::TextureCubeArray,
                    last::TypeName::ITEXTURECUBEARRAY => Token::ITextureCubeArray,
                    last::TypeName::UTEXTURECUBEARRAY => Token::UTextureCubeArray,
                    last::TypeName::TEXTUREBUFFER => Token::TextureBuffer,
                    last::TypeName::ITEXTUREBUFFER => Token::ITextureBuffer,
                    last::TypeName::UTEXTUREBUFFER => Token::UTextureBuffer,
                    last::TypeName::SAMPLER => Token::Sampler,
                    last::TypeName::SAMPLERSHADOW => Token::SamplerShadow,
                    last::TypeName::SUBPASSINPUT => Token::SubpassInput,
                    last::TypeName::ISUBPASSINPUT => Token::ISubpassInput,
                    last::TypeName::USUBPASSINPUT => Token::USubpassInput,
                    last::TypeName::SUBPASSINPUTMS => Token::SubpassInputMs,
                    last::TypeName::ISUBPASSINPUTMS => Token::ISubpassInputMs,
                    last::TypeName::USUBPASSINPUTMS => Token::USubpassInputMs,
                    other => Token::TypeName(other.to_string().into()),
                },
                last::Token::FLOAT_CONST(val) => Token::FloatConstant(val),
                last::Token::INT_CONST(val) => Token::IntConstant(val),
                last::Token::UINT_CONST(val) => Token::UIntConstant(val),
                last::Token::BOOL_CONST(val) => Token::BoolConstant(val),
                last::Token::DOUBLE_CONST(val) => Token::DoubleConstant(val),
                last::Token::LEFT_OP => Token::LeftOp,
                last::Token::RIGHT_OP => Token::RightOp,
                last::Token::INC_OP => Token::IncOp,
                last::Token::DEC_OP => Token::DecOp,
                last::Token::LE_OP => Token::LeOp,
                last::Token::GE_OP => Token::GeOp,
                last::Token::EQ_OP => Token::EqOp,
                last::Token::NE_OP => Token::NeOp,
                last::Token::AND_OP => Token::AndOp,
                last::Token::OR_OP => Token::OrOp,
                last::Token::XOR_OP => Token::XorOp,
                last::Token::MUL_ASSIGN => Token::MulAssign,
                last::Token::DIV_ASSIGN => Token::DivAssign,
                last::Token::ADD_ASSIGN => Token::AddAssign,
                last::Token::MOD_ASSIGN => Token::ModAssign,
                last::Token::LEFT_ASSIGN => Token::LeftAssign,
                last::Token::RIGHT_ASSIGN => Token::RightAssign,
                last::Token::AND_ASSIGN => Token::AndAssign,
                last::Token::XOR_ASSIGN => Token::XorAssign,
                last::Token::OR_ASSIGN => Token::OrAssign,
                last::Token::SUB_ASSIGN => Token::SubAssign,
                last::Token::LPAREN => Token::LeftParen,
                last::Token::RPAREN => Token::RightParen,
                last::Token::LBRACKET => Token::LeftBracket,
                last::Token::RBRACKET => Token::RightBracket,
                last::Token::LBRACE => Token::LeftBrace,
                last::Token::RBRACE => Token::RightBrace,
                last::Token::PERIOD => Token::Dot,
                last::Token::COMMA => Token::Comma,
                last::Token::COLON => Token::Colon,
                last::Token::EQUAL => Token::Equal,
                last::Token::SEMICOLON => Token::Semicolon,
                last::Token::BANG => Token::Bang,
                last::Token::DASH => Token::Dash,
                last::Token::TILDE => Token::Tilde,
                last::Token::PLUS => Token::Plus,
                last::Token::ASTERISK => Token::Star,
                last::Token::SLASH => Token::Slash,
                last::Token::PERCENT => Token::Percent,
                last::Token::LANGLE => Token::LeftAngle,
                last::Token::RANGLE => Token::RightAngle,
                last::Token::BAR => Token::VerticalBar,
                last::Token::CARET => Token::Caret,
                last::Token::AMPERSAND => Token::Ampersand,
                last::Token::QUESTION => Token::Question,
                last::Token::HASH => {
                    return Err(None);
                }
                last::Token::CONST => Token::Const,
                last::Token::UNIFORM => Token::Uniform,
                last::Token::BUFFER => Token::Buffer,
                last::Token::SHARED => Token::Shared,
                last::Token::COHERENT => Token::Coherent,
                last::Token::VOLATILE => Token::Volatile,
                last::Token::RESTRICT => Token::Restrict,
                last::Token::READONLY => Token::ReadOnly,
                last::Token::WRITEONLY => Token::WriteOnly,
                last::Token::LAYOUT => Token::Layout,
                last::Token::CENTROID => Token::Centroid,
                last::Token::FLAT => Token::Flat,
                last::Token::SMOOTH => Token::Smooth,
                last::Token::NOPERSPECTIVE => Token::NoPerspective,
                last::Token::PATCH => Token::Patch,
                last::Token::SAMPLE => Token::Sample,
                last::Token::INVARIANT => Token::Invariant,
                last::Token::PRECISE => Token::Precise,
                last::Token::BREAK => Token::Break,
                last::Token::CONTINUE => Token::Continue,
                last::Token::DO => Token::Do,
                last::Token::FOR => Token::For,
                last::Token::WHILE => Token::While,
                last::Token::SWITCH => Token::Switch,
                last::Token::CASE => Token::Case,
                last::Token::DEFAULT => Token::Default,
                last::Token::IF => Token::If,
                last::Token::ELSE => Token::Else,
                last::Token::SUBROUTINE => Token::Subroutine,
                last::Token::IN => Token::In,
                last::Token::OUT => Token::Out,
                last::Token::INOUT => Token::InOut,
                last::Token::DISCARD => Token::Discard,
                last::Token::RETURN => Token::Return,
                last::Token::LOWP => Token::LowPrecision,
                last::Token::MEDIUMP => Token::MediumPrecision,
                last::Token::HIGHP => Token::HighPrecision,
                last::Token::PRECISION => Token::Precision,
                last::Token::STRUCT => Token::Struct,
                // GLSL <4
                last::Token::ATTRIBUTE |
                // GLSL <4
                last::Token::VARYING |
                last::Token::COMMON
                | last::Token::PARTITION
                | last::Token::ACTIVE
                | last::Token::ASM
                | last::Token::CLASS
                | last::Token::UNION
                | last::Token::ENUM
                | last::Token::TYPEDEF
                | last::Token::TEMPLATE
                | last::Token::THIS
                | last::Token::RESOURCE
                | last::Token::GOTO
                | last::Token::INLINE
                | last::Token::NOINLINE
                | last::Token::PUBLIC
                | last::Token::STATIC
                | last::Token::EXTERN
                | last::Token::EXTERNAL
                | last::Token::INTERFACE
                | last::Token::LONG
                | last::Token::SHORT
                | last::Token::HALF
                | last::Token::FIXED
                | last::Token::UNSIGNED
                | last::Token::SUPERP
                | last::Token::INPUT
                | last::Token::OUTPUT
                | last::Token::FILTER
                | last::Token::SIZEOF
                | last::Token::CAST
                | last::Token::NAMESPACE
                | last::Token::USING => {
                    return Err(Some(last::token::ErrorKind::InvalidToken));
                }
                last::Token::WS => Token::Whitespace,
                last::Token::COMMENT => {
                    if glsl_lang_pp::util::Unescaped::new(source_token.text())
                        .chars()
                        .take(2)
                        .eq("//".chars())
                    {
                        Token::SingleLineComment
                    } else {
                        Token::MultiLineComment
                    }
                }
                last::Token::ERROR(kind) => {
                    return Err(Some(kind));
                }
            },
            end,
        ))
    }

    pub fn handle_error<E: std::error::Error + 'static>(
        &self,
        error: Error,
        masked: bool,
    ) -> Option<Item<E>> {
        if !masked {
            // Replace source id
            let error = error.with_file_id(self.file_id);
            // Return errorr
            Some(Err(LexicalError::Processor(error)))
        } else {
            None
        }
    }

    pub fn handle_file_id(&mut self, file_id: FileId) {
        self.file_id = file_id;
    }

    pub fn handle_token<'r, I, E>(
        &self,
        source_token: OutputToken,
        token_kind: last::Token,
        state: TokenState,
        tokenizer: &mut Tokenizer<'r, I>,
        token_state: &mut HandleTokenResult<E>,
    ) where
        E: std::error::Error + 'static,
        I: Iterator<Item = Result<event::Event, E>> + LocatedIterator,
        <Tokenizer<'r, I> as Iterator>::Item: MaybeToken,
    {
        if state.active() {
            match self.lang_token(&source_token, token_kind, tokenizer) {
                Ok(token) => {
                    // Try to get the next token when we encounter trivia
                    match token.1 {
                        Token::Whitespace => {}
                        Token::SingleLineComment | Token::MultiLineComment => {
                            if self.ctx.has_comments() {
                                let mut text: String =
                                    glsl_lang_pp::util::Unescaped::new(source_token.text())
                                        .chars()
                                        .skip(2)
                                        .collect();

                                let comment = match token.1 {
                                    Token::SingleLineComment => {
                                        crate::ast::CommentData::Single(text)
                                    }
                                    Token::MultiLineComment => {
                                        text.pop();
                                        text.pop();
                                        crate::ast::CommentData::Multi(text)
                                    }
                                    _ => unreachable!(),
                                }
                                .spanned(token.0, token.2);

                                self.ctx.add_comment(comment);
                            }
                        }
                        _ => {
                            token_state.push_item(Ok(token));
                        }
                    }
                }

                Err(error) => {
                    if !self.ctx.opts.allow_rs_ident || error.is_some() {
                        token_state.push_item(Err(LexicalError::Token {
                            kind: error.unwrap_or(last::token::ErrorKind::InvalidToken),
                            pos: self.position(source_token.text_range().start()),
                        }));
                    } else {
                        // Try to detect #(...)
                        let start = self.position(source_token.text_range().start());
                        let mut end = start;

                        let mut pending_items = VecDeque::new();
                        let mut pending_events = VecDeque::new();

                        pending_items.push_back(Err(LexicalError::Token {
                            kind: last::token::ErrorKind::InvalidToken,
                            pos: start,
                        }));

                        while let Some(maybe_lparen_result) = tokenizer.next() {
                            // Skip whitespace
                            if let Some(last::Token::WS) = maybe_lparen_result.as_token_kind() {
                                pending_events.push_back(maybe_lparen_result);
                                continue;
                            }

                            if let Some(last::Token::LPAREN) = maybe_lparen_result.as_token_kind() {
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
                                                    last::Token::LPAREN => level += 1,
                                                    last::Token::RPAREN => level -= 1,
                                                    _ => {}
                                                }

                                                if level > 0 {
                                                    quoted.push_str(source_token.text());
                                                } else {
                                                    end = LexerPosition::new(
                                                        self.file_id,
                                                        source_token.text_range().start().into(),
                                                    );
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
                                    Token::Identifier((quoted.into(), self.ctx.clone())),
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
        &self,
        _node: SyntaxNode,
        _kind: DirectiveKind,
        _masked: bool,
        _errors: Vec<Error>,
    ) {
        // TODO: Store directive information
    }

    pub fn handle_str_err(
        &self,
        err: ProcessStrError,
        location: &ExpandLocation,
    ) -> Item<ProcessStrError> {
        match &err {
            ProcessStrError::IncludeRequested(_, node, _) => {
                let pos = node.text_range();
                Err(LexicalError::Io(Located::new(
                    err,
                    Default::default(),
                    pos,
                    location,
                )))
            }
        }
    }

    pub fn position(&self, offset: impl Into<usize>) -> LexerPosition {
        LexerPosition::new(self.file_id, offset.into())
    }
}
