use crate::lexer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum SyntaxKind {
    /// #
    PP_EMPTY,
    /// #include
    PP_INCLUDE,
    /// #include path
    PP_INCLUDE_PATH,
    /// #define
    PP_DEFINE,
    PP_DEFINE_ARGS,
    PP_DEFINE_ARG,
    PP_DEFINE_BODY,
    /// #undef
    PP_UNDEF,
    PP_IDENT,
    /// #if
    PP_IF,
    PP_IF_EXPR,
    /// #ifdef
    PP_IFDEF,
    /// #ifndef
    PP_IFNDEF,
    /// #else
    PP_ELSE,
    /// #elif
    PP_ELIF,
    /// #endif
    PP_ENDIF,
    /// #error
    PP_ERROR,
    /// #error body
    PP_ERROR_BODY,
    /// #pragma
    PP_PRAGMA,
    /// #pragma body
    PP_PRAGMA_BODY,
    /// #extension
    PP_EXTENSION,
    /// #version
    PP_VERSION,
    PP_VERSION_NUMBER,
    PP_VERSION_PROFILE,
    /// #line
    PP_LINE,
    /// #line body
    PP_LINE_BODY,
    /// defined
    DEFINED,
    /// Identifier or keyword
    IDENT_KW,
    /// Digit sequence
    DIGITS,
    // Multi-char tokens
    /// <<
    LEFT_OP,
    /// >>
    RIGHT_OP,
    /// ++
    INC_OP,
    /// --
    DEC_OP,
    /// <=
    LE_OP,
    /// >=
    GE_OP,
    /// ==
    EQ_OP,
    /// !=
    NE_OP,
    /// &&
    AND_OP,
    /// ||
    OR_OP,
    /// ^^
    XOR_OP,
    /// *=
    MUL_ASSIGN,
    /// /=
    DIV_ASSIGN,
    /// +=
    ADD_ASSIGN,
    /// %=
    MOD_ASSIGN,
    /// <<=
    LEFT_ASSIGN,
    /// >>=
    RIGHT_ASSIGN,
    /// &=
    AND_ASSIGN,
    /// ^=
    XOR_ASSIGN,
    /// |=
    OR_ASSIGN,
    /// -=
    SUB_ASSIGN,
    PP_CONCAT,
    /// ##
    PP_CONCAT_OP,
    // Single-char tokens
    /// (
    LPAREN,
    /// )
    RPAREN,
    /// [
    LBRACKET,
    /// ]
    RBRACKET,
    // TODO: Add double-left/right bracket for Vulkan attributes?
    /// {
    LBRACE,
    /// }
    RBRACE,
    /// .
    PERIOD,
    /// ,
    COMMA,
    /// :
    COLON,
    /// =
    EQUAL,
    /// ;
    SEMICOLON,
    /// !
    BANG,
    /// -
    DASH,
    /// ~
    TILDE,
    /// +
    PLUS,
    /// *
    ASTERISK,
    /// /
    SLASH,
    /// %
    PERCENT,
    /// <
    LANGLE,
    /// >
    RANGLE,
    /// |
    BAR,
    /// ^
    CARET,
    /// &
    AMPERSAND,
    /// ?
    QUESTION,
    /// #
    HASH,
    // Other
    /// "string"
    QUOTE_STRING,
    /// <string>
    ANGLE_STRING,
    /// \
    BACKSLASH,
    /// Whitespaace
    WS,
    /// Newline
    NEWLINE,
    /// Comment (single-line or multi-line)
    COMMENT,
    /// Invalid token
    ERROR,
    // composite nodes
    ROOT,
    _LAST,
}

impl SyntaxKind {
    pub fn is_whitespace(&self) -> bool {
        matches!(self, Self::COMMENT | Self::WS | Self::NEWLINE)
    }

    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::COMMENT | Self::WS)
    }

    pub fn is_newline(&self) -> bool {
        matches!(self, Self::NEWLINE)
    }

    pub fn paste(lhs: Self, rhs: Self) -> Self {
        use SyntaxKind::*;

        match lhs {
            IDENT_KW => match rhs {
                DEFINED | IDENT_KW | DIGITS => IDENT_KW,
                _ => ERROR,
            },
            DIGITS => match rhs {
                IDENT_KW | DIGITS | PERIOD | DASH | PLUS => DIGITS,
                _ => ERROR,
            },
            LEFT_OP => match rhs {
                EQUAL => LEFT_ASSIGN,
                _ => ERROR,
            },
            RIGHT_OP => match rhs {
                EQUAL => RIGHT_ASSIGN,
                _ => ERROR,
            },
            PERIOD => match rhs {
                DIGITS => DIGITS,
                _ => ERROR,
            },
            EQUAL => match rhs {
                EQUAL => EQ_OP,
                _ => ERROR,
            },
            BANG => match rhs {
                EQUAL => NE_OP,
                _ => ERROR,
            },
            DASH => match rhs {
                DASH => DEC_OP,
                EQUAL => SUB_ASSIGN,
                _ => ERROR,
            },
            PLUS => match rhs {
                PLUS => INC_OP,
                EQUAL => ADD_ASSIGN,
                _ => ERROR,
            },
            ASTERISK => match rhs {
                EQUAL => MUL_ASSIGN,
                _ => ERROR,
            },
            SLASH => match rhs {
                EQUAL => DIV_ASSIGN,
                _ => ERROR,
            },
            PERCENT => match rhs {
                EQUAL => MOD_ASSIGN,
                _ => ERROR,
            },
            LANGLE => match rhs {
                LANGLE => LEFT_OP,
                EQUAL => LE_OP,
                _ => ERROR,
            },
            RANGLE => match rhs {
                RANGLE => RIGHT_OP,
                EQUAL => GE_OP,
                _ => ERROR,
            },
            BAR => match rhs {
                BAR => OR_OP,
                EQUAL => OR_ASSIGN,
                _ => ERROR,
            },
            CARET => match rhs {
                CARET => XOR_OP,
                EQUAL => XOR_ASSIGN,
                _ => ERROR,
            },
            AMPERSAND => match rhs {
                AMPERSAND => AND_OP,
                EQUAL => AND_ASSIGN,
                _ => ERROR,
            },
            HASH => match rhs {
                HASH => PP_CONCAT_OP,
                _ => ERROR,
            },
            _ => ERROR,
        }
    }
}

impl From<lexer::Token> for SyntaxKind {
    fn from(s: lexer::Token) -> Self {
        use SyntaxKind::*;

        match s {
            lexer::Token::IDENT_KW => IDENT_KW,
            lexer::Token::DEFINED => DEFINED,
            lexer::Token::DIGITS => DIGITS,
            lexer::Token::PERIOD => PERIOD,
            lexer::Token::PLUS => PLUS,
            lexer::Token::DASH => DASH,
            lexer::Token::SLASH => SLASH,
            lexer::Token::ASTERISK => ASTERISK,
            lexer::Token::PERCENT => PERCENT,
            lexer::Token::LANGLE => LANGLE,
            lexer::Token::RANGLE => RANGLE,
            lexer::Token::LBRACKET => LBRACKET,
            lexer::Token::RBRACKET => RBRACKET,
            lexer::Token::LPAREN => LPAREN,
            lexer::Token::RPAREN => RPAREN,
            lexer::Token::LBRACE => LBRACE,
            lexer::Token::RBRACE => RBRACE,
            lexer::Token::CARET => CARET,
            lexer::Token::BAR => BAR,
            lexer::Token::AMPERSAND => AMPERSAND,
            lexer::Token::TILDE => TILDE,
            lexer::Token::EQUAL => EQUAL,
            lexer::Token::BANG => BANG,
            lexer::Token::COLON => COLON,
            lexer::Token::SEMICOLON => SEMICOLON,
            lexer::Token::COMMA => COMMA,
            lexer::Token::QUESTION => QUESTION,
            lexer::Token::HASH => HASH,
            lexer::Token::QUOTE_STRING => QUOTE_STRING,
            lexer::Token::ANGLE_STRING => ANGLE_STRING,
            lexer::Token::BACKSLASH => BACKSLASH,
            lexer::Token::WS => WS,
            lexer::Token::NEWLINE => NEWLINE,
            lexer::Token::COMMENT => COMMENT,
            lexer::Token::ERROR => ERROR,

            lexer::Token::LEFT_OP => LEFT_OP,
            lexer::Token::RIGHT_OP => RIGHT_OP,
            lexer::Token::INC_OP => INC_OP,
            lexer::Token::DEC_OP => DEC_OP,
            lexer::Token::LE_OP => LE_OP,
            lexer::Token::GE_OP => GE_OP,
            lexer::Token::EQ_OP => EQ_OP,
            lexer::Token::NE_OP => NE_OP,
            lexer::Token::AND_OP => AND_OP,
            lexer::Token::OR_OP => OR_OP,
            lexer::Token::XOR_OP => XOR_OP,
            lexer::Token::MUL_ASSIGN => MUL_ASSIGN,
            lexer::Token::DIV_ASSIGN => DIV_ASSIGN,
            lexer::Token::ADD_ASSIGN => ADD_ASSIGN,
            lexer::Token::MOD_ASSIGN => MOD_ASSIGN,
            lexer::Token::LEFT_ASSIGN => LEFT_ASSIGN,
            lexer::Token::RIGHT_ASSIGN => RIGHT_ASSIGN,
            lexer::Token::AND_ASSIGN => AND_ASSIGN,
            lexer::Token::XOR_ASSIGN => XOR_ASSIGN,
            lexer::Token::OR_ASSIGN => OR_ASSIGN,
            lexer::Token::SUB_ASSIGN => SUB_ASSIGN,
            lexer::Token::PP_CONCAT => PP_CONCAT_OP,

            // A stray line continuation should just be part of whitespace
            lexer::Token::LINECONT => WS,
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
