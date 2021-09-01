use crate::lexer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum SyntaxKind {
    /// Identifier or keyword
    IDENT_KW = 1,
    /// defined preprocessor keyword
    DEFINED = 2,
    /// Digit sequence
    DIGITS = 3,
    // Single-char tokens
    /// .
    PERIOD = 4,
    /// +
    PLUS = 5,
    /// -
    DASH = 6,
    /// /
    SLASH = 7,
    /// *
    ASTERISK = 8,
    /// %
    PERCENT = 9,
    /// <
    LANGLE = 10,
    /// >
    RANGLE = 11,
    /// [
    LBRACKET = 12,
    /// ]
    RBRACKET = 13,
    /// (
    LPAREN = 14,
    /// )
    RPAREN = 15,
    /// {
    LBRACE = 16,
    /// }
    RBRACE = 17,
    /// ^
    CARET = 18,
    /// |
    BAR = 19,
    /// &
    AMPERSAND = 20,
    /// ~
    TILDE = 21,
    /// =
    EQUAL = 22,
    /// !
    BANG = 23,
    /// :
    COLON = 24,
    /// ;
    SEMICOLON = 25,
    /// ,
    COMMA = 26,
    /// ?
    QUESTION = 27,
    /// #
    HASH = 28,
    // Other
    /// "string"
    QUOTE_STRING = 29,
    /// <string>
    ANGLE_STRING = 30,
    /// \
    BACKSLASH = 31,
    /// Whitespace
    WS = 32,
    /// Newline
    NEWLINE = 33,
    /// Comment (single-line or multi-line)
    COMMENT = 34,
    // Replaced by WS in this step
    // LINECONT = 35,
    /// Invalid token
    ERROR = 36,
    // Multi-char tokens
    /// <<
    LEFT_OP = 37,
    /// >>
    RIGHT_OP = 38,
    /// ++
    INC_OP = 39,
    /// --
    DEC_OP = 40,
    /// <=
    LE_OP = 41,
    /// >=
    GE_OP = 42,
    /// ==
    EQ_OP = 43,
    /// !=
    NE_OP = 44,
    /// &&
    AND_OP = 45,
    /// ||
    OR_OP = 46,
    /// ^^
    XOR_OP = 47,
    /// *=
    MUL_ASSIGN = 48,
    /// /=
    DIV_ASSIGN = 49,
    /// +=
    ADD_ASSIGN = 50,
    /// %=
    MOD_ASSIGN = 51,
    /// <<=
    LEFT_ASSIGN = 52,
    /// >>=
    RIGHT_ASSIGN = 53,
    /// &=
    AND_ASSIGN = 54,
    /// ^=
    XOR_ASSIGN = 55,
    /// |=
    OR_ASSIGN = 56,
    /// -=
    SUB_ASSIGN = 57,
    /// ##
    PP_CONCAT_OP = 58,
    // Extra types
    /// Concatenation expression
    PP_CONCAT,
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
    /// AST root
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

        // A stray line continuation should just be part of whitespace
        if s == lexer::Token::LINECONT {
            return WS;
        }

        // SAFETY: Aside from LINECONT, SyntaxKind is a superset of lexer::Token
        unsafe { std::mem::transmute(std::mem::transmute::<_, u16>(s)) }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
