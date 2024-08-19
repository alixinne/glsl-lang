#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum Token {
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
    /// Line continuation (required for tracking exact offsets)
    LINECONT = 35,
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
    PP_CONCAT = 58,
}

impl Token {
    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::LINECONT | Self::WS | Self::COMMENT)
    }
}

impl From<crate::lexer::PreToken> for Token {
    fn from(pre: crate::lexer::PreToken) -> Self {
        // SAFETY: Token is a superset of PreToken
        unsafe { std::mem::transmute(std::mem::transmute::<crate::lexer::PreToken, u16>(pre)) }
    }
}
