#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum Token {
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
    /// ##
    PP_CONCAT,
    // Single-char tokens
    /// (
    LPAREN,
    /// )
    RPAREN,
    /// [
    LBRACKET,
    /// ]
    RBRACKET,
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
    /// Line continuation (required for tracking exact offsets)
    LINECONT,
    /// Invalid token
    ERROR,
}

impl Token {
    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::LINECONT | Self::WS | Self::COMMENT)
    }
}

impl From<crate::lexer::PreToken> for Token {
    fn from(pre: crate::lexer::PreToken) -> Self {
        use Token::*;

        match pre {
            crate::lexer::PreToken::IDENT_KW => IDENT_KW,
            crate::lexer::PreToken::DIGITS => DIGITS,
            crate::lexer::PreToken::PERIOD => PERIOD,
            crate::lexer::PreToken::PLUS => PLUS,
            crate::lexer::PreToken::DASH => DASH,
            crate::lexer::PreToken::SLASH => SLASH,
            crate::lexer::PreToken::ASTERISK => ASTERISK,
            crate::lexer::PreToken::PERCENT => PERCENT,
            crate::lexer::PreToken::LANGLE => LANGLE,
            crate::lexer::PreToken::RANGLE => RANGLE,
            crate::lexer::PreToken::LBRACKET => LBRACKET,
            crate::lexer::PreToken::RBRACKET => RBRACKET,
            crate::lexer::PreToken::LPAREN => LPAREN,
            crate::lexer::PreToken::RPAREN => RPAREN,
            crate::lexer::PreToken::LBRACE => LBRACE,
            crate::lexer::PreToken::RBRACE => RBRACE,
            crate::lexer::PreToken::CARET => CARET,
            crate::lexer::PreToken::BAR => BAR,
            crate::lexer::PreToken::AMPERSAND => AMPERSAND,
            crate::lexer::PreToken::TILDE => TILDE,
            crate::lexer::PreToken::EQUAL => EQUAL,
            crate::lexer::PreToken::BANG => BANG,
            crate::lexer::PreToken::COLON => COLON,
            crate::lexer::PreToken::SEMICOLON => SEMICOLON,
            crate::lexer::PreToken::COMMA => COMMA,
            crate::lexer::PreToken::QUESTION => QUESTION,
            crate::lexer::PreToken::HASH => HASH,
            crate::lexer::PreToken::QUOTE_STRING => QUOTE_STRING,
            crate::lexer::PreToken::ANGLE_STRING => ANGLE_STRING,
            crate::lexer::PreToken::BACKSLASH => BACKSLASH,
            crate::lexer::PreToken::WS => WS,
            crate::lexer::PreToken::NEWLINE => NEWLINE,
            crate::lexer::PreToken::COMMENT => COMMENT,
            crate::lexer::PreToken::LINECONT => LINECONT,
            crate::lexer::PreToken::ERROR => ERROR,
        }
    }
}
