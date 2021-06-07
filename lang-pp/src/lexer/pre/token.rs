#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum Token {
    /// Identifier or keyword
    IDENT_KW,
    /// Digit sequence
    DIGITS,
    /// .
    PERIOD,
    /// +
    PLUS,
    /// -
    DASH,
    /// /
    SLASH,
    /// *
    ASTERISK,
    /// %
    PERCENT,
    /// <
    LANGLE,
    /// >
    RANGLE,
    /// [
    LBRACKET,
    /// ]
    RBRACKET,
    /// (
    LPAREN,
    /// )
    RPAREN,
    /// {
    LBRACE,
    /// }
    RBRACE,
    /// ^
    CARET,
    /// |
    BAR,
    /// &
    AMPERSAND,
    /// ~
    TILDE,
    /// =
    EQUAL,
    /// !
    BANG,
    /// :
    COLON,
    /// ;
    SEMICOLON,
    /// ,
    COMMA,
    /// ?
    QUESTION,
    /// #
    HASH,
    /// "string"
    QUOTE_STRING,
    /// <string>
    ANGLE_STRING,
    /// \
    BACKSLASH,
    /// Whitespace
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

    pub fn from_punct(raw: &str) -> Self {
        use Token::*;

        match raw {
            "." => PERIOD,
            "+" => PLUS,
            "-" => DASH,
            "/" => SLASH,
            "*" => ASTERISK,
            "%" => PERCENT,
            "<" => LANGLE,
            ">" => RANGLE,
            "[" => LBRACKET,
            "]" => RBRACKET,
            "(" => LPAREN,
            ")" => RPAREN,
            "{" => LBRACE,
            "}" => RBRACE,
            "^" => CARET,
            "|" => BAR,
            "&" => AMPERSAND,
            "~" => TILDE,
            "=" => EQUAL,
            "!" => BANG,
            ":" => COLON,
            ";" => SEMICOLON,
            "," => COMMA,
            "?" => QUESTION,
            "#" => HASH,
            _ => ERROR,
        }
    }
}
