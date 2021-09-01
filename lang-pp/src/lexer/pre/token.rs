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
