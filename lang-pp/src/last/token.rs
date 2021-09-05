use crate::{
    parser::SyntaxKind,
    processor::event::TokenLike,
    types::{
        keywords::KeywordAtom,
        token::ErrorKind,
        type_names::{TypeNameAtom, TypeNameState},
        Token, TypeName,
    },
};

pub(super) fn token_from_syntax_kind(
    value: &impl TokenLike,
    version: u16,
    target_vulkan: bool,
    is_type_name: impl Fn(&TypeNameAtom) -> TypeNameState,
) -> (Token, Option<TypeNameState>) {
    use Token::*;
    let kind = value.kind();

    // First, try the tokens that don't require text processing
    match kind {
        SyntaxKind::PP_EMPTY
        | SyntaxKind::PP_INCLUDE
        | SyntaxKind::PP_INCLUDE_PATH
        | SyntaxKind::PP_DEFINE
        | SyntaxKind::PP_DEFINE_ARGS
        | SyntaxKind::PP_DEFINE_ARG
        | SyntaxKind::PP_DEFINE_BODY
        | SyntaxKind::PP_UNDEF
        | SyntaxKind::PP_IDENT
        | SyntaxKind::PP_IF
        | SyntaxKind::PP_IF_EXPR
        | SyntaxKind::PP_IFDEF
        | SyntaxKind::PP_IFNDEF
        | SyntaxKind::PP_ELSE
        | SyntaxKind::PP_ELIF
        | SyntaxKind::PP_ENDIF
        | SyntaxKind::PP_ERROR
        | SyntaxKind::PP_ERROR_BODY
        | SyntaxKind::PP_PRAGMA
        | SyntaxKind::PP_PRAGMA_BODY
        | SyntaxKind::PP_EXTENSION
        | SyntaxKind::PP_VERSION
        | SyntaxKind::PP_VERSION_NUMBER
        | SyntaxKind::PP_VERSION_PROFILE
        | SyntaxKind::PP_LINE
        | SyntaxKind::PP_LINE_BODY
        | SyntaxKind::ERROR
        | SyntaxKind::ROOT
        | SyntaxKind::_LAST
        | SyntaxKind::QUOTE_STRING
        | SyntaxKind::ANGLE_STRING
        | SyntaxKind::BACKSLASH
        | SyntaxKind::DEFINED
        | SyntaxKind::PP_CONCAT
        | SyntaxKind::PP_CONCAT_OP => {
            return (ERROR(ErrorKind::InvalidToken), None);
        }
        // Those need further processing
        SyntaxKind::IDENT_KW => {}
        SyntaxKind::DIGITS => {}
        SyntaxKind::LEFT_OP => {
            return (LEFT_OP, None);
        }
        SyntaxKind::RIGHT_OP => {
            return (RIGHT_OP, None);
        }
        SyntaxKind::INC_OP => {
            return (INC_OP, None);
        }
        SyntaxKind::DEC_OP => {
            return (DEC_OP, None);
        }
        SyntaxKind::LE_OP => {
            return (LE_OP, None);
        }
        SyntaxKind::GE_OP => {
            return (GE_OP, None);
        }
        SyntaxKind::EQ_OP => {
            return (EQ_OP, None);
        }
        SyntaxKind::NE_OP => {
            return (NE_OP, None);
        }
        SyntaxKind::AND_OP => {
            return (AND_OP, None);
        }
        SyntaxKind::OR_OP => {
            return (OR_OP, None);
        }
        SyntaxKind::XOR_OP => {
            return (XOR_OP, None);
        }
        SyntaxKind::MUL_ASSIGN => {
            return (MUL_ASSIGN, None);
        }
        SyntaxKind::DIV_ASSIGN => {
            return (DIV_ASSIGN, None);
        }
        SyntaxKind::ADD_ASSIGN => {
            return (ADD_ASSIGN, None);
        }
        SyntaxKind::MOD_ASSIGN => {
            return (MOD_ASSIGN, None);
        }
        SyntaxKind::LEFT_ASSIGN => {
            return (LEFT_ASSIGN, None);
        }
        SyntaxKind::RIGHT_ASSIGN => {
            return (RIGHT_ASSIGN, None);
        }
        SyntaxKind::AND_ASSIGN => {
            return (AND_ASSIGN, None);
        }
        SyntaxKind::XOR_ASSIGN => {
            return (XOR_ASSIGN, None);
        }
        SyntaxKind::OR_ASSIGN => {
            return (OR_ASSIGN, None);
        }
        SyntaxKind::SUB_ASSIGN => {
            return (SUB_ASSIGN, None);
        }
        SyntaxKind::LPAREN => {
            return (LPAREN, None);
        }
        SyntaxKind::RPAREN => {
            return (RPAREN, None);
        }
        SyntaxKind::LBRACKET => {
            return (LBRACKET, None);
        }
        SyntaxKind::RBRACKET => {
            return (RBRACKET, None);
        }
        SyntaxKind::LBRACE => {
            return (LBRACE, None);
        }
        SyntaxKind::RBRACE => {
            return (RBRACE, None);
        }
        SyntaxKind::PERIOD => {
            return (PERIOD, None);
        }
        SyntaxKind::COMMA => {
            return (COMMA, None);
        }
        SyntaxKind::COLON => {
            return (COLON, None);
        }
        SyntaxKind::EQUAL => {
            return (EQUAL, None);
        }
        SyntaxKind::SEMICOLON => {
            return (SEMICOLON, None);
        }
        SyntaxKind::BANG => {
            return (BANG, None);
        }
        SyntaxKind::DASH => {
            return (DASH, None);
        }
        SyntaxKind::TILDE => {
            return (TILDE, None);
        }
        SyntaxKind::PLUS => {
            return (PLUS, None);
        }
        SyntaxKind::ASTERISK => {
            return (ASTERISK, None);
        }
        SyntaxKind::SLASH => {
            return (SLASH, None);
        }
        SyntaxKind::PERCENT => {
            return (PERCENT, None);
        }
        SyntaxKind::LANGLE => {
            return (LANGLE, None);
        }
        SyntaxKind::RANGLE => {
            return (RANGLE, None);
        }
        SyntaxKind::BAR => {
            return (BAR, None);
        }
        SyntaxKind::CARET => {
            return (CARET, None);
        }
        SyntaxKind::AMPERSAND => {
            return (AMPERSAND, None);
        }
        SyntaxKind::QUESTION => {
            return (QUESTION, None);
        }
        SyntaxKind::HASH => {
            return (HASH, None);
        }
        SyntaxKind::WS | SyntaxKind::NEWLINE => {
            return (WS, None);
        }
        SyntaxKind::COMMENT => {
            return (COMMENT, None);
        }
    }

    // Either IDENT_KW or DIGITS, we need to examine the text to know more
    let text = value.text().to_string();
    if kind == SyntaxKind::IDENT_KW {
        // Is this a keyword?
        let keyword_atom = KeywordAtom::from(text.as_ref());

        if let Some(keyword) = Token::parse_kw(&keyword_atom) {
            return (keyword, None);
        }

        // Else it might be a built-in type name
        if let Some((type_name, state)) =
            TypeName::parse(text.as_ref(), version, target_vulkan, is_type_name)
        {
            return (TYPE_NAME(type_name), state);
        }

        // Nothing matched, it's actually an ident
        (IDENT(text.into()), None)
    } else if kind == SyntaxKind::DIGITS {
        (Token::parse_digits(&text), None)
    } else {
        unreachable!()
    }
}
