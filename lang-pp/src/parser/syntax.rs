use rowan::TextRange;
use smol_str::SmolStr;

use crate::lexer;

use super::SyntaxKind::*;
use super::{ErrorKind, ParserRun};

type InputToken = lexer::Token;

pub fn file<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    loop {
        // We need to buffer trivia before we can detect a control line
        parser.buffer_trivia();

        if let Some(token) = parser.peek() {
            match *token {
                InputToken::HASH => if_section_or_control_line(parser),
                InputToken::NEWLINE => {
                    // If we encounter a newline (which is not trivia because of the pp), bump
                    // it on its own and restart
                    parser.eat_trivia();
                    parser.bump();
                }
                _ => {
                    parser.eat_trivia();

                    while let Some(token) = parser.peek() {
                        // Bump all tokens, including the newline
                        parser.bump();

                        if *token == InputToken::NEWLINE {
                            // A newline completes the token sequence
                            break;
                        }
                    }
                }
            }
        } else {
            parser.eat_trivia();
            break;
        }
    }
}

pub fn define_body<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    // Consume trivia first
    parser.eat_trivia();

    parser.start_node(PP_DEFINE_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia
    parser.eat_trivia();
}

/// Parse a control line
fn if_section_or_control_line<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    let checkpoint = parser.checkpoint();

    parser.eat_trivia();

    // #
    parser.bump();

    // Whitespace
    parser.skip_trivia();

    // Find directive type
    let mut pp_type_name = None;
    let pp_type = if let Some(token) = parser.peek() {
        if *token == InputToken::NEWLINE {
            // Empty, don't bump newline
            Some(PP_EMPTY)
        } else {
            let mut error = None;
            let name = SmolStr::from(parser.text(token));
            pp_type_name = Some(name.clone());
            let result = match name.as_ref() {
                "include" => {
                    parser.bump();
                    pp_include(parser);
                    Some(PP_INCLUDE)
                }
                "define" => {
                    parser.bump();
                    pp_define(parser);
                    Some(PP_DEFINE)
                }
                "undef" => {
                    parser.bump();
                    pp_if_ident(parser);
                    Some(PP_UNDEF)
                }
                "line" => {
                    parser.bump();
                    pp_line(parser);
                    Some(PP_LINE)
                }
                "error" => {
                    parser.bump();
                    pp_error(parser);
                    Some(PP_ERROR)
                }
                "pragma" => {
                    parser.bump();
                    pp_pragma(parser);
                    Some(PP_PRAGMA)
                }
                "version" => {
                    parser.bump();
                    pp_version(parser);
                    Some(PP_VERSION)
                }
                "if" => {
                    parser.bump();
                    pp_if_expr(parser);
                    Some(PP_IF)
                }
                "ifdef" => {
                    parser.bump();
                    pp_if_ident(parser);
                    Some(PP_IFDEF)
                }
                "ifndef" => {
                    parser.bump();
                    pp_if_ident(parser);
                    Some(PP_IFNDEF)
                }
                "elif" => {
                    parser.bump();
                    // Also parses an expr
                    pp_if_expr(parser);
                    Some(PP_ELIF)
                }
                "else" => {
                    parser.bump();
                    // Nothing to parse
                    Some(PP_ELSE)
                }
                "endif" => {
                    parser.bump();
                    // Nothing to parse
                    Some(PP_ENDIF)
                }
                "extension" => {
                    parser.bump();
                    pp_extension(parser);
                    Some(PP_EXTENSION)
                }
                other => {
                    error = Some((
                        ErrorKind::UnknownPreprocessorDirective { name: other.into() },
                        token.range,
                    ));
                    None
                }
            };

            if let Some(error) = error {
                parser.push_error(error.0, error.1);
            }

            result
        }
    } else {
        None
    };

    // Consume trivia before checking we're at a newline
    parser.skip_trivia();

    // Consume the newline, or EOI
    match parser.peek() {
        Some(token) if *token == InputToken::NEWLINE => {
            parser.bump();
        }
        None => {
            // Nothing to bump here
        }
        Some(other) => {
            // Anything else is an error
            let mut start = other.range;

            // Bump until newline into an ERROR
            parser.start_node(ERROR);

            while let Some(token) = parser.peek() {
                // Break when we encounter the newline
                if *token == InputToken::NEWLINE {
                    break;
                }

                // Else, extend the range
                start = TextRange::new(start.start(), token.range.end());
                parser.bump();
            }

            // Finish the error node
            parser.finish_node();

            // Bump the remaining newline
            if let Some(InputToken::NEWLINE) = parser.peek().as_deref() {
                parser.bump();
            }

            // Note the error, unless it's an unknown directive: it's unknown, so don't notify an
            // error twice
            if pp_type.is_some() {
                parser.push_error(
                    ErrorKind::ExtraTokensInPreprocessorDirective {
                        name: pp_type_name.unwrap(),
                    },
                    start,
                );
            }
        }
    }

    // Finish parsing
    match pp_type {
        Some(t) => {
            parser.start_node_at(checkpoint, t);
            parser.finish_node();
        }
        None => {
            parser.start_node_at(checkpoint, ERROR);
            parser.finish_node();
        }
    }
}

fn pp_include<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    // We're about to parse a path
    parser.input.set_expect_angle_string(true);

    parser.skip_trivia();

    // Consume include path
    parser.start_node(PP_INCLUDE_PATH);
    pp_include_path(parser);
    parser.finish_node();

    parser.eat_trivia();
}

fn pp_include_path<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    if let Some(token) = parser.peek() {
        match *token {
            InputToken::ANGLE_STRING | InputToken::QUOTE_STRING => {
                parser.bump();
            }
            _ => {
                // Third form of include, just pp tokens
                pp_tokens(parser);
            }
        }
    }
}

fn pp_define<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Define name
    ident(parser);

    // Is it a function define or an object one?
    if let Some(InputToken::LPAREN) = parser.peek().as_deref() {
        // Immediate LPAREN: function-like
        let mut checkpoint = Some(parser.checkpoint());

        // Bump LPAREN
        parser.bump();

        // Read ident, comma sequence
        loop {
            parser.skip_trivia();

            let arg_checkpoint = parser.checkpoint();
            match parser
                .expect_any(
                    &[InputToken::IDENT_KW, InputToken::RPAREN],
                    &[InputToken::NEWLINE],
                )
                .as_deref()
            {
                Some(InputToken::IDENT_KW) => {
                    // Ident already bumped by expect_any
                    parser.start_node_at(arg_checkpoint, PP_DEFINE_ARG);
                    parser.finish_node();
                }
                Some(InputToken::RPAREN) => {
                    // We're done, LPAREN followed by RPAREN is an empty arg list
                    break;
                }
                _ => {
                    // Something else, propagate error
                    parser.start_node_at(checkpoint.take().unwrap(), ERROR);
                }
            }

            parser.skip_trivia();

            match parser
                .expect_any(
                    &[InputToken::COMMA, InputToken::RPAREN],
                    &[InputToken::NEWLINE],
                )
                .as_deref()
            {
                Some(InputToken::COMMA) => {
                    // More identifiers to come
                }
                Some(InputToken::RPAREN) => {
                    // We're done
                    break;
                }
                _ => {
                    // The error was already bumped and recorded by expect_any
                    parser.start_node_at(checkpoint.take().unwrap(), ERROR);
                }
            }
        }

        // Finish the checkpointed node
        if let Some(checkpoint) = checkpoint.take() {
            parser.start_node_at(checkpoint, PP_DEFINE_ARGS);
        }

        parser.finish_node();
    } else {
        // Something else: object-like
    }

    // Skip trivia after args/object-like name
    parser.skip_trivia();

    // Consume define body
    parser.start_node(PP_DEFINE_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_line<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Consume line body
    parser.start_node(PP_LINE_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_error<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Consume define body
    parser.start_node(PP_ERROR_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_pragma<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Consume define body
    parser.start_node(PP_PRAGMA_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_version<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Version
    parser.start_node(PP_VERSION_NUMBER);
    digits(parser);
    parser.finish_node();

    parser.skip_trivia();

    // Profile, if any
    if let Some(InputToken::IDENT_KW) = parser.peek().as_deref() {
        parser.start_node(PP_VERSION_PROFILE);
        parser.bump();
        parser.finish_node();
    }
}

fn pp_if_expr<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Consume if expr
    // We can't parse it yet since it might need preprocessing
    parser.start_node(PP_IF_EXPR);
    pp_tokens(parser);
    parser.finish_node();

    parser.eat_trivia();
}

fn pp_if_ident<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    parser.start_node(PP_IDENT);
    ident(parser);
    parser.finish_node();
}

fn pp_extension<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    parser.skip_trivia();

    // Extension name
    ident(parser);

    parser.skip_trivia();

    if let Some(InputToken::COLON) = parser
        .expect_any(&[InputToken::COLON], &[InputToken::NEWLINE])
        .as_deref()
    {
        parser.skip_trivia();

        // Extension behavior
        ident(parser);
    } else {
        // Let the main pp parser deal with the error
    }
}

fn digits<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    if let Some(InputToken::DIGITS) = parser.peek().as_deref() {
        parser.bump();
    } else {
        parser.start_node(ERROR);
        parser.bump();
        parser.finish_node();
    }
}

fn ident<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    if let Some(InputToken::IDENT_KW) = parser.peek().as_deref() {
        parser.bump();
    } else {
        parser.start_node(ERROR);
        parser.bump();
        parser.finish_node();
    }
}

fn pp_tokens<'i, 'cache>(parser: &mut ParserRun<'i, 'cache>) {
    // The replacement body is everything until the new-line
    loop {
        // Consume all trivia first
        parser.buffer_trivia();

        // Check if there are tokens left
        if let Some(current) = parser.peek() {
            match *current {
                InputToken::NEWLINE => {
                    // Newline terminates body
                    break;
                }
                _ => {
                    // Anything else: include buffered trivia in the body, and include the
                    // new non-trivial token
                    parser.eat_trivia();
                    parser.bump();
                }
            }
        } else {
            // TODO: EOI is an error for preprocessor directives?
            break;
        }
    }
}
