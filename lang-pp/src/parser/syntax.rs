use rowan::Checkpoint;

use lang_util::{SmolStr, TextRange};

use crate::lexer;

use super::{ErrorKind, ExpectAny, ParserRun, SyntaxKind::*};

type InputToken = lexer::Token;

pub fn file(parser: &mut ParserRun) {
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

pub fn define_body(parser: &mut ParserRun) {
    // Consume trivia first
    parser.eat_trivia();

    parser.start_node(PP_DEFINE_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia
    parser.eat_trivia();
}

/// Parse a control line
fn if_section_or_control_line(parser: &mut ParserRun) {
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
                "(" => {
                    parser.bump();
                    match pp_rs_ident(parser, token) {
                        Ok(()) => {
                            // Abort parsing a control directive, we parsed an rs_ident
                            return;
                        }
                        Err(err) => {
                            error = err.into();
                        }
                    }
                    None
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

fn pp_rs_ident(
    parser: &mut ParserRun,
    token: lexer::TextToken,
) -> Result<(), (ErrorKind, TextRange)> {
    let mut level = 1;
    while level > 0 {
        // Skip whitespace
        parser.skip_trivia();

        let peeked = parser.peek().map(|tt| SmolStr::from(parser.text(tt)));
        match peeked.as_deref() {
            Some("(") => {
                level += 1;
                parser.bump();
            }
            Some(")") => {
                level -= 1;
                parser.bump();
            }
            Some(_) => {
                parser.bump();
            }
            None => {
                return Err((
                    ErrorKind::EndOfInput {
                        expected: Box::new([lexer::Token::RPAREN]),
                    },
                    token.range,
                ));
            }
        }
    }

    Ok(())
}

fn pp_include(parser: &mut ParserRun) {
    // We're about to parse a path
    parser.input.set_expect_angle_string(true);

    parser.skip_trivia();

    // Consume include path
    parser.start_node(PP_INCLUDE_PATH);
    pp_tokens(parser);
    parser.finish_node();

    parser.eat_trivia();
}

fn pp_define(parser: &mut ParserRun) {
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
            match parser.expect_any(
                &[InputToken::IDENT_KW, InputToken::RPAREN],
                &[InputToken::NEWLINE],
            ) {
                ExpectAny::Found(found) => {
                    match *found {
                        InputToken::IDENT_KW => {
                            // Ident already bumped by expect_any
                            parser.start_node_at(arg_checkpoint, PP_DEFINE_ARG);
                            parser.finish_node();
                        }
                        InputToken::RPAREN => {
                            // We're done, LPAREN followed by RPAREN is an empty arg list
                            break;
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                }

                ExpectAny::Unexpected(other) => {
                    // Something else, propagate error
                    if let Some(checkpoint) = checkpoint.take() {
                        parser.start_node_at(checkpoint, ERROR);
                    }

                    if *other == InputToken::NEWLINE {
                        break;
                    }
                }

                ExpectAny::EndOfInput => {
                    if let Some(checkpoint) = checkpoint.take() {
                        parser.start_node_at(checkpoint, ERROR);
                    }

                    break;
                }
            }

            parser.skip_trivia();

            match parser.expect_any(
                &[InputToken::COMMA, InputToken::RPAREN],
                &[InputToken::NEWLINE],
            ) {
                ExpectAny::Found(found) => {
                    match *found {
                        InputToken::COMMA => {
                            // More identifiers to come
                        }
                        InputToken::RPAREN => {
                            // We're done
                            break;
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                }

                ExpectAny::Unexpected(other) => {
                    // Something else, propagate error
                    if let Some(checkpoint) = checkpoint.take() {
                        parser.start_node_at(checkpoint, ERROR);
                    }

                    if *other == InputToken::NEWLINE {
                        break;
                    }
                }

                ExpectAny::EndOfInput => {
                    if let Some(checkpoint) = checkpoint.take() {
                        parser.start_node_at(checkpoint, ERROR);
                    }

                    break;
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

fn pp_line(parser: &mut ParserRun) {
    parser.skip_trivia();

    // Consume line body
    parser.start_node(PP_LINE_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_error(parser: &mut ParserRun) {
    parser.skip_trivia();

    // Consume define body
    parser.start_node(PP_ERROR_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_pragma(parser: &mut ParserRun) {
    parser.skip_trivia();

    // Consume define body
    parser.start_node(PP_PRAGMA_BODY);
    pp_tokens(parser);
    parser.finish_node();

    // Finish eating trivia, not part of body
    parser.eat_trivia();
}

fn pp_version(parser: &mut ParserRun) {
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

fn pp_if_expr(parser: &mut ParserRun) {
    parser.skip_trivia();

    // Consume if expr
    // We can't parse it yet since it might need preprocessing
    parser.start_node(PP_IF_EXPR);
    pp_tokens(parser);
    parser.finish_node();

    parser.eat_trivia();
}

fn pp_if_ident(parser: &mut ParserRun) {
    parser.skip_trivia();

    parser.start_node(PP_IDENT);
    ident(parser);
    parser.finish_node();
}

fn pp_extension(parser: &mut ParserRun) {
    parser.skip_trivia();

    // Extension name
    ident(parser);

    parser.skip_trivia();

    if let ExpectAny::Found(_) = parser.expect_any(&[InputToken::COLON], &[InputToken::NEWLINE]) {
        parser.skip_trivia();

        // Extension behavior
        ident(parser);
    } else {
        // Let the main pp parser deal with the error
    }
}

fn digits(parser: &mut ParserRun) {
    let checkpoint = parser.checkpoint();

    match parser.expect_one(InputToken::DIGITS) {
        ExpectAny::Found(_) => {}
        ExpectAny::Unexpected(_) | ExpectAny::EndOfInput => {
            parser.start_node_at(checkpoint, ERROR);
            parser.finish_node();
        }
    }
}

fn ident(parser: &mut ParserRun) {
    let checkpoint = parser.checkpoint();

    match parser.expect_one(InputToken::IDENT_KW) {
        ExpectAny::Found(_) => {}
        ExpectAny::Unexpected(_) | ExpectAny::EndOfInput => {
            parser.start_node_at(checkpoint, ERROR);
            parser.finish_node();
        }
    }
}

fn pp_concat(parser: &mut ParserRun, checkpoint: Checkpoint) {
    // Start the concat node
    parser.start_node_at(checkpoint, PP_CONCAT);

    // We know there's a ## pending
    parser.bump();

    // Then, loop until the next non-trivial node
    loop {
        parser.buffer_trivia();

        if let Some(current) = parser.peek() {
            match *current {
                InputToken::NEWLINE => {
                    // End of directive
                    break;
                }
                InputToken::PP_CONCAT => {
                    // "nested" concatenation
                    parser.eat_trivia();
                    let checkpoint = parser.checkpoint();
                    pp_concat(parser, checkpoint);
                }
                _ => {
                    // Since we buffered trivia, this is a non-trivial token
                    parser.eat_trivia();
                    parser.bump();
                }
            }
        }
    }

    // Finish the concat node
    parser.finish_node();
}

fn pp_tokens(parser: &mut ParserRun) {
    // The replacement body is everything until the new-line

    // Checkpoint for maybe wrapping in a PP_CONCAT node
    let mut checkpoint = parser.checkpoint();

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
                InputToken::PP_CONCAT => {
                    // Consume trivia first
                    parser.eat_trivia();

                    // ## op, turn this into a node, consuming the checkpoint
                    let checkpoint = std::mem::replace(&mut checkpoint, parser.checkpoint());
                    pp_concat(parser, checkpoint);
                }
                _ => {
                    // Anything else: include buffered trivia in the body, and include the
                    // new non-trivial token
                    parser.eat_trivia();

                    // Update checkpoint
                    checkpoint = parser.checkpoint();

                    parser.bump();
                }
            }
        } else {
            // TODO: EOI is an error for preprocessor directives?
            break;
        }
    }
}
