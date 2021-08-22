use std::iter::Peekable;

use crate::{
    parser::SyntaxKind::{self, *},
    util::Unescaped,
};

use super::{
    event::{OutputToken, TokenLike},
    ProcessorState,
};

#[derive(Debug, Clone)]
pub struct ExprEvaluator<'i, I: Iterator<Item = &'i OutputToken>> {
    input: Peekable<I>,
    state: &'i ProcessorState,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalResult {
    Constant(Result<i32, ()>),
    Token(OutputToken),
}

impl<'i, I: Iterator<Item = &'i OutputToken>> ExprEvaluator<'i, I> {
    pub fn new(input: I, state: &'i ProcessorState) -> Self {
        Self {
            input: input.peekable(),
            state,
        }
    }

    fn bump(&mut self) -> Option<&'i OutputToken> {
        loop {
            let token = self.input.next();
            if let Some(token) = token {
                if !token.kind().is_trivia() {
                    return Some(token);
                }
            } else {
                return None;
            }
        }
    }

    fn peek(&mut self) -> Option<&'i OutputToken> {
        loop {
            let token = self.input.peek().copied();
            if let Some(token) = token {
                if !token.kind().is_trivia() {
                    return Some(token);
                } else {
                    self.input.next();
                }
            } else {
                return None;
            }
        }
    }

    fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.peek().map(|token| token.kind())
    }

    fn primary(&mut self) -> Option<Result<i32, ()>> {
        let token = self.peek()?;

        match token.kind() {
            DIGITS => {
                // Try to parse the value before bumping. If parsing fails, we'll return the DIGITS
                // token unparsed
                if let Ok(value) = lexical::parse(Unescaped::new(token.text()).to_string().as_ref())
                {
                    self.bump();
                    return Some(Ok(value));
                }
            }
            IDENT_KW => {
                self.bump();
                return Some(Ok(0));
            }
            LPAREN => {
                self.bump();

                let inner = self.expr();

                // Consume RPAREN, or return None
                if self.peek()?.kind() == RPAREN {
                    self.bump();
                    return inner;
                }
            }
            _ => {}
        }

        None
    }

    fn unary(&mut self) -> Option<Result<i32, ()>> {
        match self.peek_kind()? {
            PLUS => {
                self.bump();
                self.unary()
            }
            DASH => {
                self.bump();
                self.unary().map(|result| result.map(|val| -val))
            }
            TILDE => {
                self.bump();
                self.unary().map(|result| result.map(|val| !val))
            }
            BANG => {
                self.bump();
                self.unary()
                    .map(|result| result.map(|val| if val == 0 { 1 } else { 0 }))
            }
            DEFINED => {
                self.bump();

                match self.peek()?.kind() {
                    IDENT_KW => {
                        // Free-standing form, get the ident name
                        let ident = Unescaped::new(self.bump()?.text()).to_string();
                        return Some(Ok(if self.state.get_definition(&ident).is_some() {
                            1
                        } else {
                            0
                        }));
                    }
                    LPAREN => {
                        // Parenthesis form
                        self.bump();

                        // Try to find an ident
                        if let Some(ident) = self.peek().and_then(|token| {
                            if token.kind() == IDENT_KW {
                                Some(Unescaped::new(token.text()).to_string())
                            } else {
                                None
                            }
                        }) {
                            // Found an ident, bump it
                            self.bump();

                            // Look for the RPAREN
                            if self.peek()?.kind() == RPAREN {
                                self.bump();
                            } else {
                                // Missing RPAREN or extra tokens
                                return None;
                            }

                            return Some(Ok(if self.state.get_definition(&ident).is_some() {
                                1
                            } else {
                                0
                            }));
                        }

                        // Invalid
                        None
                    }
                    _ => {
                        // Invalid
                        None
                    }
                }
            }
            _ => self.primary(),
        }
    }

    fn binary_op(
        lhs: Option<Result<i32, ()>>,
        rhs: Option<Result<i32, ()>>,
        f: impl FnOnce(i32, i32) -> Result<(i32, bool), ()>,
    ) -> Option<Result<i32, ()>> {
        lhs.zip(rhs).map(|(lhs, rhs)| {
            lhs.and_then(|a| {
                rhs.and_then(|b| f(a, b).and_then(|(val, ovf)| if ovf { Err(()) } else { Ok(val) }))
            })
        })
    }

    fn multiplicative(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.unary();

        while let Some(kind) = self.peek_kind() {
            match kind {
                ASTERISK => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.unary(), |a, b| Ok(a.overflowing_mul(b)));
                }
                SLASH => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.unary(), |a, b| {
                        if b == 0 {
                            Err(())
                        } else {
                            Ok(a.overflowing_div(b))
                        }
                    });
                }
                PERCENT => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.unary(), |a, b| {
                        if b == 0 {
                            Err(())
                        } else {
                            Ok(a.overflowing_rem(b))
                        }
                    });
                }
                _ => {
                    break;
                }
            }
        }

        lhs
    }

    fn additive(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.multiplicative();

        while let Some(kind) = self.peek_kind() {
            match kind {
                PLUS => {
                    self.bump();
                    lhs =
                        Self::binary_op(
                            lhs,
                            self.multiplicative(),
                            |a, b| Ok(a.overflowing_add(b)),
                        );
                }
                DASH => {
                    self.bump();
                    lhs =
                        Self::binary_op(
                            lhs,
                            self.multiplicative(),
                            |a, b| Ok(a.overflowing_sub(b)),
                        );
                }
                _ => {
                    break;
                }
            }
        }

        lhs
    }

    fn shift(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.additive();

        while let Some(kind) = self.peek_kind() {
            match kind {
                LEFT_OP => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.additive(), |a, b| {
                        if b < 0 {
                            Err(())
                        } else {
                            Ok(a.overflowing_shl(b as u32))
                        }
                    });
                }
                RIGHT_OP => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.additive(), |a, b| {
                        if b < 0 {
                            Err(())
                        } else {
                            Ok(a.overflowing_shr(b as u32))
                        }
                    });
                }
                _ => {
                    break;
                }
            }
        }

        lhs
    }

    fn relational(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.shift();

        while let Some(kind) = self.peek_kind() {
            match kind {
                LANGLE => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.shift(), |a, b| {
                        Ok((if a < b { 1 } else { 0 }, false))
                    });
                }
                RANGLE => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.shift(), |a, b| {
                        Ok((if a > b { 1 } else { 0 }, false))
                    });
                }
                LE_OP => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.shift(), |a, b| {
                        Ok((if a <= b { 1 } else { 0 }, false))
                    });
                }
                GE_OP => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.shift(), |a, b| {
                        Ok((if a >= b { 1 } else { 0 }, false))
                    });
                }
                _ => {
                    break;
                }
            }
        }

        lhs
    }

    fn equality(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.relational();

        while let Some(kind) = self.peek_kind() {
            match kind {
                EQ_OP => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.relational(), |a, b| {
                        Ok((if a == b { 1 } else { 0 }, false))
                    });
                }
                NE_OP => {
                    self.bump();
                    lhs = Self::binary_op(lhs, self.relational(), |a, b| {
                        Ok((if a != b { 1 } else { 0 }, false))
                    });
                }
                _ => {
                    break;
                }
            }
        }

        lhs
    }

    fn and(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.equality();

        while self
            .peek_kind()
            .map(|kind| kind == AMPERSAND)
            .unwrap_or(false)
        {
            self.bump();
            lhs = Self::binary_op(lhs, self.equality(), |a, b| Ok((a & b, false)));
        }

        lhs
    }

    fn xor(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.and();

        while self.peek_kind().map(|kind| kind == CARET).unwrap_or(false) {
            self.bump();
            lhs = Self::binary_op(lhs, self.and(), |a, b| Ok((a ^ b, false)));
        }

        lhs
    }

    fn or(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.xor();

        while self.peek_kind().map(|kind| kind == BAR).unwrap_or(false) {
            self.bump();
            lhs = Self::binary_op(lhs, self.xor(), |a, b| Ok((a | b, false)));
        }

        lhs
    }

    fn logical_and(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.or();

        while self.peek_kind().map(|kind| kind == AND_OP).unwrap_or(false) {
            self.bump();
            lhs = Self::binary_op(lhs, self.or(), |a, b| {
                Ok((if a != 0 && b != 0 { 1 } else { 0 }, false))
            });
        }

        lhs
    }

    fn logical_or(&mut self) -> Option<Result<i32, ()>> {
        let mut lhs = self.logical_and();

        while self.peek_kind().map(|kind| kind == OR_OP).unwrap_or(false) {
            self.bump();
            lhs = Self::binary_op(lhs, self.logical_and(), |a, b| {
                Ok((if a != 0 || b != 0 { 1 } else { 0 }, false))
            });
        }

        lhs
    }

    fn expr(&mut self) -> Option<Result<i32, ()>> {
        self.logical_or()
    }

    fn next_result(&mut self) -> Option<EvalResult> {
        match self.expr() {
            Some(value) => Some(EvalResult::Constant(value)),
            None => Some(EvalResult::Token(self.bump().cloned()?)),
        }
    }
}

impl<'i, I: Iterator<Item = &'i OutputToken>> Iterator for ExprEvaluator<'i, I> {
    type Item = EvalResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_result()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use lang_util::FileId;

    use crate::{
        parser::SyntaxKind,
        processor::{
            event::{Event, TokenLike},
            nodes::{Define, DefineObject},
            ProcessorState,
        },
    };

    use super::ExprEvaluator;

    use self::EvalResult::*;

    /// Wrapper structure to compare token kinds with PartialEq instead of tokens
    #[derive(Debug, Clone, PartialEq)]
    enum EvalResult {
        Constant(Result<i32, ()>),
        Token(SyntaxKind),
    }

    fn eval(input: &str) -> Vec<EvalResult> {
        // Parse the input token sequence
        let tokens: Vec<_> = crate::processor::str::process(input, ProcessorState::default())
            .filter_map(|evt| evt.ok().and_then(Event::into_token))
            .collect();

        // Processor state for evaluation: outside of unit testing, this is provided by the
        // processor
        let mut eval_state = ProcessorState::default();
        eval_state.definition(
            Define::object(
                "IS_DEFINED".into(),
                DefineObject::from_str("1").unwrap(),
                false,
            ),
            FileId::default(),
        );

        // Evaluate
        ExprEvaluator::new(tokens.iter(), &eval_state)
            .map(|result| match result {
                super::EvalResult::Constant(value) => Constant(value),
                super::EvalResult::Token(token) => Token(token.kind()),
            })
            .collect()
    }

    #[test]
    fn test_parenthesis() {
        assert_eq!(&eval("2 + 3 * 4"), &[Constant(Ok(14))]);
        assert_eq!(&eval("(2 + 3) * 4"), &[Constant(Ok(20))]);
        assert_eq!(&eval("(((2) + (3)) * (4))"), &[Constant(Ok(20))]);
    }

    #[test]
    fn test_primary() {
        assert_eq!(&eval("0"), &[Constant(Ok(0))]);
        assert_eq!(&eval("1"), &[Constant(Ok(1))]);
        assert_eq!(&eval("FOO"), &[Constant(Ok(0))]);
    }

    #[test]
    fn test_unary() {
        assert_eq!(&eval("+0"), &[Constant(Ok(0))]);
        assert_eq!(&eval("+1"), &[Constant(Ok(1))]);
        assert_eq!(&eval("+FOO"), &[Constant(Ok(0))]);

        assert_eq!(&eval("-0"), &[Constant(Ok(0))]);
        assert_eq!(&eval("-1"), &[Constant(Ok(-1))]);
        assert_eq!(&eval("-FOO"), &[Constant(Ok(0))]);

        assert_eq!(&eval("~0"), &[Constant(Ok(!0))]);
        assert_eq!(&eval("~1"), &[Constant(Ok(!1))]);
        assert_eq!(&eval("~FOO"), &[Constant(Ok(!0))]);

        assert_eq!(&eval("!0"), &[Constant(Ok(1))]);
        assert_eq!(&eval("!1"), &[Constant(Ok(0))]);
        assert_eq!(&eval("!FOO"), &[Constant(Ok(1))]);

        assert_eq!(&eval("defined IS_DEFINED"), &[Constant(Ok(1))]);
        assert_eq!(&eval("defined NOT_DEFINED"), &[Constant(Ok(0))]);
        assert_eq!(&eval("defined ( IS_DEFINED )"), &[Constant(Ok(1))]);
        assert_eq!(&eval("defined ( NOT_DEFINED )"), &[Constant(Ok(0))]);

        assert_eq!(&eval("!defined IS_DEFINED"), &[Constant(Ok(0))]);
        assert_eq!(&eval("!defined NOT_DEFINED"), &[Constant(Ok(1))]);
        assert_eq!(&eval("!defined ( IS_DEFINED )"), &[Constant(Ok(0))]);
        assert_eq!(&eval("!defined ( NOT_DEFINED )"), &[Constant(Ok(1))]);

        // Invalid expressions
        assert_eq!(&eval("defined +"), &[]);
        assert_eq!(&eval("defined ( IS_DEFINED "), &[]);
        assert_eq!(&eval("defined ( NOT_DEFINED "), &[]);
    }

    #[test]
    fn test_multiplicative() {
        assert_eq!(&eval("1 * 2"), &[Constant(Ok(2))]);
        assert_eq!(&eval("2 * 3"), &[Constant(Ok(6))]);

        assert_eq!(&eval("1 / 2"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 / 3"), &[Constant(Ok(0))]);
        assert_eq!(&eval("6 / 2"), &[Constant(Ok(3))]);
        assert_eq!(&eval("1 / 0"), &[Constant(Err(()))]);

        assert_eq!(&eval("1 % 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("2 % 3"), &[Constant(Ok(2))]);
        assert_eq!(&eval("6 % 2"), &[Constant(Ok(0))]);
        assert_eq!(&eval("1 % 0"), &[Constant(Err(()))]);
    }

    #[test]
    fn test_additive() {
        assert_eq!(&eval("1 + 2"), &[Constant(Ok(3))]);
        assert_eq!(&eval("2 + 3"), &[Constant(Ok(5))]);

        assert_eq!(&eval("1 - 2"), &[Constant(Ok(-1))]);
        assert_eq!(&eval("2 - 3"), &[Constant(Ok(-1))]);
        assert_eq!(&eval("6 - 2"), &[Constant(Ok(4))]);
        assert_eq!(&eval("1 - 0"), &[Constant(Ok(1))]);
    }

    #[test]
    fn test_shift() {
        assert_eq!(&eval("1 << 2"), &[Constant(Ok(4))]);
        assert_eq!(&eval("2 << 3"), &[Constant(Ok(16))]);

        assert_eq!(&eval("1 >> 2"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 >> 3"), &[Constant(Ok(0))]);
        assert_eq!(&eval("6 >> 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("1 >> 0"), &[Constant(Ok(1))]);
    }

    #[test]
    fn test_relational() {
        assert_eq!(&eval("1 < 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("2 < 1"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 < 2"), &[Constant(Ok(0))]);

        assert_eq!(&eval("1 > 2"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 > 1"), &[Constant(Ok(1))]);
        assert_eq!(&eval("2 > 2"), &[Constant(Ok(0))]);

        assert_eq!(&eval("1 <= 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("2 <= 1"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 <= 2"), &[Constant(Ok(1))]);

        assert_eq!(&eval("1 >= 2"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 >= 1"), &[Constant(Ok(1))]);
        assert_eq!(&eval("2 >= 2"), &[Constant(Ok(1))]);
    }

    #[test]
    fn test_equality() {
        assert_eq!(&eval("2 == 1"), &[Constant(Ok(0))]);
        assert_eq!(&eval("2 == 2"), &[Constant(Ok(1))]);

        assert_eq!(&eval("1 != 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("2 != 2"), &[Constant(Ok(0))]);
    }

    #[test]
    fn test_and() {
        assert_eq!(&eval("2 & 1"), &[Constant(Ok(0))]);
        assert_eq!(&eval("3 & 2"), &[Constant(Ok(2))]);
    }

    #[test]
    fn test_xor() {
        assert_eq!(&eval("2 ^ 1"), &[Constant(Ok(3))]);
        assert_eq!(&eval("3 ^ 2"), &[Constant(Ok(1))]);
    }

    #[test]
    fn test_or() {
        assert_eq!(&eval("2 | 1"), &[Constant(Ok(3))]);
        assert_eq!(&eval("3 | 2"), &[Constant(Ok(3))]);
    }

    #[test]
    fn test_logical_and() {
        assert_eq!(&eval("2 && 0"), &[Constant(Ok(0))]);
        assert_eq!(&eval("3 && 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("0 && 2"), &[Constant(Ok(0))]);
        assert_eq!(&eval("0 && 0"), &[Constant(Ok(0))]);
    }

    #[test]
    fn test_logical_or() {
        assert_eq!(&eval("2 || 0"), &[Constant(Ok(1))]);
        assert_eq!(&eval("3 || 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("0 || 2"), &[Constant(Ok(1))]);
        assert_eq!(&eval("0 || 0"), &[Constant(Ok(0))]);
    }

    #[test]
    fn test_overflow() {
        assert_eq!(&eval("1 << 60"), &[Constant(Err(()))]);
    }
}
