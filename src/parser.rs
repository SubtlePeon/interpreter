use crate::ast::{Expr, Literal};
use crate::lexer::LexResult;
use crate::token::{Token, TokenType};

use std::fmt;

#[derive(Clone, Debug)]
pub enum ParseError {
    SyntaxError { token: Token },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SyntaxError { token } => match token.token_type() {
                TokenType::Eof => write!(f, "at end of file"),
                token_ty => write!(f, "got {}", token_ty.conv_case()),
            },
        }
    }
}

pub type ParserError = ();

#[derive(Clone, Debug)]
pub struct Parser<'src, I> {
    lexer: I,
    src: &'src str,
    curr: Option<Token>,
    next: Option<Token>,
}

impl <'src> Parser<'src, crate::lexer::Lexer<'src>> {
    pub fn from_lexer(lexer: crate::lexer::Lexer<'src>) -> Self {
        let src = lexer.source();

        let mut res = Self {
            lexer,
            src,
            curr: None,
            next: None,
        };

        res.advance();
        res.advance();
        res
    }
}

impl<'src, I> Parser<'src, I>
where
    I: Iterator<Item = LexResult> + 'src,
{
    /// Create a new Parser from a Lexer.
    pub fn new(lexer: I, src: &'src str) -> Self {
        let mut res = Self {
            lexer,
            src,
            curr: None,
            next: None,
        };

        res.advance();
        res.advance();
        res
    }

    /// Advance the state of the parser.
    fn advance(&mut self) {
        self.curr = self.next.clone();
        self.next = match self.lexer.next() {
            Some(Err(_err)) => todo!(),
            None => None,
            Some(Ok(tok)) => Some(tok),
        };
    }

    /// Report a parsing error.
    fn _error(&mut self, _msg: &str) {
        // TODO
    }

    /// Parse the entire program. This is usually what you want.
    pub fn parse_program(&mut self) -> Result<Expr, Vec<()>> {
        let expr = self.expect_expr().map_err(|err| vec![err])?;
        Ok(expr)
    }

    /// Parse a single expression. Definitions TBD.
    pub fn expect_expr(&mut self) -> Result<Expr, ()> {
        let lit = self.expect_literal()?;
        Ok(Expr::Literal(lit))
    }

    /// Expects the next token to be a literal. If the next token is a literal, the
    /// parser will be advanced.
    pub fn expect_literal(&mut self) -> Result<Literal, ()> {
        let Some(curr) = self.curr.as_ref() else {
            return Err(());
        };
        let Ok(lit) = Literal::from_token(curr, self.src) else {
            return Err(())
        };
        self.advance();
        Ok(lit)
    }
}

#[cfg(test)]
mod test {
    use crate::{ast::{Expr, LitKind}, lexer::Lexer};
    use super::Parser;

    #[test]
    fn bool() {
        let input = "false";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer, input);

        let expr = parser.parse_program().unwrap();
        let Expr::Literal(lit) = expr else {
            panic!("`expr` was not a `Literal`, it was: {:?}", expr);
        };

        assert_eq!(*lit.kind(), LitKind::Boolean(false));
        assert_eq!(format!("{}", lit), "false".to_owned());
    }
}
