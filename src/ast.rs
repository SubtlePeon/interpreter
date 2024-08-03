use crate::span::Span;
use crate::token::{Keyword, Token, TokenType};

use std::fmt;

/// An error due to trying to parse a [`Token`] into an [`Expr`].
///
/// TODO: might try to make the errors more context-aware.
#[derive(Clone, Copy, Debug)]
pub enum ParseExprError {
    /// The [`TokenType`] of the token was incorrect.
    ///
    /// For example, parsing a literal expects a number, string, or the keywords true
    /// or false. Passing in a token that is not one of those will result in this error.
    BadTokenType,
    /// The source text referred to by the [`Span`] of the token could not be parsed
    /// into the necessary type.
    BadTokenSpan,
}

impl fmt::Display for ParseExprError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadTokenType => write!(f, "Incorrect token type provided"),
            Self::BadTokenSpan => write!(
                f,
                "Could not parse text referred to by token span into the correct type",
            ),
        }
    }
}

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr {
    BinExpr(BinExpr),
    Literal(Literal),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(lit) => write!(f, "{}", lit),
            Self::BinExpr(bin_expr) => write!(f, "{}", bin_expr),
        }
    }
}

/// Binary operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    /// Addition with `+`.
    Add,
    /// Subtraction with `-`.
    Minus,
    /// Multiplication with `*`.
    Star,
    /// Division with `/`.
    Slash,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
        }
    }
}

/// A binary expresion.
#[derive(Clone, Debug)]
pub struct BinExpr {
    left: Box<Expr>,
    right: Box<Expr>,
    op: BinOp,
}

impl fmt::Display for BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.op, self.left, self.right)
    }
}

/// Unary operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    /// Negation with `-`.
    Minus,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LitKind {
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
    kind: LitKind,
    span: Span,
}

impl Literal {
    /// Create a literal from a token. If the token provided cannot be made into a
    /// literal, returns `None`. These token types are literals:
    /// - [`Number`][crate::token::TokenType::Number]
    /// - [`String`][crate::token::TokenType::String]
    /// - the [`Keyword`][crate::token::TokenType::Keyword]s:
    ///   - [`True`][crate::token::Keyword::True] and
    ///   - [`False`][crate::token::Keyword::False]
    pub fn from_token(token: &Token, src_text: &str) -> Result<Self, ParseExprError> {
        let kind = match (token.token_type(), token.source(src_text)) {
            (TokenType::Number, src) => {
                let num =
                    src.parse().or_else(|_| Err(ParseExprError::BadTokenSpan))?;
                LitKind::Number(num)
            }
            (TokenType::String, src) => LitKind::String(src.to_owned()),
            (TokenType::Keyword(Keyword::True), _) => LitKind::Boolean(true),
            (TokenType::Keyword(Keyword::False), _) => LitKind::Boolean(false),
            _ => return Err(ParseExprError::BadTokenType),
        };

        Ok(Self {
            kind,
            span: token.span().clone(),
        })
    }

    pub fn kind(&self) -> &LitKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            LitKind::Boolean(b) => write!(f, "{}", b),
            LitKind::Number(num) => write!(f, "{}", num),
            LitKind::String(s) => write!(f, "{}", s),
        }
    }
}
