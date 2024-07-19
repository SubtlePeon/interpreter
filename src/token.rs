use std::borrow::Cow;
use std::str::FromStr;

use crate::span::Span;

/// Keywords from Robert Nystrom's Crafting Interpretes
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl FromStr for Keyword {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "and" =>    Self::And,
            "class" =>  Self::Class,
            "else" =>   Self::Else,
            "false" =>  Self::False,
            "for" =>    Self::For,
            "fun" =>    Self::Fun,
            "if" =>     Self::If,
            "nil" =>    Self::Nil,
            "or" =>     Self::Or,
            "print" =>  Self::Print,
            "return" => Self::Return,
            "super" =>  Self::Super,
            "this" =>   Self::This,
            "true" =>   Self::True,
            "var" =>    Self::Var,
            "while" =>  Self::While,
            _ => return Err(()),
        })
    }
}

impl Keyword {
    pub fn conv_case(&self) -> &'static str {
        match self {
            Self::And => "AND",
            Self::Class => "CLASS",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::For => "FOR",
            Self::Fun => "FUN",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Print => "PRINT",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Var => "VAR",
            Self::While => "WHILE",
        }
    }

    pub fn expected_src(&self) -> &'static str {
        match self {
            Self::And => "and",
            Self::Class => "class",
            Self::Else => "else",
            Self::False => "false",
            Self::For => "for",
            Self::Fun => "fun",
            Self::If => "if",
            Self::Nil => "nil",
            Self::Or => "or",
            Self::Print => "print",
            Self::Return => "return",
            Self::Super => "super",
            Self::This => "this",
            Self::True => "true",
            Self::Var => "var",
            Self::While => "while",
        }
    }
}

/// The type of token, usually a representative of the source code symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// Opening (left) parenthesis.
    OpenParen,
    /// Closing (right) parenthesis.
    CloseParen,
    /// Opening (left) bracket.
    OpenBracket,
    /// Closing (right) bracket.
    CloseBracket,
    /// Opening (left) brace.
    OpenBrace,
    /// Closing (right) brace.
    CloseBrace,
    /// ','
    Comma,
    /// '.'
    Dot,
    /// '-'
    Minus,
    /// '+'
    Plus,
    /// ';'
    Semicolon,
    /// '/'
    Slash,
    /// '*'
    Star,
    /// '!'
    Bang,
    /// '!='
    BangEq,
    /// '='
    Eq,
    /// '=='
    EqEq,
    /// '>'
    Gt,
    /// '>='
    Ge,
    /// '<'
    Lt,
    /// '<='
    Le,
    /// An identifier (a word like 'this_is_a_variable').
    Ident,
    /// A reserved identifier.
    Keyword(Keyword),
    /// An integer or floating-point number.
    Number,
    /// A string enclosed in double quotes ('"').
    String,
    /// End of file.
    Eof,
}

impl TokenType {
    /// CodeCrafters needs the names of the token types to be in
    /// `SCREAMING_SNAKE_CASE`, so this is the function to convert their
    /// names.
    pub fn conv_case(&self) -> &'static str {
        match self {
            Self::OpenParen => "LEFT_PAREN",
            Self::CloseParen => "RIGHT_PAREN",
            Self::OpenBracket => "LEFT_BRACKET",
            Self::CloseBracket => "RIGHT_BRACKET",
            Self::OpenBrace => "LEFT_BRACE",
            Self::CloseBrace => "RIGHT_BRACE",
            Self::Comma => "COMMA",
            Self::Dot => "DOT",
            Self::Minus => "MINUS",
            Self::Plus => "PLUS",
            Self::Semicolon => "SEMICOLON",
            Self::Slash => "SLASH",
            Self::Star => "STAR",
            Self::Bang => "BANG",
            Self::BangEq => "BANG_EQUAL",
            Self::Eq => "EQUAL",
            Self::EqEq => "EQUAL_EQUAL",
            Self::Gt => "GREATER",
            Self::Ge => "GREATER_EQUAL",
            Self::Lt => "LESS",
            Self::Le => "LESS_EQUAL",
            Self::Ident => "IDENT",
            Self::Keyword(k) => k.conv_case(),
            Self::Number => "NUMBER",
            Self::String => "STRING",
            Self::Eof => "EOF",
        }
    }

    /// The expected representation of the token type.
    pub fn expected_src(&self) -> Option<&'static str> {
        Some(match self {
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::OpenBrace => "{",
            Self::CloseBrace => "}",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::Minus => "-",
            Self::Plus => "+",
            Self::Semicolon => ";",
            Self::Slash => "/",
            Self::Star => "*",
            Self::Bang => "!",
            Self::BangEq => "!=",
            Self::Eq => "=",
            Self::EqEq => "==",
            Self::Gt => ">",
            Self::Ge => ">=",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Keyword(k) => k.expected_src(),
            _ => return None,
        })
    }

    pub fn from_ident(
        span: &Span,
    ) -> Self {
        match Keyword::from_str(span.src) {
            Ok(k) => Self::Keyword(k),
            Err(_) => Self::Ident,
        }
    }
}

/// A token, which forms the output of the lexer.
#[derive(Clone, Debug)]
pub struct Token<'a> {
    ty: TokenType,
    span: Span<'a>,
}

impl<'a> Token<'a> {
    /// Create a new `Token`. Returns `None` if the indices `lo` and `hi` are not valid 
    /// byte indices (at character boundaries) for `source_text`.
    pub fn new(
        mut token_type: TokenType,
        source_text: &'a str,
        lo: usize,
        hi: usize,
        line: usize,
    ) -> Option<Self> {
        let src = source_text.get(lo..hi)?;

        if let Some(exp) = token_type.expected_src() {
            debug_assert_eq!(
                src, exp,
                "The token type of {:?} should have a src of '{}'",
                token_type, exp,
            );
        }

        let span = Span::with_src(line, src, lo, hi);

        if matches!(token_type, TokenType::Ident) {
            token_type = TokenType::from_ident(&span)
        }

        Some(Self {
            ty: token_type,
            span,
        })
    }

    /// Get the token type.
    pub fn token_type(&self) -> TokenType {
        self.ty
    }

    /// Get the source code snippet.
    pub fn source(&self) -> &'a str {
        self.span.src
    }

    /// Get the span of this token.
    pub fn span(&self) -> &Span<'a> {
        &self.span
    }

    /// Get a representative string for this token.
    pub fn repr(&self) -> Cow<str> {
        match self.ty {
            TokenType::String => format!("\"{}\"", self.span.src).into(),
            _ => self.span.src.into(),
        }
    }

    /// Get a display string for this token.
    pub fn display(&self) -> Cow<str> {
        match self.ty {
            TokenType::String => self.span.src.into(),
            // For some reason, to satisfy CodeCrafters, this needs to always be a
            // floating point
            TokenType::Number => {
                if self.span.src.contains('.') {
                    return self.span.src.into();
                } else {
                    return format!("{}.0", self.span.src).into();
                }
            },
            _ => "null".into(),
        }
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.ty.conv_case(), self.repr(), self.display())
    }
}
