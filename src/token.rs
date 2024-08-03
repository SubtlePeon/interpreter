use std::fmt;
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

pub struct KeywordError {
    src: String,
}

impl KeywordError {
    pub fn new(src: String) -> Self {
        Self { src }
    }
}

impl fmt::Display for KeywordError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Not a keyword: '{}'", self.src)
    }
}

impl FromStr for Keyword {
    type Err = KeywordError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "and" => Self::And,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::False,
            "for" => Self::For,
            "fun" => Self::Fun,
            "if" => Self::If,
            "nil" => Self::Nil,
            "or" => Self::Or,
            "print" => Self::Print,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::True,
            "var" => Self::Var,
            "while" => Self::While,
            _ => return Err(KeywordError::new(s.to_owned())),
        })
    }
}

impl Keyword {
    pub fn conv_case(&self) -> String {
        format!("{:?}", self).to_ascii_uppercase()
    }

    pub fn expected_src(&self) -> String {
        format!("{:?}", self).to_ascii_lowercase()
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
    pub fn conv_case(&self) -> String {
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
            Self::Ident => "IDENTIFIER",
            Self::Keyword(k) => return k.conv_case(),
            Self::Number => "NUMBER",
            Self::String => "STRING",
            Self::Eof => "EOF",
        }
        .to_owned()
    }

    /// The expected representation of the token type.
    pub fn repr(&self) -> Option<String> {
        Some(
            match self {
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
                Self::Keyword(k) => return Some(k.expected_src()),
                _ => return None,
            }
            .to_owned(),
        )
    }

    pub fn from_ident(src: &str) -> Self {
        match Keyword::from_str(src) {
            Ok(k) => Self::Keyword(k),
            Err(_) => Self::Ident,
        }
    }
}

/// A token, which forms the output of the lexer.
#[derive(Clone, Debug)]
pub struct Token {
    ty: TokenType,
    span: Span,
}

impl Token {
    /// Create a new `Token`. Returns `None` if the indices `lo` and `hi` are not valid
    /// byte indices (at character boundaries) for `source_text`.
    pub fn new_checked(mut token_type: TokenType, source_text: &str, span: Span) -> Option<Self> {
        let src = span.source(source_text)?;

        if let Some(exp) = token_type.repr() {
            debug_assert_eq!(
                src, exp,
                "The token type of {:?} should have a src of '{}'",
                token_type, exp,
            );
        }

        if matches!(token_type, TokenType::Ident) {
            token_type = TokenType::from_ident(src)
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
    pub fn source<'a>(&self, src_text: &'a str) -> &'a str {
        self.span.source_unchecked(src_text)
    }

    /// Get the span of this token.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Get a representative string for this token. This is the underlying
    /// source text that identifies this token (excpet for strings, which
    /// do not have the double quotes in the underlying representation).
    pub fn repr(&self, src_text: &str) -> String {
        match self.ty {
            TokenType::String => format!("\"{}\"", self.source(src_text)),
            _ => self.source(src_text).to_owned(),
        }
    }

    /// Get a display string for this token.
    pub fn display(&self, src_text: &str) -> String {
        match self.ty {
            TokenType::String => self.source(src_text).to_owned(),
            // For some reason, to satisfy CodeCrafters, this needs to always be a
            // floating point
            TokenType::Number => {
                let num: f64 = self.source(src_text).parse().expect("Valid num string");
                if num == num.round() {
                    format!("{}.0", num).into()
                } else {
                    num.to_string().into()
                }
            }
            _ => "null".into(),
        }
    }

    /// Display format as requested by CodeCrafters
    pub fn conv(&self, src_text: &str) -> String {
        format!(
            "{} {} {}",
            self.ty.conv_case(),
            self.repr(src_text),
            self.display(src_text)
        )
    }
}
