use crate::span::Span;

/// The type of open/closing delimiter.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Delim {
    /// `(...)`
    Paren,
    /// `[...]`
    Bracket,
    /// `{...}`
    Brace,
}

/// The type of token, usually a representative of the source code symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// Opening (usually left) delimiter.
    OpenDelim(Delim),
    /// Closing (usually right) delimiter.
    CloseDelim(Delim),
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
            Self::OpenDelim(Delim::Paren) => "LEFT_PAREN",
            Self::CloseDelim(Delim::Paren) => "RIGHT_PAREN",
            Self::OpenDelim(Delim::Bracket) => "LEFT_BRACKET",
            Self::CloseDelim(Delim::Bracket) => "RIGHT_BRACKET",
            Self::OpenDelim(Delim::Brace) => "LEFT_BRACE",
            Self::CloseDelim(Delim::Brace) => "RIGHT_BRACE",
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
            Self::Number => "NUMBER",
            Self::String => "STRING",
            Self::Eof => "EOF",
        }
    }

    /// The expected representation of the token type.
    pub fn expected_src(&self) -> Option<&'static str> {
        Some(match self {
            Self::OpenDelim(Delim::Paren) => "(",
            Self::CloseDelim(Delim::Paren) => ")",
            Self::OpenDelim(Delim::Bracket) => "[",
            Self::CloseDelim(Delim::Bracket) => "]",
            Self::OpenDelim(Delim::Brace) => "{",
            Self::CloseDelim(Delim::Brace) => "}",
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
            _ => return None,
        })
    }
}


/// A token, which forms the output of the lexer.
#[derive(Clone, Debug)]
pub struct Token<'a> {
    ty: TokenType,
    span: Span<'a>,
}

impl<'a> Token<'a> {
    pub fn new(
        token_type: TokenType,
        source_text: &'a str,
        lo: usize,
        hi: usize,
        line: usize,
    ) -> Self {
        let src = &source_text[lo..hi];

        // if let Some(exp) = token_type.expected_src() {
        //     debug_assert_eq!(src, exp);
        // }

        Self {
            ty: token_type,
            span: Span {
                line,
                src,
                lo,
                hi,
            },
        }
    }

    pub fn token_type(&self) -> TokenType {
        self.ty
    }

    pub fn source(&'a self) -> &'a str {
        self.span.src
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.ty.conv_case(), self.span.src, "null")
    }
}
