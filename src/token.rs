#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single character tokens
    /// The character '('
    LeftParen,
    /// The character ')'
    RightParen,
    /// The character '{'
    LeftBrace,
    /// The character '}'
    RightBrace,
    /// The character ','
    Comma,
    /// The character '.'
    Dot,
    /// The character '-'
    Minus,
    /// The character '+'
    Plus,
    /// The character ';'
    Semicolon,
    /// The character '/'
    Slash,
    /// The character '*'
    Star,

    // One or two-character tokens
    /// The character '!'
    Bang,
    /// The characters '!='
    BangEqual,
    /// The character '='
    Equal,
    /// The characters '=='
    EqualEqual,
    /// The character '>'
    Greater,
    /// The characters '>='
    GreaterEqual,
    /// The character '<'
    Less,
    /// The characters '<='
    LessEqual,

    // Literals
    /// An identifier (a word like 'this_is_a_variable')
    Ident,
    /// An integer or floating-point number
    Number,
    /// A string enclosed in double quotes ('"')
    String,

    /// End of file
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    ty: TokenType,
    source: &'a str,
    /// The line number
    line: usize,
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, source: &'a str, line: usize) -> Self {
        Self { ty, source, line }
    }

    pub fn token_type(&self) -> TokenType {
        self.ty
    }

    pub fn source(&'a self) -> &'a str {
        self.source
    }
}
