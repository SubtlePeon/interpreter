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

// CodeCrafters needs the names of the token types to be in
// `SCREAMING_SNAKE_CASE`, so this is the function to convert their
// names.
impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::LeftParen    => "LEFT_PAREN",
            Self::RightParen   => "RIGHT_PAREN",
            Self::LeftBrace    => "LEFT_BRACE",
            Self::RightBrace   => "RIGHT_BRACE",
            Self::Comma        => "COMMA",
            Self::Dot          => "DOT",
            Self::Minus        => "MINUS",
            Self::Plus         => "PLUS",
            Self::Semicolon    => "SEMICOLON",
            Self::Slash        => "SLASH",
            Self::Star         => "STAR",
            Self::Bang         => "BANG",
            Self::BangEqual    => "BANG_EQUAL",
            Self::Equal        => "EQUAL",
            Self::EqualEqual   => "EQUAL_EQUAL",
            Self::Greater      => "GREATER",
            Self::GreaterEqual => "GREATER_EQUAL",
            Self::Less         => "LESS",
            Self::LessEqual    => "LESS_EQUAL",
            Self::Ident        => "IDENT",
            Self::Number       => "NUMBER",
            Self::String       => "STRING",
            Self::Eof          => "EOF",
        };

        write!(f, "{}", name)
    }
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

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.ty, self.source, "null")
    }
}
