use crate::{
    span::Span,
    token::{Token, TokenType},
};

/// The type of lex error
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LexErrorType {
    /// Unknown character.
    Unknown(char),
    /// Unterminated string. Contains the byte index of the first quote.
    UnterminatedString(usize),
}

impl std::fmt::Display for LexErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown(c) => write!(f, "Unexpected character: {}", c),
            Self::UnterminatedString(_) => write!(f, "Unterminated string."),
        }
    }
}

impl std::error::Error for LexErrorType {}

/// Contains some context about the lex error
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LexError<'a> {
    span: Span<'a>,
    ty: LexErrorType,
}

impl std::fmt::Display for LexError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error: ", self.span.line)?;
        self.ty.fmt(f)
    }
}

impl std::error::Error for LexError<'_> {}

impl LexError<'_> {
    pub fn error_type(&self) -> LexErrorType {
        self.ty
    }
}

// TODO: should likely use `char_indices` instead
// TODO: should make `Cursor` generic over necessary iterators
/// The iterator over the characters in the source text. `Cursor` helps with peeking
/// and making sure the byte indices always lie on the boundary of characters.
#[derive(Clone, Debug)]
pub struct Cursor<'a> {
    source: &'a str,
    chars: std::str::Chars<'a>,
    index: usize,
}

impl<'a> Cursor<'a> {
    /// Create a new `Cursor`.
    pub fn new(text: &'a str) -> Self {
        Self {
            source: text,
            chars: text.chars(),
            index: 0,
        }
    }

    /// The byte index of the current position into the source text.
    pub fn index(&self) -> usize {
        self.index
    }

    /// The source text.
    pub fn source(&self) -> &'a str {
        self.source
    }

    /// The first character that would be returned next.
    pub fn first(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// The first character that would be returned next.
    ///
    /// The same thing as [`Self::first`].
    pub fn peek(&self) -> Option<char> {
        self.first()
    }

    /// The second character that would be returned next.
    pub fn second(&self) -> Option<char> {
        self.chars.clone().nth(1)
    }
}

impl Iterator for Cursor<'_> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        self.index = self
            .index
            .checked_add(c.len_utf8())
            .expect("File not too big");
        Some(c)
    }
}

// TODO: Add implementation for buffered readers for large files?
// TODO: Add implementation for `impl Iterator<Item = char>` for preprocessing?
/// The lexer. Scans through the source string and creates tokens out of them.
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    /// What line are we on?
    line: usize,
    /// Where was the beginning of the token we are lexing?
    start: usize,
    /// Have we finished lexing?
    finished: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer`.
    pub fn new(source: &'a str) -> Self {
        Self {
            cursor: Cursor::new(source),
            line: 1,
            start: 0,
            finished: false,
        }
    }

    /// Creates a new token from `self.start` to `self.cursor.index`.
    ///
    /// Resets `self.start`
    fn new_token(&mut self, token_type: TokenType) -> Token<'a> {
        let start = self.start;
        self.start = self.cursor.index();

        // Using `Cursor` should ensure that the indices are valid
        Token::new(
            token_type,
            self.cursor.source(),
            start,
            self.cursor.index(),
            self.line,
        ).expect("Lexer has valid indices for Token")
    }

    /// Creates a error from `self.start` to `self.cursor.index`.
    ///
    /// Resets `self.start`
    fn new_error(&mut self, err: LexErrorType) -> LexError<'a> {
        // Using `Cursor` should ensure that the indices are valid
        let span = Span::new(
            self.line,
            self.cursor.source(),
            self.start,
            self.cursor.index(),
        ).expect("Lexer has valid indices for Token");
        // The next token should be clean.
        self.start = self.cursor.index();

        LexError { span, ty: err }
    }

    /// Consumes characters until we hit a newline character.
    fn eat_until_newline(&mut self) {
        while let Some(c) = self.cursor.first() {
            if c == '\n' {
                break;
            }
            self.cursor.next();
        }
        // Reset start
        self.start = self.cursor.index();
    }

    /// Consumes until and including the next double quote ("). Returns a
    /// string token containing the unescaped text inside the double quotes.
    fn eat_string(&mut self) -> Result<Token<'a>, LexErrorType> {
        // Don't take the starting '"'
        self.start = self.cursor.index();

        while let Some(c) = self.cursor.first() {
            if c == '"' {
                break;
            } else if c == '\n' {
                self.line += 1;
            }
            self.cursor.next();
        }

        let tok = self.new_token(TokenType::String);

        // The next char can only be the end of file or '"'. If it is
        // the former, error; if it is the latter, consume it.
        if let None = self.cursor.next() {
            return Err(LexErrorType::UnterminatedString(self.start));
        }
        self.start = self.cursor.index();

        Ok(tok)
    }

    /// Eats a number
    fn eat_number(&mut self) -> Token<'a> {
        // Basic number parsing
        let mut after_dot = false;
        while let Some(c) = self.cursor.first() {
            if c.is_ascii_digit() {
                self.cursor.next();
            } else if c == '.' {
                // The dot should not be a part of the number literal if:
                // - This is the second dot we encountered
                // - The character after the dot is not a digit ()
                if after_dot {
                    break;
                }
                match self.cursor.second() {
                    None => break,
                    Some(c) if c.is_ascii_digit() => {
                        _ = self.cursor.next();
                        after_dot = true;
                    }
                    Some(_) => break,
                }
            } else {
                break;
            }
        }

        self.new_token(TokenType::Number)
    }

    /// Consume an identifier or keyword
    fn eat_identifier(&mut self) -> Token<'a> {
        while let Some(c) = self.cursor.first() {
            if c.is_alphanumeric() || c == '_' {
                self.cursor.next();
            } else {
                break;
            }
        }

        self.new_token(TokenType::Ident)
    }

    /// Get the next token. If at the end of the source text, this function will keep
    /// returning an `EOF` token.
    pub fn scan_token(&mut self) -> Result<Token<'a>, LexError<'a>> {
        use TokenType::*;

        while let Some(c) = self.cursor.next() {
            let token = match c {
                '(' => self.new_token(OpenParen),
                ')' => self.new_token(CloseParen),
                '{' => self.new_token(OpenBrace),
                '}' => self.new_token(CloseBrace),
                ',' => self.new_token(Comma),
                '.' => self.new_token(Dot),
                '-' => self.new_token(Minus),
                '+' => self.new_token(Plus),
                ';' => self.new_token(Semicolon),
                '*' => self.new_token(Star),
                '!' => {
                    if self.cursor.first() == Some('=') {
                        self.cursor.next();
                        self.new_token(BangEq)
                    } else {
                        self.new_token(Bang)
                    }
                }
                '=' => {
                    if self.cursor.first() == Some('=') {
                        self.cursor.next();
                        self.new_token(EqEq)
                    } else {
                        self.new_token(Eq)
                    }
                }
                '<' => {
                    if self.cursor.first() == Some('=') {
                        self.cursor.next();
                        self.new_token(Le)
                    } else {
                        self.new_token(Lt)
                    }
                }
                '>' => {
                    if self.cursor.first() == Some('=') {
                        self.cursor.next();
                        self.new_token(Ge)
                    } else {
                        self.new_token(Gt)
                    }
                }
                '/' => {
                    if self.cursor.first() == Some('/') {
                        // Comment continues until newline
                        self.eat_until_newline();
                        continue;
                    } else {
                        self.new_token(Slash)
                    }
                }
                '"' => return self.eat_string().map_err(|err| self.new_error(err)),
                c if c.is_ascii_digit() => self.eat_number(),
                '\n' => {
                    self.line += 1;
                    self.start = self.cursor.index();
                    continue;
                }

                // Ignore whitespace
                ws if ws.is_ascii_whitespace() => {
                    self.start = self.cursor.index();
                    continue
                }

                '_' => self.eat_identifier(),
                // This allows many (maybe too many) characters as identifiers.
                // The Rust language uses Unicode Standard Annex #31 (Xid_Start + '_'
                // and Xid_Continue).
                c if c.is_alphabetic() => self.eat_identifier(),

                c => return Err(self.new_error(LexErrorType::Unknown(c))),
            };

            return Ok(token);
        }

        self.finished = true;
        Ok(self.new_token(TokenType::Eof))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            None
        } else {
            match self.scan_token() {
                Ok(x) => Some(Ok(x)),
                Err(err) => Some(Err(err)),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::LexErrorType, token::TokenType};

    use super::{LexError, Lexer};

    #[test]
    fn empty_file_works() {
        let lexed: Vec<_> = Lexer::new("").map(|t| t.unwrap().token_type()).collect();
        assert_eq!(lexed, vec![TokenType::Eof]);
    }

    #[test]
    fn some_symbols_work() {
        let input_str = ",.*;>=<=";
        let lexed: Vec<_> = Lexer::new(input_str)
            .map(|t| t.unwrap().token_type())
            .collect();
        println!("{:?}", lexed);
        assert_eq!(
            lexed,
            vec![
                TokenType::Comma,
                TokenType::Dot,
                TokenType::Star,
                TokenType::Semicolon,
                TokenType::Ge,
                TokenType::Le,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn some_symbols_work_literal() {
        let input_str = ",.*;>=<=";
        let lexed: Vec<_> = Lexer::new(input_str)
            .map(|t| t.unwrap().source())
            .collect();
        assert_eq!(lexed, vec![",", ".", "*", ";", ">=", "<=", ""]);
    }

    #[test]
    fn comments_work() {
        let input_str = r"
            // Hello
            !=
            // Hi
            ;)
            ";
        let lexer = Lexer::new(input_str);
        let lexed: Result<Vec<_>, LexError> = lexer
            .map(|r| r.map(|t| t.source()))
            .collect();
        assert_eq!(lexed, Ok(vec!["!=", ";", ")", ""]));
    }

    #[test]
    fn strings_work() {
        let input_str = "\"This is a string\"";
        let lexed: Result<Vec<_>, LexError> = Lexer::new(input_str)
            .map(|r| r.map(|t| t.source()))
            .collect();
        assert_eq!(lexed, Ok(vec!["This is a string", ""]));
    }

    #[test]
    fn unterminated_string() {
        let input_str = "\"This string never ends!";
        let lexed: Result<Vec<_>, LexError> = Lexer::new(input_str)
            .map(|r| r.map(|t| t.source()))
            .collect();
        assert!(
            matches!(
                lexed.map_err(|err| err.error_type()),
                Err(LexErrorType::UnterminatedString(_)),
            )
        );
    }

    #[test]
    fn numbers_work() {
        let input_str = "1234.1234 123.0 10985 1.";
        let lexed: Result<Vec<_>, LexError> = Lexer::new(input_str)
            .map(|r| r.map(|t| t.source()))
            .collect();
        assert_eq!(lexed, Ok(vec!["1234.1234", "123.0", "10985", "1", ".", ""]));
    }

    #[test]
    fn idents_work() {
        use crate::token::Keyword;

        let input_str = "hello and me, if we sing!";
        let lexed: Result<Vec<_>, LexError> = Lexer::new(input_str)
            .map(|r| r.map(|t| t.source()))
            .collect();
        assert_eq!(lexed, Ok(vec!["hello", "and", "me", ",", "if", "we", "sing", "!", ""]));
        let lexed: Result<Vec<_>, LexError> = Lexer::new(input_str)
            .map(|r| r.map(|t| t.token_type()))
            .collect();
        assert_eq!(lexed, Ok(vec![
            TokenType::Ident,
            TokenType::Keyword(Keyword::And),
            TokenType::Ident,
            TokenType::Comma,
            TokenType::Keyword(Keyword::If),
            TokenType::Ident,
            TokenType::Ident,
            TokenType::Bang,
            TokenType::Eof,
        ]));
    }
}
