use crate::{
    span::Span,
    token::{Token, TokenType},
};

/// The type of lex error
#[derive(Clone, Debug)]
pub enum LexErrorType {
    /// Unknown character.
    Unknown(char),
}

impl std::fmt::Display for LexErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown(c) => write!(f, "Unexpected character: {}", c),
        }
    }
}

impl std::error::Error for LexErrorType {}

/// Contains some context about the lex error
#[derive(Clone, Debug)]
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

        Token::new(
            token_type,
            self.cursor.source(),
            start,
            self.cursor.index(),
            self.line,
        )
    }

    /// Creates a error from `self.start` to `self.cursor.index`.
    ///
    /// Resets `self.start`
    fn new_error(&mut self, err: LexErrorType) -> LexError<'a> {
        let span = Span::new(
            self.line,
            self.cursor.source(),
            self.start,
            self.cursor.index(),
        );
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

    /// Helper function to create tokens from the next character.
    fn eat_char(&mut self, c: char) -> Option<Result<Token<'a>, LexErrorType>> {
        use crate::token::Delim::*;
        use TokenType::*;

        Some(Ok(match c {
            '(' => self.new_token(OpenDelim(Paren)),
            ')' => self.new_token(CloseDelim(Paren)),
            '{' => self.new_token(OpenDelim(Brace)),
            '}' => self.new_token(CloseDelim(Brace)),
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
                    return None;
                } else {
                    self.new_token(Slash)
                }
            }
            '\n' => {
                self.line += 1;
                self.start = self.cursor.index();
                return None;
            }
            // Ignore whitespace for now
            ws if ws.is_ascii_whitespace() => {
                self.start = self.cursor.index();
                return None;
            }

            c => return Some(Err(LexErrorType::Unknown(c))),
        }))
    }

    pub fn scan_token(&mut self) -> Result<Token<'a>, LexError<'a>> {
        loop {
            break match self.cursor.next() {
                None => {
                    self.finished = true;
                    Ok(self.new_token(TokenType::Eof))
                }
                Some(c) => {
                    break match self.eat_char(c) {
                        Some(Ok(token)) => Ok(token),
                        Some(Err(err)) => Err(self.new_error(err)),
                        None => continue,
                    }
                }
            };
        }
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
    use crate::token::TokenType;

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
            .map(|t| t.unwrap().source().to_owned())
            .collect();
        assert_eq!(lexed, vec![",", ".", "*", ";", ">=", "<=", "",]);
    }

    #[test]
    fn comments_work() {
        let input_str = r"
            // Hello
            !=
            // Hi
            ;)
            ";
        let lexed: Result<Vec<_>, LexError> = Lexer::new(input_str)
            .map(|r| r.map(|t| t.source().to_owned()))
            .collect();
        assert_eq!(lexed.unwrap(), vec!["!=", ";", ")", "",]);
    }
}
