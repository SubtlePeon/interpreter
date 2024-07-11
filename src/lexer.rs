use crate::token::{Token, TokenType};

/// The type of lex error
#[derive(Clone, Debug)]
pub enum LexErrorType {
    /// Continue lexing. This error should normally not be returned
    ContinueLexing,
    /// Unknown character.
    UnknownCharacter(char)
}

impl std::fmt::Display for LexErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ContinueLexing =>      write!(f, "continue lexing"),
            Self::UnknownCharacter(c) => write!(f, "Unknown character: {}", c)
        }
    }
}

impl std::error::Error for LexErrorType {}

/// Contains some context about the lex error
#[derive(Clone, Debug)]
pub struct LexError {
    line: usize,
    ty: LexErrorType,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] ", self.line)?;
        self.ty.fmt(f)
    }
}

impl std::error::Error for LexError {}

// TODO: Add implementation for buffered readers for large files
// TODO: Add implementatoin for `impl Iterator<Item = char>` for preprocessing
/// The lexer. Scans through the source string and creates tokens out of them.
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    /// What line are we on?
    line: usize,
    /// Where was the beginning of the token we are lexing?
    start: usize,
    /// How far we are into the `source` string (in characters).
    current: usize,
    finished: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            line: 1,
            start: 0,
            current: 0,
            finished: false,
        }
    }

    /// Consumes the next character, incrementing `self.start` by the UTF-8
    /// byte length of that character. Returns `None` if the end of the file
    /// is reached.
    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            None => None,
            Some(c) => {
                self.current += c.len_utf8();
                Some(c)
            }
        }
    }

    fn add_token(&mut self, token_type: TokenType) -> Token<'a> {
        let start = self.start;
        self.start = self.current;

        // TODO: change for the iterator
        Token::new(
            token_type,
            &self.source[start..self.current],
            self.line,
        )
    }

    fn add_eof(&self) -> Token<'a> {
        Token::new(TokenType::Eof, "", self.line)
    }

    /// Checks if the next character is the expected character. If it is,
    /// this method consumes that character without returning it, return `true`.
    /// Otherwise, returns `false`.
    fn peek_match(&mut self, expected: char) -> bool {
        match self.chars.peek() {
            None => false,
            Some(c) if *c != expected => false,
            Some(c) => {
                self.current += c.len_utf8();
                _ = self.chars.next();
                true
            }
        }
    }

    /// Helper function to create tokens from characters
    fn accept_character(&mut self, c: char) -> Result<Token<'a>, LexErrorType> {
        use TokenType::*;

        Ok(match c {
            // Single characters
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),

            // Two character tokens
            '!' if self.peek_match('=') => self.add_token(BangEqual),
            '!' => self.add_token(Bang),
            '=' if self.peek_match('=') => self.add_token(EqualEqual),
            '=' => self.add_token(Equal),
            '<' if self.peek_match('=') => self.add_token(LessEqual),
            '<' => self.add_token(Less),
            '>' if self.peek_match('=') => self.add_token(GreaterEqual),
            '>' => self.add_token(Greater),

            // Comments continue until end of newline
            '/' if self.peek_match('/') => loop {
                match self.chars.peek() {
                    None => break self.add_eof(),
                    Some('\n') => return Err(LexErrorType::ContinueLexing),
                    Some(_) => {}
                }
            }
            '/' => self.add_token(Slash),

            '\n' => {
                self.line += 1;
                return Err(LexErrorType::ContinueLexing);
            }
            // Ignore whitespace for now
            ws if ws.is_whitespace() => return Err(LexErrorType::ContinueLexing),

            // TODO: Pass the error
            c => {
                eprintln!("[line {}] Error: Unexpected character: {}", self.line, c);
                return Err(LexErrorType::ContinueLexing);
            },
        })
    }

    pub fn scan_token(&mut self) -> Result<Token<'a>, LexError> {
        loop {
            let tok = match self.advance() {
                None => {
                    self.finished = true;
                    self.add_eof()
                }
                Some(c) => match self.accept_character(c) {
                    Ok(token) => token,
                    Err(LexErrorType::ContinueLexing) => continue,
                    Err(err) => break Err(LexError {
                        line: self.line,
                        ty: err,
                    }),
                }
            };

            break Ok(tok)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            None
        } else {
            match self.scan_token() {
                Ok(x) => Some(Ok(x)),
                Err(err) => Some(Err(err))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::TokenType;

    use super::Lexer;

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
        assert_eq!(lexed, vec![
            TokenType::Comma,
            TokenType::Dot,
            TokenType::Star,
            TokenType::Semicolon,
            TokenType::GreaterEqual,
            TokenType::LessEqual,
            TokenType::Eof,
        ]);
    }

    #[test]
    fn some_symbols_work_literal() {
        let input_str = ",.*;>=<=";
        let lexed: Vec<_> = Lexer::new(input_str)
            .map(|t| t.unwrap().source().to_owned())
            .collect();
        assert_eq!(lexed, vec![
            ",",
            ".",
            "*",
            ";",
            ">=",
            "<=",
            "",
        ]);
    }
}
