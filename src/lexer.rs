use crate::token::{Token, TokenType};

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
    fn accept_character(&mut self, c: char) -> Option<Token<'a>> {
        use TokenType::*;

        Some(match c {
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
                    Some('\n') => return None,
                    Some(_) => {}
                }
            }
            '/' => self.add_token(Slash),

            // Ignore whitespace for now
            ws if ws.is_whitespace() => return None,

            // TODO: Pass the error
            c => todo!("Unexpected character: '{}'", c),
        })
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        loop {
            let tok = match self.advance() {
                None => {
                    self.finished = true;
                    self.add_eof()
                }
                Some(c) => match self.accept_character(c) {
                    None => continue,
                    Some(token) => token,
                }
            };

            break tok
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            None
        } else {
            Some(self.scan_token())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::TokenType;

    use super::Lexer;

    #[test]
    fn empty_file_works() {
        let lexed: Vec<_> = Lexer::new("").map(|t| t.token_type()).collect();
        assert_eq!(lexed, vec![TokenType::Eof]);
    }

    #[test]
    fn some_symbols_work() {
        let input_str = ",.*;>=<=";
        let lexed: Vec<_> = Lexer::new(input_str)
            .map(|t| t.token_type())
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
            .map(|t| t.source().to_owned())
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
