#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Else,

    // Literals
    Integer(u64),
    Identifier(String),

    // Symbols
    Arrow,
    FatArrow,
    Comma,
    Colon,
    DoubleColon,
    Semicolon,
    Underscore,

    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // End of input
    Eof,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn current(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        let pos = self.pos + offset;
        if pos < self.input.len() {
            Some(self.input[pos])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();
        while let Some(c) = self.current() {
            if c.is_alphanumeric() || c == '_' {
                result.push(c);
                self.advance();
            } else {
                break;
            }
        }
        result
    }

    fn read_integer(&mut self) -> u64 {
        let mut result = 0;
        let mut nb_digits = 0;
        while let Some(c) = self.current() {
            if c.is_ascii_digit() {
                nb_digits += 1;
                result = 10 * result + (c as u8 - b'0') as u64;
                self.advance();
            } else {
                break;
            }
        }
        result
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current() {
            None => Token::Eof,
            Some('-') if self.peek(1) == Some('>') => {
                self.advance();
                self.advance();
                Token::Arrow
            }
            Some('=') if self.peek(1) == Some('>') => {
                self.advance();
                self.advance();
                Token::FatArrow
            }
            Some(':') => {
                self.advance();
                if self.current() == Some(':') {
                    self.advance();
                    Token::DoubleColon
                } else {
                    Token::Colon
                }
            }
            Some(',') => {
                self.advance();
                Token::Comma
            }
            Some(';') => {
                self.advance();
                Token::Semicolon
            }
            Some('(') => {
                self.advance();
                Token::LParen
            }
            Some(')') => {
                self.advance();
                Token::RParen
            }
            Some('[') => {
                self.advance();
                Token::LBracket
            }
            Some(']') => {
                self.advance();
                Token::RBracket
            }
            Some('{') => {
                self.advance();
                Token::LBrace
            }
            Some('}') => {
                self.advance();
                Token::RBrace
            }
            Some(c) if c.is_alphabetic() || c == '_' => {
                let ident = self.read_identifier();
                match ident.as_str() {
                    "_" => Token::Underscore,
                    "else" => Token::Else,
                    _ => Token::Identifier(ident),
                }
            }
            Some(c) if c.is_numeric() => {
                let num = self.read_integer();
                Token::Integer(num)
            }
            Some(c) => {
                panic!("Unexpected character: {c}");
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}
