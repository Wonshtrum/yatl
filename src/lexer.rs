use std::fmt::Debug;

#[derive(Debug)]
pub enum Error {
    Unexpected(Span),
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Position {
    pub row: usize,
    pub col: usize,
    pub line_start: usize,
    pub line_end: usize,
}

#[derive(Debug, Clone)]
pub struct Spanned<T: Debug + Clone> {
    pub data: T,
    pub span: Span,
}

impl<T: Debug + Clone> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.data
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Else,

    // Literals
    True,
    False,
    Integer(u64),
    Identifier(String),

    // Symbols
    Arrow,
    FatArrow,
    Dot,
    Comma,
    Colon,
    DoubleColon,
    Semicolon,
    Underscore,
    Exclamation,
    Interogation,

    // Logic operators
    And,
    Or,

    // Arithmetic operators
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,

    // Bitwise operators
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    // Comparison operators
    EQ,
    NE,
    LT,
    GT,
    LTE,
    GTE,

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Keywords
    Else,

    // Literals
    True,
    False,
    Integer,
    Identifier,

    // Symbols
    Arrow,
    FatArrow,
    Dot,
    Comma,
    Colon,
    DoubleColon,
    Semicolon,
    Underscore,
    Exclamation,
    Interogation,

    // Logic operators
    And,
    Or,

    // Arithmetic operators
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,

    // Bitwise operators
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    // Comparison operators
    EQ,
    NE,
    LT,
    GT,
    LTE,
    GTE,

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

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            // Keywords
            Self::Else => TokenKind::Else,

            // Literals
            Self::True => TokenKind::True,
            Self::False => TokenKind::False,
            Self::Integer(_) => TokenKind::Integer,
            Self::Identifier(_) => TokenKind::Identifier,

            // Symbols
            Self::Arrow => TokenKind::Arrow,
            Self::FatArrow => TokenKind::FatArrow,
            Self::Dot => TokenKind::Dot,
            Self::Comma => TokenKind::Comma,
            Self::Colon => TokenKind::Colon,
            Self::DoubleColon => TokenKind::DoubleColon,
            Self::Semicolon => TokenKind::Semicolon,
            Self::Underscore => TokenKind::Underscore,
            Self::Exclamation => TokenKind::Exclamation,
            Self::Interogation => TokenKind::Interogation,

            // Logic operators
            Self::And => TokenKind::And,
            Self::Or => TokenKind::Or,

            // Arithmetic operators
            Self::Plus => TokenKind::Plus,
            Self::Minus => TokenKind::Minus,
            Self::Times => TokenKind::Times,
            Self::Divide => TokenKind::Divide,
            Self::Modulo => TokenKind::Modulo,

            // Bitwise operators
            Self::BitAnd => TokenKind::BitAnd,
            Self::BitOr => TokenKind::BitOr,
            Self::BitXor => TokenKind::BitXor,
            Self::LShift => TokenKind::LShift,
            Self::RShift => TokenKind::RShift,

            // Comparison operators
            Self::EQ => TokenKind::EQ,
            Self::NE => TokenKind::NE,
            Self::LT => TokenKind::LT,
            Self::GT => TokenKind::GT,
            Self::LTE => TokenKind::LTE,
            Self::GTE => TokenKind::GTE,

            // Delimiters
            Self::LParen => TokenKind::LParen,
            Self::RParen => TokenKind::RParen,
            Self::LBracket => TokenKind::LBracket,
            Self::RBracket => TokenKind::RBracket,
            Self::LBrace => TokenKind::LBrace,
            Self::RBrace => TokenKind::RBrace,

            // End of input
            Self::Eof => TokenKind::Eof,
        }
    }
}

pub struct Lexer {
    pub file: String,
    pub input: Vec<char>,
    pub pos: usize,
    pub nl_map: Vec<usize>,
}

impl Lexer {
    pub fn new(file: String, input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
            file,
            nl_map: Vec::new(),
        }
    }

    pub fn index_to_position(&self, index: usize) -> Position {
        let mut line_start = 0;
        let mut line_end = 0;
        let mut row = 1;
        for pos in &self.nl_map {
            line_end = *pos;
            row += 1;
            if index < *pos {
                break;
            }
            line_start = *pos;
        }
        if line_end < index {
            while line_end < self.input.len() && self.input[line_end] != '\n' {
                line_end += 1;
            }
        }
        Position {
            row,
            col: index - line_start,
            line_start,
            line_end,
        }
    }

    pub fn substring(&self, start: usize, end: usize) -> String {
        self.input[start..end].iter().cloned().collect::<String>()
    }

    pub fn print_span(&self, span: Span) {
        let pos = self.index_to_position(span.start);
        println!("   --> {}:{}:{}", self.file, pos.row, pos.col);
        println!("    |");
        println!(
            "{: <3} | {}",
            pos.row,
            self.substring(pos.line_start, pos.line_end - 1)
        );
        println!(
            "    | {: >col$}{:^>len$}",
            "",
            "",
            col = pos.col,
            len = span.end - span.start
        );
    }

    fn span(&self, start: usize) -> Span {
        Span {
            start,
            end: self.pos,
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

    #[inline]
    fn advance(&mut self) {
        if self.input[self.pos] == '\n' {
            self.nl_map.push(self.pos + 1);
        }
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

    pub fn next_token(&mut self) -> Result<Spanned<Token>, Error> {
        self.skip_whitespace();

        let start = self.pos;
        let data = match self.current() {
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
                return Err(Error::Unexpected(Span {
                    start,
                    end: start + 1,
                }));
            }
        };
        Ok(Spanned {
            data,
            span: self.span(start),
        })
    }

    pub fn tokenize(&mut self) -> Result<Vec<Spanned<Token>>, Error> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token.kind() == TokenKind::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    pub fn pretty_print(&self, error: &Error) {
        match error {
            Error::Unexpected(span) => {
                println!(
                    "error: Unexpected character `{}`",
                    self.substring(span.start, span.end)
                );
                self.print_span(*span);
            }
        }
    }
}
