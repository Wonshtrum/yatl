use crate::source::{Source, Span, Spanned};

#[derive(Debug)]
pub enum Error {
    Unexpected(Span),
}

impl Error {
    pub fn pretty_print(&self, source: &Source) {
        match self {
            Self::Unexpected(span) => {
                println!(
                    "error: Unexpected character `{}`",
                    source.substring(span.start, span.end)
                );
                source.print_span(*span);
            }
        }
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

pub struct Lexer<'a> {
    pub source: &'a Source,
    pub pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Source) -> Lexer<'a> {
        Lexer { source, pos: 0 }
    }

    fn span(&self, start: usize) -> Span {
        Span {
            start,
            end: self.pos,
        }
    }

    fn current(&self) -> Option<char> {
        if self.pos < self.source.len() {
            Some(self.source[self.pos])
        } else {
            None
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        let pos = self.pos + offset;
        if pos < self.source.len() {
            Some(self.source[pos])
        } else {
            None
        }
    }

    #[inline]
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

    pub fn next_token(&mut self) -> Result<Spanned<Token>, Error> {
        self.skip_whitespace();

        let start = self.pos;
        let data = match self.current() {
            None => Token::Eof,
            Some('/') => {
                self.advance();
                if self.current() == Some('/') {
                    // skip comment
                    self.advance();
                    while let Some(c) = self.current() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                    return self.next_token();
                }
                Token::Divide
            }
            Some('-') => {
                self.advance();
                if self.current() == Some('>') {
                    self.advance();
                    Token::Arrow
                } else {
                    Token::Minus
                }
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
                    "true" => Token::True,
                    "false" => Token::False,
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
}
