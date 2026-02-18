use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Path, Pattern};
use crate::lexer::{Lexer, Spanned, Token, TokenKind};

#[derive(Debug)]
pub enum Error {
    Lexer(crate::lexer::Error),
    Unexpected {
        message: Option<&'static str>,
        expected: Vec<TokenKind>,
        found: Spanned<Token>,
    },
}

pub struct Parser {
    pub lexer: Lexer,
    pub tokens: Vec<Spanned<Token>>,
    pub pos: usize,
    eof: Spanned<Token>,
}

impl Parser {
    pub fn new(file: String, input: &str) -> Result<Self, Error> {
        let mut lexer = Lexer::new(file, input);
        let tokens = lexer.tokenize().map_err(Error::Lexer)?;
        Ok(Self::from_tokens(lexer, tokens))
    }

    pub fn from_tokens(lexer: Lexer, tokens: Vec<Spanned<Token>>) -> Self {
        let eof = tokens.last().unwrap().clone();
        Parser {
            lexer,
            tokens,
            pos: 0,
            eof,
        }
    }

    fn current(&self) -> &Spanned<Token> {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            &self.eof
        }
    }

    fn peek(&self, offset: usize) -> &Spanned<Token> {
        let pos = self.pos + offset;
        if pos < self.tokens.len() {
            &self.tokens[pos]
        } else {
            &self.eof
        }
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: TokenKind, message: Option<&'static str>) -> Result<(), Error> {
        if self.current().kind() != expected {
            return Err(Error::Unexpected {
                message,
                expected: vec![expected],
                found: self.current().clone(),
            });
            //panic!("Expected {:?}, found {:?}", expected, self.current());
        }
        self.advance();
        Ok(())
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, Error> {
        let mut exprs = Vec::new();
        while self.current().kind() != TokenKind::Eof {
            let expr = self.parse_expr()?;
            exprs.push(expr);
            if self.current().kind() == TokenKind::Semicolon {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(TokenKind::Eof, None)?;
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_primary()?;
        loop {
            expr = match self.current().kind() {
                TokenKind::Arrow => {
                    self.advance();
                    let pattern = self.parse_pattern()?;
                    Expr::Binding {
                        expr: Box::new(expr),
                        pattern,
                    }
                }
                TokenKind::FatArrow => {
                    self.advance();
                    let then_block = self.parse_block()?;
                    if self.current().kind() == TokenKind::Else {
                        let else_block = self.parse_block()?;
                        Expr::ThenElseFlow {
                            condition: Box::new(expr),
                            then_block,
                            else_block,
                        }
                    } else {
                        Expr::ThenFlow {
                            condition: Box::new(expr),
                            then_block,
                        }
                    }
                }
                TokenKind::Else => {
                    self.advance();
                    let else_block = self.parse_block()?;
                    Expr::ElseFlow {
                        condition: Box::new(expr),
                        else_block,
                    }
                }
                _ => return Ok(expr),
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        let expr = match &self.current().data {
            Token::Integer(n) => {
                let n = *n;
                self.advance();
                Expr::Integer(n)
            }
            Token::LParen => {
                self.advance();
                let args = self.parse_expr_list(TokenKind::RParen)?;
                Expr::Tuple { args }
            }
            Token::LBracket => {
                self.advance();
                let args = self.parse_expr_list(TokenKind::RBracket)?;
                Expr::Array { args }
            }
            Token::Identifier(_) => {
                let path = self.parse_path()?;
                match self.current().kind() {
                    TokenKind::LParen => {
                        self.advance();
                        let args = self.parse_expr_list(TokenKind::RParen)?;
                        Expr::Constructor { name: path, args }
                    }
                    TokenKind::LBrace => {
                        self.advance();
                        let args = self.parse_named_expr_list(TokenKind::RBrace)?;
                        Expr::NamedConstructor { name: path, args }
                    }
                    _ => {
                        if path.parents.is_empty() {
                            Expr::Identifier(path.name)
                        } else {
                            Expr::Path(path)
                        }
                    }
                }
            }
            _ => {
                return Err(Error::Unexpected {
                    expected: Vec::new(),
                    message: Some("primary expression"),
                    found: self.current().clone(),
                });
            } // _ => panic!("Expected expression, found: {:?}", self.current()),
        };
        Ok(expr)
    }

    fn parse_path(&mut self) -> Result<Path, Error> {
        let Token::Identifier(name) = &self.current().data else {
            // panic!("Expected path, found: {:?}", self.current());
            return Err(Error::Unexpected {
                expected: Vec::new(),
                message: Some("path"),
                found: self.current().clone(),
            });
        };
        let mut last = name.clone();
        self.advance();
        let mut parents = Vec::new();
        while self.current().kind() == TokenKind::DoubleColon {
            self.advance();
            parents.push(last);
            let Token::Identifier(name) = &self.current().data else {
                return Err(Error::Unexpected {
                    expected: vec![TokenKind::Identifier],
                    message: Some("path segment"),
                    found: self.current().clone(),
                });
                // panic!("Expected path element, found: {:?}", self.current());
            };
            last = name.clone();
            self.advance();
        }
        Ok(Path {
            name: last,
            parents,
        })
    }

    fn parse_expr_list(&mut self, terminator: TokenKind) -> Result<Vec<Expr>, Error> {
        let mut args = Vec::new();
        while self.current().kind() != terminator {
            let arg = self.parse_expr()?;
            args.push(arg);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator, Some("end of expression list"))?;
        Ok(args)
    }

    fn parse_named_expr_list(&mut self, terminator: TokenKind) -> Result<Vec<NamedExpr>, Error> {
        let mut args = Vec::new();
        while self.current().kind() != terminator {
            let Token::Identifier(name) = &self.current().data else {
                return Err(Error::Unexpected {
                    expected: vec![TokenKind::Identifier],
                    message: Some("name"),
                    found: self.current().clone(),
                });
                // panic!("Expected name, found: {:?}", self.current());
            };
            let name = name.clone();
            self.advance();
            let arg = if self.current().kind() == TokenKind::Colon {
                self.advance();
                let expr = self.parse_expr()?;
                NamedExpr::Explicit { name, expr }
            } else {
                NamedExpr::Implicit(name)
            };
            args.push(arg);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator, Some("end of named expression list"))?;
        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Block, Error> {
        self.expect(TokenKind::LBrace, Some("start of block"))?;
        let mut exprs = Vec::new();
        while self.current().kind() != TokenKind::RBrace {
            let expr = self.parse_expr()?;
            exprs.push(expr);
            if self.current().kind() == TokenKind::Semicolon {
                self.advance();
                if self.current().kind() == TokenKind::RBrace {
                    exprs.push(Expr::Unit);
                }
            } else {
                break;
            }
        }
        self.expect(TokenKind::RBrace, Some("end of block"))?;
        Ok(Block { exprs })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, Error> {
        let pattern = match &self.current().data {
            Token::Underscore => {
                self.advance();
                Pattern::Wildcard
            }
            Token::Integer(n) => {
                let n = *n;
                self.advance();
                Pattern::Integer(n)
            }
            Token::LParen => {
                self.advance();
                let patterns = self.parse_pattern_list(TokenKind::RParen)?;
                Pattern::Tuple { patterns }
            }
            Token::LBracket => {
                self.advance();
                let patterns = self.parse_pattern_list(TokenKind::RBracket)?;
                Pattern::Array { patterns }
            }
            Token::LBrace => {
                self.advance();
                self.parse_pattern_group()?
            }
            Token::Identifier(_) => {
                let path = self.parse_path()?;
                match self.current().kind() {
                    TokenKind::LParen => {
                        self.advance();
                        let patterns = self.parse_pattern_list(TokenKind::RParen)?;
                        Pattern::Constructor {
                            name: path,
                            patterns,
                        }
                    }
                    TokenKind::LBrace => {
                        self.advance();
                        let patterns = self.parse_named_pattern_list(TokenKind::RBrace)?;
                        Pattern::NamedConstructor {
                            name: path,
                            patterns,
                        }
                    }
                    _ => {
                        if path.parents.is_empty() {
                            Pattern::Identifier(path.name)
                        } else {
                            Pattern::Path(path)
                        }
                    }
                }
            }
            _ => {
                return Err(Error::Unexpected {
                    expected: Vec::new(),
                    message: Some("pattern"),
                    found: self.current().clone(),
                });
            } // _ => panic!("Expected pattern, found: {:?}", self.current()),
        };
        Ok(pattern)
    }

    fn parse_pattern_list(&mut self, terminator: TokenKind) -> Result<Vec<Pattern>, Error> {
        let mut patterns = Vec::new();
        while self.current().kind() != terminator {
            let pattern = self.parse_pattern()?;
            patterns.push(pattern);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator, Some("end of pattern list"))?;
        Ok(patterns)
    }

    fn parse_named_pattern_list(
        &mut self,
        terminator: TokenKind,
    ) -> Result<Vec<NamedPattern>, Error> {
        let mut patterns = Vec::new();
        while self.current().kind() != terminator {
            let Token::Identifier(name) = &self.current().data else {
                return Err(Error::Unexpected {
                    expected: vec![TokenKind::Identifier],
                    message: Some("pattern name"),
                    found: self.current().clone(),
                });
                // panic!("Expected name, found: {:?}", self.current());
            };
            let name = name.clone();
            self.advance();
            let pattern = if self.current().kind() == TokenKind::Colon {
                self.advance();
                let pattern = self.parse_pattern()?;
                NamedPattern::Explicit { name, pattern }
            } else {
                NamedPattern::Implicit(name)
            };
            patterns.push(pattern);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator, Some("end of named pattern list"))?;
        Ok(patterns)
    }

    fn parse_pattern_group(&mut self) -> Result<Pattern, Error> {
        if self.current().kind() == TokenKind::RBrace {
            return Ok(Pattern::MatchArms { arms: Vec::new() });
        }

        let first_pattern = self.parse_pattern()?;
        let pattern = if self.current().kind() == TokenKind::FatArrow {
            self.advance();
            let first_block = self.parse_block()?;
            let mut arms = vec![(first_pattern, first_block)];
            loop {
                if self.current().kind() == TokenKind::Comma {
                    self.advance();
                } else {
                    break;
                }
                if self.current().kind() == TokenKind::RBrace {
                    break;
                }
                let pattern = self.parse_pattern()?;
                self.expect(TokenKind::FatArrow, Some("start of arrowed expression"))?;
                let block = self.parse_block()?;
                arms.push((pattern, block));
            }
            Pattern::MatchArms { arms }
        } else {
            let mut patterns = vec![first_pattern];
            loop {
                if self.current().kind() == TokenKind::Comma {
                    self.advance();
                } else {
                    break;
                }
                if self.current().kind() == TokenKind::RBrace {
                    break;
                }
                let pattern = self.parse_pattern()?;
                patterns.push(pattern);
            }
            Pattern::Alternatives { patterns }
        };
        self.expect(TokenKind::RBrace, Some("end of pattern group"))?;
        Ok(pattern)
    }

    pub fn pretty_print(&self, error: &Error) {
        match error {
            Error::Lexer(error) => self.lexer.pretty_print(error),
            Error::Unexpected {
                message,
                expected,
                found,
            } => {
                print!("error: ");
                if let Some(message) = message {
                    print!("Expected {message}");
                    if expected.len() == 1 {
                        print!(" (token: `{:?}`)", expected[0]);
                    } else if expected.len() > 1 {
                        print!(" (tokens: {:?})", expected);
                    }
                    print!(", found:");
                } else if expected.len() == 1 {
                    print!("Expected token `{:?}`, found:", expected[0]);
                } else {
                    print!("Unexpected");
                }
                println!(" token `{:?}`", found.data);
                self.lexer.print_span(found.span);
            }
        }
    }
}
