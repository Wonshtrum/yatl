use std::fmt;

use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Path, Pattern};
use crate::lexer::{Lexer, Token, TokenKind};
use crate::source::{Source, Span, Spanned};

#[derive(Debug)]
pub enum Error {
    Lexer(crate::lexer::Error),
    Unexpected {
        message: Option<&'static str>,
        expected: Vec<TokenKind>,
        found: Spanned<Token>,
        started: Option<Span>,
    },
}

impl Error {
    pub fn pretty_print<W: fmt::Write>(&self, source: &Source, out: &mut W) -> fmt::Result {
        match self {
            Self::Lexer(error) => error.pretty_print(source, out),
            Self::Unexpected {
                message,
                expected,
                found,
                started,
            } => {
                out.write_str("error: ")?;
                if let Some(message) = message {
                    out.write_fmt(format_args!("Expected {message}"))?;
                    if expected.len() == 1 {
                        out.write_fmt(format_args!(" (token `{:?}`)", expected[0]))?;
                    } else if expected.len() > 1 {
                        out.write_fmt(format_args!(" (tokens {:?})", expected))?;
                    }
                    out.write_str(", but found")?;
                } else if expected.len() == 1 {
                    out.write_fmt(format_args!(
                        "Expected token `{:?}`, but found",
                        expected[0]
                    ))?;
                } else {
                    out.write_str("Unexpected")?;
                }
                out.write_fmt(format_args!(" token `{:?}`\n", found.data))?;
                source.print_span(found.span, out)?;
                if let Some(started) = started {
                    out.write_str("note: Started by\n")?;
                    source.print_span(*started, out)?;
                }
                Ok(())
            }
        }
    }
}

pub struct Parser<'a> {
    pub source: &'a Source,
    pub tokens: Vec<Spanned<Token>>,
    pub pos: usize,
    eof: Spanned<Token>,
}

impl<'a> Parser<'a> {
    pub fn try_new(source: &'a Source) -> Result<Parser<'a>, Error> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(Error::Lexer)?;
        Ok(Self::new(source, tokens))
    }

    pub fn new(source: &'a Source, tokens: Vec<Spanned<Token>>) -> Parser<'a> {
        let eof = tokens.last().unwrap().clone();
        Parser {
            source,
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

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(
        &mut self,
        expected: TokenKind,
        message: Option<&'static str>,
        started: Option<Span>,
    ) -> Result<Span, Error> {
        if self.current().kind() != expected {
            return Err(Error::Unexpected {
                message,
                expected: vec![expected],
                found: self.current().clone(),
                started,
            });
        }
        let span = self.current().span;
        self.advance();
        Ok(span)
    }

    pub fn parse(&mut self) -> Result<Vec<Spanned<Expr>>, Error> {
        let mut exprs = Vec::new();
        while self.current().kind() != TokenKind::Eof {
            let expr = self.parse_expr()?;
            exprs.push(expr);
            if self.current().kind() == TokenKind::Semicolon {
                self.advance();
            } else if self.current().kind() != TokenKind::Eof {
                return Err(Error::Unexpected {
                    expected: Vec::new(),
                    message: Some("end of expression or file"),
                    found: self.current().clone(),
                    started: None,
                });
            }
        }
        self.expect(TokenKind::Eof, None, None)?;
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        let mut expr = self.parse_primary()?;
        let span = expr.span;
        loop {
            expr = match self.current().kind() {
                TokenKind::Arrow => {
                    self.advance();
                    let pattern = self.parse_pattern()?;
                    span.merge(pattern.span).attach(Expr::Binding {
                        expr: Box::new(expr),
                        pattern,
                    })
                }
                TokenKind::FatArrow => {
                    self.advance();
                    let then_block = self.parse_block()?;
                    if self.current().kind() == TokenKind::Else {
                        self.advance();
                        let else_block = self.parse_block()?;
                        span.merge(else_block.span).attach(Expr::ThenElseFlow {
                            condition: Box::new(expr),
                            then_block,
                            else_block,
                        })
                    } else {
                        span.merge(then_block.span).attach(Expr::ThenFlow {
                            condition: Box::new(expr),
                            then_block,
                        })
                    }
                }
                TokenKind::Else => {
                    self.advance();
                    let else_block = self.parse_block()?;
                    span.merge(else_block.span).attach(Expr::ElseFlow {
                        condition: Box::new(expr),
                        else_block,
                    })
                }
                _ => return Ok(expr),
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Spanned<Expr>, Error> {
        while self.current().kind() == TokenKind::Semicolon {
            self.advance();
        }
        let span = self.current().span;
        let expr = match &self.current().data {
            Token::True => {
                self.advance();
                span.attach(Expr::True)
            }
            Token::False => {
                self.advance();
                span.attach(Expr::False)
            }
            Token::Integer(n) => {
                let n = *n;
                self.advance();
                span.attach(Expr::Integer(n))
            }
            Token::LParen => {
                self.advance();
                let args = self.parse_expr_list(TokenKind::RParen)?;
                let last_span = self.expect(TokenKind::RParen, Some("end of tuple"), Some(span))?;
                span.merge(last_span).attach(Expr::Tuple { args })
            }
            Token::LBracket => {
                self.advance();
                let args = self.parse_expr_list(TokenKind::RBracket)?;
                let last_span =
                    self.expect(TokenKind::RBracket, Some("end of array"), Some(span))?;
                span.merge(last_span).attach(Expr::Array { args })
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                block.span.attach(Expr::Block(block.data))
            }
            Token::Identifier(_) => {
                let path = self.parse_path()?;
                let started = Some(self.current().span);
                match self.current().kind() {
                    TokenKind::LParen => {
                        self.advance();
                        let args = self.parse_expr_list(TokenKind::RParen)?;
                        let last_span =
                            self.expect(TokenKind::RParen, Some("end of constructor"), started)?;
                        span.merge(last_span)
                            .attach(Expr::Constructor { name: path, args })
                    }
                    TokenKind::LBrace => {
                        self.advance();
                        let args = self.parse_named_expr_list(TokenKind::RBrace)?;
                        let last_span = self.expect(
                            TokenKind::RBrace,
                            Some("end of named constructor"),
                            started,
                        )?;
                        span.merge(last_span)
                            .attach(Expr::NamedConstructor { name: path, args })
                    }
                    _ => {
                        let expr = if path.parents.is_empty() {
                            Expr::Identifier(path.data.name)
                        } else {
                            Expr::Path(path.data)
                        };
                        path.span.attach(expr)
                    }
                }
            }
            _ => {
                return Err(Error::Unexpected {
                    expected: Vec::new(),
                    message: Some("primary expression"),
                    found: self.current().clone(),
                    started: None,
                });
            }
        };
        Ok(expr)
    }

    fn parse_path(&mut self) -> Result<Spanned<Path>, Error> {
        let Token::Identifier(name) = &self.current().data else {
            return Err(Error::Unexpected {
                expected: Vec::new(),
                message: Some("path"),
                found: self.current().clone(),
                started: None,
            });
        };
        let span = self.current().span;
        let mut last = name.clone();
        let mut last_span = span;
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
                    started: None,
                });
            };
            last = name.clone();
            last_span = self.current().span;
            self.advance();
        }
        Ok(span.merge(last_span).attach(Path {
            name: last,
            parents,
        }))
    }

    fn parse_expr_list(&mut self, terminator: TokenKind) -> Result<Vec<Spanned<Expr>>, Error> {
        let mut exprs = Vec::new();
        while self.current().kind() != terminator {
            let expr = self.parse_expr()?;
            exprs.push(expr);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(exprs)
    }

    fn parse_named_expr_list(
        &mut self,
        terminator: TokenKind,
    ) -> Result<Vec<Spanned<NamedExpr>>, Error> {
        let mut exprs = Vec::new();
        while self.current().kind() != terminator {
            let Token::Identifier(name) = &self.current().data else {
                return Err(Error::Unexpected {
                    expected: vec![TokenKind::Identifier],
                    message: Some("name"),
                    found: self.current().clone(),
                    started: None,
                });
            };
            let name = name.clone();
            let span = self.current().span;
            self.advance();
            let expr = if self.current().kind() == TokenKind::Colon {
                self.advance();
                let expr = self.parse_expr()?;
                span.merge(expr.span)
                    .attach(NamedExpr::Explicit { name, expr })
            } else {
                span.attach(NamedExpr::Implicit(name))
            };
            exprs.push(expr);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(exprs)
    }

    fn parse_block(&mut self) -> Result<Spanned<Block>, Error> {
        let span = self.expect(TokenKind::LBrace, Some("start of block"), None)?;
        let mut exprs = Vec::new();
        let mut value = None;
        while self.current().kind() != TokenKind::RBrace {
            let expr = self.parse_expr()?;
            if self.current().kind() == TokenKind::Semicolon {
                self.advance();
                exprs.push(expr);
            } else {
                value = Some(Box::new(expr));
                break;
            }
        }
        let last_span = self.expect(TokenKind::RBrace, Some("end of block"), Some(span))?;
        Ok(span.merge(last_span).attach(Block { exprs, value }))
    }

    fn parse_pattern(&mut self) -> Result<Spanned<Pattern>, Error> {
        let span = self.current().span;
        let pattern = match &self.current().data {
            Token::Underscore => {
                self.advance();
                span.attach(Pattern::Wildcard)
            }
            Token::True => {
                self.advance();
                span.attach(Pattern::True)
            }
            Token::False => {
                self.advance();
                span.attach(Pattern::False)
            }
            Token::Integer(n) => {
                let n = *n;
                self.advance();
                span.attach(Pattern::Integer(n))
            }
            Token::LParen => {
                self.advance();
                let patterns = self.parse_pattern_list(TokenKind::RParen)?;
                let last_span =
                    self.expect(TokenKind::RParen, Some("end of tuple pattern"), Some(span))?;
                span.merge(last_span).attach(Pattern::Tuple { patterns })
            }
            Token::LBracket => {
                self.advance();
                let patterns = self.parse_pattern_list(TokenKind::RBracket)?;
                let last_span = self.expect(
                    TokenKind::RBracket,
                    Some("end of array pattern"),
                    Some(span),
                )?;
                span.merge(last_span).attach(Pattern::Array { patterns })
            }
            Token::LBrace => {
                self.advance();
                let pattern = self.parse_pattern_group()?;
                let last_span =
                    self.expect(TokenKind::RBrace, Some("end of pattern group"), Some(span))?;
                span.merge(last_span).attach(pattern)
            }
            Token::Identifier(_) => {
                let path = self.parse_path()?;
                let started = Some(self.current().span);
                match self.current().kind() {
                    TokenKind::LParen => {
                        self.advance();
                        let patterns = self.parse_pattern_list(TokenKind::RParen)?;
                        let last_span = self.expect(
                            TokenKind::RParen,
                            Some("end of constructor pattern"),
                            started,
                        )?;
                        span.merge(last_span).attach(Pattern::Constructor {
                            name: path,
                            patterns,
                        })
                    }
                    TokenKind::LBrace => {
                        self.advance();
                        let patterns = self.parse_named_pattern_list(TokenKind::RBrace)?;
                        let last_span = self.expect(
                            TokenKind::RBrace,
                            Some("end of named constructor pattern"),
                            started,
                        )?;
                        span.merge(last_span).attach(Pattern::NamedConstructor {
                            name: path,
                            patterns,
                        })
                    }
                    _ => {
                        let pattern = if path.parents.is_empty() {
                            Pattern::Identifier(path.data.name)
                        } else {
                            Pattern::Path(path.data)
                        };
                        path.span.attach(pattern)
                    }
                }
            }
            _ => {
                return Err(Error::Unexpected {
                    expected: Vec::new(),
                    message: Some("pattern"),
                    found: self.current().clone(),
                    started: None,
                });
            }
        };
        Ok(pattern)
    }

    fn parse_pattern_list(
        &mut self,
        terminator: TokenKind,
    ) -> Result<Vec<Spanned<Pattern>>, Error> {
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
        Ok(patterns)
    }

    fn parse_named_pattern_list(
        &mut self,
        terminator: TokenKind,
    ) -> Result<Vec<Spanned<NamedPattern>>, Error> {
        let mut patterns = Vec::new();
        while self.current().kind() != terminator {
            let Token::Identifier(name) = &self.current().data else {
                return Err(Error::Unexpected {
                    expected: vec![TokenKind::Identifier],
                    message: Some("pattern name"),
                    found: self.current().clone(),
                    started: None,
                });
            };
            let name = name.clone();
            let span = self.current().span;
            self.advance();
            let pattern = if self.current().kind() == TokenKind::Colon {
                self.advance();
                let pattern = self.parse_pattern()?;
                span.merge(pattern.span)
                    .attach(NamedPattern::Explicit { name, pattern })
            } else {
                span.attach(NamedPattern::Implicit(name))
            };
            patterns.push(pattern);
            if self.current().kind() == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(patterns)
    }

    fn parse_pattern_group(&mut self) -> Result<Pattern, Error> {
        if self.current().kind() == TokenKind::RBrace {
            return Ok(Pattern::MatchArms { arms: Vec::new() });
        }

        let first_pattern = self.parse_pattern()?;
        let pattern = if self.current().kind() == TokenKind::FatArrow {
            let span = self.current().span;
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
                self.expect(
                    TokenKind::FatArrow,
                    Some("start of arrowed expression"),
                    Some(span),
                )?;
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
        Ok(pattern)
    }
}
