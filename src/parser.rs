use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Path, Pattern};
use crate::lexer::{Lexer, Token};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        Parser { tokens, pos: 0 }
    }

    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            &Token::Eof
        }
    }

    fn peek(&self, offset: usize) -> &Token {
        let pos = self.pos + offset;
        if pos < self.tokens.len() {
            &self.tokens[pos]
        } else {
            &Token::Eof
        }
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: &Token) {
        if self.current() != expected {
            panic!("Expected {:?}, found {:?}", expected, self.current());
        }
        self.advance();
    }

    pub fn parse(&mut self) -> Vec<Expr> {
        let mut exprs = Vec::new();
        while self.current() != &Token::Eof {
            let expr = self.parse_expr();
            exprs.push(expr);
            if self.current() == &Token::Semicolon {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(&Token::Eof);
        exprs
    }

    fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_primary();
        loop {
            expr = match self.current() {
                Token::Arrow => {
                    self.advance();
                    let pattern = self.parse_pattern();
                    Expr::Binding {
                        expr: Box::new(expr),
                        pattern,
                    }
                }
                Token::FatArrow => {
                    self.advance();
                    let then_block = self.parse_block();
                    if self.current() == &Token::Else {
                        let else_block = self.parse_block();
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
                Token::Else => {
                    self.advance();
                    let else_block = self.parse_block();
                    Expr::ElseFlow {
                        condition: Box::new(expr),
                        else_block,
                    }
                }
                _ => return expr,
            }
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match self.current() {
            Token::Integer(n) => {
                let n = *n;
                self.advance();
                Expr::Integer(n)
            }
            Token::LParen => {
                self.advance();
                let args = self.parse_expr_list(&Token::RParen);
                Expr::Tuple { args }
            }
            Token::LBracket => {
                self.advance();
                let args = self.parse_expr_list(&Token::RBracket);
                Expr::Array { args }
            }
            Token::Identifier(_) => {
                let path = self.parse_path();
                match self.current() {
                    Token::LParen => {
                        self.advance();
                        let args = self.parse_expr_list(&Token::RParen);
                        Expr::Constructor { name: path, args }
                    }
                    Token::LBrace => {
                        self.advance();
                        let args = self.parse_named_expr_list(&Token::RBrace);
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
            _ => panic!("Expected expression, found: {:?}", self.current()),
        }
    }

    fn parse_path(&mut self) -> Path {
        let Token::Identifier(name) = self.current() else {
            panic!("Expected path, found: {:?}", self.current());
        };
        let mut last = name.clone();
        self.advance();
        let mut parents = Vec::new();
        while self.current() == &Token::DoubleColon {
            self.advance();
            parents.push(last);
            let Token::Identifier(name) = self.current() else {
                panic!("Expected path element, found: {:?}", self.current());
            };
            last = name.clone();
            self.advance();
        }
        Path {
            name: last,
            parents,
        }
    }

    fn parse_expr_list(&mut self, terminator: &Token) -> Vec<Expr> {
        let mut args = Vec::new();
        while self.current() != terminator {
            let arg = self.parse_expr();
            args.push(arg);
            if self.current() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator);
        args
    }

    fn parse_named_expr_list(&mut self, terminator: &Token) -> Vec<NamedExpr> {
        let mut args = Vec::new();
        while self.current() != terminator {
            let Token::Identifier(name) = self.current() else {
                panic!("Expected name, found: {:?}", self.current());
            };
            let name = name.clone();
            self.advance();
            let arg = if self.current() == &Token::Colon {
                self.advance();
                let expr = self.parse_expr();
                NamedExpr::Explicit { name, expr }
            } else {
                NamedExpr::Implicit(name)
            };
            args.push(arg);
            if self.current() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator);
        args
    }

    fn parse_block(&mut self) -> Block {
        self.expect(&Token::LBrace);
        let mut exprs = Vec::new();
        while self.current() != &Token::RBrace {
            let expr = self.parse_expr();
            exprs.push(expr);
            if self.current() == &Token::Semicolon {
                self.advance();
                if self.current() == &Token::RBrace {
                    exprs.push(Expr::Unit);
                }
            } else {
                break;
            }
        }
        self.expect(&Token::RBrace);
        Block { exprs }
    }

    fn parse_pattern(&mut self) -> Pattern {
        match self.current() {
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
                let patterns = self.parse_pattern_list(&Token::RParen);
                Pattern::Tuple { patterns }
            }
            Token::LBracket => {
                self.advance();
                let patterns = self.parse_pattern_list(&Token::RBracket);
                Pattern::Array { patterns }
            }
            Token::LBrace => {
                self.advance();
                self.parse_pattern_group()
            }
            Token::Identifier(_) => {
                let path = self.parse_path();
                match self.current() {
                    Token::LParen => {
                        self.advance();
                        let patterns = self.parse_pattern_list(&Token::RParen);
                        Pattern::Constructor {
                            name: path,
                            patterns,
                        }
                    }
                    Token::LBrace => {
                        self.advance();
                        let patterns = self.parse_named_pattern_list(&Token::RBrace);
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
            _ => panic!("Expected pattern, found: {:?}", self.current()),
        }
    }

    fn parse_pattern_list(&mut self, terminator: &Token) -> Vec<Pattern> {
        let mut patterns = Vec::new();
        while self.current() != terminator {
            let pattern = self.parse_pattern();
            patterns.push(pattern);
            if self.current() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator);
        patterns
    }

    fn parse_named_pattern_list(&mut self, terminator: &Token) -> Vec<NamedPattern> {
        let mut patterns = Vec::new();
        while self.current() != terminator {
            let Token::Identifier(name) = self.current() else {
                panic!("Expected name, found: {:?}", self.current());
            };
            let name = name.clone();
            self.advance();
            let pattern = if self.current() == &Token::Colon {
                self.advance();
                let pattern = self.parse_pattern();
                NamedPattern::Explicit { name, pattern }
            } else {
                NamedPattern::Implicit(name)
            };
            patterns.push(pattern);
            if self.current() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(terminator);
        patterns
    }

    fn parse_pattern_group(&mut self) -> Pattern {
        if self.current() == &Token::RBrace {
            return Pattern::MatchArms { arms: Vec::new() };
        }

        let first_pattern = self.parse_pattern();
        if self.current() == &Token::FatArrow {
            self.advance();
            let first_block = self.parse_block();
            let mut arms = vec![(first_pattern, first_block)];
            loop {
                if self.current() == &Token::Comma {
                    self.advance();
                } else {
                    break;
                }
                if self.current() == &Token::RBrace {
                    break;
                }
                let pattern = self.parse_pattern();
                self.expect(&Token::FatArrow);
                let block = self.parse_block();
                arms.push((pattern, block));
            }
            self.expect(&Token::RBrace);
            Pattern::MatchArms { arms }
        } else {
            let mut patterns = vec![first_pattern];
            loop {
                if self.current() == &Token::Comma {
                    self.advance();
                } else {
                    break;
                }
                if self.current() == &Token::RBrace {
                    break;
                }
                let pattern = self.parse_pattern();
                patterns.push(pattern);
            }
            self.expect(&Token::RBrace);
            Pattern::Alternatives { patterns }
        }
    }
}
