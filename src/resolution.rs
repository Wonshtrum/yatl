use std::collections::HashMap;

use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Pattern};
use crate::log;
use crate::source::{Span, Spanned};

#[derive(Debug)]
pub enum Error {
    Reached,
    NotInScope { ident: String, span: Span },
}

impl<T> Spanned<T> {
    fn is_at(&self, target: Option<Span>, env: &mut Env) -> Result<(), Error> {
        if let Some(target) = target {
            if self.span == target {
                env.sealed = true;
                return Err(Error::Reached);
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Env {
    id: u32,
    sealed: bool,
    scopes: Vec<HashMap<String, u32>>,
    symbols: HashMap<Span, u32>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            id: 0,
            sealed: false,
            scopes: vec![HashMap::new()],
            symbols: HashMap::new(),
        }
    }
    pub fn push<'a>(&'a mut self) -> Nested<'a> {
        self.scopes.push(HashMap::new());
        Nested(self)
    }
    pub fn bind(&mut self, ident: &str, span: Span) {
        self.id += 1;
        self.scopes
            .last_mut()
            .unwrap()
            .insert(ident.to_owned(), self.id);
        self.symbols.insert(span, self.id);
    }
    pub fn find(&mut self, ident: &str, span: Span) -> Result<u32, Error> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(ident) {
                self.symbols.insert(span, *id);
                return Ok(*id);
            }
        }
        self.sealed = true;
        Err(Error::NotInScope {
            ident: ident.to_owned(),
            span,
        })
    }
}

pub struct Nested<'a>(&'a mut Env);
impl<'a> std::ops::Deref for Nested<'a> {
    type Target = Env;
    fn deref(&self) -> &Env {
        self.0
    }
}
impl<'a> std::ops::DerefMut for Nested<'a> {
    fn deref_mut(&mut self) -> &mut Env {
        self.0
    }
}
impl<'a> std::ops::Drop for Nested<'a> {
    fn drop(&mut self) {
        if !self.sealed {
            self.scopes.pop().unwrap();
        }
    }
}

pub fn resolve_expr(
    env: &mut Env,
    expr: &Spanned<Expr>,
    target: Option<Span>,
) -> Result<(), Error> {
    let span = expr.span;
    log!("{} {span:?}", expr.label());
    match &expr.data {
        Expr::True | Expr::False | Expr::Integer(_) => {}
        Expr::Identifier(ident) => {
            env.find(ident, span)?;
        }
        Expr::Path(path) => {
            env.find(path.top(), span)?;
        }
        Expr::Block(block) => {
            let mut nested = env.push();
            resolve_raw_block(&mut nested, block, target)?;
        }
        Expr::Array { args } | Expr::Tuple { args } => {
            for arg in args {
                resolve_expr(env, arg, target)?;
            }
        }
        Expr::Constructor { name, args } => {
            env.find(name.top(), span)?;
            for arg in args {
                resolve_expr(env, arg, target)?;
            }
        }
        Expr::NamedConstructor { name, args } => {
            env.find(name.top(), span)?;
            for arg in args {
                match &arg.data {
                    NamedExpr::Implicit(ident) => {
                        env.find(ident, span)?;
                    }
                    NamedExpr::Explicit { expr, .. } => {
                        resolve_expr(env, expr, target)?;
                    }
                }
            }
        }
        Expr::Binding { expr, pattern } => {
            resolve_expr(env, expr, target)?;
            resolve_pattern(env, pattern, target)?;
        }
        Expr::ThenFlow {
            condition,
            then_block,
        } => {
            let mut nested = env.push();
            resolve_expr(&mut nested, condition, target)?;
            resolve_block(&mut nested, then_block, target)?;
        }
        Expr::ElseFlow {
            condition,
            else_block,
        } => {
            resolve_expr(env, condition, target)?;
            resolve_block(env, else_block, target)?;
        }
        Expr::ThenElseFlow {
            condition,
            then_block,
            else_block,
        } => {
            let mut nested = env.push();
            resolve_expr(&mut nested, condition, target)?;
            resolve_block(&mut nested, then_block, target)?;
            resolve_block(&mut nested, else_block, target)?;
        }
    };
    expr.is_at(target, env)
}

pub fn resolve_block(
    env: &mut Env,
    block: &Spanned<Block>,
    target: Option<Span>,
) -> Result<(), Error> {
    log!("Block {:?}", block.span);
    resolve_raw_block(env, block, target)?;
    block.is_at(target, env)
}

pub fn resolve_raw_block(env: &mut Env, block: &Block, target: Option<Span>) -> Result<(), Error> {
    for expr in &block.exprs {
        resolve_expr(env, expr, target)?;
    }
    if let Some(expr) = &block.value {
        resolve_expr(env, expr, target)?;
    }
    Ok(())
}

pub fn resolve_pattern(
    env: &mut Env,
    pattern: &Spanned<Pattern>,
    target: Option<Span>,
) -> Result<(), Error> {
    let span = pattern.span;
    log!("{} {span:?}", pattern.label());
    match &pattern.data {
        Pattern::Wildcard | Pattern::True | Pattern::False | Pattern::Integer(_) => {}
        Pattern::Identifier(ident) => env.bind(ident, span),
        Pattern::Path(path) => {
            env.find(path.top(), span)?;
        }
        Pattern::Array { patterns } | Pattern::Tuple { patterns } => {
            for pat in patterns {
                resolve_pattern(env, pat, target)?;
            }
        }
        Pattern::Constructor { name, patterns } => {
            env.find(name.top(), span)?;
            for pat in patterns {
                resolve_pattern(env, pat, target)?;
            }
        }
        Pattern::NamedConstructor { name, patterns } => {
            env.find(name.top(), span)?;
            for pat in patterns {
                match &pat.data {
                    NamedPattern::Implicit(ident) => {
                        env.bind(ident, span);
                    }
                    NamedPattern::Explicit { pattern, .. } => {
                        resolve_pattern(env, pattern, target)?;
                    }
                }
            }
        }
        Pattern::Alternatives { patterns } => log!("TODO: Alternatives"),
        Pattern::MatchArms { arms } => {
            for (pat, block) in arms {
                let mut nested = env.push();
                resolve_pattern(&mut nested, pat, target)?;
                resolve_block(&mut nested, block, target)?;
            }
        }
    }
    pattern.is_at(target, env)
}
