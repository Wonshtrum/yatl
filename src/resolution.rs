use std::collections::HashMap;

use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Pattern};
use crate::log;
use crate::source::{Span, Spanned};

#[derive(Debug)]
pub enum Error {
    Reached,
    NotInScope(String),
}

impl<T> Spanned<T> {
    fn is_at<'a>(&self, target: Option<Span>, env: &Env<'a>) -> Result<(), Error> {
        if let Some(target) = target {
            if self.span == target {
                return Err(Error::Reached);
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Env<'a> {
    pub id: u32,
    pub parent: Option<&'a Env<'a>>,
    pub idents: HashMap<String, u32>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'a> {
        Self {
            id: 0,
            parent: None,
            idents: HashMap::new(),
        }
    }
    pub fn find(&self, ident: &str) -> Result<u32, Error> {
        if let Some(id) = self.idents.get(ident) {
            return Ok(*id);
        }
        if let Some(parent) = self.parent {
            return parent.find(ident);
        }
        Err(Error::NotInScope(ident.to_owned()))
    }
    pub fn push(&'a self) -> Env<'a> {
        Self {
            id: self.id,
            parent: Some(self),
            idents: HashMap::new(),
        }
    }
    pub fn bind(&mut self, idents: &str) {
        self.id += 1;
        self.idents.insert(idents.to_owned(), self.id);
    }
}

pub fn resolve_expr<'a>(
    env: &mut Env<'a>,
    expr: &Spanned<Expr>,
    target: Option<Span>,
) -> Result<(), Error> {
    log!("{} {:?}", expr.label(), expr.span);
    match &expr.data {
        Expr::True | Expr::False | Expr::Integer(_) => {}
        Expr::Identifier(ident) => {
            env.find(ident)?;
        }
        Expr::Path(path) => {
            env.find(path.top())?;
        }
        Expr::Block(block) => {
            resolve_raw_block(env, block, target)?;
        }
        Expr::Array { args } | Expr::Tuple { args } => {
            for arg in args {
                resolve_expr(env, arg, target)?;
            }
        }
        Expr::Constructor { name, args } => {
            env.find(name.top())?;
            for arg in args {
                resolve_expr(env, arg, target)?;
            }
        }
        Expr::NamedConstructor { name, args } => {
            env.find(name.top())?;
            for arg in args {
                match &arg.data {
                    NamedExpr::Implicit(ident) => {
                        env.find(ident)?;
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

pub fn resolve_block<'a>(
    env: &mut Env<'a>,
    block: &Spanned<Block>,
    target: Option<Span>,
) -> Result<(), Error> {
    resolve_raw_block(env, block, target)?;
    block.is_at(target, env)
}

pub fn resolve_raw_block<'a>(
    env: &mut Env<'a>,
    block: &Block,
    target: Option<Span>,
) -> Result<(), Error> {
    let mut nested = env.push();
    for expr in &block.exprs {
        resolve_expr(&mut nested, expr, target)?;
    }
    if let Some(expr) = &block.value {
        resolve_expr(&mut nested, expr, target)?;
    }
    Ok(())
}

pub fn resolve_pattern<'a>(
    env: &mut Env<'a>,
    pattern: &Spanned<Pattern>,
    target: Option<Span>,
) -> Result<(), Error> {
    log!("{} {:?}", pattern.label(), pattern.span);
    match &pattern.data {
        Pattern::Wildcard | Pattern::True | Pattern::False | Pattern::Integer(_) => {}
        Pattern::Identifier(ident) => env.bind(ident),
        Pattern::Path(path) => {
            env.find(path.top())?;
        }
        Pattern::Array { patterns } | Pattern::Tuple { patterns } => {
            for pat in patterns {
                resolve_pattern(env, pat, target)?;
            }
        }
        Pattern::Constructor { name, patterns } => {
            env.find(name.top())?;
            for pat in patterns {
                resolve_pattern(env, pat, target)?;
            }
        }
        Pattern::NamedConstructor { name, patterns } => {
            env.find(name.top())?;
            for pat in patterns {
                match &pat.data {
                    NamedPattern::Implicit(ident) => {
                        env.bind(ident);
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
