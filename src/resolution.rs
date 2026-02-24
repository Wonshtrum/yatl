use std::collections::HashMap;

use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Pattern};

#[derive(Debug)]
pub enum Error {
    NotInScope(String),
}

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

pub fn resolve_expr<'a>(env: &mut Env<'a>, expr: &Expr) -> Result<(), Error> {
    match expr {
        Expr::True | Expr::False | Expr::Integer(_) => {}

        Expr::Identifier(ident) => {
            env.find(ident)?;
        }
        Expr::Path(path) => {
            env.find(path.top())?;
        }
        Expr::Block(block) => {
            resolve_block(env, block)?;
        }
        Expr::Array { args } | Expr::Tuple { args } => {
            for arg in args {
                resolve_expr(env, arg)?;
            }
        }
        Expr::Constructor { name, args } => {
            env.find(name.top())?;
            for arg in args {
                resolve_expr(env, arg)?;
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
                        resolve_expr(env, expr)?;
                    }
                }
            }
        }
        Expr::Binding { expr, pattern } => {
            resolve_expr(env, expr)?;
            resolve_pattern(env, pattern)?;
        }
        Expr::ThenFlow {
            condition,
            then_block,
        } => {
            let mut nested = env.push();
            resolve_expr(&mut nested, &condition.data)?;
            resolve_block(&mut nested, &then_block.data)?;
        }
        Expr::ElseFlow {
            condition,
            else_block,
        } => {
            resolve_expr(env, condition)?;
            resolve_block(env, &else_block.data)?;
        }
        Expr::ThenElseFlow {
            condition,
            then_block,
            else_block,
        } => {
            let mut nested = env.push();
            resolve_expr(&mut nested, condition)?;
            resolve_block(&mut nested, &then_block.data)?;
            resolve_block(&mut nested, &else_block.data)?;
        }
    };
    Ok(())
}

pub fn resolve_block<'a>(env: &mut Env<'a>, block: &Block) -> Result<(), Error> {
    let mut nested = env.push();
    for expr in &block.exprs {
        resolve_expr(&mut nested, expr)?;
    }
    if let Some(expr) = &block.value {
        resolve_expr(&mut nested, expr)?;
    }
    Ok(())
}

pub fn resolve_pattern<'a>(env: &mut Env<'a>, pattern: &Pattern) -> Result<(), Error> {
    match pattern {
        Pattern::Wildcard | Pattern::True | Pattern::False | Pattern::Integer(_) => {}

        Pattern::Identifier(ident) => env.bind(ident),
        Pattern::Path(path) => {
            env.find(path.top())?;
        }

        Pattern::Array { patterns } | Pattern::Tuple { patterns } => {
            for pat in patterns {
                resolve_pattern(env, pat)?;
            }
        }
        Pattern::Constructor { name, patterns } => {
            env.find(name.top())?;
            for pat in patterns {
                resolve_pattern(env, pat)?;
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
                        resolve_pattern(env, pattern)?;
                    }
                }
            }
        }
        Pattern::Alternatives { patterns } => todo!(),
        Pattern::MatchArms { arms } => {
            for (pat, block) in arms {
                let mut nested = env.push();
                resolve_pattern(&mut nested, pat)?;
                resolve_block(&mut nested, block)?;
            }
        }
    }
    Ok(())
}
