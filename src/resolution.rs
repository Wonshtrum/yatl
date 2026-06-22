use std::collections::HashMap;
use std::fmt;

use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Pattern};
use crate::log;
use crate::source::{Source, Span, Spanned};

#[derive(Debug)]
pub enum Error {
    Reached,
    NotInScope {
        ident: String,
        span: Span,
    },
    ShadowInPattern {
        ident: String,
        old: Span,
        new: Span,
    },
    UnexpectedInAlternative {
        ident: String,
        span: Span,
        base_span: Span,
    },
}
impl Error {
    pub fn pretty_print<W: fmt::Write>(&self, source: &Source, out: &mut W) -> fmt::Result {
        match self {
            Error::Reached => out.write_str("reached"),
            Error::NotInScope { ident, span } => {
                out.write_fmt(format_args!(
                    "error: Identifier `{ident}` not found in scope\n"
                ))?;
                source.print_span(*span, out)
            }
            Error::ShadowInPattern { ident, old, new } => {
                out.write_fmt(format_args!(
                    "error: Identifier `{ident}` bound more than once in same pattern\n"
                ))?;
                source.print_span(*new, out)?;
                out.write_str("note: First bound here\n")?;
                source.print_span(*old, out)
            }
            Error::UnexpectedInAlternative {
                ident,
                span,
                base_span,
            } => {
                out.write_fmt(format_args!(
                    "error: Unexpected identifier `{ident}` bound in pattern\n"
                ))?;
                source.print_span(*span, out)?;
                out.write_str("note: Not found in other alternative\n")?;
                source.print_span(*base_span, out)
            }
        }
    }
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

type Scope = HashMap<String, u32>;
#[derive(Debug)]
pub struct Env {
    id: u32,
    sealed: bool,
    scopes: Vec<Scope>,
    pub symbols: HashMap<Span, u32>,
    declare: HashMap<u32, Span>,
}

impl Env {
    pub fn with_symbols(env: &[&str]) -> Self {
        let mut id = 0;
        let mut scope = HashMap::new();
        for &symbol in env {
            id += 1;
            scope.insert(symbol.to_owned(), id);
        }
        Self {
            id,
            sealed: false,
            scopes: vec![scope, HashMap::new()],
            symbols: HashMap::new(),
            declare: HashMap::new(),
        }
    }
    pub fn new() -> Self {
        Self {
            id: 0,
            sealed: false,
            scopes: vec![HashMap::new()],
            symbols: HashMap::new(),
            declare: HashMap::new(),
        }
    }
    pub fn merge(&mut self, scope: Scope) {
        let last = self.scopes.last_mut().unwrap();
        for (ident, id) in scope {
            last.insert(ident, id);
        }
    }
    pub fn push<'a>(&'a mut self) -> Nested<'a> {
        self.scopes.push(HashMap::new());
        Nested(self)
    }
    pub fn bind_pat(
        &mut self,
        alternative: Option<&(Scope, Span)>,
        ident: &str,
        span: Span,
    ) -> Result<(), Error> {
        // log!("{ident} {alternative:?}");
        let id = if let Some((base_pat, base_span)) = alternative {
            let Some(id) = base_pat.get(ident) else {
                self.sealed = true;
                return Err(Error::UnexpectedInAlternative {
                    ident: ident.to_owned(),
                    span,
                    base_span: *base_span,
                });
            };
            *id
        } else {
            self.id += 1;
            self.id
        };
        let shadow = self.scopes.last_mut().unwrap().insert(ident.to_owned(), id);
        if let Some(shadow_id) = shadow {
            let new = span;
            let old = *self.declare.get(&shadow_id).unwrap();
            self.sealed = true;
            return Err(Error::ShadowInPattern {
                ident: ident.to_owned(),
                old,
                new,
            });
        }
        self.symbols.insert(span, id);
        self.declare.insert(id, span);
        Ok(())
    }
    // pub fn bind(&mut self, ident: &str, span: Span) -> Result<(), Error> {
    //     self.id += 1;
    //     let _shadow = self
    //         .scopes
    //         .last_mut()
    //         .unwrap()
    //         .insert(ident.to_owned(), self.id);
    //     self.symbols.insert(span, self.id);
    //     Ok(())
    // }
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
impl Nested<'_> {
    fn pop(mut self) -> Scope {
        let scope = self.scopes.pop().unwrap();
        std::mem::forget(self);
        scope
    }
    fn merge(mut self) {
        let scope = self.scopes.pop().unwrap();
        self.0.merge(scope);
        std::mem::forget(self);
    }
}

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

pub fn resolve_exprs(
    env: &mut Env,
    exprs: &[Spanned<Expr>],
    target: Option<Span>,
) -> Result<(), Error> {
    for expr in exprs {
        resolve_expr(env, expr, target)?
    }
    Ok(())
}

pub fn resolve_expr(
    env: &mut Env,
    expr: &Spanned<Expr>,
    target: Option<Span>,
) -> Result<(), Error> {
    let span = expr.span;
    // log!("{} {span:?}", expr.label());
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
            env.find(name.top(), name.span)?;
            for arg in args {
                resolve_expr(env, arg, target)?;
            }
        }
        Expr::NamedConstructor { name, args } => {
            env.find(name.top(), name.span)?;
            for arg in args {
                match &arg.data {
                    NamedExpr::Implicit(ident) => {
                        env.find(ident, arg.span)?;
                    }
                    NamedExpr::Explicit { expr, .. } => {
                        resolve_expr(env, expr, target)?;
                    }
                }
            }
        }
        Expr::Binding { expr, pattern } => {
            resolve_expr(env, expr, target)?;
            let mut nested = env.push();
            resolve_pattern(&mut nested, None, pattern, target)?;
            nested.merge();
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
    // log!("Block {:?}", block.span);
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
    alternative: Option<&(Scope, Span)>,
    pattern: &Spanned<Pattern>,
    target: Option<Span>,
) -> Result<(), Error> {
    let span = pattern.span;
    // log!("{} {span:?}", pattern.label());
    match &pattern.data {
        Pattern::Wildcard | Pattern::True | Pattern::False | Pattern::Integer(_) => {}
        Pattern::Identifier(ident) => env.bind_pat(alternative, ident, span)?,
        Pattern::Path(path) => {
            env.find(path.top(), span)?;
        }
        Pattern::Array { patterns } | Pattern::Tuple { patterns } => {
            for pat in patterns {
                resolve_pattern(env, alternative, pat, target)?;
            }
        }
        Pattern::Constructor { name, patterns } => {
            env.find(name.top(), name.span)?;
            for pat in patterns {
                resolve_pattern(env, alternative, pat, target)?;
            }
        }
        Pattern::NamedConstructor { name, patterns } => {
            env.find(name.top(), name.span)?;
            for pat in patterns {
                match &pat.data {
                    NamedPattern::Implicit(ident) => {
                        env.bind_pat(alternative, ident, pat.span)?;
                    }
                    NamedPattern::Explicit { pattern, .. } => {
                        resolve_pattern(env, alternative, pattern, target)?;
                    }
                }
            }
        }
        Pattern::Alternatives { patterns } => match patterns.as_slice() {
            [] => {}
            [pat] => resolve_pattern(env, None, pat, target)?,
            [pat, pats @ ..] => {
                //[1,2] -> {
                //    [a,0],
                //    {[b,{1,2,3}]}
                //    {[_,b] => { a = 2*b; }},
                //}
                let mut nested = env.push();
                resolve_pattern(&mut nested, None, pat, target)?;
                let base = nested.pop();
                let base_span = pat.span;
                let alternative = (base, base_span);
                for pat in pats {
                    let mut nested = env.push();
                    resolve_pattern(&mut nested, Some(&alternative), pat, target)?;
                    let scope = nested.pop();
                    if scope.len() != alternative.0.len() {
                        let (ident, id) = alternative
                            .0
                            .iter()
                            .filter(|(ident, _)| !scope.contains_key(*ident))
                            .min_by_key(|(_, id)| *id)
                            .unwrap();
                        let span = *env.declare.get(id).unwrap();
                        env.sealed = true;
                        return Err(Error::UnexpectedInAlternative {
                            ident: ident.to_owned(),
                            span,
                            base_span: pat.span,
                        });
                    }
                }
                env.merge(alternative.0);
            }
        },
        Pattern::MatchArms { arms } => {
            for (pat, block) in arms {
                let mut nested = env.push();
                resolve_pattern(&mut nested, None, pat, target)?;
                resolve_block(&mut nested, block, target)?;
            }
        }
    }
    pattern.is_at(target, env)
}
