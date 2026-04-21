use std::collections::HashMap;

use crate::log;
use crate::ast::{Expr, Pattern};
use crate::source::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    True,
    False,
    Integer(u64),
    List(Vec<Value>),
    Func(&'static str)
}

#[derive(Debug)]
struct Env {
    symbols: HashMap<Span, u32>,
    values: HashMap<u32, Value>,
}

impl Env {
    fn get(&self, span: Span) -> Value {
        let id = self.symbols.get(&span).expect("symbol");
        self.values.get(&id).expect("value").clone()
    }
    fn set(&mut self, span: Span, value: Value) {
        let id = self.symbols.get(&span).expect("symbol");
        self.values.insert(*id, value);
    }
}

pub fn run(functions: &[&'static str], symbols: HashMap<Span, u32>, exprs: &[Spanned<Expr>]) -> Value {
    let mut env = Env {
        symbols,
        values: HashMap::new(),
    };
    for (i, name) in functions.iter().enumerate() {
        env.values.insert((i+1) as u32, Value::Func(name));
    }
    log!("{env:?}");
    let mut last = None;
    for expr in exprs {
        let val = eval_expr(&mut env, expr);
        last = Some(val);
    }
    last.unwrap_or(Value::Unit)
}

fn eval_expr(env: &mut Env, expr: &Spanned<Expr>) -> Value {
    match &expr.data {
        Expr::True => Value::True,
        Expr::False => Value::False,
        Expr::Integer(val) => Value::Integer(*val),
        Expr::Identifier(_) => env.get(expr.span),
        Expr::Path(_) => unimplemented!(),
        Expr::Block(block) => {
            for expr in &block.exprs {
                eval_expr(env, expr);
            }
            if let Some(value) = &block.value {
                eval_expr(env, value)
            } else {
                Value::Unit
            }
        }
        Expr::Array { args } | Expr::Tuple { args } => {
            let args = args.iter().map(|arg| eval_expr(env, arg)).collect::<Vec<_>>();
            Value::List(args)
        }
        Expr::Constructor { name, args } => {
            let args = args.iter().map(|arg| eval_expr(env, arg)).collect::<Vec<_>>();
            match env.get(name.span) {
                Value::Func("print") => {
                    log!("CALL: {args:?}");
                    Value::Unit
                },
                _ => panic!(),
            }
        }
        Expr::Binding { expr, pattern } => {
            let val = eval_expr(env, expr);
            let bound = try_bind(env, val, pattern);
            if !bound {
                panic!("binding fail");
            }
            Value::Unit
        }
        _  => unimplemented!(),
    }
}

fn try_bind(env: &mut Env, val: Value, pat: &Spanned<Pattern>) -> bool {
    match (val, &pat.data) {
        (_, Pattern::Wildcard) => true,
        (val, Pattern::Identifier(_)) => {
            env.set(pat.span, val);
            true
        },
        (Value::True, Pattern::True) => true,
        (Value::False, Pattern::False) => true,
        (Value::Integer(v), Pattern::Integer(p)) => v == *p,
        _ => false,
    }
}
