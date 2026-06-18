use std::collections::HashMap;

use crate::ast::{Block, Expr, Pattern};
use crate::log;
use crate::source::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    True,
    False,
    Integer(u64),
    List(Vec<Value>),
    Func(&'static str),
}
impl Value {
    fn truthy(&self) -> bool {
        match self {
            Value::Unit => true,
            Value::True => true,
            Value::False => false,
            Value::Integer(v) => *v != 0,
            Value::List(values) => !values.is_empty(),
            Value::Func(_) => true,
        }
    }
}

#[derive(Debug)]
struct Env {
    symbols: HashMap<Span, u32>,
    values: HashMap<u32, Value>,
}

impl Env {
    fn get(&self, span: Span) -> Value {
        let Some(id) = self.symbols.get(&span) else {
            log!("env: {self:#?}");
            log!("symbol: {span:?}");
            panic!();
        };
        self.values.get(id).expect("value").clone()
    }
    fn set(&mut self, span: Span, value: Value) {
        let Some(id) = self.symbols.get(&span) else {
            log!("env: {self:#?}");
            log!("symbol: {span:?}");
            panic!();
        };
        self.values.insert(*id, value);
    }
}

pub fn default_functions() -> Vec<&'static str> {
    ["Option", "Some", "None", "Result", "Ok", "Err", "print"].into()
}
pub fn default_symbols() -> Vec<&'static str> {
    default_functions()
}

pub fn run(
    functions: &[&'static str],
    symbols: HashMap<Span, u32>,
    exprs: &[Spanned<Expr>],
) -> Value {
    let mut env = Env {
        symbols,
        values: HashMap::new(),
    };
    for (i, name) in functions.iter().enumerate() {
        env.values.insert((i + 1) as u32, Value::Func(name));
    }
    log!("{env:?}");
    let mut last = None;
    for expr in exprs {
        let val = eval_expr(&mut env, expr);
        last = Some(val);
    }
    last.unwrap_or(Value::Unit)
}

fn eval_block(env: &mut Env, block: &Block) -> Value {
    for expr in &block.exprs {
        eval_expr(env, expr);
    }
    if let Some(value) = &block.value {
        eval_expr(env, value)
    } else {
        Value::Unit
    }
}

fn eval_expr(env: &mut Env, expr: &Spanned<Expr>) -> Value {
    match &expr.data {
        Expr::True => Value::True,
        Expr::False => Value::False,
        Expr::Integer(val) => Value::Integer(*val),
        Expr::Identifier(_) => env.get(expr.span),
        Expr::Path(_) => unimplemented!(),
        Expr::Block(block) => eval_block(env, block),
        Expr::Array { args } | Expr::Tuple { args } => {
            let args = args
                .iter()
                .map(|arg| eval_expr(env, arg))
                .collect::<Vec<_>>();
            Value::List(args)
        }
        Expr::Constructor { name, args } => {
            let args = args
                .iter()
                .map(|arg| eval_expr(env, arg))
                .collect::<Vec<_>>();
            match env.get(name.span) {
                Value::Func("print") => {
                    log!("CALL: {args:?}");
                    Value::Unit
                }
                _ => panic!(),
            }
        }
        Expr::Binding { expr, pattern } => {
            let val = eval_expr(env, expr);
            if let Some(res) = try_bind(env, val, pattern) {
                res
            } else {
                Value::False
            }
            // if !bound {
            //     panic!("binding fail");
            // }
            // Value::Unit
        }

        Expr::NamedConstructor { name, args } => todo!(),
        Expr::ThenFlow {
            condition,
            then_block,
        } => {
            if eval_expr(env, condition).truthy() {
                eval_block(env, then_block)
            } else {
                Value::False
            }
        }
        Expr::ElseFlow {
            condition,
            else_block,
        } => {
            if eval_expr(env, condition).truthy() {
                Value::True
            } else {
                eval_block(env, else_block)
            }
        }
        Expr::ThenElseFlow {
            condition,
            then_block,
            else_block,
        } => {
            if eval_expr(env, condition).truthy() {
                eval_block(env, then_block)
            } else {
                eval_block(env, else_block)
            }
        }
    }
}

fn try_bind(env: &mut Env, val: Value, pat: &Spanned<Pattern>) -> Option<Value> {
    match (val, &pat.data) {
        (_, Pattern::Wildcard) => Some(Value::Unit),
        (val, Pattern::Identifier(_)) => {
            env.set(pat.span, val);
            Some(Value::Unit)
        }

        (Value::Unit, Pattern::Tuple { patterns }) if patterns.is_empty() => Some(Value::Unit),
        (Value::True, Pattern::True) => Some(Value::Unit),
        (Value::False, Pattern::False) => Some(Value::Unit),
        (Value::Integer(val), Pattern::Integer(pat)) if val == *pat => Some(Value::Unit),

        (Value::List(values), Pattern::Array { patterns })
        | (Value::List(values), Pattern::Tuple { patterns })
            if values.len() == patterns.len() =>
        {
            values
                .into_iter()
                .zip(patterns.iter())
                .all(|(val, pat)| try_bind(env, val, pat).is_some())
                .then_some(Value::Unit)
        }

        (val, Pattern::Alternatives { patterns }) => {
            for pat in patterns {
                if try_bind(env, val.clone(), pat).is_some() {
                    return Some(Value::Unit);
                }
            }
            None
        }
        (val, Pattern::MatchArms { arms }) => {
            for (pat, expr) in arms {
                if try_bind(env, val.clone(), pat).is_some() {
                    return Some(eval_block(env, expr));
                }
            }
            None
        }

        _ => None,
    }
}
