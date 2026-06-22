use std::collections::HashMap;
use std::fmt;

use crate::ast::{Block, Expr, Pattern};
use crate::log;
use crate::source::{Source, Span, Spanned};

#[derive(Debug)]
pub enum Error {
    Custom { message: String, span: Span },
    Exit { span: Span },
}
impl Error {
    pub fn pretty_print<W: fmt::Write>(&self, source: &Source, out: &mut W) -> fmt::Result {
        match self {
            Self::Exit { span } => {
                out.write_str("exit")?;
                out.write_char('\n')?;
                source.print_span(*span, out)
            }
            Self::Custom { message, span } => {
                out.write_str(message)?;
                out.write_char('\n')?;
                source.print_span(*span, out)
            }
        }
    }
}

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
    fn get(&self, span: Span) -> Result<Value, Error> {
        let Some(id) = self.symbols.get(&span) else {
            return Err(Error::Custom {
                message: "bug: this was not parsed as an identifier".into(),
                span,
            });
        };
        let Some(val) = self.values.get(id) else {
            return Err(Error::Custom {
                message: "error: tried to use this variable before it was initialized".into(),
                span,
            });
        };
        Ok(val.clone())
    }
    fn set(&mut self, span: Span, value: Value) -> Result<(), Error> {
        let Some(id) = self.symbols.get(&span) else {
            return Err(Error::Custom {
                message: "error: tried to use this variable before it was initialized".into(),
                span,
            });
        };
        self.values.insert(*id, value);
        Ok(())
    }
}

pub fn default_functions() -> Vec<&'static str> {
    [
        "Option", "Some", "None", "Result", "Ok", "Err", "print", "exit",
    ]
    .into()
}
pub fn default_symbols() -> Vec<&'static str> {
    default_functions()
}

pub fn run(
    functions: &[&'static str],
    symbols: HashMap<Span, u32>,
    exprs: &[Spanned<Expr>],
) -> Result<Value, Error> {
    let mut env = Env {
        symbols,
        values: HashMap::new(),
    };
    for (i, name) in functions.iter().enumerate() {
        env.values.insert((i + 1) as u32, Value::Func(name));
    }
    let mut last = None;
    for expr in exprs {
        let val = eval_expr(&mut env, expr)?;
        last = Some(val);
    }
    Ok(last.unwrap_or(Value::Unit))
}

fn eval_block(env: &mut Env, block: &Block) -> Result<Value, Error> {
    for expr in &block.exprs {
        eval_expr(env, expr)?;
    }
    if let Some(value) = &block.value {
        eval_expr(env, value)
    } else {
        Ok(Value::Unit)
    }
}

fn eval_expr(env: &mut Env, expr: &Spanned<Expr>) -> Result<Value, Error> {
    match &expr.data {
        Expr::True => Ok(Value::True),
        Expr::False => Ok(Value::False),
        Expr::Integer(val) => Ok(Value::Integer(*val)),
        Expr::Identifier(_) => env.get(expr.span),
        Expr::Path(_) => Err(Error::Custom {
            message: "unimplemented: path".into(),
            span: expr.span,
        }),
        Expr::Block(block) => eval_block(env, block),
        Expr::Array { args } | Expr::Tuple { args } => args
            .iter()
            .map(|arg| eval_expr(env, arg))
            .collect::<Result<Vec<_>, Error>>()
            .map(Value::List),
        Expr::Constructor { name, args } => {
            let args = args
                .iter()
                .map(|arg| eval_expr(env, arg))
                .collect::<Result<Vec<_>, Error>>()?;
            log!("CALL: {args:?}");
            match env.get(name.span)? {
                Value::Func("exit") => Err(Error::Exit { span: name.span }),
                Value::Func("print") => Ok(Value::Unit),
                _ => Err(Error::Custom {
                    message: "unknown/unimplemented function".into(),
                    span: name.span,
                }),
            }
        }
        Expr::Binding { expr, pattern } => {
            let val = eval_expr(env, expr)?;
            if let Some(res) = try_bind(env, val, pattern)? {
                Ok(res)
            } else {
                Ok(Value::False)
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
            if eval_expr(env, condition)?.truthy() {
                eval_block(env, then_block)
            } else {
                Ok(Value::False)
            }
        }
        Expr::ElseFlow {
            condition,
            else_block,
        } => {
            if eval_expr(env, condition)?.truthy() {
                Ok(Value::True)
            } else {
                eval_block(env, else_block)
            }
        }
        Expr::ThenElseFlow {
            condition,
            then_block,
            else_block,
        } => {
            if eval_expr(env, condition)?.truthy() {
                eval_block(env, then_block)
            } else {
                eval_block(env, else_block)
            }
        }
    }
}

fn try_bind(env: &mut Env, val: Value, pat: &Spanned<Pattern>) -> Result<Option<Value>, Error> {
    match (val, &pat.data) {
        (_, Pattern::Wildcard) => Ok(Some(Value::Unit)),
        (val, Pattern::Identifier(_)) => {
            env.set(pat.span, val)?;
            Ok(Some(Value::Unit))
        }

        (Value::Unit, Pattern::Tuple { patterns }) if patterns.is_empty() => Ok(Some(Value::Unit)),
        (Value::True, Pattern::True) => Ok(Some(Value::Unit)),
        (Value::False, Pattern::False) => Ok(Some(Value::Unit)),
        (Value::Integer(val), Pattern::Integer(pat)) if val == *pat => Ok(Some(Value::Unit)),

        (Value::List(values), Pattern::Array { patterns })
        | (Value::List(values), Pattern::Tuple { patterns })
            if values.len() == patterns.len() =>
        {
            for (val, pat) in values.into_iter().zip(patterns.iter()) {
                if try_bind(env, val, pat)?.is_none() {
                    return Ok(None);
                }
            }
            Ok(Some(Value::Unit))
            // values
            //     .into_iter()
            //     .zip(patterns.iter())
            //     .all(|(val, pat)| try_bind(env, val, pat).is_some())
            //     .then_some(Value::Unit)
        }

        (val, Pattern::Alternatives { patterns }) => {
            for pat in patterns {
                if try_bind(env, val.clone(), pat)?.is_some() {
                    return Ok(Some(Value::Unit));
                }
            }
            Ok(None)
        }
        (val, Pattern::MatchArms { arms }) => {
            for (pat, expr) in arms {
                if try_bind(env, val.clone(), pat)?.is_some() {
                    return eval_block(env, expr).map(Some);
                }
            }
            Ok(None)
        }

        _ => Ok(None),
    }
}
