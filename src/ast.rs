use std::fmt;

use crate::source::Spanned;

#[derive(Clone)]
pub struct Path {
    pub name: String,
    pub parents: Vec<String>,
}

impl Path {
    pub fn top(&self) -> &str {
        if let Some(top) = self.parents.first() {
            return top;
        }
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
    pub value: Option<Box<Spanned<Expr>>>,
}

#[derive(Clone)]
pub enum NamedExpr {
    Implicit(String),
    Explicit { name: String, expr: Spanned<Expr> },
}

#[derive(Clone)]
pub enum Expr {
    // Literals
    True,
    False,
    Integer(u64),
    Identifier(String),
    Path(Path),

    Block(Block),

    // [ expr, expr, ... ]
    Array {
        args: Vec<Spanned<Expr>>,
    },
    // ( expr, expr, ... )
    Tuple {
        args: Vec<Spanned<Expr>>,
    },
    // name( expr, expr, ... )
    Constructor {
        name: Spanned<Path>,
        args: Vec<Spanned<Expr>>,
    },
    // name { name: expr, name: expr, ... }
    NamedConstructor {
        name: Spanned<Path>,
        args: Vec<Spanned<NamedExpr>>,
    },

    // expr -> pattern
    Binding {
        expr: Box<Spanned<Expr>>,
        pattern: Spanned<Pattern>,
    },

    // expr => { expr... }
    ThenFlow {
        condition: Box<Spanned<Expr>>,
        then_block: Spanned<Block>,
    },

    // expr else { expr... }
    ElseFlow {
        condition: Box<Spanned<Expr>>,
        else_block: Spanned<Block>,
    },

    // expr => { expr... } else { expr... }
    ThenElseFlow {
        condition: Box<Spanned<Expr>>,
        then_block: Spanned<Block>,
        else_block: Spanned<Block>,
    },
}

#[derive(Clone)]
pub enum NamedPattern {
    Implicit(String),
    Explicit {
        name: String,
        pattern: Spanned<Pattern>,
    },
}

#[derive(Clone)]
pub enum Pattern {
    // Literals
    True,
    False,
    Integer(u64),
    Identifier(String),
    Path(Path),
    Wildcard,

    // [ pat, pat, ... ]
    Array {
        patterns: Vec<Spanned<Pattern>>,
    },
    // ( pat, pat, ... )
    Tuple {
        patterns: Vec<Spanned<Pattern>>,
    },
    // name( pat, pat, ... )
    Constructor {
        name: Spanned<Path>,
        patterns: Vec<Spanned<Pattern>>,
    },
    // name { name: pat, name: pat, ... }
    NamedConstructor {
        name: Spanned<Path>,
        patterns: Vec<Spanned<NamedPattern>>,
    },

    // { pat, pat, ... }
    Alternatives {
        patterns: Vec<Spanned<Pattern>>,
    },
    // { pat => { expr... }, pat => { expr... }, ... }
    MatchArms {
        arms: Vec<(Spanned<Pattern>, Spanned<Block>)>,
    },
}

// ---------------------------------------------------------------------------
// Labels
// ---------------------------------------------------------------------------

impl Expr {
    pub fn label(&self) -> &'static str {
        match self {
            Self::True => "True",
            Self::False => "False",
            Self::Integer(_) => "Integer",
            Self::Identifier(_) => "Identifier",
            Self::Path(_) => "Path",
            Self::Block(_) => "Block",
            Self::Array { .. } => "Array",
            Self::Tuple { .. } => "Tuple",
            Self::Constructor { .. } => "Constructor",
            Self::NamedConstructor { .. } => "NamedConstructor",
            Self::Binding { .. } => "Binding",
            Self::ThenFlow { .. } => "ThenFlow",
            Self::ElseFlow { .. } => "ElseFlow",
            Self::ThenElseFlow { .. } => "ThenElseFlow",
        }
    }
}

impl Pattern {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Wildcard => "Wildcard",
            Self::True => "TruePat",
            Self::False => "FalsePat",
            Self::Integer(_) => "IntegerPat",
            Self::Identifier(_) => "IdentifierPat",
            Self::Path(_) => "PathPat",
            Self::Array { .. } => "ArrayPat",
            Self::Tuple { .. } => "TuplePat",
            Self::Constructor { .. } => "ConstructorPat",
            Self::NamedConstructor { .. } => "NamedConstructorPat",
            Self::Alternatives { .. } => "Alternatives",
            Self::MatchArms { .. } => "MatchArms",
        }
    }
}

// ---------------------------------------------------------------------------
// Slightly more compact Debug
// ---------------------------------------------------------------------------

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Path(\"")?;
        for p in self.parents.iter() {
            f.write_str(p)?;
            f.write_str("\", \"")?;
        }
        f.write_str(&self.name)?;
        f.write_str("\")")
    }
}

impl fmt::Debug for NamedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit(x) => f.write_fmt(format_args!("Implicit({x})")),
            Self::Explicit { name, expr } => f
                .debug_struct("Explicit")
                .field("name", name)
                .field("expr", expr)
                .finish(),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => f.write_str("True"),
            Self::False => f.write_str("False"),
            Self::Integer(x) => f.write_fmt(format_args!("Integer({x})")),
            Self::Identifier(x) => f.write_fmt(format_args!("Identifier({x:?})")),
            Self::Path(x) => x.fmt(f),
            Self::Block(x) => x.fmt(f),
            Self::Array { args } => {
                let mut t = f.debug_tuple("Array");
                for arg in args {
                    t.field(arg);
                }
                t.finish()
            }
            Self::Tuple { args } => {
                let mut t = f.debug_tuple("Tuple");
                for arg in args {
                    t.field(arg);
                }
                t.finish()
            }
            Self::Constructor { name, args } => f
                .debug_struct("Constructor")
                .field("name", name)
                .field("args", args)
                .finish(),
            Self::NamedConstructor { name, args } => f
                .debug_struct("NamedConstructor")
                .field("name", name)
                .field("args", args)
                .finish(),
            Self::Binding { expr, pattern } => f
                .debug_struct("Binding")
                .field("expr", expr)
                .field("pattern", pattern)
                .finish(),
            Self::ThenFlow {
                condition,
                then_block,
            } => f
                .debug_struct("ThenFlow")
                .field("condition", condition)
                .field("then_block", then_block)
                .finish(),
            Self::ElseFlow {
                condition,
                else_block,
            } => f
                .debug_struct("ElseFlow")
                .field("condition", condition)
                .field("else_block", else_block)
                .finish(),
            Self::ThenElseFlow {
                condition,
                then_block,
                else_block,
            } => f
                .debug_struct("ThenElseFlow")
                .field("condition", condition)
                .field("then_block", then_block)
                .field("else_block", else_block)
                .finish(),
        }
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => f.write_str("True"),
            Self::False => f.write_str("False"),
            Self::Integer(x) => f.write_fmt(format_args!("Integer({x})")),
            Self::Identifier(x) => f.write_fmt(format_args!("Identifier({x:?})")),
            Self::Path(x) => x.fmt(f),
            Self::Wildcard => f.write_str("_"),
            Self::Array { patterns } => {
                let mut t = f.debug_tuple("Array");
                for pat in patterns {
                    t.field(pat);
                }
                t.finish()
            }
            Self::Tuple { patterns } => {
                let mut t = f.debug_tuple("Tuple");
                for pat in patterns {
                    t.field(pat);
                }
                t.finish()
            }
            Self::Constructor { name, patterns } => f
                .debug_struct("Constructor")
                .field("name", name)
                .field("patterns", patterns)
                .finish(),
            Self::NamedConstructor { name, patterns } => f
                .debug_struct("NamedConstructor")
                .field("name", name)
                .field("patterns", patterns)
                .finish(),
            Self::Alternatives { patterns } => f
                .debug_struct("Alternatives")
                .field("patterns", patterns)
                .finish(),
            Self::MatchArms { arms } => {
                let mut t = f.debug_tuple("MatchArms");
                for arm in arms {
                    t.field(arm);
                }
                t.finish()
            }
        }
    }
}

impl fmt::Debug for NamedPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit(x) => f.write_fmt(format_args!("Implicit({x})")),
            Self::Explicit { name, pattern } => f
                .debug_struct("Explicit")
                .field("name", name)
                .field("pattern", pattern)
                .finish(),
        }
    }
}
