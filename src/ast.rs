use crate::source::Spanned;

#[derive(Debug, Clone)]
pub struct Path {
    pub name: String,
    pub parents: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
    pub value: Option<Box<Spanned<Expr>>>,
}

#[derive(Debug, Clone)]
pub enum NamedExpr {
    Implicit(String),
    Explicit { name: String, expr: Spanned<Expr> },
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    True,
    False,
    Integer(u64),
    Identifier(String),
    Path(Path),

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

#[derive(Debug, Clone)]
pub enum NamedPattern {
    Implicit(String),
    Explicit {
        name: String,
        pattern: Spanned<Pattern>,
    },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    // Literals
    True,
    False,
    Wildcard,
    Integer(u64),
    Identifier(String),
    Path(Path),

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
