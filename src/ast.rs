#[derive(Debug, Clone)]
pub struct Path {
    pub name: String,
    pub parents: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum NamedExpr {
    Implicit(String),
    Explicit { name: String, expr: Expr },
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    Unit,
    Integer(u64),
    Identifier(String),
    Path(Path),

    // [ expr, expr, ... ]
    Array {
        args: Vec<Expr>,
    },
    // ( expr, expr, ... )
    Tuple {
        args: Vec<Expr>,
    },
    // name( expr, expr, ... )
    Constructor {
        name: Path,
        args: Vec<Expr>,
    },
    // name { name: expr, name: expr, ... }
    NamedConstructor {
        name: Path,
        args: Vec<NamedExpr>,
    },

    // expr -> pattern
    Binding {
        expr: Box<Expr>,
        pattern: Pattern,
    },

    // expr => { expr... }
    ThenFlow {
        condition: Box<Expr>,
        then_block: Block,
    },

    // expr else { expr... }
    ElseFlow {
        condition: Box<Expr>,
        else_block: Block,
    },

    // expr => { expr... } else { expr... }
    ThenElseFlow {
        condition: Box<Expr>,
        then_block: Block,
        else_block: Block,
    },
}

#[derive(Debug, Clone)]
pub enum NamedPattern {
    Implicit(String),
    Explicit { name: String, pattern: Pattern },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Integer(u64),
    Identifier(String),
    Path(Path),

    // [ pat, pat, ... ]
    Array {
        patterns: Vec<Pattern>,
    },
    // ( pat, pat, ... )
    Tuple {
        patterns: Vec<Pattern>,
    },
    // name( pat, pat, ... )
    Constructor {
        name: Path,
        patterns: Vec<Pattern>,
    },
    // name { name: pat, name: pat, ... }
    NamedConstructor {
        name: Path,
        patterns: Vec<NamedPattern>,
    },

    // { pat, pat, ... }
    Alternatives {
        patterns: Vec<Pattern>,
    },
    // { pat => { expr... }, pat => { expr... }, ... }
    MatchArms {
        arms: Vec<(Pattern, Block)>,
    },
}
