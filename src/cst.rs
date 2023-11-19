use crate::context::{ExprIx, Loc, StatementIx, Symbol};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Do {
    pub(crate) statements: Vec<StatementIx>,
    pub(crate) ret: Option<StatementIx>,
}

impl Do {
    pub fn new(statements: Vec<StatementIx>, ret: Option<StatementIx>) -> Self {
        Self { statements, ret }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExprTy {
    // Literals
    Unit, // ()
    Int,  // 9

    // Basic
    Id(Symbol), // x
    Do(Do),     // x = y; x

    // Abstraction
    Call(ExprIx, Vec<ExprIx>), // f x y
    Fn(Symbol, ExprIx),        // x -> ...

    // Quote/Eval
    Quote(ExprIx), // :x
    Eval(ExprIx),  // eval x

    // Parenthesized
    Paren(ExprIx), // ( e )
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Expr {
    pub(crate) loc: Loc,
    pub(crate) ty: ExprTy,
}

impl Expr {
    pub fn new(loc: Loc, ty: ExprTy) -> Self {
        Self { loc, ty }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StatementTy {
    Assign(Symbol, ExprIx), // x = e
    Expr(ExprIx),           // e
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Statement {
    pub(crate) loc: Loc,
    pub(crate) ty: StatementTy,
}

impl Statement {
    pub fn new(loc: Loc, ty: StatementTy) -> Self {
        Self { loc, ty }
    }
}
