use std::{collections::HashMap, ops::Range, path::PathBuf};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, space0},
    combinator::recognize,
    multi::many0,
    sequence::{delimited, pair, tuple}, IResult,
};
use string_interner::{symbol::SymbolU32, StringInterner};
use trace::trace;

use crate::cst::{Do, Expr, ExprTy, Statement, StatementTy};

trace::init_depth_var!();

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Loc {
    source: SourceIx,
    start: usize,
    end: usize,
}

impl Loc {
    pub fn new(source: SourceIx, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    pub fn token(&self, rem: &str) -> Self {
        let source = self.source;
        let start = self.start;
        let end = self.start + rem.len();
        Self { source, start, end }
    }
    
    pub fn remaining(&self, rem: &str) -> Self {
        let source = self.source;
        let start = self.start + rem.len();
        let end = self.end;
        Self { source, start, end }
    }

    pub fn to(&self, other: Self) -> Self {
        assert_eq!(
            self.source, other.source,
            "Attempt to compare Locs with different sources"
        );
        Self::new(self.source, self.start, other.start)
    }
}

impl From<Loc> for Range<usize> {
    fn from(location: Loc) -> Self {
        location.start..location.end
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SourceIx(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExprIx(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StatementIx(usize);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Context {
    store: StringInterner,
    files: HashMap<PathBuf, SourceIx>,
    sources: Vec<(String, (Vec<StatementIx>, Option<StatementIx>))>,

    // CST Nodes
    exprs: Vec<Expr>,
    statements: Vec<Statement>,
}

pub type Symbol = SymbolU32;

struct ContextSource<'a>(&'a Context, SourceIx, usize);

impl<'a> std::fmt::Debug for ContextSource<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.debug_source(self.1, f, self.2)
    }
}

fn convert<T, E>(res: IResult<&str, T, E>, loc: Loc) -> IResult<Loc, T, ()> {
    match res {
        IResult::Ok((s, x)) => Ok((loc.remaining(s), x)),
        IResult::Err(e) => Err(nom::Err::Error(())),
    }
}

type PResult<T> = IResult<Loc, T, ()>;

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    fn insert_source(&mut self, string: String) -> SourceIx {
        let ix = SourceIx(self.sources.len());
        self.sources.push((string, (Vec::new(), None)));
        let location = Loc::new(ix, 0, self.sources[ix.0].0.len());
        self.sources[ix.0].1 = self.statements(location).expect("Parse error").1;
        ix
    }

    pub fn location(&self, location: Loc) -> &str {
        &(&self.sources[location.source.0].0)[Range::from(location)]
    }

    pub fn file(&mut self, path: &str) -> SourceIx {
        let path = PathBuf::from(path);
        match self.files.get(&path) {
            None => {
                let error = format!("Failed to open file: {:?}", &path);
                let string = std::fs::read_to_string(&path).expect(error.as_str());
                let ix = self.insert_source(string);
                self.files.insert(path, ix);
                ix
            }
            Some(source) => *source,
        }
    }

    pub fn debug(&self, source: SourceIx, depth: usize) {
        dbg!(ContextSource(self, source, depth));
    }

    fn debug_source(&self, source: SourceIx, fmt: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        if depth > 0 {
            let (statements, ret) = &self.sources[source.0].1;
            self.debug_statements(&statements, &ret, fmt, depth - 1)
        } else {
            std::fmt::Result::Ok(())
        }
    }

    fn debug_statements(
        &self,
        statements: &Vec<StatementIx>,
        ret: &Option<StatementIx>,
        fmt: &mut std::fmt::Formatter<'_>,
        depth: usize,
    ) -> std::fmt::Result {
        if depth > 0 {
            for statement in statements {
                self.debug_statement(*statement, fmt, depth - 1)?;
            }
            if let Some(ret) = ret {
                self.debug_statement(*ret, fmt, depth - 1)?
            }
        }
        std::fmt::Result::Ok(())
    }

    fn debug_statement(
        &self,
        statement: StatementIx,
        fmt: &mut std::fmt::Formatter<'_>,
        depth: usize,
    ) -> std::fmt::Result {
        if depth > 0 {
            match self.statements[statement.0].ty {
                StatementTy::Assign(sym, expr) => {
                    self.debug_sym(sym, fmt, depth - 1)?;
                    write!(fmt, " = ")?;
                    self.debug_expr(expr, fmt, depth - 1)
                }
                StatementTy::Expr(expr) => self.debug_expr(expr, fmt, depth - 1),
            }
        } else {
            std::fmt::Result::Ok(())
        }
    }

    fn debug_sym(&self, sym: Symbol, fmt: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        if depth > 0 {
            let s = self.store.resolve(sym).expect("Bad symbol");
            write!(fmt, "{}", s)
        } else {
            std::fmt::Result::Ok(())
        }
    }

    fn debug_expr(&self, expr: ExprIx, fmt: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        if depth > 0 {
            let expr = &self.exprs[expr.0];
            match &expr.ty {
                ExprTy::Unit => write!(fmt, "()"),
                ExprTy::Int => write!(fmt, "Int {:?}", self.location(expr.loc)),
                ExprTy::Id(_) => write!(fmt, "Id {:?}", self.location(expr.loc)),
                ExprTy::Do(inner) => {
                    write!(fmt, "{{")?;
                    self.debug_statements(&inner.statements, &inner.ret, fmt, depth - 1)?;
                    write!(fmt, "}}")
                }
                ExprTy::Call(f, xs) => {
                    write!(fmt, "(")?;
                    self.debug_expr(*f, fmt, depth - 1)?;
                    for x in xs {
                        self.debug_expr(*x, fmt, depth - 1)?;
                    }
                    write!(fmt, ")")
                }
                ExprTy::Fn(sym, inner) => {
                    self.debug_sym(*sym, fmt, depth - 1)?;
                    write!(fmt, " -> ")?;
                    self.debug_expr(*inner, fmt, depth - 1)
                }
                ExprTy::Quote(inner) => {
                    write!(fmt, ":")?;
                    self.debug_expr(*inner, fmt, depth - 1)
                }
                ExprTy::Eval(inner) => {
                    write!(fmt, "$")?;
                    self.debug_expr(*inner, fmt, depth - 1)
                }
                ExprTy::Paren(inner) => {
                    write!(fmt, "(")?;
                    self.debug_expr(*inner, fmt, depth - 1)?;
                    write!(fmt, ")")
                }
            }
        } else {
            std::fmt::Result::Ok(())
        }
    }

    #[trace]
    pub fn repl(&mut self) -> SourceIx {
        let mut string = String::new();
        std::io::stdin()
            .read_line(&mut string)
            .expect("Failed to read from stdin");
        self.insert_source(string)
    }

    #[trace]
    pub fn store(&mut self, location: Loc) -> Symbol {
        let string = self.location(location);
        // TODO: Find a way to eliminate the extra allocation
        self.store.get_or_intern(string.to_string())
    }

    #[trace]
    fn insert_expr(&mut self, expr: Expr) -> ExprIx {
        let ix = ExprIx(self.exprs.len());
        self.exprs.push(expr);
        ix
    }

    #[trace]
    fn insert_statement(&mut self, statement: Statement) -> StatementIx {
        let ix = StatementIx(self.statements.len());
        self.statements.push(statement);
        ix
    }

    /// id '=' expr
    /// TODO: test
    #[trace]
    fn assign(&mut self, loc: Loc) -> PResult<StatementIx> {
        let (rest, (loc, sym)) = self.id_inner(loc)?;
        let (rest, _) =
            convert(tuple((space0, tag("="), space0::<&str, ()>))(self.location(rest)), loc)?;
        let (rest, expr) = self.expr(rest)?;
        let statement = Statement::new(loc.to(rest), StatementTy::Assign(sym, expr));
        Ok((rest, self.insert_statement(statement)))
    }

    /// statement = assign | expr
    /// TODO: test
    #[trace]
    fn statement(&mut self, loc: Loc) -> PResult<StatementIx> {
        if let Ok(res) = self.assign(loc) {
            Ok(res)
        } else {
            let (rest, expr) = self.expr(loc)?;
            let statement = Statement::new(loc.to(rest), StatementTy::Expr(expr));
            Ok((rest, self.insert_statement(statement)))
        }
    }

    /// statements = (statement ';')* statement?
    /// TODO: test
    #[trace]
    pub fn statements(&mut self, loc: Loc) -> PResult<(Vec<StatementIx>, Option<StatementIx>)> {
        let mut rest = loc;
        let mut statements = vec![];
        let mut ret = None;
        while let Ok((rest1, statement)) = self.statement(rest) {
            if let Ok((rest1, _)) =
                convert(tuple((space0::<&str, ()>, tag(";"), space0))(self.location(rest1)), loc)
            {
                statements.push(statement);
                rest = rest1;
            } else {
                ret = Some(statement);
                rest = rest1;
                break;
            }
        }
        Ok((rest, (statements, ret)))
    }

    /// do = '{' statements '}'
    /// TODO: test
    #[trace]
    fn do_expr(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (rest, _) = convert(pair(tag("{"), space0::<&str, ()>)(self.location(loc)), loc)?;
        let (rest, (statements, ret)) = self.statements(rest)?;
        let (rest, _) = convert(pair(space0::<&str, ()>, tag("}"))(self.location(rest)), loc)?;
        Ok((
            rest,
            self.insert_expr(Expr::new(loc, ExprTy::Do(Do::new(statements, ret)))),
        ))
    }

    /// unit = '()'
    /// TODO: test
    #[trace]
    fn unit(&mut self, loc: Loc) -> PResult<ExprIx> {
        let s = self.location(loc);
        let (s1, inner) =
            recognize(delimited(tag("("), space0::<&str, ()>, tag(")")))(s).map_err(|_| ())?;
        let (loc, rest) = loc.split(inner);
        Ok((rest, self.insert_expr(Expr::new(loc, ExprTy::Unit))))
    }

    /// int = (digit1 '_')* digit1
    /// TODO: test
    #[trace]
    fn int(&mut self, loc: Loc) -> PResult<ExprIx> {
        let s = self.location(loc);
        let (s1, inner) = recognize(tuple((
            many0(tuple((digit1::<&str, ()>, tag("_")))),
            digit1,
        )))(s)
        .map_err(|_| ())?;
        let (loc, rest) = loc.split(inner);
        Ok((rest, self.insert_expr(Expr::new(loc, ExprTy::Int))))
    }

    #[trace]
    fn id_inner(&mut self, loc: Loc) -> PResult<(Loc, Symbol)> {
        let s = self.location(loc);
        let (s1, id) = recognize(tuple((
            alpha1::<&str, ()>,
            many0(alt((tag("_"), alphanumeric1))),
        )))(s)
        .map_err(|_| ())?;
        let (loc, rest) = loc.split(s1);
        let sym = self.store(loc);
        Ok((rest, (loc, sym)))
    }

    /// id = alpha ('_' | alnum1)*
    /// TODO: test
    #[trace]
    fn id(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (rest, (loc, sym)) = self.id_inner(loc)?;
        Ok((rest, self.insert_expr(Expr::new(loc, ExprTy::Id(sym)))))
    }

    /// quote = ':' atomic
    /// TODO: test
    #[trace]
    fn quote(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (rest, _) = pair(tag(":"), space0::<&str, ()>)(self.location(loc)).map_err(|_| ())?;
        let (rest, _) = loc.split(rest);
        let (rest, expr) = self.expr(rest)?;
        Ok((
            rest,
            self.insert_expr(Expr::new(loc.to(&rest), ExprTy::Quote(expr))),
        ))
    }

    /// eval = '$' atomic
    /// TODO: test
    #[trace]
    fn eval(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (rest, _) = pair(tag("$"), space0::<&str, ()>)(self.location(loc)).map_err(|_| ())?;
        let (rest, _) = loc.split(rest);
        let (rest, expr) = self.expr(rest)?;
        Ok((
            rest,
            self.insert_expr(Expr::new(loc.to(&rest), ExprTy::Eval(expr))),
        ))
    }

    /// atomic = unit | int | id | quote | eval | do | paren
    /// TODO: test
    #[trace]
    fn atomic(&mut self, loc: Loc) -> PResult<ExprIx> {
        self.unit(loc)
            .or_else(|_| self.int(loc))
            .or_else(|_| self.id(loc))
            .or_else(|_| self.quote(loc))
            .or_else(|_| self.eval(loc))
            .or_else(|_| self.do_expr(loc))
            .or_else(|_| self.paren(loc))
    }

    /// call = atomic+
    /// TODO: test
    #[trace]
    fn call(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (mut rest, f) = self.atomic(loc)?;
        let mut xs = vec![];
        while let Ok((rest1, x)) = self.atomic(loc) {
            xs.push(x);
            rest = rest1;
            if let Ok((rest1, _)) = space0::<&str, ()>(self.location(rest1)) {
                rest = loc.split(rest1).0;
            } else {
                break;
            }
        }
        Ok((
            rest,
            self.insert_expr(Expr::new(loc.to(&rest), ExprTy::Call(f, xs))),
        ))
    }

    /// expr = fn | call | atomic
    /// TODO: test
    #[trace]
    fn expr(&mut self, loc: Loc) -> PResult<ExprIx> {
        self.fn_expr(loc)
            .or_else(|_| self.call(loc))
            .or_else(|_| self.atomic(loc))
    }

    /// fn = id '->' expr
    /// TODO: test
    #[trace]
    fn fn_expr(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (rest, (_, sym)) = self.id_inner(loc)?;
        let (rest, _) =
            tuple((space0, tag("->"), space0::<&str, ()>))(self.location(rest)).map_err(|_| ())?;
        let (rest, _) = loc.split(rest);
        let (rest, expr) = self.expr(rest)?;
        Ok((
            rest,
            self.insert_expr(Expr::new(loc.to(&rest), ExprTy::Fn(sym, expr))),
        ))
    }

    /// paren = '(' expr ')'
    /// TODO: test
    #[trace]
    fn paren(&mut self, loc: Loc) -> PResult<ExprIx> {
        let (rest, _) = pair(tag("("), space0::<&str, ()>)(self.location(loc)).map_err(|_| ())?;
        let (rest, expr) = self.expr(loc.split(rest).0)?;
        let (rest, _) = pair(space0::<&str, ()>, tag(")"))(self.location(rest)).map_err(|_| ())?;
        let (rest, _) = loc.split(rest);
        Ok((
            rest,
            self.insert_expr(Expr::new(loc.to(&rest), ExprTy::Paren(expr))),
        ))
    }
}
