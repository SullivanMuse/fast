use crate::span::Span;

pub(crate) type Input<'a> = Span<&'a str>;

#[derive(Debug, PartialEq)]
pub(crate) struct Arm<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) pattern: Pattern<'a>,
    pub(crate) expr: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Ellipsis<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) id: Option<Input<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Statement<'a> {
    Expr(Expr<'a>),
    Assign {
        span: Input<'a>,
        pattern: Pattern<'a>,
        expr: Expr<'a>,
    },
}

#[derive(Debug, PartialEq)]
pub(crate) struct App<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) inner: Box<Expr<'a>>,
    pub(crate) arg_span: Input<'a>,
    pub(crate) args: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Case<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) subject: Box<Expr<'a>>,
    pub(crate) arms: Vec<Arm<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr<'a> {
    Int(Input<'a>),
    Tag(Input<'a>, Input<'a>),
    Id(Input<'a>),
    Expand(Ellipsis<'a>),
    Tuple(Input<'a>, Vec<Expr<'a>>),
    App(App<'a>),
    Case(Case<'a>),
    Paren(Input<'a>, Box<Expr<'a>>),
    Do {
        span: Input<'a>,
        statements: Vec<Statement<'a>>,
        ret: Option<Box<Expr<'a>>>,
    },
    Fn(Input<'a>, Input<'a>, Box<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Pattern<'a> {
    Id(Input<'a>),
    Ignore(Input<'a>),
    Int(Input<'a>),
    Tag(Input<'a>, Input<'a>),
    Collect(Ellipsis<'a>),
    Tuple(Input<'a>, Vec<Pattern<'a>>),
    App {
        span: Input<'a>,
        inner: Box<Pattern<'a>>,
        arg_span: Input<'a>,
        args: Vec<Pattern<'a>>,
    },
    Paren(Input<'a>, Box<Pattern<'a>>),
}
