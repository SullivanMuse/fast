use crate::span::Span;

pub(crate) type Input<'a> = Span<&'a str>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Arm<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) pattern: Pattern<'a>,
    pub(crate) expr: Expr<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Ellipsis<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) id: Option<Input<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Assign<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) pattern: Pattern<'a>,
    pub(crate) expr: Expr<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement<'a> {
    Expr(Expr<'a>),
    Assign(Assign<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct App<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) inner: Box<Expr<'a>>,
    pub(crate) arg_span: Input<'a>,
    pub(crate) args: Vec<Expr<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Case<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) subject: Box<Expr<'a>>,
    pub(crate) arms: Vec<Arm<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Do<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) statements: Vec<Statement<'a>>,
    pub(crate) ret: Option<Box<Expr<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr<'a> {
    Int(Input<'a>),
    Tag(Input<'a>, Input<'a>),
    Id(Input<'a>),
    Expand(Ellipsis<'a>),
    Tuple(Input<'a>, Vec<Expr<'a>>),
    App(App<'a>),
    Case(Case<'a>),
    Paren(Input<'a>, Box<Expr<'a>>),
    Do(Do<'a>),
    Fn(Input<'a>, Input<'a>, Box<Expr<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct PatternApp<'a> {
    pub(crate) span: Input<'a>,
    pub(crate) f: Box<Pattern<'a>>,
    pub(crate) arg_span: Input<'a>,
    pub(crate) xs: Vec<Pattern<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Pattern<'a> {
    Id(Input<'a>),
    Ignore(Input<'a>),
    Int(Input<'a>),
    Tag(Input<'a>, Input<'a>),
    Collect(Ellipsis<'a>),
    Tuple(Input<'a>, Vec<Pattern<'a>>),
    App(PatternApp<'a>),
    Paren(Input<'a>, Box<Pattern<'a>>),
}
