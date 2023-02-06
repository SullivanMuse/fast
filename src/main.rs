#![allow(dead_code)]

mod parse;
mod span;

use span::Span;

#[derive(Clone, Debug)]
enum Pattern<'a> {
    Ignore(Span<'a>),
    Ellipsis(Span<'a>, Option<Box<Pattern<'a>>>),
    Tuple(Span<'a>, Vec<Pattern<'a>>),
    Bind(Span<'a>),
    Tag(Span<'a>),
    Paren(Span<'a>, Box<Pattern<'a>>),
}

#[derive(Clone, Debug)]
struct Arm<'a> {
    span: Span<'a>,
    pattern: Pattern<'a>,
    body: Expr<'a>,
}

#[derive(Clone, Debug)]
enum Expr<'a> {
    Name(Span<'a>),
    Tag(Span<'a>),
    Paren(Span<'a>, Box<Expr<'a>>),
    Int(Span<'a>),
    Tuple(Span<'a>, Vec<Expr<'a>>),
    Case {
        span: Span<'a>,
        subject: Box<Expr<'a>>,
        arms: Vec<Arm<'a>>,
    },
}

fn input() -> String {
    let mut s = String::new();
    std::io::stdin().read_line(&mut s).unwrap();
    s
}

fn main() {
    use crate::parse::parse_atom;

    loop {
        let s = input();
        println!("{:?}", parse_atom((&s[..]).into()).unwrap().1);
    }
}
