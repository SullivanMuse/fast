use crate::expr::{App, Arm, Assign, Case, Do, Expr, Input, Statement};
use crate::parse_common::{parse_ellipsis, parse_id, parse_int, parse_tag};
use crate::parse_pattern::pattern;
use crate::span::Span;

use nom::combinator::consumed;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

/// parse an int expression
fn eint(s: Input) -> IResult<Input, Expr> {
    map(parse_int, Expr::Int)(s)
}

/// parse a tag expression
fn etag(s: Input) -> IResult<Input, Expr> {
    map(parse_tag, |(span1, span2)| Expr::Tag(span1, span2))(s)
}

/// parse an id
fn eid(s: Input) -> IResult<Input, Expr> {
    map(parse_id, Expr::Id)(s)
}

/// parse an atomic expression
fn eatom(s: Input) -> IResult<Input, Expr> {
    alt((eunit, eid, etag, eint, eparen))(s)
}

/// parse a list item
fn eitem(s: Input) -> IResult<Input, Expr> {
    alt((map(parse_ellipsis, Expr::Expand), eother))(s)
}

/// parse an application expression
fn eapp(s: Input) -> IResult<Input, Expr> {
    /// '(' ws (eitem ws ',' ws)* eitem? ws ')'
    fn args(s: Input) -> IResult<Input, (Input, Vec<Expr>)> {
        let (s1, args) = delimited(
            pair(tag("("), multispace0),
            map(
                pair(
                    many0(terminated(
                        eitem,
                        tuple((multispace0, tag(","), multispace0)),
                    )),
                    opt(eitem),
                ),
                |(mut xs, x)| {
                    if let Some(x) = x {
                        xs.push(x);
                    }
                    xs
                },
            ),
            pair(multispace0, tag(")")),
        )(s)?;
        let span = Span::between(s, s1);
        Ok((s1, (span, args)))
    }

    let (s1, (mut f, xs)) = pair(eatom, many0(preceded(multispace0, args)))(s)?;
    for (arg_span, args) in xs {
        let span = Span::to(s, arg_span);
        let inner = Box::new(f);
        f = Expr::App(App {
            span,
            inner,
            arg_span,
            args,
        });
    }
    Ok((s1, f))
}

/// parse a unit expression
/// eunit = '(' ')'
fn eunit(s: Input) -> IResult<Input, Expr> {
    let (s1, _) = tuple((tag("("), multispace0, tag(")")))(s)?;
    Ok((s1, Expr::Tuple(Span::between(s, s1), vec![])))
}

/// parse a tuple expression
/// etuple = (eitem ',')+ eitem?
fn etuple(s: Input) -> IResult<Input, Expr> {
    let (s1, (mut xs, x)) = pair(
        many1(terminated(
            eitem,
            tuple((multispace0, tag(","), multispace0)),
        )),
        opt(preceded(multispace0, eitem)),
    )(s)?;
    if let Some(x) = x {
        xs.push(x);
    }
    let span = Span::between(s, s1);
    Ok((s1, Expr::Tuple(span, xs)))
}

/// parse an arm of a case expression
/// arm = 'of' pattern '=' expr
fn arm(s: Input) -> IResult<Input, Arm> {
    let (s1, (pattern, expr)) = pair(
        preceded(terminated(tag("of"), multispace0), pattern),
        preceded(tuple((multispace0, tag("="), multispace0)), expr),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((
        s1,
        Arm {
            span,
            pattern,
            expr,
        },
    ))
}

/// parse a case expression
/// case = 'case' expr arm* 'end'
fn ecase(s: Input) -> IResult<Input, Expr> {
    let (s1, (subject, arms)) = pair(
        preceded(pair(tag("case"), multispace0), expr),
        terminated(
            many0(preceded(multispace0, arm)),
            pair(multispace0, tag("end")),
        ),
    )(s)?;
    let span = Span::between(s, s1);
    let subject = Box::new(subject);
    Ok((
        s1,
        Expr::Case(Case {
            span,
            subject,
            arms,
        }),
    ))
}

/// parse an assignment statement
/// assign = pattern '=' expr
fn assign(s: Input) -> IResult<Input, Statement> {
    let (s1, (pattern, expr)) = pair(
        pattern,
        preceded(tuple((multispace0, tag("="), multispace0)), expr),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((
        s1,
        Statement::Assign(Assign {
            span,
            pattern,
            expr,
        }),
    ))
}

/// parse a statement
/// statement = assign | expr
fn statement(s: Input) -> IResult<Input, Statement> {
    alt((assign, map(expr, Statement::Expr)))(s)
}

/// parse a do expression
/// do = '{' statement[;] expr '}'
fn edo(s: Input) -> IResult<Input, Expr> {
    let (s1, (statements, ret)) = delimited(
        pair(tag("{"), multispace0),
        pair(
            many0(terminated(
                statement,
                tuple((multispace0, tag(";"), multispace0)),
            )),
            opt(map(expr, Box::new)),
        ),
        pair(multispace0, tag("}")),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((
        s1,
        Expr::Do(Do {
            span,
            statements,
            ret,
        }),
    ))
}

/// parse a paren expression
/// paren = '(' expr ')'
fn eparen(s: Input) -> IResult<Input, Expr> {
    let (s1, inner) = delimited(
        pair(tag("("), multispace0),
        expr,
        pair(multispace0, tag(")")),
    )(s)?;
    let span = Span::between(s, s1);
    let expr = Expr::Paren(span, Box::new(inner));
    Ok((s1, expr))
}

/// parse an anonymous function
/// fn = param fn | param ws '->' ws expr
fn efn(s: Input) -> IResult<Input, Expr> {
    map(
        consumed(alt((
            pair(parse_id, preceded(multispace0, map(efn, Box::new))),
            pair(
                parse_id,
                preceded(
                    tuple((multispace0, tag("->"), multispace0)),
                    map(expr, Box::new),
                ),
            ),
        ))),
        |(span, (param, body))| Expr::Fn(span, param, body),
    )(s)
}

fn eother(s: Input) -> IResult<Input, Expr> {
    alt((eapp, ecase, edo))(s)
}

/// parse an expression
/// expr = fn | tuple | app | case | do
pub(crate) fn expr(s: Input) -> IResult<Input, Expr> {
    alt((efn, etuple, eother))(s)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::Pattern;

    macro_rules! assert_err {
        ($e: expr) => {
            if let Ok(_) = $e {
                assert!(false);
            }
        };
    }

    #[test]
    fn test_eunit() {
        // eparen
        let s = "()";
        let span = Span::from(s);
        assert_eq!(
            eunit(span),
            Ok((Span::new(s, 2, 2), Expr::Tuple(span, vec![]))),
        );

        let s = "(   )";
        let span = Span::from(s);
        assert_eq!(
            eunit(span),
            Ok((Span::new(s, s.len(), s.len()), Expr::Tuple(span, vec![]))),
        );

        assert_err!(eunit(Span::from(" ()")));
    }

    #[test]
    fn test_eint() {
        let s = "1234_5678";
        let span = Span::from(s);
        assert_eq!(
            eint(span),
            Ok((Span::new(s, s.len(), s.len()), Expr::Int(span))),
        );

        assert_err!(eint(Span::from(" 1234")));
    }

    #[test]
    fn test_etag() {
        let s = ": xyz";
        let span = Span::from(s);
        assert_eq!(
            etag(span),
            Ok((
                Span::new(s, s.len(), s.len()),
                Expr::Tag(span, Span::new(s, 2, s.len()))
            )),
        );

        let s = " : xyz";
        assert_err!(etag(Span::from(s)));
    }

    #[test]
    fn test_eid() {
        assert_eq!(
            eid("xyz".into()),
            Ok((Span::new("xyz", 3, 3), Expr::Id(Span::new("xyz", 0, 3)))),
        );

        assert_err!(eid("   xyz".into()));
    }

    #[test]
    fn test_eparen() {
        let s = "(  1234)";
        let span = Span::from(s);
        let expr = Expr::Paren(span, Box::new(Expr::Int(Span::new(s, 3, 7))));
        assert_eq!(eparen(span), Ok((Span::new(s, s.len(), s.len()), expr)),);

        assert_err!(eparen(Span::from("  (  1234)")));
    }

    #[test]
    fn test_efn() {
        let s = "x y z -> f(x, y)";
        let span = Span::from(s);
        let expr = Expr::Fn(
            Span::from(s),
            Span::new(s, 0, 1),
            Box::new(Expr::Fn(
                Span::new(s, 2, s.len()),
                Span::new(s, 2, 3),
                Box::new(Expr::Fn(
                    Span::new(s, 4, s.len()),
                    Span::new(s, 4, 5),
                    Box::new(Expr::App(App {
                        span: Span::new(s, 9, s.len()),
                        inner: Box::new(Expr::Id(Span::new(s, 9, 10))),
                        arg_span: Span::new(s, 10, s.len()),
                        args: vec![
                            Expr::Id(Span::new(s, 11, 12)),
                            Expr::Id(Span::new(s, 14, 15)),
                        ],
                    })),
                )),
            )),
        );
        assert_eq!(efn(span), Ok((Span::end(s), expr)),);
    }

    #[test]
    fn test_eapp() {
        let s = "f(x, y)(z)";
        let span = Span::from(s);
        assert_eq!(
            eapp(span),
            Ok((
                Span::end(s),
                Expr::App(App {
                    span: Span::from(s),
                    inner: Box::new(Expr::App(App {
                        span: Span::new(s, 0, 7),
                        inner: Box::new(Expr::Id(Span::new(s, 0, 1))),
                        arg_span: Span::new(s, 1, 7),
                        args: vec![Expr::Id(Span::new(s, 2, 3)), Expr::Id(Span::new(s, 5, 6)),],
                    })),
                    arg_span: Span::new(s, 7, 10),
                    args: vec![Expr::Id(Span::new(s, 8, 9)),],
                }),
            )),
        );
    }

    #[test]
    fn test_eatom() {
        let s = "1234";
        let expr = Expr::Int(Span::from(s));
        assert_eq!(eatom(Span::from(s)), Ok((Span::end(s), expr)),);
    }

    #[test]
    fn test_ecase() {
        let s = "case x of x = x end";
        assert_eq!(
            ecase(Span::from(s)),
            Ok((
                Span::end(s),
                Expr::Case(Case {
                    span: Span::new(s, 0, 19),
                    subject: Box::new(Expr::Id(Span::new(s, 5, 6))),
                    arms: vec![Arm {
                        span: Span::new(s, 7, 15),
                        pattern: Pattern::Id(Span::new(s, 10, 11)),
                        expr: Expr::Id(Span::new(s, 14, 15)),
                    },],
                }),
            )),
        );
    }

    #[test]
    fn test_etuple() {
        let s = "(1, 2, 3)";
        assert_eq!(
            expr(Span::from(s)),
            Ok((
                Span::end(s),
                Expr::Paren(
                    Span::from(s),
                    Box::new(Expr::Tuple(
                        Span::new(s, 1, 8),
                        vec![
                            Expr::Int(Span::new(s, 1, 2)),
                            Expr::Int(Span::new(s, 4, 5)),
                            Expr::Int(Span::new(s, 7, 8)),
                        ]
                    )),
                ),
            )),
        );
    }
}
