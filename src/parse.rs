use crate::expr::{Input, Expr, Pattern, Arm, Ellipsis, Statement};
use crate::span::Span;

use nom::{
    bytes::complete::tag,
    branch::alt,
    character::complete::{digit1, space0, alpha1, alphanumeric1},
    combinator::{cut, not, value, map, opt},
    IResult,
    multi::{many0, many1, separated_list0},
    sequence::{pair, tuple, preceded, delimited, terminated},
};

fn parse_int(s: Input) -> IResult<Input, Input> {
    let (s1, _) = tuple((
        digit1,
        many0(
            pair(
                tag("_"),
                digit1,
            )
        ),
        cut(
            not(
                pair(
                    space0,
                    tag("_"),
                )
            )
        ),
    ))(s)?;
    Ok((s1, Span::between(s, s1)))
}

fn parse_kw(s: Input) -> IResult<Input, ()> {
    value((), alt((
        tag("case"),
        tag("of"),
        tag("do"),
        tag("end"),
    )))(s)
}

fn parse_id(s: Input) -> IResult<Input, Input> {
    let (s1, _) = tuple((
        not(parse_kw),
        alpha1,
        many0(pair(tag("_"), alphanumeric1)),
    ))(s)?;
    Ok((s1, Span::between(s, s1)))
}

fn parse_tag(s: Input) -> IResult<Input, (Input, Input)> {
    let (s1, span) = preceded(
        pair(
            tag(":"),
            space0,
        ),
        parse_id,
    )(s)?;
    Ok((s1, (Span::between(s, s1), span)))
}

fn eint(s: Input) -> IResult<Input, Expr> {
    map(
        parse_int,
        Expr::Int,
    )(s)
}

fn etag(s: Input) -> IResult<Input, Expr> {
    map(
        parse_tag,
        |(span1, span2)| Expr::Tag(span1, span2),
    )(s)
}

fn eid(s: Input) -> IResult<Input, Expr> {
    map(
        parse_id,
        Expr::Id,
    )(s)
}

fn eatom(s: Input) -> IResult<Input, Expr> {
    alt((
        eunit,
        eid,
        etag,
        eint,
        eparen,
    ))(s)
}

fn parse_ellipsis(s: Input) -> IResult<Input, Ellipsis> {
    let (s1, id) = preceded(
        tag(".."),
        preceded(
            space0,
            opt(parse_id),
        ),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((s1, Ellipsis { span, id }))
}

fn eitem(s: Input) -> IResult<Input, Expr> {
    alt((
        map(
            parse_ellipsis,
            Expr::Expand,
        ),
        eother,
    ))(s)
}

fn eapp(s: Input) -> IResult<Input, Expr> {
    /// '(' ws (eitem ws ',' ws)* eitem? ws ')'
    fn args(s: Input) -> IResult<Input, (Input, Vec<Expr>)> {
        let (s1, args) = delimited(
            pair(tag("("), space0),
            map(
                pair(
                    many0(terminated(
                        eitem,
                        tuple((space0, tag(","), space0)),
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
            pair(space0, tag(")")),
        )(s)?;
        let span = Span::between(s, s1);
        Ok((s1, (span, args)))
    }
    
    let (s1, (mut f, xs)) = pair(
        eatom,
        many0(preceded(space0, args)),
    )(s)?;
    for (arg_span, args) in xs {
        let span = Span::to(s, arg_span);
        let inner = Box::new(f);
        f = Expr::App {
            span,
            inner,
            arg_span,
            args,
        };
    }
    Ok((s1, f))
}

/// eunit = '(' ')'
fn eunit(s: Input) -> IResult<Input, Expr> {
    let (s1, _) = tuple((
        tag("("),
        space0,
        tag(")"),
    ))(s)?;
    Ok((s1, Expr::Tuple(Span::between(s, s1), vec![])))
}

/// etuple = (eitem ',')+ eitem?
fn etuple(s: Input) -> IResult<Input, Expr> {
    let (s1, (mut xs, x)) = pair(
        many1(terminated(
            eitem,
            tuple((space0, tag(","), space0)),
        )),
        opt(preceded(space0, eitem)),
    )(s)?;
    if let Some(x) = x {
        xs.push(x);
    }
    let span = Span::between(s, s1);
    Ok((s1, Expr::Tuple(span, xs)))
}

fn arm(s: Input) -> IResult<Input, Arm> {
    let (s1, (pattern, expr)) = pair(
        preceded(
            terminated(tag("of"), space0),
            pattern,
        ),
        preceded(
            tuple((space0, tag("="), space0)),
            expr,
        ),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((s1, Arm { span, pattern, expr }))
}

fn ecase(s: Input) -> IResult<Input, Expr> {
    let (s1, (subject, arms)) = pair(
        preceded(
            pair(
                tag("case"),
                space0,
            ),
            expr,
        ),
        terminated(
            many0(preceded(
                space0,
                arm,
            )),
            pair(space0, tag("end")),
        ),
    )(s)?;
    let span = Span::between(s, s1);
    let subject = Box::new(subject);
    Ok((s1, Expr::Case { span, subject, arms }))
}

fn assign(s: Input) -> IResult<Input, Statement> {
    let (s1, (pattern, expr)) = pair(
        pattern,
        preceded(
            tuple((space0, tag("="), space0)),
            expr,
        ),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((s1, Statement::Assign { span, pattern, expr }))
}

fn statement(s: Input) -> IResult<Input, Statement> {
    alt((
        assign,
        map(expr, Statement::Expr),
    ))(s)
}

fn edo(s: Input) -> IResult<Input, Expr> {
    let (s1, (statements, ret)) = delimited(
        pair(tag("{"), space0),
        pair(
            many0(terminated(
                statement,
                tuple((space0, tag(";"), space0)),
            )),
            opt(map(expr, Box::new)),
        ),
        pair(space0, tag("}")),
    )(s)?;
    let span = Span::between(s, s1);
    Ok((s1, Expr::Do { span, statements, ret }))
}

fn eparen(s: Input) -> IResult<Input, Expr> {
    let (s1, inner) = delimited(
        pair(
            tag("("),
            space0,
        ),
        expr,
        pair(
            space0,
            tag(")"),
        ),
    )(s)?;
    let span = Span::between(s, s1);
    let expr = Expr::Paren(span, Box::new(inner));
    Ok((s1, expr))
}

fn eother(s: Input) -> IResult<Input, Expr> {
    alt((
        eapp,
        ecase,
        edo,
    ))(s)
}

pub(crate) fn expr(s: Input) -> IResult<Input, Expr> {
    alt((
        etuple,
        eother,
    ))(s)
}

fn pint(s: Input) -> IResult<Input, Pattern> {
    map(
        parse_int,
        Pattern::Int,
    )(s)
}

fn pid(s: Input) -> IResult<Input, Pattern> {
    map(
        parse_id,
        Pattern::Id,
    )(s)
}

fn ptag(s: Input) -> IResult<Input, Pattern> {
    map(
        parse_tag,
        |(span1, span2)| Pattern::Tag(span1, span2),
    )(s)
}

fn pignore(s: Input) -> IResult<Input, Pattern> {
    let (s1, _) = pair(
        tag("_"),
        opt(parse_id),
    )(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Ignore(span);
    Ok((s1, pat))
}

fn punit(s: Input) -> IResult<Input, Pattern> {
    let (s1, _) = tuple((tag("("), space0, tag(")")))(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Tuple(span, vec![]);
    Ok((s1, pat))
}

fn pparen(s: Input) -> IResult<Input, Pattern> {
    let (s1, inner) = delimited(
        pair(tag("("), space0),
        pattern,
        pair(space0, tag(")")),
    )(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Paren(span, Box::new(inner));
    Ok((s1, pat))
}

fn patom(s: Input) -> IResult<Input, Pattern> {
    alt((
        pint,
        pid,
        ptag,
        pignore,
        punit,
        pparen,
    ))(s)
}

fn pitem(s: Input) -> IResult<Input, Pattern> {
    alt((
        map(
            parse_ellipsis,
            Pattern::Collect,
        ),
        pother,
    ))(s)
}

fn ptuple(s: Input) -> IResult<Input, Pattern> {
    let (s1, xs) = map(
        pair(
            many1(
                terminated(
                    pitem,
                    tuple((space0, tag(","), space0)),
                )
            ),
            opt(pitem),
        ),
        |(mut xs, x)| {
            if let Some(x) = x {
                xs.push(x);
            }
            xs
        },
    )(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Tuple(span, xs);
    Ok((s1, pat))
}

fn papp(s: Input) -> IResult<Input, Pattern> {
    fn args(s: Input) -> IResult<Input, (Input, Vec<Pattern>)> {
        let (s1, xs) = delimited(
            pair(tag("("), space0),
            separated_list0(tuple((space0, tag(","), space0)), pitem),
            pair(space0, tag(")")),
        )(s)?;
        let span = Span::between(s, s1);
        Ok((s1, (span, xs)))
    }
    let (s1, (mut f, xs)) = pair(
        patom,
        many0(args),
    )(s)?;
    for (arg_span, args) in xs {
        let span = Span::to(s, arg_span);
        let inner = Box::new(f);
        f = Pattern::App {
            span,
            inner,
            arg_span,
            args,
        };
    }
    Ok((s1, f))
}

fn pother(s: Input) -> IResult<Input, Pattern> {
    alt((
        papp,
    ))(s)
}

fn pattern(s: Input) -> IResult<Input, Pattern> {
    alt((ptuple, pother))(s)
}

#[cfg(test)]
mod test {
    use super::*;

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
            Ok((Span::new(s, s.len(), s.len()), Expr::Tag(span, Span::new(s, 2, s.len())))),
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
        let expr = Expr::Paren(
            span,
            Box::new(Expr::Int(
                Span::new(s, 3, 7)
            ))
        );
        assert_eq!(
            eparen(span),
            Ok((Span::new(s, s.len(), s.len()), expr)),
        );

        assert_err!(eparen(Span::from("  (  1234)")));
    }

    #[test]
    fn test_eapp() {
        let s = "f(x, y)(z)";
        let span = Span::from(s);
        assert_eq!(
            eapp(span),
            Ok((
                Span::end(s),
                Expr::App {
                    span: Span::from(s),
                    inner: Box::new(Expr::App {
                        span: Span::new(s, 0, 7),
                        inner: Box::new(Expr::Id(Span::new(s, 0, 1))),
                        arg_span: Span::new(s, 1, 7),
                        args: vec![
                            Expr::Id(Span::new(s, 2, 3)),
                            Expr::Id(Span::new(s, 5, 6)),
                        ],
                    }),
                    arg_span: Span::new(s, 7, 10),
                    args: vec![
                        Expr::Id(Span::new(s, 8, 9)),
                    ],
                },
            )),
        );
    }

    #[test]
    fn test_eatom() {
        let s = "1234";
        let expr = Expr::Int(Span::from(s));
        assert_eq!(
            eatom(Span::from(s)),
            Ok((Span::end(s), expr)),
        );
    }

    #[test]
    fn test_ecase() {
        let s = "case x of x = x end";
        assert_eq!(
            ecase(Span::from(s)),
            Ok((
                Span::end(s),
                Expr::Case {
                    span: Span::new(s, 0, 19),
                    subject: Box::new(Expr::Id(Span::new(s, 5, 6))),
                    arms: vec![
                        Arm {
                            span: Span::new(s, 7, 15),
                            pattern: Pattern::Id(Span::new(s, 10, 11)),
                            expr: Expr::Id(Span::new(s, 14, 15)),
                        },
                    ],
                },
            )),
        );
    }

    #[test]
    fn test_pint() {
        let s = "1234";
        let span = Span::from(s);
        let pat = Pattern::Int(span);
        assert_eq!(
            pint(span),
            Ok((Span::end(s), pat)),
        );
    }

    #[test]
    fn test_ptag() {
        let s = ": xyz";
        let span = Span::from(s);
        let pat = Pattern::Tag(span, Span::new(s, 2, 5));
        assert_eq!(
            ptag(span),
            Ok((Span::end(s), pat)),
        );
    }

    #[test]
    fn test_pignore() {
        let s = "_xyz";
        let span = Span::from(s);
        let pat = Pattern::Ignore(span);
        assert_eq!(
            pignore(span),
            Ok((Span::end(s), pat)),
        );
    }

    #[test]
    fn test_punit() {
        let s = "(   )";
        let span = Span::from(s);
        let pat = Pattern::Tuple(span, vec![]);
        assert_eq!(
            punit(span),
            Ok((Span::end(s), pat)),
        );

        assert_err!(punit(Span::from("   ()")));
    }

    #[test]
    fn test_ptuple() {
        let s = "x, .., y";
        let span = Span::from(s);
        let pat = Pattern::Tuple(
            span,
            vec![
                Pattern::Id(Span::new(s, 0, 1)),
                Pattern::Collect(Ellipsis {
                    span: Span::new(s, 3, 5),
                    id: None,
                }),
                Pattern::Id(Span::new(s, 7, 8)),
            ],
        );
        assert_eq!(
            ptuple(span),
            Ok((Span::end(s), pat)),
        );

        let s = "x, ..y, z";
        let span = Span::from(s);
        let pat = Pattern::Tuple(
            span,
            vec![
                Pattern::Id(Span::new(s, 0, 1)),
                Pattern::Collect(Ellipsis {
                    span: Span::new(s, 3, 6),
                    id: Some(Span::new(s, 5, 6)),
                }),
                Pattern::Id(Span::new(s, 8, 9)),
            ],
        );
        assert_eq!(
            ptuple(span),
            Ok((Span::end(s), pat)),
        );
    }

    #[test]
    fn test_pparen() {
        let s = "(())";
        let span = Span::from(s);
        let pat = Pattern::Paren(
            Span::from(s),
            Box::new(Pattern::Tuple(
                Span::new(s, 1, 3),
                vec![],
            )),
        );
        assert_eq!(
            pparen(span),
            Ok((Span::end(s), pat)),
        );
    }

    #[test]
    fn test_papp() {
        let s = "f(x, y)(z)";
        let span = Span::from(s);
        assert_eq!(
            papp(span),
            Ok((
                Span::end(s),
                Pattern::App {
                    span: Span::from(s),
                    inner: Box::new(Pattern::App {
                        span: Span::new(s, 0, 7),
                        inner: Box::new(Pattern::Id(Span::new(s, 0, 1))),
                        arg_span: Span::new(s, 1, 7),
                        args: vec![
                            Pattern::Id(Span::new(s, 2, 3)),
                            Pattern::Id(Span::new(s, 5, 6)),
                        ],
                    }),
                    arg_span: Span::new(s, 7, 10),
                    args: vec![
                        Pattern::Id(Span::new(s, 8, 9)),
                    ],
                },
            )),
        );
    }
}
