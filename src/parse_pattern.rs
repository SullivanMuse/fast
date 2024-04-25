use crate::expr::{
    Input, Pattern, PatternApp,
};
use crate::parse_common::{parse_ellipsis, parse_id, parse_int, parse_tag};
use crate::span::Span;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0},
    combinator::{map, opt},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, terminated, tuple},
    IResult,
};

fn pint(s: Input) -> IResult<Input, Pattern> {
    map(parse_int, Pattern::Int)(s)
}

fn pid(s: Input) -> IResult<Input, Pattern> {
    map(parse_id, Pattern::Id)(s)
}

fn ptag(s: Input) -> IResult<Input, Pattern> {
    map(parse_tag, |(span1, span2)| Pattern::Tag(span1, span2))(s)
}

fn pignore(s: Input) -> IResult<Input, Pattern> {
    let (s1, _) = pair(tag("_"), opt(parse_id))(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Ignore(span);
    Ok((s1, pat))
}

fn punit(s: Input) -> IResult<Input, Pattern> {
    let (s1, _) = tuple((tag("("), multispace0, tag(")")))(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Tuple(span, vec![]);
    Ok((s1, pat))
}

fn pparen(s: Input) -> IResult<Input, Pattern> {
    let (s1, inner) = delimited(
        pair(tag("("), multispace0),
        pattern,
        pair(multispace0, tag(")")),
    )(s)?;
    let span = Span::between(s, s1);
    let pat = Pattern::Paren(span, Box::new(inner));
    Ok((s1, pat))
}

fn patom(s: Input) -> IResult<Input, Pattern> {
    alt((pint, pid, ptag, pignore, punit, pparen))(s)
}

fn pitem(s: Input) -> IResult<Input, Pattern> {
    alt((map(parse_ellipsis, Pattern::Collect), pother))(s)
}

fn ptuple(s: Input) -> IResult<Input, Pattern> {
    let (s1, xs) = map(
        pair(
            many1(terminated(
                pitem,
                tuple((multispace0, tag(","), multispace0)),
            )),
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
            pair(tag("("), multispace0),
            separated_list0(tuple((multispace0, tag(","), multispace0)), pitem),
            pair(multispace0, tag(")")),
        )(s)?;
        let span = Span::between(s, s1);
        Ok((s1, (span, xs)))
    }
    let (s1, (mut f, xs)) = pair(patom, many0(args))(s)?;
    for (arg_span, args) in xs {
        let span = Span::to(s, arg_span);
        let inner = Box::new(f);
        f = Pattern::App(PatternApp {
            span,
            f: inner,
            arg_span,
            xs: args,
        });
    }
    Ok((s1, f))
}

fn pother(s: Input) -> IResult<Input, Pattern> {
    alt((papp,))(s)
}

pub(crate) fn pattern(s: Input) -> IResult<Input, Pattern> {
    alt((ptuple, pother))(s)
}

#[cfg(test)]
mod test {
    use crate::expr::Ellipsis;

    use super::*;

    macro_rules! assert_err {
        ($e: expr) => {
            if let Ok(_) = $e {
                assert!(false);
            }
        };
    }

    #[test]
    fn test_pint() {
        let s = "1234";
        let span = Span::from(s);
        let pat = Pattern::Int(span);
        assert_eq!(pint(span), Ok((Span::end(s), pat)),);
    }

    #[test]
    fn test_ptag() {
        let s = ": xyz";
        let span = Span::from(s);
        let pat = Pattern::Tag(span, Span::new(s, 2, 5));
        assert_eq!(ptag(span), Ok((Span::end(s), pat)),);
    }

    #[test]
    fn test_pignore() {
        let s = "_xyz";
        let span = Span::from(s);
        let pat = Pattern::Ignore(span);
        assert_eq!(pignore(span), Ok((Span::end(s), pat)),);
    }

    #[test]
    fn test_punit() {
        let s = "(   )";
        let span = Span::from(s);
        let pat = Pattern::Tuple(span, vec![]);
        assert_eq!(punit(span), Ok((Span::end(s), pat)),);

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
        assert_eq!(ptuple(span), Ok((Span::end(s), pat)),);

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
        assert_eq!(ptuple(span), Ok((Span::end(s), pat)),);
    }

    #[test]
    fn test_pparen() {
        let s = "(())";
        let span = Span::from(s);
        let pat = Pattern::Paren(
            Span::from(s),
            Box::new(Pattern::Tuple(Span::new(s, 1, 3), vec![])),
        );
        assert_eq!(pparen(span), Ok((Span::end(s), pat)),);
    }

    #[test]
    fn test_papp() {
        let s = "f(x, y)(z)";
        let span = Span::from(s);
        assert_eq!(
            papp(span),
            Ok((
                Span::end(s),
                Pattern::App(PatternApp {
                    span: Span::from(s),
                    f: Box::new(Pattern::App(PatternApp {
                        span: Span::new(s, 0, 7),
                        f: Box::new(Pattern::Id(Span::new(s, 0, 1))),
                        arg_span: Span::new(s, 1, 7),
                        xs: vec![
                            Pattern::Id(Span::new(s, 2, 3)),
                            Pattern::Id(Span::new(s, 5, 6)),
                        ],
                    })),
                    arg_span: Span::new(s, 7, 10),
                    xs: vec![Pattern::Id(Span::new(s, 8, 9)),],
                }),
            )),
        );
    }
}
