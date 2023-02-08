use nom::{
    bytes::complete::tag,
    branch::alt,
    character::complete::{digit1, space0, alpha1, alphanumeric1},
    combinator::{cut, not, value, map, opt},
    IResult,
    multi::{many0, many1},
    sequence::{pair, tuple, preceded, delimited, terminated},
};
use nom_locate::LocatedSpan;

type Input<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Copy, Debug)]
struct Span<'a> {
    input: Input<'a>,
    len: usize,
}

impl<'a> Span<'a> {
    fn new(start: Input<'a>, end: Input<'a>) -> Self {
        let input = start;
        let len = end.location_offset().checked_sub(start.location_offset()).unwrap_or(0);
        Self { input, len }
    }
}

#[derive(Debug)]
struct Arm<'a> {
    span: Span<'a>,
    pattern: Pattern<'a>,
    expr: Expr<'a>,
}

#[derive(Debug)]
struct Ellipsis<'a> {
    span: Span<'a>,
    id: Option<Span<'a>>,
}

#[derive(Debug)]
enum Expr<'a> {
    Int(Span<'a>),
    Tag(Span<'a>),
    Name(Span<'a>),
    Expand(Ellipsis<'a>),
    Tuple(Span<'a>, Vec<Expr<'a>>),
    App(Span<'a>, Box<Expr<'a>>, Vec<Vec<Expr<'a>>>),
    Case {
        span: Span<'a>,
        subject: Box<Expr<'a>>,
        arms: Vec<Arm<'a>>,
    }
}

#[derive(Debug)]
enum Pattern<'a> {
    Id(Span<'a>),
    Ignore(Span<'a>),
    Int(Span<'a>),
    Tag(Span<'a>),
    Collect(Ellipsis<'a>),
    Tuple(Span<'a>, Vec<Pattern<'a>>),
    App(Span<'a>, Box<Pattern<'a>>, Vec<Vec<Pattern<'a>>>),
}

fn parse_int(s: Input) -> IResult<Input, Span> {
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
    Ok((s1, Span::new(s, s1)))
}

fn parse_kw(s: Input) -> IResult<Input, ()> {
    value((), alt((
        tag("case"),
        tag("of"),
        tag("do"),
        tag("end"),
    )))(s)
}

fn parse_id(s: Input) -> IResult<Input, Span> {
    let (s1, _) = tuple((
        not(parse_kw),
        alpha1,
        many0(pair(tag("_"), alphanumeric1)),
    ))(s)?;
    Ok((s1, Span::new(s, s1)))
}

fn parse_tag(s: Input) -> IResult<Input, Span> {
    preceded(
        pair(
            tag(":"),
            space0,
        ),
        parse_id,
    )(s)
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
        Expr::Tag,
    )(s)
}

fn eid(s: Input) -> IResult<Input, Expr> {
    map(
        parse_id,
        Expr::Name,
    )(s)
}

fn eparen(s: Input) -> IResult<Input, Expr> {
    delimited(tag("("), expr, tag(")"))(s)
}

fn eatom(s: Input) -> IResult<Input, Expr> {
    alt((
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
    let span = Span::new(s, s1);
    Ok((s1, Ellipsis { span, id }))
}

fn eitem(s: Input) -> IResult<Input, Expr> {
    alt((
        map(
            parse_ellipsis,
            Expr::Expand,
        ),
        expr_no_tuple,
    ))(s)
}

fn eapp(s: Input) -> IResult<Input, Expr> {
    let list = map(
        pair(
            many1(terminated(
                preceded(space0, eitem),
                preceded(space0, tag(",")),
            )),
            opt(preceded(space0, eitem)),
        ),
        |(mut xs, x)| {
            if let Some(x) = x {
                xs.push(x);
            }
            xs
        },
    );
    let (s1, (f, xs)) = pair(
        eatom,
        preceded(space0, many0(delimited(tag("("), list, tag(")")))),
    )(s)?;
    let span = Span::new(s, s1);
    if xs.len() == 0 {
        Ok((s1, f))
    } else {
        Ok((s1, Expr::App(span, Box::new(f), xs)))
    }
}

fn etuple(s: Input) -> IResult<Input, Expr> {
    let (s1, (mut xs, x)) = pair(
        many1(terminated(
            preceded(space0, eitem),
            preceded(space0, tag(",")),
        )),
        opt(preceded(space0, eitem)),
    )(s)?;
    if let Some(x) = x {
        xs.push(x);
    }
    let span = Span::new(s, s1);
    Ok((s1, Expr::Tuple(span, xs)))
}

fn arm(s: Input) -> IResult<Input, Arm> {
    let (s1, (pattern, expr)) = pair(
        preceded(
            tuple((space0, tag("of"), space0)),
            cut(pattern),
        ),
        preceded(
            tuple((space0, tag("="), space0)),
            expr,
        ),
    )(s)?;
    let span = Span::new(s, s1);
    Ok((s1, Arm { span, pattern, expr }))
}

fn ecase(s: Input) -> IResult<Input, Expr> {
    let (s1, (subject, arms)) = pair(
        preceded(
            pair(
                tag("case"),
                space0,
            ),
            expr_no_tuple,
        ),
        delimited(
            space0,
            many0(arm),
            pair(
                space0,
                tag("end"),
            )
        ),
    )(s)?;
    let span = Span::new(s, s1);
    let subject = Box::new(subject);
    Ok((s1, Expr::Case { span, subject, arms }))
}

fn expr_no_tuple(s: Input) -> IResult<Input, Expr> {
    alt((
        eapp,
        ecase,
    ))(s)
}

fn expr(s: Input) -> IResult<Input, Expr> {
    alt((expr_no_tuple, etuple))(s)
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
        Pattern::Tag,
    )(s)
}

fn pignore(s: Input) -> IResult<Input, Pattern> {
    let (s1, _) = pair(tag("_"), opt(parse_id))(s)?;
    Ok((s1, Pattern::Ignore(Span::new(s, s1))))
}

fn pparen(s: Input) -> IResult<Input, Pattern> {
    delimited(tag("("), pattern, tag(")"))(s)
}

fn patom(s: Input) -> IResult<Input, Pattern> {
    alt((
        pint,
        pid,
        ptag,
        pignore,
        pparen,
    ))(s)
}

fn pitem(s: Input) -> IResult<Input, Pattern> {
    alt((
        map(
            parse_ellipsis,
            Pattern::Collect,
        ),
        pat_no_tuple,
    ))(s)
}

fn ptuple(s: Input) -> IResult<Input, Pattern> {
    let (s1, xs) = map(
        pair(
            many1(
                terminated(
                    preceded(space0, pitem),
                    pair(space0, tag(",")),
                )
            ),
            opt(preceded(space0, pitem)),
        ),
        |(mut xs, x)| {
            if let Some(x) = x {
                xs.push(x);
            }
            xs
        },
    )(s)?;
    let span = Span::new(s, s1);
    Ok((s1, Pattern::Tuple(span, xs)))
}

fn papp(s: Input) -> IResult<Input, Pattern> {
    let items = map(
        pair(
            many1(
                terminated(
                    preceded(space0, pitem),
                    pair(space0, tag(",")),
                )
            ),
            opt(preceded(space0, pitem)),
        ),
        |(mut xs, x)| {
            if let Some(x) = x {
                xs.push(x);
            }
            xs
        },
    );
    let (s1, (f, xs)) = pair(
        patom,
        many0(delimited(tag("("), items, tag(")"))),
    )(s)?;
    let span = Span::new(s, s1);
    if xs.len() == 0 {
        Ok((s1, f))
    } else {
        Ok((s1, Pattern::App(span, Box::new(f), xs)))
    }
}

fn pat_no_tuple(s: Input) -> IResult<Input, Pattern> {
    alt((
        papp,
    ))(s)
}

fn pattern(s: Input) -> IResult<Input, Pattern> {
    alt((pat_no_tuple, ptuple))(s)
}

fn main() {
    fn input() -> String {
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        s
    }

    loop {
        let s = input();
        let span = s.as_str().into();
        println!("{:?}", expr(span));
    }
}
