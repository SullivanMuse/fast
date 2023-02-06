use crate::span::{Input, Span};
use crate::{Expr, Pattern, Arm};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, space0, alphanumeric1, digit1},
    combinator::{not, opt, success},
    IResult,
    multi::{many0, many1, separated_list0},
    Parser,
    sequence::{tuple, delimited, terminated, preceded},
};

fn spanned<'a, O, E, F>(mut f: F) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, (Span<'a>, O), E>
where
    F: Parser<Input<'a>, O, E>,
{
    move |s| {
        let (s1, r) = f.parse(s)?;
        Ok((s1, (Span::new(s, s1), r)))
    }
}

fn span<'a, O, E, F>(mut f: F) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, Span<'a>, E>
where
    F: Parser<Input<'a>, O, E>,
{
    move |s| {
        let (s1, _) = f.parse(s)?;
        Ok((s1, Span::new(s, s1)))
    }
}

/// pignore = '_' ('_' | alnum)*
fn parse_pignore(s: Input) -> IResult<Input, Pattern> {
    let p = tuple((tag("_"), many0(alt((tag("_"), alphanumeric1)))));
    span(p).map(Pattern::Ignore).parse(s)
}

/// pellipsis = '..' (pignore | pbind)?
fn parse_pellipsis(s: Input) -> IResult<Input, Pattern> {
    let p = preceded(tuple((tag(".."), space0)), opt(alt((parse_pignore, parse_pbind))));
    spanned(p).map(|(span, inner)| Pattern::Ellipsis(span, inner.map(Box::new))).parse(s)
}

/// pbind = alpha ('_' | alnum)*
fn parse_pbind(s: Input) -> IResult<Input, Pattern> {
    span(parse_id).map(Pattern::Bind).parse(s)
}

/// ptag = ':' alpha ('_' | alnum)*
fn parse_ptag(s: Input) -> IResult<Input, Pattern> {
    let p = tuple((tag(":"), space0, parse_id));
    span(p).map(Pattern::Tag).parse(s)
}

/// ptuple = '(' ((atom ',')+ atom?)? ')'
fn parse_ptuple(s: Input) -> IResult<Input, Pattern> {
    let inner = tuple((many1(terminated(parse_patom, tuple((space0, tag(","), space0)))), opt(parse_patom)))
        .map(|(mut v, x)| {
            if let Some(x) = x {
                v.push(x);
            }
            v
        })
        .or(success(vec![]));
    let p = delimited(tuple((tag("("), space0)), inner, tuple((space0, tag(")"))));
    spanned(p).map(|(span, items)| Pattern::Tuple(span, items)).parse(s)
}

/// pparen = '(' patom ')'
fn parse_pparen(s: Input) -> IResult<Input, Pattern> {
    let p = delimited(tuple((tag("("), space0)), parse_patom, tuple((space0, tag(")"))));
    spanned(p).map(|(span, inner)| Pattern::Paren(span, Box::new(inner))).parse(s)
}

/// patom = ptuple | pbind | ptag | pellipsis | pignore | ptuple | pparen
fn parse_patom(s: Input) -> IResult<Input, Pattern> {
    alt((parse_pbind, parse_ptag, parse_pellipsis, parse_pignore, parse_ptuple, parse_pparen))(s)
}


/// id = alpha ('_' | alnum)*
fn parse_id(s: Input) -> IResult<Input, Span> {
    let p = tuple((alpha1, many0(alt((tag("_"), alphanumeric1)))));
    span(p).parse(s)
}

/// name = !('case' | 'of' | 'end') id
fn parse_name(s: Input) -> IResult<Input, Expr> {
    let (s, _) = not(alt((tag("case"), tag("of"), tag("end"))))(s)?;
    parse_id.map(Expr::Name).parse(s)
}

/// paren = '(' atom ')'
fn parse_paren(s: Input) -> IResult<Input, Expr> {
    let p = spanned(delimited(tuple((tag("("), space0)), parse_atom, tuple((space0, tag(")")))));
    p.map(|(span, r)| Expr::Paren(span, Box::new(r))).parse(s)
}

/// int = digit+ ('_' digit+)*
fn parse_int(s: Input) -> IResult<Input, Expr> {
    let p = tuple((digit1, many0(tuple((tag("_"), digit1)))));
    span(p).map(Expr::Int).parse(s)
}

/// tuple = '(' ((atom ',')+ atom?)? ')'
fn parse_tuple(s: Input) -> IResult<Input, Expr> {
    let inner = tuple((many1(terminated(parse_atom, tuple((space0, tag(","), space0)))), opt(parse_atom)))
        .map(|(mut v, x)| {
            if let Some(x) = x {
                v.push(x);
            }
            v
        })
        .or(success(vec![]));
    let p = delimited(tuple((tag("("), space0)), inner, tuple((space0, tag(")"))));
    spanned(p).map(|(span, items)| Expr::Tuple(span, items)).parse(s)
}

/// tag = ':' id
fn parse_tag(s: Input) -> IResult<Input, Expr> {
    let p = tuple((tag(":"), space0, parse_id));
    span(p).map(Expr::Tag).parse(s)
}

/// atom = paren | tuple | id | int
pub(crate) fn parse_atom(s: Input) -> IResult<Input, Expr> {
    alt((parse_case, parse_paren, parse_tuple, parse_name, parse_tag, parse_int))(s)
}

/// arm
fn parse_arm(s: Input) -> IResult<Input, Arm> {
    let p = tuple((parse_patom, space0, tag("->"), space0, parse_atom));
    spanned(p).map(|(span, (pattern, _, _, _, body))| Arm { span, pattern, body }).parse(s)
}

/// case = 'case' expr 'of' arm[','] 'end'
fn parse_case(s: Input) -> IResult<Input, Expr> {
    let p = tuple((tag("case"), space0, parse_atom, space0, tag("of"), separated_list0(tuple((space0, tag(","), space0)), parse_arm), space0, tag("end")));
    spanned(p).map(|(span, (_case, _, subject, _, _of, arms, _, _end))|
        Expr::Case { span, subject: Box::new(subject), arms }).parse(s)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::parse_paren;
    
    macro_rules! assert_let {
        ($p: pat = $e: expr) => {
            if let $p = $e {
            } else { assert!(false); }
        };
    }

    #[test]
    fn test_parse_name() {
        assert_let!(Ok((_, Expr::Name(_))) = parse_name("hello".into()));
        assert_let!(Err(_) = parse_name("case".into()));
    }

    #[test]
    fn test_parse_int() {
        assert_let!(Ok((_, Expr::Int(_))) = parse_int("1234_5678".into()));
        assert_let!(Err(_) = parse_int("Hello!".into()));
    }

    #[test]
    fn test_parse_tag() {
        assert_let!(Ok((_, Expr::Tag(_))) = parse_tag(":x".into()));
    }

    #[test]
    fn test_parse_paren() {
        assert_let!(Ok((_, Expr::Paren(_, _))) = parse_paren("(    ( ))".into()));
    }

    #[test]
    fn test_parse_tuple() {
        assert_let!(Ok((_, Expr::Tuple(_, _))) = parse_tuple("(1, 2, 3)".into()));
    }
}
