use crate::expr::{
    Ellipsis, Input,
};
use crate::span::Span;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{cut, not, opt, value},
    multi::{many0},
    sequence::{pair, preceded, tuple},
    IResult,
};

/// parse an integer
pub(crate) fn parse_int(s: Input) -> IResult<Input, Input> {
    let (s1, _) = tuple((
        digit1,
        many0(pair(tag("_"), digit1)),
        cut(not(pair(multispace0, tag("_")))),
    ))(s)?;
    Ok((s1, Span::between(s, s1)))
}

/// parse a keyword
pub(crate) fn parse_kw(s: Input) -> IResult<Input, ()> {
    value((), alt((tag("case"), tag("of"), tag("do"), tag("end"))))(s)
}

/// parse an id
pub(crate) fn parse_id(s: Input) -> IResult<Input, Input> {
    let (s1, _) = tuple((not(parse_kw), alpha1, many0(pair(tag("_"), alphanumeric1))))(s)?;
    Ok((s1, Span::between(s, s1)))
}

/// parse a tag
pub(crate) fn parse_tag(s: Input) -> IResult<Input, (Input, Input)> {
    let (s1, span) = preceded(pair(tag(":"), multispace0), parse_id)(s)?;
    Ok((s1, (Span::between(s, s1), span)))
}

/// parse an ellipsis expression
pub(crate) fn parse_ellipsis(s: Input) -> IResult<Input, Ellipsis> {
    let (s1, id) = preceded(tag(".."), preceded(multispace0, opt(parse_id)))(s)?;
    let span = Span::between(s, s1);
    Ok((s1, Ellipsis { span, id }))
}
