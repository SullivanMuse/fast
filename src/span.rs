use nom::{
    error::{ErrorKind, ParseError},
    Compare, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice,
};
use std::{ops::{Range, RangeFrom, RangeFull, RangeTo}, cell::RefCell, rc::Rc};
use unwrap::unwrap;

use crate::context::Context;

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct Span<T> {
    context: Rc<RefCell<Context>>,
    inner: T,
    start: usize,
    end: usize,
}

impl<'a> Span<&'a str> {
    pub(crate) fn value_i64(&self) -> i64 {
        unwrap!(
            self.as_inner().parse::<i64>(),
            "interpreter: {:?} failed to parse to i64",
            self
        )
    }
}

// impl<T> std::fmt::Debug for Span<T> {
//     fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         fmt.debug_tuple("Span")
//            .field(&self.start)
//            .field(&self.end)
//            .finish()
//     }
// }

impl<T> std::fmt::Debug for Span<T>
where
    T: std::fmt::Debug + Slice<Range<usize>>,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_tuple("Span").field(&self.as_inner()).finish()
    }
}

impl<T> Span<T> {
    pub(crate) fn new(context: Rc<RefCell<Context>>, inner: T, start: usize, end: usize) -> Self {
        Self { context, inner, start, end }
    }

    pub(crate) fn fresh(inner: T, start: usize, end: usize) -> Self {
        Self::new(Rc::new(RefCell::new(Context::new())), inner, start, end)
    }

    #[allow(dead_code)]
    pub(crate) fn end(inner: T) -> Self
    where
        T: InputLength,
    {
        let length = inner.input_len();
        Span::fresh(inner, length, length)
    }

    pub(crate) fn as_inner(&self) -> T
    where
        T: Slice<Range<usize>>,
    {
        self.inner.slice(self.start..self.end)
    }

    pub(crate) fn between(first: Span<T>, second: Span<T>) -> Self
    where
        T: Clone,
    {
        Span::new(second.context.clone(), first.inner.clone(), first.start, second.start)
    }

    pub(crate) fn to(first: Span<T>, second: Span<T>) -> Self
    where
        T: Clone,
    {
        Self::new(second.context.clone(), first.inner.clone(), first.start, second.end)
    }
}

impl<T> From<T> for Span<T>
where
    T: InputLength,
{
    fn from(inner: T) -> Self {
        let length = inner.input_len();
        let context = Rc::new(RefCell::new(Context::new()));
        Self::new(context, inner, 0, length)
    }
}

impl<T> InputLength for Span<T>
where
    T: InputLength,
{
    fn input_len(&self) -> usize {
        self.end
            .min(self.inner.input_len())
            .saturating_sub(self.start)
    }
}

impl<T> Slice<Range<usize>> for Span<T>
where
    T: Clone,
{
    fn slice(&self, range: Range<usize>) -> Self {
        let start = self.start + range.start;
        let end = self.start + range.end;
        Self::new(self.context.clone(), self.inner.clone(), start, end)
    }
}

impl<T> Slice<RangeFrom<usize>> for Span<T>
where
    T: Clone,
{
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        let start = self.start + range.start;
        let end = self.end;
        Self::new(self.context.clone(), self.inner.clone(), start, end)
    }
}

impl<T> Slice<RangeTo<usize>> for Span<T>
where
    T: Clone,
{
    fn slice(&self, range: RangeTo<usize>) -> Self {
        let start = self.start;
        let end = self.start + range.end;
        Self::new(self.context.clone(), self.inner.clone(), start, end)
    }
}

impl<T> Slice<RangeFull> for Span<T>
where
    Span<T>: Copy,
{
    fn slice(&self, _: RangeFull) -> Self {
        *self
    }
}

impl<T> InputTake for Span<T>
where
    T: Clone,
{
    /// We're taking bytes, so we use AsBytes
    fn take(&self, count: usize) -> Self {
        self.slice(..count)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.slice(count..), self.slice(..count))
    }
}

impl<T> InputTakeAtPosition for Span<T>
where
    T: InputTakeAtPosition + InputLength + InputIter + Clone + Slice<Range<usize>>,
    Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Clone,
{
    type Item = <T as InputIter>::Item;

    fn split_at_position<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.as_inner().position(predicate) {
            None => Err(Err::Incomplete(nom::Needed::new(1))),
            Some(n) => Ok(self.take_split(n)),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.as_inner().position(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.as_inner().position(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.as_inner().input_len() == 0 {
                    Err(Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl<T> InputIter for Span<T>
where
    T: InputIter + Slice<Range<usize>>,
{
    type Item = <T as InputIter>::Item;
    type Iter = <T as InputIter>::Iter;
    type IterElem = <T as InputIter>::IterElem;

    fn iter_indices(&self) -> Self::Iter {
        self.as_inner().iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.as_inner().iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_inner().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_inner().slice_index(count)
    }
}

impl<T, U> Compare<U> for Span<T>
where
    T: Compare<U> + Slice<Range<usize>>,
{
    fn compare(&self, t: U) -> nom::CompareResult {
        self.as_inner().compare(t)
    }

    fn compare_no_case(&self, t: U) -> nom::CompareResult {
        self.as_inner().compare_no_case(t)
    }
}

impl<T> Offset for Span<T> {
    fn offset(&self, second: &Self) -> usize {
        second.start.saturating_sub(self.start)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{
        branch::alt, bytes::complete::tag, character::complete::alpha1, sequence::pair, IResult,
    };

    #[test]
    fn test_tag() {
        let s = "hello";
        let span = Span::from(s);

        fn hello(s: Span<&str>) -> IResult<Span<&str>, Span<&str>> {
            tag("hello")(s)
        }

        assert_eq!(hello(span), Ok((Span::fresh(s, 5, 5), Span::fresh(s, 0, 5))),);
    }

    #[test]
    fn test_alpha1() {
        let s = "hello";
        let span = Span::from(s);

        fn id(s: Span<&str>) -> IResult<Span<&str>, Span<&str>> {
            alpha1(s)
        }

        assert_eq!(id(span), Ok((Span::fresh(s, 5, 5), Span::fresh(s, 0, 5))),);
    }

    #[test]
    fn test_alt() {
        let s = "hello";
        let span = Span::from(s);

        fn parse(s: Span<&str>) -> IResult<Span<&str>, Span<&str>> {
            alt((tag("thing"), alpha1))(s)
        }

        assert_eq!(parse(span), Ok((Span::fresh(s, 5, 5), Span::fresh(s, 0, 5))),);
    }

    #[test]
    fn test_pair() {
        let s = "thinghello";
        let span = Span::from(s);

        fn parse(s: Span<&str>) -> IResult<Span<&str>, (Span<&str>, Span<&str>)> {
            pair(tag("thing"), alpha1)(s)
        }

        assert_eq!(
            parse(span),
            Ok((
                Span::fresh(s, 10, 10),
                (Span::fresh(s, 0, 5), Span::fresh(s, 5, 10)),
            )),
        );
    }
}
