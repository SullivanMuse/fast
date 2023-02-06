use nom_locate::LocatedSpan;

pub(crate) type Input<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Copy)]
pub(crate) struct Span<'a> {
    start: LocatedSpan<&'a str>,
    end: LocatedSpan<&'a str>,
}

impl<'a> std::fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Span")
         .field(self.start.fragment())
         .field(&self.start.location_offset())
         .field(&self.end.location_offset())
         .finish()
    }
}

impl<'a> Span<'a> {
    pub(crate) fn new(start: LocatedSpan<&'a str>, end: LocatedSpan<&'a str>) -> Self {
        Self { start, end }
    }
}
