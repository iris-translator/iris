#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Span {
    /// The start of the span, inclusive
    pub start: u32,
    /// The end of the span, exclusive
    pub end: u32,
}

impl From<oxc::span::Span> for Span {
    fn from(span: oxc::span::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}
