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

impl From<ruff::span::TextRange> for Span {
    fn from(value: ruff::span::TextRange) -> Self {
        Self {
            start: value.start().to_u32(),
            end: value.end().to_u32(),
        }
    }
}

impl Into<ruff::span::TextRange> for Span {
    fn into(self) -> ruff::span::TextRange {
        ruff::span::TextRange::new(ruff::span::TextSize::from(self.start), ruff::span::TextSize::from(self.end))
    }
}

impl Into<oxc::span::Span> for Span {
    fn into(self) -> oxc::span::Span {
        oxc::span::Span::new(self.start, self.end)
    }
}