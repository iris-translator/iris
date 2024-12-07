use crate::shared::literals::StringLiteral;
use crate::shared::span::Span;

#[derive(Debug, Clone)]
pub struct Directive {
    pub span: Span,
    pub expression: StringLiteral,
    pub directive: String,
}

#[derive(Debug, Clone)]
pub struct Hashbang {
    pub span: Span,
    pub value: String,
}
