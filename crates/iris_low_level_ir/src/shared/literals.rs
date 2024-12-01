use crate::shared::atom::Atom;
use super::span::Span;

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NumericLiteral<'a> {
    pub value: f64,
    pub raw: Atom<'a>,
    pub span: Span,
    pub base: NumberBase,
}

#[derive(Clone, Debug, Copy)]
pub enum NumberBase {
    // Integers
    Binary = 0,
    Octal = 1,
    Decimal = 2,
    Hexadecimal = 3,

    // Float
    Float = 4,

    // BigInt
    BigInt = 5,
}

#[derive(Debug, Clone)]
pub struct StringLiteral<'a> {
    pub value: Atom<'a>,
    pub prefix: StringLiteralPrefix,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum StringLiteralPrefix {
    Empty,
    Unicode,
    Raw { uppercase: bool },
}

#[derive(Debug, Clone, Copy)]
pub enum FStringPrefix {
    Regular,
    Raw { uppercase_r: bool },
}

/// A template literal, e.g. `foo` or `foo${bar}baz`, or in Python, f-string, e.g. `f"{bar}"`
#[derive(Debug, Clone)]
pub struct TemplateLiteral<'a> {
    pub quasis: Vec<Atom<'a>>,
    // TODO
    pub expressions: Vec<String>,
    pub span: Span,
    pub prefix: FStringPrefix,
}
