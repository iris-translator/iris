use ruff::ast::str::Quote;
use ruff::ast::str_prefix::{AnyStringPrefix, StringLiteralPrefix as RuffStringLiteralPrefix};
use ruff::ast::{AnyStringFlags, FStringFlags, StringLiteralFlags};

use super::span::Span;
use crate::shared::expressions::Expression;

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NumericLiteral {
    pub value: f64,
    pub raw: String,
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

impl NumberBase {
    pub fn radix(self) -> u32 {
        match self {
            NumberBase::Binary => 2,
            NumberBase::Octal => 8,
            NumberBase::Decimal => 10,
            NumberBase::Hexadecimal => 16,
            NumberBase::Float => 10,
            NumberBase::BigInt => unreachable!(),
        }
    }
}

impl From<oxc::ast::ast::NumberBase> for NumberBase {
    fn from(value: oxc::ast::ast::NumberBase) -> Self {
        match value {
            oxc::ast::ast::NumberBase::Binary => NumberBase::Binary,
            oxc::ast::ast::NumberBase::Octal => NumberBase::Octal,
            oxc::ast::ast::NumberBase::Decimal => NumberBase::Decimal,
            oxc::ast::ast::NumberBase::Hex => NumberBase::Hexadecimal,
            oxc::ast::ast::NumberBase::Float => NumberBase::Float,
        }
    }
}

impl Into<oxc::ast::ast::NumberBase> for NumberBase {
    fn into(self) -> oxc::ast::ast::NumberBase {
        match self {
            NumberBase::Binary => oxc::ast::ast::NumberBase::Binary,
            NumberBase::Octal => oxc::ast::ast::NumberBase::Octal,
            NumberBase::Decimal => oxc::ast::ast::NumberBase::Decimal,
            NumberBase::Hexadecimal => oxc::ast::ast::NumberBase::Hex,
            NumberBase::Float => oxc::ast::ast::NumberBase::Float,
            NumberBase::BigInt => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub prefix: StringLiteralPrefix,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum StringLiteralPrefix {
    Empty,
    Unicode,
    Raw { uppercase: bool },
}

impl From<RuffStringLiteralPrefix> for StringLiteralPrefix {
    fn from(value: RuffStringLiteralPrefix) -> Self {
        match value {
            RuffStringLiteralPrefix::Empty => StringLiteralPrefix::Empty,
            RuffStringLiteralPrefix::Unicode => StringLiteralPrefix::Unicode,
            RuffStringLiteralPrefix::Raw { uppercase } => StringLiteralPrefix::Raw { uppercase },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FStringPrefix {
    Regular,
    Raw { uppercase_r: bool },
}

impl From<ruff::ast::str_prefix::FStringPrefix> for FStringPrefix {
    fn from(value: ruff::ast::str_prefix::FStringPrefix) -> Self {
        match value {
            ruff::ast::str_prefix::FStringPrefix::Regular => FStringPrefix::Regular,
            ruff::ast::str_prefix::FStringPrefix::Raw { uppercase_r } => {
                FStringPrefix::Raw { uppercase_r }
            }
        }
    }
}

impl Into<StringLiteralFlags> for StringLiteralPrefix {
    fn into(self) -> StringLiteralFlags {
        AnyStringFlags::new(
            AnyStringPrefix::Regular(match self {
                StringLiteralPrefix::Empty => RuffStringLiteralPrefix::Empty,
                StringLiteralPrefix::Unicode => RuffStringLiteralPrefix::Unicode,
                StringLiteralPrefix::Raw { uppercase } => {
                    RuffStringLiteralPrefix::Raw { uppercase }
                }
            }),
            Quote::Double,
            false,
        )
        .into()
    }
}

impl Into<FStringFlags> for FStringPrefix {
    fn into(self) -> FStringFlags {
        AnyStringFlags::new(
            AnyStringPrefix::Format(match self {
                FStringPrefix::Regular => ruff::ast::str_prefix::FStringPrefix::Regular,
                FStringPrefix::Raw { uppercase_r } => {
                    ruff::ast::str_prefix::FStringPrefix::Raw { uppercase_r }
                }
            }),
            Quote::Double,
            false,
        )
        .into()
    }
}

/// A template literal, e.g. `foo` or `foo${bar}baz`, or in Python, f-string, e.g. `f"{bar}"`
#[derive(Debug, Clone)]
pub struct TemplateLiteral {
    pub span: Span,
    pub quasis: Vec<String>,
    pub expressions: Vec<Expression>,
    pub prefix: FStringPrefix,
}

#[derive(Debug, Clone)]
pub struct NullLiteral {
    pub span: Span,
}
