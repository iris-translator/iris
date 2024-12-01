//! Part of this file is ported from [oxc](https://github.com/oxc-project/oxc/blob/main/oxc_ast/src/ast/js.rs).
use crate::shared::atom::Atom;
use crate::shared::literals::{BooleanLiteral, NumericLiteral, StringLiteral};
use crate::shared::span::Span;

#[derive(Debug, Clone)]
pub struct UnaryExpression<'a> {
    pub span: Span,
    pub operator: UnaryOperator,
    // TODO
    pub argument: Expression<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    UnaryPlus,
    UnaryNegation,
    LogicalNot,
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression<'a> {
    pub span: Span,
    pub operator: BinaryOperator,
    // TODO
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Equality = 0,
    Inequality = 1,

    // JavaScript-specific
    StrictEquality = 2,
    StrictInequality = 3,

    // Comparison
    LessThan = 4,
    LessEqualThan = 5,
    GreaterThan = 6,
    GreaterEqualThan = 7,

    // Arithmetic
    Addition = 8,
    Subtraction = 9,
    Multiplication = 10,
    Division = 11,
    DivisionWithoutRemainder = 12,
    Remainder = 13,
    Exponential = 14,

    // Binary
    ShiftLeft = 15,
    ShiftRight = 16,
    ShiftRightZeroFill = 17,
    BitwiseOR = 18,
    BitwiseXOR = 19,
    BitwiseAnd = 20,

    // Relational
    In = 21,
    Instanceof = 22,
}

#[derive(Debug, Clone)]
pub struct LogicalExpression<'a> {
    pub span: Span,
    pub operator: LogicalOperator,
    // TODO
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperator {
    LogicalOR,
    LogicalAND,

    // JavaScript-specific
    Coalesce,
}

#[derive(Debug, Clone)]
pub struct ConditionalExpression<'a> {
    pub span: Span,
    // TODO
    pub test: Expression<'a>,
    pub consequent: Expression<'a>,
    pub alternate: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpression<'a> {
    pub span: Span,
    // For operator, we can simply transform the assignment with other operators into a simple assignment with the operator
    // TODO
    pub target: Expression<'a>,
    pub value: Expression<'a>,
}

// JavaScript-specific / C-like
#[derive(Debug, Clone)]
pub struct UpdateExpression<'a> {
    pub span: Span,
    pub operator: UpdateOperator,
    // TODO
    pub target: Expression<'a>,
    pub prefix: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateOperator {
    Increment = 0,
    Decrement = 1,
}

#[derive(Debug, Clone)]
pub struct Identifier<'a> {
    pub span: Span,
    pub name: Atom<'a>,
}


#[derive(Debug, Clone)]
pub enum Expression<'a> {
    BooleanLiteral(BooleanLiteral),
    NumericLiteral(NumericLiteral<'a>),
    StringLiteral(StringLiteral<'a>),

    Identifier(Identifier<'a>),

    UnaryExpression(Box<UnaryExpression<'a>>),
    BinaryExpression(Box<BinaryExpression<'a>>),
    LogicalExpression(Box<LogicalExpression<'a>>),
    ConditionalExpression(Box<ConditionalExpression<'a>>),
    AssignmentExpression(Box<AssignmentExpression<'a>>),
    UpdateExpression(Box<UpdateExpression<'a>>),
}
