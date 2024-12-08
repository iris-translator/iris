//! Part of this file is ported from [oxc](https://github.com/oxc-project/oxc/blob/main/oxc_ast/src/ast) and [ruff](https://github.com/astral-sh/ruff/tree/main/crates/ruff_python_ast/src).

use crate::shared::declarations::FunctionBody;
use crate::shared::literals::*;
use crate::shared::span::Span;

#[derive(Debug, Clone)]
pub enum Expression {
    BooleanLiteral(BooleanLiteral),
    NullLiteral(NullLiteral),
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
    TemplateLiteral(Box<TemplateLiteral>),

    ArrayExpression(Box<ArrayExpression>),
    ObjectExpression(Box<ObjectExpression>),
    SetExpression(Box<SetExpression>),
    TupleExpression(Box<TupleExpression>),
    SequenceExpression(Box<SequenceExpression>),

    Identifier(Identifier),

    UnaryExpression(Box<UnaryExpression>),
    BinaryExpression(Box<BinaryExpression>),
    LogicalExpression(Box<LogicalExpression>),
    ConditionalExpression(Box<ConditionalExpression>),
    AssignmentExpression(Box<AssignmentExpression>),
    UpdateExpression(Box<UpdateExpression>),
    CallExpression(Box<CallExpression>),
    ChainExpression(Box<ChainExpression>),
    LambdaExpression(Box<LambdaExpression>),
    ParenthesizedExpression(Box<ParenthesizedExpression>),

    StaticMemberExpression(Box<StaticMemberExpression>),
    ComputedMemberExpression(Box<ComputedMemberExpression>),

    // Specific, for ease of use
    SpreadElement(Box<SpreadElement>),
    Elision(Elision),
    ObjectProperty(Box<ObjectProperty>),
}


#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub span: Span,
    pub operator: UnaryOperator,
    pub argument: Expression,
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

impl From<oxc::ast::ast::UnaryOperator> for UnaryOperator {
    fn from(value: oxc::ast::ast::UnaryOperator) -> Self {
        match value {
            oxc::ast::ast::UnaryOperator::UnaryPlus => UnaryOperator::UnaryPlus,
            oxc::ast::ast::UnaryOperator::UnaryNegation => UnaryOperator::UnaryNegation,
            oxc::ast::ast::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
            oxc::ast::ast::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
            oxc::ast::ast::UnaryOperator::Typeof => UnaryOperator::Typeof,
            oxc::ast::ast::UnaryOperator::Void => UnaryOperator::Void,
            oxc::ast::ast::UnaryOperator::Delete => UnaryOperator::Delete,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub span: Span,
    pub operator: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // ==
    Equality,
    // !=
    Inequality,

    // JavaScript-specific
    StrictEquality,
    StrictInequality,

    // Comparison
    LessThan,
    LessEqualThan,
    GreaterThan,
    GreaterEqualThan,

    // Arithmetic
    Addition,
    Subtraction,
    Multiplication,
    // Python-specific -- @
    MatMultiplication,
    Division,
    FloorDivision,
    Remainder,
    Exponential,

    // Binary
    ShiftLeft,
    ShiftRight,
    // JavaScript-specific -- >>>
    ShiftRightZeroFill,
    BitwiseOR,
    BitwiseXOR,
    BitwiseAnd,

    // Relational
    In,
    Instanceof,
}

impl From<oxc::ast::ast::BinaryOperator> for BinaryOperator {
    fn from(value: oxc::ast::ast::BinaryOperator) -> Self {
        match value {
            oxc::ast::ast::BinaryOperator::Addition => BinaryOperator::Addition,
            oxc::ast::ast::BinaryOperator::Subtraction => BinaryOperator::Subtraction,
            oxc::ast::ast::BinaryOperator::Multiplication => BinaryOperator::Multiplication,
            oxc::ast::ast::BinaryOperator::Division => BinaryOperator::Division,
            oxc::ast::ast::BinaryOperator::Remainder => BinaryOperator::Remainder,
            oxc::ast::ast::BinaryOperator::Exponential => BinaryOperator::Exponential,

            oxc::ast::ast::BinaryOperator::ShiftLeft => BinaryOperator::ShiftLeft,
            oxc::ast::ast::BinaryOperator::ShiftRight => BinaryOperator::ShiftRight,
            oxc::ast::ast::BinaryOperator::ShiftRightZeroFill => BinaryOperator::ShiftRightZeroFill,
            oxc::ast::ast::BinaryOperator::BitwiseOR => BinaryOperator::BitwiseOR,
            oxc::ast::ast::BinaryOperator::BitwiseXOR => BinaryOperator::BitwiseXOR,
            oxc::ast::ast::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,

            oxc::ast::ast::BinaryOperator::Equality => BinaryOperator::Equality,
            oxc::ast::ast::BinaryOperator::Inequality => BinaryOperator::Inequality,
            oxc::ast::ast::BinaryOperator::StrictEquality => BinaryOperator::StrictEquality,
            oxc::ast::ast::BinaryOperator::StrictInequality => BinaryOperator::StrictInequality,
            oxc::ast::ast::BinaryOperator::LessThan => BinaryOperator::LessThan,
            oxc::ast::ast::BinaryOperator::LessEqualThan => BinaryOperator::LessEqualThan,
            oxc::ast::ast::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            oxc::ast::ast::BinaryOperator::GreaterEqualThan => BinaryOperator::GreaterEqualThan,

            oxc::ast::ast::BinaryOperator::In => BinaryOperator::In,
            oxc::ast::ast::BinaryOperator::Instanceof => BinaryOperator::Instanceof,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub span: Span,
    pub operator: LogicalOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperator {
    LogicalOR,
    LogicalAND,

    // JavaScript-specific
    Coalesce,
}

#[derive(Debug, Clone)]
pub struct ConditionalExpression {
    pub span: Span,
    pub test: Expression,
    pub consequent: Expression,
    pub alternate: Expression,
}

#[derive(Debug, Clone)]
pub struct ParenthesizedExpression {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    pub span: Span,
    // For operator, we can simply transform the assignment with other operators into a simple assignment with the operator
    pub target: Expression,
    pub value: Expression,
}

// JavaScript-specific / C-like
#[derive(Debug, Clone)]
pub struct UpdateExpression {
    pub span: Span,
    // Otherwise it's a decrement
    pub increment: bool,
    pub target: Expression,
    pub prefix: bool,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub span: Span,
    pub callee: Expression,
    pub arguments: Vec<Expression>,

    // JS-like, `new`
    pub new: bool,
    pub optional: bool,
}

#[derive(Debug, Clone)]
pub struct ChainExpression {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct SpreadElement {
    pub span: Span,
    pub argument: Expression,
}

#[derive(Debug, Clone)]
pub struct StaticMemberExpression {
    pub span: Span,
    pub object: Expression,
    pub property: Identifier,
}

#[derive(Debug, Clone)]
pub struct ComputedMemberExpression {
    pub span: Span,
    pub object: Expression,
    pub property: Expression,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
    pub symbol_id: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct Elision {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub span: Span,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub span: Span,
    pub properties: Vec<Expression>,
    pub trailing_comma: bool,
}

#[derive(Debug, Clone)]
pub struct SetExpression {
    pub span: Span,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct TupleExpression {
    pub span: Span,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ObjectProperty {
    pub span: Span,
    pub kind: PropertyKind,
    pub key: Expression,
    pub value: Expression,
    pub method: bool,
    pub shorthand: bool,
    pub computed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropertyKind {
    Init = 0,
    Get = 1,
    Set = 2,
}

#[derive(Debug, Clone)]
pub struct AwaitExpression {
    pub span: Span,
    pub argument: Expression,
}

#[derive(Debug, Clone)]
pub struct YieldExpression {
    pub span: Span,
    pub argument: Option<Expression>,
    pub delegate: bool,
}

#[derive(Debug, Clone)]
pub struct LambdaExpression {
    pub span: Span,
    pub params: Vec<Expression>,
    pub body: FunctionBody,
    pub r#async: bool,
    // Like `lambda x: x`, or `(x) => x` no explicit return
    pub expression: bool,
}

#[derive(Debug, Clone)]
pub struct SequenceExpression {
    pub span: Span,
    pub expressions: Vec<Expression>,
}
