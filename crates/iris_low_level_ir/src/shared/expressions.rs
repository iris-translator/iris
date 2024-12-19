//! Part of this file is ported from [oxc](https://github.com/oxc-project/oxc/blob/main/oxc_ast/src/ast) and [ruff](https://github.com/astral-sh/ruff/tree/main/crates/ruff_python_ast/src).

use crate::shared::declarations::FunctionBody;
use crate::shared::literals::*;
use crate::shared::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::shared::span::Span;
use crate::shared::{AssignmentTarget, FormalParameters, Function, VariableDeclaration};

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

    AwaitExpression(Box<AwaitExpression>),
    YieldExpression(Box<YieldExpression>),
    UnaryExpression(Box<UnaryExpression>),
    BinaryExpression(Box<BinaryExpression>),
    LogicalExpression(Box<LogicalExpression>),
    FunctionExpression(Box<Function>),
    ConditionalExpression(Box<ConditionalExpression>),
    AssignmentExpression(Box<AssignmentExpression>),
    UpdateExpression(Box<UpdateExpression>),
    CallExpression(Box<CallExpression>),
    ChainExpression(Box<ChainExpression>),
    LambdaExpression(Box<LambdaExpression>),
    ParenthesizedExpression(Box<ParenthesizedExpression>),

    MemberExpression(Box<MemberExpression>),

    // Specific, for ease of use
    SpreadElement(Box<SpreadElement>),
    Elision(Elision),
    ObjectProperty(Box<ObjectProperty>),
    VariableDeclaration(Box<VariableDeclaration>),
    AssignmentTarget(Box<AssignmentTarget>),
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub span: Span,
    pub operator: UnaryOperator,
    pub argument: Expression,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub span: Span,
    pub operator: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub span: Span,
    pub operator: LogicalOperator,
    pub left: Expression,
    pub right: Expression,
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
    pub target: AssignmentTarget,
    pub value: Expression,
}

// JavaScript-specific / C-like
#[derive(Debug, Clone)]
pub struct UpdateExpression {
    pub span: Span,
    // Otherwise it's a decrement
    pub increment: bool,
    pub target: AssignmentTarget,
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
pub struct MemberExpression {
    pub span: Span,
    pub object: Expression,
    pub property: Expression,
    pub computed: bool,
    pub private: bool,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
    pub symbol_id: Option<usize>,
    pub private: bool,
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

impl From<oxc::ast::ast::PropertyKind> for PropertyKind {
    fn from(value: oxc::ast::ast::PropertyKind) -> Self {
        match value {
            oxc::ast::ast::PropertyKind::Init => PropertyKind::Init,
            oxc::ast::ast::PropertyKind::Get => PropertyKind::Get,
            oxc::ast::ast::PropertyKind::Set => PropertyKind::Set,
        }
    }
}

impl Into<oxc::ast::ast::PropertyKind> for PropertyKind {
    fn into(self) -> oxc::ast::ast::PropertyKind {
        match self {
            PropertyKind::Init => oxc::ast::ast::PropertyKind::Init,
            PropertyKind::Get => oxc::ast::ast::PropertyKind::Get,
            PropertyKind::Set => oxc::ast::ast::PropertyKind::Set,
        }
    }
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
    pub params: FormalParameters,
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
