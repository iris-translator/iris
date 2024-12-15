//! Part of this file is ported from [oxc](https://github.com/oxc-project/oxc/blob/main/oxc_ast/src/ast) and [ruff](https://github.com/astral-sh/ruff/tree/main/crates/ruff_python_ast/src).
use crate::shared::expressions::{Expression, Identifier};
use crate::shared::span::Span;
use crate::shared::{VariableDeclaration, Function};
use crate::shared::module_declarations::{ExportAllDeclaration, ExportDefaultDeclaration, ExportNamedDeclaration, ImportDeclaration};

#[derive(Debug, Clone)]
pub enum Statement {
    BlockStatement(Box<BlockStatement>),
    BreakStatement(Box<BreakStatement>),
    ContinueStatement(ContinueStatement),
    DebuggerStatement(DebuggerStatement),
    EmptyStatement(EmptyStatement),
    ExpressionStatement(Box<ExpressionStatement>),
    ForStatement(Box<ForStatement>),
    IfStatement(Box<IfStatement>),
    ReturnStatement(Box<ReturnStatement>),
    SwitchStatement(Box<SwitchStatement>),
    ThrowStatement(Box<ThrowStatement>),
    TryStatement(Box<TryStatement>),
    WhileStatement(Box<WhileStatement>),
    ScopeStatement(Box<ScopeStatement>),

    VariableDeclaration(Box<VariableDeclaration>),
    FunctionDeclaration(Box<Function>),

    ImportDeclaration(Box<ImportDeclaration>),
    ExportAllDeclaration(Box<ExportAllDeclaration>),
    ExportDefaultDeclaration(Box<ExportDefaultDeclaration>),
    ExportNamedDeclaration(Box<ExportNamedDeclaration>),
}

#[derive(Debug, Clone)]
pub struct EmptyStatement {
    // Also equivalent to Python `pass` statement.
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub span: Span,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct BreakStatement {
    pub span: Span,
    // We currently don't support labels since it's JavaScript / C-like feature, which is not applicable for universal code.
}

#[derive(Debug, Clone)]
pub struct DebuggerStatement {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ContinueStatement {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub span: Span,
    pub test: Expression,
    pub consequent: Statement,
    pub alternate: Option<Statement>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub span: Span,
    pub argument: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub span: Span,
    pub left: Expression,
    pub right: Expression,
    pub body: Statement,

    // JavaScript-specific: we merge `ForOf` and `ForIn`, since we don't have to care about the iteration type.
    // If it is for-of, the `iteration` field will be true.
    pub iteration: bool,
    pub r#await: bool,
}

#[derive(Debug, Clone)]
pub struct ThrowStatement {
    pub span: Span,
    pub argument: Expression,
}

#[derive(Debug, Clone)]
pub struct TryStatement {
    pub span: Span,
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub span: Span,
    pub param: Expression,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub span: Span,
    pub test: Expression,
    pub body: Statement,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct SwitchStatement {
    pub span: Span,
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub span: Span,
    pub test: Option<Expression>,
    pub consequent: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct ScopeStatement {
    pub span: Span,
    pub r#type: ScopeType,
    pub identifiers: Vec<Identifier>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeType {
    Global,
    Nonlocal
}

