use crate::shared::derivatives::Directive;
use crate::shared::expressions::{Expression, Identifier};
use crate::shared::span::Span;
use crate::shared::statements::{BlockStatement, Statement};
use crate::shared::{BindingPattern, BindingRestElement};

#[derive(Debug, Clone)]
pub enum Declaration {
    VariableDeclaration(Box<VariableDeclaration>),
    FunctionDeclaration(Box<Function>),
    ClassDeclaration(Box<ClassDeclaration>),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub span: Span,
    pub kind: VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub span: Span,
    pub id: BindingPattern,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum VariableDeclarationKind {
    Var,
    Const,
    Let,
    Using,
    AwaitUsing,

    /// Python does not have these kinds of declarations. So we can handle it as `ambiguous` for now, despite that it is similar to `var` in JavaScript.
    Ambiguous,
}

impl From<oxc::ast::ast::VariableDeclarationKind> for VariableDeclarationKind {
    fn from(kind: oxc::ast::ast::VariableDeclarationKind) -> Self {
        match kind {
            oxc::ast::ast::VariableDeclarationKind::Var => Self::Var,
            oxc::ast::ast::VariableDeclarationKind::Const => Self::Const,
            oxc::ast::ast::VariableDeclarationKind::Let => Self::Let,
            oxc::ast::ast::VariableDeclarationKind::Using => Self::Using,
            oxc::ast::ast::VariableDeclarationKind::AwaitUsing => Self::AwaitUsing,
        }
    }
}

impl Into<oxc::ast::ast::VariableDeclarationKind> for VariableDeclarationKind {
    fn into(self) -> oxc::ast::ast::VariableDeclarationKind {
        match self {
            Self::Var | Self::Ambiguous => oxc::ast::ast::VariableDeclarationKind::Var,
            Self::Const => oxc::ast::ast::VariableDeclarationKind::Const,
            Self::Let => oxc::ast::ast::VariableDeclarationKind::Let,
            Self::Using => oxc::ast::ast::VariableDeclarationKind::Using,
            Self::AwaitUsing => oxc::ast::ast::VariableDeclarationKind::AwaitUsing,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub r#type: FunctionType,
    pub id: Option<Identifier>,
    pub params: FormalParameters,
    pub body: Option<FunctionBody>,
    pub r#async: bool,
    pub generator: bool,
    pub decorators: Vec<Decorator>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionType {
    FunctionDeclaration,
    FunctionExpression,
}

impl From<oxc::ast::ast::FunctionType> for FunctionType {
    fn from(kind: oxc::ast::ast::FunctionType) -> Self {
        match kind {
            oxc::ast::ast::FunctionType::FunctionDeclaration => Self::FunctionDeclaration,
            oxc::ast::ast::FunctionType::FunctionExpression => Self::FunctionExpression,
            _ => unimplemented!(),
        }
    }
}

impl Into<oxc::ast::ast::FunctionType> for FunctionType {
    fn into(self) -> oxc::ast::ast::FunctionType {
        match self {
            Self::FunctionDeclaration => oxc::ast::ast::FunctionType::FunctionDeclaration,
            Self::FunctionExpression => oxc::ast::ast::FunctionType::FunctionExpression,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FormalParameter {
    pub span: Span,
    pub id: BindingPattern,
    pub decorators: Vec<Decorator>,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum FormalParameterKind {
    FormalParameter,
    UniqueFormalParameters,
    ArrowFormalParameters,
    Signature,
}

impl From<oxc::ast::ast::FormalParameterKind> for FormalParameterKind {
    fn from(kind: oxc::ast::ast::FormalParameterKind) -> Self {
        match kind {
            oxc::ast::ast::FormalParameterKind::FormalParameter => Self::FormalParameter,
            oxc::ast::ast::FormalParameterKind::UniqueFormalParameters => {
                Self::UniqueFormalParameters
            }
            oxc::ast::ast::FormalParameterKind::ArrowFormalParameters => {
                Self::ArrowFormalParameters
            }
            oxc::ast::ast::FormalParameterKind::Signature => Self::Signature,
        }
    }
}

impl From<FormalParameterKind> for oxc::ast::ast::FormalParameterKind {
    fn from(kind: FormalParameterKind) -> Self {
        match kind {
            FormalParameterKind::FormalParameter => oxc::ast::ast::FormalParameterKind::FormalParameter,
            FormalParameterKind::UniqueFormalParameters => {
                oxc::ast::ast::FormalParameterKind::UniqueFormalParameters
            }
            FormalParameterKind::ArrowFormalParameters => {
                oxc::ast::ast::FormalParameterKind::ArrowFormalParameters
            }
            FormalParameterKind::Signature => oxc::ast::ast::FormalParameterKind::Signature,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FormalParameters {
    pub span: Span,
    pub kind: FormalParameterKind,
    pub items: Vec<FormalParameter>,
    pub rest: Option<BindingRestElement>,
}

#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub span: Span,
    pub statements: Vec<Statement>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone)]
pub struct Decorator {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    pub span: Span,
    pub id: Identifier,
    pub body: ClassBody,
    pub super_class: Option<Expression>,
    pub decorators: Vec<Decorator>,
}

#[derive(Debug, Clone)]
pub struct ClassBody {
    pub span: Span,
    pub body: Vec<ClassElement>,
}

#[derive(Debug, Clone)]
pub enum ClassElement {
    StaticBlock(Box<StaticBlock>),
    MethodDefinition(MethodDefinition),
    PropertyDefinition(PropertyDefinition),
}

#[derive(Debug, Clone)]
pub struct StaticBlock {
    pub span: Span,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct MethodDefinition {
    pub span: Span,
    pub kind: MethodDefinitionKind,
    pub decorators: Vec<Decorator>,
    pub value: Function,
    pub computed: bool,
    pub r#static: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum MethodDefinitionKind {
    /// - `constructor() {}` in JavaScript
    /// - `def __init__(self): pass` in Python
    Constructor,
    Method,
    Get,
    Set,

    /// Like traits in Rust, interfaces in TypeScript, etc.
    /// - Python: like `__str__`, `__repr__`, etc.
    /// - JavaScript: like `Symbol.iterator`, `Symbol.toStringTag`, etc.
    Prototype,
}

#[derive(Debug, Clone)]
pub struct PropertyDefinition {
    pub span: Span,
    pub key: Expression,
    pub value: Expression,
    pub computed: bool,
    pub r#static: bool,
}
