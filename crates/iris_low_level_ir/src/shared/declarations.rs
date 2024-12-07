use crate::shared::derivatives::Directive;
use crate::shared::expressions::{Expression, Identifier};
use crate::shared::span::Span;
use crate::shared::statements::{BlockStatement, Statement};

#[derive(Debug, Clone)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
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
    pub id: Expression,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum VariableDeclarationKind {
    Var,
    Const,
    Let,
    Using,
    AwaitUsing,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub id: Identifier,
    pub params: Vec<FormalParameter>,
    pub body: FunctionBody,
    pub r#async: bool,
    pub generator: bool,
    pub decorators: Vec<Decorator>
}

#[derive(Debug, Clone)]
pub struct FormalParameter {
    pub span: Span,
    pub id: Expression,
    pub init: Option<Expression>,
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
    pub value: FunctionDeclaration,
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
