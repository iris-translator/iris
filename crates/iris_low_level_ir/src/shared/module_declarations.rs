use either::Either;
use crate::shared::{Expression, Identifier, Span, Statement, StringLiteral};

#[derive(Debug, Clone)]
pub struct ImportDeclaration {
    pub span: Span,
    pub specifiers: Option<Vec<ImportDeclarationSpecifier>>,
    pub source: StringLiteral,
}

#[derive(Debug, Clone)]
pub enum ImportDeclarationSpecifier {
    ImportSpecifier(Box<ImportSpecifier>),
    ImportDefaultSpecifier(Box<ImportDefaultSpecifier>),
    ImportNamespaceSpecifier(Box<ImportNamespaceSpecifier>),
}

#[derive(Debug, Clone)]
pub struct ImportSpecifier {
    pub span: Span,
    pub imported: Expression,
    pub local: Identifier,
}

#[derive(Debug, Clone)]
pub struct ImportDefaultSpecifier {
    pub span: Span,
    pub local: Identifier,
}

#[derive(Debug, Clone)]
pub struct ImportNamespaceSpecifier {
    pub span: Span,
    pub local: Identifier,
}

#[derive(Debug, Clone)]
pub struct ExportNamedDeclaration {
    pub span: Span,
    // For the sake of simplicity, the `statement` contains the entire declaration.
    pub declaration: Option<Statement>,
    pub specifiers: Vec<ExportSpecifier>,
    pub source: Option<StringLiteral>,
}

#[derive(Debug, Clone)]
pub struct ExportSpecifier {
    pub span: Span,
    pub exported: Expression,
    pub local: Expression,
}

#[derive(Debug, Clone)]
pub struct ExportDefaultDeclaration {
    pub span: Span,
    // Here, the statement represents FunctionDeclaration and ClassDeclaration.
    pub declaration: Box<Either<Expression, Statement>>,
    pub local: Expression,
}

#[derive(Debug, Clone)]
pub struct ExportAllDeclaration {
    pub span: Span,
    pub exported: Option<Expression>,
    pub source: StringLiteral,
}