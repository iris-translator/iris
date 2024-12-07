use crate::shared::derivatives::{Directive, Hashbang};
use crate::shared::span::Span;
use crate::shared::statements::Statement;

#[derive(Debug, Clone)]
pub struct Program {
    pub span: Span,
    pub directives: Vec<Directive>,
    pub hashbang: Option<Hashbang>,
    pub body: Vec<Statement>,
}
