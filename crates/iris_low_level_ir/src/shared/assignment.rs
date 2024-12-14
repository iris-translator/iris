use crate::shared::{Expression, Identifier, MemberExpression, Span};

#[derive(Debug, Clone)]
pub struct BindingPattern {
    pub kind: BindingPatternKind,
}

#[derive(Debug, Clone)]
pub enum BindingPatternKind {
    BindingIdentifier(Box<Identifier>),
    ObjectPattern(Box<ObjectPattern>),
    ArrayPattern(Box<ArrayPattern>),
    AssignmentPattern(Box<AssignmentPattern>),
}

#[derive(Debug, Clone)]
pub struct ObjectPattern {
    pub span: Span,
    pub properties: Vec<BindingProperty>,
    pub rest: Option<BindingRestElement>,
}

#[derive(Debug, Clone)]
pub struct BindingProperty {
    pub span: Span,
    pub key: Expression,
    pub value: BindingPattern,
    pub shorthand: bool,
    pub computed: bool,
}

#[derive(Debug, Clone)]
pub struct ArrayPattern {
    pub span: Span,
    pub elements: Vec<Option<BindingPattern>>,
    pub rest: Option<BindingRestElement>,
}

#[derive(Debug, Clone)]
pub struct BindingRestElement {
    pub span: Span,
    pub argument: BindingPattern,
}

#[derive(Debug, Clone)]
pub struct AssignmentPattern {
    pub span: Span,
    pub left: BindingPattern,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct ArrayAssignmentTarget {
    pub span: Span,
    pub elements: Vec<Option<AssignmentTarget>>,
    pub rest: Option<AssignmentTargetRest>,
    pub trailing_comma: bool,
}

#[derive(Debug, Clone)]
pub struct ObjectAssignmentTarget {
    pub span: Span,
    pub properties: Vec<AssignmentTargetProperty>,
    pub rest: Option<AssignmentTargetRest>,
}

#[derive(Debug, Clone)]
pub struct AssignmentTargetWithDefault {
    pub span: Span,
    pub binding: AssignmentTarget,
    pub init: Expression,
}
#[derive(Debug, Clone)]
pub struct AssignmentTargetRest {
    pub span: Span,
    pub target: AssignmentTarget,
}

#[derive(Debug, Clone)]
pub enum AssignmentTarget {
    AssignmentTargetIdentifier(Identifier),
    MemberExpression(Box<MemberExpression>),
    ArrayAssignmentTarget(Box<ArrayAssignmentTarget>),
    ObjectAssignmentTarget(Box<ObjectAssignmentTarget>),
    AssignmentTargetWithDefault(Box<AssignmentTargetWithDefault>),
}

impl AssignmentTarget {
    pub fn into_expression(self) -> Expression {
        match self {
            AssignmentTarget::AssignmentTargetIdentifier(identifier) => Expression::Identifier(identifier),
            AssignmentTarget::MemberExpression(member_expression) => Expression::MemberExpression(member_expression),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignmentTargetProperty {
    AssignmentTargetPropertyIdentifier(Box<AssignmentTargetPropertyIdentifier>),
    AssignmentTargetPropertyProperty(Box<AssignmentTargetPropertyProperty>),
}

#[derive(Debug, Clone)]
pub struct AssignmentTargetPropertyIdentifier {
    pub span: Span,
    pub binding: Identifier,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignmentTargetPropertyProperty {
    pub span: Span,
    pub name: Expression,
    pub binding: AssignmentTarget,
}
