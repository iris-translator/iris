use std::mem;
use crate::toolchain::semantic::SymbolTable;
use crate::toolchain::traverse::{Traverse, TraverseCtx};
use crate::toolchain::utils::constant_evaluation::ConstantEvaluation;
use crate::toolchain::index::Idx;
use iris_low_level_ir::shared::*;
use anyhow::Result;
use oxc::ast::match_expression;
use crate::toolchain::ast::AstBuilder;
use crate::toolchain::span::SPAN;
use crate::toolchain::allocator::CloneIn;

pub struct IrisTraverseToIR<'a> {
    pub ast: &'a AstBuilder<'a>,
    pub ir: Program,
    pub symbols: SymbolTable,
}

impl<'a> Traverse<'a> for IrisTraverseToIR<'a> {
    fn enter_program(&mut self, node: &mut oxc::ast::ast::Program<'a>, ctx: &mut TraverseCtx<'a>) {
        todo!()
    }

    fn enter_expression(&mut self, node: &mut oxc::ast::ast::Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        todo!()
    }
}

impl<'a> IrisTraverseToIR<'_> {
    pub fn trans_expression(&self, it: &oxc::ast::ast::Expression) -> Expression {
        match it {
            oxc::ast::ast::Expression::BooleanLiteral(it) => Expression::BooleanLiteral(Self::trans_boolean_literal(it)),
            oxc::ast::ast::Expression::NullLiteral(it) => Expression::NullLiteral(Self::trans_null_literal(it)),
            oxc::ast::ast::Expression::NumericLiteral(it) => Expression::NumericLiteral(Self::trans_numeric_literal(it)),
            oxc::ast::ast::Expression::BigIntLiteral(it) => Expression::NumericLiteral(Self::trans_bigint_literal(it)),
            oxc::ast::ast::Expression::RegExpLiteral(it) => unimplemented!("RegExpLiteral"),
            oxc::ast::ast::Expression::StringLiteral(it) => Expression::StringLiteral(Self::trans_string_literal(it)),
            oxc::ast::ast::Expression::TemplateLiteral(it) => Expression::TemplateLiteral(Box::new(Self::trans_template_literal(it))),
            oxc::ast::ast::Expression::ArrayExpression(it) => Expression::ArrayExpression(Box::new(self.trans_array_expression(it))),
            oxc::ast::ast::Expression::ArrowFunctionExpression(it) => unimplemented!("ArrowFunctionExpression"),
            oxc::ast::ast::Expression::AssignmentExpression(it) => Expression::AssignmentExpression(Box::new(self.trans_assignment_expression(it).unwrap())),
            _ => unimplemented!(),
        }
    }

    pub fn trans_boolean_literal(it: &oxc::ast::ast::BooleanLiteral) -> BooleanLiteral {
        BooleanLiteral {
            span: it.span.into(),
            value: it.value,
        }
    }

    pub fn trans_null_literal(it: &oxc::ast::ast::NullLiteral) -> NullLiteral {
        NullLiteral {
            span: it.span.into()
        }
    }

    pub fn trans_numeric_literal(it: &oxc::ast::ast::NumericLiteral) -> NumericLiteral {
        NumericLiteral {
            span: it.span.into(),
            value: it.value,
            raw: it.raw.to_string(),
            base: match it.base {
                oxc::ast::ast::NumberBase::Binary => NumberBase::Binary,
                oxc::ast::ast::NumberBase::Octal => NumberBase::Octal,
                oxc::ast::ast::NumberBase::Decimal => NumberBase::Decimal,
                oxc::ast::ast::NumberBase::Hex => NumberBase::Hexadecimal,
                oxc::ast::ast::NumberBase::Float => NumberBase::Float,
            },
        }
    }

    pub fn trans_bigint_literal(it: &oxc::ast::ast::BigIntLiteral) -> NumericLiteral {
        NumericLiteral {
            span: it.span.into(),
            value: it.raw.trim_end_matches('n').parse().unwrap(),
            raw: it.raw.to_string(),
            base: NumberBase::BigInt,
        }
    }

    pub fn trans_regexp_literal(it: &oxc::ast::ast::RegExpLiteral) -> ! {
        unimplemented!()
    }

    pub fn trans_string_literal(it: &oxc::ast::ast::StringLiteral) -> StringLiteral {
        StringLiteral {
            span: it.span.into(),
            value: it.value.to_string(),
            prefix: StringLiteralPrefix::Empty,
        }
    }

    pub fn trans_template_literal(it: &oxc::ast::ast::TemplateLiteral) -> TemplateLiteral {
        todo!()
    }

    pub fn trans_identifier_reference(it: &oxc::ast::ast::IdentifierReference, ctx: &TraverseCtx<'a>) -> Identifier {
        let reference = ctx.symbols().get_reference(it.reference_id());
        Identifier {
            span: it.span.into(),
            name: it.name.to_string(),
            symbol_id: reference.symbol_id().map(|x| x.index()),
        }
    }

    pub fn trans_binding_identifier(it: &oxc::ast::ast::BindingIdentifier, ctx: &TraverseCtx<'a>) -> Identifier {
        Identifier {
            span: it.span.into(),
            name: it.name.to_string(),
            symbol_id: it.symbol_id.get().map(|x| x.index()),
        }
    }

    pub fn trans_array_expression(&self, it: &oxc::ast::ast::ArrayExpression) -> ArrayExpression {
        use oxc::ast::ast::ArrayExpressionElement;
        ArrayExpression {
            span: it.span.into(),
            elements: it.elements.iter().map(|x| match x {
                ArrayExpressionElement::Elision(it) => Expression::Elision(Elision {
                    span: it.span.into(),
                }),
                ArrayExpressionElement::SpreadElement(it) => unimplemented!(),
                expr @ match_expression!(ArrayExpressionElement) => self.trans_expression(&expr.as_expression().unwrap())
            }).collect::<Vec<_>>(),
        }
    }

    pub fn trans_arrow_function_expression(it: &oxc::ast::ast::ArrowFunctionExpression) -> LambdaExpression {
        todo!()
    }

    pub fn trans_assignment_expression(&self, it: &oxc::ast::ast::AssignmentExpression) -> Option<AssignmentExpression> {
        if !matches!(it.operator, oxc::ast::ast::AssignmentOperator::Assign) {
            // First we need to transform the assignment with other operators into a simple assignment with the operator
            let bin = BinaryExpression {
                span: SPAN.into(),
                left: self.trans_expression(it.left.get_expression()?),
                operator: it.operator.to_binary_operator()?.into(),
                right: self.trans_expression(&it.right),
            };
            Some(AssignmentExpression {
                span: it.span.into(),
                target: bin.left.clone(),
                value: Expression::BinaryExpression(Box::new(bin)),
            })
        } else {
            Some(AssignmentExpression {
                span: it.span.into(),
                target: self.trans_expression(it.left.get_expression()?),
                value: self.trans_expression(&it.right.clone_in(self.ast.allocator)),
            })
        }
    }

    pub fn trans_await_expression(&self, it: &oxc::ast::ast::AwaitExpression) -> AwaitExpression {
        AwaitExpression {
            span: it.span.into(),
            argument: self.trans_expression(&it.argument),
        }
    }

    pub fn trans_binary_expression(&self, it: &oxc::ast::ast::BinaryExpression) -> BinaryExpression {
        BinaryExpression {
            span: it.span.into(),
            operator: it.operator.into(),
            left: self.trans_expression(&it.left),
            right: self.trans_expression(&it.right),
        }
    }

    pub fn trans_argument(&self, it: &oxc::ast::ast::Argument) -> Expression {
        use oxc::ast::ast::Argument;
        match it {
            Argument::SpreadElement(it) => Expression::SpreadElement(Box::new(self.trans_spread_element(it))),
            it @ match_expression!(Argument) => self.trans_expression(it.as_expression().unwrap())
        }
    }

    pub fn trans_call_expression(&self, it: &oxc::ast::ast::CallExpression) -> CallExpression {
        CallExpression {
            span: it.span.into(),
            callee: self.trans_expression(&it.callee),
            arguments: it.arguments.iter().map(|x| self.trans_argument(x)).collect::<Vec<_>>(),
            new: false,
            optional: it.optional,
        }
    }

    pub fn trans_new_expression(&self, it: &oxc::ast::ast::NewExpression) -> CallExpression {
        CallExpression {
            span: it.span.into(),
            callee: self.trans_expression(&it.callee),
            arguments: it.arguments.iter().map(|x| self.trans_argument(x)).collect::<Vec<_>>(),
            new: true,
            optional: false,
        }
    }

    pub fn trans_spread_element(&self, it: &oxc::ast::ast::SpreadElement) -> SpreadElement {
        SpreadElement {
            span: it.span.into(),
            argument: self.trans_expression(&it.argument),
        }
    }

    pub fn trans_chain_expression(&self, it: &oxc::ast::ast::ChainExpression) -> ChainExpression {
        unimplemented!()
    }

    pub fn trans_conditional_expression(&self, it: &oxc::ast::ast::ConditionalExpression) -> ConditionalExpression {
        ConditionalExpression {
            span: it.span.into(),
            test: self.trans_expression(&it.test),
            consequent: self.trans_expression(&it.consequent),
            alternate: self.trans_expression(&it.alternate),
        }
    }

    pub fn trans_logical_expression(&self, it: &oxc::ast::ast::LogicalExpression) -> LogicalExpression {
        LogicalExpression {
            span: it.span.into(),
            operator: match it.operator {
                oxc::ast::ast::LogicalOperator::And => LogicalOperator::LogicalAND,
                oxc::ast::ast::LogicalOperator::Or => LogicalOperator::LogicalOR,
                oxc::ast::ast::LogicalOperator::Coalesce => LogicalOperator::Coalesce,
            },
            left: self.trans_expression(&it.left),
            right: self.trans_expression(&it.right),
        }
    }

    pub fn trans_sequence_expression(&self, it: &oxc::ast::ast::SequenceExpression) -> SequenceExpression {
        SequenceExpression {
            span: it.span.into(),
            expressions: it.expressions.iter().map(|x| self.trans_expression(x)).collect::<Vec<_>>(),
        }
    }

    pub fn trans_parenthesized_expression(&self, it: &oxc::ast::ast::ParenthesizedExpression) -> ParenthesizedExpression {
        ParenthesizedExpression {
            span: it.span.into(),
            expression: self.trans_expression(&it.expression),
        }
    }

    pub fn trans_unary_expression(&self, it: &oxc::ast::ast::UnaryExpression) -> UnaryExpression {
        UnaryExpression {
            span: it.span.into(),
            operator: it.operator.into(),
            argument: self.trans_expression(&it.argument),
        }
    }

    pub fn trans_update_expression(&self, it: &oxc::ast::ast::UpdateExpression) -> UpdateExpression {
        UpdateExpression {
            span: it.span.into(),
            increment: matches!(it.operator, oxc::ast::ast::UpdateOperator::Increment),
            target: self.trans_expression(&it.argument.get_expression().unwrap()),
            prefix: it.prefix,
        }
    }

    pub fn trans_yield_expression(&self, it: &oxc::ast::ast::YieldExpression) -> YieldExpression {
        YieldExpression {
            span: it.span.into(),
            argument: it.argument.as_ref().map(|x| self.trans_expression(x)),
            delegate: it.delegate,
        }
    }
}
