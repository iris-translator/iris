use iris_low_level_ir::shared::*;
use oxc::allocator::Allocator;
use oxc::ast::ast::{
    Argument as OxcArgument, ArrayExpressionElement, BigintBase, Expression as OxcExpression,
    PropertyKey, TemplateElementValue,
};
use oxc::ast::{AstBuilder, NONE};
use oxc::span::SPAN;

pub struct EcmaCharacterization<'a> {
    pub alloc: &'a Allocator,
    pub builder: AstBuilder<'a>,
    pub ast: Option<oxc::ast::ast::Program<'a>>,
}

impl<'a> EcmaCharacterization<'a> {
    pub fn new(alloc: &'a Allocator) -> Self {
        EcmaCharacterization {
            alloc,
            builder: AstBuilder::new(alloc),
            ast: None,
        }
    }

    pub fn trans_statement(&self, statement: &mut Statement) -> oxc::ast::ast::Statement {
        match statement {
            Statement::BlockStatement(block) => self.trans_block_statement(block),
            _ => unimplemented!(),
        }
    }

    pub fn trans_block_statement(&self, block: &mut BlockStatement) -> oxc::ast::ast::Statement {
        self.builder.statement_block(
            SPAN,
            self.builder.vec_from_iter(
                block
                    .statements
                    .iter_mut()
                    .map(|stmt| self.trans_statement(stmt)),
            ),
        )
    }

    pub fn trans_expression(&self, expression: &Expression) -> OxcExpression {
        match expression {
            Expression::BooleanLiteral(boolean_literal) => {
                self.trans_boolean_literal(boolean_literal)
            }
            Expression::NullLiteral(null_literal) => self.trans_null_literal(null_literal),
            Expression::NumericLiteral(numeric_literal) => {
                self.trans_numeric_literal(numeric_literal)
            }
            Expression::StringLiteral(string_literal) => self.trans_string_literal(string_literal),
            Expression::TemplateLiteral(template_literal) => {
                self.trans_template_literal(template_literal)
            }
            Expression::ArrayExpression(array_expression) => {
                self.trans_array_expression(array_expression)
            }
            Expression::ObjectExpression(object_expression) => {
                self.trans_object_expression(object_expression)
            }
            Expression::SetExpression(set_expression) => self.trans_set_expression(set_expression),
            Expression::TupleExpression(tuple_expression) => {
                self.trans_tuple_expression(tuple_expression)
            }
            Expression::SequenceExpression(sequence_expression) => {
                self.trans_sequence_expression(sequence_expression)
            }
            Expression::ParenthesizedExpression(parenthesized_expression) => {
                self.trans_parenthesized_expression(parenthesized_expression)
            }
            Expression::AwaitExpression(await_expression) => {
                self.trans_await_expression(await_expression)
            }
            Expression::YieldExpression(yield_expression) => {
                self.trans_yield_expression(yield_expression)
            }
            Expression::UnaryExpression(unary_expression) => {
                self.trans_unary_operation(unary_expression)
            }
            Expression::LogicalExpression(logical_expression) => {
                self.trans_logical_expression(logical_expression)
            }
            Expression::ConditionalExpression(cond_expression) => {
                self.trans_conditional_expression(cond_expression)
            }
            Expression::CallExpression(call_expression) => {
                self.trans_call_expression(call_expression)
            }
            Expression::BinaryExpression(binary_expression) => {
                self.trans_binary_expression(binary_expression)
            }
            _ => unimplemented!(),
        }
    }

    pub fn trans_boolean_literal(&self, boolean_literal: &BooleanLiteral) -> OxcExpression {
        self.builder
            .expression_boolean_literal(SPAN, boolean_literal.value)
    }

    pub fn trans_null_literal(&self, _: &NullLiteral) -> OxcExpression {
        self.builder.expression_null_literal(SPAN)
    }

    pub fn trans_numeric_literal(&self, numeric_literal: &NumericLiteral) -> OxcExpression {
        if matches!(numeric_literal.base, NumberBase::BigInt) {
            self.builder.expression_big_int_literal(
                SPAN,
                self.builder.atom(numeric_literal.raw.as_str()),
                BigintBase::Decimal,
            )
        } else {
            self.builder.expression_numeric_literal(
                SPAN,
                numeric_literal.value,
                Some(self.builder.atom(numeric_literal.raw.as_str())),
                numeric_literal.base.into(),
            )
        }
    }

    pub fn trans_string_literal(&self, string_literal: &StringLiteral) -> OxcExpression {
        self.builder.expression_string_literal(
            SPAN,
            self.builder.atom(string_literal.value.as_str()),
            Some(self.builder.atom(string_literal.value.as_str())),
        )
    }

    pub fn trans_template_literal(&self, template_literal: &TemplateLiteral) -> OxcExpression {
        self.builder.expression_template_literal(
            SPAN,
            self.builder
                .vec_from_iter(
                    template_literal
                        .quasis
                        .iter()
                        .enumerate()
                        .map(|(idx, quasi)| {
                            self.builder.template_element(
                                SPAN,
                                idx == template_literal.quasis.len(),
                                TemplateElementValue {
                                    raw: self.builder.atom(quasi.as_str()),
                                    cooked: Some(self.builder.atom(quasi.as_str())),
                                },
                            )
                        }),
                ),
            self.builder.vec_from_iter(
                template_literal
                    .expressions
                    .iter()
                    .map(|expr| self.trans_expression(expr)),
            ),
        )
    }

    pub fn trans_array_expression(&self, array_expression: &ArrayExpression) -> OxcExpression {
        self.builder.expression_array(
            SPAN,
            self.builder
                .vec_from_iter(array_expression.elements.iter().map(|expr| match &expr {
                    Expression::Elision(_) => self.builder.array_expression_element_elision(SPAN),
                    Expression::SpreadElement(spread) => {
                        self.builder.array_expression_element_spread_element(
                            SPAN,
                            self.trans_expression(&spread.argument),
                        )
                    }
                    _ => ArrayExpressionElement::from(self.trans_expression(expr)),
                })),
            None,
        )
    }

    pub fn trans_object_expression(&self, object_expression: &ObjectExpression) -> OxcExpression {
        self.builder.expression_object(
            SPAN,
            self.builder
                .vec_from_iter(object_expression.properties.iter().map(|expr| match expr {
                    Expression::SpreadElement(spread) => {
                        self.builder.object_property_kind_spread_element(
                            SPAN,
                            self.trans_expression(&spread.argument),
                        )
                    }
                    Expression::ObjectProperty(property) => {
                        self.builder.object_property_kind_object_property(
                            SPAN,
                            property.kind.into(),
                            match &property.key {
                                Expression::Identifier(ident) => {
                                    if ident.private {
                                        self.builder.property_key_private_identifier(
                                            SPAN,
                                            self.builder.atom(&ident.name),
                                        )
                                    } else {
                                        self.builder.property_key_identifier_name(
                                            SPAN,
                                            self.builder.atom(&ident.name),
                                        )
                                    }
                                }
                                _ => PropertyKey::from(self.trans_expression(&property.key)),
                            },
                            self.trans_expression(&property.value),
                            property.method,
                            property.shorthand,
                            property.computed,
                        )
                    }
                    _ => unreachable!(),
                })),
            None,
        )
    }

    pub fn trans_tuple_expression(&self, tuple_expression: &TupleExpression) -> OxcExpression {
        eprintln!("Warning: tuple proposal is still in stage 2. This may not work as expected.");
        self.builder.expression_new(
            SPAN,
            self.builder.expression_identifier_reference(SPAN, "Tuple"),
            self.builder.vec1(oxc::ast::ast::Argument::ArrayExpression(
                self.builder.alloc(
                    self.builder.array_expression(
                        SPAN,
                        self.builder
                            .vec_from_iter(tuple_expression.elements.iter().map(
                                |expr| match &expr {
                                    Expression::Elision(_) => {
                                        self.builder.array_expression_element_elision(SPAN)
                                    }
                                    Expression::SpreadElement(spread) => {
                                        self.builder.array_expression_element_spread_element(
                                            SPAN,
                                            self.trans_expression(&spread.argument),
                                        )
                                    }
                                    _ => ArrayExpressionElement::from(self.trans_expression(expr)),
                                },
                            )),
                        None,
                    ),
                ),
            )),
            NONE,
        )
    }

    pub fn trans_set_expression(&self, set_expression: &SetExpression) -> OxcExpression {
        self.builder.expression_new(
            SPAN,
            self.builder.expression_identifier_reference(SPAN, "Set"),
            self.builder.vec1(oxc::ast::ast::Argument::ArrayExpression(
                self.builder.alloc(
                    self.builder.array_expression(
                        SPAN,
                        self.builder.vec_from_iter(
                            set_expression.elements.iter().map(|expr| {
                                ArrayExpressionElement::from(self.trans_expression(expr))
                            }),
                        ),
                        None,
                    ),
                ),
            )),
            NONE,
        )
    }

    pub fn trans_sequence_expression(
        &self,
        sequence_expression: &SequenceExpression,
    ) -> OxcExpression {
        self.builder.expression_sequence(
            SPAN,
            self.builder.vec_from_iter(
                sequence_expression
                    .expressions
                    .iter()
                    .map(|expr| self.trans_expression(expr)),
            ),
        )
    }

    pub fn trans_parenthesized_expression(
        &self,
        parenthesized_expression: &ParenthesizedExpression,
    ) -> OxcExpression {
        self.builder.expression_parenthesized(
            SPAN,
            self.trans_expression(&parenthesized_expression.expression),
        )
    }

    pub fn trans_identifier(&self, identifier: &Identifier) -> OxcExpression {
        self.builder
            .expression_identifier_reference(SPAN, self.builder.atom(&identifier.name))
    }

    pub fn trans_await_expression(&self, await_expression: &AwaitExpression) -> OxcExpression {
        self.builder
            .expression_await(SPAN, self.trans_expression(&await_expression.argument))
    }

    pub fn trans_yield_expression(&self, yield_expression: &YieldExpression) -> OxcExpression {
        self.builder.expression_yield(
            SPAN,
            yield_expression.delegate,
            yield_expression
                .argument
                .as_ref()
                .map(|expr| self.trans_expression(expr)),
        )
    }

    pub fn trans_unary_expression(&self, unary_expression: &UnaryExpression) -> OxcExpression {
        self.builder.expression_unary(
            SPAN,
            unary_expression.operator.into(),
            self.trans_expression(&unary_expression.argument),
        )
    }

    pub fn trans_logical_expression(
        &self,
        logical_expression: &LogicalExpression,
    ) -> OxcExpression {
        self.builder.expression_logical(
            SPAN,
            self.trans_expression(&logical_expression.left),
            logical_expression.operator.into(),
            self.trans_expression(&logical_expression.right),
        )
    }

    pub fn trans_call_expression(&self, call_expression: &CallExpression) -> OxcExpression {
        if call_expression.new {
            self.builder.expression_new(
                SPAN,
                self.trans_expression(&call_expression.callee),
                self.builder.vec_from_iter(
                    call_expression
                        .arguments
                        .iter()
                        .map(|expr| self.trans_argument(expr)),
                ),
                NONE,
            )
        } else {
            self.builder.expression_call(
                SPAN,
                self.trans_expression(&call_expression.callee),
                NONE,
                self.builder.vec_from_iter(
                    call_expression
                        .arguments
                        .iter()
                        .map(|expr| self.trans_argument(expr)),
                ),
                call_expression.optional,
            )
        }
    }

    pub fn trans_argument(&self, argument: &Expression) -> oxc::ast::ast::Argument {
        match argument {
            Expression::SpreadElement(spread) => self
                .builder
                .argument_spread_element(SPAN, self.trans_expression(&spread.argument)),
            _ => oxc::ast::ast::Argument::from(self.trans_expression(argument)),
        }
    }

    pub fn trans_conditional_expression(
        &self,
        cond_expression: &ConditionalExpression,
    ) -> OxcExpression {
        self.builder.expression_conditional(
            SPAN,
            self.trans_expression(&cond_expression.test),
            self.trans_expression(&cond_expression.consequent),
            self.trans_expression(&cond_expression.alternate),
        )
    }

    pub fn trans_unary_operation(&self, unary_expression: &UnaryExpression) -> OxcExpression {
        self.builder.expression_unary(
            SPAN,
            unary_expression.operator.into(),
            self.trans_expression(&unary_expression.argument),
        )
    }

    pub fn trans_binary_expression(&self, binary_expression: &BinaryExpression) -> OxcExpression {
        match binary_expression.operator {
            BinaryOperator::MatMultiplication => self.builder.expression_call(
                SPAN,
                self.builder.expression_identifier_reference(SPAN, "matmul"),
                NONE,
                self.builder.vec_from_array([
                    OxcArgument::from(self.trans_expression(&binary_expression.left)),
                    OxcArgument::from(self.trans_expression(&binary_expression.right)),
                ]),
                false,
            ),
            _ if binary_expression.operator.is_pattern_matching() => {
                panic!("Error: Pattern matching is still stage 1.")
            }
            _ => self.builder.expression_binary(
                SPAN,
                self.trans_expression(&binary_expression.left),
                binary_expression.operator.into(),
                self.trans_expression(&binary_expression.right),
            ),
        }
    }
}
