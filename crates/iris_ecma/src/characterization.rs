use iris_low_level_ir::shared::*;
use oxc::allocator::Allocator;
use oxc::ast::ast::{
    Argument as OxcArgument, ArrayExpressionElement, AssignmentOperator,
    AssignmentTarget as OxcAssignmentTarget, BigintBase, Expression as OxcExpression,
    ForStatementLeft, MemberExpression as OxcMemberExpression, PropertyKey, SimpleAssignmentTarget,
    Statement as OxcStatement, TemplateElementValue,
};
use oxc::ast::{AstBuilder, NONE};
use oxc::span::SPAN;
use oxc::syntax::operator::UpdateOperator;

pub struct EcmaCharacterization<'a> {
    pub alloc: &'a Allocator,
    pub builder: AstBuilder<'a>,
    pub ast: Option<oxc::ast::ast::Program<'a>>,
}

impl<'a> EcmaCharacterization<'a> {
    pub fn new(alloc: &'a Allocator) -> Self {
        EcmaCharacterization { alloc, builder: AstBuilder::new(alloc), ast: None }
    }

    pub fn trans_statement(&self, statement: &Statement) -> OxcStatement {
        match statement {
            Statement::BlockStatement(block) => self.trans_block_statement(block),

            Statement::VariableDeclaration(declaration) => OxcStatement::VariableDeclaration(
                self.builder.alloc(self.trans_variable_declaration(declaration)),
            ),
            _ => unimplemented!(),
        }
    }

    pub fn trans_block_statement(&self, block: &BlockStatement) -> OxcStatement {
        self.builder.statement_block(
            SPAN,
            self.builder
                .vec_from_iter(block.statements.iter().map(|stmt| self.trans_statement(stmt))),
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
            Expression::StringLiteral(string_literal) => OxcExpression::StringLiteral(
                self.builder.alloc(self.trans_string_literal(string_literal)),
            ),
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
            Expression::MemberExpression(member_expression) => {
                OxcExpression::from(self.trans_member_expression(member_expression))
            }
            Expression::AssignmentExpression(assignment_expression) => {
                self.trans_assignment_expression(assignment_expression)
            }
            _ => unimplemented!(),
        }
    }

    pub fn trans_boolean_literal(&self, boolean_literal: &BooleanLiteral) -> OxcExpression {
        self.builder.expression_boolean_literal(SPAN, boolean_literal.value)
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

    pub fn trans_string_literal(
        &self,
        string_literal: &StringLiteral,
    ) -> oxc::ast::ast::StringLiteral {
        self.builder.string_literal(
            SPAN,
            self.builder.atom(string_literal.value.as_str()),
            Some(self.builder.atom(string_literal.value.as_str())),
        )
    }

    pub fn trans_template_literal(&self, template_literal: &TemplateLiteral) -> OxcExpression {
        self.builder.expression_template_literal(
            SPAN,
            self.builder.vec_from_iter(template_literal.quasis.iter().enumerate().map(
                |(idx, quasi)| {
                    self.builder.template_element(
                        SPAN,
                        idx == template_literal.quasis.len(),
                        TemplateElementValue {
                            raw: self.builder.atom(quasi.as_str()),
                            cooked: Some(self.builder.atom(quasi.as_str())),
                        },
                    )
                },
            )),
            self.builder.vec_from_iter(
                template_literal.expressions.iter().map(|expr| self.trans_expression(expr)),
            ),
        )
    }

    pub fn trans_array_expression(&self, array_expression: &ArrayExpression) -> OxcExpression {
        self.builder.expression_array(
            SPAN,
            self.builder.vec_from_iter(array_expression.elements.iter().map(|expr| match &expr {
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
            self.builder.vec_from_iter(object_expression.properties.iter().map(
                |expr| match expr {
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
                },
            )),
            None,
        )
    }

    pub fn trans_tuple_expression(&self, tuple_expression: &TupleExpression) -> OxcExpression {
        eprintln!("Warning: tuple proposal is still in stage 2. This may not work as expected.");
        self.builder.expression_new(
            SPAN,
            self.builder.expression_identifier_reference(SPAN, "Tuple"),
            self.builder.vec1(oxc::ast::ast::Argument::ArrayExpression(self.builder.alloc(
                self.builder.array_expression(
                    SPAN,
                    self.builder.vec_from_iter(tuple_expression.elements.iter().map(|expr| {
                        match &expr {
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
                        }
                    })),
                    None,
                ),
            ))),
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
                sequence_expression.expressions.iter().map(|expr| self.trans_expression(expr)),
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
        self.builder.expression_identifier_reference(SPAN, self.builder.atom(&identifier.name))
    }

    pub fn trans_await_expression(&self, await_expression: &AwaitExpression) -> OxcExpression {
        self.builder.expression_await(SPAN, self.trans_expression(&await_expression.argument))
    }

    pub fn trans_yield_expression(&self, yield_expression: &YieldExpression) -> OxcExpression {
        self.builder.expression_yield(
            SPAN,
            yield_expression.delegate,
            yield_expression.argument.as_ref().map(|expr| self.trans_expression(expr)),
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
                    call_expression.arguments.iter().map(|expr| self.trans_argument(expr)),
                ),
                NONE,
            )
        } else {
            self.builder.expression_call(
                SPAN,
                self.trans_expression(&call_expression.callee),
                NONE,
                self.builder.vec_from_iter(
                    call_expression.arguments.iter().map(|expr| self.trans_argument(expr)),
                ),
                call_expression.optional,
            )
        }
    }

    pub fn trans_argument(&self, argument: &Expression) -> oxc::ast::ast::Argument {
        match argument {
            Expression::SpreadElement(spread) => {
                self.builder.argument_spread_element(SPAN, self.trans_expression(&spread.argument))
            }
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

    pub fn trans_member_expression(
        &self,
        member_expression: &MemberExpression,
    ) -> OxcMemberExpression {
        if member_expression.computed {
            self.builder.member_expression_computed(
                SPAN,
                self.trans_expression(&member_expression.object),
                self.trans_expression(&member_expression.property),
                member_expression.optional,
            )
        } else if member_expression.private {
            self.builder.member_expression_private_field_expression(
                SPAN,
                self.trans_expression(&member_expression.object),
                match &member_expression.property {
                    Expression::Identifier(idt) => {
                        self.builder.private_identifier(SPAN, idt.name.as_str())
                    }
                    _ => unreachable!(),
                },
                member_expression.optional,
            )
        } else {
            self.builder.member_expression_static(
                SPAN,
                self.trans_expression(&member_expression.object),
                match &member_expression.property {
                    Expression::Identifier(idt) => {
                        self.builder.identifier_name(SPAN, idt.name.as_str())
                    }
                    _ => unreachable!(),
                },
                member_expression.optional,
            )
        }
    }

    pub fn trans_update_expression(&self, update_expression: &UpdateExpression) -> OxcExpression {
        self.builder.expression_update(
            SPAN,
            if update_expression.increment {
                UpdateOperator::Increment
            } else {
                UpdateOperator::Decrement
            },
            update_expression.prefix,
            self.trans_assignment_target_into_simple(&update_expression.target),
        )
    }

    pub fn trans_assignment_target(
        &self,
        assignment_target: &AssignmentTarget,
    ) -> OxcAssignmentTarget {
        match assignment_target {
            AssignmentTarget::AssignmentTargetIdentifier(idt) => OxcAssignmentTarget::from(
                self.builder.simple_assignment_target_identifier_reference(
                    SPAN,
                    self.builder.atom(&idt.name),
                ),
            ),
            AssignmentTarget::MemberExpression(member_expr) => {
                OxcAssignmentTarget::from(self.trans_member_expression(member_expr))
            }
            AssignmentTarget::ArrayAssignmentTarget(array_assignment_target) => OxcAssignmentTarget::from(self
                                                                                                              .builder
                                                                                                              .assignment_target_pattern_array_assignment_target(
                                                                                                                  SPAN,
                                                                                                                  self.builder.vec_from_iter(
                                                                                                                      array_assignment_target
                                                                                                                          .elements
                                                                                                                          .iter()
                                                                                                                          .map(|element| element
                                                                                                                              .as_ref()
                                                                                                                              .map(|ele| self.trans_assignment_target_into_maybe_default(ele))
                                                                                                                          )
                                                                                                                  ),
                                                                                                                  array_assignment_target.rest.as_ref().map(|rest| self.trans_assignment_target_rest(rest)),
                                                                                                                  None,
                                                                                                              ),
            ),
            AssignmentTarget::ObjectAssignmentTarget(object_assignment_target) =>
                OxcAssignmentTarget::from(
                    self.builder.assignment_target_pattern_object_assignment_target(
                        SPAN,
                        self.builder.vec_from_iter(
                            object_assignment_target
                                .properties
                                .iter()
                                .map(|property| match property {
                                    AssignmentTargetProperty::AssignmentTargetPropertyProperty(property) => {
                                        self.builder.assignment_target_property_assignment_target_property_property(
                                            SPAN,
                                            match &property.name {
                                                Expression::Identifier(idt) if idt.private =>
                                                    self.builder.property_key_private_identifier(SPAN, self.builder.atom(&idt.name)),
                                                Expression::Identifier(idt) =>
                                                    self.builder.property_key_identifier_name(SPAN, self.builder.atom(&idt.name)),
                                                _ => PropertyKey::from(self.trans_expression(&property.name)),
                                            },
                                            self.trans_assignment_target_into_maybe_default(&property.binding),
                                        )
                                    }
                                    AssignmentTargetProperty::AssignmentTargetPropertyIdentifier(idt) => {
                                        self
                                            .builder
                                            .assignment_target_property_assignment_target_property_identifier(
                                                SPAN,
                                                self.builder.identifier_reference(
                                                    SPAN,
                                                    self.builder.atom(idt.binding.name.as_str()),
                                                ),
                                                idt.init.as_ref().map(|init| self.trans_expression(init)),
                                            )
                                    }
                                })
                        ),
                        object_assignment_target
                            .rest
                            .as_ref()
                            .map(|rest| self.trans_assignment_target_rest(rest)),
                    )
                ),
            _ => unimplemented!()
        }
    }

    pub fn trans_assignment_target_into_maybe_default(
        &self,
        assignment_target: &AssignmentTarget,
    ) -> oxc::ast::ast::AssignmentTargetMaybeDefault {
        match assignment_target {
            AssignmentTarget::AssignmentTargetWithDefault(atwd) => {
                oxc::ast::ast::AssignmentTargetMaybeDefault::AssignmentTargetWithDefault(
                    self.builder.alloc(self.trans_assignment_target_with_default(atwd)),
                )
            }
            _ => oxc::ast::ast::AssignmentTargetMaybeDefault::from(
                self.trans_assignment_target(&assignment_target),
            ),
        }
    }

    pub fn trans_assignment_target_rest(
        &self,
        assignment_target_rest: &AssignmentTargetRest,
    ) -> oxc::ast::ast::AssignmentTargetRest {
        self.builder.assignment_target_rest(
            SPAN,
            self.trans_assignment_target(&assignment_target_rest.target),
        )
    }

    pub fn trans_assignment_target_with_default(
        &self,
        assignment_target_with_default: &AssignmentTargetWithDefault,
    ) -> oxc::ast::ast::AssignmentTargetWithDefault {
        self.builder.assignment_target_with_default(
            SPAN,
            self.trans_assignment_target(&assignment_target_with_default.binding),
            self.trans_expression(&assignment_target_with_default.init),
        )
    }

    pub fn trans_assignment_target_into_simple(
        &self,
        assignment_target: &AssignmentTarget,
    ) -> SimpleAssignmentTarget {
        match assignment_target {
            AssignmentTarget::AssignmentTargetIdentifier(idt) => self
                .builder
                .simple_assignment_target_identifier_reference(SPAN, self.builder.atom(&idt.name)),
            AssignmentTarget::MemberExpression(member_expr) => {
                SimpleAssignmentTarget::from(self.trans_member_expression(member_expr))
            }
            _ => unreachable!(),
        }
    }

    pub fn trans_assignment_expression(
        &self,
        assignment_expression: &AssignmentExpression,
    ) -> OxcExpression {
        self.builder.expression_assignment(
            SPAN,
            AssignmentOperator::Assign,
            self.trans_assignment_target(&assignment_expression.target),
            self.trans_expression(&assignment_expression.value),
        )
    }

    pub fn trans_variable_declaration(
        &self,
        variable_declaration: &VariableDeclaration,
    ) -> oxc::ast::ast::VariableDeclaration {
        self.builder.variable_declaration(
            SPAN,
            variable_declaration.kind.into(),
            self.builder.vec_from_iter(
                variable_declaration
                    .declarations
                    .iter()
                    .map(|decl| self.trans_variable_declarator(decl, variable_declaration.kind)),
            ),
            false,
        )
    }

    pub fn trans_variable_declarator(
        &self,
        variable_declarator: &VariableDeclarator,
        kind: VariableDeclarationKind,
    ) -> oxc::ast::ast::VariableDeclarator {
        self.builder.variable_declarator(
            SPAN,
            kind.into(),
            self.trans_binding_pattern(&variable_declarator.id),
            variable_declarator.init.as_ref().map(|init| self.trans_expression(init)),
            false,
        )
    }

    pub fn trans_binding_pattern(
        &self,
        binding_pattern: &BindingPattern,
    ) -> oxc::ast::ast::BindingPattern {
        self.builder.binding_pattern(
            self.trans_binding_pattern_kind(&binding_pattern.kind),
            NONE,
            false,
        )
    }

    pub fn trans_binding_pattern_kind(
        &self,
        binding_pattern: &BindingPatternKind,
    ) -> oxc::ast::ast::BindingPatternKind {
        match binding_pattern {
            BindingPatternKind::BindingIdentifier(idt) => self
                .builder
                .binding_pattern_kind_binding_identifier(SPAN, self.builder.atom(&idt.name)),
            BindingPatternKind::AssignmentPattern(assignment) => {
                self.builder.binding_pattern_kind_assignment_pattern(
                    SPAN,
                    self.trans_binding_pattern(&assignment.left),
                    self.trans_expression(&assignment.right),
                )
            }
            _ => unimplemented!(),
        }
    }

    pub fn trans_expression_statement(
        &self,
        expression_statement: &ExpressionStatement,
    ) -> OxcStatement {
        self.builder
            .statement_expression(SPAN, self.trans_expression(&expression_statement.expression))
    }

    pub fn trans_for_statement_left(&self, left: &Expression) -> ForStatementLeft {
        match left {
            Expression::VariableDeclaration(variable_decl) => {
                ForStatementLeft::VariableDeclaration(
                    self.builder.alloc(self.trans_variable_declaration(variable_decl)),
                )
            }
            Expression::AssignmentTarget(assignment_target) => {
                ForStatementLeft::from(self.trans_assignment_target(assignment_target))
            }
            _ => unreachable!(),
        }
    }

    pub fn trans_for_statement(&self, for_statement: &ForStatement) -> OxcStatement {
        if for_statement.iteration {
            self.builder.statement_for_of(
                SPAN,
                for_statement.r#await,
                self.trans_for_statement_left(&for_statement.left),
                self.trans_expression(&for_statement.right),
                self.trans_statement(&for_statement.body),
            )
        } else {
            self.builder.statement_for_in(
                SPAN,
                self.trans_for_statement_left(&for_statement.left),
                self.trans_expression(&for_statement.right),
                self.trans_statement(&for_statement.body),
            )
        }
    }

    pub fn trans_break_statement(&self) -> OxcStatement {
        self.builder.statement_break(SPAN, None)
    }

    pub fn trans_continue_statement(&self) -> OxcStatement {
        self.builder.statement_continue(SPAN, None)
    }

    pub fn trans_debugger_statement(&self) -> OxcStatement {
        self.builder.statement_debugger(SPAN)
    }

    pub fn trans_empty_statement(&self) -> OxcStatement {
        self.builder.statement_empty(SPAN)
    }

    pub fn trans_return_statement(&self, return_statement: &ReturnStatement) -> OxcStatement {
        self.builder.statement_return(
            SPAN,
            return_statement.argument.as_ref().map(|expr| self.trans_expression(expr)),
        )
    }

    pub fn trans_if_statement(&self, if_statement: &IfStatement) -> OxcStatement {
        self.builder.statement_if(
            SPAN,
            self.trans_expression(&if_statement.test),
            self.trans_statement(&if_statement.consequent),
            if_statement.alternate.as_ref().map(|alt| self.trans_statement(alt)),
        )
    }

    pub fn trans_while_statement(&self, while_statement: &WhileStatement) -> OxcStatement {
        self.builder.statement_while(
            SPAN,
            self.trans_expression(&while_statement.test),
            self.trans_statement(&while_statement.body),
        )
    }

    pub fn trans_function_declaration(
        &self,
        function_declaration: &Function,
    ) -> oxc::ast::ast::Function {
        self.builder.function(
            SPAN,
            function_declaration.r#type.into(),
            function_declaration
                .id
                .as_ref()
                .map(|id| self.builder.binding_identifier(SPAN, id.name.as_str())),
            function_declaration.generator,
            function_declaration.r#async,
            false,
            NONE,
            NONE,
            self.trans_formal_parameters(&function_declaration.params),
            NONE,
            function_declaration.body.as_ref().map(|body| self.trans_function_body(body)),
        )
    }

    pub fn trans_decorator(&self, decorator: &Decorator) -> oxc::ast::ast::Decorator {
        self.builder.decorator(SPAN, self.trans_expression(&decorator.expression))
    }

    pub fn trans_formal_parameter(
        &self,
        formal_parameter: &FormalParameter,
    ) -> oxc::ast::ast::FormalParameter {
        self.builder.formal_parameter(
            SPAN,
            self.builder.vec_from_iter(
                formal_parameter.decorators.iter().map(|decorator| self.trans_decorator(decorator)),
            ),
            self.trans_binding_pattern(&formal_parameter.id),
            None,
            false,
            false,
        )
    }

    pub fn trans_formal_parameters(
        &self,
        formal_parameters: &FormalParameters,
    ) -> oxc::ast::ast::FormalParameters {
        self.builder.formal_parameters(
            SPAN,
            formal_parameters.kind.into(),
            self.builder.vec_from_iter(
                formal_parameters.items.iter().map(|param| self.trans_formal_parameter(param)),
            ),
            formal_parameters.rest.as_ref().map(|rest| self.trans_binding_rest_element(rest)),
        )
    }

    pub fn trans_binding_rest_element(
        &self,
        binding_rest_element: &BindingRestElement,
    ) -> oxc::ast::ast::BindingRestElement {
        self.builder
            .binding_rest_element(SPAN, self.trans_binding_pattern(&binding_rest_element.argument))
    }

    pub fn trans_function_body(&self, body: &FunctionBody) -> oxc::ast::ast::FunctionBody {
        self.builder.function_body(
            SPAN,
            self.builder.vec_from_iter(
                body.directives.iter().map(|directive| self.trans_directive(directive)),
            ),
            self.builder.vec_from_iter(
                body.statements.iter().map(|statement| self.trans_statement(statement)),
            ),
        )
    }

    pub fn trans_directive(&self, directive: &Directive) -> oxc::ast::ast::Directive {
        self.builder.directive(
            SPAN,
            self.trans_string_literal(&directive.expression),
            self.builder.atom(directive.directive.as_str()),
        )
    }
}
