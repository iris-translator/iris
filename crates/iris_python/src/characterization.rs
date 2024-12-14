use iris_low_level_ir::shared::*;
use ruff::ast::ModModule;
use ruff::ast::name::Name;
use ruff::ast::*;
use ruff::span::{TextRange, TextSize};
use std::str::FromStr;

pub struct PythonCharacterization {
    pub ast: Option<ModModule>,
}

impl PythonCharacterization {
    pub fn new() -> Self {
        Self { ast: None }
    }

    pub fn build(&mut self, program: Program) {
        self.ast = Some(ModModule {
            range: program.span.clone().into(),
            body: program
                .body
                .iter()
                .map(|x| self.trans_statement(x))
                .flatten()
                .collect(),
        })
    }

    pub fn trans_statement(&mut self, statement: &Statement) -> Vec<Stmt> {
        match statement {
            Statement::BlockStatement(block_statement) => {
                self.trans_block_statement(block_statement)
            }
            Statement::ExpressionStatement(expression_statement) => {
                vec![self.trans_expression_statement(expression_statement)]
            }
            Statement::ForStatement(for_statement) => {
                vec![Stmt::For(self.trans_for_statement(for_statement))]
            }
            Statement::WhileStatement(while_statement) => {
                vec![Stmt::While(self.trans_while_statement(while_statement))]
            }
            Statement::IfStatement(if_statement) => {
                vec![Stmt::If(self.trans_if_statement(if_statement))]
            }
            Statement::BreakStatement(break_statement) => {
                vec![Stmt::Break(self.trans_break_statement(break_statement))]
            }
            Statement::ContinueStatement(continue_statement) => {
                vec![Stmt::Continue(
                    self.trans_continue_statement(continue_statement),
                )]
            }
            Statement::EmptyStatement(empty_statement) => {
                vec![Stmt::Pass(self.trans_empty_statement(empty_statement))]
            }
            Statement::ReturnStatement(return_statement) => {
                vec![Stmt::Return(self.trans_return_statement(return_statement))]
            }
            Statement::TryStatement(_) => unimplemented!(),
            Statement::FunctionDeclaration(func) => {
                vec![Stmt::FunctionDef(self.trans_function_declaration(func))]
            }
            Statement::VariableDeclaration(variable_declaration) => {
                self.trans_variable_declaration(variable_declaration)
            }
            _ => unimplemented!("Unsupported statement: {:?}", statement),
        }
    }

    pub fn trans_block_statement(&mut self, block_statement: &BlockStatement) -> Vec<Stmt> {
        block_statement
            .statements
            .iter()
            .map(|stmt| self.trans_statement(stmt))
            .flatten()
            .collect()
    }

    pub fn trans_expression(&mut self, expression: &Expression) -> Expr {
        match expression {
            Expression::BooleanLiteral(boolean_literal) => {
                Expr::BooleanLiteral(self.trans_boolean_literal(boolean_literal))
            }
            Expression::NullLiteral(null_literal) => {
                Expr::NoneLiteral(self.trans_null_literal(null_literal))
            }
            Expression::NumericLiteral(numeric_literal) => {
                Expr::NumberLiteral(self.trans_numeric_literal(numeric_literal))
            }
            Expression::StringLiteral(string_literal) => {
                Expr::StringLiteral(self.trans_string_literal(string_literal))
            }
            Expression::TemplateLiteral(template_literal) => {
                Expr::FString(self.trans_template_literal(template_literal))
            }
            Expression::ArrayExpression(array_expression) => {
                Expr::List(self.trans_array_expression(array_expression))
            }
            Expression::ObjectExpression(object_expression) => {
                Expr::Dict(self.trans_object_expression(object_expression))
            }
            Expression::SetExpression(set_expression) => {
                Expr::Set(self.trans_set_expression(set_expression))
            }
            Expression::TupleExpression(tuple_expression) => {
                Expr::Tuple(self.trans_tuple_expression(tuple_expression))
            }
            Expression::SequenceExpression(sequence_expression) => {
                self.trans_sequence_expression(sequence_expression)
            }
            Expression::ParenthesizedExpression(parenthesized_expression) => {
                self.trans_expression(&parenthesized_expression.expression)
            }
            Expression::Identifier(identifier) => Expr::Name(self.trans_identifier(identifier)),
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
            Expression::LambdaExpression(lambda) => Expr::Lambda(self.trans_lambda(lambda)),
            Expression::ConditionalExpression(conditional_expression) => {
                Expr::If(self.trans_conditional_expression(conditional_expression))
            }
            Expression::CallExpression(call_expression) => {
                Expr::Call(self.trans_call_expression(call_expression))
            }
            Expression::MemberExpression(member_expression) => {
                self.trans_member_expression(member_expression)
            }
            Expression::BinaryExpression(binary_expression) => {
                self.trans_binary_operation(binary_expression)
            }
            Expression::AssignmentTarget(assignment_target) => {
                self.trans_expression(&assignment_target.clone().into_expression())
            }
            Expression::AssignmentExpression(_) | Expression::UpdateExpression(_) => {
                unreachable!(
                    "Assignment and update expressions should be handled in the statement. That's because in Python, they are statements, not expressions."
                )
            }
            _ => unimplemented!("Unsupported expression: {:?}", expression),
        }
    }

    pub fn trans_boolean_literal(
        &mut self,
        boolean_literal: &BooleanLiteral,
    ) -> ExprBooleanLiteral {
        ExprBooleanLiteral {
            range: boolean_literal.span.clone().into(),
            value: boolean_literal.value,
        }
    }

    pub fn trans_null_literal(&mut self, null_literal: &NullLiteral) -> ExprNoneLiteral {
        ExprNoneLiteral {
            range: null_literal.span.clone().into(),
        }
    }

    pub fn trans_numeric_literal(&mut self, numeric_literal: &NumericLiteral) -> ExprNumberLiteral {
        ExprNumberLiteral {
            range: numeric_literal.span.clone().into(),
            value: match numeric_literal.base {
                NumberBase::Float => Number::Float(numeric_literal.value),
                NumberBase::BigInt => Number::Int(
                    Int::from_str_radix(
                        numeric_literal.raw.as_str().trim_end_matches("n"),
                        numeric_literal.base.radix(),
                        numeric_literal.raw.as_str().trim_end_matches("n"),
                    )
                    .unwrap(),
                ),
                _ => Number::Int(
                    Int::from_str_radix(
                        numeric_literal.raw.as_str(),
                        numeric_literal.base.radix(),
                        numeric_literal.raw.as_str(),
                    )
                    .unwrap(),
                ),
            },
        }
    }

    pub fn trans_string_literal(
        &mut self,
        string_literal: &iris_low_level_ir::shared::StringLiteral,
    ) -> ExprStringLiteral {
        ExprStringLiteral {
            range: string_literal.span.clone().into(),
            value: StringLiteralValue::single(ruff::ast::StringLiteral {
                range: string_literal.span.clone().into(),
                value: string_literal.value.clone().into_boxed_str(),
                flags: string_literal.prefix.into(),
            }),
        }
    }

    pub fn string_into_string_literal_value(string: String) -> StringLiteralValue {
        StringLiteralValue::single(ruff::ast::StringLiteral {
            range: TextRange::empty(TextSize::new(string.len() as u32)),
            value: string.into_boxed_str(),
            flags: StringLiteralPrefix::Empty.into(),
        })
    }

    pub fn trans_template_literal(&mut self, template_literal: &TemplateLiteral) -> ExprFString {
        let mut elements =
            Vec::with_capacity(template_literal.expressions.len() + template_literal.quasis.len());
        for (quasi, expr) in template_literal
            .quasis
            .iter()
            .zip(template_literal.expressions.iter())
        {
            elements.push(FStringElement::Literal(FStringLiteralElement {
                range: template_literal.span.clone().into(),
                value: quasi.clone().into_boxed_str(),
            }));
            elements.push(FStringElement::Expression(FStringExpressionElement {
                range: template_literal.span.clone().into(),
                expression: Box::new(self.trans_expression(expr)),
                debug_text: None,
                conversion: ConversionFlag::None,
                format_spec: None,
            }));
        }
        ExprFString {
            range: template_literal.span.clone().into(),
            value: FStringValue::single(FString {
                range: template_literal.span.clone().into(),
                elements: elements.into(),
                flags: template_literal.prefix.into(),
            }),
        }
    }

    pub fn trans_logical_expression(&mut self, logical_expression: &LogicalExpression) -> Expr {
        Expr::BoolOp(ExprBoolOp {
            range: logical_expression.span.clone().into(),
            op: match logical_expression.operator {
                LogicalOperator::And => BoolOp::And,
                LogicalOperator::Or => BoolOp::Or,
            },
            values: vec![
                self.trans_expression(&logical_expression.left),
                self.trans_expression(&logical_expression.right),
            ],
        })
    }

    pub fn trans_binary_operation(&mut self, binary_expression: &BinaryExpression) -> Expr {
        if binary_expression.operator.is_arithmetic() || binary_expression.operator.is_bitwise() {
            Expr::BinOp(ExprBinOp {
                range: binary_expression.span.clone().into(),
                left: Box::new(self.trans_expression(&binary_expression.left)),
                op: binary_expression.operator.into(),
                right: Box::new(self.trans_expression(&binary_expression.right)),
            })
        } else if binary_expression.operator.is_comparison() {
            Expr::Compare(ExprCompare {
                range: binary_expression.span.clone().into(),
                left: Box::new(self.trans_expression(&binary_expression.left)),
                ops: vec![binary_expression.operator.into()].into_boxed_slice(),
                comparators: vec![self.trans_expression(&binary_expression.right)]
                    .into_boxed_slice(),
            })
        } else if matches!(binary_expression.operator, BinaryOperator::Instanceof) {
            Expr::Call(ExprCall {
                range: binary_expression.span.clone().into(),
                func: Box::new(Expr::Name(ExprName {
                    // `"isinstance".len()` is 10
                    range: TextRange::empty(TextSize::new(10)),
                    id: "isinstance".into(),
                    ctx: ExprContext::Load,
                })),
                arguments: Arguments {
                    range: binary_expression.span.clone().into(),
                    args: vec![
                        self.trans_expression(&binary_expression.left),
                        self.trans_expression(&binary_expression.right),
                    ]
                    .into_boxed_slice(),
                    keywords: vec![].into_boxed_slice(),
                },
            })
        } else {
            unimplemented!(
                "Unsupported binary operator: {:?}",
                binary_expression.operator
            )
        }
    }

    pub fn trans_unary_operation(&mut self, unary_expression: &UnaryExpression) -> Expr {
        match unary_expression.operator {
            UnaryOperator::Delete => {
                todo!("Need to hoist the expression to a statement")
            }
            UnaryOperator::Void => {
                // Transform it into `None`, with executed expression.
                // For instance, `void 0` should be transformed into `(0, None)[-1]`.
                // This is tended to avoid removing side effects, e.g. `void console.log("Hello, world!")` should still print the message.
                // Thus, the expected result should be `(console.log("Hello, world!"), None)[-1]`, though it should be handled into `print` function.
                self.trans_sequence_expression(&SequenceExpression {
                    span: unary_expression.span.clone(),
                    expressions: vec![unary_expression.argument.clone(), Expression::NullLiteral(NullLiteral {
                        span: unary_expression.span.clone(),
                    })],
                })
            }
            UnaryOperator::Typeof => {
                eprintln!(
                    "Warning: `typeof` has similar approach in Python, which is `type` function. However, it is a function that returns the type of the object, instead of string. IRIS will firstly transform it into `type` function."
                );
                eprintln!(
                    "For further functions, it should be configured in the configuration file."
                );
                Expr::Call(ExprCall {
                    range: unary_expression.span.clone().into(),
                    func: Box::new(Expr::Name(ExprName {
                        range: TextRange::empty(TextSize::new(4)),
                        id: "type".into(),
                        ctx: ExprContext::Load,
                    })),
                    arguments: Arguments {
                        range: unary_expression.span.clone().into(),
                        args: vec![self.trans_expression(&unary_expression.argument)]
                            .into_boxed_slice(),
                        keywords: vec![].into_boxed_slice(),
                    },
                })
            }
            _ => Expr::UnaryOp(ExprUnaryOp {
                range: unary_expression.span.clone().into(),
                op: unary_expression.operator.into(),
                operand: Box::new(self.trans_expression(&unary_expression.argument)),
            }),
        }
    }

    pub fn trans_lambda(&mut self, lambda_expression: &LambdaExpression) -> ExprLambda {
        if lambda_expression.expression {
            ExprLambda {
                range: lambda_expression.span.clone().into(),
                parameters: Some(Box::new(self.trans_formal_parameters(&lambda_expression.params))),
                body: Box::new(
                    match lambda_expression
                        .body
                        .statements
                        .get(0)
                        .expect("Expression lambda must have a body")
                    {
                        Statement::ExpressionStatement(expression) => {
                            self.trans_expression(&expression.expression)
                        }
                        _ => panic!("Expression lambda must have a body"),
                    },
                ),
            }
        } else {
            unimplemented!("It should be hoisted as a function, which is not supported yet")
        }
    }

    pub fn trans_conditional_expression(
        &mut self,
        conditional_expression: &ConditionalExpression,
    ) -> ExprIf {
        ExprIf {
            range: conditional_expression.span.clone().into(),
            test: Box::new(self.trans_expression(&conditional_expression.test)),
            body: Box::new(self.trans_expression(&conditional_expression.consequent)),
            orelse: Box::new(self.trans_expression(&conditional_expression.alternate)),
        }
    }

    pub fn trans_object_expression(&mut self, object_expression: &ObjectExpression) -> ExprDict {
        ExprDict {
            range: object_expression.span.clone().into(),
            items: object_expression
                .properties
                .iter()
                .map(|prop| self.trans_object_property(prop))
                .collect(),
        }
    }

    pub fn trans_object_property(&mut self, object_property: &Expression) -> DictItem {
        match object_property {
            Expression::ObjectProperty(prop) => DictItem {
                key: Some(self.trans_expression(&prop.key)),
                value: self.trans_expression(&prop.value),
            },
            Expression::SpreadElement(_) => {
                unreachable!("SpreadElement should be transformed from oxc.")
            }
            _ => unreachable!("Invalid property type in object expression."),
        }
    }

    pub fn trans_yield_expression(&mut self, yield_expression: &YieldExpression) -> Expr {
        if yield_expression.delegate {
            Expr::YieldFrom(ExprYieldFrom {
                range: yield_expression.span.clone().into(),
                value: Box::new(
                    self.trans_expression(
                        &yield_expression
                            .argument
                            .clone()
                            .expect("Yield from must have an argument"),
                    ),
                ),
            })
        } else {
            Expr::Yield(ExprYield {
                range: yield_expression.span.clone().into(),
                value: yield_expression
                    .clone()
                    .argument
                    .map(|arg| Box::new(self.trans_expression(&arg))),
            })
        }
    }

    pub fn trans_await_expression(&mut self, await_expression: &AwaitExpression) -> Expr {
        Expr::Await(ExprAwait {
            range: await_expression.span.clone().into(),
            value: Box::new(self.trans_expression(&await_expression.argument)),
        })
    }

    pub fn trans_identifier(
        &mut self,
        identifier: &iris_low_level_ir::shared::Identifier,
    ) -> ExprName {
        ExprName {
            range: identifier.span.clone().into(),
            id: Name::from(identifier.name.clone()),
            ctx: ExprContext::Load,
        }
    }

    pub fn trans_identifier_to_identifier(
        identifier: &iris_low_level_ir::shared::Identifier,
    ) -> ruff::ast::Identifier {
        ruff::ast::Identifier {
            id: Name::from(identifier.name.clone()),
            range: identifier.span.clone().into(),
        }
    }

    pub fn trans_call_expression(&mut self, call_expression: &CallExpression) -> ExprCall {
        ExprCall {
            range: call_expression.span.clone().into(),
            func: Box::new(self.trans_expression(&call_expression.callee)),
            arguments: Arguments {
                range: call_expression.span.clone().into(),
                args: call_expression
                    .arguments
                    .iter()
                    .map(|x| self.trans_expression(x))
                    .collect::<Vec<Expr>>()
                    .into_boxed_slice(),
                keywords: vec![].into_boxed_slice(),
            },
        }
    }

    pub fn trans_member_expression(&mut self, member_expression: &MemberExpression) -> Expr {
        if member_expression.computed {
            Expr::Subscript(ExprSubscript {
                range: member_expression.span.clone().into(),
                value: Box::new(self.trans_expression(&member_expression.object)),
                slice: Box::new(self.trans_expression(&member_expression.property)),
                ctx: ExprContext::Load,
            })
        } else {
            Expr::Attribute(ExprAttribute {
                range: member_expression.span.clone().into(),
                value: Box::new(self.trans_expression(&member_expression.object)),
                attr: match &member_expression.property {
                    Expression::Identifier(identifier) => ruff::ast::Identifier {
                        id: Name::from(identifier.name.clone()),
                        range: TextRange::empty(TextSize::new(identifier.name.len() as u32)),
                    },
                    _ => unreachable!("Invalid property type in member expression."),
                },
                ctx: ExprContext::Load,
            })
        }
    }

    pub fn trans_array_expression(&mut self, array_expression: &ArrayExpression) -> ExprList {
        ExprList {
            range: array_expression.span.clone().into(),
            elts: array_expression
                .elements
                .iter()
                .map(|x| self.trans_expression(x))
                .collect::<Vec<_>>(),
            ctx: ExprContext::Load,
        }
    }

    pub fn trans_sequence_expression(&mut self, sequence_expression: &SequenceExpression) -> Expr {
        Expr::Subscript(ExprSubscript {
            range: sequence_expression.span.clone().into(),
            value: Box::new(Expr::Tuple(ExprTuple {
                range: sequence_expression.span.clone().into(),
                elts: sequence_expression.expressions.iter().map(|x| self.trans_expression(x)).collect(),
                ctx: ExprContext::Load,
                parenthesized: true,
            })),
            slice: Box::from(Expr::UnaryOp(ExprUnaryOp {
                range: sequence_expression.span.clone().into(),
                op: UnaryOp::USub,
                operand: Box::new(Expr::NumberLiteral(ExprNumberLiteral {
                    range: sequence_expression.span.clone().into(),
                    value: Number::Int(Int::from(1u8)),
                })),
            })),
            ctx: ExprContext::Load,
        })
    }

    pub fn trans_set_expression(&mut self, set_expression: &SetExpression) -> ExprSet {
        ExprSet {
            range: set_expression.span.clone().into(),
            elts: set_expression
                .elements
                .iter()
                .map(|x| self.trans_expression(x))
                .collect::<Vec<_>>(),
        }
    }

    pub fn trans_tuple_expression(&mut self, tuple_expression: &TupleExpression) -> ExprTuple {
        ExprTuple {
            range: tuple_expression.span.clone().into(),
            elts: tuple_expression
                .elements
                .iter()
                .map(|x| self.trans_expression(x))
                .collect(),
            parenthesized: true,
            ctx: ExprContext::Load,
        }
    }

    pub fn trans_expression_statement(
        &mut self,
        expression_statement: &ExpressionStatement,
    ) -> Stmt {
        match &expression_statement.expression {
            Expression::AssignmentExpression(assignment_expression) => {
                Stmt::Assign(self.trans_assignment_expression(assignment_expression))
            }
            Expression::UnaryExpression(unary)
                if matches!(unary.operator, UnaryOperator::Delete) =>
            {
                println!("Warning: `delete` operator is not supported in Python. It will be transformed into a pass statement.");
                Stmt::Delete(StmtDelete {
                    range: expression_statement.span.clone().into(),
                    targets: vec![self.trans_expression(&unary.argument)],
                })
            }
            _ => Stmt::Expr(StmtExpr {
                range: expression_statement.span.clone().into(),
                value: Box::new(self.trans_expression(&expression_statement.expression)),
            }),
        }
    }

    pub fn trans_assignment_expression(
        &mut self,
        assignment_expression: &AssignmentExpression,
    ) -> StmtAssign {
        StmtAssign {
            range: assignment_expression.span.clone().into(),
            targets: vec![self.trans_assignment_target(&assignment_expression.target)],
            value: Box::from(self.trans_expression(&assignment_expression.value)),
        }
    }

    pub fn trans_assignment_target(&mut self, assignment_target: &AssignmentTarget) -> Expr {
        match assignment_target {
            AssignmentTarget::AssignmentTargetIdentifier(identifier) => {
                Expr::Name(self.trans_identifier(identifier))
            }
            AssignmentTarget::MemberExpression(member_expression) => {
                self.trans_member_expression(member_expression)
            }
            _ => unimplemented!(),
        }
    }

    pub fn trans_for_statement(&mut self, for_statement: &ForStatement) -> StmtFor {
        StmtFor {
            is_async: for_statement.r#await,
            range: for_statement.span.clone().into(),
            target: Box::new(self.trans_expression(&for_statement.left)),
            iter: Box::new(self.trans_expression(&for_statement.right)),
            body: self.trans_statement(&for_statement.body),
            orelse: vec![],
        }
    }

    pub fn trans_while_statement(&mut self, while_statement: &WhileStatement) -> StmtWhile {
        StmtWhile {
            range: while_statement.span.clone().into(),
            test: Box::new(self.trans_expression(&while_statement.test)),
            body: self.trans_statement(&while_statement.body),
            orelse: vec![],
        }
    }

    pub fn trans_break_statement(&mut self, break_statement: &BreakStatement) -> StmtBreak {
        StmtBreak {
            range: break_statement.span.clone().into(),
        }
    }

    pub fn trans_continue_statement(
        &mut self,
        continue_statement: &ContinueStatement,
    ) -> StmtContinue {
        StmtContinue {
            range: continue_statement.span.clone().into(),
        }
    }

    pub fn trans_empty_statement(&mut self, empty_statement: &EmptyStatement) -> StmtPass {
        StmtPass {
            range: empty_statement.span.clone().into(),
        }
    }

    pub fn trans_if_statement(&mut self, if_statement: &IfStatement) -> StmtIf {
        StmtIf {
            range: if_statement.span.clone().into(),
            test: Box::new(self.trans_expression(&if_statement.test)),
            body: self.trans_statement(&if_statement.consequent),
            // TODO: de-nest the elif clauses
            elif_else_clauses: if let Some(alternate) = &if_statement.alternate {
                vec![ElifElseClause {
                    range: if_statement.span.clone().into(),
                    test: None,
                    body: self.trans_statement(alternate),
                }]
            } else {
                vec![]
            },
        }
    }

    pub fn trans_return_statement(&mut self, return_statement: &ReturnStatement) -> StmtReturn {
        StmtReturn {
            range: return_statement.span.clone().into(),
            value: return_statement
                .argument
                .as_ref()
                .map(|x| Box::new(self.trans_expression(x))),
        }
    }

    pub fn trans_function_declaration(
        &mut self,
        function_declaration: &Function,
    ) -> StmtFunctionDef {
        StmtFunctionDef {
            range: function_declaration.span.clone().into(),
            parameters: Box::new(self.trans_formal_parameters(&function_declaration.params)),
            name: Self::trans_identifier_to_identifier(
                &function_declaration
                    .id
                    .as_ref()
                    .expect("This term you must have a name"),
            ),
            is_async: function_declaration.r#async,
            type_params: None,
            decorator_list: vec![],
            returns: None,
            body: function_declaration
                .body
                .as_ref()
                .unwrap()
                .statements
                .iter()
                .map(|x| self.trans_statement(x))
                .flatten()
                .collect(),
        }
    }

    pub fn trans_binding_identifier_to_parameter(
        binding_identifier: &iris_low_level_ir::shared::Identifier,
    ) -> Parameter {
        Parameter {
            range: binding_identifier.span.clone().into(),
            name: ruff::ast::Identifier {
                id: Name::from(binding_identifier.name.clone()),
                range: TextRange::empty(TextSize::new(binding_identifier.name.len() as u32)),
            },
            annotation: None,
        }
    }

    pub fn trans_formal_parameters(&mut self, formal_parameters: &FormalParameters) -> Parameters {
        Parameters {
            range: formal_parameters.span.clone().into(),
            posonlyargs: vec![],
            args: formal_parameters
                .items
                .iter()
                .map(|param| match &param.id.kind {
                    BindingPatternKind::BindingIdentifier(idt) => ParameterWithDefault {
                        range: idt.span.clone().into(),
                        parameter: Self::trans_binding_identifier_to_parameter(idt),
                        default: None,
                    },
                    BindingPatternKind::AssignmentPattern(assignment) => ParameterWithDefault {
                        range: assignment.span.clone().into(),
                        parameter: match &assignment.left.kind {
                            BindingPatternKind::BindingIdentifier(idt) => {
                                Self::trans_binding_identifier_to_parameter(idt)
                            }
                            _ => unimplemented!(),
                        },
                        default: Some(Box::new(self.trans_expression(&assignment.right))),
                    },
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>(),
            vararg: formal_parameters.rest.as_ref().map(|rest| {
                Box::new(match &rest.argument.kind {
                    BindingPatternKind::BindingIdentifier(idt) => {
                        Self::trans_binding_identifier_to_parameter(idt)
                    }
                    _ => unimplemented!(),
                })
            }),
            kwonlyargs: vec![],
            kwarg: None,
        }
    }

    pub fn trans_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Vec<Stmt> {
        variable_declaration
            .declarations
            .iter()
            .filter_map(|decl| self.trans_variable_declarator(decl))
            .collect()
    }

    pub fn trans_variable_declarator(
        &mut self,
        variable_declarator: &VariableDeclarator,
    ) -> Option<Stmt> {
        variable_declarator.init.as_ref().map(|init| {
            Stmt::Assign(StmtAssign {
                range: variable_declarator.span.clone().into(),
                targets: vec![match &variable_declarator.id.kind {
                    BindingPatternKind::BindingIdentifier(idt) => {
                        Expr::Name(self.trans_identifier(&idt))
                    }
                    _ => unimplemented!(),
                }],
                value: Box::new(self.trans_expression(init)),
            })
        })
    }
}
