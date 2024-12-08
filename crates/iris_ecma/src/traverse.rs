use oxc::allocator::Allocator;
use crate::toolchain::traverse::{Traverse, TraverseCtx};
use crate::toolchain::index::Idx;
use iris_low_level_ir::shared::*;
use oxc::ast::{match_assignment_target, match_declaration, match_expression, match_member_expression};
use oxc::semantic::{ScopeTree, SymbolTable};
use oxc::span::GetSpan;
use oxc_traverse::traverse_mut;
use crate::toolchain::span::SPAN;

pub struct IrisTraverseToIR {
    pub ir: Option<Program>,
}

impl IrisTraverseToIR {
    pub fn new() -> Self {
        Self {
            ir: None,
        }
    }

    pub fn build<'a>(&mut self, allocator: &'a Allocator, program: &'a mut oxc::ast::ast::Program<'a>, symbols: SymbolTable, scopes: ScopeTree) {
        traverse_mut(self, allocator, program, symbols, scopes);
    }
}

impl Traverse<'_> for IrisTraverseToIR {
    fn enter_program(&mut self, node: &mut oxc::ast::ast::Program<'_>, ctx: &mut TraverseCtx<'_>) {
        self.ir = Some(self.trans_program(node, ctx));
    }
}

impl<'a> IrisTraverseToIR {
    pub fn trans_expression(&mut self, it: &oxc::ast::ast::Expression, ctx: &mut TraverseCtx<'a>) -> Expression {
        use oxc::ast::ast::Expression as OxcExpression;
        match it {
            OxcExpression::BooleanLiteral(it) => Expression::BooleanLiteral(Self::trans_boolean_literal(it)),
            OxcExpression::NullLiteral(it) => Expression::NullLiteral(Self::trans_null_literal(it)),
            OxcExpression::NumericLiteral(it) => Expression::NumericLiteral(Self::trans_numeric_literal(it)),
            OxcExpression::BigIntLiteral(it) => Expression::NumericLiteral(Self::trans_bigint_literal(it)),
            OxcExpression::RegExpLiteral(_it) => unimplemented!("RegExpLiteral"),
            OxcExpression::StringLiteral(it) => Expression::StringLiteral(Self::trans_string_literal(it)),
            OxcExpression::TemplateLiteral(it) => Expression::TemplateLiteral(Box::new(self.trans_template_literal(it, ctx))),
            OxcExpression::ArrayExpression(it) => Expression::ArrayExpression(Box::new(self.trans_array_expression(it, ctx))),
            OxcExpression::ArrowFunctionExpression(it) => Expression::LambdaExpression(Box::new(self.trans_arrow_function_expression(it, ctx))),
            OxcExpression::AssignmentExpression(it) => Expression::AssignmentExpression(Box::new(self.trans_assignment_expression(it, ctx).unwrap())),
            OxcExpression::AwaitExpression(it) => Expression::AwaitExpression(Box::new(self.trans_await_expression(it, ctx))),
            OxcExpression::BinaryExpression(it) => Expression::BinaryExpression(Box::new(self.trans_binary_expression(it, ctx))),
            OxcExpression::CallExpression(it) => Expression::CallExpression(Box::new(self.trans_call_expression(it, ctx))),
            OxcExpression::ChainExpression(it) => Expression::ChainExpression(Box::new(self.trans_chain_expression(it, ctx))),
            OxcExpression::ConditionalExpression(it) => Expression::ConditionalExpression(Box::new(self.trans_conditional_expression(it, ctx))),
            OxcExpression::FunctionExpression(it) => Expression::FunctionExpression(Box::new(self.trans_function_expression(it, ctx))),
            OxcExpression::LogicalExpression(it) => Expression::LogicalExpression(Box::new(self.trans_logical_expression(it, ctx))),
            OxcExpression::SequenceExpression(it) => Expression::SequenceExpression(Box::new(self.trans_sequence_expression(it, ctx))),
            OxcExpression::UnaryExpression(it) => Expression::UnaryExpression(Box::new(self.trans_unary_expression(it, ctx))),
            OxcExpression::UpdateExpression(it) => Expression::UpdateExpression(Box::new(self.trans_update_expression(it, ctx))),
            OxcExpression::YieldExpression(it) => Expression::YieldExpression(Box::new(self.trans_yield_expression(it, ctx))),
            OxcExpression::ObjectExpression(it) => Expression::ObjectExpression(Box::new(self.object_expression(it, ctx))),
            OxcExpression::Identifier(it) => Expression::Identifier(self.trans_identifier_reference(it, ctx)),
            it @ match_member_expression!(OxcExpression) => Expression::MemberExpression(Box::new(self.trans_member_expression(it.as_member_expression().unwrap(), ctx))),
            OxcExpression::ParenthesizedExpression(it) => Expression::ParenthesizedExpression(Box::new(self.trans_parenthesized_expression(it, ctx))),
            OxcExpression::NewExpression(it) => Expression::CallExpression(Box::new(self.trans_new_expression(it, ctx))),
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

    // pub fn trans_regexp_literal(_it: &oxc::ast::ast::RegExpLiteral) -> ! {
    //     unimplemented!()
    // }

    pub fn trans_string_literal(it: &oxc::ast::ast::StringLiteral) -> StringLiteral {
        StringLiteral {
            span: it.span.into(),
            value: it.value.to_string(),
            prefix: StringLiteralPrefix::Empty,
        }
    }

    pub fn trans_template_literal(&mut self, it: &oxc::ast::ast::TemplateLiteral, ctx: &mut TraverseCtx<'a>) -> TemplateLiteral {
        TemplateLiteral {
            span: it.span.into(),
            quasis: it.quasis.iter().map(|x| x.value.raw.to_string()).collect::<Vec<_>>(),
            expressions: it.expressions.iter().map(|x| self.trans_expression(x, ctx)).collect::<Vec<_>>(),
            prefix: FStringPrefix::Regular,
        }
    }

    pub fn trans_identifier_reference(&mut self, it: &oxc::ast::ast::IdentifierReference, ctx: &mut TraverseCtx<'a>) -> Identifier {
        let reference = ctx.symbols().get_reference(it.reference_id());
        Identifier {
            span: it.span.into(),
            name: it.name.to_string(),
            symbol_id: reference.symbol_id().map(|x| x.index()),
            private: false,
        }
    }

    pub fn trans_binding_identifier(it: &oxc::ast::ast::BindingIdentifier) -> Identifier {
        Identifier {
            span: it.span.into(),
            name: it.name.to_string(),
            symbol_id: it.symbol_id.get().map(|x| x.index()),
            private: false,
        }
    }

    pub fn trans_identifier_name(it: &oxc::ast::ast::IdentifierName) -> Identifier {
        Identifier {
            span: it.span.into(),
            name: it.name.to_string(),
            symbol_id: None,
            private: false,
        }
    }

    pub fn trans_private_identifier(it: &oxc::ast::ast::PrivateIdentifier) -> Identifier {
        Identifier {
            span: it.span.into(),
            name: it.name.to_string(),
            symbol_id: None,
            private: true,
        }
    }

    pub fn trans_array_expression(&mut self, it: &oxc::ast::ast::ArrayExpression, ctx: &mut TraverseCtx<'a>) -> ArrayExpression {
        use oxc::ast::ast::ArrayExpressionElement;
        ArrayExpression {
            span: it.span.into(),
            elements: it.elements.iter().map(|x| match x {
                ArrayExpressionElement::Elision(it) => Expression::Elision(Elision {
                    span: it.span.into(),
                }),
                ArrayExpressionElement::SpreadElement(it) => Expression::SpreadElement(Box::new(self.trans_spread_element(it, ctx))),
                expr @ match_expression!(ArrayExpressionElement) => self.trans_expression(expr.as_expression().unwrap(), ctx)
            }).collect::<Vec<_>>(),
        }
    }

    pub fn trans_arrow_function_expression(&mut self, it: &oxc::ast::ast::ArrowFunctionExpression, ctx: &mut TraverseCtx<'a>) -> LambdaExpression {
        LambdaExpression {
            span: it.span.into(),
            params: self.trans_formal_parameters(&it.params, ctx),
            body: self.trans_function_body(&it.body, ctx),
            r#async: it.r#async,
            expression: it.expression,
        }
    }

    pub fn trans_function_expression(&mut self, it: &oxc::ast::ast::Function, ctx: &mut TraverseCtx<'a>) -> Function {
        Function {
            span: it.span.into(),
            r#type: FunctionType::FunctionExpression,
            params: self.trans_formal_parameters(&it.params, ctx),
            body: it.body.as_ref().map(|x| self.trans_function_body(x, ctx)),
            r#async: it.r#async,
            generator: it.generator,
            decorators: vec![],
            id: it.id.as_ref().map(|x| Self::trans_binding_identifier(x)),
        }
    }

    pub fn trans_directive(it: &oxc::ast::ast::Directive) -> Directive {
        Directive {
            span: it.span.into(),
            expression: Self::trans_string_literal(&it.expression),
            directive: it.directive.to_string(),
        }
    }

    pub fn trans_function_body(&mut self, it: &oxc::ast::ast::FunctionBody, ctx: &mut TraverseCtx<'a>) -> FunctionBody {
        FunctionBody {
            span: it.span.into(),
            statements: it.statements.iter().map(|x| self.trans_statement(x, ctx)).collect::<Vec<_>>(),
            directives: it.directives.iter().map(|x| Self::trans_directive(x)).collect::<Vec<_>>(),
        }
    }

    pub fn trans_function_declaration(&mut self, it: &oxc::ast::ast::Function, ctx: &mut TraverseCtx<'a>) -> Function {
        Function {
            span: it.span.into(),
            r#type: FunctionType::FunctionDeclaration,
            params: self.trans_formal_parameters(&it.params, ctx),
            body: it.body.as_ref().map(|x| self.trans_function_body(x, ctx)),
            r#async: it.r#async,
            generator: it.generator,
            // The decorator for function is not Stage 4.
            decorators: vec![],
            id: it.id.as_ref().map(|x| Self::trans_binding_identifier(x)),
        }
    }

    pub fn trans_declaration(&mut self, it: &oxc::ast::ast::Declaration, ctx: &mut TraverseCtx<'a>) -> Declaration {
        match it {
            oxc::ast::ast::Declaration::VariableDeclaration(it) => Declaration::VariableDeclaration(Box::new(self.trans_variable_declaration(it, ctx))),
            oxc::ast::ast::Declaration::FunctionDeclaration(it) => Declaration::FunctionDeclaration(Box::new(self.trans_function_declaration(it, ctx))),
            _ => unimplemented!()
        }
    }

    pub fn trans_decorator(&mut self, it: &oxc::ast::ast::Decorator, ctx: &mut TraverseCtx<'a>) -> Decorator {
        Decorator {
            span: it.span.into(),
            expression: self.trans_expression(&it.expression, ctx),
        }
    }


    pub fn trans_assignment_expression(&mut self, it: &oxc::ast::ast::AssignmentExpression, ctx: &mut TraverseCtx<'a>) -> Option<AssignmentExpression> {
        if !matches!(it.operator, oxc::ast::ast::AssignmentOperator::Assign) {
            // First we need to transform the assignment with other operators into a simple assignment with the operator
            let bin = BinaryExpression {
                span: SPAN.into(),
                left: self.trans_expression(it.left.get_expression()?, ctx),
                operator: it.operator.to_binary_operator()?.into(),
                right: self.trans_expression(&it.right, ctx),
            };
            Some(AssignmentExpression {
                span: it.span.into(),
                target: self.trans_assignment_target(&it.left, ctx),
                value: Expression::BinaryExpression(Box::new(bin)),
            })
        } else {
            Some(AssignmentExpression {
                span: it.span.into(),
                target: self.trans_assignment_target(&it.left, ctx),
                value: self.trans_expression(&it.right, ctx),
            })
        }
    }

    pub fn trans_await_expression(&mut self, it: &oxc::ast::ast::AwaitExpression, ctx: &mut TraverseCtx<'a>) -> AwaitExpression {
        AwaitExpression {
            span: it.span.into(),
            argument: self.trans_expression(&it.argument, ctx),
        }
    }

    pub fn trans_binary_expression(&mut self, it: &oxc::ast::ast::BinaryExpression, ctx: &mut TraverseCtx<'a>) -> BinaryExpression {
        BinaryExpression {
            span: it.span.into(),
            operator: it.operator.into(),
            left: self.trans_expression(&it.left, ctx),
            right: self.trans_expression(&it.right, ctx),
        }
    }

    pub fn trans_argument(&mut self, it: &oxc::ast::ast::Argument, ctx: &mut TraverseCtx<'a>) -> Expression {
        use oxc::ast::ast::Argument;
        match it {
            Argument::SpreadElement(it) => Expression::SpreadElement(Box::new(self.trans_spread_element(it, ctx))),
            it @ match_expression!(Argument) => self.trans_expression(it.as_expression().unwrap(), ctx)
        }
    }

    pub fn trans_call_expression(&mut self, it: &oxc::ast::ast::CallExpression, ctx: &mut TraverseCtx<'a>) -> CallExpression {
        CallExpression {
            span: it.span.into(),
            callee: self.trans_expression(&it.callee, ctx),
            arguments: it.arguments.iter().map(|x| self.trans_argument(x, ctx)).collect::<Vec<_>>(),
            new: false,
            optional: it.optional,
        }
    }

    pub fn trans_new_expression(&mut self, it: &oxc::ast::ast::NewExpression, ctx: &mut TraverseCtx<'a>) -> CallExpression {
        CallExpression {
            span: it.span.into(),
            callee: self.trans_expression(&it.callee, ctx),
            arguments: it.arguments.iter().map(|x| self.trans_argument(x, ctx)).collect::<Vec<_>>(),
            new: true,
            optional: false,
        }
    }

    pub fn trans_spread_element(&mut self, it: &oxc::ast::ast::SpreadElement, ctx: &mut TraverseCtx<'a>) -> SpreadElement {
        SpreadElement {
            span: it.span.into(),
            argument: self.trans_expression(&it.argument, ctx),
        }
    }

    pub fn trans_chain_expression(&mut self, it: &oxc::ast::ast::ChainExpression, ctx: &mut TraverseCtx<'a>) -> ChainExpression {
        ChainExpression {
            span: it.span.into(),
            expression: self.trans_chain_element(&it.expression, ctx),
        }
    }

    pub fn trans_chain_element(&mut self, it: &oxc::ast::ast::ChainElement, ctx: &mut TraverseCtx<'a>) -> Expression {
        use oxc::ast::ast::ChainElement;
        match it {
            ChainElement::CallExpression(it) => Expression::CallExpression(Box::new(self.trans_call_expression(it, ctx))),
            member_expr @ match_member_expression!(ChainElement) => Expression::MemberExpression(Box::new(self.trans_member_expression(member_expr.as_member_expression().unwrap(), ctx))),
            _ => unreachable!()
        }
    }

    pub fn trans_conditional_expression(&mut self, it: &oxc::ast::ast::ConditionalExpression, ctx: &mut TraverseCtx<'a>) -> ConditionalExpression {
        ConditionalExpression {
            span: it.span.into(),
            test: self.trans_expression(&it.test, ctx),
            consequent: self.trans_expression(&it.consequent, ctx),
            alternate: self.trans_expression(&it.alternate, ctx),
        }
    }

    pub fn trans_logical_expression(&mut self, it: &oxc::ast::ast::LogicalExpression, ctx: &mut TraverseCtx<'a>) -> LogicalExpression {
        LogicalExpression {
            span: it.span.into(),
            operator: match it.operator {
                oxc::ast::ast::LogicalOperator::And => LogicalOperator::LogicalAND,
                oxc::ast::ast::LogicalOperator::Or => LogicalOperator::LogicalOR,
                oxc::ast::ast::LogicalOperator::Coalesce => LogicalOperator::Coalesce,
            },
            left: self.trans_expression(&it.left, ctx),
            right: self.trans_expression(&it.right, ctx),
        }
    }

    pub fn trans_sequence_expression(&mut self, it: &oxc::ast::ast::SequenceExpression, ctx: &mut TraverseCtx<'a>) -> SequenceExpression {
        SequenceExpression {
            span: it.span.into(),
            expressions: it.expressions.iter().map(|x| self.trans_expression(x, ctx)).collect::<Vec<_>>(),
        }
    }

    pub fn trans_parenthesized_expression(&mut self, it: &oxc::ast::ast::ParenthesizedExpression, ctx: &mut TraverseCtx<'a>) -> ParenthesizedExpression {
        ParenthesizedExpression {
            span: it.span.into(),
            expression: self.trans_expression(&it.expression, ctx),
        }
    }

    pub fn trans_unary_expression(&mut self, it: &oxc::ast::ast::UnaryExpression, ctx: &mut TraverseCtx<'a>) -> UnaryExpression {
        UnaryExpression {
            span: it.span.into(),
            operator: it.operator.into(),
            argument: self.trans_expression(&it.argument, ctx),
        }
    }

    pub fn trans_update_expression(&mut self, it: &oxc::ast::ast::UpdateExpression, ctx: &mut TraverseCtx<'a>) -> UpdateExpression {
        UpdateExpression {
            span: it.span.into(),
            increment: matches!(it.operator, oxc::ast::ast::UpdateOperator::Increment),
            target: self.trans_simple_assignment_target(&it.argument, ctx),
            prefix: it.prefix,
        }
    }

    pub fn trans_yield_expression(&mut self, it: &oxc::ast::ast::YieldExpression, ctx: &mut TraverseCtx<'a>) -> YieldExpression {
        YieldExpression {
            span: it.span.into(),
            argument: it.argument.as_ref().map(|x| self.trans_expression(x, ctx)),
            delegate: it.delegate,
        }
    }

    pub fn object_expression(&mut self, it: &oxc::ast::ast::ObjectExpression, ctx: &mut TraverseCtx<'a>) -> ObjectExpression {
        ObjectExpression {
            span: it.span.into(),
            properties: it.properties.iter().map(|x| match x {
                oxc::ast::ast::ObjectPropertyKind::ObjectProperty(it) => Expression::ObjectProperty(Box::new(self.trans_object_property(it, ctx))),
                oxc::ast::ast::ObjectPropertyKind::SpreadProperty(it) => Expression::SpreadElement(Box::new(self.trans_spread_element(it, ctx)))
            }).collect::<Vec<_>>(),
            trailing_comma: it.trailing_comma.is_some(),
        }
    }

    pub fn trans_object_property(&mut self, it: &oxc::ast::ast::ObjectProperty, ctx: &mut TraverseCtx<'a>) -> ObjectProperty {
        use oxc::ast::ast::PropertyKey;
        ObjectProperty {
            span: it.span.into(),
            kind: it.kind.into(),
            shorthand: it.shorthand,
            computed: it.computed,
            key: match &it.key {
                PropertyKey::StaticIdentifier(id) => Expression::Identifier(Identifier {
                    span: id.span.into(),
                    name: id.name.to_string(),
                    symbol_id: None,
                    private: false,
                }),
                PropertyKey::PrivateIdentifier(id) => Expression::Identifier(Identifier {
                    span: id.span.into(),
                    name: id.name.to_string(),
                    symbol_id: None,
                    private: true,
                }),
                it @ match_expression!(PropertyKey) => self.trans_expression(it.as_expression().unwrap(), ctx)
            },
            value: self.trans_expression(&it.value, ctx),
            method: it.method,
        }
    }

    pub fn trans_statement(&mut self, it: &oxc::ast::ast::Statement, ctx: &mut TraverseCtx<'a>) -> Statement {
        use oxc::ast::ast::Statement as OxcStatement;
        match it {
            OxcStatement::BlockStatement(it) => Statement::BlockStatement(Box::new(self.trans_block_statement(it, ctx))),
            OxcStatement::BreakStatement(it) => Statement::BreakStatement(Box::new(self.trans_break_statement(it))),
            OxcStatement::ContinueStatement(it) => Statement::ContinueStatement(self.trans_continue_statement(it)),
            OxcStatement::DebuggerStatement(it) => Statement::DebuggerStatement(self.trans_debugger_statement(it)),
            OxcStatement::EmptyStatement(it) => Statement::EmptyStatement(self.trans_empty_statement(it)),
            OxcStatement::ExpressionStatement(it) => Statement::ExpressionStatement(Box::new(self.trans_expression_statement(it, ctx))),
            OxcStatement::ForStatement(it) => Statement::BlockStatement(Box::new(self.trans_three_part_for_statement(it, ctx))),
            OxcStatement::IfStatement(it) => Statement::IfStatement(Box::new(self.trans_if_statement(it, ctx))),
            OxcStatement::WhileStatement(it) => Statement::WhileStatement(Box::new(self.trans_while_statement(it, ctx))),
            OxcStatement::DoWhileStatement(it) => Statement::WhileStatement(Box::new(self.trans_do_while_statement(it, ctx))),
            OxcStatement::ForInStatement(it) => Statement::ForStatement(Box::new(self.trans_for_in_statement(it, ctx))),
            OxcStatement::ForOfStatement(it) => Statement::ForStatement(Box::new(self.trans_for_of_statement(it, ctx))),
            OxcStatement::ReturnStatement(it) => Statement::ReturnStatement(Box::new(self.trans_return_statement(it, ctx))),
            decl @ match_declaration!(OxcStatement) => Statement::Declaration(Box::new(self.trans_declaration(decl.as_declaration().unwrap(), ctx))),
            _ => unimplemented!(),
        }
    }

    pub fn trans_block_statement(&mut self, it: &oxc::ast::ast::BlockStatement, ctx: &mut TraverseCtx<'a>) -> BlockStatement {
        BlockStatement {
            span: it.span.into(),
            statements: it.body.iter().map(|x| self.trans_statement(x, ctx)).collect::<Vec<_>>(),
        }
    }

    pub fn trans_break_statement(&mut self, it: &oxc::ast::ast::BreakStatement) -> BreakStatement {
        if it.label.is_some() {
            unimplemented!("We currently do not support labels since it's JavaScript / C-like feature, which is not applicable for universal code.")
        }
        BreakStatement {
            span: it.span.into(),
        }
    }

    pub fn trans_continue_statement(&mut self, it: &oxc::ast::ast::ContinueStatement) -> ContinueStatement {
        ContinueStatement {
            span: it.span.into(),
        }
    }

    pub fn trans_debugger_statement(&mut self, it: &oxc::ast::ast::DebuggerStatement) -> DebuggerStatement {
        DebuggerStatement {
            span: it.span.into(),
        }
    }

    pub fn trans_empty_statement(&mut self, it: &oxc::ast::ast::EmptyStatement) -> EmptyStatement {
        EmptyStatement {
            span: it.span.into(),
        }
    }

    pub fn trans_expression_statement(&mut self, it: &oxc::ast::ast::ExpressionStatement, ctx: &mut TraverseCtx<'a>) -> ExpressionStatement {
        ExpressionStatement {
            span: it.span.into(),
            expression: self.trans_expression(&it.expression, ctx),
        }
    }

    /// Three part for is C-like for loop, but may not applicable for other languages. It can be transformed into a while loop, but there should handle some edge cases.
    pub fn trans_three_part_for_statement(&mut self, it: &oxc::ast::ast::ForStatement, ctx: &mut TraverseCtx<'a>) -> BlockStatement {
        use oxc::ast::ast::ForStatementInit;
        // First, we need to scope the init.
        let init = it.init.as_ref().map(|init| {
            match init {
                ForStatementInit::VariableDeclaration(decl) => Statement::Declaration(Box::from(Declaration::VariableDeclaration(Box::from(self.trans_variable_declaration(decl, ctx))))),
                it @ match_expression!(ForStatementInit) => {
                    let expr = it.as_expression().unwrap();
                    Statement::ExpressionStatement(Box::new(ExpressionStatement {
                        span: expr.span().into(),
                        expression: self.trans_expression(expr, ctx),
                    }))
                }
            }
        });
        // We should append the increment to the end of the body.
        let body = it.update.as_ref().map(|update| {
            match &it.body {
                oxc::ast::ast::Statement::BlockStatement(block) => {
                    let mut block = self.trans_block_statement(block, ctx);
                    block.statements.push(
                        Statement::ExpressionStatement(Box::new(ExpressionStatement {
                            span: update.span().into(),
                            expression: self.trans_expression(update, ctx),
                        }))
                    );
                    block
                }
                _ => {
                    BlockStatement {
                        span: it.span.into(),
                        statements: vec![
                            self.trans_statement(&it.body, ctx),
                            Statement::ExpressionStatement(Box::new(ExpressionStatement {
                                span: it.span.into(),
                                expression: self.trans_expression(update, ctx)
                            }))
                        ],
                    }
                }
            }
        });
        let new_while = WhileStatement {
            span: it.span.into(),
            test: it.test.as_ref().map(|x| self.trans_expression(x, ctx)).unwrap_or(Expression::BooleanLiteral(BooleanLiteral {
                span: it.span.into(),
                value: true,
            })),
            body: body.map(|x| Statement::BlockStatement(Box::new(x))).unwrap_or(self.trans_statement(&it.body, ctx)),
        };
        BlockStatement {
            span: it.span.into(),
            statements: if init.is_some() {
                vec![init.unwrap(), Statement::WhileStatement(Box::new(new_while))]
            } else {
                vec![Statement::WhileStatement(Box::new(new_while))]
            },
        }
    }

    pub fn trans_while_statement(&mut self, it: &oxc::ast::ast::WhileStatement, ctx: &mut TraverseCtx<'a>) -> WhileStatement {
        WhileStatement {
            span: it.span.into(),
            test: self.trans_expression(&it.test, ctx),
            body: self.trans_statement(&it.body, ctx),
        }
    }

    /// Translate the following code like this:
    ///
    /// ```js
    /// let i = 0;
    /// do {
    ///     // body
    ///     test = false;
    ///     i++;
    /// } while (i < 10);
    /// ```
    ///
    /// Into:
    ///
    /// ```js
    /// let i = 0;
    /// while (true) {
    ///     // body
    ///     test = false;
    ///     i++;
    ///     if (!(i < 10)) break;
    /// }
    pub fn trans_do_while_statement(&mut self, it: &oxc::ast::ast::DoWhileStatement, ctx: &mut TraverseCtx<'a>) -> WhileStatement {
        // We will modify it to `while` and `if` related statement.
        let body = self.trans_statement(&it.body, ctx);
        let new_if = IfStatement {
            span: it.span.into(),
            test: Expression::UnaryExpression(Box::from(UnaryExpression {
                span: it.span.into(),
                operator: UnaryOperator::LogicalNot,
                argument: self.trans_expression(&it.test, ctx),
            })),
            consequent: Statement::BreakStatement(Box::from(BreakStatement {
                span: it.span.into(),
            })),
            alternate: None,
        };
        WhileStatement {
            span: it.span.into(),
            test: Expression::BooleanLiteral(BooleanLiteral {
                span: it.test.span().into(),
                value: true,
            }),
            body: match body {
                Statement::BlockStatement(block) => {
                    let mut block = block;
                    block.statements.push(Statement::IfStatement(Box::new(new_if)));
                    Statement::BlockStatement(block)
                }
                _ => {
                    let block = BlockStatement {
                        span: it.span.into(),
                        statements: vec![body, Statement::IfStatement(Box::new(new_if))],
                    };
                    Statement::BlockStatement(Box::new(block))
                }
            },
        }
    }


    pub fn trans_return_statement(&mut self, it: &oxc::ast::ast::ReturnStatement, ctx: &mut TraverseCtx<'a>) -> ReturnStatement {
        ReturnStatement {
            span: it.span.into(),
            argument: it.argument.as_ref().map(|x| self.trans_expression(x, ctx)),
        }
    }

    pub fn trans_variable_declaration(&mut self, it: &oxc::ast::ast::VariableDeclaration, ctx: &mut TraverseCtx<'a>) -> VariableDeclaration {
        VariableDeclaration {
            span: it.span.into(),
            kind: it.kind.into(),
            declarations: it.declarations.iter().map(|x| self.trans_variable_declarator(x, ctx)).collect::<Vec<_>>(),
        }
    }

    pub fn trans_variable_declarator(&mut self, it: &oxc::ast::ast::VariableDeclarator, ctx: &mut TraverseCtx<'a>) -> VariableDeclarator {
        VariableDeclarator {
            span: it.span.into(),
            init: it.init.as_ref().map(|x| self.trans_expression(x, ctx)),
            id: self.trans_binding_pattern(&it.id, ctx),
        }
    }

    pub fn trans_binding_pattern(&mut self, it: &oxc::ast::ast::BindingPattern, ctx: &mut TraverseCtx<'a>) -> BindingPattern {
        BindingPattern {
            kind: match &it.kind {
                oxc::ast::ast::BindingPatternKind::BindingIdentifier(it) => BindingPatternKind::BindingIdentifier(Box::from(Self::trans_binding_identifier(it))),
                oxc::ast::ast::BindingPatternKind::ObjectPattern(it) => BindingPatternKind::ObjectPattern(Box::from(self.trans_object_pattern(it, ctx))),
                oxc::ast::ast::BindingPatternKind::ArrayPattern(it) => BindingPatternKind::ArrayPattern(Box::from(self.trans_array_pattern(it, ctx))),
                oxc::ast::ast::BindingPatternKind::AssignmentPattern(it) => BindingPatternKind::AssignmentPattern(Box::from(self.trans_assignment_pattern(it, ctx))),
            }
        }
    }

    pub fn trans_object_pattern(&mut self, it: &oxc::ast::ast::ObjectPattern, ctx: &mut TraverseCtx<'a>) -> ObjectPattern {
        ObjectPattern {
            span: it.span.into(),
            properties: it.properties.iter().map(|x| self.trans_binding_property(x, ctx)).collect::<Vec<_>>(),
            rest: it.rest.as_ref().map(|x| self.trans_binding_rest_element(x, ctx)),
        }
    }

    pub fn trans_array_pattern(&mut self, it: &oxc::ast::ast::ArrayPattern, ctx: &mut TraverseCtx<'a>) -> ArrayPattern {
        ArrayPattern {
            span: it.span.into(),
            elements: it.elements.iter().map(|x| x.as_ref().map(|y| self.trans_binding_pattern(y, ctx))).collect::<Vec<_>>(),
            rest: it.rest.as_ref().map(|x| self.trans_binding_rest_element(x, ctx)),
        }
    }

    pub fn trans_assignment_pattern(&mut self, it: &oxc::ast::ast::AssignmentPattern, ctx: &mut TraverseCtx<'a>) -> AssignmentPattern {
        AssignmentPattern {
            span: it.span.into(),
            left: self.trans_binding_pattern(&it.left, ctx),
            right: self.trans_expression(&it.right, ctx),
        }
    }

    pub fn trans_binding_property(&mut self, it: &oxc::ast::ast::BindingProperty, ctx: &mut TraverseCtx<'a>) -> BindingProperty {
        use oxc::ast::ast::PropertyKey;
        BindingProperty {
            span: it.span.into(),
            value: self.trans_binding_pattern(&it.value, ctx),
            key: match &it.key {
                PropertyKey::StaticIdentifier(id) => Expression::Identifier(Self::trans_identifier_name(id)),
                PropertyKey::PrivateIdentifier(id) => Expression::Identifier(Self::trans_private_identifier(id)),
                it @ match_expression!(PropertyKey) => self.trans_expression(it.as_expression().unwrap(), ctx)
            },
            shorthand: it.shorthand,
            computed: it.computed,
        }
    }

    pub fn trans_binding_rest_element(&mut self, it: &oxc::ast::ast::BindingRestElement, ctx: &mut TraverseCtx<'a>) -> BindingRestElement {
        BindingRestElement {
            span: it.span.into(),
            argument: self.trans_binding_pattern(&it.argument, ctx),
        }
    }

    pub fn trans_array_assignment_target(&mut self, it: &oxc::ast::ast::ArrayAssignmentTarget, ctx: &mut TraverseCtx<'a>) -> ArrayAssignmentTarget {
        ArrayAssignmentTarget {
            span: it.span.into(),
            elements: it.elements.iter().map(|x| x.as_ref().map(|y| self.trans_assignment_target_maybe_default(y, ctx))).collect::<Vec<_>>(),
            rest: it.rest.as_ref().map(|x| self.trans_assignment_target_rest(x, ctx)),
            trailing_comma: it.trailing_comma.is_some(),
        }
    }

    pub fn trans_object_assignment_target(&mut self, it: &oxc::ast::ast::ObjectAssignmentTarget, ctx: &mut TraverseCtx<'a>) -> ObjectAssignmentTarget {
        ObjectAssignmentTarget {
            span: it.span.into(),
            properties: it.properties.iter().map(|x| self.trans_assignment_target_property(x, ctx)).collect::<Vec<_>>(),
            rest: it.rest.as_ref().map(|x| self.trans_assignment_target_rest(x, ctx)),
        }
    }

    pub fn trans_assignment_target_property(&mut self, it: &oxc::ast::ast::AssignmentTargetProperty, ctx: &mut TraverseCtx<'a>) -> AssignmentTargetProperty {
        match it {
            oxc::ast::ast::AssignmentTargetProperty::AssignmentTargetPropertyIdentifier(it) => AssignmentTargetProperty::AssignmentTargetPropertyIdentifier(Box::new(self.trans_assignment_target_property_identifier(it, ctx))),
            oxc::ast::ast::AssignmentTargetProperty::AssignmentTargetPropertyProperty(it) => AssignmentTargetProperty::AssignmentTargetPropertyProperty(Box::new(self.trans_assignment_target_property_property(it, ctx)))
        }
    }

    pub fn trans_assignment_target_property_identifier(&mut self, it: &oxc::ast::ast::AssignmentTargetPropertyIdentifier, ctx: &mut TraverseCtx<'a>) -> AssignmentTargetPropertyIdentifier {
        AssignmentTargetPropertyIdentifier {
            span: it.span.into(),
            binding: self.trans_identifier_reference(&it.binding, ctx),
            init: it.init.as_ref().map(|x| self.trans_expression(x, ctx)),
        }
    }

    pub fn trans_assignment_target_property_property(&mut self, it: &oxc::ast::ast::AssignmentTargetPropertyProperty, ctx: &mut TraverseCtx<'a>) -> AssignmentTargetPropertyProperty {
        AssignmentTargetPropertyProperty {
            span: it.span.into(),
            name: self.trans_expression(it.name.as_expression().unwrap(), ctx),
            binding: self.trans_assignment_target_maybe_default(&it.binding, ctx),
        }
    }

    pub fn trans_assignment_target(&mut self, it: &oxc::ast::ast::AssignmentTarget, ctx: &mut TraverseCtx<'a>) -> AssignmentTarget {
        use oxc::ast::ast::{AssignmentTarget as OxcAssignmentTarget};
        match it {
            OxcAssignmentTarget::AssignmentTargetIdentifier(it) => AssignmentTarget::AssignmentTargetIdentifier(self.trans_identifier_reference(it, ctx)),
            OxcAssignmentTarget::ArrayAssignmentTarget(it) => AssignmentTarget::ArrayAssignmentTarget(Box::from(self.trans_array_assignment_target(it, ctx))),
            OxcAssignmentTarget::ObjectAssignmentTarget(it) => AssignmentTarget::ObjectAssignmentTarget(Box::from(self.trans_object_assignment_target(it, ctx))),
            OxcAssignmentTarget::ComputedMemberExpression(it) => AssignmentTarget::MemberExpression(Box::from(self.trans_computed_member_expression(it, ctx))),
            OxcAssignmentTarget::StaticMemberExpression(it) => AssignmentTarget::MemberExpression(Box::from(self.trans_static_member_expression(it, ctx))),
            OxcAssignmentTarget::PrivateFieldExpression(it) => AssignmentTarget::MemberExpression(Box::from(self.trans_private_field_expression(it, ctx))),
            _ => unreachable!(),
        }
    }

    pub fn trans_simple_assignment_target(&mut self, it: &oxc::ast::ast::SimpleAssignmentTarget, ctx: &mut TraverseCtx<'a>) -> AssignmentTarget {
        use oxc::ast::ast::{SimpleAssignmentTarget as OxcAssignmentTarget};
        match it {
            OxcAssignmentTarget::AssignmentTargetIdentifier(it) => AssignmentTarget::AssignmentTargetIdentifier(self.trans_identifier_reference(it, ctx)),
            OxcAssignmentTarget::ComputedMemberExpression(it) => AssignmentTarget::MemberExpression(Box::from(self.trans_computed_member_expression(it, ctx))),
            OxcAssignmentTarget::StaticMemberExpression(it) => AssignmentTarget::MemberExpression(Box::from(self.trans_static_member_expression(it, ctx))),
            OxcAssignmentTarget::PrivateFieldExpression(it) => AssignmentTarget::MemberExpression(Box::from(self.trans_private_field_expression(it, ctx))),
            _ => unreachable!(),
        }
    }

    pub fn trans_assignment_target_maybe_default(&mut self, it: &oxc::ast::ast::AssignmentTargetMaybeDefault, ctx: &mut TraverseCtx<'a>) -> AssignmentTarget {
        match it {
            oxc::ast::ast::AssignmentTargetMaybeDefault::AssignmentTargetWithDefault(it) => AssignmentTarget::AssignmentTargetWithDefault(Box::from(AssignmentTargetWithDefault {
                span: it.span.into(),
                binding: self.trans_assignment_target(&it.binding, ctx),
                init: self.trans_expression(&it.init, ctx),
            })),
            _ => self.trans_assignment_target(it.as_assignment_target().unwrap(), ctx)
        }
    }

    pub fn trans_assignment_target_rest(&mut self, it: &oxc::ast::ast::AssignmentTargetRest, ctx: &mut TraverseCtx<'a>) -> AssignmentTargetRest {
        AssignmentTargetRest {
            span: it.span.into(),
            target: self.trans_assignment_target(&it.target, ctx),
        }
    }

    pub fn trans_computed_member_expression(&mut self, it: &oxc::ast::ast::ComputedMemberExpression, ctx: &mut TraverseCtx<'a>) -> MemberExpression {
        MemberExpression {
            span: it.span.into(),
            object: self.trans_expression(&it.object, ctx),
            property: self.trans_expression(&it.expression, ctx),
            computed: true,
            private: false,
        }
    }

    pub fn trans_static_member_expression(&mut self, it: &oxc::ast::ast::StaticMemberExpression, ctx: &mut TraverseCtx<'a>) -> MemberExpression {
        MemberExpression {
            span: it.span.into(),
            object: self.trans_expression(&it.object, ctx),
            property: Expression::Identifier(Self::trans_identifier_name(&it.property)),
            computed: false,
            private: false,
        }
    }

    pub fn trans_private_field_expression(&mut self, it: &oxc::ast::ast::PrivateFieldExpression, ctx: &mut TraverseCtx<'a>) -> MemberExpression {
        MemberExpression {
            span: it.span.into(),
            object: self.trans_expression(&it.object, ctx),
            property: Expression::Identifier(Self::trans_private_identifier(&it.field)),
            computed: false,
            private: true,
        }
    }

    pub fn trans_for_statement_left(&mut self, it: &oxc::ast::ast::ForStatementLeft, ctx: &mut TraverseCtx<'a>) -> Expression {
        use oxc::ast::ast::ForStatementLeft;
        match it {
            ForStatementLeft::VariableDeclaration(it) => Expression::VariableDeclaration(Box::from(self.trans_variable_declaration(it, ctx))),
            expr @ match_member_expression!(ForStatementLeft) => Expression::MemberExpression(Box::from(self.trans_member_expression(expr.as_member_expression().unwrap(), ctx))),
            target @ match_assignment_target!(ForStatementLeft) => Expression::AssignmentTarget(Box::from(self.trans_assignment_target(target.as_assignment_target().unwrap(), ctx))),
        }
    }

    pub fn trans_for_in_statement(&mut self, it: &oxc::ast::ast::ForInStatement, ctx: &mut TraverseCtx<'a>) -> ForStatement {
        ForStatement {
            span: it.span.into(),
            iteration: false,
            left: self.trans_for_statement_left(&it.left, ctx),
            right: self.trans_expression(&it.right, ctx),
            body: self.trans_statement(&it.body, ctx),
            r#await: false,
        }
    }

    pub fn trans_for_of_statement(&mut self, it: &oxc::ast::ast::ForOfStatement, ctx: &mut TraverseCtx<'a>) -> ForStatement {
        ForStatement {
            span: it.span.into(),
            iteration: true,
            left: self.trans_for_statement_left(&it.left, ctx),
            right: self.trans_expression(&it.right, ctx),
            body: self.trans_statement(&it.body, ctx),
            r#await: it.r#await,
        }
    }

    pub fn trans_if_statement(&mut self, it: &oxc::ast::ast::IfStatement, ctx: &mut TraverseCtx<'a>) -> IfStatement {
        IfStatement {
            span: it.span.into(),
            test: self.trans_expression(&it.test, ctx),
            consequent: self.trans_statement(&it.consequent, ctx),
            alternate: it.alternate.as_ref().map(|x| self.trans_statement(x, ctx)),
        }
    }

    pub fn trans_member_expression(&mut self, it: &oxc::ast::ast::MemberExpression, ctx: &mut TraverseCtx<'a>) -> MemberExpression {
        match it {
            oxc::ast::ast::MemberExpression::ComputedMemberExpression(it) => self.trans_computed_member_expression(it, ctx),
            oxc::ast::ast::MemberExpression::StaticMemberExpression(it) => self.trans_static_member_expression(it, ctx),
            oxc::ast::ast::MemberExpression::PrivateFieldExpression(it) => self.trans_private_field_expression(it, ctx),
        }
    }


    pub fn trans_formal_parameter(&mut self, it: &oxc::ast::ast::FormalParameter, ctx: &mut TraverseCtx<'a>) -> FormalParameter {
        FormalParameter {
            span: it.span.into(),
            id: self.trans_binding_pattern(&it.pattern, ctx),
            decorators: it.decorators.iter().map(|x| self.trans_decorator(x, ctx)).collect::<Vec<_>>(),
        }
    }

    pub fn trans_formal_parameters(&mut self, it: &oxc::ast::ast::FormalParameters, ctx: &mut TraverseCtx<'a>) -> FormalParameters {
        FormalParameters {
            span: it.span.into(),
            kind: it.kind.into(),
            items: it.items.iter().map(|x| self.trans_formal_parameter(x, ctx)).collect::<Vec<_>>(),
            rest: it.rest.as_ref().map(|x| self.trans_binding_rest_element(x, ctx)),
        }
    }

    pub fn trans_hashbang(it: &oxc::ast::ast::Hashbang) -> Hashbang {
        Hashbang {
            span: it.span.into(),
            value: it.value.to_string(),
        }
    }

    pub fn trans_program(&mut self, it: &oxc::ast::ast::Program, ctx: &mut TraverseCtx<'a>) -> Program {
        Program {
            span: it.span.into(),
            body: it.body.iter().map(|x| self.trans_statement(x, ctx)).collect::<Vec<_>>(),
            directives: it.directives.iter().map(|x| Self::trans_directive(x)).collect::<Vec<_>>(),
            hashbang: it.hashbang.as_ref().map(|x| Self::trans_hashbang(x)),
        }
    }
}
