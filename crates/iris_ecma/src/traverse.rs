use std::mem;
use crate::toolchain::ast::Visit;
use crate::toolchain::utils::constant_evaluation::ConstantEvaluation;
use iris_low_level_ir::shared::*;


pub struct IrisTraverseToIR {
    pub ir: Program
}

impl<'a> Visit<'a> for IrisTraverseToIR {
    fn visit_program(&mut self, it: &oxc::ast::ast::Program<'a>) {

    }

    fn visit_hashbang(&mut self, it: &oxc::ast::ast::Hashbang<'a>) {
        self.ir.hashbang = Some(Hashbang {
            span: it.span.into(),
            value: it.value.to_string()
        });
    }

    fn visit_expression(&mut self, it: &oxc::ast::ast::Expression<'a>) {
        todo!()
    }
}

impl<'a> IrisTraverseToIR {
    pub fn trans_boolean_literal(it: &oxc::ast::ast::BooleanLiteral) -> BooleanLiteral {
        BooleanLiteral {
            span: it.span.into(),
            value: it.value
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
            }
        }
    }

    pub fn trans_bigint_literal(it: &oxc::ast::ast::BigIntLiteral) -> NumericLiteral {
        NumericLiteral {
            span: it.span.into(),
            value: it.raw.trim_end_matches('n').parse().unwrap(),
            raw: it.raw.to_string(),
            base: NumberBase::BigInt
        }
    }

    pub fn trans_regexp_literal(it: &oxc::ast::ast::RegExpLiteral) -> ! {
        unimplemented!()
    }

    pub fn trans_string_literal(it: &oxc::ast::ast::StringLiteral) -> StringLiteral {
        StringLiteral {
            span: it.span.into(),
            value: it.value.to_string(),
            prefix: StringLiteralPrefix::Empty
        }
    }

    pub fn trans_template_literal(it: &oxc::ast::ast::TemplateLiteral) -> TemplateLiteral {
        todo!()
    }

    pub fn trans_identifier(it: &oxc::ast::ast::IdentifierReference) -> Identifier {
        Identifier {
            span: it.span.into(),
            name: it.name.to_string()
        }
    }

}
