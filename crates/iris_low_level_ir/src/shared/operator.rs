use std::error::Error;
use std::fmt::Display;

use oxc::ast::ast::{
    BinaryOperator as OxcBinaryOperator, LogicalOperator as OxcLogicalOperator,
    UnaryOperator as OxcUnaryOperator,
};
use ruff::ast::{
    BoolOp as RuffLogicalOperator, CmpOp as RuffComparisonOperator, Operator as RuffBinaryOperator,
    UnaryOp as RuffUnaryOperator,
};

#[derive(Debug, Clone)]
pub struct TransformError {
    pub message: String,
}

impl Display for TransformError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TransformError: {}", self.message)
    }
}

impl Error for TransformError {
    fn description(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    UnaryPlus,
    UnaryNegation,
    LogicalNot,
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}

impl From<OxcUnaryOperator> for UnaryOperator {
    fn from(value: OxcUnaryOperator) -> Self {
        match value {
            OxcUnaryOperator::UnaryPlus => UnaryOperator::UnaryPlus,
            OxcUnaryOperator::UnaryNegation => UnaryOperator::UnaryNegation,
            OxcUnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
            OxcUnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
            OxcUnaryOperator::Typeof => UnaryOperator::Typeof,
            OxcUnaryOperator::Void => UnaryOperator::Void,
            OxcUnaryOperator::Delete => UnaryOperator::Delete,
        }
    }
}

impl Into<OxcUnaryOperator> for UnaryOperator {
    fn into(self) -> OxcUnaryOperator {
        match self {
            UnaryOperator::UnaryPlus => OxcUnaryOperator::UnaryPlus,
            UnaryOperator::UnaryNegation => OxcUnaryOperator::UnaryNegation,
            UnaryOperator::LogicalNot => OxcUnaryOperator::LogicalNot,
            UnaryOperator::BitwiseNot => OxcUnaryOperator::BitwiseNot,
            UnaryOperator::Typeof => OxcUnaryOperator::Typeof,
            UnaryOperator::Void => OxcUnaryOperator::Void,
            UnaryOperator::Delete => OxcUnaryOperator::Delete,
        }
    }
}

impl From<RuffUnaryOperator> for UnaryOperator {
    fn from(value: RuffUnaryOperator) -> Self {
        match value {
            RuffUnaryOperator::UAdd => UnaryOperator::UnaryPlus,
            RuffUnaryOperator::USub => UnaryOperator::UnaryNegation,
            RuffUnaryOperator::Not => UnaryOperator::LogicalNot,
            RuffUnaryOperator::Invert => UnaryOperator::BitwiseNot,
        }
    }
}

impl Into<RuffUnaryOperator> for UnaryOperator {
    fn into(self) -> RuffUnaryOperator {
        match self {
            UnaryOperator::UnaryPlus => RuffUnaryOperator::UAdd,
            UnaryOperator::UnaryNegation => RuffUnaryOperator::USub,
            UnaryOperator::LogicalNot => RuffUnaryOperator::Not,
            UnaryOperator::BitwiseNot => RuffUnaryOperator::Invert,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // ==
    Equality,
    // !=
    Inequality,

    // JavaScript-specific
    StrictEquality,
    StrictInequality,

    // Comparison
    LessThan,
    LessEqualThan,
    GreaterThan,
    GreaterEqualThan,

    // Arithmetic
    Addition,
    Subtraction,
    Multiplication,
    // Python-specific -- @
    MatMultiplication,
    Division,
    FloorDivision,
    Remainder,
    Exponential,

    // Binary
    ShiftLeft,
    ShiftRight,
    // JavaScript-specific -- >>>
    ShiftRightZeroFill,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,

    // Relational
    In,
    NotIn,
    Instanceof,

    Is,
    IsNot,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            BinaryOperator::Addition => "+",
            BinaryOperator::Subtraction => "-",
            BinaryOperator::Multiplication => "*",
            BinaryOperator::MatMultiplication => "@",
            BinaryOperator::Division => "/",
            BinaryOperator::FloorDivision => "//",
            BinaryOperator::Remainder => "%",
            BinaryOperator::Exponential => "**",

            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
            BinaryOperator::ShiftRightZeroFill => ">>>",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::BitwiseAnd => "&",

            BinaryOperator::Equality => "==",
            BinaryOperator::Inequality => "!=",
            BinaryOperator::StrictEquality => "===",
            BinaryOperator::StrictInequality => "!==",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessEqualThan => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterEqualThan => ">=",

            BinaryOperator::In => "in",
            BinaryOperator::NotIn => "not in",
            BinaryOperator::Instanceof => "instanceof",

            BinaryOperator::Is => "is",
            BinaryOperator::IsNot => "is not",
        };
        write!(f, "{}", op)
    }
}

impl BinaryOperator {
    #[must_use]
    pub fn is_arithmetic(self) -> bool {
        matches!(
            self,
            BinaryOperator::Addition
                | BinaryOperator::Subtraction
                | BinaryOperator::Multiplication
                | BinaryOperator::MatMultiplication
                | BinaryOperator::Division
                | BinaryOperator::FloorDivision
                | BinaryOperator::Remainder
                | BinaryOperator::Exponential
        )
    }

    #[must_use]
    pub fn is_bitwise(self) -> bool {
        matches!(
            self,
            BinaryOperator::ShiftLeft
                | BinaryOperator::ShiftRight
                | BinaryOperator::ShiftRightZeroFill
                | BinaryOperator::BitwiseOr
                | BinaryOperator::BitwiseXor
                | BinaryOperator::BitwiseAnd
        )
    }

    #[must_use]
    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            BinaryOperator::Equality
                | BinaryOperator::Inequality
                | BinaryOperator::StrictEquality
                | BinaryOperator::StrictInequality
                | BinaryOperator::LessThan
                | BinaryOperator::LessEqualThan
                | BinaryOperator::GreaterThan
                | BinaryOperator::GreaterEqualThan
        )
    }

    #[must_use]
    pub fn is_relational(self) -> bool {
        matches!(self, BinaryOperator::In | BinaryOperator::NotIn | BinaryOperator::Instanceof)
    }

    #[must_use]
    pub fn is_pattern_matching(self) -> bool {
        matches!(self, BinaryOperator::Is | BinaryOperator::IsNot)
    }
}

impl From<OxcBinaryOperator> for BinaryOperator {
    fn from(value: OxcBinaryOperator) -> Self {
        match value {
            OxcBinaryOperator::Addition => BinaryOperator::Addition,
            OxcBinaryOperator::Subtraction => BinaryOperator::Subtraction,
            OxcBinaryOperator::Multiplication => BinaryOperator::Multiplication,
            OxcBinaryOperator::Division => BinaryOperator::Division,
            OxcBinaryOperator::Remainder => BinaryOperator::Remainder,
            OxcBinaryOperator::Exponential => BinaryOperator::Exponential,

            OxcBinaryOperator::ShiftLeft => BinaryOperator::ShiftLeft,
            OxcBinaryOperator::ShiftRight => BinaryOperator::ShiftRight,
            OxcBinaryOperator::ShiftRightZeroFill => BinaryOperator::ShiftRightZeroFill,
            OxcBinaryOperator::BitwiseOR => BinaryOperator::BitwiseOr,
            OxcBinaryOperator::BitwiseXOR => BinaryOperator::BitwiseXor,
            OxcBinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,

            OxcBinaryOperator::Equality => BinaryOperator::Equality,
            OxcBinaryOperator::Inequality => BinaryOperator::Inequality,
            OxcBinaryOperator::StrictEquality => BinaryOperator::StrictEquality,
            OxcBinaryOperator::StrictInequality => BinaryOperator::StrictInequality,
            OxcBinaryOperator::LessThan => BinaryOperator::LessThan,
            OxcBinaryOperator::LessEqualThan => BinaryOperator::LessEqualThan,
            OxcBinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            OxcBinaryOperator::GreaterEqualThan => BinaryOperator::GreaterEqualThan,

            OxcBinaryOperator::In => BinaryOperator::In,
            OxcBinaryOperator::Instanceof => BinaryOperator::Instanceof,
        }
    }
}

impl From<RuffBinaryOperator> for BinaryOperator {
    fn from(value: RuffBinaryOperator) -> Self {
        match value {
            RuffBinaryOperator::Add => BinaryOperator::Addition,
            RuffBinaryOperator::Sub => BinaryOperator::Subtraction,
            RuffBinaryOperator::Mult => BinaryOperator::Multiplication,
            RuffBinaryOperator::MatMult => BinaryOperator::MatMultiplication,
            RuffBinaryOperator::FloorDiv => BinaryOperator::FloorDivision,
            RuffBinaryOperator::Div => BinaryOperator::Division,
            RuffBinaryOperator::Mod => BinaryOperator::Remainder,
            RuffBinaryOperator::Pow => BinaryOperator::Exponential,

            RuffBinaryOperator::LShift => BinaryOperator::ShiftLeft,
            RuffBinaryOperator::RShift => BinaryOperator::ShiftRight,

            RuffBinaryOperator::BitOr => BinaryOperator::BitwiseOr,
            RuffBinaryOperator::BitXor => BinaryOperator::BitwiseXor,
            RuffBinaryOperator::BitAnd => BinaryOperator::BitwiseAnd,
        }
    }
}

impl Into<RuffBinaryOperator> for BinaryOperator {
    fn into(self) -> RuffBinaryOperator {
        match self {
            BinaryOperator::Addition => RuffBinaryOperator::Add,
            BinaryOperator::Subtraction => RuffBinaryOperator::Sub,
            BinaryOperator::Multiplication => RuffBinaryOperator::Mult,
            BinaryOperator::MatMultiplication => RuffBinaryOperator::MatMult,
            BinaryOperator::FloorDivision => RuffBinaryOperator::FloorDiv,
            BinaryOperator::Division => RuffBinaryOperator::Div,
            BinaryOperator::Remainder => RuffBinaryOperator::Mod,
            BinaryOperator::Exponential => RuffBinaryOperator::Pow,

            BinaryOperator::ShiftLeft => RuffBinaryOperator::LShift,
            BinaryOperator::ShiftRight => RuffBinaryOperator::RShift,

            BinaryOperator::BitwiseOr => RuffBinaryOperator::BitOr,
            BinaryOperator::BitwiseXor => RuffBinaryOperator::BitXor,
            BinaryOperator::BitwiseAnd => RuffBinaryOperator::BitAnd,

            _ => unreachable!(
                "These are not binary operators in Ruff. You may refer to comparison operators."
            ),
        }
    }
}

impl From<RuffComparisonOperator> for BinaryOperator {
    fn from(value: RuffComparisonOperator) -> Self {
        match value {
            RuffComparisonOperator::Eq => BinaryOperator::Equality,
            RuffComparisonOperator::NotEq => BinaryOperator::Inequality,
            RuffComparisonOperator::Lt => BinaryOperator::LessThan,
            RuffComparisonOperator::LtE => BinaryOperator::LessEqualThan,
            RuffComparisonOperator::Gt => BinaryOperator::GreaterThan,
            RuffComparisonOperator::GtE => BinaryOperator::GreaterEqualThan,
            RuffComparisonOperator::Is => BinaryOperator::Is,
            RuffComparisonOperator::IsNot => BinaryOperator::IsNot,
            RuffComparisonOperator::In => BinaryOperator::In,
            RuffComparisonOperator::NotIn => BinaryOperator::NotIn,
        }
    }
}

impl Into<RuffComparisonOperator> for BinaryOperator {
    fn into(self) -> RuffComparisonOperator {
        match self {
            BinaryOperator::Equality | BinaryOperator::StrictEquality => RuffComparisonOperator::Eq,
            BinaryOperator::Inequality | BinaryOperator::StrictInequality => {
                RuffComparisonOperator::NotEq
            }
            BinaryOperator::LessThan => RuffComparisonOperator::Lt,
            BinaryOperator::LessEqualThan => RuffComparisonOperator::LtE,
            BinaryOperator::GreaterThan => RuffComparisonOperator::Gt,
            BinaryOperator::GreaterEqualThan => RuffComparisonOperator::GtE,
            BinaryOperator::Is => RuffComparisonOperator::Is,
            BinaryOperator::IsNot => RuffComparisonOperator::IsNot,
            BinaryOperator::In => RuffComparisonOperator::In,
            BinaryOperator::NotIn => RuffComparisonOperator::NotIn,
            _ => unreachable!(
                "These are not comparison operators in Ruff. You may refer to binary operators."
            ),
        }
    }
}

impl Into<OxcBinaryOperator> for BinaryOperator {
    fn into(self) -> OxcBinaryOperator {
        match self {
            BinaryOperator::Addition => OxcBinaryOperator::Addition,
            BinaryOperator::Subtraction => OxcBinaryOperator::Subtraction,
            BinaryOperator::Multiplication => OxcBinaryOperator::Multiplication,
            BinaryOperator::Division => OxcBinaryOperator::Division,
            BinaryOperator::Remainder => OxcBinaryOperator::Remainder,
            BinaryOperator::Exponential => OxcBinaryOperator::Exponential,

            BinaryOperator::ShiftLeft => OxcBinaryOperator::ShiftLeft,
            BinaryOperator::ShiftRight => OxcBinaryOperator::ShiftRight,
            BinaryOperator::ShiftRightZeroFill => OxcBinaryOperator::ShiftRightZeroFill,
            BinaryOperator::BitwiseOr => OxcBinaryOperator::BitwiseOR,
            BinaryOperator::BitwiseXor => OxcBinaryOperator::BitwiseXOR,
            BinaryOperator::BitwiseAnd => OxcBinaryOperator::BitwiseAnd,

            BinaryOperator::Equality => OxcBinaryOperator::Equality,
            BinaryOperator::Inequality => OxcBinaryOperator::Inequality,
            BinaryOperator::StrictEquality => OxcBinaryOperator::StrictEquality,
            BinaryOperator::StrictInequality => OxcBinaryOperator::StrictInequality,
            BinaryOperator::LessThan => OxcBinaryOperator::LessThan,
            BinaryOperator::LessEqualThan => OxcBinaryOperator::LessEqualThan,
            BinaryOperator::GreaterThan => OxcBinaryOperator::GreaterThan,
            BinaryOperator::GreaterEqualThan => OxcBinaryOperator::GreaterEqualThan,

            BinaryOperator::In => OxcBinaryOperator::In,
            BinaryOperator::Instanceof => OxcBinaryOperator::Instanceof,

            _ => unreachable!(
                "These are not binary operators in Oxc. You may refer to comparison operators."
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperator {
    Or,
    And,
}

impl From<OxcLogicalOperator> for LogicalOperator {
    fn from(value: OxcLogicalOperator) -> Self {
        match value {
            OxcLogicalOperator::Or => LogicalOperator::Or,
            OxcLogicalOperator::And => LogicalOperator::And,
            OxcLogicalOperator::Coalesce => {
                unreachable!("Coalesce operator is not supported, as it is already transformed.")
            }
        }
    }
}

impl From<RuffLogicalOperator> for LogicalOperator {
    fn from(value: RuffLogicalOperator) -> Self {
        match value {
            RuffLogicalOperator::Or => LogicalOperator::Or,
            RuffLogicalOperator::And => LogicalOperator::And,
        }
    }
}

impl Into<OxcLogicalOperator> for LogicalOperator {
    fn into(self) -> OxcLogicalOperator {
        match self {
            LogicalOperator::Or => OxcLogicalOperator::Or,
            LogicalOperator::And => OxcLogicalOperator::And,
        }
    }
}

impl Into<RuffLogicalOperator> for LogicalOperator {
    fn into(self) -> RuffLogicalOperator {
        match self {
            LogicalOperator::Or => RuffLogicalOperator::Or,
            LogicalOperator::And => RuffLogicalOperator::And,
        }
    }
}
