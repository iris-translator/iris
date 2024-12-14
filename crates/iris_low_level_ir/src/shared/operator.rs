use std::error::Error;
use std::fmt::Display;
use oxc::ast::ast::{
    BinaryOperator as OxcBinaryOperator, LogicalOperator as OxcLogicalOperator,
    UnaryOperator as OxcUnaryOperator,
};
use ruff::ast::{Operator as RuffBinaryOperator, CmpOp as RuffComparisonOperator, UnaryOp as RuffUnaryOperator, BoolOp as RuffLogicalOperator};

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