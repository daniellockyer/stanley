use std::fmt::{Debug, Formatter, Error};

#[derive(Clone, PartialEq)]
pub enum Expression {
    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryExpression(UnaryOperator, Box<Expression>),
    VariableMapping(String, Types),
    BitVector(i64, Types),
    BooleanLiteral(bool),
}

#[derive(Copy, Clone)]
pub enum BinaryOperator {
    // Normal operators
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    // Bitwise operators
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    // Comparison operators
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    // Boolean logical operators
    And,
    Or,
    Xor,
    Implication,
    BiImplication,
}

#[derive(Clone, PartialEq)]
pub enum UnaryOperator {
    Negation,
    BitwiseNot,
    Not,
}

#[derive(Clone, PartialEq)]
pub enum Types {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Unknown
}

impl PartialEq for BinaryOperator {
    fn eq(&self, _rhs: &BinaryOperator) -> bool {
        return self == _rhs;
    }
}

impl Debug for Expression {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Expression::BinaryExpression(ref l, op, ref r) => write!(fmt, "Expression::BinaryExpression({:?} {:?} {:?})", l, op, r),
            Expression::UnaryExpression(ref op, ref r) => write!(fmt, "Expression::UnaryExpression({:?} {:?})", op, r),
            Expression::VariableMapping (ref name, ref var_type) => write!(fmt, "Expression::VariableMapping({:?}:{:?})", name, var_type),
            Expression::BitVector(ref val, ref s) => write!(fmt, "Expression::BitVector({:?}:{:?})", val, s),
            Expression::BooleanLiteral(b) => write!(fmt, "Expression::BooleanLiteral({:?})", b)
        }
    }
}

impl Debug for BinaryOperator {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            BinaryOperator::Addition => write!(fmt, "+"),
            BinaryOperator::Subtraction => write!(fmt, "-"),
            BinaryOperator::Multiplication => write!(fmt, "*"),
            BinaryOperator::Division => write!(fmt, "/"),
            BinaryOperator::Modulo => write!(fmt, "%"),
            BinaryOperator::BitwiseOr => write!(fmt, "|"),
            BinaryOperator::BitwiseAnd => write!(fmt, "&"),
            BinaryOperator::BitwiseXor => write!(fmt, "^"),
            BinaryOperator::BitwiseLeftShift => write!(fmt, "<<"),
            BinaryOperator::BitwiseRightShift => write!(fmt, ">>"),
            BinaryOperator::LessThan => write!(fmt, "<"),
            BinaryOperator::LessThanOrEqual => write!(fmt, "<="),
            BinaryOperator::GreaterThan => write!(fmt, ">"),
            BinaryOperator::GreaterThanOrEqual => write!(fmt, ">="),
            BinaryOperator::Equal => write!(fmt, "=="),
            BinaryOperator::NotEqual => write!(fmt, "!="),
            BinaryOperator::And => write!(fmt, "AND"),
            BinaryOperator::Or => write!(fmt, "OR"),
            BinaryOperator::Xor => write!(fmt, "XOR"),
            BinaryOperator::Implication => write!(fmt, "IMPLIES"),
            BinaryOperator::BiImplication => write!(fmt, "EQUIV"),
        }
    }
}

impl Debug for UnaryOperator {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            UnaryOperator::Negation => { write!(fmt, "-") },
            UnaryOperator::BitwiseNot => { write!(fmt, "!") },
            UnaryOperator::Not => { write!(fmt, "NOT") }
        }
    }
}

impl Debug for Types {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Types::Bool => { write!(fmt, "bool") },
            Types::I8 => { write!(fmt, "i8") },
            Types::I16 => { write!(fmt, "i16") },
            Types::I32 => { write!(fmt, "i32") },
            Types::I64 => { write!(fmt, "i64") },
            Types::U8 => { write!(fmt, "u8") },
            Types::U16 => { write!(fmt, "u16") },
            Types::U32 => { write!(fmt, "u32") },
            Types::U64 => { write!(fmt, "u64") },
            Types::Unknown => { write!(fmt, "?") }
        }
    }
}