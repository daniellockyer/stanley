use std::fmt::{Debug, Formatter, Error};
use rustc::ty::Ty;
use rustc::ty::TypeVariants::*;
use syntax::ast::IntTy::*;
use syntax::ast::UintTy::*;

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

pub fn type_to_enum(x: Ty) -> Types {
    match x.sty {
        TyBool => Types::Bool,
        TyInt(a) => {
            match a {
                I8 => Types::I8,
                I16 => Types::I16,
                I32 => Types::I32,
                I64 => Types::I64,
                _ => unreachable!() //TODO: Is
            }
        },
        TyUint(a) => {
            match a {
                U8 => Types::U8,
                U16 => Types::U16,
                U32 => Types::U32,
                U64 => Types::U64,
                _ => unreachable!() //TODO: Us
            }
        }
        _ => Types::Unknown
    }
}

impl PartialEq for BinaryOperator {
    fn eq(&self, _rhs: &BinaryOperator) -> bool {
        return self == _rhs;
    }
}

impl Debug for Expression {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Expression::BinaryExpression(ref l, op, ref r) => write!(fmt, "BinaryExpression({:?} {:?} {:?})", l, op, r),
            Expression::UnaryExpression(ref op, ref r) => write!(fmt, "UnaryExpression({:?} {:?})", op, r),
            Expression::VariableMapping (ref name, ref var_type) => write!(fmt, "VariableMapping({:?}:{:?})", name, var_type),
            Expression::BitVector(ref val, ref s) => write!(fmt, "BitVector({:?}:{:?})", val, s),
            Expression::BooleanLiteral(b) => write!(fmt, "BooleanLiteral({:?})", b)
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