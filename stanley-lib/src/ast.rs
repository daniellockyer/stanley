use std::fmt::{Debug, Formatter, Error};

pub enum Expression {
    BinaryExpression(Box<Expression>, BinaryOperator, Box<Expression>),
    Number(i32),
    BooleanLiteral(bool),
}

#[derive(Copy, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo
}

impl Debug for Expression {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Expression::BinaryExpression(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            Expression::Number(n) => write!(fmt, "{:?}", n),
            Expression::BooleanLiteral(b) => write!(fmt, "{:?}", b)
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
        }
    }
}