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
        use self::Expression::*;
        match *self {
            BinaryExpression(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            Number(n) => write!(fmt, "{:?}", n),
            BooleanLiteral(b) => write!(fmt, "{:?}", b)
        }
    }
}

impl Debug for BinaryOperator {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::BinaryOperator::*;
        match *self {
            Addition => write!(fmt, "+"),
            Subtraction => write!(fmt, "-"),
            Multiplication => write!(fmt, "*"),
            Division => write!(fmt, "/"),
            Modulo => write!(fmt, "%"),
        }
    }
}