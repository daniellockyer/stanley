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

#[derive(Copy, Clone, PartialEq)]
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
    Not,
}

#[derive(Clone, PartialEq, Copy)]
pub enum Types {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Bool,
    Void,
    Unknown
}

pub fn type_to_enum(x: Ty) -> Types {
    match x.sty {
        TyBool => Types::Bool,
        TyInt(a) => match a {
            I8 => Types::I8,
            I16 => Types::I16,
            I32 => Types::I32,
            I64 => Types::I64,
            _ => unreachable!()
        },
        TyUint(a) => match a {
            U8 => Types::U8,
            U16 => Types::U16,
            U32 => Types::U32,
            U64 => Types::U64,
            _ => unreachable!()
        },
        _ => Types::Unknown
    }
}

pub fn string_to_type(s: String) -> Types {
    match s.as_str() {
        "bool" => Types::Bool,
        "i8" => Types::I8,
        "i16" => Types::I16,
        "i32" => Types::I32,
        "i64" => Types::I64,
        "u8" => Types::U8,
        "u16" => Types::U16,
        "u32" => Types::U32,
        "u64" => Types::U64,
        "()" => Types::Void,
        _ => unimplemented!(),
    }
}

impl Debug for Expression {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Expression::BinaryExpression(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", op, l, r),
            Expression::UnaryExpression(ref op, ref r) => write!(fmt, "({:?} {:?})", op, r),
            Expression::VariableMapping (ref name, ref var_type) => write!(fmt, "{}:{:?}", name, var_type),
            Expression::BitVector(ref val, ref s) => write!(fmt, "{:?}:{:?}", val, s),
            Expression::BooleanLiteral(ref b) => write!(fmt, "{:?}", b)
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
            BinaryOperator::And => write!(fmt, "∧"),
            BinaryOperator::Or => write!(fmt, "∨"),
            BinaryOperator::Xor => write!(fmt, "XOR"),
            BinaryOperator::Implication => write!(fmt, "->"),
            BinaryOperator::BiImplication => write!(fmt, "<->"),
        }
    }
}

impl Debug for UnaryOperator {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            UnaryOperator::Negation => write!(fmt, "-"),
            UnaryOperator::Not => write!(fmt, "¬")
        }
    }
}

impl Debug for Types {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Types::Bool => write!(fmt, "bool"),
            Types::I8 => write!(fmt, "i8"),
            Types::I16 => write!(fmt, "i16"),
            Types::I32 => write!(fmt, "i32"),
            Types::I64 => write!(fmt, "i64"),
            Types::U8 => write!(fmt, "u8"),
            Types::U16 => write!(fmt, "u16"),
            Types::U32 => write!(fmt, "u32"),
            Types::U64 => write!(fmt, "u64"),
            Types::Void => write!(fmt, "()"),
            Types::Unknown => write!(fmt, "?")
        }
    }
}

pub fn determine_evaluation_type(expression: &Expression) -> Types {
    match ty_check(expression) {
        Ok(_) => {
            match *expression {
                Expression::BinaryExpression(ref l, ref op, _) => {
                    match *op {
                        BinaryOperator::Addition | BinaryOperator::Subtraction | BinaryOperator::Multiplication
                        | BinaryOperator::Division | BinaryOperator::Modulo | BinaryOperator::BitwiseLeftShift
                        | BinaryOperator::BitwiseRightShift | BinaryOperator::BitwiseOr | BinaryOperator::BitwiseAnd
                        | BinaryOperator::BitwiseXor => determine_evaluation_type(l),
                        BinaryOperator::LessThan | BinaryOperator::LessThanOrEqual | BinaryOperator::GreaterThan 
                        | BinaryOperator::GreaterThanOrEqual | BinaryOperator::Equal | BinaryOperator::NotEqual
                        | BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor
                        | BinaryOperator::Implication | BinaryOperator::BiImplication => Types::Bool,
                    }
                },
                Expression::UnaryExpression(_, ref expr) => determine_evaluation_type(expr),
                Expression::VariableMapping(_, ref ty) => *ty,
                Expression::BooleanLiteral(_) => Types::Bool,
                Expression::BitVector(_, ref ty) => match *ty {
                    Types::Bool | Types::Void | Types::Unknown => error!("Invalid or Unsupported integer type: \"{:?}\"", ty),
                    _ => *ty
                }
            }
        },
        Err(e) => error!("{}", e),
    }
}

pub fn same_signedness(type1: Types, type2: Types) -> bool {
    match type1 {
        Types::U8 | Types::U16 | Types::U32 | Types::U64 => match type2 {
            Types::U8 | Types::U16 | Types::U32 | Types::U64 => true,
            Types::I8 | Types::I16 | Types::I32 | Types::I64 => false,
            _ => error!("Cannot find numeric signedness of `{:?}`", type2)
        },
        Types::I8 | Types::I16 | Types::I32 | Types::I64 => match type2 {
            Types::U8 | Types::U16 | Types::U32 | Types::U64 => false,
            Types::I8 | Types::I16 | Types::I32 | Types::I64 => true,
            _ => error!("Cannot find numeric signedness of `{:?}`", type2)
        },
        _ => error!("Cannot find numeric signedness of `{:?}`", type1)
    }
}

pub fn ty_check(expression: &Expression) -> Result<bool, String> {
    match *expression {
        Expression::BooleanLiteral(_) => Ok(true),
        Expression::VariableMapping(ref name, ref ty) => match *ty {
            Types::Void => Err(format!("Variable `{}` has void type!", name)),
            _ => Ok(true)
        },
        Expression::UnaryExpression(ref op, ref expr) => {
            match *op {
                UnaryOperator::Negation => match ty_check(expr) {
                    Ok(_) => match determine_evaluation_type(expr) {
                        Types::Bool => Err(format!("Invalid use of operator {:?} on boolean value {:?}", *op, *expr)),
                        _ => Ok(true)
                    },
                    Err(e) => Err(e)
                },
                UnaryOperator::Not => match determine_evaluation_type(expr) {
                    Types::Bool => Ok(true),
                    _ => Err(format!("Invalid use of operator {:?} on non-boolean value {:?}", *op, *expr))
                }
            }
        },
        Expression::BitVector(_, ref ty) => match *ty {
            Types::U8 | Types::U16 | Types::U32 | Types::U64 | Types::I8 | Types::I16 | Types::I32 | Types::I64 => Ok(true),
            _ => Err(format!("Invalid or unsupported integer type: \"{:?}\"", ty))
        },
        Expression::BinaryExpression(ref l, ref op, ref r) => {
            match ty_check(l) {
                Ok(_) => {
                    match ty_check(r) {
                        Ok(_) => {
                            let l_type: Types = determine_evaluation_type(l);
                            let r_type: Types = determine_evaluation_type(r);

                            match *op {
                                BinaryOperator::Addition | BinaryOperator::Subtraction | BinaryOperator::Multiplication | BinaryOperator::Division | BinaryOperator::Modulo => {
                                    if (l_type == Types::Bool) || (r_type == Types::Bool) {
                                        Err(format!("Invalid use of binary operator {:?} on boolean value: {:?} and {:?}", op, l_type, r_type))
                                    } else if l_type != r_type {
                                        Err(format!("Binary operand types do not match: {:?} {:?} {:?}", l_type, op, r_type))
                                    } else {
                                        Ok(true)
                                    }
                                },
                                BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift => {
                                    if (l_type == Types::Bool) || (r_type == Types::Bool) {
                                        Err(format!("Invalid use of binary operator {:?} on boolean value: {:?} and {:?}", op, l_type, r_type))
                                    } else if !same_signedness(l_type, r_type) {
                                        Err(format!("Binary operand types do not match: {:?} {:?} {:?}", l_type, op, r_type))
                                    } else {
                                        Ok(true)
                                    }
                                },
                                BinaryOperator::BitwiseOr | BinaryOperator::BitwiseAnd | BinaryOperator::BitwiseXor => {
                                    if l_type != r_type {
                                        Err(format!("Binary operand types do not match: {:?} {:?} {:?}", l_type, op, r_type))
                                    } else {
                                        Ok(true)
                                    }
                                },
                                BinaryOperator::LessThan | BinaryOperator::LessThanOrEqual | BinaryOperator::GreaterThan | BinaryOperator::GreaterThanOrEqual => {
                                    if (l_type == Types::Bool) || (r_type == Types::Bool) {
                                        Err(format!("Invalid use of binary operator {:?} on boolean value: {:?} and {:?}", op, l_type, r_type))
                                    } else if l_type != r_type {
                                        Err(format!("Binary operand types do not match: {:?} {:?} {:?}", l_type, op, r_type))
                                    } else {
                                        Ok(true)
                                    }
                                },
                                BinaryOperator::Equal | BinaryOperator::NotEqual => {
                                    if l_type != r_type {
                                        Err(format!("Binary operand types do not match: {:?} {:?} {:?}", l_type, op, r_type))
                                    } else {
                                        Ok(true)
                                    }
                                },
                                BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor | BinaryOperator::Implication | BinaryOperator::BiImplication => {
                                    if (l_type != Types::Bool) || (r_type != Types::Bool) {
                                        Err(format!("Invalid use of binary operator {:?} on boolean value: {:?} and {:?}", op, l_type, r_type))
                                    } else if l_type != r_type {
                                        Err(format!("Binary operand types do not match: {:?} {:?} {:?}", l_type, op, r_type))
                                    } else {
                                        Ok(true)
                                    }
                                }
                            }
                        },
                        Err(e) => Err(e)
                    }
                },
                Err(e) => Err(e)
            }
        }
    }
}
