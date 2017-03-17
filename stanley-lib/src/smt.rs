use rustproof_libsmt::backends::smtlib2::*;
use rustproof_libsmt::backends::backend::*;
use rustproof_libsmt::backends::z3;
use rustproof_libsmt::theories::{bitvec, core};
use rustproof_libsmt::logics::qf_aufbv::*;
use petgraph::graph::NodeIndex;
use ast::{Expression, BinaryOperator, UnaryOperator, Types};

//use rustc::mir::*;
use std::fmt::Debug;
use regex::Regex;

use ast;

pub fn run_solver(verification_condition: &Expression, name: &String) {
    let mut z3: z3::Z3 = Default::default();
    let mut solver = SMTLib2::new(Some(QFAUFBV));
    let simplified_condition = ast::simplify_expression(&verification_condition);
    let vcon = solver.expr2smtlib(&simplified_condition);
    let _ = solver.assert(core::OpCodes::Not, &[vcon]);
    let (_, check) = solver.solve(&mut z3, false);

    match check {
        SMTRes::Sat(_, ref model) => {
            let re = Regex::new(r".+(\(define-fun\s+([a-zA-Z0-9]+).*\s+#x([0-9a-f]+)\))+").unwrap();
            let text = model.clone().unwrap();

            println!("!! [INVALID] -- {}", name);
            /*println!("{:?}", verification_condition);
            println!("{:?}", simplified_condition);*/

            for cap in re.captures_iter(&text) {
                println!("   {:7} = {:10?} (0x{})", &cap[2], i64::from_str_radix(&cap[3], 16).unwrap(), &cap[3]);
            }
        },
        SMTRes::Unsat(..) => println!("[VALID] -- {}", name),
        SMTRes::Error(ref error, _) => println!("[ERROR]\n{}\n", error)
    }
}

pub trait Pred2SMT {
    type Idx: Debug + Clone;
    type Logic: Logic;

    fn expr2smtlib (&mut self, &Expression) -> Self::Idx;
}

impl Pred2SMT for SMTLib2<QFAUFBV> {
    type Idx = NodeIndex;
    type Logic = QFAUFBV;

    fn expr2smtlib (&mut self, vc: &Expression) -> Self::Idx {
        match *vc {
            Expression::BinaryExpression (ref left, ref op, ref right) => {
                let l = self.expr2smtlib(left.as_ref());
                let r = self.expr2smtlib(right.as_ref());

                match *op {
                    BinaryOperator::Addition => self.assert(bitvec::OpCodes::BvAdd, &[l,r]),
                    BinaryOperator::Subtraction => self.assert(bitvec::OpCodes::BvSub, &[l,r]),
                    BinaryOperator::Multiplication => self.assert(bitvec::OpCodes::BvMul, &[l,r]),
                    BinaryOperator::Division => self.assert(bitvec::OpCodes::BvSDiv, &[l,r]),
                    BinaryOperator::Modulo => self.assert(bitvec::OpCodes::BvSMod, &[l,r]),
                    BinaryOperator::BitwiseOr => self.assert(core::OpCodes::Or, &[l,r]),
                    BinaryOperator::BitwiseAnd => self.assert(core::OpCodes::And, &[l,r]),
                    BinaryOperator::BitwiseXor => self.assert(core::OpCodes::Xor, &[l,r]),
                    BinaryOperator::BitwiseLeftShift => self.assert(bitvec::OpCodes::BvShl, &[l,r]),
                    BinaryOperator::BitwiseRightShift => self.assert(bitvec::OpCodes::BvAShr, &[l,r]),
                    BinaryOperator::LessThan => self.assert(bitvec::OpCodes::BvSLt, &[l,r]),
                    BinaryOperator::LessThanOrEqual => self.assert(bitvec::OpCodes::BvSLe, &[l,r]),
                    BinaryOperator::GreaterThan => self.assert(bitvec::OpCodes::BvSGt, &[l,r]),
                    BinaryOperator::GreaterThanOrEqual => self.assert(bitvec::OpCodes::BvSGe, &[l,r]),
                    BinaryOperator::Equal | BinaryOperator::BiImplication => self.assert(core::OpCodes::Cmp, &[l,r]),
                    BinaryOperator::NotEqual => {
                        let eq = self.assert(core::OpCodes::Cmp, &[l,r]);
                        self.assert(core::OpCodes::Not, &[eq])
                    },
                    BinaryOperator::And => self.assert(core::OpCodes::And, &[l,r]),
                    BinaryOperator::Or => self.assert(core::OpCodes::Or, &[l,r]),
                    BinaryOperator::Xor => return self.assert(core::OpCodes::Xor, &[l,r]),
                    BinaryOperator::Implication => return self.assert(core::OpCodes::Imply, &[l,r])
                }
            },
            Expression::UnaryExpression (ref op, ref e) => {
                let n = self.expr2smtlib(e.as_ref());
                match *op {
                    UnaryOperator::Negation => self.assert(bitvec::OpCodes::BvNeg, &[n]),
                    UnaryOperator::Not => self.assert(core::OpCodes::Not, &[n]),
                }
            },
            Expression::VariableMapping (ref v, ref ty) => {
                if self.contains_mapping(&v) {
                    return self.get_by_name(&v);
                }

                self.new_var(Some(&v), match *ty {
                    Types::Bool => bitvec::Sorts::Bool,
                    Types::Void | Types::Unknown => unimplemented!(),
                    _ => bitvec::Sorts::BitVector(bitvector_size(*ty))
                })
            },
            Expression::BooleanLiteral (ref b) => self.new_const(core::OpCodes::Const(*b)),
            Expression::BitVector (ref value, ref size) => bv_const!(self, *value as u64, bitvector_size(*size))
        }
    }
}

fn bitvector_size(ty: Types) -> usize {
    match ty {
        Types::I8 | Types::U8 => 8,
        Types::I16 | Types::U16 => 16,
        Types::I32 | Types::U32 => 32,
        Types::I64 | Types::U64 => 64,
        _ => unreachable!()
    }
}