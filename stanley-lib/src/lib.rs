//! Stanley is a Rust compiler plugin to verify functions annotated with
//! preconditions and postconditions.

#![feature(plugin_registrar, rustc_private)]

#[macro_use] extern crate rustproof_libsmt;
extern crate petgraph;
extern crate syntax;

extern crate rustc;
extern crate rustc_plugin;
extern crate rustc_trans;
extern crate rustc_data_structures;
extern crate rustc_const_math;

use rustc_plugin::Registry;
use rustc::mir::transform::{Pass, MirPass, MirSource};
use rustc::mir::*;
use rustc::ty::{TyCtxt, Ty, TypeVariants};
use rustc::middle::const_val::ConstVal;
use rustc_const_math::ConstInt;
use syntax::feature_gate::AttributeType;
use syntax::ast::{MetaItemKind, NestedMetaItemKind, Attribute};
use ast::{Expression, BinaryOperator, UnaryOperator, Types};
use rustc_data_structures::indexed_vec::Idx;

use rustproof_libsmt::backends::smtlib2::*;
use rustproof_libsmt::backends::backend::*;
use rustproof_libsmt::backends::z3;
use rustproof_libsmt::theories::{bitvec, core};
use rustproof_libsmt::logics::qf_abv::*;
use petgraph::graph::NodeIndex;

use std::fmt::Debug;

#[macro_export]
macro_rules! error {
    ($($args:tt)*) => {{
        use std::io::Write;
        let stderr = ::std::io::stderr();
        let mut stderr = stderr.lock();
        write!(stderr, "\n[!] Error:\n").unwrap();
        writeln!(stderr, $($args)*).unwrap();
        write!(stderr, "\n\n").unwrap();
        ::std::process::exit(1)
    }}
}

mod ast;
mod condition_parser;

struct StanleyMir;

pub struct MirData<'tcx> {
    block_data: Vec<&'tcx BasicBlockData<'tcx>>,
    mir: &'tcx Mir<'tcx>,
    func_return_type: Ty<'tcx>,
}

impl <'tcx> Pass for StanleyMir {}

impl <'tcx> MirPass<'tcx> for StanleyMir {
    fn run_pass<'a>(&mut self, tcx: TyCtxt<'a, 'tcx, 'tcx>, src: MirSource, mir: &mut Mir<'tcx>) {
        let item_id = src.item_id();

        let def_id = tcx.hir.local_def_id(item_id);
        let name = tcx.item_path_str(def_id);
        let attrs = tcx.hir.attrs(item_id);

        let (pre_string, post_string) = parse_attributes(attrs);

        if pre_string == "" || post_string == "" {
            return;
        }

        let mut pre_string_expression = parse_condition(pre_string);
        let mut post_string_expression = parse_condition(post_string);

        let mut data = MirData {
            block_data: Vec::new(),
            mir: mir,
            func_return_type: mir.return_ty
        };

        for block in mir.basic_blocks() {
            data.block_data.push(block);
        }

        //println!("{:#?}", pre_string_expression);
        //println!("{:#?}", post_string_expression);

        pre_string_expression = walk_and_replace(pre_string_expression, &data);
        post_string_expression = walk_and_replace(post_string_expression, &data);

        //println!("{:#?}", pre_string_expression);
        //println!("{:#?}", post_string_expression);

        ast::ty_check(&pre_string_expression).unwrap_or_else(|e| error!("{}", e));
        ast::ty_check(&post_string_expression).unwrap_or_else(|e| error!("{}", e));

        print!("----- {} -- ", name);

        let weakest_precondition = gen(0, &data, &post_string_expression);

        // Create the verification condition, P -> WP
        let verification_condition = Expression::BinaryExpression(
            Box::new(pre_string_expression.clone()),
            ast::BinaryOperator::Implication,
            Box::new(weakest_precondition.clone())
        );

        // Check that the verification condition is correctly typed
        ast::ty_check(&verification_condition).unwrap_or_else(|e| error!("{}", e));

        let mut z3: z3::Z3 = Default::default();
        let mut solver = SMTLib2::new(Some(QF_ABV));
        let vcon = solver.expr2smtlib(&verification_condition);
        let _ = solver.assert(core::OpCodes::Not, &[vcon]);
        let (_, check) = solver.solve(&mut z3, false);

        match check {
            SMTRes::Sat(_, ref model) => println!("Verification Condition is not valid.\n\n{}", model.clone().unwrap()),
            SMTRes::Unsat(..) => println!("Verification Condition is valid."),
            SMTRes::Error(ref error, _) => println!("Error in Verification Condition Generation.\n{}\n", error)
        }
    }
}

pub trait Pred2SMT {
    type Idx: Debug + Clone;
    type Logic: Logic;

    fn expr2smtlib (&mut self, &Expression) -> Self::Idx;
}

impl Pred2SMT for SMTLib2<QF_ABV> {
    type Idx = NodeIndex;
    type Logic = QF_ABV;

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
                let sort = match *ty {
                    Types::Bool => bitvec::Sorts::Bool,
                    Types::I8 | Types::U8 => bitvec::Sorts::BitVector(8),
                    Types::I16 | Types::U16 => bitvec::Sorts::BitVector(16),
                    Types::I32 | Types::U32 => bitvec::Sorts::BitVector(32),
                    Types::I64 | Types::U64 => bitvec::Sorts::BitVector(64),
                    Types::Void | Types::Unknown => unreachable!(),
                };
                return self.new_var(Some(&v), sort);
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

fn gen(index: usize, data: &MirData, post_expression: &Expression) -> Expression {
    let mut wp: Expression = Expression::VariableMapping("!!!!".to_string(), Types::Void);

    match data.block_data[index].terminator.clone().unwrap().kind {
        TerminatorKind::Assert{target, ..} | TerminatorKind::Goto{target} => { wp = gen(target.index(), data, post_expression); },
        TerminatorKind::Return => { return post_expression.clone(); },
        TerminatorKind::Call{func, ..} => match func {
            Operand::Constant (ref c) => {
                if format!("{:?}", c.literal).contains("begin_panic") {
                    return Expression::BooleanLiteral(false);
                }
            },
            Operand::Consume (..) => unimplemented!()
        },
        TerminatorKind::If{cond, targets} => {
            let wp_if = gen(targets.0.index(), data, post_expression);
            let wp_else = gen(targets.1.index(), data, post_expression);

            let condition = match cond {
                Operand::Constant (ref constant) => match constant.literal {
                    Literal::Value {ref value} => match *value {
                        ConstVal::Bool (ref boolean) => Expression::BooleanLiteral(*boolean),
                        _ => unreachable!()
                    },
                    _ => unimplemented!()
                },
                Operand::Consume(c) => gen_lvalue(c, data)
            };

            let not_condition = Expression::UnaryExpression(UnaryOperator::Not, Box::new(condition.clone()));

            wp = Expression::BinaryExpression(
                Box::new(Expression::BinaryExpression(Box::new(condition.clone()), BinaryOperator::Implication, Box::new(wp_if))),
                ast::BinaryOperator::And,
                Box::new(Expression::BinaryExpression(Box::new(not_condition.clone()), BinaryOperator::Implication, Box::new(wp_else)))
            );
        },
        TerminatorKind::DropAndReplace{..} | TerminatorKind::Drop{..} | TerminatorKind::Unreachable |
            TerminatorKind::Resume | TerminatorKind::Switch{..} | TerminatorKind::SwitchInt{..} => unimplemented!(),
    }

    let mut stmts = data.block_data[index].statements.clone();
    stmts.reverse();

    for stmt in stmts {
        wp = gen_stmt(wp, stmt, data);
    }

    wp
}

fn gen_lvalue(lvalue: Lvalue, data: &MirData) -> Expression {
    match lvalue {
        Lvalue::Local(index) => {
            match data.mir.local_kind(index) {
                LocalKind::Arg => Expression::VariableMapping(data.mir.local_decls[index].name.unwrap().as_str().to_string(), ast::string_to_type(data.mir.local_decls[index].ty.clone().to_string())),
                LocalKind::Temp => {
                    let mut ty = data.mir.local_decls[index].ty.clone().to_string();
                    
                    if let TypeVariants::TyTuple(s, _) = data.mir.local_decls[index].ty.sty {
                        if s.len() > 0 {
                            ty = s[0].to_string();
                        }
                    }
                    Expression::VariableMapping("tmp".to_string() + index.index().to_string().as_str(), ast::string_to_type(ty))
                },
                LocalKind::Var => Expression::VariableMapping("var".to_string() + index.index().to_string().as_str(), ast::string_to_type(data.mir.local_decls[index].ty.clone().to_string())),
                LocalKind::ReturnPointer => Expression::VariableMapping("ret".to_string(), ast::type_to_enum(data.func_return_type.clone())),
            }
        },
        Lvalue::Projection(pro) => {
            let index2: String = match pro.as_ref().elem.clone() {
                ProjectionElem::Field(ref field, _) => (field.index() as i32).to_string(),
                ProjectionElem::Index(_) | _ => unimplemented!(),
            };

            let lvalue_name;
            let lvalue_type_string;

            match pro.as_ref().base {
                Lvalue::Local(variable) => {
                    match data.mir.local_kind(variable) {
                        LocalKind::Arg => {
                            lvalue_name = data.mir.local_decls[variable].name.unwrap().as_str().to_string();
                            lvalue_type_string = data.mir.local_decls[variable].ty.clone().to_string();
                        },
                        LocalKind::Temp => {
                            lvalue_name = "tmp".to_string() + variable.index().to_string().as_str();

                            match data.mir.local_decls[variable].ty.sty {
                                TypeVariants::TyTuple(s, _) => lvalue_type_string = s[0].to_string(),
                                _ => unimplemented!(),
                            }
                        },
                        LocalKind::Var => {
                            let i = index2.parse::<usize>().unwrap();
                            lvalue_name = "var".to_string() + variable.index().to_string().as_str();

                            match data.mir.local_decls[variable].ty.sty {
                                TypeVariants::TyTuple(s, _) => lvalue_type_string = s[i].to_string(),
                                _ => unimplemented!(),
                            }
                        },
                        LocalKind::ReturnPointer => unimplemented!()
                    }
                },
                Lvalue::Static(_) | Lvalue::Projection(_) => unimplemented!()
            };

            let index3: String = match pro.as_ref().elem.clone() {
                ProjectionElem::Field(ref field, _) => (field.index() as i32).to_string(),
                ProjectionElem::Index(_) | _ => unimplemented!(),
            };

            Expression::VariableMapping(lvalue_name + "." + index3.as_str(), ast::string_to_type(lvalue_type_string))
        },
        Lvalue::Static(_) => unimplemented!()
    }
}

fn gen_stmt(mut wp: Expression, stmt: Statement, data: &MirData) -> Expression {
    let lvalue: Lvalue;
    let rvalue: Rvalue;

    match stmt.kind {
        StatementKind::Assign(ref lval, ref rval) => {
            lvalue = lval.clone();
            rvalue = rval.clone();
        },
        _ => return wp
    }

    let var = gen_lvalue(lvalue, data);
    let mut expression = Vec::new();

    match rvalue.clone() {
        Rvalue::CheckedBinaryOp(ref binop, ref lval, ref rval) | Rvalue::BinaryOp(ref binop, ref lval, ref rval) => {
            let lvalue = gen_expression(lval, data);
            let rvalue = gen_expression(rval, data);

            let op: BinaryOperator = match *binop {
                BinOp::Add => {
                    //wp = overflow_check(&wp, &var, binop, &lvalue, &rvalue);
                    BinaryOperator::Addition
                },
                BinOp::Sub => {
                    //wp = overflow_check(&wp, &var, binop, &lvalue, &rvalue);
                    BinaryOperator::Subtraction
                },
                BinOp::Mul => {
                    //wp = overflow_check(&wp, &var, binop, &lvalue, &rvalue);
                    BinaryOperator::Multiplication
                },
                BinOp::Div => {
                    //wp = overflow_check(&wp, &var, binop, &lvalue, &rvalue);
                    BinaryOperator::Division
                },
                BinOp::Rem => {
                    //wp = overflow_check(&wp, &var, binop, &lvalue, &rvalue);
                    BinaryOperator::Modulo
                },
                BinOp::BitOr => BinaryOperator::BitwiseOr,
                BinOp::BitAnd => BinaryOperator::BitwiseAnd,
                BinOp::BitXor => BinaryOperator::BitwiseXor,
                BinOp::Shl => BinaryOperator::BitwiseLeftShift,
                BinOp::Shr => BinaryOperator::BitwiseRightShift,
                BinOp::Lt => BinaryOperator::LessThan,
                BinOp::Le => BinaryOperator::LessThanOrEqual,
                BinOp::Gt => BinaryOperator::GreaterThan,
                BinOp::Ge => BinaryOperator::GreaterThanOrEqual,
                BinOp::Eq => BinaryOperator::Equal,
                BinOp::Ne => BinaryOperator::NotEqual,
            };

            expression.push(Expression::BinaryExpression(Box::new(lvalue), op, Box::new(rvalue)));
        },
        Rvalue::UnaryOp(ref unop, ref val) => {
            let op = match *unop {
                UnOp::Not => UnaryOperator::Not,
                UnOp::Neg => UnaryOperator::Negation,
            };

            expression.push(Expression::UnaryExpression(op, Box::new(gen_expression(val, data))));
        },
        Rvalue::Use(ref operand) => { expression.push(gen_expression(operand, data)); },
        Rvalue::Aggregate(ref ag_kind, ref vec_operand) => match *ag_kind {
            AggregateKind::Tuple => {
                for operand in vec_operand.iter() {
                    expression.push(Expression::VariableMapping(format!("{:?}", operand), gen_ty(operand, data)));
                }
            },
            _ => error!("Unsupported aggregate: only tuples are supported")
        },
        Rvalue::Cast(..) | Rvalue::Ref(..) => { expression.push(var.clone()); },
        Rvalue::Box(..) | Rvalue::Len(..) | _ => unimplemented!()
    };

    for expr in &expression {
        wp = substitute_variable_with_expression(&wp, &var, expr);
    }

    wp
}
/*
fn overflow_check(wp: &Expression, var: &Expression, binop: &BinOp, lvalue: &Expression, rvalue: &Expression) -> Expression {
    match var {
        Expression::VariableMapping(name, ty) => Expression::BinaryExpression(Box::new(wp.clone()), BinaryOperator::And, Box::new(
            match var.var_type {
                Types::I8 => signed_overflow(binop, 8u8, lvalue, rvalue),
                Types::I16 => signed_overflow(binop, 16u8, lvalue, rvalue),
                Types::I32 => signed_overflow(binop, 32u8, lvalue, rvalue),
                Types::I64 => signed_overflow(binop, 64u8, lvalue, rvalue),
                Types::U8 | Types::U16 | Types::U32 | Types::U64 => {
                    unsigned_overflow(binop, lvalue, rvalue)
                },
                _ => panic!("Unsupported return type of binary operation: {}", var.var_type),
            }
        )),
        _ => unimplemented!()
    }
}

fn signed_overflow(binop: &BinOp, size: u8, lvalue: &Expression, rvalue: &Expression) -> Expression {
    match *binop {
        BinOp::Add => signed_add(size, lvalue, rvalue),
        BinOp::Mul => signed_mul(lvalue, rvalue),
        BinOp::Sub => signed_sub(size, lvalue, rvalue),
        BinOp::Div => signed_div(size, lvalue, rvalue),
        BinOp::Rem => signed_div(size, lvalue, rvalue),
        BinOp::Shl => unimplemented!(),
        BinOp::Shr => unimplemented!(),
        BinOp::BitOr => unimplemented!(),
        BinOp::BitAnd => unimplemented!(),
        BinOp::BitXor => unimplemented!(),
        BinOp::Lt => unimplemented!(),
        BinOp::Le => unimplemented!(),
        BinOp::Gt => unimplemented!(),
        BinOp::Ge => unimplemented!(),
        BinOp::Eq => unimplemented!(),
        BinOp::Ne => unimplemented!(),
    }
}*/

fn substitute_variable_with_expression(source_expression: &Expression, target: &Expression, replacement: &Expression) -> Expression {
    match *source_expression {
        Expression::BinaryExpression(ref left, ref op, ref right) => {
            let new_left = Box::new(substitute_variable_with_expression(left, target, replacement));
            let new_right = Box::new(substitute_variable_with_expression(right, target, replacement));
            Expression::BinaryExpression(new_left, *op, new_right)
        },
        Expression::UnaryExpression(ref op, ref expr) => {
            Expression::UnaryExpression((*op).clone(), Box::new(substitute_variable_with_expression(expr, target, replacement)))
        },
        Expression::VariableMapping(_, _) if source_expression == target => replacement.clone(),
        _ => (*source_expression).clone()
    }
}

fn gen_ty(operand: &Operand, data: &MirData) -> Types {
    let type_string = match operand.clone() {
        Operand::Constant(ref constant) => constant.ty.to_string(),
        Operand::Consume(ref lvalue) => {
            match *lvalue {
                Lvalue::Local(ref variable) => {
                    match data.mir.local_kind(*variable) {
                        LocalKind::Arg | LocalKind::Temp | LocalKind::Var => data.mir.local_decls[*variable].ty.to_string(),
                        _ => unimplemented!()
                    }
                },
                Lvalue::Static(_) | Lvalue::Projection(_) => unimplemented!()
            }
        }
    };

    ast::string_to_type(type_string)
}

fn get_argument_type(name: String, data: &MirData) -> Types {
    for arg in data.mir.args_iter() {
        let ref arg2 = data.mir.local_decls[arg];

        let a = arg2.name.unwrap().as_str();
        let arg_name = String::from_utf8_lossy(a.as_bytes());

        if name == arg_name {
            return ast::type_to_enum(arg2.ty)
        }
    }
    Types::Unknown
}

fn walk_and_replace(expression: Expression, data: &MirData) -> Expression {
    match expression {
        Expression::VariableMapping(a, b) => {
            let aa = a.clone();
            let mut bb = b.clone();

            if bb == Types::Unknown {
                if aa == "ret" {
                    bb = ast::type_to_enum(data.func_return_type);
                } else {
                    bb = get_argument_type(a, data);
                }
            }

            Expression::VariableMapping(aa, bb)
        },
        Expression::BinaryExpression(a, b, c) => {
            let aa = walk_and_replace(*a.clone(), data);
            let ba = b.clone();
            let ca = walk_and_replace(*c.clone(), data);
            Expression::BinaryExpression(Box::new(aa), ba, Box::new(ca))
        },
        Expression::UnaryExpression(a, b) => {
            let aa = a.clone();
            let ba = walk_and_replace(*b.clone(), data);
            Expression::UnaryExpression(aa, Box::new(ba))
        },
        _ => expression.clone()
    }
}

fn gen_expression(operand: &Operand, data: &MirData) -> Expression {
    match *operand {
        Operand::Consume (ref l) => gen_lvalue(l.clone(), data),
        Operand::Constant (ref c) => match c.literal {
            Literal::Value {ref value} => match *value {
                ConstVal::Bool(ref const_bool) => Expression::BooleanLiteral(*const_bool),
                ConstVal::Integral(ref const_int) => match *const_int {
                    ConstInt::I8(i) => Expression::BitVector(i as i64, Types::I8),
                    ConstInt::I16(i) => Expression::BitVector(i as i64, Types::I16),
                    ConstInt::I32(i) => Expression::BitVector(i as i64, Types::I32),
                    ConstInt::I64(i) => Expression::BitVector(i as i64, Types::I64),
                    ConstInt::U8(i) => Expression::BitVector(i as i64, Types::U8),
                    ConstInt::U16(i) => Expression::BitVector(i as i64, Types::U16),
                    ConstInt::U32(i) => Expression::BitVector(i as i64, Types::U32),
                    ConstInt::U64(i) => Expression::BitVector(i as i64, Types::U64),
                    _ => unimplemented!()
                },
                _ => unimplemented!()
            },
            Literal::Item {..} => unimplemented!(),
            Literal::Promoted {..} => unimplemented!(),
        }
    }
}

fn parse_condition(condition: String) -> Expression {
    condition_parser::parse_Condition(&*condition)
        .unwrap_or_else(|e| error!("Error parsing condition \"{}\" with error \"{:?}\"", condition, e))
}

fn parse_attributes(attrs: &[Attribute]) -> (String, String) {
    let mut pre_string = "".to_string();
    let mut post_string = "".to_string();

    for attr in attrs {
        if let MetaItemKind::List(ref items) = (*attr).value.node {
            for item in items {
                if let NestedMetaItemKind::MetaItem(ref i_string) = item.node {
                    if let MetaItemKind::NameValue(ref literal) = i_string.node {
                        if let syntax::ast::LitKind::Str(ref attr_param_value, _) = literal.node {
                            match i_string.name.to_string().as_ref() {
                                "pre" => pre_string = attr_param_value.to_string(),
                                "post" => post_string = attr_param_value.to_string(),
                                _ => error!("I only accept `pre` and `post`. You gave me \"{}\"", i_string.name)
                            }
                        }
                    }
                }
            }
        }
    }

    (pre_string, post_string)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
	let stanleymir = StanleyMir {};
    reg.register_attribute("condition".to_string(), AttributeType::Whitelisted);
    reg.register_mir_pass(Box::new(stanleymir));
}
