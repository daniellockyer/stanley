//! Stanley is a Rust compiler plugin to verify functions annotated with
//! preconditions and postconditions.

#![feature(plugin_registrar, rustc_private)]

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

macro_rules! gen_name {
    ($start:expr, $index:expr, $depth:expr) =>
        ($start.to_string() + $index.index().to_string().as_str() + $depth.to_string().as_str())
}

#[macro_use] extern crate rustproof_libsmt;
extern crate petgraph;
extern crate regex;
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

mod smt;
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

        pre_string_expression = walk_and_replace(pre_string_expression, &data);
        post_string_expression = walk_and_replace(post_string_expression, &data);

        ast::ty_check(&pre_string_expression).unwrap_or_else(|e| error!("{}", e));
        ast::ty_check(&post_string_expression).unwrap_or_else(|e| error!("{}", e));

        let weakest_precondition = gen(0, 0, &data, &post_string_expression);

        let verification_condition = Expression::BinaryExpression(
            Box::new(weakest_precondition.clone()),
            ast::BinaryOperator::Implication,
            Box::new(pre_string_expression.clone()),
        );

        ast::ty_check(&verification_condition).unwrap_or_else(|e| error!("{}", e));

        smt::run_solver(&verification_condition, &name);
    }
}

    let mut wp: Expression = Expression::VariableMapping("!!!!".to_string(), Types::Void);
fn gen(index: usize, depth: usize, data: &MirData, post_expression: &Expression) -> Expression {

    if depth >= 10 {
        return Expression::BooleanLiteral(true);
    }

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
        TerminatorKind::SwitchInt{discr, targets, ..} => {
            let wp_if = gen(targets[1].index(), depth+1, data, post_expression);
            let wp_else = gen(targets[0].index(), depth+1, data, post_expression);

            let condition = match discr {
                Operand::Constant (ref constant) => match constant.literal {
                    Literal::Value {ref value} => match *value {
                        ConstVal::Bool (ref boolean) => Expression::BooleanLiteral(*boolean),
                        _ => unimplemented!()
                    },
                    _ => unimplemented!()
                },
                Operand::Consume(c) => gen_lvalue(c, data, depth)
            };

            let not_condition = Expression::UnaryExpression(UnaryOperator::Not, Box::new(condition.clone()));

            wp = Expression::BinaryExpression(
                Box::new(Expression::BinaryExpression(Box::new(condition.clone()), BinaryOperator::Implication, Box::new(wp_if))),
                ast::BinaryOperator::And,
                Box::new(Expression::BinaryExpression(Box::new(not_condition.clone()), BinaryOperator::Implication, Box::new(wp_else)))
            );
        },
        TerminatorKind::DropAndReplace{..} | TerminatorKind::Drop{..} | TerminatorKind::Unreachable |
            TerminatorKind::Resume => unimplemented!(),
    }

    let mut stmts = data.block_data[index].statements.clone();
    stmts.reverse();

    for stmt in stmts {
        wp = gen_stmt(wp, stmt, data, depth);
    }

    wp
}

fn gen_lvalue(lvalue: Lvalue, data: &MirData, depth: usize) -> Expression {
    match lvalue {
        Lvalue::Local(index) => match data.mir.local_kind(index) {
            LocalKind::Arg => Expression::VariableMapping(data.mir.local_decls[index].name.unwrap().as_str().to_string(), ast::string_to_type(data.mir.local_decls[index].ty.to_string())),
            LocalKind::Temp => {
                let mut ty = data.mir.local_decls[index].ty.to_string();

                if let TypeVariants::TyTuple(s, _) = data.mir.local_decls[index].ty.sty {
                    if s.len() > 0 {
                        ty = s[0].to_string();
                    }
                }
                Expression::VariableMapping(gen_name!("tmp", index, depth), ast::string_to_type(ty))
            },
            LocalKind::Var => Expression::VariableMapping(gen_name!("var", index, depth), ast::string_to_type(data.mir.local_decls[index].ty.to_string())),
            LocalKind::ReturnPointer => Expression::VariableMapping("ret".to_string(), ast::type_to_enum(data.func_return_type)),
        },
        Lvalue::Projection(pro) => {
            let lvalue_name;
            let lvalue_type_string;

            match pro.as_ref().base {
                Lvalue::Local(variable) => match data.mir.local_kind(variable) {
                    LocalKind::Arg => {
                        lvalue_name = data.mir.local_decls[variable].name.unwrap().as_str().to_string();
                        lvalue_type_string = data.mir.local_decls[variable].ty.to_string();
                    },
                    LocalKind::Temp => {
                        lvalue_name = gen_name!("tmp", variable, depth);

                        match data.mir.local_decls[variable].ty.sty {
                            TypeVariants::TyTuple(s, _) => lvalue_type_string = s[0].to_string(),
                            _ => unimplemented!()
                        }
                    },
                    LocalKind::Var => {
                        lvalue_name = gen_name!("var", variable, depth);

                        let i = match pro.as_ref().elem.clone() {
                            ProjectionElem::Field(ref field, _) => (field.index() as i64).to_string(),
                            _ => unimplemented!()
                        }.parse::<usize>().unwrap();

                        match data.mir.local_decls[variable].ty.sty {
                            TypeVariants::TyTuple(s, _) => lvalue_type_string = s[i].to_string(),
                            _ => unimplemented!()
                        }
                    },
                    LocalKind::ReturnPointer => unimplemented!()
                },
                _ => unimplemented!()
            };

            Expression::VariableMapping(lvalue_name, ast::string_to_type(lvalue_type_string))
        },
        Lvalue::Static(_) => unimplemented!()
    }
}

fn gen_stmt(wp: Expression, stmt: Statement, data: &MirData, depth: usize) -> Expression {
    let lvalue: Lvalue;
    let rvalue: Rvalue;

    match stmt.kind {
        StatementKind::Assign(ref lval, ref rval) => {
            lvalue = lval.clone();
            rvalue = rval.clone();
        },
        _ => return wp
    }

    let var = gen_lvalue(lvalue, data, depth);
    let mut expression = Expression::VariableMapping("!!!!".to_string(), Types::Void);

    match rvalue {
        Rvalue::CheckedBinaryOp(ref binop, ref lval, ref rval) | Rvalue::BinaryOp(ref binop, ref lval, ref rval) => {
            let lvalue2 = gen_expression(lval, data, depth);
            let rvalue2 = gen_expression(rval, data, depth);

            let op: BinaryOperator = match *binop {
                BinOp::Add => {
                    //wp = smt::overflow_check(&wp, &var, binop, &lvalue, &rvalue);
                    BinaryOperator::Addition
                },
            expression = Expression::BinaryExpression(Box::new(lvalue2.clone()), (match *binop {
                BinOp::Add => BinaryOperator::Addition,
                BinOp::Sub => BinaryOperator::Subtraction,
                BinOp::Mul => BinaryOperator::Multiplication,
                BinOp::Div => BinaryOperator::Division,
                BinOp::Rem => BinaryOperator::Modulo,
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
            }), Box::new(rvalue2));
        },
        Rvalue::UnaryOp(ref unop, ref val) => {
            expression = Expression::UnaryExpression(match *unop {
                UnOp::Not => UnaryOperator::Not,
                UnOp::Neg => UnaryOperator::Negation,
            }, Box::new(gen_expression(val, data, depth)));
        },
        Rvalue::Aggregate(ref ag_kind, ref vec_operand) => match *ag_kind {
            AggregateKind::Tuple => {
                for operand in vec_operand.iter() {
                    expression = Expression::VariableMapping(format!("{:?}", operand), ast::string_to_type(match operand.clone() {
                        Operand::Constant(ref constant) => constant.ty.to_string(),
                        Operand::Consume(ref lvalue) => match *lvalue {
                            Lvalue::Local(ref variable) => match data.mir.local_kind(*variable) {
                                LocalKind::Arg | LocalKind::Temp | LocalKind::Var => data.mir.local_decls[*variable].ty.to_string(),
                                _ => unimplemented!()
                            },
                            Lvalue::Static(_) | Lvalue::Projection(_) => unimplemented!()
                        }
                    }));
                }
            },
            _ => error!("Unsupported aggregate: only tuples are supported")
        },
        Rvalue::Use(ref operand) => { expression = gen_expression(operand, data, depth); },
        Rvalue::Cast(..) | Rvalue::Ref(..) => { expression = var.clone(); },
        Rvalue::Box(..) | Rvalue::Len(..) | Rvalue::Repeat(..) | Rvalue::Discriminant(..) => unimplemented!()
    };

    substitute_variable_with_expression(&wp, &var, &expression)
}

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
    ast::string_to_type(match operand.clone() {
        Operand::Constant(ref constant) => constant.ty.to_string(),
        Operand::Consume(ref lvalue) => match *lvalue {
            Lvalue::Local(ref variable) => match data.mir.local_kind(*variable) {
                LocalKind::Arg | LocalKind::Temp | LocalKind::Var => data.mir.local_decls[*variable].ty.to_string(),
                _ => unimplemented!()
            },
            Lvalue::Static(_) | Lvalue::Projection(_) => unimplemented!()
        }
    })
}

fn walk_and_replace(expression: Expression, data: &MirData) -> Expression {
    match expression {
        Expression::VariableMapping(a, b) => {
            let aa = a.clone();
            let mut bb = b;

            if bb == Types::Unknown {
                if aa == "ret" {
                    bb = ast::type_to_enum(data.func_return_type);
                } else {
                    for arg in data.mir.args_iter() {
                        let arg2 = &data.mir.local_decls[arg];
                        let a2 = arg2.name.unwrap().as_str();

                        if a == String::from_utf8_lossy(a2.as_bytes()) {
                            bb = ast::type_to_enum(arg2.ty);
                            break;
                        }
                    }
                }
            }

            Expression::VariableMapping(aa, bb)
        },
        Expression::BinaryExpression(a, b, c) => {
            let aa = Box::new(walk_and_replace(*a.clone(), data));
            let ca = Box::new(walk_and_replace(*c.clone(), data));
            Expression::BinaryExpression(aa, b, ca)
        },
        Expression::UnaryExpression(a, b) => {
            let ba = Box::new(walk_and_replace(*b.clone(), data));
            Expression::UnaryExpression(a, ba)
        },
        _ => expression.clone()
    }
}

fn gen_expression(operand: &Operand, data: &MirData, depth: usize) -> Expression {
    match *operand {
        Operand::Consume (ref l) => gen_lvalue(l.clone(), data, depth),
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
            _ => unimplemented!()
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
    reg.register_attribute("condition".to_string(), AttributeType::Whitelisted);
    reg.register_mir_pass(Box::new(StanleyMir {}));
}
