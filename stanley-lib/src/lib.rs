//! Stanley is a Rust compiler plugin to verify functions annotated with
//! preconditions and postconditions.

#![feature(plugin_registrar, rustc_private)]

extern crate z3;
extern crate syntax;

extern crate rustc;
extern crate rustc_plugin;
extern crate rustc_trans;
extern crate rustc_data_structures;
extern crate rustc_const_math;

use z3::*;
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
/*    arg_data: Vec<&'tcx LocalDecl<'tcx>>,
    var_data: Vec<&'tcx LocalDecl<'tcx>>,
    temp_data: Vec<&'tcx LocalDecl<'tcx>>,*/
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
     /*       arg_data: Vec::new(),
            var_data: Vec::new(),
            temp_data: Vec::new(),*/
            mir: mir,
            func_return_type: mir.return_ty
        };

        for block in mir.basic_blocks() {
            data.block_data.push(block);
        }

        for arg_data in mir.args_iter() {
            data.arg_data.push(&mir.local_decls[arg_data]);
        }

        for var_data in mir.vars_iter() {
            data.var_data.push(&mir.local_decls[var_data]);
        }

        for temp_data in mir.temps_iter() {
            data.temp_data.push(&mir.local_decls[temp_data]);
        }

        //println!("{:#?}", pre_string_expression);
        //println!("{:#?}", post_string_expression);

        pre_string_expression = walk_and_replace(pre_string_expression, &data);
        post_string_expression = walk_and_replace(post_string_expression, &data);

        //println!("{:#?}", pre_string_expression);
        //println!("{:#?}", post_string_expression);

        ast::ty_check(&pre_string_expression).unwrap_or_else(|e| error!("{}", e));
        ast::ty_check(&post_string_expression).unwrap_or_else(|e| error!("{}", e));

        let weakest_precondition = gen(0, &data, &post_string_expression);

        // Create the verification condition, P -> WP
        let verification_condition = Expression::BinaryExpression(
            Box::new(pre_string_expression.clone()),
            ast::BinaryOperator::Implication,
            Box::new(weakest_precondition.clone())
        );
        
        println!("{:?}", verification_condition);

        // Check that the verification condition is correctly typed
        ast::ty_check(&verification_condition).unwrap_or_else(|e| error!("{}", e));

        println!("\n\nVerification Condition: {:?}\n", verification_condition);

        /*let cfg = Config::new();
        let ctx = Context::new(&cfg);

        let x = ctx.named_int_const("x");
        let y = ctx.named_int_const("y");
        let zero = ctx.from_i64(0);
        let two = ctx.from_i64(2);
        let seven = ctx.from_i64(7);

        let solver = Solver::new(&ctx);
        solver.assert(&x.gt(&y));
        solver.assert(&y.gt(&zero));
        solver.assert(&y.rem(&seven)._eq(&two));
        solver.assert(&x.add(&[&two]).gt(&seven));
        assert!(solver.check());

        let model = solver.get_model();
        let xv = model.eval(&x).unwrap().as_i64().unwrap();
        let yv = model.eval(&y).unwrap().as_i64().unwrap();
        println!("x: {}, y: {}", xv, yv);*/

        println!("\n------------------------------------\n");
    }
}

fn gen(index: usize, data: &MirData, post_expression: &Expression) -> Expression {
    let mut wp: Expression = Expression::VariableMapping("!!!!".to_string(), Types::Void);

    println!("Visiting {}", index);
    println!("Terminator: {:?}", data.block_data[index].terminator);

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

    /*let mut stmts = data.block_data[index].statements.clone();
    stmts.reverse();

    for stmt in stmts {
        wp = gen_stmt(wp, stmt, data);
    }*/

    wp
}

fn gen_lvalue(lvalue: Lvalue, data: &MirData) -> Expression {
    match lvalue {
        Lvalue::Local(index) => {
            match data.mir.local_kind(index) {
                LocalKind::Arg => Expression::VariableMapping(data.arg_data[index.index()].name.unwrap().as_str().to_string(), ast::string_to_type(data.arg_data[index.index()].ty.clone().to_string())),
                LocalKind::Temp => {
                    let ty = data.mir.local_decls[index].ty.clone().to_string();
                    if let TypeVariants::TyTuple(_, t) = data.temp_data[index.index()].ty.sty {
                        t.to_string();
                    }
                    Expression::VariableMapping("tmp".to_string() + index.index().to_string().as_str(), ast::string_to_type(ty))
                },
                LocalKind::Var => Expression::VariableMapping("var".to_string() + index.index().to_string().as_str(), ast::string_to_type(data.var_data[index.index()].ty.clone().to_string())),
                LocalKind::ReturnPointer => Expression::VariableMapping("return".to_string(), ast::type_to_enum(data.func_return_type.clone())),
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
                    let index = variable.index();

                    match data.mir.local_kind(variable) {
                        LocalKind::Arg => {
                            lvalue_name = data.arg_data[index].name.unwrap().as_str().to_string();
                            lvalue_type_string = data.arg_data[index].ty.clone().to_string();
                        },
                        LocalKind::Temp => {
                            lvalue_name = "tmp".to_string() + index.to_string().as_str();

                            match data.temp_data[index].ty.sty {
                                TypeVariants::TyTuple(_, t) => lvalue_type_string = t.to_string(),
                                _ => unimplemented!(),
                            }
                        },
                        LocalKind::Var => {
                            let i = index2.parse::<usize>().unwrap();
                            lvalue_name = "var".to_string() + index.to_string().as_str();

                            match data.var_data[index].ty.sty {
                                TypeVariants::TyTuple(_, t) => lvalue_type_string = t.to_string(),
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
            };

            expression.push(Expression::BinaryExpression(Box::new(lvalue), op, Box::new(rvalue)));
        },
        Rvalue::UnaryOp(ref unop, ref val) => {
            let exp = gen_expression(val, data);

            let op = match *unop {
                UnOp::Not => {
                    if ast::determine_evaluation_type(&exp) == Types::Bool {
                        UnaryOperator::Not
                    } else {
                        UnaryOperator::BitwiseNot
                    }
                },
                UnOp::Neg => UnaryOperator::Negation,
            };

            expression.push(Expression::UnaryExpression(op, Box::new(exp)));
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


    // Replace any appearance of var in the weakest precondition with the expression
    for expr in &expression {
        println!("\n\n\t{:?}\nRep\t{:?} to\t{:?}", wp, var, expr);
        wp = substitute_variable_with_expression(&wp, &var, expr);
        println!("\t{:?}", wp);
    }

    wp
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
    let type_string = match operand.clone() {
        Operand::Constant(ref constant) => constant.ty.to_string(),
        Operand::Consume(ref lvalue) => {
            match *lvalue {
                Lvalue::Local(ref variable) => {
                    match data.mir.local_kind(*variable) {
                        LocalKind::Arg => data.arg_data[variable.index()].ty.to_string(),
                        LocalKind::Temp => data.temp_data[variable.index()].ty.to_string(),
                        LocalKind::Var => data.var_data[variable.index()].ty.to_string(),
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
    for arg in data.arg_data.iter() {
        let a = arg.name.unwrap().as_str();
        let arg_name = String::from_utf8_lossy(a.as_bytes());

        if name == arg_name {
            return ast::type_to_enum(arg.ty)
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
