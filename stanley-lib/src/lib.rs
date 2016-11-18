//! Stanley is a Rust compiler plugin to verify functions annotated with
//! preconditions and postconditions.

#![feature(plugin_registrar, rustc_private)]

extern crate z3;
extern crate syntax;

#[macro_use]
extern crate rustc;
extern crate rustc_plugin;
extern crate rustc_trans;
extern crate rustc_data_structures;

mod ast;
mod condition_parser;

use rustc_plugin::Registry;
use rustc::mir::transform::{Pass, MirPass, MirSource};
use rustc::mir::*;
use rustc::ty::{TyCtxt, Ty};
use syntax::feature_gate::AttributeType;
use syntax::codemap::Spanned;
use syntax::ast::{MetaItemKind, NestedMetaItemKind, Attribute_};
use rustc_data_structures::indexed_vec::Idx;
use rustc::middle::const_val::ConstVal;
use ast::{Expression, BinaryOperator, UnaryOperator};

struct StanleyMir;

pub struct MirData<'tcx> {
    block_data: Vec<&'tcx BasicBlockData<'tcx>>,
    arg_data: Vec<&'tcx LocalDecl<'tcx>>,
    var_data: Vec<&'tcx LocalDecl<'tcx>>,
    temp_data: Vec<&'tcx LocalDecl<'tcx>>,
    func_return_type: Ty<'tcx>,
}

impl <'tcx> Pass for StanleyMir {}

impl <'tcx> MirPass<'tcx> for StanleyMir {
    fn run_pass<'a>(&mut self, tcx: TyCtxt<'a, 'tcx, 'tcx>, src: MirSource, mir: &mut Mir<'tcx>) {
        let item_id = src.item_id();
        let def_id = tcx.map.local_def_id(item_id);
        let name = tcx.item_path_str(def_id);
        let attrs = tcx.map.attrs(item_id);

        let (pre_string, post_string) = parse_attributes(attrs);

        if pre_string == "" || post_string == "" {
            return;
        }

        println!("{:?}\t{:?}\t{}\t{}", mir.return_ty, name, pre_string, post_string);

        let pre_string_expression = parse_condition(pre_string);
        let post_string_expression = parse_condition(post_string);
        
        /*println!("{:?}", pre_string_expression);
        println!("{:?}", post_string_expression);*/

        let mut data = MirData {
            block_data: Vec::new(),
            arg_data: Vec::new(),
            var_data: Vec::new(),
            temp_data: Vec::new(),
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

        let weakest_precondition = gen(0, &mut data, &post_string_expression);

        println!("{:?}", weakest_precondition);

        let verification_condition: Expression = Expression::BinaryExpression(
            Box::new(pre_string_expression),
            BinaryOperator::Implication,
            Box::new(weakest_precondition));

        /*match expression::ty_check(&verification_condition) {
            Ok(_) => {},
            Err(e) => panic!("{}", e),
        }*/

        //gen_smtlib(&verification_condition, name, debug);

        println!("\n\n\n\n");
    }
}

fn gen(index: usize, data: &mut MirData, post_expr: &Expression) -> Expression {
    let mut wp: Expression = Expression::BooleanLiteral(true);
    let terminator = data.block_data[index].terminator.clone().unwrap().kind;

    match terminator {
        TerminatorKind::Assert{target, ..} |
        TerminatorKind::Goto{target} => {
            wp = gen(target.index(), data, post_expr)
        },
        TerminatorKind::Return => {
            return (*post_expr).clone();
        },
        TerminatorKind::Call{func, ..} => {
            match func {
                Operand::Constant (ref c) => {
                    let s = format!("{:?}", c.literal);
                    if s.contains("begin_panic") {
                        return Expression::BooleanLiteral(false);
                    }
                },
                Operand::Consume (..) => unimplemented!(),
            };
            unreachable!();
        },
        TerminatorKind::If{cond, targets} => {
            let wp_if = gen(targets.0.index(), data, post_expr);
            let wp_else = gen(targets.1.index(), data, post_expr);

            let condition = match cond {
                Operand::Constant (ref constant) => {
                    match constant.literal {
                        Literal::Value {ref value} => {
                            match *value {
                                ConstVal::Bool (ref boolean) => {
                                    Expression::BooleanLiteral(*boolean)
                                },
                                _ => unreachable!(),
                            }
                        },
                        _ => unimplemented!(),
                    }
                },
                _ => { Expression::BooleanLiteral(false) } //TODO: Fix this
                //Operand::Consume(c) => { gen_lvalue(c, data) },
            };
            // Negate the conditional expression
            let not_condition = Expression::UnaryExpression(UnaryOperator::Not, Box::new(condition.clone()));

            // wp(If c x else y) => (c -> x) AND ((NOT c) -> y)
            wp = Expression::BinaryExpression(
                Box::new(Expression::BinaryExpression(
                    Box::new(condition.clone()),
                    BinaryOperator::Implication,
                    Box::new(wp_if)
                )),
                BinaryOperator::And,
                Box::new(Expression::BinaryExpression(
                    Box::new(not_condition.clone()),
                    BinaryOperator::Implication,
                    Box::new(wp_else)
                ))
            );
        },
        TerminatorKind::DropAndReplace{..} => unimplemented!(),
        TerminatorKind::Drop{..} => unimplemented!(),
        TerminatorKind::Unreachable => unimplemented!(),
        TerminatorKind::Resume => unimplemented!(),
        TerminatorKind::Switch{..} => unimplemented!(),
        TerminatorKind::SwitchInt{..} => unimplemented!()
    }

    let mut stmts = data.block_data[index].statements.clone();
    stmts.reverse();

    for stmt in stmts {
        wp = gen_stmt(wp, stmt, data);
    }

    wp
}

fn gen_stmt(mut wp: Expression, stmt: Statement, data: &mut MirData) -> Expression {
    if debug {
        println!("processing statement\t{:?}\ninto expression\t\t{:?}", stmt, wp);
    }

    let lvalue: Option<Lvalue>;
    let rvalue: Option<Rvalue>;

    // Store the values of the statement
    match stmt.kind {
        StatementKind::Assign(ref lval, ref rval) => {
            lvalue = Some(lval.clone());
            rvalue = Some(rval.clone());
        },
        //_ => return Some(wp)
    }
    // The variable or temp on the left-hand side of the assignment
    let mut var = gen_lvalue(lvalue.unwrap(), data);

    // The expression on the right-hand side of the assignment
    let mut expression = Vec::new();
    match rvalue.clone().unwrap() {
        Rvalue::CheckedBinaryOp(ref binop, ref loperand, ref roperand) => {
            let lvalue: Expression = gen_expression(loperand, data);
            let rvalue: Expression = gen_expression(roperand, data);
            let op: BinaryOperator = match *binop {
                BinOp::Add => BinaryOperator::Addition,
                BinOp::Sub => BinaryOperator::Subtraction,
                BinOp::Mul => BinaryOperator::Multiplication,
                BinOp::Div => BinaryOperator::Division,
                BinOp::Rem => BinaryOperator::Modulo,
                BinOp::Shl => BinaryOperator::BitwiseLeftShift,
                BinOp::Shr => BinaryOperator::BitwiseRightShift,
                _ => panic!("Unsupported checked binary operation!"),
            };

            var.name = var.name + ".0";

            // Add the new BinaryExpressionData to the expression vector
            expression.push(Expression::BinaryExpression(Box::new(lvalue), op, Box::new(rvalue)));
        },

        Rvalue::BinaryOp(ref binop, ref lval, ref rval) => {
            let lvalue: Expression = gen_expression(lval, data);
            let rvalue: Expression = gen_expression(rval, data);
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
            // Add the expression to the vector
            expression.push(Expression::BinaryExpression(Box::new(lvalue), op, Box::new(rvalue)));
        },
        // Generates Rvalue to a UnaryOp
        Rvalue::UnaryOp(ref unop, ref val) => {
            let exp: Expression = gen_expression(val, data);
            let op: UnaryOperator = match *unop {
                UnOp::Not => {
                    if determine_evaluation_type(&exp) == Types::Bool {
                        UnaryOperator::Not
                    } else {
                        UnaryOperator::BitwiseNot
                    }
                },
                UnOp::Neg => UnaryOperator::Negation,
            };
            // push the ne new exp onto the expression: Vec<>
            expression.push(Expression::UnaryExpression(op, Box::new(exp)));
        },
        Rvalue::Use(ref operand) => {
            expression.push(gen_expression(operand, data));
        },
        Rvalue::Aggregate(ref ag_kind, ref vec_operand) => {
            match *ag_kind {
                AggregateKind::Tuple => {
                    for operand in vec_operand.iter() {
                        expression.push(Expression::VariableMapping(format!("{:?}", operand), gen_ty(operand, data)));
                    }
                },
                _ => panic!("Unsupported aggregate: only tuples are supported"),
            }
        },
        Rvalue::Cast(..) => { expression.push(var.clone()); },
        Rvalue::Ref(..) => { expression.push(var.clone()); },
        Rvalue::Box(..) => unimplemented!(),
        Rvalue::Len(..) => unimplemented!(),
        _ => unimplemented!(),
    };

    // Replace any appearance of var in the weakest precondition with the expression
    for expr in &expression {
        substitute_variable_with_expression(&mut wp, &var, expr);
    }
    
    wp
}

fn parse_condition(condition: String) -> Expression {
    match condition_parser::parse_Condition(&*condition) {
        Ok(e) => e,
        Err(e) => {
            println!("{:?}", e);
            panic!("Error parsing condition \"{}\"", condition)
        }
    }
}

fn parse_attributes(attrs: &[Spanned<Attribute_>]) -> (String, String) {
    let mut pre_string = "".to_string();
    let mut post_string = "".to_string();

    for attr in attrs {
        if let MetaItemKind::List(_, ref items) = attr.node.value.node {
            for item in items {
                if let NestedMetaItemKind::MetaItem(ref i_string) = item.node {
                    if let MetaItemKind::NameValue(ref attr_param_name, ref literal) = i_string.node {
                        if let syntax::ast::LitKind::Str(ref attr_param_value, _) = literal.node {

                            match attr_param_name.to_string().as_ref() {
                                "pre" => pre_string = attr_param_value.to_string(),
                                "post" => post_string = attr_param_value.to_string(),
                                _ => panic!("I only accept `pre` and `post`. You gave me \"{}\"", attr_param_name)
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