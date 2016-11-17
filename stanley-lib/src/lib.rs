//! Stanley is a Rust compiler plugin to verify functions annotated with
//! preconditions and postconditions.

#![feature(plugin_registrar, rustc_private)]
#![allow(unused_variables, unused_mut)]

extern crate z3;
extern crate syntax;

#[macro_use]
extern crate rustc;
extern crate rustc_plugin;
extern crate rustc_trans;

mod ast;
mod condition_parser;

use rustc_plugin::Registry;
use rustc::mir::*;
use rustc::mir::transform::{Pass, MirPass, MirSource};
use rustc::ty::TyCtxt;
use syntax::feature_gate::AttributeType;
use syntax::codemap::Spanned;
use syntax::ast::{MetaItemKind, NestedMetaItemKind, Attribute_};
use ast::Expression;

struct StanleyMir;

impl <'tcx> Pass for StanleyMir{}

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
    }
}

fn parse_condition(condition: String) -> Expression {
    match condition_parser::parse_Condition(&*condition) {
        Ok(e) => e,
        Err(e) => panic!("Error parsing condition \"{}\": \"{:?}\"", condition, e)
    }
}

fn parse_attributes(attrs: &[Spanned<Attribute_>]) -> (String, String) {
    let mut pre_string = "".to_string();
    let mut post_string = "".to_string();

    for attr in attrs {
        if let MetaItemKind::List(ref attr_name, ref items) = attr.node.value.node {
            if attr_name != "condition" {
                continue;
            }

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
