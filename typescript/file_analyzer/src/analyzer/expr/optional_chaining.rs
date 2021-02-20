use super::IdCtx;
use super::TypeOfMode;
use crate::util::RemoveTypes;
use crate::{analyzer::Analyzer, validator, validator::ValidateWith, ValidationResult};
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::ROptChainExpr;
use stc_ts_types::Type;
use swc_common::TypeEq;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &ROptChainExpr, type_ann: Option<&Type>) -> ValidationResult {
        let span = node.span;

        match &*node.expr {
            RExpr::Member(me) => {
                let prop = self.validate_key(&me.prop, me.computed)?;
                let obj = me.obj.validate_with(self)?;

                // TODO: Optimize
                let orig = obj.clone();
                let obj = obj.remove_falsy();

                let is_obj_optional = !orig.normalize().type_eq(&obj.normalize());

                let ty = self.access_property(span, obj, &prop, TypeOfMode::RValue, IdCtx::Var)?;

                //

                if is_obj_optional {
                    Ok(Type::union(vec![Type::undefined(span), ty]))
                } else {
                    Ok(ty)
                }
            }

            RExpr::Call(ce) => {
                let ty = ce.validate_with_args(self, type_ann)?;

                Ok(Type::union(vec![Type::undefined(span), ty]))
            }

            _ => unreachable!("Onvalid optional chaining expression found",),
        }
    }
}
