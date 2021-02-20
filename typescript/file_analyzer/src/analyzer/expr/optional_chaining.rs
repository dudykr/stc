use super::IdCtx;
use super::TypeOfMode;
use crate::util::RemoveTypes;
use crate::{
    analyzer::{Analyzer, Ctx},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::ROptChainExpr;
use stc_ts_types::Type;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &ROptChainExpr, type_ann: Option<&Type>) -> ValidationResult {
        let span = node.span;

        match &*node.expr {
            RExpr::Member(me) => {
                let prop = self.validate_key(&me.prop, me.computed)?;
                let obj = me.obj.validate_with(self)?;
                let mut obj = obj.remove_falsy();

                if obj.normalize().is_ref_type() {
                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..self.ctx
                    };
                    obj = self.with_ctx(ctx).expand_fully(span, obj, true)?;
                }

                let ty = self.access_property(span, obj, &prop, TypeOfMode::RValue, IdCtx::Var)?;

                //

                Ok(Type::union(vec![Type::undefined(span), ty]))
            }

            RExpr::Call(ce) => {
                let ty = ce.validate_with_args(self, type_ann)?;

                Ok(Type::union(vec![Type::undefined(span), ty]))
            }

            _ => unreachable!("Onvalid optional chaining expression found",),
        }
    }
}
