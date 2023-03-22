use stc_ts_ast_rnode::{RCallExpr, RCallee, RExpr, RMemberExpr, RMemberProp, ROptCall, ROptChainBase, ROptChainExpr};
use stc_ts_errors::DebugExt;
use stc_ts_types::Type;

use crate::{
    analyzer::{
        expr::{IdCtx, TypeOfMode},
        Analyzer, Ctx,
    },
    util::RemoveTypes,
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &ROptChainExpr, type_ann: Option<&Type>) -> VResult<Type> {
        let span = node.span;

        match &*node.base {
            ROptChainBase::Member(me) => {
                let prop = self.validate_key(
                    &match &me.prop {
                        RMemberProp::Ident(i) => RExpr::Ident(i.clone()),
                        RMemberProp::Computed(c) => *c.expr.clone(),
                        RMemberProp::PrivateName(p) => RExpr::PrivateName(p.clone()),
                    },
                    matches!(me.prop, RMemberProp::Computed(_)),
                )?;
                let obj = me.obj.validate_with_default(self)?;

                let is_obj_optional = self.can_be_undefined(span, &obj, true)?;

                let obj = obj.remove_falsy();

                let ctx = Ctx {
                    in_opt_chain: true,
                    ..self.ctx
                };
                let ty = self
                    .with_ctx(ctx)
                    .access_property(span, &obj, &prop, TypeOfMode::RValue, IdCtx::Var, Default::default())
                    .context("tried to access property to validate an optional chaining expression")?;

                //

                if is_obj_optional {
                    Ok(ty.union_with_undefined(span))
                } else {
                    Ok(ty)
                }
            }

            ROptChainBase::Call(ce) => ce.validate_with_args(self, type_ann),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &ROptCall, type_ann: Option<&Type>) -> VResult<Type> {
        let span = node.span;

        let ty = RCallExpr {
            node_id: node.node_id,
            span,
            callee: RCallee::Expr(node.callee.clone()),
            args: node.args.clone(),
            type_args: node.type_args.clone(),
        }
        .validate_with_args(self, type_ann)?;

        Ok(ty.union_with_undefined(span))
    }
}

pub(crate) fn is_obj_opt_chaining(obj: &RExpr) -> bool {
    match obj {
        RExpr::OptChain(..) => true,
        RExpr::Member(RMemberExpr { obj, .. }) => is_obj_opt_chaining(obj),
        _ => false,
    }
}
