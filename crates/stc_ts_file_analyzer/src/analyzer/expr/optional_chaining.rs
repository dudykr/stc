use stc_ts_ast_rnode::{RExpr, RExprOrSuper, RMemberExpr, ROptChainExpr};
use stc_ts_errors::DebugExt;
use stc_ts_types::Type;
use stc_utils::ext::TypeVecExt;
use swc_ecma_ast::TsKeywordTypeKind;

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
    fn validate(&mut self, node: &ROptChainExpr, type_ann: Option<&Type>) -> VResult {
        let span = node.span;

        match &*node.expr {
            RExpr::Member(me) => {
                let prop = self.validate_key(&me.prop, me.computed)?;
                let obj = me.obj.validate_with(self)?;

                let is_obj_optional = self.is_obj_optional(&obj)?;

                let obj = obj.remove_falsy();

                let ctx = Ctx {
                    in_opt_chain: true,
                    ..self.ctx
                };
                let ty = self
                    .with_ctx(ctx)
                    .access_property(
                        span,
                        &obj,
                        &prop,
                        TypeOfMode::RValue,
                        IdCtx::Var,
                        Default::default(),
                    )
                    .context(
                        "tried to access property to validate an optional chaining expression",
                    )?;

                //

                if is_obj_optional {
                    let mut types = vec![Type::undefined(span, Default::default()), ty];
                    types.dedup_type();
                    Ok(Type::union(types))
                } else {
                    Ok(ty)
                }
            }

            RExpr::Call(ce) => {
                let ty = ce.validate_with_args(self, type_ann)?;

                Ok(Type::union(vec![
                    Type::undefined(span, Default::default()),
                    ty,
                ]))
            }

            _ => unreachable!("Onvalid optional chaining expression found",),
        }
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn is_obj_optional(&mut self, obj: &Type) -> VResult<bool> {
        if obj.is_kwd(TsKeywordTypeKind::TsNullKeyword)
            || obj.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
        {
            return Ok(true);
        }

        match obj.normalize() {
            Type::Union(u) => {
                for ty in u.types.iter() {
                    if self.is_obj_optional(ty)? {
                        return Ok(true);
                    }
                }
                return Ok(false);
            }
            _ => {}
        }

        Ok(false)
    }
}

pub(super) fn is_obj_opt_chaining(obj: &RExpr) -> bool {
    match obj {
        RExpr::OptChain(..) => true,
        RExpr::Member(RMemberExpr {
            obj: RExprOrSuper::Expr(obj),
            ..
        }) => is_obj_opt_chaining(&obj),
        _ => false,
    }
}
