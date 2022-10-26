use std::borrow::Cow;

use stc_ts_ast_rnode::{
    RCallExpr, RExpr, RMemberExpr, RMemberProp, ROptCall, ROptChainBase, ROptChainExpr,
};
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
    fn validate(&mut self, node: &ROptChainExpr, type_ann: Option<&Type>) -> VResult<Type> {
        let span = node.span;

        match &node.base {
            ROptChainBase::Member(me) => {
                let computed = matches!(me.prop, RMemberProp::Computed(_));

                let prop = self.validate_key(
                    &*match &me.prop {
                        RMemberProp::Ident(i) => Cow::Owned(RExpr::Ident(i.clone())),
                        RMemberProp::PrivateName(i) => Cow::Owned(RExpr::PrivateName(i.clone())),
                        RMemberProp::Computed(e) => Cow::Borrowed(&*e.expr),
                    },
                    computed,
                )?;
                let obj = me.obj.validate_with_default(self)?;

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

            ROptChainBase::Call(ce) => ce.validate_with_args(self, type_ann),

            _ => unreachable!("Invalid optional chaining expression found",),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &ROptCall, type_ann: Option<&Type>) -> VResult<Type> {
        let ty = RCallExpr {
            node_id: e.node_id,
            span: e.span,
            callee: stc_ts_ast_rnode::RCallee::Expr(e.callee.clone()),
            args: e.args.clone(),
            type_args: e.type_args.clone(),
        }
        .validate_with_args(self, type_ann)?;

        Ok(Type::union(vec![
            Type::undefined(e.span, Default::default()),
            ty,
        ]))
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
