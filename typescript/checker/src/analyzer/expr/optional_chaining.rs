use super::TypeOfMode;
use crate::{
    analyzer::{Analyzer, Ctx},
    ty::TypeExt,
    util::RemoveTypes,
    validator,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use stc_ast_rnode::RExpr;
use stc_ast_rnode::ROptChainExpr;
use stc_types::Type;
use swc_ecma_ast::*;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut ROptChainExpr, type_ann: Option<&Type>) -> ValidationResult {
        let span = node.span;

        match &mut *node.expr {
            RExpr::Member(me) => {
                let obj = me.obj.validate_with(self)?;
                let mut obj = box obj.remove_falsy();

                if obj.is_ref_type() {
                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..self.ctx
                    };
                    obj = self.with_ctx(ctx).expand_fully(span, obj, true)?;
                }

                let ty =
                    self.access_property(span, obj, &mut me.prop, me.computed, TypeOfMode::RValue)?;

                //

                if self.rule().strict_null_checks {
                    Ok(Type::union(vec![Type::undefined(span), ty]))
                } else {
                    Ok(ty)
                }
            }

            RExpr::Call(ce) => {
                let ty = ce.validate_with_args(self, type_ann)?;

                if self.rule().strict_null_checks {
                    Ok(Type::union(vec![Type::undefined(span), ty]))
                } else {
                    Ok(ty)
                }
            }

            _ => unreachable!("Onvalid optional chaining expression found",),
        }
    }
}
