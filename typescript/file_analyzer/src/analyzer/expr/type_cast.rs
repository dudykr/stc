use super::{super::Analyzer, TypeOfMode};
use crate::analyzer::util::ResultExt;
use crate::{
    analyzer::util::instantiate_class, ty::Type, validator, validator::ValidateWith,
    ValidationResult,
};
use stc_ts_ast_rnode::RTsAsExpr;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeAssertion;
use stc_ts_errors::Error;
use stc_ts_types::TypeParamInstantiation;
use swc_common::TypeEq;
use swc_common::{Span, Spanned};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RTsTypeAssertion,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        // We don't apply type annotation because it can corrupt type checking.
        let orig_ty = e
            .expr
            .validate_with_args(self, (mode, type_args, type_ann))?;

        self.validate_type_cast(e.span, orig_ty, &e.type_ann)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RTsAsExpr,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        // We don't apply type annotation because it can corrupt type checking.
        let orig_ty = e
            .expr
            .validate_with_args(self, (mode, type_args, type_ann))?;

        self.validate_type_cast(e.span, orig_ty, &e.type_ann)
    }
}

impl Analyzer<'_, '_> {
    /// ```ts
    /// var unionTuple3: [number, string | number] = [10, "foo"];
    /// var unionTuple4 = <[number, number]>unionTuple3;
    /// ```
    ///
    /// is valid, while
    ///
    /// ```ts
    /// var unionTuple3: [number, string | number] = [10, "foo"];
    /// var unionTuple4: [number, number] = unionTuple3;
    /// ```
    ///
    /// results in error.
    fn validate_type_cast(
        &mut self,
        span: Span,
        orig_ty: Box<Type>,
        to: &RTsType,
    ) -> ValidationResult {
        let orig_ty = self.expand_fully(span, orig_ty, true)?;

        let casted_ty = to.validate_with(self)?;
        let mut casted_ty = instantiate_class(self.ctx.module_id, casted_ty);
        self.prevent_inference_while_simplifying(&mut casted_ty);
        casted_ty = self.simplify(casted_ty);

        self.prevent_expansion(&mut casted_ty);

        self.validate_type_cast_inner(span, &orig_ty, &casted_ty)
            .report(&mut self.storage);

        Ok(casted_ty)
    }

    fn validate_type_cast_inner(
        &mut self,
        span: Span,
        orig: &Type,
        casted: &Type,
    ) -> ValidationResult<()> {
        match orig {
            Type::Union(ref rt) => {
                let castable = rt.types.iter().any(|v| casted.type_eq(v));

                if castable {
                    return Ok(());
                }
            }

            _ => {}
        }

        match casted {
            Type::Tuple(ref lt) => {
                //
                match *orig.normalize() {
                    Type::Tuple(ref rt) => {
                        //
                        if lt.elems.len() != rt.elems.len() {
                            Err(Error::InvalidTupleCast {
                                span,
                                left: lt.span(),
                                right: rt.span(),
                            })?;
                        }

                        let mut all_castable = true;
                        //
                        for (i, left_element) in lt.elems.iter().enumerate() {
                            // if rt.types.len() >= i {
                            //     all_castable = false;
                            //     break;
                            // }
                            let right_element = &rt.elems[i];

                            let res = self.validate_type_cast_inner(
                                span,
                                &right_element.ty,
                                &left_element.ty,
                            );

                            if res.is_err() {
                                all_castable = false;
                                break;
                            }
                        }

                        if all_castable {
                            return Ok(());
                        }
                    }

                    _ => {}
                }
            }

            Type::Array(ref lt) => {
                //
                match orig {
                    Type::Tuple(ref rt) => {
                        if rt.elems[0].ty.type_eq(&lt.elem_type) {
                            return Ok(());
                        }
                    }

                    // fallback to .assign
                    _ => {}
                }
            }

            // fallback to .assign
            _ => {}
        }

        // self.assign(&casted_ty, &orig_ty, span)?;

        match casted {
            Type::Tuple(ref rt) => {
                //
                match orig {
                    Type::Tuple(ref lt) => {}
                    _ => {}
                }
            }
            _ => {}
        }

        if self.has_overlap(&orig, &casted)? {
            return Ok(());
        }

        Err(Error::NonOverlappingTypeCast { span })
    }

    pub(crate) fn has_overlap(&mut self, l: &Type, r: &Type) -> ValidationResult<bool> {
        let l = l.normalize();
        let r = r.normalize();

        if l.type_eq(r) {
            return Ok(true);
        }

        Ok(false)
    }
}
