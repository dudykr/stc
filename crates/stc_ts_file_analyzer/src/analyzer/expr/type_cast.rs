use std::borrow::Cow;

use stc_ts_ast_rnode::{RTsAsExpr, RTsLit, RTsTypeAssertion};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_types::{Interface, KeywordType, LitType, TypeElement, TypeParamInstantiation};
use stc_utils::{cache::Freeze, dev_span};
use swc_common::{Span, Spanned, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;

use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::TypeOfMode,
        scope::ExpandOpts,
        util::{make_instance_type, ResultExt},
        Analyzer, NormalizeTypeOpts,
    },
    ty::Type,
    util::is_str_or_union,
    validator,
    validator::ValidateWith,
    VResult,
};

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct CastableOpts {
    /// `true` if we are checking for `A extends B` relation.
    pub disallow_different_classes: bool,

    pub allow_assignment_to_param_constraint: bool,

    pub disallow_special_assignment_to_empty_class: bool,
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RTsTypeAssertion,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        // We don't apply type annotation because it can corrupt type checking.
        let mut casted_ty = e.type_ann.validate_with(self)?;
        casted_ty.freeze();
        let mut orig_ty = e.expr.validate_with_args(self, (mode, type_args, Some(&casted_ty)))?;
        orig_ty.freeze();

        self.validate_type_cast(e.span, orig_ty, casted_ty)
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
    ) -> VResult<Type> {
        if e.node_id.is_invalid() {
            return e.type_ann.validate_with(self);
        }

        // We don't apply type annotation because it can corrupt type checking.
        let casted_ty = e.type_ann.validate_with(self)?;
        let orig_ty = e.expr.validate_with_args(self, (mode, type_args, Some(&casted_ty)))?;

        self.validate_type_cast(e.span, orig_ty, casted_ty)
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
    fn validate_type_cast(&mut self, span: Span, orig_ty: Type, casted_ty: Type) -> VResult<Type> {
        let mut orig_ty = self.expand(
            span,
            orig_ty,
            ExpandOpts {
                full: true,
                expand_union: true,
                ..Default::default()
            },
        )?;
        orig_ty.freeze();

        let mut casted_ty = make_instance_type(casted_ty);
        self.prevent_inference_while_simplifying(&mut casted_ty);
        casted_ty = self.simplify(casted_ty);

        self.prevent_expansion(&mut casted_ty);
        casted_ty.freeze();

        self.validate_type_cast_inner(span, &orig_ty, &casted_ty).report(&mut self.storage);
        Ok(casted_ty)
    }

    fn validate_type_cast_inner(&mut self, span: Span, orig: &Type, casted: &Type) -> VResult<()> {
        // I don't know why this is valid, but `stringLiteralsWithTypeAssertions01.ts`
        // has some tests for this.
        if is_str_or_union(orig) && casted.is_str() {
            return Ok(());
        }

        if let Type::Union(ref rt) = orig.normalize() {
            let castable = rt.types.iter().any(|v| casted.type_eq(v));

            if castable {
                return Ok(());
            }
        }

        match casted.normalize() {
            Type::Tuple(ref lt) => {
                //
                if let Type::Tuple(ref rt) = orig.normalize() {
                    //
                    if lt.elems.len() != rt.elems.len() {
                        Err(ErrorKind::InvalidTupleCast {
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

                        let res = self.validate_type_cast_inner(span, &right_element.ty, &left_element.ty);

                        if res.is_err() {
                            all_castable = false;
                            break;
                        }
                    }

                    if all_castable {
                        return Ok(());
                    }
                }
            }

            Type::Array(ref lt) => {
                //
                if let Type::Tuple(ref rt) = orig.normalize() {
                    if !rt.elems.is_empty() && rt.elems[0].ty.type_eq(&lt.elem_type) {
                        return Ok(());
                    }
                }
            }

            // fallback to .assign
            _ => {}
        }

        // self.assign(&casted_ty, &orig_ty, span)?;

        // interface P {}
        // interface C extends P {}
        //
        // declare var c:C
        // declare var p:P
        //
        // console.log(c as C)
        // console.log(c as P)
        // console.log(p as C)
        // console.log(p as P)
        //
        // We can cast P to C
        if let Some(true) = self.extends(span, orig, casted, Default::default()) {
            return Ok(());
        }
        if let Some(true) = self.extends(span, casted, orig, Default::default()) {
            return Ok(());
        }

        self.castable(span, orig, casted, Default::default())
            .and_then(|castable| {
                if castable {
                    Ok(())
                } else {
                    Err(ErrorKind::NonOverlappingTypeCast {
                        span,
                        from: box orig.clone(),
                        to: box casted.clone(),
                    }
                    .into())
                }
            })
            .convert_err(|err| ErrorKind::NonOverlappingTypeCast {
                span,
                from: box orig.clone(),
                to: box casted.clone(),
            })
    }

    pub(crate) fn has_overlap(&mut self, span: Span, l: &Type, r: &Type, opts: CastableOpts) -> VResult<bool> {
        let _tracing = dev_span!("has_overlap");

        let l = l.normalize();
        let r = r.normalize();

        if l.type_eq(r) {
            return Ok(true);
        }

        Ok(self.castable(span, l, r, opts)? || self.castable(span, r, l, opts)?)
    }

    /// # Parameters
    ///
    /// - `l`: from
    /// - `r`: to
    pub(crate) fn castable(&mut self, span: Span, from: &Type, to: &Type, opts: CastableOpts) -> VResult<bool> {
        let _tracing = dev_span!("castable");

        let from = self
            .normalize(
                Some(span),
                Cow::Borrowed(from),
                NormalizeTypeOpts {
                    preserve_intersection: true,
                    preserve_union: true,
                    preserve_global_this: true,
                    ..Default::default()
                },
            )?
            .freezed();
        let to = self
            .normalize(
                Some(span),
                Cow::Borrowed(to),
                NormalizeTypeOpts {
                    preserve_intersection: true,
                    preserve_union: true,
                    preserve_global_this: true,
                    ..Default::default()
                },
            )?
            .freezed();

        // If type is conditional, infer union
        let from = if from.is_conditional() {
            let value = self.get_conditional_type_means(span, from.normalize());
            Cow::Owned(Type::new_union(span, value.freezed()))
        } else {
            from
        };
        let from = from.normalize();

        let to = if to.is_conditional() {
            let value = self.get_conditional_type_means(span, to.normalize());
            Cow::Owned(Type::new_union(span, value.freezed()))
        } else {
            to
        };
        let to = to.normalize();

        if from.type_eq(to) {
            return Ok(true);
        }

        // Overlaps with all types.
        if from.is_any() || from.is_kwd(TsKeywordTypeKind::TsNullKeyword) || from.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
            return Ok(true);
        }

        if from.is_kwd(TsKeywordTypeKind::TsStringKeyword) && to.is_tpl() {
            return Ok(true);
        }

        if let Type::TypeLit(to) = to {
            if to.members.is_empty() {
                return Ok(true);
            }
        }

        if from.type_eq(to) {
            return Ok(true);
        }

        match (from, to) {
            (
                Type::Lit(LitType {
                    lit: RTsLit::Number(..), ..
                }),
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }),
            ) => return Ok(true),
            (
                Type::Lit(LitType { lit: RTsLit::Str(..), .. }),
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                }),
            ) => return Ok(true),
            (
                Type::Lit(LitType { lit: RTsLit::Bool(..), .. }),
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }),
            ) => return Ok(true),
            (
                Type::Lit(LitType {
                    lit: RTsLit::BigInt(..), ..
                }),
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                }),
            ) => return Ok(true),
            (
                Type::Lit(LitType {
                    lit: RTsLit::Number(..), ..
                }),
                Type::Lit(LitType {
                    lit: RTsLit::Number(..), ..
                }),
            )
            | (Type::Lit(LitType { lit: RTsLit::Str(..), .. }), Type::Lit(LitType { lit: RTsLit::Str(..), .. }))
            | (
                Type::Lit(LitType {
                    lit: RTsLit::BigInt(..), ..
                }),
                Type::Lit(LitType {
                    lit: RTsLit::BigInt(..), ..
                }),
            ) => return Ok(false),

            (Type::Function(..), Type::Interface(Interface { name, .. })) if name == "Function" => return Ok(true),
            _ => {}
        }

        // TODO(kdy1): More check
        if from.is_fn_type() && to.is_fn_type() {
            return Ok(false);
        }

        if let (Type::TypeLit(lt), Type::TypeLit(rt)) = (from, to) {
            // It's an error if type of the parameter of index signature is same but type
            // annotation is different.
            for lm in &lt.members {
                for rm in &rt.members {
                    if let (TypeElement::Index(lm), TypeElement::Index(rm)) = (lm, rm) {
                        if lm.params.type_eq(&rm.params) {
                            if let Some(lt) = &lm.type_ann {
                                if let Some(rt) = &rm.type_ann {
                                    if self.assign(span, &mut Default::default(), lt, rt).is_err()
                                        && self.assign(span, &mut Default::default(), rt, lt).is_err()
                                    {
                                        return Ok(false);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // TODO(kdy1): This is wrong
        if from.is_type_lit() && to.is_type_lit() {
            return Ok(true);
        }

        if let Type::Union(l) = from {
            for l in &l.types {
                if self.castable(span, l, to, opts)? {
                    return Ok(true);
                }
            }

            return Ok(false);
        }

        match to {
            Type::Union(to) => {
                for to in &to.types {
                    if self.castable(span, from, to, opts)? {
                        return Ok(true);
                    }
                }

                return Ok(false);
            }

            Type::Intersection(to) => {
                for to in &to.types {
                    if self.castable(span, from, to, opts)? {
                        return Ok(true);
                    }
                }

                return Ok(false);
            }
            _ => {}
        }

        if from.is_num() {
            if self.can_be_casted_to_number_in_rhs(span, to) {
                return Ok(true);
            }
        }

        if from.is_class() && (to.is_interface() || to.is_type_lit()) {
            return Ok(false);
        }

        match to.normalize() {
            Type::Tpl(to) => {
                if let Type::Tpl(from) = from.normalize() {
                    return Ok(!self.tpl_lit_type_definitely_unrelated(span, from, to)?);
                }
            }

            Type::Conditional(to) => {
                return Ok(self.castable(span, from, &to.true_type, opts)? || self.castable(span, from, &to.false_type, opts)?);
            }
            _ => (),
        }

        // class A {}
        // class B extends A {}
        //
        // We can cast A to B, thus from = A, to = B.
        if let Ok(()) = self.assign_with_opts(
            &mut Default::default(),
            from,
            to,
            AssignOpts {
                span,
                disallow_different_classes: opts.disallow_different_classes,
                allow_assignment_to_param_constraint: opts.allow_assignment_to_param_constraint,
                disallow_special_assignment_to_empty_class: opts.disallow_special_assignment_to_empty_class,
                allow_unknown_rhs: Some(true),
                for_castablity: true,
                ..Default::default()
            },
        ) {
            return Ok(true);
        }

        Ok(false)
    }
}
