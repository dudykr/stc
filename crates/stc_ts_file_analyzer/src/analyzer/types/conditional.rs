use std::{borrow::Cow, collections::HashMap};

use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_type_ops::Fix;
use stc_ts_types::{Conditional, Intersection, Type, TypeParam, Union};
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    dev_span,
    ext::TypeVecExt,
};
use swc_common::{Span, SyntaxContext, TypeEq};
use tracing::{debug, info};

use super::NormalizeTypeOpts;
use crate::{
    analyzer::{generic::InferTypeOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(crate) fn normalize_conditional<'a>(
        &mut self,
        actual_span: Span,
        mut c: Conditional,
        opts: NormalizeTypeOpts,
    ) -> VResult<Cow<'a, Type>> {
        let span = if c.span.is_dummy() { actual_span } else { c.span };
        // TODO(kdy1): Cleanup
        c = match self.expand_conditional_type(span, Type::Conditional(c)).foldable() {
            Type::Conditional(c) => c,
            ty => return Ok(Cow::Owned(ty)),
        };

        ALLOW_DEEP_CLONE.set(&(), || {
            let ty = dump_type_as_string(&Type::Conditional(c.clone()));

            debug!("normalize: conditional: {}", ty)
        });

        c.check_type = Box::new(
            self.normalize(
                Some(span),
                Cow::Borrowed(&c.check_type),
                NormalizeTypeOpts {
                    preserve_keyof: true,
                    ..Default::default()
                },
            )
            .context("tried to normalize the `check` type of a conditional type")?
            .freezed()
            .into_owned()
            .freezed(),
        );

        c.extends_type = Box::new(
            self.normalize(Some(span), Cow::Borrowed(&c.extends_type), Default::default())
                .unwrap_or(Cow::Borrowed(&c.extends_type))
                .freezed()
                .into_owned()
                .freezed(),
        );

        if let Some(v) = self.extends(span, &c.check_type, &c.extends_type, Default::default()) {
            info!("normalize: conditional: check_type extends extends_type: {:?}", v);
            let ty = if v { &c.true_type } else { &c.false_type };
            // TODO(kdy1): Optimize
            let ty = self
                .normalize(Some(span), Cow::Borrowed(ty), opts)
                .context("tried to normalize the calculated type of a conditional type")?
                .into_owned();
            return Ok(Cow::Owned(ty));
        }
        if let Type::Param(TypeParam {
            name,
            constraint: Some(check_type_constraint),
            ..
        }) = c.check_type.normalize()
        {
            let new_type = self
                .reduce_conditional_type(
                    c.span,
                    &c.check_type,
                    check_type_constraint,
                    &c.extends_type,
                    &c.true_type,
                    &c.false_type,
                    c.metadata,
                )
                .context("tried to reduce conditional type")?;

            if let Some(new_type) = new_type {
                return self.normalize(Some(span), Cow::Owned(new_type), opts);
            }
        }

        if let Type::Union(check_type_union) = c.check_type.normalize() {
            let mut all = true;
            let mut types = vec![];
            for check_type in &check_type_union.types {
                let res = self.extends(span, check_type, &c.extends_type, Default::default());
                if let Some(v) = res {
                    if v {
                        if !c.true_type.is_never() {
                            types.push(check_type.clone());
                        }
                    } else {
                        if !c.false_type.is_never() {
                            types.push(check_type.clone());
                        }
                    }
                } else {
                    all = false;
                    break;
                }
            }

            if all {
                let new = Type::Union(Union {
                    span: actual_span.with_ctxt(SyntaxContext::empty()),
                    types,
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
                .fixed();

                new.assert_valid();

                return Ok(Cow::Owned(new));
            }
        }

        // TODO: Optimize
        // If we can calculate type using constraints, do so.

        // TODO(kdy1): PERF
        if let Type::Param(TypeParam {
            name,
            constraint: Some(check_type_constraint),
            ..
        }) = c.check_type.normalize_mut()
        {
            // We removes unmatchable constraints.
            // It means, for
            //
            // T: a type param extends string | undefined
            // A: T extends null | undefined ? never : T
            //
            // We removes `undefined` from parents of T.

            if let Type::Union(check_type_union) = check_type_constraint.normalize() {
                let mut all = true;
                let mut types = vec![];
                for check_type in &check_type_union.types {
                    let res = self.extends(c.span, check_type, &c.extends_type, Default::default());
                    if let Some(v) = res {
                        if v {
                            if !c.true_type.is_never() {
                                types.push(check_type.clone());
                            }
                        } else {
                            if !c.false_type.is_never() {
                                types.push(check_type.clone());
                            }
                        }
                    } else {
                        all = false;
                        break;
                    }
                }

                if all {
                    types.dedup_type();
                    let new = Type::Union(Union {
                        span: actual_span.with_ctxt(SyntaxContext::empty()),
                        types,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    });

                    **check_type_constraint = new;

                    let mut params = HashMap::default();
                    params.insert(name.clone(), ALLOW_DEEP_CLONE.set(&(), || *c.check_type.clone().fixed().freezed()));
                    let c = self.expand_type_params(&params, c, Default::default())?;
                    let c = Type::Conditional(c);
                    c.assert_valid();

                    return Ok(Cow::Owned(c));
                }
            }
        }

        Ok(Cow::Owned(Type::Conditional(c)))
    }

    pub(crate) fn has_type_param_for_conditional(c: &Type) -> bool {
        match c.normalize() {
            Type::Param(..) => true,
            Type::Intersection(Intersection { types, .. }) => types.iter().any(Self::has_type_param_for_conditional),
            Type::Conditional(Conditional { check_type, .. }) => Self::has_type_param_for_conditional(check_type),
            _ => false,
        }
    }

    pub(crate) fn overwrite_conditional(&self, span: Span, c: &Conditional) -> Type {
        if Self::has_type_param_for_conditional(&c.check_type) {
            if c.check_type.type_eq(&c.true_type) {
                Type::new_intersection(span, [*(c.check_type).clone(), *(c.extends_type).clone()]).freezed()
            } else {
                let mut params = HashMap::default();
                if let Some(ty) = c.check_type.as_type_param() {
                    params.insert(
                        ty.name.clone(),
                        Type::new_intersection(span, [*(c.check_type).clone(), *(c.extends_type).clone()]).freezed(),
                    );
                }
                let true_type = *c.true_type.clone();
                let res = self.expand_type_params(&params, true_type, Default::default());
                if let Ok(ty) = res {
                    ty.freezed()
                } else {
                    *c.true_type.clone()
                }
            }
        } else {
            *c.true_type.clone()
        }
    }

    pub(crate) fn expand_conditional_type(&self, span: Span, ty: Type) -> Type {
        let _tracing = dev_span!("expand_conditional_type");

        if !ty.is_conditional() {
            return ty;
        }

        let ty = ty.foldable();
        if let Type::Conditional(Conditional {
            mut check_type,
            mut extends_type,
            mut true_type,
            mut false_type,
            metadata,
            ..
        }) = ty
        {
            extends_type.freeze();
            check_type.freeze();

            // We need to handle infer type.
            let type_params = self
                .infer_ts_infer_types(
                    span,
                    &extends_type,
                    &check_type,
                    InferTypeOpts {
                        exclude_null_and_undefined: true,
                        ..Default::default()
                    },
                )
                .ok();

            if let Some(type_params) = type_params {
                check_type = Box::new(self.expand_type_params(&type_params, *check_type, Default::default()).unwrap());
                extends_type = Box::new(self.expand_type_params(&type_params, *extends_type, Default::default()).unwrap());

                true_type = Box::new(self.expand_type_params(&type_params, *true_type, Default::default()).unwrap());
                false_type = Box::new(self.expand_type_params(&type_params, *false_type, Default::default()).unwrap());
            }

            if check_type.is_class() {
                if let Type::Class(check_type) = check_type.normalize_mut() {
                    if let Type::Constructor(..) = extends_type.normalize() {
                        return *true_type;
                    }
                }
            }

            return Type::Conditional(Conditional {
                span,
                check_type,
                extends_type,
                true_type,
                false_type,
                metadata,
                tracker: Default::default(),
            });
        }

        ty
    }
}
