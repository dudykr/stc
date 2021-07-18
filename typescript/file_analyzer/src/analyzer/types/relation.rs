use std::borrow::Cow;

use crate::{analyzer::Analyzer, ValidationResult};
use fxhash::FxHashSet;
use stc_ts_types::{ternary::TscTernary, Operator, Type, TypeParam};
use swc_atoms::JsWord;
use swc_common::{Span, TypeEq};
use swc_ecma_ast::TsTypeOperatorOp;
use tracing::instrument;

#[derive(Debug, Clone, Copy)]
pub enum RelationCheckMode {
    Identity,
    Subtype,
    Comparable,
    Assignability,
}

impl Default for RelationCheckMode {
    fn default() -> Self {
        Self::Identity
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TypeRelatedToOpts {
    pub mode: RelationCheckMode,
    pub report_errors: bool,
    pub intesection_state: TscIntersectionState,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TscIntersectionState {
    pub property_check: bool,
}

impl Analyzer<'_, '_> {
    /// Ported from `structuredTypeRelatedTo` of `tsc`.
    #[instrument(skip(self, span, source, target, opts))]
    pub(crate) fn structured_type_related_to(
        &mut self,
        span: Span,
        source: &Type,
        target: &Type,
        opts: TypeRelatedToOpts,
    ) -> ValidationResult<TscTernary> {
        if opts.intesection_state.property_check {
            return self.properties_related_to(
                source,
                target,
                Default::default(),
                TypeRelatedToOpts {
                    intesection_state: Default::default(),
                    ..opts
                },
            );
        }

        match opts.mode {
            RelationCheckMode::Identity => {
                match (source.normalize(), target.normalize()) {
                    (
                        Type::Operator(Operator {
                            op: TsTypeOperatorOp::KeyOf,
                            ty: soruce,
                            ..
                        }),
                        Type::Operator(Operator {
                            op: TsTypeOperatorOp::KeyOf,
                            ty: target,
                            ..
                        }),
                    ) => {
                        return self.is_related_to(
                            span,
                            source,
                            target,
                            TypeRelatedToOpts {
                                report_errors: false,
                                ..opts
                            },
                        )
                    }

                    _ => {}
                }
                let mut result = TscTernary::False;
                match (source.normalize(), target.normalize()) {
                    (Type::IndexedAccessType(source), Type::IndexedAccessType(target)) => {
                        result = self.is_related_to(
                            span,
                            &source.obj_type,
                            &target.obj_type,
                            TypeRelatedToOpts {
                                report_errors: false,
                                ..opts
                            },
                        )?;
                        if result.as_bool() {
                            result &= self.is_related_to(
                                span,
                                &source.obj_type,
                                &target.obj_type,
                                TypeRelatedToOpts {
                                    report_errors: false,
                                    ..opts
                                },
                            )?;
                            if result.as_bool() {
                                return Ok(result);
                            }
                        }
                    }

                    _ => {}
                }

                match (source.normalize(), target.normalize()) {
                    (Type::Conditional(source), Type::Conditional(target)) => {
                        result = self.is_related_to(
                            span,
                            &source.check_type,
                            &target.check_type,
                            TypeRelatedToOpts {
                                report_errors: false,
                                ..opts
                            }?,
                        );
                        if result.as_bool() {
                            result &= self.is_related_to(
                                span,
                                &source.extends_type,
                                &target.extends_type,
                                TypeRelatedToOpts {
                                    report_errors: false,
                                    ..opts
                                },
                            )?;

                            if result.as_bool() {
                                result &= self.is_related_to(
                                    span,
                                    &source.true_type,
                                    &target.true_type,
                                    TypeRelatedToOpts {
                                        report_errors: false,
                                        ..opts
                                    },
                                )?;

                                if result.as_bool() {
                                    result &= self.is_related_to(
                                        span,
                                        &source.false_type,
                                        &target.false_type,
                                        TypeRelatedToOpts {
                                            report_errors: false,
                                            ..opts
                                        },
                                    )?;

                                    if result.as_bool() {
                                        return Ok(result);
                                    }
                                }
                            }
                        }
                    }

                    _ => {}
                }

                // if (flags & TypeFlags.Substitution) {
                //         return isRelatedTo((<SubstitutionType>source).substitute,
                // (<SubstitutionType>target).substitute, /*reportErrors*/ false);
                //     }

                return Ok(TscTernary::False);
            }
            _ => {}
        }

        unimplemented!()
    }

    /// Ported from `isRelatedTo` of `tsc`.
    #[instrument(skip(self, span, source, target, opts))]
    fn is_related_to(
        &mut self,
        span: Span,
        source: &Type,
        target: &Type,
        opts: TypeRelatedToOpts,
    ) -> ValidationResult<TscTernary> {
        let source = self.normalize(Some(span), Cow::Borrowed(source), Default::default())?;
        let target = self.normalize(Some(span), Cow::Borrowed(target), Default::default())?;

        match (&*source, &*target) {
            (Type::TypeLit(..), Type::Lit(..) | Type::Keyword(..)) => {
                if self.is_simple_type_related_to(source, target, opts)? {
                    return Ok(TscTernary::True);
                }

                // TODO: reportErrorResults(originalSource, originalTarget,
                // Ternary.False, !!(getObjectFlags(originalSource) &
                // ObjectFlags.JsxAttributes));

                return Ok(TscTernary::False);
            }
            _ => {}
        }

        if source.type_eq(target) {
            return Ok(TscTernary::True);
        }

        match opts.mode {
            RelationCheckMode::Identity => return self.is_identical_to(span, source, target),
            _ => {}
        }

        // We fastpath comparing a type parameter to exactly its constraint, as this is
        // _super_ common, and otherwise, for type parameters in large unions,
        // causes us to need to compare the union to itself, as we break down
        // the _target_ union first, _then_ get the source constraint - so for every
        // member of the target, we attempt to find a match in the source. This avoids
        // that in cases where the target is exactly the constraint.
        match &*source {
            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => {
                if constraint.type_eq(target) {
                    return Ok(TscTernary::True);
                }
            }
            _ => {}
        }

        match opts.mode {
            RelationCheckMode::Comparable => {
                if !target.is_never() {
                    if self.is_simple_type_related_to(
                        span,
                        &target,
                        &source,
                        TypeRelatedToOpts {
                            report_errors: false,
                            ..opts
                        },
                    )? || self.is_simple_type_related_to(span, &source, &target, opts)?
                    {
                        return Ok(TscTernary::True);
                    }
                }
            }
            _ => {}
        }
    }

    /// Ported from `isIdenticalTo` of `tsc`.
    #[instrument(skip(self, span, source, target))]
    fn is_identical_to(&mut self, span: Span, source: &Type, target: &Type) -> ValidationResult<TscTernary> {}

    /// Ported from `isSimpleTypeRelatedTo` of `tsc`.
    #[instrument(skip(self, span, source, target, opts))]
    fn is_simple_type_related_to(
        &mut self,
        span: Span,
        source: &Type,
        target: &Type,
        opts: TypeRelatedToOpts,
    ) -> ValidationResult<bool> {
    }

    /// Ported from `propertiesRelatedTo` of `tsc`.
    #[instrument(skip(self, span, source, target, exclude_properties, opts))]
    fn properties_related_to(
        &mut self,
        span: Span,
        source: &Type,
        target: &Type,
        exclude_properties: FxHashSet<JsWord>,
        opts: TypeRelatedToOpts,
    ) -> ValidationResult<TscTernary> {
        unimplemented!()
    }
}
