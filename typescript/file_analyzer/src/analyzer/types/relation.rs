use crate::{analyzer::Analyzer, ValidationResult};
use fxhash::FxHashSet;
use stc_ts_types::{ternary::TscTernary, Operator, Type};
use swc_atoms::JsWord;
use swc_ecma_ast::TsTypeOperatorOp;
use tracing::instrument;

#[derive(Debug, Clone, Copy)]
pub enum RelationCheckMode {
    Identity,
    Subtype,
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
    pub report_errros: bool,
    pub intesection_state: TscIntersectionState,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TscIntersectionState {
    pub property_check: bool,
}

impl Analyzer<'_, '_> {
    /// Ported from `structuredTypeRelatedTo` of `tsc`.
    #[instrument(skip(self, source, target, opts))]
    pub(crate) fn structured_type_related_to(
        &mut self,
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
                            source,
                            target,
                            TypeRelatedToOpts {
                                report_errros: false,
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
                            &source.obj_type,
                            &target.obj_type,
                            TypeRelatedToOpts {
                                report_errros: false,
                                ..opts
                            },
                        )?;
                        if result.as_bool() {
                            result &= self.is_related_to(
                                &source.obj_type,
                                &target.obj_type,
                                TypeRelatedToOpts {
                                    report_errros: false,
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
                            &source.check_type,
                            &target.check_type,
                            TypeRelatedToOpts {
                                report_errros: false,
                                ..opts
                            }?,
                        );
                        if result.as_bool() {
                            result &= self.is_related_to(
                                &source.extends_type,
                                &target.extends_type,
                                TypeRelatedToOpts {
                                    report_errros: false,
                                    ..opts
                                },
                            )?;

                            if result.as_bool() {
                                result &= self.is_related_to(
                                    &source.true_type,
                                    &target.true_type,
                                    TypeRelatedToOpts {
                                        report_errros: false,
                                        ..opts
                                    },
                                )?;

                                if result.as_bool() {
                                    result &= self.is_related_to(
                                        &source.false_type,
                                        &target.false_type,
                                        TypeRelatedToOpts {
                                            report_errros: false,
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
    fn is_related_to(&mut self, source: &Type, target: &Type, opts: TypeRelatedToOpts) -> ValidationResult<TscTernary> {
        unimplemented!()
    }

    /// Ported from `propertiesRelatedTo` of `tsc`.
    fn properties_related_to(
        &mut self,
        source: &Type,
        target: &Type,
        exclude_properties: FxHashSet<JsWord>,
        opts: TypeRelatedToOpts,
    ) -> ValidationResult<TscTernary> {
        unimplemented!()
    }
}
