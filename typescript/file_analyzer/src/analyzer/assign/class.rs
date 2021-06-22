use super::{AssignData, AssignOpts};
use crate::{analyzer::Analyzer, ValidationResult};
use itertools::Itertools;
use stc_ts_errors::{DebugExt, Error};
use stc_ts_types::{Class, ClassDef, ClassMember, QueryExpr, Type, TypeLitMetadata};
use std::borrow::Cow;
use swc_common::EqIgnoreSpan;
use swc_ecma_ast::Accessibility;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_class_def(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l: &ClassDef,
        r: &Type,
    ) -> ValidationResult<()> {
        let r = r.normalize();

        match r {
            Type::Ref(..) => {
                let r = self.expand_top_ref(opts.span, Cow::Borrowed(r))?;
                return self.assign_to_class_def(data, opts, l, &r);
            }

            Type::Query(r_ty) => match &*r_ty.expr {
                QueryExpr::TsEntityName(e) => {
                    let rhs = self
                        .resolve_typeof(opts.span, e)
                        .context("tried to resolve typeof for assignment")?;

                    return self.assign_to_class_def(data, opts, l, &rhs);
                }
                QueryExpr::Import(_) => {}
            },

            Type::ClassDef(rc) => {
                if l.eq_ignore_span(rc) {
                    return Ok(());
                }

                if !l.is_abstract && rc.is_abstract {
                    return Err(Error::CannotAssignAbstractConstructorToNonAbstractConstructor { span: opts.span });
                }

                if !rc.is_abstract {
                    // class Child extends Parent
                    // let c: Child;
                    // let p: Parent;
                    // `p = c` is valid
                    if let Some(parent) = &rc.super_class {
                        if self.assign_to_class_def(data, opts, l, &parent).is_ok() {
                            return Ok(());
                        }
                    }
                }

                let new_body;
                let r_body = if rc.super_class.is_some() {
                    if let Some(members) = self.collect_class_members(&[], r)? {
                        new_body = members;
                        &*new_body
                    } else {
                        return Err(Error::Unimplemented {
                            span: opts.span,
                            msg: format!("Failed to collect class members"),
                        });
                    }
                } else {
                    &*rc.body
                };

                for (i, lm) in l.body.iter().enumerate() {
                    self.assign_class_members_to_class_member(data, opts, lm, r_body)
                        .with_context(|| {
                            format!(
                                "tried to assign class members to {}th class member\n{:#?}\n{:#?}",
                                i, lm, r_body
                            )
                        })?;
                }

                return Ok(());
            }

            Type::TypeLit(..) | Type::Interface(..) => {
                let rhs = self.type_to_type_lit(opts.span, r)?.unwrap();

                let mut lhs_members = vec![];
                for lm in &l.body {
                    let lm = self.make_type_el_from_class_member(lm, true)?;
                    let lm = match lm {
                        Some(v) => v,
                        None => {
                            // Instance property does not exist at the moment.
                            continue;
                        }
                    };
                    lhs_members.push(lm);
                }

                self.assign_to_type_elements(
                    data,
                    AssignOpts {
                        allow_unknown_rhs: true,
                        ..opts
                    },
                    l.span,
                    &lhs_members,
                    &r,
                    TypeLitMetadata {
                        specified: true,
                        ..Default::default()
                    },
                )
                .context("tried to assign type elements to a class member")?;

                return Ok(());
            }
            _ => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class definition\n{:#?}", r),
        })
    }

    pub(super) fn assign_to_class(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l: &Class,
        r: &Type,
    ) -> ValidationResult<()> {
        // debug_assert!(!span.is_dummy());

        let r = r.normalize();

        match r {
            Type::Ref(..) => {
                let r = self.expand_top_ref(opts.span, Cow::Borrowed(r))?;
                return self.assign_to_class(data, opts, l, &r);
            }

            Type::Class(rc) => {
                if l.eq_ignore_span(rc) {
                    return Ok(());
                }

                let new_body;
                let r_body = if rc.def.super_class.is_some() {
                    if let Some(members) = self.collect_class_members(&[], r)? {
                        new_body = members;
                        &*new_body
                    } else {
                        return Err(Error::Unimplemented {
                            span: opts.span,
                            msg: format!("Failed to collect class members"),
                        });
                    }
                } else {
                    &*rc.def.body
                };

                for (i, lm) in l.def.body.iter().enumerate() {
                    self.assign_class_members_to_class_member(data, opts, lm, r_body)
                        .with_context(|| {
                            format!(
                                "tried to assign class members to {}th class member\n{:#?}\n{:#?}",
                                i, lm, r_body
                            )
                        })?;
                }

                if !rc.def.is_abstract {
                    // class Child extends Parent
                    // let c: Child;
                    // let p: Parent;
                    // `p = c` is valid
                    if let Some(parent) = &rc.def.super_class {
                        let parent = self
                            .instantiate_class(opts.span, &parent)
                            .context("tried to instanitate class to asssign the super class to a class")?;
                        if self.assign_to_class(data, opts, l, &parent).is_ok() {
                            return Ok(());
                        }
                    }
                }

                if opts.disallow_different_classes {
                    return Err(Error::SimpleAssignFailed { span: opts.span });
                }

                return Ok(());
            }

            Type::TypeLit(..) | Type::Interface(..) | Type::Intersection(..) => {
                let mut lhs_members = vec![];
                for lm in &l.def.body {
                    let lm = self.make_type_el_from_class_member(lm, false)?;
                    let lm = match lm {
                        Some(v) => v,
                        None => {
                            continue;
                        }
                    };
                    lhs_members.push(lm);
                }

                self.assign_to_type_elements(
                    data,
                    AssignOpts {
                        allow_unknown_rhs: true,
                        ..opts
                    },
                    l.span,
                    &lhs_members,
                    &r,
                    TypeLitMetadata {
                        specified: true,
                        ..Default::default()
                    },
                )
                .context("tried to assign type elements to class members")?;

                return Ok(());
            }

            _ => {}
        };

        // Everything left is assignable to empty classes, including classes with only
        // constructors.
        let is_empty = l
            .def
            .body
            .iter()
            .find(|member| match member {
                ClassMember::Constructor(_) => false,
                _ => true,
            })
            .is_none();
        if !l.def.is_abstract && is_empty {
            return Ok(());
        }

        match r {
            Type::Lit(..) | Type::Keyword(..) => return Err(Error::SimpleAssignFailed { span: opts.span }),
            _ => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class\n{:#?}", r),
        })
    }

    fn assign_class_members_to_class_member(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l: &ClassMember,
        r: &[ClassMember],
    ) -> ValidationResult<()> {
        let span = opts.span;

        match l {
            ClassMember::Constructor(lc) => {
                for rm in r {
                    match rm {
                        ClassMember::Constructor(rc) => {
                            self.assign_params(data, opts, &lc.params, &rc.params)?;
                            // TODO: Validate parameters and etc..
                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }
            ClassMember::Method(lm) => {
                for rmember in r {
                    match rmember {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(rm) => {
                            //
                            if self.key_matches(span, &lm.key, &rm.key, false) {
                                if lm.span.lo == rm.span.lo && lm.span.hi == rm.span.hi {
                                    return Ok(());
                                }

                                if rm.accessibility == Some(Accessibility::Private) || rm.key.is_private() {
                                    return Err(Error::PrivateMethodIsDifferent { span });
                                }

                                // TODO: Parameters.
                                self.assign_with_opts(data, opts, &lm.ret_ty, &rm.ret_ty)
                                    .context("tried to assign return type of a class method")?;

                                return Ok(());
                            }
                        }
                        ClassMember::Property(_) => {}
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                if lm.accessibility == Some(Accessibility::Private) || lm.key.is_private() {
                    return Err(Error::PrivateMethodIsDifferent { span });
                }

                if lm.is_optional {
                    return Ok(());
                }

                return Err(Error::SimpleAssignFailed { span });
            }
            ClassMember::Property(lp) => {
                for rm in r {
                    match rm {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(_) => {}
                        ClassMember::Property(rp) => {
                            if lp.is_static == rp.is_static
                                && lp.is_static == rp.is_static
                                && self.key_matches(span, &lp.key, &rp.key, false)
                            {
                                if lp.span.lo == rp.span.lo && lp.span.hi == rp.span.hi {
                                    return Ok(());
                                }

                                if rp.accessibility == Some(Accessibility::Private) || rp.key.is_private() {
                                    return Err(Error::PrivatePropertyIsDifferent { span });
                                }

                                if let Some(lt) = &lp.value {
                                    if let Some(rt) = &rp.value {
                                        return self.assign_inner(data, &lt, &rt, opts);
                                    }
                                }

                                return Ok(());
                            }
                        }
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                if lp.accessibility == Some(Accessibility::Private) || lp.key.is_private() {
                    return Err(Error::PrivatePropertyIsDifferent { span });
                }

                if lp.is_optional {
                    return Ok(());
                }

                if opts.use_missing_fields_for_class {
                    let err = Error::MissingFields { span, fields: vec![] };
                    return Err(Error::Errors {
                        span,
                        errors: vec![err],
                    });
                } else {
                    return Err(Error::SimpleAssignFailed { span });
                }
            }
            ClassMember::IndexSignature(_) => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("fine-grained class assignment to lhs memeber: {:#?}", l),
        })
    }
}
