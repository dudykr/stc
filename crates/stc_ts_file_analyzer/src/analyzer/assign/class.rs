use std::borrow::Cow;

use itertools::Itertools;
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_types::{Class, ClassDef, ClassMember, Type, TypeLitMetadata};
use swc_common::EqIgnoreSpan;
use swc_ecma_ast::Accessibility;

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_class_def(&self, data: &mut AssignData, l: &ClassDef, r: &Type, opts: AssignOpts) -> VResult<()> {
        let r = self.normalize(Some(opts.span), Cow::Borrowed(r), Default::default())?;

        match r.normalize() {
            Type::ClassDef(rc) => {
                if l.eq_ignore_span(rc) {
                    return Ok(());
                }

                if !l.is_abstract && rc.is_abstract {
                    return Err(ErrorKind::CannotAssignAbstractConstructorToNonAbstractConstructor { span: opts.span }.into());
                }

                if !rc.is_abstract {
                    // class Child extends Parent
                    // let c: Child;
                    // let p: Parent;
                    // `p = c` is valid
                    if let Some(parent) = &rc.super_class {
                        if self.assign_to_class_def(data, l, parent, opts).is_ok() {
                            return Ok(());
                        }
                    }
                }

                let new_body;
                let r_body = if rc.super_class.is_some() {
                    if let Some(members) = self.collect_class_members(&[], &r)? {
                        new_body = members;
                        &*new_body
                    } else {
                        return Err(ErrorKind::Unimplemented {
                            span: opts.span,
                            msg: "Failed to collect class members".to_string(),
                        }
                        .into());
                    }
                } else {
                    &*rc.body
                };

                for (i, lm) in l.body.iter().enumerate() {
                    self.assign_class_members_to_class_member(data, lm, r_body, opts)
                        .with_context(|| format!("tried to assign class members to {}th class member\n{:#?}\n{:#?}", i, lm, r_body))?;
                }

                let lhs_members = l
                    .body
                    .iter()
                    .filter_map(|m| self.make_type_el_from_class_member(m, true))
                    .collect_vec();

                self.assign_to_type_elements(
                    data,
                    l.span,
                    &lhs_members,
                    &r,
                    TypeLitMetadata {
                        specified: true,
                        ..Default::default()
                    },
                    AssignOpts {
                        allow_unknown_rhs: Some(true),
                        is_assigning_to_class_members: true,
                        ..opts
                    },
                )
                .context("tried to assign to type elements created from a class")?;

                return Ok(());
            }

            Type::TypeLit(..) | Type::Interface(..) | Type::Intersection(..) => {
                let rhs = self
                    .convert_type_to_type_lit(opts.span, Cow::Borrowed(&*r), Default::default())?
                    .unwrap();

                let lhs_members = l
                    .body
                    .iter()
                    .filter_map(|m| self.make_type_el_from_class_member(m, true))
                    .collect_vec();

                self.assign_to_type_elements(
                    data,
                    l.span,
                    &lhs_members,
                    &r,
                    TypeLitMetadata {
                        specified: true,
                        ..Default::default()
                    },
                    AssignOpts {
                        allow_unknown_rhs: Some(true),
                        is_assigning_to_class_members: true,
                        ..opts
                    },
                )
                .context("tried to assign type elements to a class member")?;

                return Ok(());
            }
            _ => {}
        }

        Err(ErrorKind::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class definition\n{:#?}", r),
        }
        .into())
    }

    pub(super) fn assign_to_class(&self, data: &mut AssignData, l: &Class, r: &Type, opts: AssignOpts) -> VResult<()> {
        // debug_assert!(!span.is_dummy());

        let r = self.normalize(Some(opts.span), Cow::Borrowed(r), Default::default())?;

        match r.normalize() {
            Type::Class(rc) => {
                if l.eq_ignore_span(rc) {
                    return Ok(());
                }

                let new_body;
                let r_body = if rc.def.super_class.is_some() {
                    if let Some(members) = self.collect_class_members(&[], &r)? {
                        new_body = members;
                        &*new_body
                    } else {
                        return Err(ErrorKind::Unimplemented {
                            span: opts.span,
                            msg: "Failed to collect class members".to_string(),
                        }
                        .into());
                    }
                } else {
                    &*rc.def.body
                };

                for (i, lm) in l.def.body.iter().enumerate() {
                    self.assign_class_members_to_class_member(data, lm, r_body, opts)
                        .with_context(|| format!("tried to assign class members to {}th class member\n{:#?}\n{:#?}", i, lm, r_body))?;
                }

                let lhs_members = l
                    .def
                    .body
                    .iter()
                    .filter_map(|m| self.make_type_el_from_class_member(m, false))
                    .collect_vec();

                self.assign_to_type_elements(
                    data,
                    l.span,
                    &lhs_members,
                    &r,
                    TypeLitMetadata {
                        specified: true,
                        ..Default::default()
                    },
                    AssignOpts {
                        allow_unknown_rhs: Some(true),
                        is_assigning_to_class_members: true,
                        report_assign_failure_for_missing_properties: opts.report_assign_failure_for_missing_properties.or_else(|| {
                            if l.def.body.iter().any(|m| matches!(m, ClassMember::Method(..))) {
                                return Some(false);
                            }

                            Some(true)
                        }),
                        ..opts
                    },
                )
                .context("tried to assign to type elements created from a class")?;

                if !rc.def.is_abstract {
                    // class Child extends Parent
                    // let c: Child;
                    // let p: Parent;
                    // `p = c` is valid
                    if let Some(parent) = &rc.def.super_class {
                        let parent = self
                            .instantiate_class(opts.span, parent)
                            .context("tried to instantiated class to assign the super class to a class")?;
                        if self.assign_to_class(data, l, &parent, opts).is_ok() {
                            return Ok(());
                        }
                    }
                }

                if opts.disallow_different_classes {
                    return Err(ErrorKind::SimpleAssignFailed {
                        span: opts.span,
                        cause: None,
                    }
                    .context("opts.disallow_different_classes is true"));
                }

                return Ok(());
            }

            Type::TypeLit(..) | Type::Interface(..) | Type::Intersection(..) => {
                let lhs_members = l
                    .def
                    .body
                    .iter()
                    .filter_map(|m| self.make_type_el_from_class_member(m, false))
                    .collect_vec();

                self.assign_to_type_elements(
                    data,
                    l.span,
                    &lhs_members,
                    &r,
                    TypeLitMetadata {
                        specified: true,
                        ..Default::default()
                    },
                    AssignOpts {
                        allow_unknown_rhs: Some(true),
                        check_for_common_properties: Some(false),
                        is_assigning_to_class_members: true,
                        report_assign_failure_for_missing_properties: opts.report_assign_failure_for_missing_properties.or_else(|| {
                            Some(match r.normalize() {
                                Type::Interface(r) => !r.extends.is_empty(),
                                _ => false,
                            })
                        }),
                        ..opts
                    },
                )
                .context("tried to assign type elements to class members")?;

                return Ok(());
            }

            _ => {}
        };

        // Everything left is assignable to empty classes, including classes with only
        // constructors.
        if !opts.disallow_special_assignment_to_empty_class {
            let is_empty = !l.def.body.iter().any(|member| !matches!(&member, ClassMember::Constructor(_)));
            if !l.def.is_abstract && is_empty {
                return Ok(());
            }
        }

        if let Type::Lit(..) | Type::Keyword(..) = r.normalize() {
            return Err(ErrorKind::SimpleAssignFailed {
                span: opts.span,
                cause: None,
            }
            .context("cannot assign literal or keyword to a class"));
        }

        Err(ErrorKind::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class\n{:#?}", r),
        }
        .into())
    }

    fn assign_class_members_to_class_member(
        &self,
        data: &mut AssignData,
        l: &ClassMember,
        r: &[ClassMember],
        opts: AssignOpts,
    ) -> VResult<()> {
        let span = opts.span;

        match l {
            ClassMember::Constructor(lc) => {
                for rm in r {
                    if let ClassMember::Constructor(rc) = rm {
                        match (lc.accessibility, rc.accessibility) {
                            (Some(Accessibility::Public) | None, Some(Accessibility::Private | Accessibility::Protected))
                            | (Some(Accessibility::Protected), Some(Accessibility::Private)) => {
                                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("accessibility differs"));
                            }
                            _ => {}
                        }
                    }
                }
            }

            ClassMember::IndexSignature(..) => {}
            ClassMember::Method(lm) => {
                for r_member in r {
                    match r_member {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(rm) => {
                            //
                            if self.key_matches(span, &lm.key, &rm.key, false) {
                                if lm.span.lo == rm.span.lo && lm.span.hi == rm.span.hi {
                                } else {
                                    if rm.accessibility == Some(Accessibility::Private) || rm.key.is_private() {
                                        return Err(ErrorKind::PrivateMethodIsDifferent { span }.into());
                                    }
                                }

                                self.assign_to_fn_like(
                                    data,
                                    true,
                                    lm.type_params.as_ref(),
                                    &lm.params,
                                    Some(&lm.ret_ty),
                                    rm.type_params.as_ref(),
                                    &rm.params,
                                    Some(&rm.ret_ty),
                                    opts,
                                )
                                .context("tried to assign a class method to another one")?;

                                return Ok(());
                            }
                        }
                        ClassMember::Property(_) => {}
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                if lm.accessibility == Some(Accessibility::Private) || lm.key.is_private() {
                    return Err(ErrorKind::PrivateMethodIsDifferent { span }.into());
                }

                if lm.is_optional {
                    return Ok(());
                }
            }
            ClassMember::Property(lp) => {
                for rm in r {
                    match rm {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(_) => {}
                        ClassMember::Property(rp) => {
                            if lp.is_static == rp.is_static && self.key_matches(span, &lp.key, &rp.key, false) {
                                if let Some(lt) = &lp.value {
                                    if let Some(rt) = &rp.value {
                                        self.assign_inner(data, lt, rt, opts)
                                            .context("tried to assign a class property to another")?;
                                    }
                                }

                                if lp.span.lo == rp.span.lo && lp.span.hi == rp.span.hi {
                                } else {
                                    if rp.accessibility == Some(Accessibility::Private) || rp.key.is_private() {
                                        return Err(ErrorKind::PrivatePropertyIsDifferent { span }.into());
                                    }

                                    if lp.accessibility == Some(Accessibility::Private) && rp.accessibility != Some(Accessibility::Private)
                                    {
                                        return Err(ErrorKind::PrivatePropertyIsDifferent { span }.into());
                                    }
                                }

                                return Ok(());
                            }
                        }
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                if lp.accessibility == Some(Accessibility::Private) || lp.key.is_private() {
                    return Err(ErrorKind::PrivatePropertyIsDifferent { span }.into());
                }

                if lp.is_optional {
                    return Ok(());
                }

                if opts.use_missing_fields_for_class {
                    let err = ErrorKind::MissingFields { span, fields: vec![] }.into();
                    return Err(ErrorKind::Errors { span, errors: vec![err] }.into());
                } else {
                    return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.into());
                }
            }
        }

        Ok(())
    }
}
