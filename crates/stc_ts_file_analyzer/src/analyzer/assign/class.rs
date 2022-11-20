use std::borrow::Cow;

use stc_ts_errors::{ctx, ErrorKind};
use stc_ts_types::{Class, ClassDef, ClassMember, Type, TypeLitMetadata};
use stc_utils::cache::Freeze;
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
    pub(super) fn assign_to_class_def(&mut self, data: &mut AssignData, l: &ClassDef, r: &Type, opts: AssignOpts) -> VResult<()> {
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
                        if self.assign_to_class_def(data, l, &parent, opts).is_ok() {
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
                            msg: format!("Failed to collect class members"),
                        }
                        .into());
                    }
                } else {
                    &*rc.body
                };

                for (i, lm) in l.body.iter().enumerate() {
                    let _ctx = ctx!(format!(
                        "tried to assign class members to {}th class member\n{:#?}\n{:#?}",
                        i, lm, r_body
                    ));
                    self.assign_class_members_to_class_member(data, lm, r_body, opts);
                }

                return Ok(());
            }

            Type::TypeLit(..) | Type::Interface(..) => {
                let rhs = self.convert_type_to_type_lit(opts.span, Cow::Borrowed(&*r))?.unwrap();

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

                let _ctx = ctx!("tried to assign type elements to a class member");
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
                )?;

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

    pub(super) fn assign_to_class(&mut self, data: &mut AssignData, l: &Class, r: &Type, opts: AssignOpts) -> VResult<()> {
        // debug_assert!(!span.is_dummy());

        let r = r.normalize();

        match r {
            Type::Ref(..) => {
                let mut r = self.expand_top_ref(opts.span, Cow::Borrowed(r), Default::default())?;
                r.make_clone_cheap();
                return self.assign_to_class(data, l, &r, opts);
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
                        return Err(ErrorKind::Unimplemented {
                            span: opts.span,
                            msg: format!("Failed to collect class members"),
                        }
                        .into());
                    }
                } else {
                    &*rc.def.body
                };

                for (i, lm) in l.def.body.iter().enumerate() {
                    let _ctx = ctx!(format!(
                        "tried to assign class members to {}th class member\n{:#?}\n{:#?}",
                        i, lm, r_body
                    ));
                    self.assign_class_members_to_class_member(data, lm, r_body, opts)?;
                }

                if !rc.def.is_abstract {
                    // class Child extends Parent
                    // let c: Child;
                    // let p: Parent;
                    // `p = c` is valid
                    if let Some(parent) = &rc.def.super_class {
                        let _ctx = ctx!("tried to instantiated class to assign the super class to a class");
                        let parent = self.instantiate_class(opts.span, &parent)?;
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

                let _ctx = ctx!("tried to assign type elements to class members");

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
                )?;

                return Ok(());
            }

            _ => {}
        };

        // Everything left is assignable to empty classes, including classes with only
        // constructors.
        if !opts.disallow_special_assignment_to_empty_class {
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
        }

        match r {
            Type::Lit(..) | Type::Keyword(..) => {
                return Err(ErrorKind::SimpleAssignFailed {
                    span: opts.span,
                    cause: None,
                }
                .context("cannot assign literal or keyword to a class"))
            }
            _ => {}
        }

        Err(ErrorKind::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class\n{:#?}", r),
        }
        .into())
    }

    fn assign_class_members_to_class_member(
        &mut self,
        data: &mut AssignData,
        l: &ClassMember,
        r: &[ClassMember],
        opts: AssignOpts,
    ) -> VResult<()> {
        let span = opts.span;

        match l {
            ClassMember::Constructor(lc) => {
                for rm in r {
                    match rm {
                        ClassMember::Constructor(rc) => {
                            self.assign_params(data, &lc.params, &rc.params, opts)?;
                            // TODO(kdy1): Validate parameters and etc..
                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }
            ClassMember::Method(lm) => {
                for r_member in r {
                    match r_member {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(rm) => {
                            //
                            if self.key_matches(span, &lm.key, &rm.key, false) {
                                if lm.span.lo == rm.span.lo && lm.span.hi == rm.span.hi {
                                    return Ok(());
                                }

                                if rm.accessibility == Some(Accessibility::Private) || rm.key.is_private() {
                                    return Err(ErrorKind::PrivateMethodIsDifferent { span }.into());
                                }

                                let _ctx = ctx!("tried to assign a class method to another one");
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
                                )?;

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

                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("failed to assign a class member to another one"));
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
                                if let Some(lt) = &lp.value {
                                    if let Some(rt) = &rp.value {
                                        let _ctx = ctx!("tried to assign a class proeprty to another");
                                        self.assign_inner(data, &lt, &rt, opts)?;
                                    }
                                }

                                if lp.span.lo == rp.span.lo && lp.span.hi == rp.span.hi {
                                    return Ok(());
                                }

                                if rp.accessibility == Some(Accessibility::Private) || rp.key.is_private() {
                                    return Err(ErrorKind::PrivatePropertyIsDifferent { span }.into());
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
            ClassMember::IndexSignature(_) => {}
        }

        Err(ErrorKind::Unimplemented {
            span: opts.span,
            msg: format!("fine-grained class assignment to lhs member: {:#?}", l),
        }
        .into())
    }
}
