use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::Class;
use stc_ts_types::ClassMember;
use stc_ts_types::Type;
use std::borrow::Cow;
use swc_common::EqIgnoreSpan;
use swc_ecma_ast::Accessibility;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_class(&mut self, opts: AssignOpts, l: &Class, r: &Type) -> ValidationResult<()> {
        // debug_assert!(!span.is_dummy());

        let r = r.normalize();

        match r {
            Type::Ref(..) => {
                let r = self.expand_top_ref(opts.span, Cow::Borrowed(r))?;
                return self.assign_to_class(opts, l, &r);
            }

            Type::Class(rc) => {
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
                        if self.assign_to_class(opts, l, &parent).is_ok() {
                            return Ok(());
                        }
                    }
                }

                let new_body;
                let r_body = if rc.super_class.is_some() {
                    if let Some(members) = self.collect_class_members(r)? {
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
                    self.assign_class_members_to_class_member(opts, lm, r_body)
                        .with_context(|| {
                            format!(
                                "tried to assign class members to {}th class member\n{:#?}\n{:#?}",
                                i, lm, r_body
                            )
                        })?;
                }

                return Ok(());
            }
            Type::Interface(rhs) => {
                // It's legal to assign an interface to a class if all class
                // memebers are public.
                //
                // See: classWithOnlyPublicMembersEquivalentToInterface.ts

                // TODO: Verify that all class members all public.

                for lm in &l.body {
                    let lm = self.make_type_el_from_class_member(lm)?;
                    let lm = match lm {
                        Some(v) => v,
                        None => {
                            // Static members does not affect equivalance.
                            //
                            // See: classWithOnlyPublicMembersEquivalentToInterface2
                            continue;
                        }
                    };
                    self.assign_type_elements_to_type_element(opts, &mut vec![], &lm, &rhs.body)
                        .context("tried to assign type elements to a class member")?;
                }

                // TODO: Assign parent interfaces

                return Ok(());
            }

            Type::TypeLit(rhs) => {
                for lm in &l.body {
                    let lm = self.make_type_el_from_class_member(lm)?;
                    let lm = match lm {
                        Some(v) => v,
                        None => {
                            continue;
                        }
                    };
                    self.assign_type_elements_to_type_element(opts, &mut vec![], &lm, &rhs.members)
                        .context("tried to assign type elements to a class member")?;
                }

                return Ok(());
            }

            Type::Intersection(rhs) => {
                let rhs = self
                    .type_to_type_lit(opts.span, r)
                    .context("tried to convert a type to type literal to assign it to a class")?;
                if let Some(rhs) = rhs.as_deref() {
                    for lm in &l.body {
                        let lm = self.make_type_el_from_class_member(lm)?;
                        let lm = match lm {
                            Some(v) => v,
                            None => {
                                continue;
                            }
                        };
                        self.assign_type_elements_to_type_element(opts, &mut vec![], &lm, &rhs.members)
                            .context("tried to assign type elements to a class member")?;
                    }

                    return Ok(());
                }
            }

            _ => {}
        };

        // Everything left is assignable to empty classes, including classes with only
        // constructors.
        let is_empty = l
            .body
            .iter()
            .find(|member| match member {
                ClassMember::Constructor(_) => false,
                _ => true,
            })
            .is_none();
        if !l.is_abstract && is_empty {
            return Ok(());
        }

        match r {
            Type::Lit(..) => return Err(Error::SimpleAssignFailed { span: opts.span }),
            _ => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class\n{:#?}", r),
        })
    }

    fn assign_class_members_to_class_member(
        &mut self,
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
                            self.assign_params(opts, &lc.params, &rc.params)?;
                            // TODO: Validate parameters and etc..
                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }
            ClassMember::Method(lm) => {
                if lm.accessibility == Some(Accessibility::Private) {
                    return Err(Error::PrivateMethodIsDifferent { span });
                }

                for rmember in r {
                    match rmember {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(rm) => {
                            //
                            if self.assign(&lm.key.ty(), &rm.key.ty(), opts.span).is_ok() {
                                if rm.accessibility == Some(Accessibility::Private) {
                                    return Err(Error::PrivateMethodIsDifferent { span });
                                }

                                // TODO: Parameters.
                                self.assign_with_opts(opts, &lm.ret_ty, &rm.ret_ty)
                                    .context("tried to assign return type of a class method")?;

                                return Ok(());
                            }
                        }
                        ClassMember::Property(_) => {}
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                if lm.is_optional {
                    return Ok(());
                }

                return Err(Error::SimpleAssignFailed { span });
            }
            ClassMember::Property(lp) => {
                if lp.accessibility == Some(Accessibility::Private) {
                    return Err(Error::PrivatePropertyIsDifferent { span });
                }

                for rm in r {
                    match rm {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(_) => {}
                        ClassMember::Property(rp) => {
                            if self.assign(&lp.key.ty(), &rp.key.ty(), opts.span).is_ok() {
                                if rp.accessibility == Some(Accessibility::Private) {
                                    return Err(Error::PrivatePropertyIsDifferent { span });
                                }

                                if let Some(lt) = &lp.value {
                                    if let Some(rt) = &rp.value {
                                        return self.assign_inner(&lt, &rt, opts);
                                    }
                                }

                                return Ok(());
                            }
                        }
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                if lp.is_optional {
                    return Ok(());
                }

                return Err(Error::SimpleAssignFailed { span });
            }
            ClassMember::IndexSignature(_) => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("fine-grained class assignment to lhs memeber: {:#?}", l),
        })
    }
}
