use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::Class;
use stc_ts_types::ClassMember;
use stc_ts_types::Type;
use swc_common::EqIgnoreSpan;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_class(&mut self, opts: AssignOpts, l: &Class, r: &Type) -> ValidationResult<()> {
        // debug_assert!(!span.is_dummy());

        let r = r.normalize();

        // Everything is assignable to empty classes, including classes with only
        // constructors.
        let is_empty = l
            .body
            .iter()
            .find(|member| match member {
                ClassMember::Constructor(_) => false,
                _ => true,
            })
            .is_none();
        if is_empty {
            return Ok(());
        }

        match r {
            Type::Class(r) => {
                if l.eq_ignore_span(r) {
                    return Ok(());
                }

                // class Child extends Parent
                // let c: Child;
                // let p: Parent;
                // `p = c` is valid
                if let Some(parent) = &r.super_class {
                    if self.assign_to_class(opts, l, &parent).is_ok() {
                        return Ok(());
                    }
                }

                for lm in &l.body {
                    self.assign_class_members_to_class_member(opts, lm, &r.body)?;
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
            _ => {}
        };

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
        match l {
            ClassMember::Constructor(_) => {}
            ClassMember::Method(_) => {}
            ClassMember::Property(lp) => {
                for rm in r {
                    match rm {
                        ClassMember::Constructor(_) => {}
                        ClassMember::Method(_) => {}
                        ClassMember::Property(rp) => {
                            if self.assign(&lp.key.ty(), &rp.key.ty(), opts.span).is_ok() {
                                if let Some(lt) = &lp.value {
                                    if let Some(rt) = &rp.value {
                                        return self.assign(&lt, &rt, opts.span);
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

                // TODO: Report error
            }
            ClassMember::IndexSignature(_) => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("fine-grained class assignment to lhs memeber: {:#?}", l),
        })
    }
}
