use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::Error;
use stc_ts_types::Class;
use stc_ts_types::ClassMember;
use stc_ts_types::Type;
use swc_common::EqIgnoreSpan;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_class(&self, opts: AssignOpts, l: &Class, r: &Type) -> ValidationResult<()> {
        // debug_assert!(!span.is_dummy());

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

                Err(Error::Unimplemented {
                    span: opts.span,
                    msg: format!("fine-grained class assignment"),
                })?
            }
            _ => {}
        };

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("Assignment of non-class object to class\n{:#?}", r),
        })
    }
}
