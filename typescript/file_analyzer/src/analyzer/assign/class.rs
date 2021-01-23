use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::Error;
use stc_ts_types::Class;
use stc_ts_types::Type;
use swc_common::EqIgnoreSpan;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_class(&self, opts: AssignOpts, l: &Class, r: &Type) -> ValidationResult<()> {
        // debug_assert!(!span.is_dummy());

        if l.body.is_empty() {
            return Ok(());
        }

        match r {
            Type::Class(r) => {
                if l.eq_ignore_span(r) {
                    return Ok(());
                }

                let mut parent = &r.super_class;

                // class Child extends Parent
                // let c: Child;
                // let p: Parent;
                // `p = c` is valid
                while let Some(ref p) = parent {
                    match p.normalize() {
                        Type::Class(ref p_cls) => {
                            if l.eq_ignore_span(p_cls) {
                                return Ok(());
                            }

                            parent = &p_cls.super_class;
                        }
                        _ => Err(Error::Unimplemented {
                            span: opts.span,
                            msg: format!("fine-grained class assignment"),
                        })?,
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
            msg: format!("Assignment of non-class object to class"),
        })
    }
}
