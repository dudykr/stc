use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_types::ClassMember;
use stc_ts_types::TypeElement;

impl Analyzer<'_, '_> {
    pub(super) fn assign_class_members_to_type_element(
        &mut self,
        opts: AssignOpts,
        el: &TypeElement,
        rhs_members: &[ClassMember],
    ) -> ValidationResult<()> {
        match el {
            TypeElement::Call(_) => {}
            TypeElement::Constructor(_) => {}
            TypeElement::Property(lp) => {
                for rhs_member in rhs_members {
                    match rhs_member {
                        ClassMember::Method(_) => {}
                        ClassMember::Property(rp) => {
                            // Check for property
                            if self.assign(&lp.key.ty(), &rp.key.ty(), opts.span).is_ok() {
                                if let Some(lt) = &lp.type_ann {
                                    if let Some(rt) = &rp.value {
                                        self.assign(&lt, &rt, opts.span)?;
                                    }
                                }
                            }
                        }
                        _ => continue,
                    }
                }

                if lp.optional {
                    return Ok(());
                }

                // TODO: Report error.
            }
            TypeElement::Method(_) => {}
            TypeElement::Index(_) => {}
        }

        Ok(())
    }
}
