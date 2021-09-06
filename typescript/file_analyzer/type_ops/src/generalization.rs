use rnode::{Visit, VisitWith};
use stc_ts_types::{LitType, Type};

pub(crate) struct GeneralizableLiteralChecker {
    pub found: bool,
}

impl Visit<Type> for GeneralizableLiteralChecker {
    fn visit(&mut self, ty: &Type) {
        match ty {
            Type::Lit(LitType { metadata, .. }) => {
                if metadata.common.prevent_generalization {
                    return;
                }

                self.found = true;
                return;
            }
            _ => {}
        }

        ty.visit_children_with(self);
    }
}
