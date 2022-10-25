use rnode::{Visit, VisitMut, VisitMutWith, VisitWith};
use stc_ts_ast_rnode::RIdent;
use stc_ts_types::{LitType, Type};
use tracing::instrument;

#[instrument(skip(ty))]
pub fn prevent_generalize<N>(ty: &mut N)
where
    N: VisitMutWith<PreventGeneralization>,
{
    ty.visit_mut_with(&mut PreventGeneralization { _priv: () });
}

pub(crate) struct GeneralizableLiteralFinder {
    pub found: bool,
}

impl Visit<Type> for GeneralizableLiteralFinder {
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

pub struct PreventGeneralization {
    _priv: (),
}

impl VisitMut<Type> for PreventGeneralization {
    fn visit_mut(&mut self, ty: &mut Type) {
        {
            let mut checker = GeneralizableLiteralFinder { found: false };
            ty.visit_with(&mut checker);

            if !checker.found {
                return;
            }
        }

        ty.nm();
        ty.metadata_mut().prevent_generalization = true;

        ty.visit_mut_children_with(self);
    }
}

/// Prevent interop with hygiene.
impl VisitMut<RIdent> for PreventGeneralization {
    fn visit_mut(&mut self, _: &mut RIdent) {}
}
