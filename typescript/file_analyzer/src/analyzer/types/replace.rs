use crate::analyzer::Analyzer;
use rnode::{VisitMut, VisitMutWith};
use stc_ts_types::Type;
use swc_common::TypeEq;

impl Analyzer<'_, '_> {
    /// Used to prevent infinite recursion while assignment.
    pub(crate) fn replace(&mut self, ty: &mut Type, map: &[(&Type, &Type)]) {
        ty.visit_mut_with(&mut Replacer { map })
    }
}

struct Replacer<'a, 'b, 'c> {
    map: &'a [(&'b Type, &'c Type)],
}

impl VisitMut<Type> for Replacer<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        for (pred, new) in self.map {
            if (**pred).type_eq(&*ty) {
                *ty = (**new).clone();
                return;
            }
        }

        // TODO: PERF
        ty.normalize_mut();

        ty.visit_mut_children_with(self);

        for (pred, new) in self.map {
            if (**pred).type_eq(&*ty) {
                *ty = (**new).clone();
                return;
            }
        }
    }
}
