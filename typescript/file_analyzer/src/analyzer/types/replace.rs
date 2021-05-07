use crate::analyzer::Analyzer;
use rnode::VisitMut;
use rnode::VisitMutWith;
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

        ty.visit_mut_children_with(self);

        for (pred, new) in self.map {
            if (**pred).type_eq(&*ty) {
                *ty = (**new).clone();
                return;
            }
        }
    }
}
