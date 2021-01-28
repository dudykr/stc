use crate::analyzer::Analyzer;
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(crate) fn expand_this(&mut self, ty: &mut Type) {
        let this_ty = self.scope.this();

        if let Some(this) = this_ty {
            ty.visit_mut_with(&mut ThisReplacer { this_ty: &this })
        }
    }
}

#[derive(Default)]
struct ThisFinder {
    found: bool,
}

impl Visit<Type> for ThisFinder {
    fn visit(&mut self, ty: &Type) {
        ty.visit_children_with(self);

        match ty {
            Type::This(..) => {
                self.found = true;
            }
            _ => {}
        }
    }
}

struct ThisReplacer<'a> {
    this_ty: &'a Type,
}

impl VisitMut<Type> for ThisReplacer<'_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        // Fast path.
        {
            let mut v = ThisFinder::default();
            ty.visit_with(&mut v);
            if !v.found {
                return;
            }
        }

        ty.normalize_mut();
        match ty {
            Type::This(..) => {
                *ty = self.this_ty.clone();
            }
            _ => {}
        }
    }
}
