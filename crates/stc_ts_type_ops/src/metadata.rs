use rnode::{Visit, VisitWith};
use stc_ts_types::Type;

pub struct TypeFinder {
    found: bool,
    check: fn(&Type) -> bool,
}

impl TypeFinder {
    pub fn find<N>(node: &N, check: fn(&Type) -> bool) -> bool
    where
        N: VisitWith<Self>,
    {
        let mut v = TypeFinder {
            found: false,
            check,
        };
        node.visit_with(&mut v);
        v.found
    }
}

impl Visit<Type> for TypeFinder {
    fn visit(&mut self, ty: &Type) {
        if self.found {
            return;
        }

        if (self.check)(ty) {
            self.found = true;
            return;
        }

        ty.visit_children_with(self);
    }
}
