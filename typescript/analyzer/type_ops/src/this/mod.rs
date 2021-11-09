use rnode::{Visit, VisitWith};
use stc_ts_types::Type;

pub fn contains_this(ty: &Type) -> bool {
    let mut v = ThisFinder::default();
    ty.visit_with(&mut v);

    v.found
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
