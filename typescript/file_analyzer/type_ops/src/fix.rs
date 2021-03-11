use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_types::Type;

pub trait Fix: Sized {
    fn fix(&mut self);

    fn fixed(mut self) -> Self {
        self.fix();
        self
    }
}

impl Fix for Type {
    fn fix(&mut self) {
        self.visit_mut_with(&mut Fixer);
    }
}

struct Fixer;

impl VisitMut<Type> for Fixer {
    fn visit_mut(&mut self, ty: &mut Type) {
        {
            let ty = ty.normalize();
            if ty.is_keyword() || ty.is_lit() {
                return;
            }
        }

        ty.normalize_mut();
        ty.visit_mut_children_with(self);

        match ty {
            Type::Union(u) => match u.types.len() {
                0 => {
                    *ty = Type::never(u.span);
                    return;
                }
                1 => {
                    let elem = u.types.drain(..).next().unwrap();
                    *ty = elem;
                    return;
                }
                _ => {}
            },
            _ => {}
        }
    }
}
