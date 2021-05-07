use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_types::Array;
use stc_ts_types::Intersection;
use stc_ts_types::Type;
use stc_ts_types::Union;
use swc_common::TypeEq;

pub trait Fix: Sized {
    fn fix(&mut self);

    fn fixed(mut self) -> Self {
        self.fix();
        self
    }
}

macro_rules! impl_fix {
    ($T:ty) => {
        impl Fix for $T {
            fn fix(&mut self) {
                self.visit_mut_with(&mut Fixer);
            }
        }
    };
}

impl_fix!(Type);
impl_fix!(Array);
impl_fix!(Union);
impl_fix!(Intersection);

struct Fixer;

impl VisitMut<Union> for Fixer {
    fn visit_mut(&mut self, u: &mut Union) {
        u.visit_mut_children_with(self);

        let mut new: Vec<Type> = Vec::with_capacity(u.types.capacity());
        for ty in u.types.drain(..) {
            if new.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }
            new.push(ty);
        }
        u.types = new;
    }
}

impl VisitMut<Intersection> for Fixer {
    fn visit_mut(&mut self, ty: &mut Intersection) {
        ty.visit_mut_children_with(self);

        let mut new: Vec<Type> = Vec::with_capacity(ty.types.capacity());
        for ty in ty.types.drain(..) {
            if new.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }
            new.push(ty);
        }
        ty.types = new;
    }
}

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

            Type::Intersection(i) => match i.types.len() {
                0 => {
                    *ty = Type::any(i.span);
                    return;
                }
                1 => {
                    let elem = i.types.drain(..).next().unwrap();
                    *ty = elem;
                    return;
                }
                _ => {}
            },
            _ => {}
        }
    }
}
