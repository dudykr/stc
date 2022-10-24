use rnode::{VisitMut, VisitMutWith};
use stc_ts_types::{
    Array, Conditional, FnParam, Intersection, KeywordTypeMetadata, Type, TypeOrSpread, TypeParam,
    Union, Valid,
};
use swc_common::TypeEq;

pub trait Fix: Sized {
    fn fix(&mut self);

    fn fixed(mut self) -> Self {
        self.fix();
        self
    }
}

impl<T> Fix for Vec<T>
where
    T: Fix,
{
    fn fix(&mut self) {
        self.iter_mut().for_each(|item| item.fix())
    }
}

impl<T> Fix for Option<T>
where
    T: Fix,
{
    fn fix(&mut self) {
        match self {
            Some(v) => v.fix(),
            None => {}
        }
    }
}

impl<T> Fix for Box<T>
where
    T: Fix,
{
    fn fix(&mut self) {
        (**self).fix()
    }
}

macro_rules! impl_fix {
    ($T:ty) => {
        impl Fix for $T {
            #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
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
impl_fix!(TypeOrSpread);
impl_fix!(Conditional);
impl_fix!(FnParam);
impl_fix!(TypeParam);

struct Fixer;

impl VisitMut<Union> for Fixer {
    fn visit_mut(&mut self, u: &mut Union) {
        u.visit_mut_children_with(self);

        let mut new: Vec<Type> = Vec::with_capacity(u.types.capacity());
        for ty in u.types.drain(..) {
            if ty.is_never() {
                continue;
            }

            if new.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }

            if ty.normalize().is_union_type() {
                let u = ty.foldable().union_type().unwrap();
                for ty in u.types {
                    if new.iter().any(|stored| stored.type_eq(&ty)) {
                        continue;
                    }
                    if ty.is_never() {
                        continue;
                    }
                    new.push(ty);
                }
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

            if ty.normalize().is_intersection_type() {
                let i = ty.foldable().intersection_type().unwrap();
                for ty in i.types {
                    if new.iter().any(|stored| stored.type_eq(&ty)) {
                        continue;
                    }
                    new.push(ty);
                }
                continue;
            }

            new.push(ty);
        }
        ty.types = new;
    }
}

impl Fixer {
    fn fix_type(&mut self, ty: &mut Type) {
        if ty.is_arc() {
            return;
        }

        if matches!(ty, Type::Keyword(..) | Type::Lit(..)) {
            return;
        }

        if ty.is_valid() {
            return;
        }

        ty.normalize_mut();
        ty.visit_mut_children_with(self);

        match ty {
            Type::Union(u) => match u.types.len() {
                0 => {
                    *ty = Type::never(
                        u.span,
                        KeywordTypeMetadata {
                            common: u.metadata.common,
                            ..Default::default()
                        },
                    );
                    return;
                }
                1 => {
                    let mut elem = u.types.drain(..).next().unwrap();
                    elem.respan(u.span);
                    *ty = elem;
                    return;
                }
                _ => {}
            },

            Type::Intersection(i) => match i.types.len() {
                0 => {
                    *ty = Type::any(
                        i.span,
                        KeywordTypeMetadata {
                            common: i.metadata.common,
                            ..Default::default()
                        },
                    );
                    return;
                }
                1 => {
                    let mut elem = i.types.drain(..).next().unwrap();
                    elem.respan(i.span);
                    *ty = elem;
                    return;
                }
                _ => {}
            },
            _ => {}
        }
    }
}

impl VisitMut<Type> for Fixer {
    fn visit_mut(&mut self, ty: &mut Type) {
        self.fix_type(ty);

        ty.assert_valid();
    }
}
