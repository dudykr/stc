use rnode::{VisitMut, VisitMutWith};
use stc_ts_types::{
    ArcCowType, Array, Conditional, FnParam, Intersection, KeywordTypeMetadata, Type, TypeOrSpread, TypeParam, Union, Valid,
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
            fn fix(&mut self) {
                let _tracing = stc_utils::dev_span!("fix");

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
impl_fix!(ArcCowType);

struct Fixer;

impl VisitMut<Union> for Fixer {
    fn visit_mut(&mut self, u: &mut Union) {
        for ty in &u.types {
            ty.assert_valid();
        }

        let mut new: Vec<ArcCowType> = Vec::with_capacity(u.types.capacity());
        for ty in u.types.drain(..) {
            if ty.is_never() {
                continue;
            }

            if new.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }

            if ty.is_union_type() {
                let u = ty.into_owned().expect_union_type();
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
        for ty in &ty.types {
            ty.assert_valid();
        }

        let mut new: Vec<ArcCowType> = Vec::with_capacity(ty.types.capacity());
        for ty in ty.types.drain(..) {
            if new.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }

            if ty.is_intersection() {
                let i = ty.into_owned().expect_intersection();
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
        if matches!(ty, Type::Keyword(..) | Type::Lit(..)) {
            return;
        }

        if ty.is_valid() {
            return;
        }

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
                }
                1 => {
                    let mut elem = u.types.drain(..).next().unwrap();
                    elem.normalize_mut().respan(u.span);
                    *ty = elem.into_owned();
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
                }
                1 => {
                    let mut elem = i.types.drain(..).next().unwrap();
                    elem.normalize_mut().respan(i.span);
                    *ty = elem.into_owned();
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
    }
}

impl VisitMut<ArcCowType> for Fixer {
    fn visit_mut(&mut self, ty: &mut ArcCowType) {
        match ty {
            ArcCowType::Owned(ty) => {
                ty.visit_mut_with(self);
            }
            ArcCowType::Arc(_) => {
                // Freezed types are valid.
            }
        }
    }
}
