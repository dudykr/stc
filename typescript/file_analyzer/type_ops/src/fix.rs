use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_types::Array;
use stc_ts_types::Intersection;
use stc_ts_types::Type;
use stc_ts_types::Union;
use swc_common::TypeEq;
use swc_ecma_ast::TsKeywordTypeKind;

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

impl VisitMut<Array> for Fixer {
    fn visit_mut(&mut self, arr: &mut Array) {
        arr.visit_mut_children_with(self);

        if arr.elem_type.normalize().is_union_type() {
            match arr.elem_type.normalize_mut() {
                Type::Union(u) => {
                    // Drop `null`s and `undefined`s.
                    let has_other = u.types.iter().any(|ty| {
                        !ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                            && !ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                    });

                    if has_other {
                        u.types.retain(|ty| {
                            !ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                                && !ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                        });
                    }
                }
                _ => {}
            }
        }
    }
}

impl VisitMut<Union> for Fixer {
    fn visit_mut(&mut self, u: &mut Union) {
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
