use rnode::{Visit, VisitMut, VisitMutWith, VisitWith};
use rustc_hash::FxHashMap;

use crate::{CowType, Type};

/// Replaces all types which matches `matcher` with `replacer`.
///
///  - `replacer` is called iff `matcher` returns `true`.
///
///  - `matcher` is optimization, and it **should not** be recursive because
/// `replace_type` invokes it recursively.
pub fn replace_type<M, R>(ty: &mut Type, matcher: M, replacer: R)
where
    M: Fn(&Type) -> bool,
    R: Fn(&mut Type) -> Option<Type>,
{
    let mut cache = FxHashMap::default();
    ty.visit_mut_with(&mut TypeReplacer {
        cache: &mut cache,

        matcher: &matcher,
        replacer: &replacer,
    });
}

type Cache = FxHashMap<*const (), bool>;

struct TypeReplacer<'a, M, R>
where
    M: Fn(&Type) -> bool,
    R: Fn(&mut Type) -> Option<Type>,
{
    cache: &'a mut Cache,

    matcher: &'a M,
    replacer: &'a R,
}

impl<M, R> VisitMut<Type> for TypeReplacer<'_, M, R>
where
    M: Fn(&Type) -> bool,
    R: Fn(&mut Type) -> Option<Type>,
{
    fn visit_mut(&mut self, ty: &mut Type) {
        {
            let mut finder = Finder {
                cache: self.cache,
                matcher: self.matcher,
                found: false,
            };
            ty.visit_with(&mut finder);
            if !finder.found {
                return;
            }
        }

        if (self.matcher)(ty) {
            if let Some(new_ty) = (self.replacer)(ty) {
                *ty = new_ty;
                return;
            }
        }

        ty.visit_mut_children_with(self);
    }
}

struct Finder<'a, M> {
    cache: &'a mut Cache,
    matcher: &'a M,
    found: bool,
}

impl<M> Visit<Type> for Finder<'_, M>
where
    M: Fn(&Type) -> bool,
{
    fn visit(&mut self, ty: &Type) {
        if (self.matcher)(ty) {
            self.found = true;
        }

        if self.found {
            return;
        }

        let key = ty as *const Type as *const ();

        if let Some(v) = self.cache.get(&key).copied() {
            self.found |= v;
            return;
        }

        ty.visit_children_with(self);

        self.cache.insert(key, self.found);
    }
}
