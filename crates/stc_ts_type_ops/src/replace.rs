use rnode::{Visit, VisitMut, VisitMutWith, VisitWith};
use stc_ts_types::Type;

/// Replaces all types which matches `matcher` with `replacer`.
///
///  - `replacer` is called iff `matcher` returns `true`.
///
///  - `matcher` is optimization, and it **should not** be recursive because
/// `replace_type` invokes it recursively.
pub fn replace_type<M, R>(ty: &mut Type, matcher: M, replacer: R)
where
    M: Fn(&Type) -> bool,
    R: Fn(&Type) -> Option<Type>,
{
    ty.visit_mut_with(&mut TypeReplacer {
        matcher: &matcher,
        replacer: &replacer,
    });
}

struct TypeReplacer<'a, M, R>
where
    M: Fn(&Type) -> bool,
    R: Fn(&Type) -> Option<Type>,
{
    matcher: &'a M,
    replacer: &'a R,
}

impl<M, R> VisitMut<Type> for TypeReplacer<'_, M, R>
where
    M: Fn(&Type) -> bool,
    R: Fn(&Type) -> Option<Type>,
{
    fn visit_mut(&mut self, ty: &mut Type) {
        {
            let mut finder = Finder {
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

        ty.normalize_mut();
        ty.visit_mut_children_with(self);
    }
}

struct Finder<'a, M> {
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

        ty.visit_children_with(self);
    }
}
