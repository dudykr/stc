use fxhash::FxHashSet;
use rnode::{Visit, VisitMut, VisitMutWith, VisitWith};
use stc_ts_generics::type_param::finder::TypeParamDeclFinder;
use stc_ts_types::{Id, Mapped, Type, TypeLit};

use crate::analyzer::{scope::Scope, Analyzer};

impl Analyzer<'_, '_> {
    /// Convert type parameters declared in dead scopes to `{}`.
    ///
    /// This does not touch type parameters declared in parent scopes and this
    /// method should be callled when a type had escaped a scope.
    ///
    /// In this way, we can handle both of cases below.
    ///
    /// ## Invalid assignment.
    ///
    /// ```ts
    /// function foo<T, U>() {
    ///     var a: T;
    ///     var b: U;
    ///     a = b; // This is wrong.
    /// }
    /// ```
    ///
    /// This case is handled because type parameters are not touched (while we
    /// are analyzing function) by the method.
    ///
    /// ## Escaping
    ///
    /// ```ts
    /// function foo<T>() {}
    /// function bar<T>() {}
    ///
    /// var a = foo(); // type is {}
    /// a = bar();
    /// ```
    ///
    /// This method is called at the end of each call and each `T` is converted
    /// to `{}` even though span hygiene differs.
    pub(crate) fn replace_invalid_type_params(&mut self, ty: &mut Type) {
        if self.is_builtin {
            return;
        }

        let declared = {
            let mut v = TypeParamDeclFinder::default();
            ty.visit_with(&mut v);
            v.params
        };

        let mut v = TypeParamEscapeHandler { analyzer: self, declared };

        ty.visit_mut_with(&mut v);
    }

    fn is_type_param_dead(&mut self, name: &Id) -> bool {
        fn is_dead(s: &Scope, name: &Id) -> bool {
            if s.is_root() {
                return true;
            }

            if let Some(..) = s.facts.types.get(name) {
                return false;
            }
            if let Some(..) = s.types.get(name) {
                return false;
            }

            match s.parent {
                Some(p) => is_dead(&p, name),
                None => return true,
            }
        }

        is_dead(&self.scope, name)
    }
}

struct TypeParamEscapeVisitor<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
    declared: &'a FxHashSet<Id>,
    should_work: bool,
}

impl Visit<Type> for TypeParamEscapeVisitor<'_, '_, '_> {
    fn visit(&mut self, ty: &Type) {
        match ty {
            Type::Param(ty) => {
                if self.declared.contains(&ty.name) {
                    return;
                }

                if self.analyzer.is_type_param_dead(&ty.name) {
                    self.should_work = true;
                    return;
                }
            }
            _ => {}
        }

        ty.visit_children_with(self);
    }
}

struct TypeParamEscapeHandler<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,

    /// Type parameters declared by the type we are visiting.
    declared: FxHashSet<Id>,
}

impl VisitMut<Mapped> for TypeParamEscapeHandler<'_, '_, '_> {
    fn visit_mut(&mut self, _: &mut Mapped) {}
}

impl VisitMut<Type> for TypeParamEscapeHandler<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        if !ty.is_type_param() {
            // Fast path
            let mut v = TypeParamEscapeVisitor {
                analyzer: self.analyzer,
                declared: &self.declared,
                should_work: false,
            };
            ty.visit_with(&mut v);
            if !v.should_work {
                return;
            }
        }

        // TODO(kdy1): PERF
        ty.normalize_mut();
        ty.visit_mut_children_with(self);

        match ty {
            Type::Param(param) => {
                if self.declared.contains(&param.name) {
                    return;
                }

                if self.analyzer.is_type_param_dead(&param.name) {
                    *ty = Type::TypeLit(TypeLit {
                        span: param.span,
                        members: vec![],
                        metadata: Default::default(),
                    });
                    return;
                }
            }
            _ => {}
        }
    }
}
