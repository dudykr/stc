use crate::analyzer::Analyzer;
use fxhash::FxHashSet;
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ts_types::Id;
use stc_ts_types::Type;
use stc_ts_types::TypeLit;

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
        let mut v = TypeParamEscapeHandler {
            analyzer: self,
            declared: Default::default(),
        };

        ty.visit_mut_with(&mut v);
    }

    fn is_type_param_dead(&mut self, name: &Id) -> bool {
        match self.find_type(self.ctx.module_id, name) {
            Ok(Some(..)) => false,
            _ => true,
        }
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

impl VisitMut<Type> for TypeParamEscapeHandler<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        match ty.normalize() {
            Type::Function(ty) => {
                if let Some(type_params) = &ty.type_params {
                    self.declared
                        .extend(type_params.params.iter().map(|param| param.name.clone()));
                }
            }
            _ => {}
        }

        {
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

        ty.normalize_mut();
        ty.visit_mut_children_with(self);

        match ty {
            Type::Param(param) => {
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
