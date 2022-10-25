use std::borrow::Cow;

use rnode::{VisitMut, VisitMutWith};
use stc_ts_type_ops::this::contains_this;
use stc_ts_types::{ClassMember, ClassProperty, Id, Key, Method, Type};

use crate::analyzer::Analyzer;

impl Analyzer<'_, '_> {
    pub(crate) fn this_has_property_named(&mut self, p: &Id) -> bool {
        if self.scope.is_this_ref_to_object_lit() || self.scope.is_this_ref_to_class() {
            if let Some(declaring) = &self.scope.declaring_prop() {
                if *p.sym() == *declaring.sym() {
                    return true;
                }
            }
        }

        if self.scope.is_this_ref_to_class() {
            for (_, m) in self.scope.class_members() {
                match m {
                    ClassMember::Method(Method {
                        key,
                        is_static: false,
                        ..
                    })
                    | ClassMember::Property(ClassProperty {
                        key,
                        is_static: false,
                        ..
                    }) => match key {
                        Key::Normal { sym, .. } => {
                            if *p.sym() == *sym {
                                return true;
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        false
    }

    /// Expand `this` contained in `ty`.
    pub(crate) fn expand_this_in_type(&mut self, ty: &mut Type) {
        let this_ty = self.scope.this();

        if let Some(this) = this_ty.map(Cow::into_owned) {
            ty.visit_mut_with(&mut ThisReplacer {
                this_ty: this,
                analyzer: self,
            })
        }
    }
}

struct ThisReplacer<'a, 'b, 'c> {
    this_ty: Type,
    analyzer: &'a mut Analyzer<'b, 'c>,
}

/// Noop.
impl VisitMut<ClassMember> for ThisReplacer<'_, '_, '_> {
    fn visit_mut(&mut self, _: &mut ClassMember) {}
}

impl VisitMut<Type> for ThisReplacer<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        // Fast path.
        if !contains_this(&*ty) {
            return;
        }

        // TODO(kdy1): PERF
        ty.normalize_mut();
        ty.visit_mut_children_with(self);
        match ty {
            Type::This(..) => {
                *ty = self.this_ty.clone();
            }
            Type::Instance(i) => {
                if let Ok(instantiated) = self.analyzer.instantiate_class(i.span, &i.ty) {
                    *ty = instantiated;
                }
            }
            _ => {}
        }
    }
}
