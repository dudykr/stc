use std::borrow::Cow;

use crate::analyzer::Analyzer;
use rnode::{Visit, VisitMut, VisitMutWith, VisitWith};
use stc_ts_types::{ClassMember, ClassProperty, Id, Key, Method, Type};

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
                        key, is_static: false, ..
                    })
                    | ClassMember::Property(ClassProperty {
                        key, is_static: false, ..
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

    pub(crate) fn expand_this(&mut self, ty: &mut Type) {
        let this_ty = self.scope.this();

        if let Some(this) = this_ty.map(Cow::into_owned) {
            ty.visit_mut_with(&mut ThisReplacer {
                this_ty: this,
                analyzer: self,
            })
        }
    }
}

#[derive(Default)]
struct ThisFinder {
    found: bool,
}

impl Visit<Type> for ThisFinder {
    fn visit(&mut self, ty: &Type) {
        ty.visit_children_with(self);

        match ty {
            Type::This(..) => {
                self.found = true;
            }
            _ => {}
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
        {
            let mut v = ThisFinder::default();
            ty.visit_with(&mut v);
            if !v.found {
                return;
            }
        }

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
