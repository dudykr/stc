use fxhash::FxBuildHasher;
use indexmap::IndexSet;
use itertools::Itertools;
use rnode::{Visit, VisitWith};
use stc_ts_types::{Id, Type, TypeParam, TypeParamDecl};
use swc_common::DUMMY_SP;

use crate::analyzer::Analyzer;

impl Analyzer<'_, '_> {
    pub(crate) fn add_required_type_params(&self, ty: &mut Type) {
        if let Some(instance) = ty.as_instance_mut() {
            self.add_required_type_params(&mut instance.ty);
            return;
        }

        let mut finder = TypeParamUsageFinder::default();

        ty.visit_with(&mut finder);

        if finder.used.is_empty() {
            if let Some(f) = ty.as_fn_type_mut() {
                f.type_params = None;
            }
            return;
        }

        let params = finder
            .used
            .into_iter()
            .map(|name| TypeParam {
                span: DUMMY_SP,
                name,
                constraint: None,
                default: None,
                metadata: Default::default(),
                tracker: Default::default(),
            })
            .collect_vec();

        if let Some(f) = ty.as_fn_type_mut() {
            // Create new type param decls if required.
            match &mut f.type_params {
                Some(v) => {
                    v.params = params;
                }
                None => {
                    f.type_params = Some(TypeParamDecl {
                        span: DUMMY_SP,
                        params,
                        tracker: Default::default(),
                    })
                }
            }
        }
    }
}

#[derive(Default)]
struct TypeParamUsageFinder {
    used: IndexSet<Id, FxBuildHasher>,
}

/// Ignore usages of type parameters in `constraint`.
impl Visit<TypeParam> for TypeParamUsageFinder {
    fn visit(&mut self, ty: &TypeParam) {
        ty.default.visit_with(self);
    }
}

impl Visit<Type> for TypeParamUsageFinder {
    fn visit(&mut self, ty: &Type) {
        ty.visit_children_with(self);

        if let Type::Param(p) = ty.normalize() {
            self.used.insert(p.name.clone());
        }
    }
}
