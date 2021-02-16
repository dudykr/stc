use crate::analyzer::Analyzer;
use fxhash::FxBuildHasher;
use indexmap::IndexSet;
use itertools::Itertools;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_types::Id;
use stc_ts_types::Type;
use stc_ts_types::TypeParam;
use stc_ts_types::TypeParamDecl;
use swc_common::DUMMY_SP;

impl Analyzer<'_, '_> {
    pub(crate) fn add_required_type_params(&self, ty: &mut Type) {
        let mut finder = TypeParamUsageFinder::default();

        ty.visit_with(&mut finder);

        if finder.used.is_empty() {
            match ty {
                Type::Function(f) => {
                    f.type_params = None;
                }
                _ => {}
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
            })
            .collect_vec();
        match ty {
            Type::Function(f) => {
                // Create new type param decls if required.
                match &mut f.type_params {
                    Some(v) => {
                        v.params = params;
                    }
                    None => f.type_params = Some(TypeParamDecl { span: DUMMY_SP, params }),
                }
            }
            _ => {}
        }
    }
}

#[derive(Default)]
struct TypeParamUsageFinder {
    used: IndexSet<Id, FxBuildHasher>,
}

impl Visit<Type> for TypeParamUsageFinder {
    fn visit(&mut self, ty: &Type) {
        ty.visit_children_with(self);

        match ty {
            Type::Param(p) => {
                self.used.insert(p.name.clone());
            }
            _ => {}
        }
    }
}
