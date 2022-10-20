use rnode::{VisitMut, VisitMutWith};
use stc_ts_types::{Mapped, Type, TypeParam};
use stc_ts_utils::MapWithMut;

pub fn reduce(m: &Mapped) -> Option<Type> {
    let mut type_param = m.type_param.clone();
    type_param.constraint.visit_mut_with(&mut ConstraintReducer);
    type_param.constraint.map(|v| *v)
}

struct ConstraintReducer;

impl VisitMut<Type> for ConstraintReducer {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty.visit_mut_children_with(self);

        match ty {
            Type::Param(TypeParam {
                constraint: Some(box arr_ty @ Type::Array(..)),
                ..
            }) => {
                let arr_ty = arr_ty.take();
                *ty = arr_ty;
                return;
            }
            _ => {}
        }
    }
}
