use crate::util::map_with_mut::MapWithMut;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_types::{Mapped, Type, TypeParam};

pub fn reduce(m: &Mapped) -> Option<Box<Type>> {
    let mut type_param = m.type_param.clone();
    type_param.constraint.visit_mut_with(&mut ConstraintReducer);
    type_param.constraint
}

struct ConstraintReducer;

impl VisitMut<Type> for ConstraintReducer {
    fn visit_mut(&mut self, ty: &mut Type) {
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
