use stc_ts_types::{replace::replace_type, Mapped, Type, TypeParam};
use stc_ts_utils::MapWithMut;

pub fn reduce(m: &Mapped) -> Option<Type> {
    let mut type_param = m.type_param.clone();

    if let Some(constraint) = &mut type_param.constraint {
        replace_type(
            constraint,
            |ty| {
                match ty {
                    Type::Param(TypeParam {
                        constraint: Some(box arr_ty),
                        ..
                    }) if arr_ty.is_array() => return true,
                    _ => (),
                }

                false
            },
            |ty| {
                if let Type::Param(TypeParam {
                    constraint: Some(box arr_ty @ Type::Array(..)),
                    ..
                }) = ty
                {
                    let arr_ty = arr_ty.take();
                    return Some(arr_ty);
                }

                None
            },
        )
    }

    type_param.constraint.map(|v| *v)
}
