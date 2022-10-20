use stc_ts_types::{name::Name, Type};

/// Used to detect `form` of union types.
///
/// For example, `T | PromiseLike<T>` has identical `form` with `void |
/// PrmomiseLike<void>`
#[derive(Debug, PartialEq, Eq)]
pub(super) enum OldTypeForm {
    Ref(Name),
    Other,
    Array(Box<OldTypeForm>),
    TypeLit { prop_count: usize },
    Fn { ret_ty: Box<OldTypeForm> },
}

impl From<&Type> for OldTypeForm {
    fn from(ty: &Type) -> Self {
        match ty.normalize() {
            Type::Ref(r) => Self::Ref(r.type_name.clone().into()),
            Type::Array(ty) => Self::Array(box Self::from(&*ty.elem_type)),
            Type::TypeLit(ty) => Self::TypeLit {
                prop_count: ty.members.len(),
            },
            Type::Function(f) => Self::Fn {
                ret_ty: box Self::from(&*f.ret_ty),
            },
            _ => Self::Other,
        }
    }
}
