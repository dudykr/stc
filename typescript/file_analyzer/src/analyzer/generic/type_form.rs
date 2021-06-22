use stc_ts_types::{name::Name, Type};

/// Used to detect `form` of union types.
///
/// For example, `T | PromiseLike<T>` has identical `form` with `void |
/// PrmomiseLike<void>`
#[derive(Debug, PartialEq, Eq)]
pub(super) enum TypeForm {
    Ref(Name),
    Other,
    Array(Box<TypeForm>),
    TypeLit { prop_count: usize },
}

impl From<&Type> for TypeForm {
    fn from(ty: &Type) -> Self {
        match ty.normalize() {
            Type::Ref(r) => Self::Ref(r.type_name.clone().into()),
            Type::Array(ty) => Self::Array(box Self::from(&*ty.elem_type)),
            Type::TypeLit(ty) => Self::TypeLit {
                prop_count: ty.members.len(),
            },
            _ => Self::Other,
        }
    }
}
