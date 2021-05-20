use stc_ts_types::name::Name;
use stc_ts_types::Type;

/// Used to detect `form` of union types.
///
/// For example, `T | PromiseLike<T>` has identical `form` with `void |
/// PrmomiseLike<void>`
#[derive(Debug, PartialEq, Eq)]
pub(super) enum TypeForm {
    Ref(Name),
    Other,
}

impl From<&Type> for TypeForm {
    fn from(ty: &Type) -> Self {
        match ty.normalize() {
            Type::Ref(r) => Self::Ref(r.type_name.clone().into()),
            _ => Self::Other,
        }
    }
}
