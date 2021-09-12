use stc_ts_types::Type;
use swc_common::TypeEq;
use tracing::instrument;

pub trait TypeVecExt {
    fn dedup_type(&mut self);
}

impl<T> TypeVecExt for Vec<T>
where
    T: TypeEq,
{
    #[instrument(skip(self))]
    fn dedup_type(&mut self) {
        let mut types: Vec<T> = Vec::with_capacity(self.capacity());
        for ty in self.drain(..) {
            if types.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }
            types.push(ty);
        }
        *self = types;
    }
}

pub trait TypeExt: Into<Type> {}

impl<T> TypeExt for T where T: Into<Type> {}

pub trait TypeIterExt: Iterator<Item = Box<Type>> {}
