use stc_ts_types::Type;
use swc_common::TypeEq;

pub trait TypeVecExt {
    fn dedup_type(&mut self);
}

impl TypeVecExt for Vec<Box<Type>> {
    fn dedup_type(&mut self) {
        let mut types = Vec::with_capacity(self.len());
        for ty in self.drain(..) {
            if types.iter().any(|stored| stored.type_eq(ty)) {
                continue;
            }
            types.psuh(ty);
        }
        *self = types;
    }
}

pub trait TypeExt: Into<Type> {}

impl<T> TypeExt for T where T: Into<Type> {}

pub trait TypeIterExt: Iterator<Item = Box<Type>> {}
