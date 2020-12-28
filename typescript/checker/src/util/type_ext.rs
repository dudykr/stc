use stc_types::Type;
use swc_common::TypeEq;

pub trait TypeVecExt {
    fn dedup_type(&mut self);
}

impl TypeVecExt for Vec<Box<Type>> {
    fn dedup_type(&mut self) {
        self.dedup_by(|a, b| a.type_eq(&*b))
    }
}

pub trait TypeExt: Into<Type> {}

impl<T> TypeExt for T where T: Into<Type> {}

pub trait TypeIterExt: Iterator<Item = Box<Type>> {}
