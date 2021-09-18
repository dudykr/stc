use stc_ts_types::Type;

pub trait TypeExt: Into<Type> {}

impl<T> TypeExt for T where T: Into<Type> {}

pub trait TypeIterExt: Iterator<Item = Box<Type>> {}
