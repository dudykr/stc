use stc_ts_types::Type;
use swc_common::TypeEq;
use tracing::instrument;

pub trait TypeExt: Into<Type> {}

impl<T> TypeExt for T where T: Into<Type> {}

pub trait TypeIterExt: Iterator<Item = Box<Type>> {}
