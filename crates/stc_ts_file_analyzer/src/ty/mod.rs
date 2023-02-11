use rnode::FoldWith;
use stc_ts_type_ops::{generalization::LitGeneralizer, tuple_to_array::TupleToArray, Fix};
pub(crate) use stc_ts_types::*;
use stc_utils::dev_span;

pub mod type_facts;

pub trait TypeExt: Into<Type> {
    fn generalize_lit(self) -> Type {
        let _tracing = dev_span!("Type::generalize_lit");

        self.into().fold_with(&mut LitGeneralizer).fixed()
    }

    fn generalize_tuple(self) -> Type {
        let _tracing = dev_span!("Type::generalize_tuple");

        self.into().fold_with(&mut TupleToArray)
    }
}

impl<T> TypeExt for T where T: Into<Type> {}
