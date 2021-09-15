use self::generalize::TupleToArray;
use retain_mut::RetainMut;
use rnode::FoldWith;
use stc_ts_type_ops::{generalization::LitGeneralizer, Fix};
pub(crate) use stc_ts_types::*;
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::instrument;

mod generalize;
pub mod type_facts;

pub trait TypeExt: Into<Type> {
    #[instrument(skip(self,))]
    fn generalize_lit(self) -> Type {
        self.into().fold_with(&mut LitGeneralizer).fixed()
    }

    #[instrument(skip(self))]
    fn generalize_tuple(self) -> Type {
        self.into().fold_with(&mut TupleToArray)
    }
}

impl<T> TypeExt for T where T: Into<Type> {}
