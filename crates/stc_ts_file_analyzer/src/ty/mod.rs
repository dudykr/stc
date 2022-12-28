use rnode::FoldWith;
use stc_ts_type_ops::{generalization::LitGeneralizer, tuple_to_array::TupleToArray, Fix};
pub(crate) use stc_ts_types::*;
use tracing::instrument;

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

    fn drop_enum_variant_name(self) -> Type {
        let ty: Type = self.into();

        if ty.is_enum_variant() {
            let e = ty.expect_enum_variant();
            Type::EnumVariant(EnumVariant { name: None, ..e })
        } else {
            ty
        }
    }
}

impl<T> TypeExt for T where T: Into<Type> {}
