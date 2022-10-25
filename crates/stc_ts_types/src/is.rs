//! Smarter version of `derive(Is)`
#![allow(deprecated)]

use crate::*;

macro_rules! impl_is {
    ($variant:ident,$type_name:ident, $is_name:ident,$as_name:ident,$as_mut_name:ident,$opt_name:ident,$expect_name:ident) => {
        impl Type {
            pub fn $is_name(&self) -> bool {
                matches!(self.normalize(), Type::$variant(_))
            }

            pub fn $as_name(&self) -> Option<&$type_name> {
                match self.normalize() {
                    Type::$variant(ty) => Some(ty),
                    _ => None,
                }
            }
        }
    };
}

impl_is!(
    Array,
    Array,
    is_array,
    as_array,
    as_array_mut,
    array,
    expect_array
);
