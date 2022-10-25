//! Smarter version of `derive(Is)`
#![allow(deprecated)]

use debug_unreachable::debug_unreachable;

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

            /// This method normalizes the type **only if** the underlying type is the
            /// required variant.
            pub fn $as_mut_name(&mut self) -> Option<&mut $type_name> {
                if self.$is_name() {
                    match self.normalize_mut() {
                        Type::$variant(ty) => Some(ty),
                        _ => unsafe {
                            debug_unreachable!("`$is_name` is true, so this branch is unreachable")
                        },
                    }
                } else {
                    None
                }
            }

            /// This method normalizes the type **only if** the underlying type is the
            /// required variant.
            pub fn $opt_name(mut self) -> Option<$type_name> {
                if self.$is_name() {
                    self.normalize_mut();
                    match self {
                        Type::$variant(ty) => Some(ty),
                        _ => unsafe {
                            debug_unreachable!("`$is_name` is true, so this branch is unreachable")
                        },
                    }
                } else {
                    None
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
