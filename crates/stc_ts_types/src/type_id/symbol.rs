use std::sync::atomic::{AtomicU64, Ordering::SeqCst};

use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, TypeEq};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize,
)]
pub struct SymbolId(u64);

impl SymbolId {
    pub fn generate() -> Self {
        static GENERATOR: AtomicU64 = AtomicU64::new(0);

        let id = GENERATOR.fetch_add(1, SeqCst);

        SymbolId(id)
    }
}

macro_rules! known {
    (
        $(
            $name:ident => $str_name:expr,
        )*
    ) => {

        /// Known symbols.
        impl SymbolId {
            $(
                pub fn $name() -> Self {
                    static CACHED: Lazy<SymbolId> = Lazy::new(|| SymbolId::generate());

                    *CACHED
                }
            )*

            pub fn known(s:&str) -> Self {
                match s {
                    $(
                        $str_name => Self::$name(),
                    )*

                    _ => {
                        panic!("Unknown builtin symbol {}", s)
                    }
                }
            }
        }
    };
}

known!(
    iterator => "iterator",
    async_iterator => "asyncIterator",
    has_instance => "hasInstance",
    is_concat_spreadable => "isConcatSpreadable",
    r#match => "match",
    replace => "replace",
    search => "search",
    species => "species",
    split => "split",
    to_primitive => "toPrimitive",
    to_string_tag => "toStringTag",
    unscopables => "unscopables",
    match_all => "matchAll",
);
