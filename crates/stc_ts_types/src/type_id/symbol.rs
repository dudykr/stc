use std::sync::atomic::{AtomicU64, Ordering::SeqCst};

use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, TypeEq};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan, TypeEq, Visit)]
pub struct SymbolId(
    u64,
    #[use_eq]
    #[visit(ignore)]
    Option<&'static str>,
);

impl SymbolId {
    pub fn generate() -> Self {
        Self::gen(None)
    }

    fn gen(static_name: Option<&'static str>) -> Self {
        static GENERATOR: AtomicU64 = AtomicU64::new(0);

        let id = GENERATOR.fetch_add(1, SeqCst);

        SymbolId(id, static_name)
    }
}

impl Serialize for SymbolId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.1 {
            Some(v) => serializer.serialize_str(v),
            None => serializer.serialize_str(&format!("{:?}", self.0)),
        }
    }
}

impl<'de> Deserialize<'de> for SymbolId {
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: String = Deserialize::deserialize(d)?;

        match s.parse::<u64>() {
            Ok(v) => Ok(SymbolId(v, None)),
            Err(_) => Ok(Self::known(&s)),
        }
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
                    static CACHED: Lazy<SymbolId> = Lazy::new(|| SymbolId::gen(Some($str_name)));

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
