use once_cell::sync::Lazy;
use stc_visit::Visit;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::SeqCst;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan, TypeEq, Visit)]
pub struct SymbolId(u64);

impl SymbolId {
    pub fn generate() -> Self {
        static GENERATOR: AtomicU64 = AtomicU64::new(0);

        let id = GENERATOR.fetch_add(1, SeqCst);

        SymbolId(id)
    }
}

macro_rules! known {
    ($name:ident, $str_name:expr) => {
        impl SymbolId {
            pub fn $name() -> Self {
                static CACHED: Lazy<SymbolId> = Lazy::new(|| SymbolId::generate());

                *CACHED
            }
        }
    };
}

known!(iterator, "iterator");
