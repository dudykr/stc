use stc_visit::Visit;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::SeqCst;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan, TypeEq, Visit)]
pub struct ClassId(u64);

impl ClassId {
    pub fn generate() -> Self {
        static GENERATOR: AtomicU64 = AtomicU64::new(0);

        let id = GENERATOR.fetch_add(1, SeqCst);

        ClassId(id)
    }
}
