use stc_visit::Visit;
use std::sync::atomic::{AtomicU64, Ordering::SeqCst};
use swc_common::{EqIgnoreSpan, TypeEq};

/// This is not `data` part of class and as a result `type_eq` and
/// `eq_ignore_span` always return `true`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct ClassId(u64);

impl ClassId {
    pub fn generate() -> Self {
        static GENERATOR: AtomicU64 = AtomicU64::new(0);

        let id = GENERATOR.fetch_add(1, SeqCst);

        ClassId(id)
    }
}

/// Always true.
impl TypeEq for ClassId {
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

/// Always true.
impl EqIgnoreSpan for ClassId {
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}
