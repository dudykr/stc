use std::sync::atomic::{AtomicU32, Ordering::SeqCst};

use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, TypeEq};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct DestructurId(u32);

impl DestructurId {
    pub fn generate() -> Self {
        static GENERATOR: AtomicU32 = AtomicU32::new(1);

        let id = GENERATOR.fetch_add(1, SeqCst);

        DestructurId(id)
    }

    pub fn get(id: u32) -> Self {
        DestructurId(id)
    }

    pub fn extract(&self) -> u32 {
        self.0
    }
}

impl TypeEq for DestructurId {
    fn type_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl EqIgnoreSpan for DestructurId {
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self == other
    }
}
