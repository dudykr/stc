use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, TypeEq};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct DestructureId(u32);

impl DestructureId {
    pub fn get(id: u32) -> Self {
        DestructureId(id)
    }

    pub fn extract(&self) -> u32 {
        self.0
    }
}

impl TypeEq for DestructureId {
    fn type_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl EqIgnoreSpan for DestructureId {
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self == other
    }
}
