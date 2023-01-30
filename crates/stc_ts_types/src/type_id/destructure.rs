use stc_visit::Visit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit, Default)]
pub struct DestructureId(pub u32);
