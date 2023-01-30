use std::{cell::Cell, ops::Add};

use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, TypeEq};

use crate::std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct DestructureId(u32);

impl DestructureId {
    thread_local! {
        static LAST_ID: Rc<Cell<u32>> = Rc::new(Cell::new(0));
    }

    pub fn generate() -> Self {
        let id = DestructureId::LAST_ID.with(|id| {
            let last_id = id.get().add(1);
            id.set(last_id);
            last_id
        });

        DestructureId(id)
    }

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
