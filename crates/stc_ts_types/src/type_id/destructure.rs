use serde::{Deserialize, Serialize};
use stc_visit::Visit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit, Default, Serialize, Deserialize)]
pub struct DestructureId(pub u32);
impl DestructureId {
    pub fn next_id(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}
