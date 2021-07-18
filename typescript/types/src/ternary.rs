/// Ported from `Ternary` of `tsc`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TscTernary {
    False = 0,
    Unknown = 1,
    Maybe = 3,
    True = -1,
}

impl TscTernary {
    pub fn as_bool(self) -> bool {
        self != Self::False
    }
}
