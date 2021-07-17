/// Ported from `Ternary` of `tsc`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TscTernary {
    False,
    Unknown,
    Maybe,
    True,
}
