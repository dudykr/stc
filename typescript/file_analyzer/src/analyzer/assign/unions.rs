use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    ValidationResult,
};
use stc_ts_types::{Tuple, Union};

impl Analyzer<'_, '_> {
    /// # Cases
    ///
    /// Cases handled by this methods are
    ///
    ///  - lhs = `(["a", number] | ["b", number] | ["c", string]);`
    ///  - rhs = `[("b" | "a"), 1];`
    pub(super) fn assign_tuple_to_union(
        &mut self,
        data: &mut AssignData,
        l: &Union,
        r: &Tuple,
        opts: AssignOpts,
    ) -> Option<ValidationResult<()>> {
        None
    }
}
