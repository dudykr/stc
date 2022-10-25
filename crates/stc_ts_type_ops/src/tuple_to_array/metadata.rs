use rnode::{VisitMut, VisitMutWith};
use stc_ts_ast_rnode::RIdent;
use stc_ts_types::Type;
use tracing::instrument;

/// TODO(kdy1): Optimize by visiting only tuple types.
#[instrument(skip(ty))]
pub fn prevent_tuple_to_array(ty: &mut Type) {
    ty.visit_mut_with(&mut PreventTupleToArray);
}

struct PreventTupleToArray;

impl VisitMut<Type> for PreventTupleToArray {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.nm();
        ty.metadata_mut().prevent_tuple_to_array = true;

        ty.visit_mut_children_with(self);
    }
}

/// Prevent interop with hygiene.
impl VisitMut<RIdent> for PreventTupleToArray {
    fn visit_mut(&mut self, _: &mut RIdent) {}
}
