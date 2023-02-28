use rnode::{VisitMut, VisitMutWith};
use stc_ts_types::Ref;

pub struct ExpansionPreventer {
    pub is_for_ignoring: bool,
}

impl VisitMut<Ref> for ExpansionPreventer {
    fn visit_mut(&mut self, ty: &mut Ref) {
        ty.visit_mut_children_with(self);

        if self.is_for_ignoring {
            ty.metadata.ignore_no_expand = true;
        } else {
            ty.metadata.no_expand = true;
        }
    }
}
