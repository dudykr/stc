use stc_visit::{VisitMut, VisitMutWith};
use swc_common::util::take::Take;
use triomphe::Arc;

use crate::ArcCow;

pub struct Freezer;

impl<T> VisitMut<ArcCow<T>> for Freezer
where
    T: VisitMutWith<Self> + Take,
    ArcCow<T>: VisitMutWith<Self>,
{
    fn visit_mut(&mut self, n: &mut ArcCow<T>) {
        match n {
            ArcCow::Arc(_) => (),
            ArcCow::Owned(v) => {
                // Deep
                v.visit_mut_with(self);
                let v = *v.take();

                *n = ArcCow::Arc(Arc::new(v))
            }
        }
    }
}
