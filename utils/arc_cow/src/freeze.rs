use crate::{ArcCow, BoxedArcCow};
use stc_visit::{VisitMut, VisitMutWith};
use swc_common::util::take::Take;
use triomphe::Arc;

pub struct Freezer;

impl<T> VisitMut<ArcCow<T>> for Freezer
where
    T: VisitMutWith<Self> + Take,
    ArcCow<T>: VisitMutWith<Self>,
{
    fn visit_mut(&mut self, n: &mut ArcCow<T>) {
        match n {
            ArcCow::Arc(_) => return,
            ArcCow::Raw(v) => {
                // Deep
                v.visit_mut_with(self);
                let v = v.take();

                *n = ArcCow::Arc(Arc::new(v))
            }
        }
    }
}

impl<T> VisitMut<BoxedArcCow<T>> for Freezer
where
    T: VisitMutWith<Self> + Take,
    BoxedArcCow<T>: VisitMutWith<Self>,
{
    fn visit_mut(&mut self, n: &mut BoxedArcCow<T>) {
        match n {
            BoxedArcCow::Arc(_) => return,
            BoxedArcCow::Boxed(v) => {
                (&mut **v).visit_mut_with(self);

                let v = v.take();

                *n = BoxedArcCow::Arc(Arc::new(v))
            }
        }
    }
}
