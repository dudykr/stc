use stc_utils::cache::Freeze;
use stc_visit::{VisitMut, VisitMutWith};
use swc_common::util::take::Take;
use triomphe::Arc;

use crate::{private::Freezed, ArcCow};

pub struct Freezer;

impl<T> VisitMut<ArcCow<T>> for Freezer
where
    T: Take + Freeze,
    ArcCow<T>: VisitMutWith<Self>,
{
    fn visit_mut(&mut self, n: &mut ArcCow<T>) {
        match n {
            ArcCow::Arc(_) => (),
            ArcCow::Owned(v) => {
                // Deep
                (**v).freeze();
                let v = (**v).take();

                *n = ArcCow::Arc(Freezed(Arc::new(v)))
            }
        }
    }
}
