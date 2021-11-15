use crate::{ArcCow, BoxedArcCow};
use stc_visit::{Fold, FoldWith};
use triomphe::{Arc, UniqueArc};

pub struct Freezer;

impl<T> Fold<ArcCow<T>> for Freezer
where
    T: FoldWith<Self>,
    ArcCow<T>: FoldWith<Self>,
{
    fn fold(&mut self, value: ArcCow<T>) -> ArcCow<T> {
        match value {
            ArcCow::Arc(_) => value,
            ArcCow::Raw(v) => {
                let v = v.fold_with(self);
                ArcCow::Arc(Arc::new(v))
            }
        }
    }
}

impl<T> Fold<BoxedArcCow<T>> for Freezer
where
    T: FoldWith<Self>,
    BoxedArcCow<T>: FoldWith<Self>,
{
    fn fold(&mut self, value: BoxedArcCow<T>) -> BoxedArcCow<T> {
        match value {
            BoxedArcCow::Arc(_) => value,
            BoxedArcCow::Boxed(v) => {
                let v = UniqueArc::into_inner(v);
                let v = v.fold_with(self);

                BoxedArcCow::Arc(Arc::new(v))
            }
        }
    }
}
