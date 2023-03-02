use stc_visit::{VisitMut, VisitMutWith};
use swc_common::util::take::Take;
use triomphe::Arc;

use crate::{private::PrivateArc, ArcCow};

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
                (**v).visit_mut_with(self);
                let v = (**v).take();

                *n = ArcCow::Arc(PrivateArc(Arc::new(v)))
            }
        }
    }
}

pub trait AssertCloneCheap {
    /// Assert that `self` is cheap to clone. This noop on production build.
    fn assert_clone_cheap(&self);
}

impl<T> AssertCloneCheap for Option<T>
where
    T: AssertCloneCheap,
{
    #[inline]
    fn assert_clone_cheap(&self) {
        #[cfg(debug_assertions)]
        if let Some(v) = self {
            v.assert_clone_cheap()
        }
    }
}

impl<T> AssertCloneCheap for &'_ T
where
    T: ?Sized + AssertCloneCheap,
{
    #[inline]
    fn assert_clone_cheap(&self) {
        #[cfg(debug_assertions)]
        (**self).assert_clone_cheap()
    }
}

impl<T> AssertCloneCheap for Box<T>
where
    T: ?Sized + AssertCloneCheap,
{
    #[inline]
    fn assert_clone_cheap(&self) {
        #[cfg(debug_assertions)]
        (**self).assert_clone_cheap()
    }
}

impl<T> AssertCloneCheap for Vec<T>
where
    T: AssertCloneCheap,
{
    #[inline]
    fn assert_clone_cheap(&self) {
        #[cfg(debug_assertions)]
        for v in self {
            v.assert_clone_cheap()
        }
    }
}

impl<T> AssertCloneCheap for [T]
where
    T: AssertCloneCheap,
{
    #[inline]
    fn assert_clone_cheap(&self) {
        #[cfg(debug_assertions)]
        for v in self {
            v.assert_clone_cheap()
        }
    }
}
