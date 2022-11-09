#![allow(clippy::partialeq_ne_impl)]
#![allow(incomplete_features)]
#![feature(specialization)]

use triomphe::Arc;

pub use self::boxed::BoxedArcCow;
use crate::freeze::Freezer;

#[macro_use]
mod macros;
mod boxed;
pub mod freeze;

pub enum ArcCow<T>
where
    T: 'static,
{
    Arc(Arc<T>),
    Raw(T),
}

impl_traits!(ArcCow, Raw);

impl<T> ArcCow<T> {
    /// This is deep freeze, but doesn't work if `self <- Freezed <- NonFreezed`
    /// exists.
    #[inline]
    pub fn freeze(&mut self)
    where
        Self: VisitMutWith<Freezer>,
    {
        self.visit_mut_with(&mut Freezer);
    }
}

impl<T> From<T> for ArcCow<T> {
    #[inline]
    fn from(data: T) -> Self {
        ArcCow::Raw(data)
    }
}

impl<T> ArcCow<T>
where
    T: Clone,
{
    #[inline]
    pub fn into_inner(self) -> T {
        match self {
            Self::Arc(v) => match Arc::try_unwrap(v) {
                Ok(v) => v,
                Err(v) => (*v).clone(),
            },
            Self::Raw(v) => v,
        }
    }
}

pub fn _assert_trait_impls() {
    fn _assert<P>()
    where
        P: Default + std::fmt::Debug + Clone + std::hash::Hash + PartialEq + Eq + Ord,
    {
    }

    _assert::<ArcCow<String>>();
    _assert::<BoxedArcCow<String>>();
}
