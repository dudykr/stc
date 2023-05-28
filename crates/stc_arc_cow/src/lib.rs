#![allow(incomplete_features)]
#![feature(specialization)]

use std::{fmt::Debug, hash::Hash, ops::Deref};

use serde::{de::DeserializeOwned, Deserialize, Serialize};
use stc_utils::cache::{AssertCloneCheap, Freeze};
use stc_visit::{FoldWith, VisitMutWith, VisitWith, Visitable};
use swc_common::{util::take::Take, EqIgnoreSpan, Spanned, TypeEq};
use triomphe::Arc;

pub use crate::private::Freezed;

pub mod freeze;
mod private;

pub enum ArcCow<T>
where
    T: 'static + Take + Freeze,
{
    Arc(Freezed<T>),
    Owned(Box<T>),
}

impl<T> Spanned for ArcCow<T>
where
    T: Spanned + Take + Freeze,
{
    #[inline]
    fn span(&self) -> swc_common::Span {
        (**self).span()
    }
}

impl<T> Deref for ArcCow<T>
where
    T: Take + Freeze,
{
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self {
            ArcCow::Arc(v) => &v.0,
            ArcCow::Owned(v) => v,
        }
    }
}
impl<T> PartialEq<T> for ArcCow<T>
where
    T: PartialEq + Take + Freeze,
{
    #[inline]
    fn eq(&self, other: &T) -> bool {
        (**self).eq(other)
    }

    #[inline]
    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, other: &T) -> bool {
        (**self).ne(other)
    }
}

impl<T> PartialEq for ArcCow<T>
where
    T: PartialEq + Take + Freeze,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if let (ArcCow::Arc(l), ArcCow::Arc(r)) = (self, other) {
            if Arc::ptr_eq(&l.0, &r.0) {
                return true;
            }
        }
        (**self).eq(&**other)
    }

    #[inline]
    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, other: &Self) -> bool {
        (**self).ne(&**other)
    }
}
impl<T> Eq for ArcCow<T> where T: Eq + Take + Freeze {}

impl<T> Default for ArcCow<T>
where
    T: Default + Take + Freeze,
{
    #[inline]
    fn default() -> Self {
        ArcCow::from(T::default())
    }
}
impl<T> Clone for ArcCow<T>
where
    T: Clone + Take + Freeze,
{
    #[inline]
    fn clone(&self) -> Self {
        match self {
            ArcCow::Arc(v) => ArcCow::Arc(v.clone()),
            ArcCow::Owned(v) => Self::from((*v).clone()),
        }
    }
}
impl<T> PartialOrd for ArcCow<T>
where
    T: PartialOrd + Take + Freeze,
{
    #[inline]
    fn partial_cmp(&self, r: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**r)
    }
}
impl<T> Ord for ArcCow<T>
where
    T: Ord + Take + Freeze,
{
    #[inline]
    fn cmp(&self, r: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**r)
    }
}
impl<T> Hash for ArcCow<T>
where
    T: Hash + Take + Freeze,
{
    #[inline]
    fn hash<H>(&self, hasher: &mut H)
    where
        H: std::hash::Hasher,
    {
        (**self).hash(hasher)
    }
}
impl<T> Debug for ArcCow<T>
where
    T: Debug + Take + Freeze,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArcCow::Arc(..) => write!(f, "arc: ")?,
            ArcCow::Owned(..) => write!(f, "raw: ")?,
        }
        Debug::fmt(&**self, f)
    }
}

impl<T> TypeEq for ArcCow<T>
where
    T: TypeEq + Take + Freeze,
{
    #[inline]
    fn type_eq(&self, other: &Self) -> bool {
        if let (ArcCow::Arc(l), ArcCow::Arc(r)) = (self, other) {
            if Arc::ptr_eq(&l.0, &r.0) {
                return true;
            }
        }
        (**self).type_eq(&**other)
    }
}
impl<T> EqIgnoreSpan for ArcCow<T>
where
    T: EqIgnoreSpan + Take + Freeze,
{
    #[inline]
    fn eq_ignore_span(&self, other: &Self) -> bool {
        (**self).eq_ignore_span(&**other)
    }
}
impl<T> Visitable for ArcCow<T> where T: Take + Freeze {}

impl<T, V> VisitWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: VisitWith<V> + Take + Freeze,
{
    #[inline]
    fn visit_children_with(&self, v: &mut V) {
        (**self).visit_children_with(v)
    }
}
impl<T, V> VisitMutWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: Clone + VisitMutWith<V> + Take + Freeze,
{
    #[inline]
    fn visit_mut_children_with(&mut self, v: &mut V) {
        self.normalize_mut().visit_mut_children_with(v)
    }
}
impl<T, V> FoldWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: Clone + FoldWith<V> + Take + Freeze,
{
    #[inline]
    fn fold_children_with(self, v: &mut V) -> Self {
        Self::from(self.into_inner().fold_children_with(v))
    }
}
impl<T> ArcCow<T>
where
    T: Clone + Take + Freeze,
{
    #[inline(always)]
    pub fn normalize(&self) -> &T {
        self
    }

    /// Makes `self` [ArcCow::Raw]
    #[inline]
    pub fn normalize_mut(&mut self) -> &mut T {
        match self {
            ArcCow::Arc(v) => {
                let data = Arc::make_unique(&mut v.0);
                let data = (**data).take();

                *self = ArcCow::Owned(Box::new(data));
                match self {
                    ArcCow::Owned(v) => &mut *v,
                    _ => unreachable!(),
                }
            }
            ArcCow::Owned(v) => &mut *v,
        }
    }
}

impl<T> ArcCow<T>
where
    T: Take + Clone + Freeze,
{
    #[inline(always)]
    pub fn freezed(mut self) -> Self {
        self.freeze();
        self
    }

    pub fn new_freezed(mut data: T) -> Self {
        data.freeze();
        Self::Arc(Freezed(Arc::new(data)))
    }

    /// This panics if `self` is not cheap to clone.
    #[inline(always)]
    pub fn cheap_clone(&self) -> Self {
        match self {
            ArcCow::Arc(c) => ArcCow::Arc(c.clone()),
            ArcCow::Owned(_) => unreachable!("this is not cheap to clone"),
        }
    }
}

impl<T> From<Arc<T>> for ArcCow<T>
where
    T: Take + Freeze,
{
    fn from(arc: Arc<T>) -> Self {
        (*arc).assert_clone_cheap();

        ArcCow::Arc(Freezed(arc))
    }
}

impl<T> From<T> for ArcCow<T>
where
    T: Take + Freeze,
{
    #[inline(always)]
    fn from(data: T) -> Self {
        ArcCow::Owned(Box::new(data))
    }
}

impl<T> From<Box<T>> for ArcCow<T>
where
    T: Take + Freeze,
{
    #[inline(always)]
    fn from(data: Box<T>) -> Self {
        ArcCow::Owned(data)
    }
}

impl<T> ArcCow<T>
where
    T: Clone + Take + Freeze,
{
    #[inline]
    pub fn into_inner(self) -> T {
        match self {
            Self::Arc(v) => match Arc::try_unwrap(v.0) {
                Ok(v) => v,
                Err(v) => (*v).clone(),
            },
            Self::Owned(v) => *v,
        }
    }
}

impl<T> Serialize for ArcCow<T>
where
    T: Serialize + Take + Freeze,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (**self).serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for ArcCow<T>
where
    T: DeserializeOwned + Take + Freeze,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let t: T = Deserialize::deserialize(deserializer)?;

        Ok(ArcCow::from(t))
    }
}

impl<T> Freeze for ArcCow<T>
where
    T: Take + Clone + Freeze,
{
    fn is_clone_cheap(&self) -> bool {
        match self {
            ArcCow::Arc(..) => true,
            ArcCow::Owned(..) => false,
        }
    }

    fn freeze(&mut self) {
        match self {
            ArcCow::Arc(..) => {}
            ArcCow::Owned(data) => {
                (**data).freeze();

                *self = ArcCow::Arc(Freezed(Arc::new((**data).take())));
            }
        }
    }
}
