#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(specialization)]

use std::{fmt::Debug, hash::Hash, ops::Deref};

use serde::{de::DeserializeOwned, Deserialize, Serialize};
use stc_visit::{FoldWith, VisitMutWith, VisitWith, Visitable};
use swc_common::{util::take::Take, EqIgnoreSpan, Spanned, TypeEq};
use triomphe::Arc;

use crate::freeze::Freezer;

pub mod freeze;

pub enum ArcCow<T>
where
    T: 'static + Take,
{
    Arc(Arc<T>),
    Owned(Box<T>),
}

impl<T> Spanned for ArcCow<T>
where
    T: Spanned + Take,
{
    #[inline]
    fn span(&self) -> swc_common::Span {
        (**self).span()
    }
}

impl<T> Deref for ArcCow<T>
where
    T: Take,
{
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self {
            ArcCow::Arc(v) => v,
            ArcCow::Owned(v) => v,
        }
    }
}
impl<T> PartialEq<T> for ArcCow<T>
where
    T: PartialEq + Take,
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
    T: PartialEq + Take,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if let (ArcCow::Arc(l), ArcCow::Arc(r)) = (self, other) {
            if Arc::ptr_eq(l, r) {
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
impl<T> Eq for ArcCow<T> where T: Eq + Take {}

impl<T> Default for ArcCow<T>
where
    T: Default + Take,
{
    #[inline]
    fn default() -> Self {
        ArcCow::from(T::default())
    }
}
impl<T> Clone for ArcCow<T>
where
    T: Clone + Take,
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
    T: PartialOrd + Take,
{
    #[inline]
    fn partial_cmp(&self, r: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**r)
    }
}
impl<T> Ord for ArcCow<T>
where
    T: Ord + Take,
{
    #[inline]
    fn cmp(&self, r: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**r)
    }
}
impl<T> Hash for ArcCow<T>
where
    T: Hash + Take,
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
    T: Debug + Take,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArcCow::Arc(..) => write!(f, "arc: ")?,
            ArcCow::Owned(..) => write!(f, "raw: ")?,
        }
        Debug::fmt(&**self, f)
    }
}
impl<T> From<Arc<T>> for ArcCow<T>
where
    T: Take,
{
    #[inline]
    fn from(arc: Arc<T>) -> Self {
        Self::Arc(arc)
    }
}
impl<T> TypeEq for ArcCow<T>
where
    T: TypeEq + Take,
{
    #[inline]
    fn type_eq(&self, other: &Self) -> bool {
        if let (ArcCow::Arc(l), ArcCow::Arc(r)) = (self, other) {
            if Arc::ptr_eq(l, r) {
                return true;
            }
        }
        (**self).type_eq(&**other)
    }
}
impl<T> EqIgnoreSpan for ArcCow<T>
where
    T: EqIgnoreSpan + Take,
{
    #[inline]
    fn eq_ignore_span(&self, other: &Self) -> bool {
        (**self).eq_ignore_span(&**other)
    }
}
impl<T> Visitable for ArcCow<T> where T: Take {}

impl<T, V> VisitWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: VisitWith<V> + Take,
{
    #[inline]
    fn visit_children_with(&self, v: &mut V) {
        (**self).visit_children_with(v)
    }
}
impl<T, V> VisitMutWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: Clone + VisitMutWith<V> + Take,
{
    #[inline]
    fn visit_mut_children_with(&mut self, v: &mut V) {
        self.normalize_mut().visit_mut_children_with(v)
    }
}
impl<T, V> FoldWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: Clone + FoldWith<V> + Take,
{
    #[inline]
    fn fold_children_with(self, v: &mut V) -> Self {
        Self::from(self.into_inner().fold_children_with(v))
    }
}
impl<T> ArcCow<T>
where
    T: Clone + Take,
{
    /// Makes `self` [ArcCow::Raw]
    #[inline]
    pub fn normalize_mut(&mut self) -> &mut T {
        match self {
            ArcCow::Arc(v) => {
                let data = Arc::make_unique(v);
                let data = (**data).take();

                *self = ArcCow::Owned(box data);
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
    T: Take,
{
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

impl<T> From<T> for ArcCow<T>
where
    T: Take,
{
    #[inline(always)]
    fn from(data: T) -> Self {
        ArcCow::Owned(box data)
    }
}

impl<T> From<Box<T>> for ArcCow<T>
where
    T: Take,
{
    #[inline(always)]
    fn from(data: Box<T>) -> Self {
        ArcCow::Owned(data)
    }
}

impl<T> ArcCow<T>
where
    T: Clone + Take,
{
    #[inline]
    pub fn into_inner(self) -> T {
        match self {
            Self::Arc(v) => match Arc::try_unwrap(v) {
                Ok(v) => v,
                Err(v) => (*v).clone(),
            },
            Self::Owned(v) => *v,
        }
    }
}

impl<T> Serialize for ArcCow<T>
where
    T: Serialize + Take,
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
    T: DeserializeOwned + Take,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let t: T = Deserialize::deserialize(deserializer)?;

        Ok(ArcCow::from(t))
    }
}
