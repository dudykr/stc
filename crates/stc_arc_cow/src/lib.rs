#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(specialization)]

use std::ops::Deref;

use serde::{de::DeserializeOwned, Deserialize, Serialize};
use stc_visit::{FoldWith, VisitMutWith, VisitWith, Visitable};
use swc_common::{EqIgnoreSpan, Spanned, TypeEq};
use triomphe::Arc;

use crate::freeze::Freezer;

pub mod freeze;

pub enum ArcCow<T>
where
    T: 'static,
{
    Arc(Arc<T>),
    Owned(Box<T>),
}

impl<T> Spanned for ArcCow<T>
where
    T: Spanned,
{
    #[inline]
    fn span(&self) -> swc_common::Span {
        (**self).span()
    }
}

impl<T> Deref for ArcCow<T> {
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
    T: PartialEq,
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
    T: PartialEq,
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
impl<T> Eq for ArcCow<T> where T: Eq {}

impl<T> Default for ArcCow<T>
where
    T: Default,
{
    #[inline]
    fn default() -> Self {
        ArcCow::from(T::default())
    }
}
impl<T> Clone for ArcCow<T>
where
    T: Clone,
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
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, r: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**r)
    }
}
impl<T> Ord for ArcCow<T>
where
    T: Ord,
{
    #[inline]
    fn cmp(&self, r: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**r)
    }
}
impl<T> std::hash::Hash for ArcCow<T>
where
    T: std::hash::Hash,
{
    #[inline]
    fn hash<H>(&self, hasher: &mut H)
    where
        H: std::hash::Hasher,
    {
        (**self).hash(hasher)
    }
}
impl<T> std::fmt::Debug for ArcCow<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArcCow::Arc(..) => write!(f, "arc: ")?,
            ArcCow::Owned(..) => write!(f, "raw: ")?,
        }
        std::fmt::Debug::fmt(&**self, f)
    }
}
impl<T> From<Arc<T>> for ArcCow<T> {
    #[inline]
    fn from(arc: Arc<T>) -> Self {
        Self::Arc(arc)
    }
}
impl<T> TypeEq for ArcCow<T>
where
    T: TypeEq,
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
    T: EqIgnoreSpan,
{
    #[inline]
    fn eq_ignore_span(&self, other: &Self) -> bool {
        (**self).eq_ignore_span(&**other)
    }
}
impl<T> Visitable for ArcCow<T> {}

impl<T, V> VisitWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: VisitWith<V>,
{
    #[inline]
    fn visit_children_with(&self, v: &mut V) {
        (**self).visit_children_with(v)
    }
}
impl<T, V> VisitMutWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: Clone + VisitMutWith<V>,
{
    #[inline]
    fn visit_mut_children_with(&mut self, v: &mut V) {
        self.normalize_mut().visit_mut_children_with(v)
    }
}
impl<T, V> FoldWith<V> for ArcCow<T>
where
    V: ?Sized,
    T: Clone + FoldWith<V>,
{
    #[inline]
    fn fold_children_with(self, v: &mut V) -> Self {
        Self::from(self.into_inner().fold_children_with(v))
    }
}
impl<T> ArcCow<T>
where
    T: Clone,
{
    /// Makes `self` [ArcCow::Raw]
    #[inline]
    pub fn normalize_mut(&mut self) -> &mut T {
        match self {
            ArcCow::Arc(v) => Arc::make_mut(v),
            ArcCow::Owned(v) => &mut *v,
        }
    }
}

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
    #[inline(always)]
    fn from(data: T) -> Self {
        ArcCow::Owned(box data)
    }
}

impl<T> From<Box<T>> for ArcCow<T> {
    #[inline(always)]
    fn from(data: Box<T>) -> Self {
        ArcCow::Owned(data)
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
            Self::Owned(v) => *v,
        }
    }
}

pub fn _assert_trait_impl() {
    fn _assert<P>()
    where
        P: Default + std::fmt::Debug + Clone + std::hash::Hash + PartialEq + Eq + Ord,
    {
    }

    _assert::<ArcCow<String>>();
}

impl<T> Serialize for ArcCow<T>
where
    T: Serialize,
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
    T: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let t: T = Deserialize::deserialize(deserializer)?;

        Ok(ArcCow::from(t))
    }
}
