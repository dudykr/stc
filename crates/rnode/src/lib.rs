//! > RefCell everywhere!
//!
//! This crate exists because it's impoisslbe to mutalbily borrow two elemements
//! from vector.
//!
//! This crate workarounds it by wrapping items in vector with `Rc<RefCell<T>>`.
//! After doing so, borrow checker will be happy with two mutable borrows.

pub use rnode_macros::define_rnode;
pub use stc_visit::{Fold, FoldWith, Visit, VisitMut, VisitMutWith, VisitWith};
use std::cell::RefCell;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;
use swc_common::EqIgnoreSpan;
use swc_common::Spanned;
use swc_common::TypeEq;

/// Send + Sync, under assumption user does not modify inner variable without
/// `&mut` or ownership. Otherwidse it is UB. All interior mutabilities are
/// removed on creation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Unsafe<R: RNode> {
    inner: R,
}

impl<R> EqIgnoreSpan for Unsafe<R>
where
    R: RNode + EqIgnoreSpan,
{
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self.inner.eq_ignore_span(&other.inner)
    }
}

impl<R> TypeEq for Unsafe<R>
where
    R: RNode + TypeEq,
{
    fn type_eq(&self, other: &Self) -> bool {
        self.inner.type_eq(&other.inner)
    }
}

unsafe impl<R: RNode> Send for Unsafe<R> {}

unsafe impl<R: RNode> Sync for Unsafe<R> {}

impl<R> Spanned for Unsafe<R>
where
    R: Spanned + RNode,
{
    fn span(&self) -> swc_common::Span {
        self.inner.span()
    }
}

impl<R> Unsafe<R>
where
    R: RNode,
{
    pub fn into_inner(self) -> R {
        self.inner
    }
}

impl<R, V> VisitWith<V> for Unsafe<R>
where
    R: RNode,
    V: ?Sized + Visit<R>,
{
    fn visit_children_with(&self, visitor: &mut V) {
        visitor.visit(&self.inner)
    }
}

impl<R, V> VisitMutWith<V> for Unsafe<R>
where
    R: RNode,
    V: ?Sized + VisitMut<R>,
{
    fn visit_mut_children_with(&mut self, v: &mut V) {
        v.visit_mut(&mut self.inner);
    }
}

impl<R, V> FoldWith<V> for Unsafe<R>
where
    R: RNode,
    V: ?Sized + Fold<R>,
{
    fn fold_children_with(mut self, v: &mut V) -> Self {
        self.inner = v.fold(self.inner);
        self
    }
}

impl<R> Deref for Unsafe<R>
where
    R: RNode,
{
    type Target = R;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<R> DerefMut for Unsafe<R>
where
    R: RNode,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub trait RNode: Clone {
    type Orig;

    fn from_orig(orig: Self::Orig) -> Self;

    fn into_orig(self) -> Self::Orig;

    fn wrap(self) -> Unsafe<Self> {
        Unsafe {
            inner: Self::from_orig(self.into_orig()),
        }
    }
}

impl<R> RNode for Box<R>
where
    R: RNode,
{
    type Orig = <R as RNode>::Orig;

    fn from_orig(orig: Self::Orig) -> Self {
        Box::new(R::from_orig(orig))
    }

    fn into_orig(self) -> Self::Orig {
        (*self).into_orig()
    }
}

impl<R> RNode for Rc<R>
where
    R: RNode,
{
    type Orig = <R as RNode>::Orig;

    fn from_orig(orig: Self::Orig) -> Self {
        Rc::new(R::from_orig(orig))
    }

    fn into_orig(self) -> Self::Orig {
        match Rc::try_unwrap(self) {
            Ok(v) => v.into_orig(),
            Err(rc) => (*rc).clone().into_orig(),
        }
    }
}

impl<R> RNode for RefCell<R>
where
    R: RNode,
{
    type Orig = <R as RNode>::Orig;

    fn from_orig(orig: Self::Orig) -> Self {
        RefCell::new(R::from_orig(orig))
    }

    fn into_orig(self) -> Self::Orig {
        R::into_orig(self.into_inner())
    }
}

impl<R> RNode for Vec<R>
where
    R: RNode,
{
    type Orig = Vec<<R as RNode>::Orig>;

    fn from_orig(orig: Self::Orig) -> Self {
        orig.into_iter().map(R::from_orig).collect()
    }

    fn into_orig(self) -> Self::Orig {
        self.into_iter().map(R::into_orig).collect()
    }
}

impl<R> RNode for Option<R>
where
    R: RNode,
{
    type Orig = Option<R::Orig>;

    fn from_orig(orig: Self::Orig) -> Self {
        match orig {
            Some(v) => Some(R::from_orig(v)),
            None => None,
        }
    }

    fn into_orig(self) -> Self::Orig {
        match self {
            Some(v) => Some(v.into_orig()),
            None => None,
        }
    }
}

/// Helper for derive macro. Do **not** implement this manullay.
pub trait IntoRNode<R> {
    fn into_rnode(self) -> R;
}

impl<R, N> IntoRNode<R> for N
where
    R: RNode<Orig = Self>,
{
    fn into_rnode(self) -> R {
        RNode::from_orig(self)
    }
}

pub trait RNodeOf<R> {}

impl<R, T> RNodeOf<T> for R where Self: RNode<Orig = T> {}
