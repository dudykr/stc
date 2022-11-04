//! RNode: Node with node id

use std::sync::Arc;

pub use rnode_macros::define_rnode;
use serde::{Deserialize, Serialize};
use stc_visit::Visitable;
pub use stc_visit::{Fold, FoldWith, Visit, VisitMut, VisitMutWith, VisitWith};
use swc_common::{EqIgnoreSpan, TypeEq};

/// Alternative for span. This is much more reliable than span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NodeId(u32);

/// Always returns `true` as the struct is an alternative for span.
impl EqIgnoreSpan for NodeId {
    #[inline]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

/// Always returns `true` as id of a node is not type.
impl TypeEq for NodeId {
    #[inline]
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

impl NodeId {
    pub const fn is_invalid(self) -> bool {
        self.0 == 0
    }

    pub const fn invalid() -> Self {
        Self(0)
    }
}

impl Visitable for NodeId {}

/// Noop.
impl<V: ?Sized> VisitWith<V> for NodeId {
    fn visit_children_with(&self, _: &mut V) {}
}

/// Noop.
impl<V: ?Sized> VisitMutWith<V> for NodeId {
    fn visit_mut_children_with(&mut self, _: &mut V) {}
}

/// Noop.
impl<V: ?Sized> FoldWith<V> for NodeId {
    fn fold_children_with(self, _: &mut V) -> Self {
        self
    }
}

#[derive(Debug)]
pub struct NodeIdGenerator {
    /// If the stored value is zero, it's an invalid id generator.
    inner: u32,
}

impl Default for NodeIdGenerator {
    fn default() -> Self {
        Self { inner: 1 }
    }
}

impl NodeIdGenerator {
    pub fn invalid() -> Self {
        Self { inner: 0 }
    }

    pub fn make<R>(&mut self, orig: R::Orig) -> R
    where
        R: RNode,
    {
        R::from_orig(self, orig)
    }

    pub fn gen(&mut self) -> NodeId {
        let v = self.inner;
        if v == 0 {
            return NodeId::invalid();
        }
        self.inner += 1;
        NodeId(v)
    }
}

pub trait RNode: Clone + Send + Sync {
    type Orig;

    fn from_orig(id_generator: &mut NodeIdGenerator, orig: Self::Orig) -> Self;

    fn into_orig(self) -> Self::Orig;
}

impl<R> RNode for Box<R>
where
    R: RNode,
{
    type Orig = <R as RNode>::Orig;

    fn from_orig(g: &mut NodeIdGenerator, orig: Self::Orig) -> Self {
        Box::new(R::from_orig(g, orig))
    }

    fn into_orig(self) -> Self::Orig {
        (*self).into_orig()
    }
}

impl<R> RNode for Vec<R>
where
    R: RNode,
{
    type Orig = Vec<<R as RNode>::Orig>;

    fn from_orig(g: &mut NodeIdGenerator, orig: Self::Orig) -> Self {
        orig.into_iter().map(|orig| R::from_orig(g, orig)).collect()
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

    fn from_orig(g: &mut NodeIdGenerator, orig: Self::Orig) -> Self {
        match orig {
            Some(v) => Some(R::from_orig(g, v)),
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

/// Helper for derive macro. Do **not** implement this manually.
pub trait IntoRNode<R> {
    fn into_rnode(self, g: &mut NodeIdGenerator) -> R;
}

impl<R, N> IntoRNode<R> for N
where
    R: RNode<Orig = Self>,
{
    fn into_rnode(self, g: &mut NodeIdGenerator) -> R {
        RNode::from_orig(g, self)
    }
}

impl<T> RNode for Arc<T>
where
    T: RNode,
{
    type Orig = <T as RNode>::Orig;

    fn from_orig(id_generator: &mut NodeIdGenerator, orig: Self::Orig) -> Self {
        let t = T::from_orig(id_generator, orig);
        Arc::new(t)
    }

    fn into_orig(self) -> Self::Orig {
        match Arc::try_unwrap(self) {
            Ok(v) => v.into_orig(),
            Err(err) => (*err).clone().into_orig(),
        }
    }
}
