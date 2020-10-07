use std::cell::RefCell;
use std::rc::Rc;

pub use rnode_macros::define_rnode;

pub trait RNode: Clone {
    type Orig;

    fn from_orig(orig: Self::Orig) -> Self;

    fn into_orig(self) -> Self::Orig;
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
