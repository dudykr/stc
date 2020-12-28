use std::cell::RefCell;
use std::rc::Rc;
use swc_common::Span;

pub trait Fold<T> {
    fn fold(&mut self, value: T) -> T;
}

pub trait FoldWith<V: ?Sized>: Sized {
    fn fold_with(self, visitor: &mut V) -> Self
    where
        V: Fold<Self>,
    {
        visitor.fold(self)
    }

    fn fold_children_with(self, visitor: &mut V) -> Self;
}

impl<T, V> Fold<T> for V
where
    T: FoldWith<Self>,
    V: ?Sized,
{
    default fn fold(&mut self, val: T) -> T {
        val.fold_children_with(self)
    }
}

impl<T, V> FoldWith<V> for Box<T>
where
    V: ?Sized + Fold<T>,
{
    fn fold_children_with(self, v: &mut V) -> Self {
        Box::new(v.fold(*self))
    }
}

impl<T, V> FoldWith<V> for RefCell<T>
where
    V: ?Sized + Fold<T>,
{
    fn fold_children_with(self, v: &mut V) -> Self {
        RefCell::new(v.fold(self.into_inner()))
    }
}

impl<T, V> Fold<T> for &'_ mut V
where
    T: FoldWith<Self>,
    V: ?Sized + Fold<T>,
{
    fn fold(&mut self, value: T) -> T {
        (**self).fold(value)
    }
}

impl<V> FoldWith<V> for Span
where
    V: ?Sized,
{
    /// Noop
    #[inline]
    fn fold_children_with(self, _: &mut V) -> Self {
        self
    }
}

impl<T, V> FoldWith<V> for Vec<T>
where
    V: ?Sized + Fold<T>,
{
    fn fold_children_with(self, visitor: &mut V) -> Self {
        self.into_iter().map(|node| visitor.fold(node)).collect()
    }
}

impl<T, V> FoldWith<V> for Option<T>
where
    V: ?Sized + Fold<T>,
{
    fn fold_children_with(self, visitor: &mut V) -> Self {
        match self {
            Some(value) => Some(visitor.fold(value)),
            None => None,
        }
    }
}

impl<T, V> FoldWith<V> for Rc<T>
where
    V: ?Sized + Fold<T>,
{
    /// Noop if it's stored in somewhere. TODO(kdy1): panic?
    #[inline]
    fn fold_children_with(self, visitor: &mut V) -> Self {
        match Rc::try_unwrap(self) {
            Ok(value) => Rc::new(visitor.fold(value)),
            Err(value) => value,
        }
    }
}
