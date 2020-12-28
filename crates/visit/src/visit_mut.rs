use std::cell::RefCell;
use std::rc::Rc;
use swc_common::Span;

pub trait VisitMut<T> {
    fn visit_mut(&mut self, value: &mut T);
}

pub trait VisitMutWith<V: ?Sized>: Sized {
    fn visit_mut_with(&mut self, visitor: &mut V)
    where
        V: VisitMut<Self>,
    {
        visitor.visit_mut(self)
    }

    fn visit_mut_children_with(&mut self, visitor: &mut V);
}

impl<T, V: ?Sized> VisitMut<T> for V
where
    T: VisitMutWith<Self>,
{
    default fn visit_mut(&mut self, val: &mut T) {
        val.visit_mut_children_with(self)
    }
}

impl<T, V> VisitMutWith<V> for Box<T>
where
    V: ?Sized + VisitMut<T>,
{
    fn visit_mut_children_with(&mut self, v: &mut V) {
        v.visit_mut(self)
    }
}

impl<T, V> VisitMutWith<V> for RefCell<T>
where
    V: ?Sized + VisitMut<T>,
{
    fn visit_mut_children_with(&mut self, v: &mut V) {
        v.visit_mut(&mut *self.get_mut())
    }
}

impl<T, V> VisitMut<T> for &'_ mut V
where
    T: VisitMutWith<Self>,
    V: ?Sized + VisitMut<T>,
{
    fn visit_mut(&mut self, value: &mut T) {
        (**self).visit_mut(value)
    }
}

impl<V> VisitMutWith<V> for Span
where
    V: ?Sized,
{
    /// Noop
    #[inline]
    fn visit_mut_children_with(&mut self, _: &mut V) {}
}

impl<T, V> VisitMutWith<V> for Vec<T>
where
    V: ?Sized + VisitMut<T>,
{
    fn visit_mut_children_with(&mut self, visitor: &mut V) {
        self.iter_mut().for_each(|value| visitor.visit_mut(value))
    }
}

impl<T, V> VisitMutWith<V> for Option<T>
where
    V: ?Sized + VisitMut<T>,
{
    fn visit_mut_children_with(&mut self, visitor: &mut V) {
        match self {
            Some(value) => visitor.visit_mut(value),
            None => {}
        }
    }
}

impl<T, V> VisitMutWith<V> for Rc<RefCell<T>>
where
    V: ?Sized + VisitMut<T>,
{
    fn visit_mut_children_with(&mut self, visitor: &mut V) {
        visitor.visit_mut(&mut *self.borrow_mut())
    }
}
