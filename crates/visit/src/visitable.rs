use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use swc_common::Span;

/// Marker to prevent mistakes.
pub trait Visitable {}

impl<T> Visitable for Vec<T> where T: Visitable {}

impl<T> Visitable for [T] where T: Visitable {}

impl<T> Visitable for Box<T> where T: ?Sized + Visitable {}

impl<T> Visitable for &'_ T where T: ?Sized + Visitable {}

impl<T> Visitable for &'_ mut T where T: ?Sized + Visitable {}

impl<T> Visitable for Rc<T> where T: Visitable {}

impl<T> Visitable for Arc<T> where T: Visitable {}

impl<T> Visitable for Option<T> where T: Visitable {}

impl<T> Visitable for RefCell<T> where T: Visitable {}

impl Visitable for Span {}
