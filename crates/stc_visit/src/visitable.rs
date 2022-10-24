use std::{cell::RefCell, rc::Rc, sync::Arc};

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
