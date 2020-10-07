# rnode

> RefCell everywhere!

This crate exists because it's impoisslbe to mutalbily borrow two elemements from vector.

This crate workarounds it by wrapping items in vector with `Rc<RefCell<T>>`.
After doing so, borrow checker will be happy with two mutable borrows.
