use std::cell::RefCell;

/// Declares an error context. This contexts are added to errors reported while
/// the returned RAII guard is alive.
#[macro_export]
macro_rules! ctx {
    ($s:expr) => {{
        if cfg!(debug_assertions) {
            Some($crate::context::new($s))
        } else {
            None
        }
    }};
}

#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn new(_: String) -> () {}

#[cfg(debug_assertions)]
pub fn new(context: String) -> ErrorContextGuard {
    with_ctx(|ctx| ctx.push(context));
    ErrorContextGuard { _priv: () }
}

#[must_use]
#[cfg(debug_assertions)]
pub struct ErrorContextGuard {
    _priv: (),
}

#[cfg(debug_assertions)]
impl Drop for ErrorContextGuard {
    fn drop(&mut self) {
        with_ctx(|ctx| assert!(ctx.pop().is_some()))
    }
}

#[cfg(debug_assertions)]
fn with_ctx(f: impl FnOnce(&mut Vec<String>)) {
    thread_local! {
        static CTX: RefCell<Vec<String>> = RefCell::new(Vec::new());
    }
    CTX.with(|ctx| f(&mut *ctx.borrow_mut()))
}
