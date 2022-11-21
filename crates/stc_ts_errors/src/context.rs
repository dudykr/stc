use std::cell::RefCell;

/// Declares an error context. This contexts are added to errors reported while
/// the returned RAII guard is alive.
#[macro_export]
macro_rules! ctx {
    ($($t:tt)*) => {{
        if cfg!(debug_assertions) {
            Some($crate::context::new(|| {
                ($($t)*).to_string()
            }))
        } else {
            None
        }
    }};
}

#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn new(_: impl Fn() -> String) -> () {}

#[cfg(debug_assertions)]
pub fn new(context: impl Fn() -> String) -> ErrorContextGuard {
    use std::mem::transmute;
    let context = box context as Ctx<'_>;
    let tr = unsafe {
        // Safety: We are creating a scoped context that is only valid while the guard
        // is alive.
        transmute(context)
    };

    with_ctx(|ctx| ctx.push(tr));
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

type Ctx<'a> = Box<dyn 'a + Fn() -> String>;

#[cfg(debug_assertions)]
pub(crate) fn with_ctx<R>(f: impl FnOnce(&mut Vec<Ctx>) -> R) -> R {
    thread_local! {
        static CTX: RefCell<Vec<Ctx<'static>>> = RefCell::new(Vec::new());
    }
    CTX.with(|ctx| f(&mut *ctx.borrow_mut()))
}
