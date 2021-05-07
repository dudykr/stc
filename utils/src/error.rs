use crate::panic_context;
use std::cell::RefCell;
use std::fmt::Display;

#[cfg(debug_assertions)]
pub struct ContextGuard {
    _panic_ctxt: panic_context::PanicContext,
}

#[cfg(debug_assertions)]
impl Drop for ContextGuard {
    fn drop(&mut self) {
        with_ctx(|ctx| assert!(ctx.pop().is_some()))
    }
}

#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn context(_: impl Display) -> () {}

/// Add a context to created errors and configures a context for panic.
#[cfg(debug_assertions)]
pub fn context(msg: impl Display) -> ContextGuard {
    let msg = msg.to_string();
    with_ctx(|ctx| ctx.push(msg.clone()));
    ContextGuard {
        _panic_ctxt: panic_context::enter(msg),
    }
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub fn current_context() -> Vec<String> {
    #[cfg(not(debug_assertions))]
    return vec![];

    #[cfg(debug_assertions)]
    with_ctx(|v| v.clone())
}

#[cfg(debug_assertions)]
fn with_ctx<T>(f: impl FnOnce(&mut Vec<String>) -> T) -> T {
    thread_local! {
        static CTX: RefCell<Vec<String>> = RefCell::new(Vec::new());
    }
    CTX.with(|ctx| f(&mut *ctx.borrow_mut()))
}
