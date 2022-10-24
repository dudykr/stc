use std::cell::RefCell;

use crate::panic_context;

#[macro_export]
macro_rules! debug_ctx {
    ($s:expr) => {{
        if cfg!(debug_assertions) {
            Some($crate::error::span($s))
        } else {
            None
        }
    }};
}

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
pub fn span(_: String) -> () {}

/// Add a context to created errors and configures a context for panic.
#[cfg(debug_assertions)]
pub fn span(msg: String) -> ContextGuard {
    let msg = msg.to_string();
    with_ctx(|ctx| ctx.push(msg.clone()));
    ContextGuard {
        _panic_ctxt: panic_context::new(msg),
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
