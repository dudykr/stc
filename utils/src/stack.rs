use std::cell::RefCell;
use std::mem::replace;
use swc_common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackOverflowError {
    pub span: Span,
}

pub struct StartGuard {
    /// Previous value.
    prev: usize,
}

impl Drop for StartGuard {
    fn drop(&mut self) {
        with_ctx(|v| *v = self.prev)
    }
}

/// Start tracking for stack overflows. [track] will return error on `max`-th
/// call. (Currently it's noop)
pub fn start(max: usize) -> StartGuard {
    let prev = with_ctx(|v| replace(v, max));
    StartGuard { prev }
}

pub struct TrackGuard {
    _priv: (),
}

impl Drop for TrackGuard {
    fn drop(&mut self) {
        with_ctx(|v| *v += 1)
    }
}

/// Should be stored as a variable like `let _stack = stack::track(span);`.
pub fn track(span: Span) -> Result<TrackGuard, StackOverflowError> {
    with_ctx(|v| {
        if *v == 0 {
            return Err(StackOverflowError { span });
        }

        *v -= 1;

        Ok(TrackGuard { _priv: () })
    })
}

/// closure argument: Stack left
fn with_ctx<T>(f: impl FnOnce(&mut usize) -> T) -> T {
    thread_local! {
        static CTX: RefCell<usize> = RefCell::new(0);
    }
    CTX.with(|ctx| f(&mut *ctx.borrow_mut()))
}
