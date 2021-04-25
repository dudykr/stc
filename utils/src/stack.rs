use swc_common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackOverflowError {
    pub span: Span,
}

pub struct StartGuard;

impl Drop for StartGuard {
    fn drop(&mut self) {}
}

/// Start tracking for stack overflows. [track] will return error on `max`-th
/// call. (Currently it's noop)
pub fn start(_max: usize) -> StartGuard {
    StartGuard
}

pub struct TrackGuard;

impl Drop for TrackGuard {
    fn drop(&mut self) {}
}

/// Should be stored as a variable like `let _stack = stack::track(span);`.
pub fn track(_span: Span) -> Result<(), StackOverflowError> {
    Ok(())
}
