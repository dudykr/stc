/// Creates an entered span for `tracing` on debug builds.
/// On release builds, this is a no-op.
#[macro_export]
macro_rules! debug_ctx {
    ($s:expr) => {{
        #[cfg(debug_assertions)]
        tracing::error_span!($s).entered();
    }};
}
