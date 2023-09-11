use std::time::{Duration, Instant};

pub struct PerfTimer<'l> {
    pub start: Instant,
    pub logger: &'l dyn Fn(&str, Duration),
}

impl<'l> PerfTimer<'l> {
    pub fn with_logger(logger: &'l dyn Fn(&str, Duration)) -> Self {
        Self {
            start: Instant::now(),
            logger,
        }
    }

    pub fn noop() -> Self {
        Self::with_logger(&|_, _| {})
    }

    pub fn log_trace() -> Self {
        Self::with_logger(&loggers::log_trace)
    }

    pub fn log_debug() -> Self {
        Self::with_logger(&loggers::log_debug)
    }

    pub fn log_info() -> Self {
        Self::with_logger(&loggers::log_info)
    }

    pub fn log_warn() -> Self {
        Self::with_logger(&loggers::log_warn)
    }

    pub fn tracing_debug() -> Self {
        Self::with_logger(&loggers::tracing_debug)
    }

    pub fn tracing_warn() -> Self {
        Self::with_logger(&loggers::tracing_warn)
    }
}

impl<'l> PerfTimer<'l> {
    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }

    pub fn log(&self, scope: &str) {
        (self.logger)(scope, self.start.elapsed())
    }
}

mod loggers {
    use std::time::Duration;

    pub fn log_trace(scope: &str, duration: Duration) {
        log::trace!("{} took {:?}", scope, duration);
    }

    pub fn log_debug(scope: &str, duration: Duration) {
        log::debug!("{} took {:?}", scope, duration);
    }

    pub fn log_info(scope: &str, duration: Duration) {
        log::info!("{} took {:?}", scope, duration);
    }

    pub fn log_warn(scope: &str, duration: Duration) {
        log::warn!("{} took {:?}", scope, duration);
    }

    pub fn tracing_debug(scope: &str, duration: Duration) {
        tracing::debug!("{} took {:?}", scope, duration);
    }

    pub fn tracing_warn(scope: &str, duration: Duration) {
        tracing::warn!("{} took {:?}", scope, duration);
    }
}
