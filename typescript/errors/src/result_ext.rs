use crate::Error;

pub trait DebugExt<T>: Into<Result<T, Error>> {
    #[inline]
    #[track_caller]
    fn context(self, msg: &str) -> Result<T, Error> {
        self.into().map_err(|err: Error| err.context(msg))
    }

    #[inline]
    #[track_caller]
    fn with_context<F>(self, msg: F) -> Result<T, Error>
    where
        F: FnOnce() -> String,
    {
        self.into().map_err(|err: Error| err.context(msg()))
    }
}

impl<T> DebugExt<T> for Result<T, Error> {}
