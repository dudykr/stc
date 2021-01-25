use crate::Error;

pub trait DebugExt<T>: Into<Result<T, Error>> {
    #[inline]
    fn context(self, msg: &str) -> Result<T, Error> {
        self.into().map_err(|err: Error| err.context(msg))
    }

    #[inline]
    fn with_context<F>(self, msg: F) -> Result<T, Error>
    where
        F: FnOnce() -> String,
    {
        self.into().map_err(|err: Error| err.context(msg()))
    }
}
