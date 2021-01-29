use crate::Error;

pub trait DebugExt<T>: Into<Result<T, Box<Error>>> {
    #[inline]
    #[track_caller]
    fn context(self, msg: &str) -> Result<T, Box<Error>> {
        self.into().map_err(|err: Box<Error>| err.context(msg))
    }

    #[inline]
    #[track_caller]
    fn with_context<F>(self, msg: F) -> Result<T, Box<Error>>
    where
        F: FnOnce() -> String,
    {
        self.into().map_err(|err: Box<Error>| err.context(msg()))
    }
}

impl<T> DebugExt<T> for Result<T, Box<Error>> {}
