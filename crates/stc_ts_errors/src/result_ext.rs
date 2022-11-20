use crate::ErrorKind;

pub trait DebugExt<T>: Into<Result<T, ErrorKind>> {
    fn convert_err<F>(self, op: F) -> Result<T, ErrorKind>
    where
        F: FnOnce(ErrorKind) -> ErrorKind,
    {
        self.into().map_err(|err: ErrorKind| err.convert(op))
    }

    #[inline]
    #[track_caller]
    fn context(self, msg: &str) -> Result<T, ErrorKind> {
        if !cfg!(debug_assertions) {
            return self.into();
        }

        self.into().map_err(|err: ErrorKind| err.context(msg))
    }

    #[inline]
    #[track_caller]
    fn with_context<F>(self, msg: F) -> Result<T, ErrorKind>
    where
        F: FnOnce() -> String,
    {
        if !cfg!(debug_assertions) {
            return self.into();
        }

        self.into().map_err(|err: ErrorKind| err.context(msg()))
    }
}

impl<T> DebugExt<T> for Result<T, ErrorKind> {}
