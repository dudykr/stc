use crate::{Error, ErrorKind};

pub trait DebugExt<T>: Into<Result<T, Error>> {
    fn convert_err<F>(self, op: F) -> Result<T, Error>
    where
        F: FnOnce(ErrorKind) -> ErrorKind,
    {
        self.into().map_err(|err: Error| err.convert(op))
    }
}

impl<T> DebugExt<T> for Result<T, Error> {}
