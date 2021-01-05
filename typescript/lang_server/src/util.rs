use anyhow::Error;
use stc_ts_ls_base_db::Canceled;
use std::fmt;

pub(crate) fn is_canceled(e: &Error) -> bool {
    e.downcast_ref::<Canceled>().is_some()
}

#[derive(Debug)]
pub struct LspError {
    pub code: i32,
    pub message: String,
}

impl LspError {
    fn new(code: i32, message: String) -> LspError {
        LspError { code, message }
    }
}

impl fmt::Display for LspError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Language Server request failed with {}. ({})",
            self.code, self.message
        )
    }
}

impl std::error::Error for LspError {}
