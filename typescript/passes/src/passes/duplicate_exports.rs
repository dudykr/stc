use crate::Pass;
use stc_ts_errors::Error;
use std::mem::take;

pub fn duplicate_exports() -> impl Pass {
    DuplicateExports::default()
}

#[derive(Default)]
struct DuplicateExports {
    errors: Vec<Error>,
}

impl Pass for DuplicateExports {
    fn name() -> &'static str {
        "duplicate-exports"
    }

    fn take_errors(&mut self) -> Vec<Error> {
        take(&mut self.errors)
    }
}
