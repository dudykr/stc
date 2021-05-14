use super::Pass;

pub fn duplicate_exports() -> impl Pass {
    DuplicateExports {}
}

struct DuplicateExports {}

impl Pass for DuplicateExports {
    fn name() -> &'static str {
        "duplicate-exports"
    }
}
