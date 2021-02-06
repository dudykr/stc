use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::FnParam;

impl Analyzer<'_, '_> {
    pub(crate) fn assign_params(&mut self, opts: AssignOpts, l: &[FnParam], r: &[FnParam]) -> ValidationResult<()> {
        let span = opts.span;

        if l.len() != r.len() {
            return Err(box Error::Unimplemented {
                span,
                msg: format!("l.params.len() = {}; r.params.len() = {};", l.len(), r.len()),
            });
        }

        for (lp, rp) in l.iter().zip(r.iter()) {
            self.assign_inner(&lp.ty, &rp.ty, opts)
                .context("tried to assign a method parameter to a method parameter")?;
        }

        Ok(())
    }
}
