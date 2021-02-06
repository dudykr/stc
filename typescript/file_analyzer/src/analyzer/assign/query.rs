use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_types::QueryExpr;
use stc_ts_types::QueryType;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_query_type(
        &mut self,
        opts: AssignOpts,
        to: &QueryType,
        rhs: &Type,
    ) -> ValidationResult<()> {
        let rhs = rhs.normalize();

        match &*to.expr {
            QueryExpr::TsEntityName(e) => {
                let to = self
                    .resolve_typeof(opts.span, e)
                    .context("tried to resolve typeof for assignment")?;

                return self.assign_with_opts(opts, &to, rhs);
            }
            QueryExpr::Import(_) => {
                unimplemented!("assignment of query type with import")
            }
        }
    }

    pub(super) fn assign_from_query_type(
        &mut self,
        opts: AssignOpts,
        to: &Type,
        rhs: &QueryType,
    ) -> ValidationResult<()> {
        let to = to.normalize();

        match &*rhs.expr {
            QueryExpr::TsEntityName(e) => {
                let rhs = self
                    .resolve_typeof(opts.span, e)
                    .context("tried to resolve typeof for assignment")?;

                return self.assign_with_opts(opts, to, &rhs);
            }
            QueryExpr::Import(_) => {
                unimplemented!("assignment of query type with import")
            }
        }
    }
}
