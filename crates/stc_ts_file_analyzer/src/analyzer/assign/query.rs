use stc_ts_errors::DebugExt;
use stc_ts_types::{QueryExpr, QueryType, Type};

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_query_type(&mut self, data: &mut AssignData, opts: AssignOpts, to: &QueryType, rhs: &Type) -> VResult<()> {
        let rhs = rhs.normalize();

        match &*to.expr {
            QueryExpr::TsEntityName(e) => {
                let to = self
                    .resolve_typeof(opts.span, e)
                    .context("tried to resolve typeof for assignment")?;

                return self.assign_with_opts(data, opts, &to, rhs);
            }
            QueryExpr::Import(_) => {
                unimplemented!("assignment of query type with import")
            }
        }
    }

    pub(super) fn assign_query_type_to_type(&mut self, data: &mut AssignData, opts: AssignOpts, to: &Type, rhs: &QueryType) -> VResult<()> {
        let to = to.normalize();

        match &*rhs.expr {
            QueryExpr::TsEntityName(e) => {
                let rhs = self
                    .resolve_typeof(opts.span, e)
                    .context("tried to resolve typeof for assignment")?;

                return self.assign_with_opts(data, opts, to, &rhs);
            }
            QueryExpr::Import(_) => {
                unimplemented!("assignment of query type with import")
            }
        }
    }
}
