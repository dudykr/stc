use stc_ts_errors::{DebugExt, Error};
use stc_ts_types::{QueryExpr, QueryType, Type};

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_query_type(&mut self, data: &mut AssignData, to: &QueryType, rhs: &Type, opts: AssignOpts) -> VResult<()> {
        let rhs = rhs.normalize();

        match &*to.expr {
            QueryExpr::TsEntityName(e) => {
                let to = self
                    .resolve_typeof(opts.span, e)
                    .context("tried to resolve typeof for assignment")?;

                if to.is_global_this() {
                    return Err(Error::SimpleAssignFailed {
                        span: opts.span,
                        cause: None,
                    }
                    .context("global this"));
                }

                return self.assign_with_opts(data, &to, rhs, opts);
            }
            QueryExpr::Import(_) => {
                unimplemented!("assignment of query type with import")
            }
        }
    }

    pub(super) fn assign_query_type_to_type(&mut self, data: &mut AssignData, to: &Type, rhs: &QueryType, opts: AssignOpts) -> VResult<()> {
        let to = to.normalize();

        if let QueryExpr::TsEntityName(e) = &*rhs.expr {
            let rhs = self
                .resolve_typeof(opts.span, e)
                .context("tried to resolve typeof for assignment")?;

            if rhs.is_global_this() {
                return Err(Error::SimpleAssignFailed {
                    span: opts.span,
                    cause: None,
                }
                .context("global this"));
            }

            return self.assign_with_opts(data, to, &rhs, opts);
        } else {
            unimplemented!("assignment of query type with import")
        }
    }
}
