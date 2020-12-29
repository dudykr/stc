use super::super::Analyzer;
use crate::{
    analyzer::{pat::PatMode, scope::ScopeKind, Ctx},
    validator,
    validator::ValidateWith,
};
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RCatchClause;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut RCatchClause) {
        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            ..self.ctx
        };
        self.with_ctx(ctx).with_child(
            ScopeKind::Block,
            Default::default(),
            |child: &mut Analyzer| {
                match &mut s.param {
                    Some(pat) => {
                        pat.validate_with(child)?;
                    }
                    None => {}
                }

                s.body.visit_mut_with(child);

                Ok(())
            },
        )
    }
}
