use crate::{
    analyzer::{pat::PatMode, scope::ScopeKind, Analyzer, Ctx},
    validator,
    validator::ValidateWith,
};
use rnode::VisitWith;
use stc_ts_ast_rnode::RCatchClause;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RCatchClause) {
        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            ..self.ctx
        };
        self.with_ctx(ctx)
            .with_child(ScopeKind::Block, Default::default(), |child: &mut Analyzer| {
                match &s.param {
                    Some(pat) => {
                        pat.validate_with(child)?;
                    }
                    None => {}
                }

                s.body.visit_with(child);

                Ok(())
            })
    }
}
