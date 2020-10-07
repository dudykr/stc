use super::super::Analyzer;
use crate::{
    analyzer::{pat::PatMode, scope::ScopeKind, util::ResultExt, Ctx},
    validator,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use stc_types::Type;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::Value::Known;
use swc_ecma_visit::{VisitMutWith, VisitWith};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut CatchClause) {
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
