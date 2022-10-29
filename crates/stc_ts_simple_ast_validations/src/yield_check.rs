use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RIdent, RArrowExpr, RAssignExpr, RExpr, RFunction, RVarDeclarator};
use stc_ts_errors::Error;
use stc_ts_storage::Storage;
use swc_common::Spanned;

pub struct YieldCheck<'a, 'b> {
    pub in_generator: bool,
    pub errors: &'a mut Storage<'b>,
}

impl Visit<RExpr> for YieldCheck<'_, '_> {
    fn visit(&mut self, e: &RExpr) {
        e.visit_children_with(self);

        match &*e {
            RExpr::Yield(..) => {
                if !self.in_generator {
                    self.errors.report(Error::TS1212 { span: e.span() })
                }
            }
            RExpr::Ident(RIdent { ref sym, .. }) if sym == "yield" => {
                if !self.in_generator {
                    self.errors.report(Error::TS1212 { span: e.span() })
                }
            }
            _ => {}
        }
    }
}

impl Visit<RArrowExpr> for YieldCheck<'_, '_> {
    fn visit(&mut self, f: &RArrowExpr) {
        f.params.visit_with(self);

        let old = self.in_generator;
        self.in_generator = f.is_generator;
        f.body.visit_with(self);
        self.in_generator = old;
    }
}

impl Visit<RFunction> for YieldCheck<'_, '_> {
    fn visit(&mut self, f: &RFunction) {
        f.decorators.visit_with(self);
        f.params.visit_with(self);

        let old = self.in_generator;
        self.in_generator = f.is_generator;
        f.body.visit_with(self);
        self.in_generator = old;
    }
}
