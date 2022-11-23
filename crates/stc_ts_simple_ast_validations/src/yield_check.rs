use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RArrowExpr, RAssignExpr, RExpr, RFunction, RVarDeclarator};

#[derive(Default)]
pub struct YieldValueUsageFinder {
    pub found: bool,
}

impl Visit<RAssignExpr> for YieldValueUsageFinder {
    fn visit(&mut self, e: &RAssignExpr) {
        e.visit_children_with(self);

        if let RExpr::Yield(..) = &*e.right {
            self.found = true;
        }
    }
}

impl Visit<RVarDeclarator> for YieldValueUsageFinder {
    fn visit(&mut self, v: &RVarDeclarator) {
        v.visit_children_with(self);

        if let Some(RExpr::Yield(..)) = v.init.as_deref() {
            self.found = true;
        }
    }
}

/// noop
impl Visit<RArrowExpr> for YieldValueUsageFinder {
    fn visit(&mut self, _: &RArrowExpr) {}
}

/// noop
impl Visit<RFunction> for YieldValueUsageFinder {
    fn visit(&mut self, _: &RFunction) {}
}
