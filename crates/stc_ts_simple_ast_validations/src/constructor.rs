use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RArrowExpr, RClass, RFunction, RSeqExpr, RSuper};
use swc_common::Span;

#[derive(Debug, Default)]
pub struct ConstructorSuperCallFinder {
    pub has_valid_super_call: bool,

    in_nested: bool,
    pub nested_super_calls: Vec<Span>,
}

impl Visit<RSuper> for ConstructorSuperCallFinder {
    fn visit(&mut self, s: &RSuper) {
        if self.in_nested {
            self.nested_super_calls.push(s.span);
        } else {
            self.has_valid_super_call = true;
        }
    }
}

impl Visit<RFunction> for ConstructorSuperCallFinder {
    fn visit(&mut self, f: &RFunction) {
        f.decorators.visit_with(self);
        f.params.visit_with(self);

        let old = self.in_nested;
        self.in_nested = true;
        f.body.visit_with(self);
        self.in_nested = old;
    }
}

impl Visit<RArrowExpr> for ConstructorSuperCallFinder {
    fn visit(&mut self, f: &RArrowExpr) {
        f.params.visit_with(self);

        let old = self.in_nested;
        self.in_nested = true;
        f.body.visit_with(self);
        self.in_nested = old;
    }
}

/// Ignore nested classes.
impl Visit<RClass> for ConstructorSuperCallFinder {
    fn visit(&mut self, _: &RClass) {}
}

/// computedPropertyNames30_ES5.ts says
///
/// `Ideally, we would capture this. But the reference is
/// illegal, and not capturing this is consistent with
/// treatment of other similar violations.`
impl Visit<RSeqExpr> for ConstructorSuperCallFinder {
    fn visit(&mut self, v: &RSeqExpr) {
        if self.in_nested {
            return;
        }
        v.visit_children_with(self);
    }
}
