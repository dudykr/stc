use swc_common::SyntaxContext;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_visit::Node;
use swc_ecma_visit::Visit;
use swc_ecma_visit::VisitWith;

pub fn assert_no_empty_ctxt_ident(m: &Module) {
    if !cfg!(debug_assertions) {
        return;
    }

    m.visit_with(&Invalid { span: DUMMY_SP }, &mut AssertNoEmptyCtxt);
}

struct AssertNoEmptyCtxt;

impl Visit for AssertNoEmptyCtxt {
    fn visit_member_expr(&mut self, n: &MemberExpr, _: &dyn Node) {
        n.obj.visit_with(n, self);
        if n.computed {
            n.prop.visit_with(n, self);
        }
    }

    fn visit_expr(&mut self, n: &Expr, _: &dyn Node) {
        n.visit_children_with(self);

        match n {
            Expr::Ident(i) => {
                if i.span.ctxt == SyntaxContext::empty() {
                    unreachable!("ts_resolver has a bug")
                }
            }
            _ => {}
        }
    }
}
