use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RFnDecl;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_errors::Error;
use stc_ts_storage::Storage;

/// Handles
///
/// ```ts
/// // This is invalid
/// foo();
/// bar();
/// bar() {}
/// ```
pub struct AmbientFunctionHandler<'a, 'b> {
    pub last_ambient_name: Option<RIdent>,
    pub errors: &'a mut Storage<'b>,
}

impl Visit<RStmt> for AmbientFunctionHandler<'_, '_> {
    fn visit(&mut self, node: &RStmt) {
        node.visit_children_with(self);

        match node {
            RStmt::Decl(RDecl::Fn(..)) => {}
            _ => {
                // .take() is same as self.last_ambient_name = None
                if let Some(ref i) = self.last_ambient_name.take() {
                    self.errors.report(Error::TS2391 { span: i.span });
                }
            }
        }
    }
}

impl Visit<RFnDecl> for AmbientFunctionHandler<'_, '_> {
    fn visit(&mut self, node: &RFnDecl) {
        if node.declare {
            return;
        }

        if node.function.body.is_none() {
            if let Some(ref name) = self.last_ambient_name {
                if node.ident.sym != name.sym {
                    self.errors.report(Error::TS2389 { span: name.span });
                }
            }
            self.last_ambient_name = Some(node.ident.clone());
        } else {
            if let Some(ref name) = self.last_ambient_name {
                if node.ident.sym == name.sym {
                    self.last_ambient_name = None;
                } else {
                    self.errors.report(Error::TS2389 {
                        span: node.ident.span,
                    });
                    self.last_ambient_name = None;
                }
            }
        }
    }
}

impl Visit<RTsModuleDecl> for AmbientFunctionHandler<'_, '_> {
    fn visit(&mut self, _: &RTsModuleDecl) {}
}
