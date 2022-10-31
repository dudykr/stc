use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RDecl, RFnDecl, RIdent, RStmt, RTsModuleDecl, RTsNamespaceDecl};
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

impl AmbientFunctionHandler<'_, '_> {
    pub fn handle_missing_impl(&mut self) {
        if let Some(id) = self.last_ambient_name.take() {
            self.errors.report(Error::FnImplMissingOrNotFollowedByDecl { span: id.span })
        }
    }
}

impl Visit<RStmt> for AmbientFunctionHandler<'_, '_> {
    fn visit(&mut self, node: &RStmt) {
        node.visit_children_with(self);

        match node {
            RStmt::Decl(RDecl::Fn(..)) => {}
            _ => {
                self.handle_missing_impl();
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
                    self.errors.report(Error::FnImplMissingOrNotFollowedByDecl { span: name.span });
                }
            }
            self.last_ambient_name = Some(node.ident.clone());
        } else {
            if let Some(ref name) = self.last_ambient_name {
                if node.ident.sym == name.sym {
                    self.last_ambient_name = None;
                } else {
                    self.errors.report(Error::TS2389 { span: node.ident.span });
                    self.last_ambient_name = None;
                }
            }
        }
    }
}

impl Visit<RTsNamespaceDecl> for AmbientFunctionHandler<'_, '_> {
    fn visit(&mut self, value: &RTsNamespaceDecl) {
        if value.declare {
            return;
        }

        value.visit_children_with(self);
    }
}

impl Visit<RTsModuleDecl> for AmbientFunctionHandler<'_, '_> {
    fn visit(&mut self, decl: &RTsModuleDecl) {
        if decl.declare {
            return;
        }

        decl.visit_children_with(self);
    }
}
