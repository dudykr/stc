use rnode::{VisitMut, VisitMutWith};
use stc_ts_ast_rnode::{
    RBlockStmt, RClassMember, RDecl, REmptyStmt, RExportDecl, RIdent, RModuleDecl, RModuleItem, RPropName, RStmt, RTsModuleDecl,
};
use swc_common::DUMMY_SP;

/// Handles
///
/// ```ts
/// foo();
/// bar();
/// bar() {}
/// ```
#[derive(Debug, Default)]
pub(crate) struct RealImplRemover {
    last_ambient_fn_name: Option<RIdent>,
}

impl VisitMut<RStmt> for RealImplRemover {
    fn visit_mut(&mut self, node: &mut RStmt) {
        node.visit_mut_children_with(self);

        match node {
            RStmt::Decl(RDecl::Fn(ref decl)) => {
                if decl.function.body.is_none() {
                    self.last_ambient_fn_name = Some(decl.ident.clone());
                } else {
                    let name = self.last_ambient_fn_name.take();
                    if let Some(prev_name) = name {
                        if prev_name.sym == decl.ident.sym {
                            *node = RStmt::Empty(REmptyStmt { span: DUMMY_SP });
                            return;
                        }
                    }
                }
            }
            _ => {}
        }
    }
}
impl VisitMut<RModuleItem> for RealImplRemover {
    fn visit_mut(&mut self, node: &mut RModuleItem) {
        node.visit_mut_children_with(self);

        match &node {
            RModuleItem::ModuleDecl(RModuleDecl::ExportDecl(RExportDecl { decl: RDecl::Fn(decl), .. })) => {
                if decl.function.body.is_none() {
                    self.last_ambient_fn_name = Some(decl.ident.clone());
                } else {
                    let name = self.last_ambient_fn_name.take();
                    if let Some(prev_name) = name {
                        if prev_name.sym == decl.ident.sym {
                            *node = RModuleItem::Stmt(RStmt::Empty(REmptyStmt { span: DUMMY_SP }));
                            return;
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

impl VisitMut<Vec<RClassMember>> for RealImplRemover {
    fn visit_mut(&mut self, members: &mut Vec<RClassMember>) {
        let mut last_ambient = None;

        members.retain_mut(|member| match member {
            RClassMember::Method(m) => match m.key {
                RPropName::Ident(ref i) => {
                    if m.function.body.is_none() {
                        last_ambient = Some(i.sym.clone());
                        return true;
                    }

                    if let Some(prev_name) = last_ambient.take() {
                        if prev_name == i.sym {
                            return false;
                        }
                    }
                    return true;
                }
                _ => return true,
            },
            _ => true,
        });
    }
}

/// Noop.
impl VisitMut<RBlockStmt> for RealImplRemover {
    fn visit_mut(&mut self, _: &mut RBlockStmt) {}
}

/// Noop.
impl VisitMut<RTsModuleDecl> for RealImplRemover {
    fn visit_mut(&mut self, _: &mut RTsModuleDecl) {}
}
