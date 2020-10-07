use retain_mut::RetainMut;
use swc_common::{util::move_map::MoveMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_visit::{VisitMut, VisitMutWith};

/// Handles
///
/// ```ts
/// 
/// foo();
/// bar();
/// bar() {}
/// ```
#[derive(Debug, Default)]
pub(crate) struct RealImplRemover {
    last_ambient_fn_name: Option<Ident>,
}

impl VisitMut for RealImplRemover {
    fn visit_mut_stmt(&mut self, node: &mut Stmt) {
        node.visit_mut_children_with(self);

        match node {
            Stmt::Decl(Decl::Fn(ref decl)) => {
                if decl.function.body.is_none() {
                    self.last_ambient_fn_name = Some(decl.ident.clone());
                } else {
                    let name = self.last_ambient_fn_name.take();
                    if let Some(prev_name) = name {
                        if prev_name.sym == decl.ident.sym {
                            *node = Stmt::Empty(EmptyStmt { span: DUMMY_SP });
                            return;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_mut_module_item(&mut self, node: &mut ModuleItem) {
        node.visit_mut_children_with(self);

        match &node {
            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                decl: Decl::Fn(decl),
                ..
            })) => {
                if decl.function.body.is_none() {
                    self.last_ambient_fn_name = Some(decl.ident.clone());
                } else {
                    let name = self.last_ambient_fn_name.take();
                    if let Some(prev_name) = name {
                        if prev_name.sym == decl.ident.sym {
                            *node = ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP }));
                            return;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_mut_class_members(&mut self, members: &mut Vec<ClassMember>) {
        let mut last_ambient = None;

        members.retain_mut(|member| match member {
            ClassMember::Method(m) => match m.key {
                PropName::Ident(ref i) => {
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

    #[inline]
    fn visit_mut_block_stmt(&mut self, _: &mut BlockStmt) {}

    #[inline]
    fn visit_mut_ts_module_decl(&mut self, _: &mut TsModuleDecl) {}
}
