use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RClass;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RClassProp;
use stc_ts_ast_rnode::REmptyStmt;
use stc_ts_ast_rnode::RExportDefaultExpr;
use stc_ts_ast_rnode::RFunction;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RModule;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_dts_mutations::ClassMemberMut;
use stc_ts_dts_mutations::ClassMut;
use stc_ts_dts_mutations::ClassPropMut;
use stc_ts_dts_mutations::ExportDefaultMut;
use stc_ts_dts_mutations::FunctionMut;
use stc_ts_dts_mutations::ModuleItemMut;
use stc_ts_dts_mutations::Mutations;
use stc_ts_dts_mutations::PatMut;
use stc_ts_dts_mutations::VarDeclMut;
use stc_ts_utils::HasNodeId;
use stc_ts_utils::MapWithMut;
use std::mem::take;
use swc_common::DUMMY_SP;

pub fn apply_mutations(mutations: &mut Mutations, m: &mut RModule) {
    let mut v = Operator { mutations };
    m.visit_mut_with(&mut v);
}

struct Operator<'a> {
    mutations: &'a mut Mutations,
}

impl VisitMut<Vec<RModuleItem>> for Operator<'_> {
    fn visit_mut(&mut self, items: &mut Vec<RModuleItem>) {
        let mut new = Vec::with_capacity(items.len() * 11 / 10);

        for mut item in items.take() {
            let node_id = item.node_id();

            if let Some(node_id) = node_id {
                if let Some(ModuleItemMut { prepend_stmts, .. }) = self.mutations.for_module_items.get_mut(&node_id) {
                    new.extend(take(prepend_stmts).into_iter().map(RModuleItem::Stmt));
                }
            }

            item.visit_mut_children_with(self);

            new.push(item);

            if let Some(node_id) = node_id {
                if let Some(ModuleItemMut { append_stmts, .. }) = self.mutations.for_module_items.get_mut(&node_id) {
                    new.extend(take(append_stmts).into_iter().map(RModuleItem::Stmt));
                }
            }
        }

        *items = new;
    }
}

impl VisitMut<RVarDeclarator> for Operator<'_> {
    fn visit_mut(&mut self, d: &mut RVarDeclarator) {
        d.visit_mut_children_with(self);

        if let Some(VarDeclMut { remove_init }) = self.mutations.for_var_decls.remove(&d.node_id) {
            if remove_init {
                d.init = None;
            }
        }
    }
}

impl VisitMut<RClass> for Operator<'_> {
    fn visit_mut(&mut self, c: &mut RClass) {
        c.visit_mut_children_with(self);

        if let Some(ClassMut {
            super_class,
            additional_members: _,
        }) = self.mutations.for_classes.remove(&c.node_id)
        {
            if let Some(super_class) = super_class {
                c.super_class = Some(super_class);
            }
        }
    }
}

impl VisitMut<RFunction> for Operator<'_> {
    fn visit_mut(&mut self, f: &mut RFunction) {
        f.visit_mut_children_with(self);

        if let Some(FunctionMut { ret_ty }) = self.mutations.for_fns.remove(&f.node_id) {
            if let Some(ret_ty) = ret_ty {
                f.return_type = Some(ret_ty.into())
            }
        }
    }
}

impl VisitMut<RClassMember> for Operator<'_> {
    fn visit_mut(&mut self, member: &mut RClassMember) {
        let node_id = match member {
            RClassMember::Constructor(c) => c.node_id,
            RClassMember::Method(m) => m.node_id,
            RClassMember::PrivateMethod(m) => m.node_id,
            RClassMember::ClassProp(p) => p.node_id,
            RClassMember::PrivateProp(p) => p.node_id,
            RClassMember::TsIndexSignature(s) => s.node_id,
            RClassMember::Empty(_) => return,
        };

        if let Some(ClassMemberMut { remove }) = self.mutations.for_class_members.remove(&node_id) {
            if remove {
                *member = RClassMember::Empty(REmptyStmt { span: DUMMY_SP });
                return;
            }
        }

        member.visit_mut_children_with(self);
    }
}

impl VisitMut<RClassProp> for Operator<'_> {
    fn visit_mut(&mut self, p: &mut RClassProp) {
        p.visit_mut_children_with(self);

        if let Some(ClassPropMut { ty }) = self.mutations.for_class_props.remove(&p.node_id) {
            if let Some(ty) = ty {
                p.type_ann = Some(ty.into())
            }
        }
    }
}

impl VisitMut<RIdent> for Operator<'_> {
    fn visit_mut(&mut self, i: &mut RIdent) {
        i.visit_mut_children_with(self);

        if let Some(PatMut { ty, optional }) = self.mutations.for_pats.remove(&i.node_id) {
            if let Some(ty) = ty {
                i.type_ann = Some(ty.into())
            }
            if let Some(optional) = optional {
                i.optional = optional;
            }
        }
    }
}

impl VisitMut<RObjectPat> for Operator<'_> {
    fn visit_mut(&mut self, obj: &mut RObjectPat) {
        obj.visit_mut_children_with(self);

        if let Some(PatMut { ty, optional }) = self.mutations.for_pats.remove(&obj.node_id) {
            if let Some(ty) = ty {
                obj.type_ann = Some(ty.into())
            }
            if let Some(optional) = optional {
                obj.optional = optional;
            }
        }
    }
}

impl VisitMut<RArrayPat> for Operator<'_> {
    fn visit_mut(&mut self, arr: &mut RArrayPat) {
        arr.visit_mut_children_with(self);

        if let Some(PatMut { ty, optional }) = self.mutations.for_pats.remove(&arr.node_id) {
            if let Some(ty) = ty {
                arr.type_ann = Some(ty.into())
            }
            if let Some(optional) = optional {
                arr.optional = optional;
            }
        }
    }
}

impl VisitMut<RRestPat> for Operator<'_> {
    fn visit_mut(&mut self, r: &mut RRestPat) {
        r.visit_mut_children_with(self);

        if let Some(PatMut { ty, optional: _ }) = self.mutations.for_pats.remove(&r.node_id) {
            if let Some(ty) = ty {
                r.type_ann = Some(ty.into())
            }
        }
    }
}

impl VisitMut<RExportDefaultExpr> for Operator<'_> {
    fn visit_mut(&mut self, export: &mut RExportDefaultExpr) {
        export.visit_mut_children_with(self);

        if let Some(ExportDefaultMut { replace_with }) = self.mutations.for_export_defaults.remove(&export.node_id) {
            if let Some(expr) = replace_with {
                export.expr = expr;
            }
        }
    }
}
