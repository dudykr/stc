use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RClassMethod;
use stc_ts_ast_rnode::RClassProp;
use stc_ts_ast_rnode::REmptyStmt;
use stc_ts_ast_rnode::RFunction;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RModule;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_dts_mutations::ClassMemberMut;
use stc_ts_dts_mutations::ClassPropMut;
use stc_ts_dts_mutations::FunctionMut;
use stc_ts_dts_mutations::Mutations;
use stc_ts_dts_mutations::PatMut;
use swc_common::DUMMY_SP;

pub fn apply_mutations(mutations: Mutations, m: &mut RModule) {
    let mut v = Operator { mutations };
    m.visit_mut_with(&mut v);
}

struct Operator {
    mutations: Mutations,
}

impl VisitMut<RFunction> for Operator {
    fn visit_mut(&mut self, f: &mut RFunction) {
        f.visit_mut_children_with(self);

        if let Some(FunctionMut { ret_ty }) = self.mutations.for_fns.remove(&f.node_id) {
            if let Some(ret_ty) = ret_ty {
                f.return_type = Some(ret_ty.into())
            }
        }
    }
}

impl VisitMut<RClassMember> for Operator {
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

impl VisitMut<RClassProp> for Operator {
    fn visit_mut(&mut self, p: &mut RClassProp) {
        p.visit_mut_children_with(self);

        if let Some(ClassPropMut { ty }) = self.mutations.for_class_props.remove(&p.node_id) {
            if let Some(ty) = ty {
                p.type_ann = Some(ty.into())
            }
        }
    }
}

impl VisitMut<RIdent> for Operator {
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

impl VisitMut<RObjectPat> for Operator {
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

impl VisitMut<RArrayPat> for Operator {
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
