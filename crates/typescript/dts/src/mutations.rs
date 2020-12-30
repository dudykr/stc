use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::REmptyStmt;
use stc_ts_ast_rnode::RFunction;
use stc_ts_ast_rnode::RModule;
use stc_ts_dts_mutations::ClassMemberMut;
use stc_ts_dts_mutations::FunctionMut;
use stc_ts_dts_mutations::Mutations;
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
        member.visit_mut_children_with(self);

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
    }
}
