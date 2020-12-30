use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RFunction;
use stc_ts_ast_rnode::RModule;
use stc_ts_dts_mutations::FunctionMut;
use stc_ts_dts_mutations::Mutations;

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
