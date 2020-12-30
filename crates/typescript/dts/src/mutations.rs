use rnode::VisitMutWith;
use stc_ts_ast_rnode::RModule;
use stc_ts_dts_mutations::Mutations;

pub fn apply_mutations(mutations: Mutations, m: &mut RModule) {
    let mut v = Operator { mutations };
    m.visit_mut_with(&mut v);
}

struct Operator {
    mutations: Mutations,
}
