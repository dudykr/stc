use fxhash::{FxHashMap, FxHashSet};
use petgraph::graphmap::DiGraphMap;
use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RClassMember, RExpr, RMemberExpr, RMemberProp};
use stc_ts_types::{rprop_name_to_expr, Id};

use crate::{
    analyzer::Analyzer,
    util::graph::{Inliner, NodeId},
};

impl Analyzer<'_, '_> {
    /// Calculate the order of the evaluation of class members.
    /// This is used to avoid reevaluation if possible.
    ///
    /// Note that this is not perfect, and if class methods references each
    /// other, we have to evaluate them again with `any` for references.
    ///
    ///
    /// This method ignores order of class properties or parameter properties.
    /// So the length of returned vector can be smaller than length of
    /// `members`.
    ///
    /// Note that the boey constructor is analyzed.
    pub(super) fn calc_eval_order_of_class_methods(
        &mut self,
        mut remaining_indexes: Vec<usize>,
        members: &[RClassMember],
    ) -> Vec<usize> {
        let mut keys = Inliner::<Key>::default();
        let mut graph = DiGraphMap::<NodeId<Key>, ()>::default();
        // Map of an index from remaining_indexes to a node id.
        let mut defined = FxHashMap::<usize, NodeId<Key>>::default();

        // Now we should create a dependency graph.
        for (index, member) in members.iter().enumerate() {
            if !remaining_indexes.contains(&index) {
                continue;
            }

            match member {
                RClassMember::Constructor(_) | RClassMember::StaticBlock(_) => continue,

                RClassMember::TsIndexSignature(_) => {}

                RClassMember::ClassProp(_)
                | RClassMember::PrivateProp(_)
                | RClassMember::Empty(_) => {
                    // unreachable!
                    continue;
                }

                RClassMember::Method(..) | RClassMember::PrivateMethod(..) => {
                    let key = match member {
                        RClassMember::Method(v) => match rprop_name_to_expr(v.key.clone()) {
                            RExpr::Ident(i) => Key::Id(i.into()),
                            _ => continue,
                        },
                        RClassMember::PrivateMethod(v) => Key::Private(v.key.id.clone().into()),
                        _ => unreachable!(),
                    };
                    let key = keys.inline(key);
                    defined.insert(index, key);
                    graph.add_node(key);

                    let mut visitor = MethodAnalyzer {
                        result: Default::default(),
                    };
                    member.visit_with(&mut visitor);

                    for dep in visitor.result.depends_on {
                        let dep = keys.inline(dep);
                        graph.add_node(dep);

                        graph.add_edge(key, dep, ());
                    }
                }
            }
        }

        let indexes = remaining_indexes.to_vec();

        // We now iterate over indexes, and sort them topologically.
        for (i, i_key) in indexes.iter().copied().enumerate() {
            let i_key = match defined.get(&i_key) {
                Some(v) => *v,
                None => continue,
            };
            for (j, j_key) in indexes.iter().copied().enumerate().filter(|(_, j)| i < *j) {
                let j_key = match defined.get(&j_key) {
                    Some(v) => *v,
                    None => continue,
                };

                // Swap if required.

                if graph.contains_edge(i_key, j_key) {
                    remaining_indexes.swap(i, j);
                }
            }
        }

        remaining_indexes
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Key {
    Id(Id),
    Private(Id),
}
/// Analzyed information of a class member.
#[derive(Default)]
struct AnalysisResult {
    /// `this.#foo` or `this.foo`.
    depends_on: FxHashSet<Key>,
}

struct MethodAnalyzer {
    result: AnalysisResult,
}

impl Visit<RMemberExpr> for MethodAnalyzer {
    fn visit(&mut self, e: &RMemberExpr) {
        e.obj.visit_with(self);
        e.prop.visit_with(self);

        match &*e.obj {
            RExpr::This(..) => {
                // We detects this.#foo and this.foo
                match &e.prop {
                    RMemberProp::Ident(i) => {
                        self.result.depends_on.insert(Key::Id(i.into()));
                    }
                    RMemberProp::PrivateName(i) => {
                        self.result
                            .depends_on
                            .insert(Key::Private(i.id.clone().into()));
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}
