use crate::analyzer::Analyzer;
use crate::util::graph::Inliner;
use crate::util::graph::NodeId;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::graphmap::DiGraphMap;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_types::rprop_name_to_expr;
use stc_ts_types::Id;

impl Analyzer<'_, '_> {
    /// This method ignores order of class properties or parameter properties.
    /// So the length of returned vector can be smaller than length of
    /// `members`.
    ///
    /// Note that the boey constructor is analyzed.
    pub(super) fn calc_order_of_class_methods(
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
                RClassMember::Constructor(_) => continue,

                RClassMember::TsIndexSignature(_) => {}

                RClassMember::ClassProp(_) | RClassMember::PrivateProp(_) | RClassMember::Empty(_) => {
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
        if e.computed {
            e.prop.visit_with(self);
        }

        match &e.obj {
            RExprOrSuper::Super(_) => {}
            RExprOrSuper::Expr(box RExpr::This(..)) => {
                // We detects this.#foo and this.foo
                match &*e.prop {
                    RExpr::Ident(i) => {
                        self.result.depends_on.insert(Key::Id(i.into()));
                    }
                    RExpr::PrivateName(i) => {
                        self.result.depends_on.insert(Key::Private(i.id.clone().into()));
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}
