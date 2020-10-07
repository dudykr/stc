use crate::analyzer::Analyzer;
use crate::util::graph::Inliner;
use crate::util::graph::NodeId;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::graphmap::DiGraphMap;
use stc_types::Id;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::prop_name_to_expr;
use swc_ecma_visit::noop_visit_type;
use swc_ecma_visit::Node;
use swc_ecma_visit::Visit;
use swc_ecma_visit::VisitWith;

impl Analyzer<'_, '_> {
    /// This method ignores order of class properties or parameter properties.
    /// So the length of returned vector can be smaller than length of
    /// `members`.
    ///
    /// Note that the boey constructor is analyzed.
    pub(super) fn calc_order_of_class_methods(
        &mut self,
        mut remaining_indexes: Vec<usize>,
        members: &[ClassMember],
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
                ClassMember::Constructor(_) => continue,

                ClassMember::TsIndexSignature(_) => {}

                ClassMember::ClassProp(_) | ClassMember::PrivateProp(_) | ClassMember::Empty(_) => {
                    // unreachable!
                    continue;
                }

                ClassMember::Method(..) | ClassMember::PrivateMethod(..) => {
                    let key = match member {
                        ClassMember::Method(v) => match prop_name_to_expr(v.key.clone()) {
                            Expr::Ident(i) => Key::Id(i.into()),
                            _ => continue,
                        },
                        ClassMember::PrivateMethod(v) => Key::Private(v.key.id.clone().into()),
                        _ => unreachable!(),
                    };
                    let key = keys.inline(key);
                    defined.insert(index, key);
                    graph.add_node(key);

                    let mut visitor = MethodAnalyzer {
                        result: Default::default(),
                    };
                    member.visit_with(&Invalid { span: DUMMY_SP }, &mut visitor);

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

impl Visit for MethodAnalyzer {
    noop_visit_type!();

    fn visit_member_expr(&mut self, e: &MemberExpr, _: &dyn Node) {
        e.obj.visit_with(e, self);
        if e.computed {
            e.prop.visit_with(e, self);
        }

        match &e.obj {
            ExprOrSuper::Super(_) => {}
            ExprOrSuper::Expr(box Expr::This(..)) => {
                // We detects this.#foo and this.foo
                match &*e.prop {
                    Expr::Ident(i) => {
                        self.result.depends_on.insert(Key::Id(i.into()));
                    }
                    Expr::PrivateName(i) => {
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
