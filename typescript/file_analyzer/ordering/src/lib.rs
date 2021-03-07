#![allow(incomplete_features)]
#![feature(specialization)]

use fxhash::FxHashMap;
use petgraph::graphmap::DiGraphMap;

use self::types::Sortable;

mod class;
mod object;
mod stmt;
pub mod types;

pub fn calc_eval_order<T>(nodes: &[T]) -> Vec<usize>
where
    T: Sortable,
{
    let mut graph = DiGraphMap::default();
    let mut declared_by = FxHashMap::<_, Vec<usize>>::default();

    for (idx, node) in nodes.iter().enumerate() {
        graph.add_node(idx);
        let declared_ids = node.declares();
        for id in declared_ids {
            declared_by.entry(id).or_default().push(idx);
        }
    }

    for (idx, node) in nodes.iter().enumerate() {
        let used_ids = node.uses();
        for id in used_ids {
            if let Some(declarator_indexes) = declared_by.get(&id) {
                for &declarator_idx in declarator_indexes {
                    if idx == declarator_idx {
                        continue;
                    }
                    graph.add_edge(idx, declarator_idx, ());
                }
            }
        }
    }

    unimplemented!()
}
