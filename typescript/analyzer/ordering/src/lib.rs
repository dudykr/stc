#![allow(incomplete_features)]
#![feature(specialization)]

use self::types::Sortable;
use fxhash::{FxBuildHasher, FxHashSet};
use indexmap::IndexSet;
use petgraph::{algo::all_simple_paths, graphmap::DiGraphMap, EdgeDirection::Outgoing};
use std::{collections::VecDeque, iter::from_fn};
use swc_common::collections::{AHashMap, AHashSet};

mod class;
mod object;
pub mod stmt;
pub mod types;

/// # Returns
///
/// This function returns a vector of orders.
/// A vector with multiple elements indicates a circular reference.
pub fn calc_eval_order<T>(nodes: &[T]) -> Vec<Vec<usize>>
where
    T: Sortable,
{
    let mut graph = DiGraphMap::default();
    let mut declared_by = AHashMap::<_, Vec<usize>>::default();
    let mut used = AHashMap::<_, AHashSet<_>>::default();

    for (idx, node) in nodes.iter().enumerate() {
        let decls = node.get_decls();

        if decls.is_empty() {
            let used_vars = node.uses();
            used.entry(idx).or_default().extend(used_vars);
            // Assign expressions like `a = b` comes here.
        } else {
            for (id, deps) in decls {
                declared_by.entry(id).or_default().push(idx);

                used.entry(idx).or_default().extend(deps);
            }
        }
    }

    for (idx, deps) in used {
        for dep in deps {
            if let Some(declarator_indexes) = declared_by.get(&dep) {
                for &declarator_index in declarator_indexes {
                    if declarator_index != idx {
                        graph.add_edge(idx, declarator_index, ());
                    }
                }
            }
        }
    }

    let mut queue = VecDeque::default();
    queue.extend(0..nodes.len());

    iter_from_graph(graph, queue).collect()
}

fn iter_from_graph(mut graph: DiGraphMap<usize, ()>, mut queue: VecDeque<usize>) -> impl Iterator<Item = Vec<usize>> {
    let mut done = FxHashSet::default();
    from_fn(move || {
        while let Some(id) = queue.pop_front() {
            if done.contains(&id) {
                continue;
            }

            let deps = graph.neighbors_directed(id, Outgoing).collect::<Vec<_>>();

            if deps.is_empty() {
                // Remove dependencies to other node.
                graph.remove_node(id);
                done.insert(id);
                return Some(vec![id]);
            }

            let mut all_nodes_in_circle = all_nodes_in_circle(id, &done, &mut Default::default(), &graph);
            all_nodes_in_circle.reverse();

            if all_nodes_in_circle.is_empty() {
                queue.push_front(id);

                // This node does not have circular reference.
                for dep in deps.into_iter().rev() {
                    queue.push_front(dep);
                }

                continue;
            }

            let deps_of_circle = all_nodes_in_circle
                .iter()
                .flat_map(|&id| {
                    graph
                        .neighbors_directed(id, Outgoing)
                        .filter(|dep| !done.contains(&dep) && !all_nodes_in_circle.contains(dep))
                })
                .collect::<Vec<_>>();

            if !deps_of_circle.is_empty() {
                queue.push_front(id);

                // Handle dependencies first.
                for dep in deps_of_circle.into_iter().rev() {
                    queue.push_front(dep);
                }

                continue;
            }

            done.extend(all_nodes_in_circle.iter().copied());
            return Some(all_nodes_in_circle.into_iter().collect());
        }

        None
    })
}

fn all_nodes_in_circle(
    id: usize,
    done: &FxHashSet<usize>,
    already_in_index: &mut IndexSet<usize, FxBuildHasher>,
    graph: &DiGraphMap<usize, ()>,
) -> IndexSet<usize, FxBuildHasher> {
    let deps = graph
        .neighbors_directed(id, Outgoing)
        .filter(|dep| !done.contains(&dep) && !already_in_index.contains(dep))
        .collect::<Vec<_>>();

    let mut ids = deps
        .iter()
        .copied()
        .flat_map(|dep| {
            let mut paths = all_simple_paths::<Vec<_>, _>(&graph, dep, id, 0, None).collect::<Vec<_>>();

            for path in paths.iter_mut() {
                path.reverse();
            }

            paths
        })
        .flatten()
        .filter(|&idx| !done.contains(&idx) && !already_in_index.contains(&idx))
        .collect::<IndexSet<_, _>>();

    already_in_index.extend(ids.iter().copied());
    let mut new_ids = IndexSet::<_, FxBuildHasher>::default();

    for &dep_id in &ids {
        let others = all_nodes_in_circle(dep_id, done, already_in_index, graph);
        new_ids.extend(others)
    }
    ids.extend(new_ids);

    ids
}
