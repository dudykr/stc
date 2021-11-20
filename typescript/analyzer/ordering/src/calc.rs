use petgraph::EdgeDirection::Outgoing;
use std::hash::Hash;
use swc_common::collections::{AHashMap, AHashSet};
use swc_fast_graph::digraph::FastDiGraphMap;
use swc_graph_analyzer::{DepGraph, GraphAnalyzer};
use tracing::trace;

pub(crate) struct Deps<'a, I>
where
    I: Eq + Hash,
{
    pub declared_by: &'a AHashMap<I, Vec<usize>>,
    pub used_by_idx: &'a AHashMap<usize, AHashSet<I>>,
}

/// Returns `(cycles, graph)`.
pub(crate) fn to_graph<I>(deps: &Deps<I>, len: usize) -> (Vec<Vec<usize>>, FastDiGraphMap<usize, ()>)
where
    I: Eq + Hash,
{
    let mut a = GraphAnalyzer::new(deps);

    for idx in 0..len {
        a.load(idx);
    }

    let res = a.into_result();

    (res.cycles, res.graph)
}

impl<I> DepGraph for Deps<'_, I>
where
    I: Eq + Hash,
{
    type ModuleId = usize;

    fn deps_of(&self, module_id: Self::ModuleId) -> Vec<Self::ModuleId> {
        let used = self.used_by_idx.get(&module_id);
        let used = match used {
            Some(v) => v,
            None => return Default::default(),
        };

        let mut buf = vec![];

        for used in used {
            let deps = self.declared_by.get(used);

            if let Some(deps) = deps {
                buf.extend(deps.iter());
            }
        }

        buf
    }
}

pub(crate) fn calc_order(
    cycles: Vec<Vec<usize>>,
    graph: &mut FastDiGraphMap<usize, ()>,
    len: usize,
) -> Vec<Vec<usize>> {
    let mut done = AHashSet::default();
    let mut orders = vec![];

    loop {
        for idx in 0..len {
            let next = calc_one(&done, &cycles, graph, idx);

            done.extend(next.iter().copied());

            if !next.is_empty() {
                orders.push(next);
            }
        }

        if (0..len).into_iter().all(|idx| done.contains(&idx)) {
            break;
        }
    }

    orders
}

fn calc_one(
    done: &AHashSet<usize>,
    cycles: &[Vec<usize>],
    graph: &mut FastDiGraphMap<usize, ()>,
    idx: usize,
) -> Vec<usize> {
    if cfg!(debug_assertions) {
        trace!("calc_one(idx = {:?})", idx);
    }

    if done.contains(&idx) {
        return vec![];
    }

    if let Some(cycle) = cycles.iter().find(|v| v.contains(&idx)) {
        return cycle.clone();
    }

    let deps = graph.neighbors_directed(idx, Outgoing).collect::<Vec<_>>();

    for dep in deps {
        let v = calc_one(done, cycles, graph, dep);
        if v.is_empty() {
            continue;
        }
        return v;
    }

    return vec![idx];
}
