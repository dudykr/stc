use petgraph::graphmap::DiGraphMap;
use swc_common::collections::{AHashMap, AHashSet};

pub struct Usages<I> {
    declared_by: AHashMap<I, Vec<usize>>,
    used_by_idx: AHashMap<usize, AHashSet<I>>,

    graph: DiGraphMap<usize, ()>,

    output: Vec<Vec<usize>>,
}

impl<I> Default for Usages<I> {
    fn default() -> Self {
        Self {
            declared_by: Default::default(),
            used_by_idx: Default::default(),
            graph: Default::default(),
            output: Default::default(),
        }
    }
}

impl<I> Usages<I> {
    pub fn add_to_output(&mut self, idx: usize) {
        if self.output.iter().any(|vec| vec.contains(&idx)) {
            return;
        }
    }

    pub fn into_output(self) -> Vec<Vec<usize>> {
        self.output
    }
}
