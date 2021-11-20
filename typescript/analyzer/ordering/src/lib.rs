//! Currently this crate is not used.

#![allow(incomplete_features)]
#![feature(specialization)]

use self::types::Sortable;
use crate::calc::{calc_order, to_graph, Deps};
use either::Either;
use rayon::prelude::*;
use swc_common::collections::{AHashMap, AHashSet};

mod calc;
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
    let usages = nodes
        .par_iter()
        .map(|node| {
            let decls = node.get_decls();

            if decls.is_empty() {
                Either::Left(())
            } else {
                Either::Right(decls)
            }
        })
        .collect::<Vec<_>>();

    let mut declared_by: AHashMap<_, Vec<usize>> = Default::default();
    let mut used_by_idx: AHashMap<usize, AHashSet<_>> = Default::default();

    let mut pures = vec![];

    for (idx, usage) in usages.into_iter().enumerate() {
        match usage {
            Either::Left(()) => {
                pures.push(idx);
            }
            Either::Right(decls) => {
                for (id, deps) in decls {
                    declared_by.entry(id).or_default().push(idx);

                    used_by_idx.entry(idx).or_default().extend(deps);
                }
            }
        }
    }

    let (cycles, mut graph) = to_graph(
        &Deps {
            declared_by: &declared_by,
            used_by_idx: &used_by_idx,
        },
        nodes.len(),
    );

    let mut orders = calc_order(cycles, &pures, &mut graph, nodes.len());

    orders.extend(pures.into_iter().map(|v| vec![v]));

    orders
}
