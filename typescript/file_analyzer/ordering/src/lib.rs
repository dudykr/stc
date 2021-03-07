use self::types::Sortable;

mod class;
mod object;
mod stmt;
pub mod types;

pub fn calc_eval_order<T>(t: &[T]) -> Vec<usize>
where
    T: Sortable,
{
}
