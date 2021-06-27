#![feature(specialization)]
#![allow(incomplete_features)]

use stc_ts_types::Key;

pub mod type_param;

#[derive(Debug, Clone, Copy, Default)]
pub struct ExpandGenericOpts<'a> {
    /// If this field is empty, all properites will be expanded.
    ///
    /// [Key::Computed] is not allowed.
    pub props: &'a [Key],
}
