use stc_ts_types::Type;
use swc_common::TypeEq;

use super::Analyzer;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Relation {
    #[default]
    Identical,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct IsRelatedOpts {
    pub kind: Relation,
}

#[derive(Debug, Default)]
struct IsRelatedData {}

impl Analyzer<'_, '_> {
    pub(crate) fn is_type_related_to(&mut self, source: &Type, target: &Type, opts: IsRelatedOpts) -> bool {
        let mut data = IsRelatedData::default();
        self.is_type_related_to_inner(&mut data, source, target, opts)
    }

    fn is_type_related_to_inner(&mut self, data: &mut IsRelatedData, source: &Type, target: &Type, opts: IsRelatedOpts) -> bool {
        if source.type_eq(target) {
            return true;
        }

        false
    }
}
