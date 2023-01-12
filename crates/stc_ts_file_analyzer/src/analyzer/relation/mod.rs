use stc_ts_types::Type;
use swc_common::TypeEq;

use super::Analyzer;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Relation {
    #[default]
    Identity,
    Comparable,
    StrictSubtype,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct IsRelatedOpts {
    pub kind: Relation,
}

#[derive(Debug, Default)]
struct IsRelatedData {}

impl Analyzer<'_, '_> {
    /// Ported from `isTypeRelatedTo` of `tsc`.
    pub(crate) fn is_type_related_to(&mut self, source: &Type, target: &Type, opts: IsRelatedOpts) -> bool {
        let mut data = IsRelatedData::default();
        self.is_type_related_to_inner(&mut data, source, target, opts)
    }

    fn is_type_related_to_inner(&mut self, data: &mut IsRelatedData, source: &Type, target: &Type, opts: IsRelatedOpts) -> bool {
        let relation = opts.kind;

        if source.type_eq(target) {
            return true;
        }

        if relation == Relation::Comparable && !target.is_never() && self.is_simple_type_related_to(target, source, opts)
            || self.is_simple_type_related_to(source, target, opts)
        {
            return true;
        }

        match opts.kind {
            Relation::Identity => false,
            Relation::Comparable => false,
            Relation::StrictSubtype => true,
        }
    }

    fn is_simple_type_related_to(&mut self, source: &Type, target: &Type, opts: IsRelatedOpts) -> bool {}
}
