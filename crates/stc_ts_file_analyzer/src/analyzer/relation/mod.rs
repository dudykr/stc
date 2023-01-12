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

#[derive(Debug, Default)]
struct IsRelatedData {}

impl Analyzer<'_, '_> {
    /// Ported from `isTypeRelatedTo` of `tsc`.
    pub(crate) fn is_type_related_to(&mut self, source: &Type, target: &Type, relation: Relation) -> bool {
        let mut data = IsRelatedData::default();
        self.is_type_related_to_inner(&mut data, source, target, relation)
    }

    fn is_type_related_to_inner(&mut self, data: &mut IsRelatedData, source: &Type, target: &Type, relation: Relation) -> bool {
        if source.type_eq(target) {
            return true;
        }

        if relation != Relation::Identity {
            if relation == Relation::Comparable && !target.is_never() && self.is_simple_type_related_to(target, source, relation)
                || self.is_simple_type_related_to(source, target, relation)
            {
                return true;
            }
        }

        // if source.is_structured_or_instantiable() ||
        // target.is_structured_or_instantiable() {
        self.check_type_related_to(source, target, relation)
        // }

        // false
    }

    /// TODO: Implement
    ///
    /// Ported from `isSimpleTypeRelatedTo` of `tsc`.
    fn is_simple_type_related_to(&mut self, source: &Type, target: &Type, relation: Relation) -> bool {
        false
    }

    /// TODO: Implement
    ///
    /// Ported from `checkTypeRelatedTo` of `tsc`.
    fn check_type_related_to(&mut self, source: &Type, target: &Type, relation: Relation) -> bool {
        match relation {
            Relation::Identity => false,
            Relation::Comparable => false,
            Relation::StrictSubtype => {
                // TODO: This should be false.
                // This is true just because this function is not implemented yet
                true
            }
        }
    }
}
