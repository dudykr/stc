use stc_ts_types::Type;
use swc_common::TypeEq;
use swc_ecma_ast::TsKeywordTypeKind;

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
        let (s, t) = (source, target);

        if t.is_any() || t.is_unknown() || s.is_never() {
            return true;
        }

        if t.is_never() {
            return false;
        }

        if s.is_str_like() && t.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return true;
        }

        // TODO
        // if (s & TypeFlags.StringLiteral && s & TypeFlags.EnumLiteral &&
        //     t & TypeFlags.StringLiteral && !(t & TypeFlags.EnumLiteral) &&
        //     (source as StringLiteralType).value === (target as
        // StringLiteralType).value) return true;

        if s.is_number_like() && t.is_kwd(TsKeywordTypeKind::TsNumberKeyword) {
            return true;
        }

        // TODO
        // if (s & TypeFlags.NumberLiteral && s & TypeFlags.EnumLiteral &&
        //     t & TypeFlags.NumberLiteral && !(t & TypeFlags.EnumLiteral) &&
        //     (source as NumberLiteralType).value === (target as
        // NumberLiteralType).value) return true;

        if s.is_bigint_like() && t.is_kwd(TsKeywordTypeKind::TsBigIntKeyword) {
            return true;
        }

        if s.is_bool_like() && t.is_kwd(TsKeywordTypeKind::TsBooleanKeyword) {
            return true;
        }

        if s.is_symbol_like() && t.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
            return true;
        }

        // TODO
        // if (s & TypeFlags.Enum && t & TypeFlags.Enum && source.symbol.escapedName ===
        // target.symbol.escapedName &&     isEnumTypeRelatedTo(source.symbol,
        // target.symbol, errorReporter)) return true;

        // TODO
        // if (s & TypeFlags.EnumLiteral && t & TypeFlags.EnumLiteral) {
        //     if (s & TypeFlags.Union && t & TypeFlags.Union &&
        // isEnumTypeRelatedTo(source.symbol, target.symbol, errorReporter)) return
        // true;     if (s & TypeFlags.Literal && t & TypeFlags.Literal &&
        // (source as LiteralType).value === (target as LiteralType).value &&
        //         isEnumTypeRelatedTo(source.symbol, target.symbol, errorReporter))
        // return true; }

        // In non-strictNullChecks mode, `undefined` and `null` are assignable to
        // anything except `never`. Since unions and intersections may reduce to
        // `never`, we exclude them here.

        if s.is_undefined()
            && (!self.rule().strict_null_checks && !(t.is_union_type() || t.is_intersection())
                || (t.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || t.is_kwd(TsKeywordTypeKind::TsVoidKeyword)))
        {
            return true;
        }
        if s.is_null() && (!self.rule().strict_null_checks && !(t.is_union_type() || t.is_intersection()) || t.is_null()) {
            return true;
        }

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
