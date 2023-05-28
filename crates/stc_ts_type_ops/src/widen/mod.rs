use rnode::{Fold, FoldWith};
use stc_ts_types::{Array, ArrayMetadata, KeywordType, PropertySignature, Type};
use swc_common::TypeEq;
use swc_ecma_ast::TsKeywordTypeKind;

/// Type widener.
///
/// - Tuple => Array
/// - null => any
/// - undefined => any
pub struct Widen {
    pub tuple_to_array: bool,
}

impl Fold<Type> for Widen {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.normalize_mut();
        let ty = ty.fold_children_with(self);

        match ty {
            Type::Keyword(
                ty @ KeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword | TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                },
            ) => Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..ty
            }),

            Type::Lit(..) => ty.force_generalize_top_level_literals(),

            Type::Tuple(tuple) if self.tuple_to_array => {
                let span = tuple.span;

                let mut types: Vec<Type> = vec![];

                for element in tuple.elems {
                    if types.iter().any(|item| item.type_eq(&element.ty)) {
                        continue;
                    }

                    types.push(*element.ty);
                }

                let elem_type = Box::new(Type::new_union(span, types));
                Type::Array(Array {
                    span,
                    elem_type,
                    metadata: ArrayMetadata {
                        common: tuple.metadata.common,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
            }

            _ => ty,
        }
    }
}

impl Fold<PropertySignature> for Widen {
    fn fold(&mut self, mut value: PropertySignature) -> PropertySignature {
        value = value.fold_children_with(self);

        value.optional = true;

        value
    }
}
