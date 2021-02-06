use super::Type;
use crate::type_facts::TypeFacts;
use rnode::Fold;
use rnode::FoldWith;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_types::IndexedAccessType;
use stc_ts_types::Union;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

pub(super) struct TypeFactsHandler {
    pub facts: TypeFacts,
}

impl Fold<RTsKeywordType> for TypeFactsHandler {
    fn fold(&mut self, ty: RTsKeywordType) -> RTsKeywordType {
        if self.facts.contains(TypeFacts::Truthy) {
            match ty.kind {
                TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword => {
                    return RTsKeywordType {
                        span: ty.span,
                        kind: TsKeywordTypeKind::TsNeverKeyword,
                    }
                }
                _ => {}
            }
        }

        let keyword_types = &[
            (TypeFacts::TypeofNEString, TsKeywordTypeKind::TsStringKeyword),
            (TypeFacts::TypeofNENumber, TsKeywordTypeKind::TsNumberKeyword),
            (TypeFacts::TypeofNEBoolean, TsKeywordTypeKind::TsBooleanKeyword),
            (TypeFacts::TypeofNEBigInt, TsKeywordTypeKind::TsBigIntKeyword),
            (TypeFacts::TypeofNESymbol, TsKeywordTypeKind::TsSymbolKeyword),
        ];

        for (neq, kwd) in keyword_types {
            if self.facts.contains(*neq) {
                return RTsKeywordType {
                    span: ty.span,
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                };
            }
        }
        {
            let keyword_types = &[
                (TypeFacts::TypeofEQString, TsKeywordTypeKind::TsStringKeyword),
                (TypeFacts::TypeofEQNumber, TsKeywordTypeKind::TsNumberKeyword),
                (TypeFacts::TypeofEQBoolean, TsKeywordTypeKind::TsBooleanKeyword),
                (TypeFacts::TypeofEQBigInt, TsKeywordTypeKind::TsBigIntKeyword),
                (TypeFacts::TypeofEQSymbol, TsKeywordTypeKind::TsSymbolKeyword),
            ];

            let has_any = keyword_types.iter().any(|&(fact, _)| self.facts.contains(fact));

            if has_any {
                let allowed_keywords = keyword_types
                    .iter()
                    .filter(|&&(fact, _)| self.facts.contains(fact))
                    .map(|v| v.1)
                    .collect::<Vec<_>>();

                if !allowed_keywords.contains(&ty.kind) {
                    return RTsKeywordType {
                        span: ty.span,
                        kind: TsKeywordTypeKind::TsNeverKeyword,
                    };
                }
            }
        }

        ty
    }
}

impl Fold<Union> for TypeFactsHandler {
    fn fold(&mut self, mut u: Union) -> Union {
        u = u.fold_children_with(self);

        u.types.retain(|v| !v.is_never());

        if self.facts.contains(TypeFacts::TypeofNEFunction) {
            u.types.retain(|ty| match ty.normalize() {
                Type::Function(..) => false,
                _ => true,
            });
        }

        if self.facts != TypeFacts::None {
            if self.facts.contains(TypeFacts::TypeofEQString) {
                u.types.retain(|ty| match ty.normalize() {
                    Type::Lit(RTsLitType {
                        lit: RTsLit::Str(..), ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) => true,
                    _ => false,
                });
            }
        }

        u
    }
}

impl Fold<Type> for TypeFactsHandler {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.foldable();
        ty = ty.fold_children_with(self);
        let span = ty.span();

        match ty {
            Type::Union(ref u) if u.types.is_empty() => return *Type::never(u.span),
            Type::Intersection(ref i) if i.types.iter().any(|ty| ty.is_never()) => return *Type::never(i.span),

            Type::Keyword(..) => {}

            Type::IndexedAccessType(IndexedAccessType { span, .. }) => {
                // Treat as any and apply type facts.
                let simple = facts_to_union(span, self.facts);
                if !simple.is_never() {
                    return *simple;
                }
            }
            _ => {}
        }

        ty
    }
}

fn facts_to_union(span: Span, facts: TypeFacts) -> Box<Type> {
    let mut types = vec![];
    if facts.contains(TypeFacts::TypeofEQString) {
        types.push(box Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsStringKeyword,
        }));
    }

    if facts.contains(TypeFacts::TypeofEQNumber) {
        types.push(box Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsNumberKeyword,
        }));
    }

    if facts.contains(TypeFacts::TypeofEQBoolean) {
        types.push(box Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsBooleanKeyword,
        }));
    }

    Type::union(types)
}
