use super::Type;
use crate::analyzer::Analyzer;
use crate::type_facts::TypeFacts;
use rnode::Fold;
use rnode::FoldWith;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_types::ClassMember;
use stc_ts_types::Constructor;
use stc_ts_types::Function;
use stc_ts_types::IndexedAccessType;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::Union;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

pub(crate) struct TypeFactsHandler<'a, 'b, 'c> {
    /// Used to expand references.
    pub analyzer: &'a mut Analyzer<'b, 'c>,
    pub facts: TypeFacts,
}

impl TypeFactsHandler<'_, '_, '_> {
    fn can_be_primitive(&mut self, ty: &Type) -> bool {
        let ty = if let Ok(ty) = self.analyzer.expand_top_ref(ty.span(), Cow::Borrowed(ty)) {
            ty
        } else {
            return true;
        };

        match ty.normalize() {
            Type::Interface(..) | Type::TypeLit(..) | Type::Class(..) => return false,
            _ => {}
        }

        true
    }
}

impl Fold<TypeElement> for TypeFactsHandler<'_, '_, '_> {
    #[inline]
    fn fold(&mut self, el: TypeElement) -> TypeElement {
        el
    }
}

impl Fold<ClassMember> for TypeFactsHandler<'_, '_, '_> {
    #[inline]
    fn fold(&mut self, m: ClassMember) -> ClassMember {
        m
    }
}

impl Fold<Function> for TypeFactsHandler<'_, '_, '_> {
    #[inline]
    fn fold(&mut self, m: Function) -> Function {
        m
    }
}

impl Fold<Constructor> for TypeFactsHandler<'_, '_, '_> {
    #[inline]
    fn fold(&mut self, m: Constructor) -> Constructor {
        m
    }
}

impl Fold<RTsKeywordType> for TypeFactsHandler<'_, '_, '_> {
    fn fold(&mut self, ty: RTsKeywordType) -> RTsKeywordType {
        if self.facts.contains(TypeFacts::Truthy) {
            match ty.kind {
                TsKeywordTypeKind::TsVoidKeyword
                | TsKeywordTypeKind::TsUndefinedKeyword
                | TsKeywordTypeKind::TsNullKeyword => {
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

        if ty.kind != TsKeywordTypeKind::TsAnyKeyword {
            for (neq, kwd) in keyword_types {
                if self.facts.contains(*neq) && *kwd == ty.kind {
                    return RTsKeywordType {
                        span: ty.span,
                        kind: TsKeywordTypeKind::TsNeverKeyword,
                    };
                }
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

impl Fold<Union> for TypeFactsHandler<'_, '_, '_> {
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
            if self.facts.contains(TypeFacts::TypeofEQString)
                || self.facts.contains(TypeFacts::TypeofEQBoolean)
                || self.facts.contains(TypeFacts::TypeofEQNumber)
            {
                u.types.retain(|ty| match ty.normalize() {
                    Type::Lit(RTsLitType {
                        lit: RTsLit::Str(..), ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) if !self.facts.contains(TypeFacts::TypeofEQString) => false,

                    Type::Lit(RTsLitType {
                        lit: RTsLit::Bool(..), ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                        ..
                    }) if !self.facts.contains(TypeFacts::TypeofEQBoolean) => false,

                    Type::Lit(RTsLitType {
                        lit: RTsLit::Number(..),
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) if !self.facts.contains(TypeFacts::TypeofEQNumber) => false,

                    Type::Param(..) => false,

                    _ => self.can_be_primitive(&ty),
                });
            }
        }

        u
    }
}

impl Fold<Type> for TypeFactsHandler<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO: Don't do anything if type fact is none.

        match ty.normalize() {
            Type::Lit(RTsLitType {
                span,
                lit: RTsLit::Bool(v),
                ..
            }) => {
                if self.facts.contains(TypeFacts::Truthy) && !v.value {
                    return Type::never(*span);
                }

                if self.facts.contains(TypeFacts::Falsy) && v.value {
                    return Type::never(*span);
                }
            }
            _ => {}
        }

        ty = ty.foldable();
        ty = ty.fold_children_with(self);
        let span = ty.span();

        match ty {
            Type::Union(ref u) if u.types.is_empty() => return Type::never(u.span),
            Type::Union(u) if u.types.len() == 1 => return u.types.into_iter().next().unwrap(),
            Type::Intersection(ref i) if i.types.iter().any(|ty| ty.is_never()) => return Type::never(i.span),

            Type::Keyword(..) => {}

            Type::IndexedAccessType(IndexedAccessType { span, .. }) | Type::TypeLit(TypeLit { span, .. }) => {
                // Treat as any and apply type facts.
                let simple = facts_to_union(span, self.facts);
                if !simple.is_never() {
                    return simple;
                }
            }
            _ => {}
        }

        ty
    }
}

fn facts_to_union(span: Span, facts: TypeFacts) -> Type {
    let mut types = vec![];
    if facts.contains(TypeFacts::TypeofEQString) {
        types.push(Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsStringKeyword,
        }));
    }

    if facts.contains(TypeFacts::TypeofEQNumber) {
        types.push(Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsNumberKeyword,
        }));
    }

    if facts.contains(TypeFacts::TypeofEQBoolean) {
        types.push(Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsBooleanKeyword,
        }));
    }

    Type::union(types)
}
