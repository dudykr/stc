use crate::{analyzer::Analyzer, type_facts::TypeFacts};
use rnode::{Fold, FoldWith, NodeId};
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RPat, RRestPat, RTsKeywordType, RTsLit, RTsLitType};
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_type_ops::Fix;
use stc_ts_types::{
    ClassDef, ClassMember, Conditional, Constructor, FnParam, Function, IndexedAccessType, Intersection, Mapped, Type,
    TypeElement, TypeLit, Union,
};
use stc_ts_utils::MapWithMut;
use std::borrow::Cow;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::{debug, instrument};

impl Analyzer<'_, '_> {
    /// TODO: Note: This method preserves [Type::Ref] in some cases.
    ///
    /// Those are preserved if
    ///
    ///  - it's Promise<T>
    #[instrument(skip(self, facts, ty))]
    pub fn apply_type_facts_to_type(&mut self, facts: TypeFacts, mut ty: Type) -> Type {
        if self.is_builtin {
            return ty;
        }

        if facts.contains(TypeFacts::TypeofEQNumber)
            || facts.contains(TypeFacts::TypeofEQString)
            || facts.contains(TypeFacts::TypeofEQBoolean)
        {
            match ty {
                Type::Param(..) | Type::IndexedAccessType(..) => {
                    ty = Type::Intersection(Intersection {
                        span: ty.span(),
                        types: vec![ty],
                    });
                }
                _ => {}
            }
        }

        let cnt = if facts.contains(TypeFacts::TypeofEQString) {
            1
        } else {
            0
        } + if facts.contains(TypeFacts::TypeofEQNumber) {
            1
        } else {
            0
        } + if facts.contains(TypeFacts::TypeofEQBigInt) {
            1
        } else {
            0
        } + if facts.contains(TypeFacts::TypeofEQBoolean) {
            1
        } else {
            0
        };
        if cnt >= 2 {
            return Type::never(ty.span());
        }

        let before = dump_type_as_string(&self.cm, &ty);
        ty = ty.fold_with(&mut TypeFactsHandler { analyzer: self, facts });

        // Add `(...args: any) => any` for typeof foo === 'function'
        if facts.contains(TypeFacts::TypeofEQFunction) {
            let param = FnParam {
                span: DUMMY_SP,
                pat: RPat::Rest(RRestPat {
                    span: DUMMY_SP,
                    dot3_token: DUMMY_SP,
                    node_id: NodeId::invalid(),
                    arg: box RPat::Ident(RBindingIdent {
                        node_id: NodeId::invalid(),
                        id: RIdent::new("args".into(), DUMMY_SP),
                        type_ann: None,
                    }),
                    type_ann: None,
                }),
                ty: box Type::any(DUMMY_SP),
                required: false,
            };
            let fn_type = Type::Function(Function {
                span: DUMMY_SP,
                type_params: None,
                params: vec![param],
                ret_ty: box Type::any(DUMMY_SP),
            });
            match ty.normalize_mut() {
                Type::Union(u) => {
                    let has_fn = u.types.iter().any(|ty| match ty.normalize() {
                        Type::Function(..) => true,
                        _ => false,
                    });

                    if !has_fn {
                        u.types.push(fn_type)
                    }
                }
                ty => {
                    *ty = Type::Union(Union {
                        span: ty.span(),
                        types: vec![ty.take(), fn_type],
                    })
                }
            }
        }

        let after = dump_type_as_string(&self.cm, &ty);

        debug!("[types/fact] {} => {}\nTypeFacts: {:?}", before, after, facts);

        ty.fixed()
    }
}

struct TypeFactsHandler<'a, 'b, 'c> {
    /// Used to expand references.
    analyzer: &'a mut Analyzer<'b, 'c>,
    facts: TypeFacts,
}

impl TypeFactsHandler<'_, '_, '_> {
    #[instrument(skip(self, ty))]
    fn can_be_primitive(&mut self, ty: &Type) -> bool {
        let ty = if let Ok(ty) = self
            .analyzer
            .expand_top_ref(ty.span(), Cow::Borrowed(ty), Default::default())
        {
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

impl Fold<Conditional> for TypeFactsHandler<'_, '_, '_> {
    #[inline]
    fn fold(&mut self, ty: Conditional) -> Conditional {
        ty
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

        if ty.kind == TsKeywordTypeKind::TsNullKeyword && self.facts.contains(TypeFacts::NENull) {
            return RTsKeywordType {
                kind: TsKeywordTypeKind::TsNeverKeyword,
                ..ty
            };
        }

        if ty.kind == TsKeywordTypeKind::TsUndefinedKeyword && self.facts.contains(TypeFacts::NEUndefined) {
            return RTsKeywordType {
                kind: TsKeywordTypeKind::TsNeverKeyword,
                ..ty
            };
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

impl Fold<Intersection> for TypeFactsHandler<'_, '_, '_> {
    fn fold(&mut self, ty: Intersection) -> Intersection {
        let mut ty = ty.fold_children_with(self);

        let has_keyword = |kind| ty.types.iter().any(|ty| ty.normalize().is_kwd(kind));

        // TODO: Support literal type.
        let has_str = has_keyword(TsKeywordTypeKind::TsStringKeyword);
        let has_num = has_keyword(TsKeywordTypeKind::TsNumberKeyword);
        let has_bool = has_keyword(TsKeywordTypeKind::TsBooleanKeyword);

        if !has_str && self.facts.contains(TypeFacts::TypeofEQString) {
            ty.types.push(Type::Keyword(RTsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsStringKeyword,
            }));
        }

        if !has_num && self.facts.contains(TypeFacts::TypeofEQNumber) {
            ty.types.push(Type::Keyword(RTsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsNumberKeyword,
            }));
        }

        if !has_bool && self.facts.contains(TypeFacts::TypeofEQBoolean) {
            ty.types.push(Type::Keyword(RTsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsBooleanKeyword,
            }));
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

/// Noop because type facts should not be applied recursively.
impl Fold<ClassDef> for TypeFactsHandler<'_, '_, '_> {
    fn fold(&mut self, ty: ClassDef) -> ClassDef {
        ty
    }
}

/// Noop because type facts should not be applied recursively.
impl Fold<Mapped> for TypeFactsHandler<'_, '_, '_> {
    fn fold(&mut self, ty: Mapped) -> Mapped {
        ty
    }
}

impl Fold<Type> for TypeFactsHandler<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        let span = ty.span();

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

        if !span.is_dummy() {
            if ty.normalize().is_ref_type() {
                if let Ok(ty) = self
                    .analyzer
                    .expand_top_ref(ty.span(), Cow::Borrowed(&ty), Default::default())
                {
                    if ty.normalize().is_ref_type() {
                        return ty.into_owned();
                    }
                    return ty.into_owned().fold_with(self);
                } else {
                    return ty;
                }
            }
        }

        match ty.normalize() {
            Type::Class(..) | Type::ClassDef(..) | Type::TypeLit(..)
                if self.facts.contains(TypeFacts::TypeofNEObject) =>
            {
                return Type::never(span);
            }
            _ => {}
        }

        ty = ty.foldable();
        ty = ty.fold_children_with(self);

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
