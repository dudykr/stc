use crate::{
    analyzer::Analyzer,
    debug::print_backtrace,
    ty::Type,
    util::{is_str_lit_or_union, EqIgnoreSpan, Marker, TypeEq},
};
use swc_atoms::js_word;
use swc_common::{Mark, Span, Spanned};
use swc_ecma_ast::{
    Expr, Number, Str, TsKeywordType, TsKeywordTypeKind, TsLit, TsLitType, TsTypeOperatorOp,
};
use swc_ts_types::{
    Array, Class, ClassMember, Fold, FoldWith, IndexedAccessType, Mapped, Operator, TypeElement,
    TypeLit, TypeParam, Union, VisitMutWith,
};

#[derive(Debug, Clone, Copy)]
pub(super) struct Config {
    /// If the mark is applied, it means that the type should not be
    /// generalized.
    prevent_mark: Mark,

    prevent_complex_simplification_mark: Mark,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            prevent_mark: Mark::fresh(Mark::root()),
            prevent_complex_simplification_mark: Mark::fresh(Mark::root()),
        }
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn may_generalize(&self, ty: &Type) -> bool {
        log::trace!("may_generalize({:?})", ty);
        match ty {
            Type::Function(f) => {
                if !self.may_generalize(&f.ret_ty) {
                    return false;
                }
                for param in &f.params {
                    if !self.may_generalize(&param.ty) {
                        return false;
                    }
                }
            }
            Type::Union(u) => {
                if u.types.iter().any(|ty| !self.may_generalize(ty)) {
                    return false;
                }
            }
            _ => {}
        }

        let mut ctxt = ty.span().ctxt().clone();
        loop {
            let m = ctxt.remove_mark();
            if m == Mark::root() {
                break;
            }

            if m == self.generalizer.prevent_mark {
                return false;
            }
        }

        true
    }

    pub(super) fn prevent_generalize(&self, ty: &mut Type) {
        let span = ty.span();
        let span = span.apply_mark(self.generalizer.prevent_mark);

        ty.respan(span)
    }

    pub(super) fn prevent_inference_while_simplifying(&self, ty: &mut Type) {
        ty.visit_mut_with(&mut Marker {
            mark: self.generalizer.prevent_complex_simplification_mark,
        });
    }

    pub(super) fn prevent_generalize_span(&self, span: Span) -> Span {
        span.apply_mark(self.generalizer.prevent_mark)
    }

    pub(super) fn simplify(&self, ty: Box<Type>) -> Box<Type> {
        ty.fold_with(&mut Simplifier {
            prevent_generalize_mark: self.generalizer.prevent_mark,
            prevent_simplification_mark: self.generalizer.prevent_complex_simplification_mark,
        })
    }
}

/// Simplifies the type.
struct Simplifier {
    prevent_generalize_mark: Mark,
    prevent_simplification_mark: Mark,
}

impl Simplifier {
    fn should_skip_inference(&mut self, span: Span) -> bool {
        let mut ctxt = span.ctxt;
        loop {
            let m = ctxt.remove_mark();
            if m == Mark::root() {
                break;
            }

            if m == self.prevent_simplification_mark {
                return true;
            }
        }

        false
    }
}

impl Fold for Simplifier {
    fn fold_union(&mut self, mut union: Union) -> Union {
        union.types.retain(|ty| {
            if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                | ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
            {
                return false;
            }

            true
        });

        union
    }

    fn fold_type(&mut self, mut ty: Type) -> Type {
        match ty {
            Type::Array(Array {
                elem_type:
                    box Type::IndexedAccessType(IndexedAccessType {
                        obj_type: box Type::Param(..),
                        index_type:
                            box Type::Param(TypeParam {
                                constraint:
                                    Some(box Type::Operator(Operator {
                                        op: TsTypeOperatorOp::KeyOf,
                                        ty: box Type::Param(..),
                                        ..
                                    })),
                                ..
                            }),
                        ..
                    }),
                ..
            }) => return ty,

            _ => {}
        }

        if self.should_skip_inference(ty.span()) {
            match ty.normalize() {
                Type::IndexedAccessType(IndexedAccessType {
                    obj_type:
                        box Type::Param(TypeParam {
                            constraint: Some(..),
                            ..
                        }),
                    ..
                }) => {
                    return ty;
                }
                _ => {}
            }
        }

        ty = ty.fold_children_with(self);

        match ty {
            Type::Union(u) if u.types.is_empty() => {
                return *Type::never(u.span);
            }

            // TODO: Handle optional and reaonly
            Type::Mapped(Mapped {
                type_param:
                    TypeParam {
                        name: p1,
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty,
                                ..
                            })),
                        ..
                    },
                ty:
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        obj_type,
                        index_type: box Type::Param(p2),
                        ..
                    })),
                ..
            }) if p1 == p2.name && obj_type == ty => return *ty,

            Type::IndexedAccessType(IndexedAccessType {
                span,
                readonly,
                obj_type: box Type::Intersection(obj),
                index_type:
                    index_type
                    @
                    box Type::Lit(TsLitType {
                        lit: TsLit::Str(..),
                        ..
                    }),
                ..
            }) if obj.types.iter().all(|ty| match &**ty {
                Type::TypeLit(..) => true,
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                    ..
                }) => true,
                _ => false,
            }) =>
            {
                let mut members = obj
                    .types
                    .into_iter()
                    .filter_map(|ty| match *ty {
                        Type::TypeLit(ty) => Some(ty.members),
                        _ => None,
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                members.dedup_by(|a, b| {
                    if let Some(a_key) = a.key() {
                        if let Some(b_key) = b.key() {
                            if a_key.eq_ignore_span(b_key) {
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                });

                return Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly,
                    obj_type: box Type::TypeLit(TypeLit { span, members }),
                    index_type,
                })
                .fold_with(self);
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                readonly,
                obj_type: box Type::Union(obj),
                index_type:
                    box Type::Lit(TsLitType {
                        lit: TsLit::Str(s), ..
                    }),
                ..
            }) if obj.types.iter().all(|ty| match &**ty {
                Type::TypeLit(..) => true,
                _ => false,
            }) =>
            {
                let mut types = obj
                    .types
                    .into_iter()
                    .map(|ty| match *ty {
                        Type::TypeLit(ty) => ty.members,
                        _ => unreachable!(),
                    })
                    .flatten()
                    .filter_map(|element| {
                        let span = element.span();

                        match element {
                            TypeElement::Property(p) => match &*p.key {
                                Expr::Ident(i) if i.sym == s.value => {
                                    Some(p.type_ann.unwrap_or_else(|| Type::any(span)))
                                }
                                _ => None,
                            },
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>();

                types.dedup_by(|a, b| a.type_eq(&*b));

                return *Type::union(types);
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: box Type::Tuple(tuple),
                index_type:
                    box Type::Lit(TsLitType {
                        lit:
                            TsLit::Str(Str {
                                value: js_word!("length"),
                                ..
                            }),
                        ..
                    }),
                ..
            }) => {
                let span = span.apply_mark(self.prevent_generalize_mark);
                return Type::Lit(TsLitType {
                    span,
                    lit: TsLit::Number(Number {
                        span,
                        value: tuple.elems.len() as _,
                    }),
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: box Type::Array(..),
                index_type:
                    box Type::Lit(TsLitType {
                        lit:
                            TsLit::Str(Str {
                                value: js_word!("length"),
                                ..
                            }),
                        ..
                    }),
                ..
            }) => {
                return Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: box Type::Tuple(tuple),
                index_type:
                    box Type::Lit(TsLitType {
                        lit: TsLit::Str(Str { value, .. }),
                        ..
                    }),
                ..
            }) if value.parse::<usize>().is_ok() => {
                let idx = value.parse::<usize>().unwrap();
                return tuple
                    .elems
                    .into_iter()
                    .map(|el| *el.ty)
                    .nth(idx)
                    .unwrap_or_else(|| *Type::never(span));
            }
            Type::Union(ty) if ty.types.len() == 1 => return *ty.types.into_iter().next().unwrap(),

            // Convert
            //
            //
            // [P in "value"]: {
            //     value: {
            //         [P in "value"]: T[P];
            //     };
            // }[P];
            //
            //
            // into
            //
            // [P in "value"]: {
            //     value: T['value'];
            // }[P];
            //
            // and then into
            //
            // [P in "value"]: {
            //     value: T['value'];
            // }['value'];
            Type::Mapped(Mapped {
                span,
                ty:
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        obj_type: box Type::TypeLit(TypeLit { members, .. }),
                        index_type: box Type::Param(index_type),
                        ..
                    })),
                type_param:
                    TypeParam {
                        name,
                        constraint: Some(constraint),
                        ..
                    },
                ..
            }) if index_type.name == name && is_str_lit_or_union(&constraint) => {
                let mut new_members = vec![];

                for member in &members {
                    for key in constraint.iter_union() {
                        let key = match key {
                            Type::Lit(TsLitType {
                                lit: TsLit::Str(v), ..
                            }) => v.clone(),
                            _ => unreachable!(),
                        };

                        if let Some(member_key) = member.key() {
                            let member_key = match member_key {
                                Expr::Ident(i) => i,
                                _ => unimplemented!(),
                            };

                            if member_key.sym != key.value {
                                continue;
                            }
                        }

                        match member {
                            TypeElement::Call(_) => unimplemented!(),
                            TypeElement::Constructor(_) => unimplemented!(),
                            TypeElement::Property(p) => new_members.push(p.clone().into()),
                            TypeElement::Method(_) => {
                                unimplemented!("Expansion of TypeElement::Method")
                            }
                            TypeElement::Index(_) => unimplemented!(),
                        }
                    }
                }

                return Type::TypeLit(TypeLit {
                    span,
                    members: new_members,
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Param(TypeParam {
                        constraint: Some(box Type::TypeLit(TypeLit { members, .. })),
                        ..
                    }),
                index_type:
                    box Type::Lit(TsLitType {
                        lit: TsLit::Str(v), ..
                    }),
                ..
            })
            | Type::IndexedAccessType(IndexedAccessType {
                obj_type: box Type::TypeLit(TypeLit { members, .. }),
                index_type:
                    box Type::Lit(TsLitType {
                        lit: TsLit::Str(v), ..
                    }),
                ..
            }) if members.iter().any(|element| match element.key() {
                Some(Expr::Ident(key)) => key.sym == v.value,
                _ => false,
            }) =>
            {
                let el = members
                    .into_iter()
                    .find(|element| match element.key() {
                        Some(Expr::Ident(key)) => key.sym == v.value,
                        _ => false,
                    })
                    .unwrap();

                match el {
                    TypeElement::Call(_) => {
                        unimplemented!("Generic mapped type inference involving `Call` element")
                    }
                    TypeElement::Constructor(_) => unimplemented!(
                        "Generic mapped type inference involving `Constructor` element"
                    ),
                    TypeElement::Property(p) => {
                        let span = p.span;
                        return *p.type_ann.unwrap_or_else(|| Type::any(span));
                    }
                    TypeElement::Method(_) => {
                        unimplemented!("Generic mapped type inference involving `Method` element")
                    }
                    TypeElement::Index(_) => unimplemented!(
                        "Generic mapped type inference involving IndexSignature element"
                    ),
                }
            }

            Type::IndexedAccessType(IndexedAccessType {
                obj_type: box Type::Class(Class { body, .. }),
                index_type:
                    box Type::Lit(TsLitType {
                        lit: TsLit::Str(s), ..
                    }),
                ..
            }) if body.iter().any(|member| match member {
                ClassMember::Constructor(_) => false,
                ClassMember::Method(_) => false,
                ClassMember::Property(p) => match &*p.key {
                    Expr::Ident(i) => i.sym == s.value,
                    _ => false,
                },
                ClassMember::IndexSignature(_) => false,
            }) =>
            {
                let member = body
                    .into_iter()
                    .find(|member| match member {
                        ClassMember::Constructor(_) => false,
                        ClassMember::Method(_) => false,
                        ClassMember::Property(p) => match &*p.key {
                            Expr::Ident(i) => i.sym == s.value,
                            _ => unreachable!(),
                        },
                        ClassMember::IndexSignature(_) => false,
                    })
                    .unwrap();

                match member {
                    ClassMember::Method(_) => unimplemented!(),
                    ClassMember::Property(p) => {
                        if let Some(value) = p.value {
                            return *value;
                        }

                        return *Type::any(p.span);
                    }
                    ClassMember::Constructor(_) => unreachable!(),
                    ClassMember::IndexSignature(_) => unreachable!(),
                }
            }

            Type::IndexedAccessType(IndexedAccessType {
                obj_type: box Type::Class(Class { body, .. }),
                index_type: box Type::Union(keys),
                ..
            }) if keys.types.iter().all(|ty| is_str_lit_or_union(&ty)) => {
                let mut new_types = keys
                    .types
                    .into_iter()
                    .map(|key| match *key {
                        Type::Lit(TsLitType {
                            lit: TsLit::Str(s), ..
                        }) => s,
                        _ => unreachable!(),
                    })
                    .map(|key| {
                        let member = body
                            .iter()
                            .find(|member| match member {
                                ClassMember::Constructor(_) => false,
                                ClassMember::Method(_) => false,
                                ClassMember::Property(p) => match &*p.key {
                                    Expr::Ident(i) => i.sym == key.value,
                                    _ => unreachable!(),
                                },
                                ClassMember::IndexSignature(_) => false,
                            })
                            .unwrap();

                        match member {
                            ClassMember::Method(_) => unimplemented!(),
                            ClassMember::Property(p) => {
                                if let Some(value) = &p.value {
                                    return value.clone();
                                }

                                return Type::any(p.span);
                            }
                            ClassMember::Constructor(_) => unreachable!(),
                            ClassMember::IndexSignature(_) => unreachable!(),
                        }
                    })
                    .collect::<Vec<_>>();

                new_types.dedup_by(|a, b| a.type_eq(&b));

                return *Type::union(new_types);
            }

            _ => {}
        }

        ty
    }
}
