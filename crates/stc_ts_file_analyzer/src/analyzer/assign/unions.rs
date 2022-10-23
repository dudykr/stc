use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    ValidationResult,
};
use itertools::Itertools;
use stc_ts_errors::{DebugExt, Error};
use stc_ts_types::{
    KeywordType, PropertySignature, Tuple, TupleElement, Type, TypeElement, TypeLit, Union, UnionMetadata,
};
use stc_utils::cache::Freeze;
use std::borrow::Cow;
use swc_common::Span;
use swc_ecma_ast::TsKeywordTypeKind;

impl Analyzer<'_, '_> {
    /// # Cases
    ///
    /// Cases handled by this methods are
    ///
    ///  - lhs = `(["a", number] | ["b", number] | ["c", string]);`
    ///  - rhs = `[("b" | "a"), 1];`
    pub(super) fn assign_to_union(
        &mut self,
        data: &mut AssignData,
        l: &Type,
        r: &Type,
        opts: AssignOpts,
    ) -> Option<ValidationResult<()>> {
        let r_res = self.flatten_unions_for_assignment(opts.span, Cow::Borrowed(r));

        match r_res {
            Ok(mut r) => {
                r.make_clone_cheap();

                if r.normalize().is_union_type() {
                    Some(
                        self.assign_with_opts(data, opts, l, &r)
                            .context("tried to assign to a flattened union to another union"),
                    )
                } else {
                    None
                }
            }
            Err(_) => None,
        }
    }

    fn flatten_unions_for_assignment(&mut self, span: Span, ty: Cow<Type>) -> ValidationResult<Type> {
        let ty = self.normalize(Some(span), ty, Default::default())?;

        match ty.normalize() {
            Type::Tuple(ty) => {
                let mut tuple = Type::Tuple(Tuple {
                    elems: Default::default(),
                    ..*ty
                });

                for el in &ty.elems {
                    self.append_tuple_element_to_type(span, &mut tuple, el)
                        .context("tried to append an element to a type")?;
                }

                Ok(tuple)
            }
            Type::TypeLit(ty) => {
                let mut type_lit = Type::TypeLit(TypeLit {
                    members: Default::default(),
                    ..*ty
                });

                for el in &ty.members {
                    self.append_type_element_to_type(span, &mut type_lit, el)
                        .context("tried to append an element to a type")?;
                }

                Ok(type_lit)
            }
            _ => Ok(ty.into_owned()),
        }
    }

    /// TODO(kdy1): Use Cow<TupleElement>
    fn append_type_element_to_type(&mut self, span: Span, to: &mut Type, el: &TypeElement) -> ValidationResult<()> {
        match el {
            TypeElement::Property(el) => {
                if let Some(el_ty) = &el.type_ann {
                    if let Some(ty) = expand_union(&el_ty) {
                        let mut to_types = (0..ty.types.len()).map(|_| to.clone()).collect_vec();

                        for (idx, el_ty) in ty.types.iter().enumerate() {
                            self.append_type_element_to_type(
                                span,
                                &mut to_types[idx],
                                &TypeElement::Property(PropertySignature {
                                    type_ann: Some(box el_ty.clone()),
                                    ..el.clone()
                                }),
                            )?;
                        }

                        *to = Type::Union(Union {
                            span: ty.span,
                            types: to_types,
                            metadata: ty.metadata,
                        });

                        return Ok(());
                    }
                }
            }

            _ => {}
        }

        match to.normalize_mut() {
            Type::Union(to) => {
                for to in &mut to.types {
                    self.append_type_element_to_type(span, to, el)?;
                }

                Ok(())
            }
            Type::TypeLit(to) => {
                to.members.push(el.clone());

                Ok(())
            }
            _ => Err(Error::SimpleAssignFailed { span, cause: None }),
        }
    }

    /// TODO(kdy1): Use Cow<TupleElement>
    fn append_tuple_element_to_type(&mut self, span: Span, to: &mut Type, el: &TupleElement) -> ValidationResult<()> {
        if let Some(el_ty) = expand_union(&el.ty) {
            let mut to_types = (0..el_ty.types.len()).map(|_| to.clone()).collect_vec();

            for (idx, el_ty) in el_ty.types.iter().enumerate() {
                self.append_tuple_element_to_type(
                    span,
                    &mut to_types[idx],
                    &TupleElement {
                        span: el.span,
                        label: el.label.clone(),
                        ty: box el_ty.clone(),
                    },
                )?;
            }

            *to = Type::Union(Union {
                span: el_ty.span,
                types: to_types,
                metadata: el_ty.metadata,
            });

            return Ok(());
        }

        match to.normalize_mut() {
            Type::Union(to) => {
                for to in &mut to.types {
                    self.append_tuple_element_to_type(span, to, el)?;
                }

                Ok(())
            }
            Type::Tuple(to) => {
                to.elems.push(el.clone());

                Ok(())
            }
            _ => Err(Error::SimpleAssignFailed { span, cause: None }),
        }
    }
}

/// Expands `boolean` to `true | false`.
fn expand_union(t: &Type) -> Option<Cow<Union>> {
    match t.normalize() {
        Type::Keyword(KeywordType {
            span,
            metadata,
            kind: TsKeywordTypeKind::TsBooleanKeyword,
            ..
        }) => Some(Cow::Owned(Union {
            span: *span,
            types: Vec::new(),
            metadata: UnionMetadata {
                common: metadata.common,
                ..Default::default()
            },
        })),
        Type::Union(ty) => Some(Cow::Borrowed(ty)),
        _ => None,
    }
}
