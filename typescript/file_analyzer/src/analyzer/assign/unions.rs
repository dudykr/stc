use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    ValidationResult,
};
use itertools::Itertools;
use stc_ts_errors::{DebugExt, Error};
use stc_ts_types::{Tuple, TupleElement, Type, Union};
use std::borrow::Cow;
use swc_common::Span;

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
            Ok(r) => {
                if r.normalize().is_union_type() {
                    Some(
                        self.assign_with_opts(data, opts, l, &r)
                            .context("tried to assign to a "),
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

        match &*ty {
            Type::Tuple(ty) => {
                let mut tuple = Type::Tuple(Tuple {
                    span: ty.span,
                    elems: Default::default(),
                });

                for el in &ty.elems {
                    self.append_tuple_element_to_tuple(span, &mut tuple, el)
                        .context("tried to append an element to a type")?;
                }

                Ok(tuple)
            }
            _ => Ok(ty.into_owned()),
        }
    }

    /// TODO: Use Cow<TupleElement>
    fn append_tuple_element_to_tuple(&mut self, span: Span, to: &mut Type, el: &TupleElement) -> ValidationResult<()> {
        match el.ty.normalize() {
            Type::Union(el_ty) => {
                let mut to_types = (0..el_ty.types.len()).map(|_| to.clone()).collect_vec();

                for (idx, el_ty) in el_ty.types.iter().enumerate() {
                    self.append_tuple_element_to_tuple(
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
                });

                return Ok(());
            }
            _ => {}
        }

        match to.normalize_mut() {
            Type::Union(to) => {
                for to in &mut to.types {
                    self.append_tuple_element_to_tuple(span, to, el)?;
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
