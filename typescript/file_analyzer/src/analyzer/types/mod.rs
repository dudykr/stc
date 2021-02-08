use super::Analyzer;
use crate::util::type_ext::TypeVecExt;
use crate::ValidationResult;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_types::Array;
use stc_ts_types::ClassMember;
use stc_ts_types::MethodSignature;
use stc_ts_types::PropertySignature;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_utils::MapWithMut;
use std::borrow::Cow;
use swc_common::Spanned;

impl Analyzer<'_, '_> {
    pub(crate) fn type_to_type_lit<'a>(&mut self, ty: &'a Type) -> ValidationResult<Option<Cow<'a, TypeLit>>> {
        let ty = ty.normalize();

        Ok(Some(match ty {
            Type::Ref(..) => {
                let ty = self.expand_top_ref(ty.span(), Cow::Borrowed(ty))?;
                return self
                    .type_to_type_lit(&ty)
                    .map(|o| o.map(Cow::into_owned).map(Cow::Owned));
            }

            Type::TypeLit(t) => Cow::Borrowed(t),
            Type::Enum(e) => self.enum_to_type_lit(e).map(Cow::Owned)?,
            Type::Class(c) => {
                let mut els = vec![];
                if let Some(s) = &c.super_class {
                    let super_els = self.type_to_type_lit(s)?;
                    els.extend(super_els.map(|ty| ty.into_owned().members).into_iter().flatten());
                }

                // TODO: Override

                for member in &c.body {
                    els.extend(self.make_type_el_from_class_member(member)?);
                }

                Cow::Owned(TypeLit {
                    span: c.span,
                    members: els,
                })
            }
            _ => return Ok(None),
        }))
    }
    pub(crate) fn normalize_tuples(&mut self, ty: &mut Type) {
        ty.visit_mut_with(&mut TupleNormalizer);
    }

    pub(crate) fn kinds_of_type_elements(&mut self, els: &[TypeElement]) -> Vec<u8> {
        let mut v = els
            .iter()
            .map(|v| match v {
                TypeElement::Call(_) => 1,
                TypeElement::Constructor(_) => 2,
                TypeElement::Property(_) => 3,
                TypeElement::Method(_) => 4,
                TypeElement::Index(_) => 5,
            })
            .collect::<Vec<_>>();
        v.sort();
        v
    }

    /// Utility method to convert a class member to a type element.
    ///
    /// This method is used while inferring types and while assigning type
    /// element to class member or vice versa.
    #[inline]
    pub(super) fn make_type_el_from_class_member(&self, member: &ClassMember) -> ValidationResult<Option<TypeElement>> {
        Ok(Some(match member {
            ClassMember::Constructor(c) => TypeElement::Constructor(c.clone()),
            ClassMember::Method(m) => {
                if m.is_static {
                    return Ok(None);
                }
                TypeElement::Method(MethodSignature {
                    span: m.span,
                    key: m.key.clone(),
                    type_params: m.type_params.clone(),
                    params: m.params.clone(),
                    optional: m.is_optional,
                    ret_ty: Some(m.ret_ty.clone()),
                    readonly: false,
                })
            }
            ClassMember::Property(p) => {
                if p.is_static {
                    return Ok(None);
                }

                TypeElement::Property(PropertySignature {
                    span: p.span,
                    key: p.key.clone(),
                    params: vec![],
                    optional: p.is_optional,
                    type_params: None,
                    readonly: p.readonly,
                    type_ann: p.value.clone(),
                })
            }
            ClassMember::IndexSignature(i) => TypeElement::Index(i.clone()),
        }))
    }
}

struct TupleNormalizer;

impl VisitMut<Type> for TupleNormalizer {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match ty.normalize() {
            Type::Tuple(..) => {
                let span = ty.span();
                let mut types = ty
                    .take()
                    .foldable()
                    .tuple()
                    .unwrap()
                    .elems
                    .into_iter()
                    .map(|elem| elem.ty)
                    .collect::<Vec<_>>();
                types.dedup_type();

                *ty = Type::Array(Array {
                    span,
                    elem_type: Type::union(types),
                });
            }
            _ => {}
        }
    }
}
