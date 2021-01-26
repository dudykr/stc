use super::Analyzer;
use crate::ValidationResult;
use stc_ts_types::ClassMember;
use stc_ts_types::MethodSignature;
use stc_ts_types::PropertySignature;
use stc_ts_types::TypeElement;

impl Analyzer<'_, '_> {
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
