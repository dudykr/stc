use super::Analyzer;
use crate::ValidationResult;
use stc_ts_types::ClassMember;
use stc_ts_types::ConstructorSignature;
use stc_ts_types::MethodSignature;
use stc_ts_types::PropertySignature;
use stc_ts_types::TypeElement;

impl Analyzer<'_, '_> {
    /// Utility method to convert a class member to a type element.
    ///
    /// This method is used while inferring types and while assigning type
    /// element to class member or vice versa.
    #[inline]
    pub(super) fn make_type_el_from_class_member(&self, member: &ClassMember) -> ValidationResult<TypeElement> {
        Ok(match member {
            ClassMember::Constructor(c) => TypeElement::Constructor(ConstructorSignature {}),
            ClassMember::Method(m) => TypeElement::Method(MethodSignature {}),
            ClassMember::Property(p) => TypeElement::Property(PropertySignature {}),
            ClassMember::IndexSignature(i) => TypeElement::Index(i.clone()),
        })
    }
}
