use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::Error;
use stc_ts_types::Id;
use stc_ts_types::Type;
use swc_common::Span;

/// Handles merging of types.
///
/// These methods are used while merging declarations like class and interface
/// or vice versa.
impl Analyzer<'_, '_> {
    pub(crate) fn merge_type_decl(&mut self, span: Span, name: &Id, orig: &Type, new: &Type) -> ValidationResult<Type> {
        let err = match self.merge_type_decl_from_to(span, name, orig, new) {
            Ok(v) => return Ok(v),
            Err(err) => err,
        };

        self.merge_type_decl_from_to(span, name, new, orig)
            .or_else(|_| Err(err))
    }
    fn merge_type_decl_from_to(&mut self, span: Span, name: &Id, orig: &Type, new: &Type) -> ValidationResult<Type> {
        let orig = orig.normalize();
        let new = new.normalize();
        match (orig, new) {
            (Type::ClassDef(l), Type::Interface(r)) => {
                //
            }
        }

        Err(Error::DuplicateName {
            span,
            name: name.clone(),
        })
    }
}
