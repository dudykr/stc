use crate::{analyzer::Analyzer, ValidationResult};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_types::{ClassDef, ClassMember, IndexSignature, Type};
use stc_utils::ext::ValueExt;
use std::borrow::Cow;
use swc_common::Span;

impl Analyzer<'_, '_> {
    /// Get [IndexSignature] from `ty`, if there's one.
    pub(crate) fn get_index_signature(&mut self, span: Span, ty: &Type) -> ValidationResult<Option<IndexSignature>> {
        (|| -> ValidationResult<_> {
            let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default())?;

            // TODO: Support type literals and interfaces.

            match &*ty {
                Type::ClassDef(cls) => return self.get_index_signature_from_class(span, &cls),
                _ => Ok(None),
            }
        })()
        .with_context(|| {
            format!(
                "tried to get index signature of '{}'",
                dump_type_as_string(&self.cm, &ty)
            )
        })
    }

    pub(crate) fn get_index_signature_from_class(
        &mut self,
        span: Span,
        class: &ClassDef,
    ) -> ValidationResult<Option<IndexSignature>> {
        for member in &class.body {
            match member {
                ClassMember::IndexSignature(i) => return i.clone().as_some().as_ok(),
                _ => {}
            }
        }

        if let Some(super_class) = &class.super_class {
            return self.get_index_signature(span, &super_class);
        }

        Ok(None)
    }
}
