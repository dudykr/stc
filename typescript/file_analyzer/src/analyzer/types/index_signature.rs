use crate::{analyzer::Analyzer, ValidationResult};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_types::{ClassDef, IndexSignature, Type};
use std::borrow::Cow;
use swc_common::Span;

impl Analyzer<'_, '_> {
    pub(crate) fn get_index_signature(&mut self, span: Span, ty: &Type) -> ValidationResult<Option<IndexSignature>> {
        (|| -> ValidationResult<_> {
            let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default())?;

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
        Ok(None)
    }
}
