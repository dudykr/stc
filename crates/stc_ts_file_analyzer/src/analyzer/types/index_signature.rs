use std::borrow::Cow;

use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_types::{ClassDef, ClassMember, IndexSignature, Type};
use stc_utils::ext::ValueExt;
use swc_common::Span;

use crate::{analyzer::Analyzer, VResult};

impl Analyzer<'_, '_> {
    /// Get [IndexSignature] from `ty`, if there's one.
    pub(crate) fn get_index_signature(&mut self, span: Span, ty: &Type) -> VResult<Option<IndexSignature>> {
        (|| -> VResult<_> {
            let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default())?;

            // TODO(kdy1): Support type literals and interfaces.

            match ty.normalize() {
                Type::ClassDef(cls) => return self.get_index_signature_from_class(span, &cls),
                _ => Ok(None),
            }
        })()
        .with_context(|| format!("tried to get index signature of '{}'", dump_type_as_string(&self.cm, &ty)))
    }

    pub(crate) fn get_index_signature_from_class(&mut self, span: Span, class: &ClassDef) -> VResult<Option<IndexSignature>> {
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
