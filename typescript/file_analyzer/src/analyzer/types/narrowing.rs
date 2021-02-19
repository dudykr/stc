use crate::analyzer::Analyzer;
use crate::util::type_ext::TypeVecExt;
use crate::ValidationResult;
use stc_ts_types::Type;
use stc_ts_types::Union;
use swc_common::Span;

impl Analyzer<'_, '_> {
    pub(crate) fn narrowed_type_of_assignment(
        &mut self,
        span: Span,
        declared: Type,
        actual: &Type,
    ) -> ValidationResult {
        let declared = declared.foldable();

        match declared {
            Type::Union(declared) => {
                let mut new_types = vec![];
                for declared in declared.types {
                    let ty = self.narrowed_type_of_assignment(span, declared, actual)?;
                    new_types.push(ty);
                }

                new_types.dedup_type();

                Ok(Type::Union(Union {
                    span: declared.span,
                    types: new_types,
                }))
            }
            _ => {
                if let Ok(()) = self.assign(&declared, &actual, span) {
                    return Ok(declared);
                }

                Ok(actual.clone())
            }
        }
    }
}
