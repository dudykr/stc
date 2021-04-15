use crate::analyzer::assign::AssignOpts;
use crate::analyzer::Analyzer;
use crate::util::type_ext::TypeVecExt;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_types::Type;
use stc_ts_types::Union;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;

impl Analyzer<'_, '_> {
    pub(crate) fn narrowed_type_of_assignment(
        &mut self,
        span: Span,
        declared: Type,
        actual: &Type,
    ) -> ValidationResult {
        match actual.normalize() {
            Type::Ref(..) => {
                let actual = self
                    .expand_top_ref(span, Cow::Borrowed(actual))
                    .context("tried to expand actual type to calculate narrowed type")?;

                return self.narrowed_type_of_assignment(span, declared, &actual);
            }
            Type::Union(actual) => {
                let mut new_types = vec![];
                for actual in &actual.types {
                    let ty = self.narrowed_type_of_assignment(span, declared.clone(), &actual)?;
                    new_types.push(ty);
                }

                new_types.dedup_type();

                new_types.retain(|ty| !ty.is_never());
                if new_types.is_empty() {
                    return Ok(Type::never(actual.span));
                }

                return Ok(Type::Union(Union {
                    span: actual.span,
                    types: new_types,
                }));
            }
            _ => {}
        }

        match declared.normalize() {
            Type::Ref(..) => {
                let declared = self
                    .expand_top_ref(span, Cow::Owned(declared))
                    .context("tried to expand declared type to calculate narrowed type")?
                    .into_owned();

                return self.narrowed_type_of_assignment(span, declared, actual);
            }
            _ => {}
        }

        let declared = declared.foldable();

        match declared {
            Type::Union(declared) => {
                let mut new_types = vec![];
                for declared in declared.types {
                    let ty = self.narrowed_type_of_assignment(span, declared, actual)?;
                    new_types.push(ty);
                }

                new_types.dedup_type();

                new_types.retain(|ty| !ty.is_never());
                if new_types.is_empty() {
                    return Ok(Type::never(actual.span()));
                }

                Ok(Type::Union(Union {
                    span: actual.span(),
                    types: new_types,
                }))
            }
            _ => {
                if let Ok(()) = self.assign_with_opts(
                    AssignOpts {
                        span,
                        allow_unknown_rhs: true,
                        ..Default::default()
                    },
                    &declared,
                    &actual,
                ) {
                    return Ok(declared);
                }

                Ok(Type::never(actual.span()))
            }
        }
    }
}
