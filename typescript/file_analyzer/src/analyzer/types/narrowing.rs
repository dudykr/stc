use crate::analyzer::assign::AssignOpts;
use crate::analyzer::Analyzer;
use crate::util::type_ext::TypeVecExt;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_type_ops::Fix;
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
        declared.assert_valid();
        actual.assert_valid();

        let declared = self
            .normalize(Some(span), Cow::Owned(declared), Default::default())
            .context("tried to normalize decalred type")?;
        let actual = self
            .normalize(Some(span), Cow::Borrowed(&actual), Default::default())
            .context("tried to normalize decalred type")?;

        match actual.normalize() {
            Type::Union(actual) => {
                let mut new_types = vec![];
                for actual in &actual.types {
                    let ty = self.narrowed_type_of_assignment(span, declared.clone().into_owned(), &actual)?;
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
                })
                .fixed());
            }
            _ => {}
        }

        let declared = declared.into_owned().foldable();

        match declared {
            Type::Union(declared) => {
                let mut new_types = vec![];
                for declared in declared.types {
                    let ty = self.narrowed_type_of_assignment(span, declared, &actual)?;
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
                })
                .fixed())
            }
            _ => {
                if let Ok(()) = self.assign_with_opts(
                    &mut Default::default(),
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
