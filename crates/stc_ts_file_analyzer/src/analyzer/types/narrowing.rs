use std::borrow::Cow;

use stc_ts_errors::DebugExt;
use stc_ts_type_ops::Fix;
use stc_ts_types::{KeywordTypeMetadata, Type, Union, UnionMetadata};
use stc_utils::{cache::Freeze, ext::TypeVecExt};
use swc_common::{Span, Spanned};

use super::NormalizeTypeOpts;
use crate::{
    analyzer::{assign::AssignOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(crate) fn narrowed_type_of_assignment(&mut self, span: Span, declared: Type, actual: &Type) -> VResult<Type> {
        declared.assert_valid();
        actual.assert_valid();

        let opts = NormalizeTypeOpts {
            in_type: true,
            ..Default::default()
        };

        let mut declared = self
            .normalize(Some(span), Cow::Owned(declared), opts)
            .context("tried to normalize declared type")?;
        declared.freeze();

        let mut actual = self
            .normalize(Some(span), Cow::Borrowed(actual), opts)
            .context("tried to normalize declared type")?;
        actual.freeze();

        if let Type::Union(actual) = actual.normalize() {
            let mut new_types = vec![];
            for actual in &actual.types {
                let ty = self.narrowed_type_of_assignment(span, declared.clone().into_owned(), actual)?;
                new_types.push(ty);
            }

            new_types.dedup_type();

            new_types.retain(|ty| !ty.is_never());
            if new_types.is_empty() {
                return Ok(Type::never(
                    actual.span,
                    KeywordTypeMetadata {
                        common: actual.metadata.common,
                        ..Default::default()
                    },
                ));
            }

            return Ok(Type::Union(Union {
                span: actual.span,
                types: new_types,
                metadata: actual.metadata,
                tracker: Default::default(),
            })
            .fixed());
        }

        let mut declared = declared.into_owned();
        declared.normalize_mut();
        // TODO(kdy1): PERF

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
                    return Ok(Type::never(
                        actual.span(),
                        KeywordTypeMetadata {
                            common: actual.metadata(),
                            ..Default::default()
                        },
                    ));
                }

                Ok(Type::Union(Union {
                    span: actual.span(),
                    types: new_types,
                    metadata: UnionMetadata {
                        common: actual.metadata(),
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
                .fixed())
            }
            _ => {
                if let Ok(()) = self.assign_with_opts(
                    &mut Default::default(),
                    &declared,
                    &actual,
                    AssignOpts {
                        span,
                        allow_unknown_rhs: Some(true),
                        ..Default::default()
                    },
                ) {
                    return Ok(declared);
                }

                Ok(Type::never(
                    actual.span(),
                    KeywordTypeMetadata {
                        common: actual.metadata(),
                        ..Default::default()
                    },
                ))
            }
        }
    }
}
