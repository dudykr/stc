use std::borrow::Cow;

use stc_ts_ast_rnode::RPat;
use stc_ts_types::{FnParam, RestType, TupleElement, Type, TypeOrSpread};
use stc_utils::cache::Freeze;
use swc_common::Span;

use super::NormalizeTypeOpts;
use crate::{
    analyzer::{expr::GetIteratorOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(crate) fn relate_spread_likes<T, S, F>(&mut self, span: Span, targets: &[T], sources: &[S], relate: F) -> VResult<()>
    where
        T: SpreadLike,
        S: SpreadLike,
        F: FnMut(&mut Self, &TypeOrSpread, &TypeOrSpread),
    {
        let mut targets_iter = targets.iter();
        let mut sources_iter = sources.iter();

        loop {
            let source = match sources_iter.next() {
                Some(source) => source,
                None => break,
            };
            let target = match targets_iter.next() {
                Some(target) => target,
                None => break,
            };

            let target_span = target.span();
            let target_spread = target.spread();
            let target_label = target.label();
            let target_ty = target.ty();

            let target_ty = self
                .normalize(
                    Some(target_span),
                    Cow::Borrowed(&target_ty),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        ..Default::default()
                    },
                )?
                .freezed();

            if let Some(spread) = target_spread {
                match target_ty.normalize() {
                    Type::Tuple(tuple) => {
                        let expanded = self.expand_spread_likes(tuple.span, &tuple.elems)?;

                        result.extend(expanded);
                    }

                    _ => {
                        let e_ty = self.get_iterator_element_type(
                            span,
                            Cow::Borrowed(&target_ty),
                            false,
                            GetIteratorOpts {
                                disallow_str: true,
                                ..Default::default()
                            },
                        )?;

                        result.push(TypeOrSpread {
                            span: target_span,
                            spread: Some(spread),
                            ty: Box::new(Type::Rest(RestType {
                                span: spread,
                                ty: Box::new(target_ty.into_owned()),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                        });
                        continue;
                    }
                }
            }

            result.push(TypeOrSpread {
                span: target_span,
                spread: None,
                ty: Box::new(target_ty.into_owned()),
            });
        }

        Ok(())
    }
}

#[auto_impl::auto_impl(&)]
pub(crate) trait SpreadLike {
    fn span(&self) -> Span;
    fn label(&self) -> Option<&RPat>;
    fn spread(&self) -> Option<Span>;
    fn ty(&self) -> &Type;
}

impl SpreadLike for TupleElement {
    fn span(&self) -> Span {
        self.span
    }

    fn label(&self) -> Option<&RPat> {
        self.label.as_ref()
    }

    fn spread(&self) -> Option<Span> {
        None
    }

    fn ty(&self) -> &Type {
        &self.ty
    }
}

impl SpreadLike for FnParam {
    fn span(&self) -> Span {
        self.span
    }

    fn label(&self) -> Option<&RPat> {
        Some(&self.pat)
    }

    fn spread(&self) -> Option<Span> {
        match &self.pat {
            RPat::Rest(p) => Some(p.dot3_token),
            _ => None,
        }
    }

    fn ty(&self) -> &Type {
        &self.ty
    }
}

impl SpreadLike for TypeOrSpread {
    fn span(&self) -> Span {
        self.span
    }

    fn label(&self) -> Option<&RPat> {
        None
    }

    fn spread(&self) -> Option<Span> {
        self.spread
    }

    fn ty(&self) -> &Type {
        &self.ty
    }
}
