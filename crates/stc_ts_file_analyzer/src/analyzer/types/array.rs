use std::borrow::Cow;

use stc_ts_ast_rnode::RPat;
use stc_ts_types::{FnParam, TupleElement, Type, TypeOrSpread};
use stc_utils::cache::Freeze;
use swc_common::Span;

use super::NormalizeTypeOpts;
use crate::{analyzer::Analyzer, VResult};

impl Analyzer<'_, '_> {
    /// `(this, source, target, is_iterator)`
    pub(crate) fn relate_spread_likes<T, S, F>(
        &mut self,
        span: Span,
        sources: &mut dyn Iterator<Item = &S>,
        targets: &mut dyn Iterator<Item = &T>,
        mut relate: F,
    ) -> VResult<()>
    where
        T: SpreadLike,
        S: SpreadLike,
        F: FnMut(&mut Self, &Type, &Type, bool) -> VResult<()>,
    {
        loop {
            let source = match sources.next() {
                Some(source) => source,
                None => break,
            };
            let target = match targets.next() {
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

            let source_spread = source.spread();
            let source_ty = source.ty();

            let source_ty = self
                .normalize(
                    Some(source.span()),
                    Cow::Borrowed(&source_ty),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        ..Default::default()
                    },
                )?
                .freezed();

            match (target_spread, source_spread) {
                (None, None) => {
                    // Trivial case.
                    relate(self, &source_ty, &target_ty, false)?;
                }

                (Some(target_spread), Some(source_spread)) => {
                    if relate(self, &source_ty, &target_ty, true).is_ok() {
                        return Ok(());
                    }
                }

                (Some(target_spread), None) => {
                    if let Type::Tuple(target_tuple) = target_ty.normalize() {
                        // Handle
                        //
                        //   param: (...x: [boolean, sting, ...number])
                        //   arg: (true, 'str')
                        //      or
                        //   arg: (true, 'str', 10)

                        // Consume `sources`
                        self.relate_spread_likes(span, sources, &mut target_tuple.elems.iter(), relate)?;
                        continue;
                    }
                }

                (None, Some(source_spread)) => {}
            }
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
