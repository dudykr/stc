use std::borrow::Cow;

use stc_ts_ast_rnode::RPat;
use stc_ts_types::{FnParam, TupleElement, Type, TypeOrSpread};
use stc_utils::cache::Freeze;
use swc_common::Span;

use super::NormalizeTypeOpts;
use crate::{analyzer::Analyzer, VResult};

impl Analyzer<'_, '_> {
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
        F: FnMut(&mut Self, &Type, &Type) -> VResult<()>,
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
                    relate(self, &source_ty, &target_ty)?;
                    continue;
                }

                (Some(target_spread), None) => {
                    match target_ty.normalize() {
                        Type::Tuple(target_tuple) if target_tuple.elems.is_empty() => {
                            // Handle
                            //
                            //   param: (...x: [boolean, sting, ...number])
                            //   arg: (true, 'str')
                            //      or
                            //   arg: (true, 'str', 10)

                            let res = self
                                .assign_with_opts(
                                    &mut Default::default(),
                                    &param_ty.elems[0].ty,
                                    &arg.ty,
                                    AssignOpts {
                                        span: arg.span(),
                                        allow_iterable_on_rhs: true,
                                        ..Default::default()
                                    },
                                )
                                .map_err(|err| {
                                    ErrorKind::WrongArgType {
                                        span: arg.span(),
                                        inner: box err,
                                    }
                                    .into()
                                })
                                .context("tried to assign to first element of a tuple type of a parameter");

                            match res {
                                Ok(_) => {}
                                Err(err) => {
                                    report_err!(err);
                                    return Ok(());
                                }
                            };

                            for param_elem in param_ty.elems.iter().skip(1) {
                                let arg = match args_iter.next() {
                                    Some(v) => v,
                                    None => {
                                        // TODO(kdy1): Argument count
                                        break;
                                    }
                                };

                                // TODO(kdy1): Check if arg.spread is none.
                                // The logic below is correct only if the arg is not
                                // spread.

                                let res = self
                                    .assign_with_opts(
                                        &mut Default::default(),
                                        &param_elem.ty,
                                        &arg.ty,
                                        AssignOpts {
                                            span: arg.span(),
                                            allow_iterable_on_rhs: true,
                                            ..Default::default()
                                        },
                                    )
                                    .convert_err(|err| ErrorKind::WrongArgType {
                                        span: arg.span(),
                                        inner: box err.into(),
                                    })
                                    .context("tried to assign to element of a tuple type of a parameter");

                                match res {
                                    Ok(_) => {}
                                    Err(err) => {
                                        report_err!(err);
                                        return Ok(());
                                    }
                                };
                            }

                            // Skip default type checking logic.
                            continue;
                        }
                    }
                }

                (Some(target_spread), Some(source_spread)) => {}

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
