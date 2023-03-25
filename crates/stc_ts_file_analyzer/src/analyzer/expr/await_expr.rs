use std::borrow::Cow;

use stc_ts_ast_rnode::{RAwaitExpr, RIdent, RTsEntityName};
use stc_ts_errors::DebugExt;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{IdCtx, Key, Ref, Type, TypeParamInstantiation};
use stc_utils::cache::Freeze;
use swc_atoms::js_word;
use swc_common::{Span, SyntaxContext};

use crate::{
    analyzer::{expr::TypeOfMode, Analyzer},
    util::unwrap_builtin_with_single_arg,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAwaitExpr, type_ann: Option<&Type>) -> VResult<Type> {
        let span = e.span;

        let arg_type_ann = type_ann
            .map(|ty| {
                // If type annotation is Promise<T>, we use PromiseLike<T> as the annotation.

                if let Type::Ref(Ref {
                    type_name: RTsEntityName::Ident(RIdent {
                        sym: js_word!("Promise"), ..
                    }),
                    type_args: Some(type_args),
                    ..
                }) = ty.normalize()
                {
                    if let Some(ty) = type_args.params.first() {
                        return ty;
                    }
                }

                ty
            })
            .map(|item| {
                let span = span.with_ctxt(SyntaxContext::empty());

                Type::Ref(Ref {
                    span,
                    type_name: RTsEntityName::Ident(RIdent::new("PromiseLike".into(), span)),
                    type_args: Some(box TypeParamInstantiation {
                        span,
                        params: vec![item.clone()],
                    }),
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
            })
            .freezed();

        self.with(|a: &mut Analyzer| -> VResult<_> {
            let mut arg_ty = e
                .arg
                .validate_with_args(a, (TypeOfMode::RValue, None, arg_type_ann.as_ref()))
                .context("tried to validate the argument of an await expr")?;
            arg_ty.freeze();

            if let Ok(arg) = a.get_awaited_type(span, Cow::Borrowed(&arg_ty), false) {
                return Ok(arg.into_owned());
            }

            Ok(arg_ty)
        })
        .map(|mut ty| {
            ty.reposition(e.span);
            ty
        })
    }
}

impl Analyzer<'_, '_> {
    pub(crate) fn get_awaited_type<'a>(&mut self, span: Span, ty: Cow<'a, Type>, error_on_missing_then: bool) -> VResult<Cow<'a, Type>> {
        if let Some(arg) = unwrap_builtin_with_single_arg(&ty, "Promise").or_else(|| unwrap_builtin_with_single_arg(&ty, "PromiseLike")) {
            return self
                .get_awaited_type(span, Cow::Borrowed(arg), false)
                .map(Cow::into_owned)
                .map(Cow::Owned);
        }

        if let Type::Union(ty) = ty.normalize() {
            let mut types = Vec::with_capacity(ty.types.len());

            for ty in &ty.types {
                types.push(self.get_awaited_type(span, Cow::Borrowed(ty), error_on_missing_then)?.into_owned());
            }

            return Ok(Cow::Owned(Type::new_union(span, types)));
        }

        let res = self.access_property(
            span,
            &ty,
            &Key::Normal { span, sym: "then".into() },
            TypeOfMode::RValue,
            IdCtx::Var,
            Default::default(),
        );

        let res = if error_on_missing_then { Some(res?) } else { res.ok() };

        Ok(res
            .and_then(|then_ty| {
                if let Type::Function(f) = then_ty.normalize() {
                    // Default type of the first type parameter is awaited type.
                    if let Some(type_params) = &f.type_params {
                        if let Some(ty) = type_params.params.first() {
                            if let Some(ty) = &ty.default {
                                return Some(Cow::Owned(*ty.clone()));
                            }
                        }
                    }
                }

                None
            })
            .unwrap_or(ty))
    }
}
