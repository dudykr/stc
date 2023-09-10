use std::borrow::Cow;

use rnode::{Fold, FoldWith};
use stc_ts_ast_rnode::{RBindingIdent, RFnDecl, RFnExpr, RFunction, RIdent, RParamOrTsParamProp, RPat, RTsEntityName};
use stc_ts_errors::{ErrorKind, Errors};
use stc_ts_type_ops::Fix;
use stc_ts_types::{CallSignature, Class, ClassMetadata, Function, Id, KeywordType, KeywordTypeMetadata, Ref, TypeElement};
use stc_ts_utils::find_ids_in_pat;
use stc_utils::cache::Freeze;
use swc_common::{Span, Spanned, SyntaxContext};
use swc_ecma_ast::TsKeywordTypeKind;
use ty::TypeExt;

use crate::{
    analyzer::{pat::PatMode, scope::VarKind, util::ResultExt, Analyzer, Ctx, ScopeKind},
    ty,
    ty::{FnParam, Tuple, Type, TypeParam},
    validator,
    validator::ValidateWith,
    VResult,
};

mod return_type;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, f: &RFunction, name: Option<&RIdent>) -> VResult<ty::Function> {
        let marks = self.marks();

        if !self.ctx.reevaluating() && !self.ctx.ignore_errors && f.body.is_some() {
            if let Some(id) = name {
                let v = self.data.fn_impl_spans.entry(id.into()).or_default();

                v.push(f.span);
                // TODO(kdy1): Make this efficient by report same error only once.
                if v.len() >= 2 {
                    for &span in &*v {
                        self.storage.report(ErrorKind::DuplicateFnImpl { span }.into())
                    }
                }
            }
        }

        self.with_child(ScopeKind::Fn, Default::default(), |child: &mut Analyzer| {
            child.ctx.allow_new_target = true;
            child.ctx.in_fn_with_return_type = f.return_type.is_some();
            child.ctx.in_async = f.is_async;
            child.ctx.in_generator = f.is_generator;
            child.ctx.in_static_method = false;
            child.ctx.in_static_property_initializer = false;
            child.ctx.in_static_block = false;
            child.ctx.super_references_super_class = false;

            let mut errors = Errors::default();

            {
                // Validate params
                // TODO(kdy1): Move this to parser
                let mut has_optional = false;
                for p in &f.params {
                    if has_optional {
                        match p.pat {
                            RPat::Ident(RBindingIdent {
                                id: RIdent { optional: true, .. },
                                ..
                            })
                            | RPat::Rest(..) => {}
                            _ => {
                                child.storage.report(ErrorKind::TS1016 { span: p.span() }.into());
                            }
                        }
                    }

                    if let RPat::Ident(RBindingIdent {
                        id: RIdent { optional, .. },
                        ..
                    }) = p.pat
                    {
                        // Allow optional after optional parameter
                        if optional {
                            has_optional = true;
                        }
                    }
                }
            }

            let mut type_params = try_opt!(f.type_params.validate_with(child));

            if type_params.is_none() {
                if let Some(mutations) = &child.mutations {
                    if let Some(ann) = mutations.for_callable.get(&f.node_id) {
                        type_params = ann.type_params.clone();
                    }
                }
            }

            let params = {
                let prev_len = child.scope.borrow().declaring_parameters.len();
                let ids: Vec<Id> = find_ids_in_pat(&f.params);
                child.scope.borrow_mut().declaring_parameters.extend(ids);

                let ctx = Ctx {
                    pat_mode: PatMode::Decl,
                    in_fn_without_body: f.body.is_none(),
                    allow_ref_declaring: false,
                    is_fn_param: true,
                    ..child.ctx
                };
                let res = f.params.validate_with(&mut *child.with_ctx(ctx));

                child.scope.borrow_mut().declaring_parameters.truncate(prev_len);

                res?
            };

            let mut declared_ret_ty = {
                let ctx = Ctx {
                    in_actual_type: true,
                    ..child.ctx
                };
                try_opt!(f.return_type.validate_with(&mut *child.with_ctx(ctx)))
            }
            .freezed();

            child.scope.borrow().declared_return_type = declared_ret_ty.clone();

            if let Some(ty) = &mut declared_ret_ty {
                ty.freeze();

                child.expand_return_type_of_fn(ty).report(&mut child.storage);
            }

            if let Some(ret_ty) = declared_ret_ty {
                let span = ret_ty.span();
                let metadata = ret_ty.metadata();
                declared_ret_ty = Some(match ret_ty {
                    Type::ClassDef(def) => Type::Class(Class {
                        span,
                        def,
                        metadata: ClassMetadata {
                            common: metadata,
                            ..Default::default()
                        },
                        tracker: Default::default(),
                    }),

                    _ => ret_ty,
                });
            }

            if let Some(ty) = &mut declared_ret_ty {
                if let Type::Ref(..) = ty.normalize() {
                    child.prevent_expansion(ty);
                }
            }

            let span = f.span;
            let is_async = f.is_async;
            let is_generator = f.is_generator;

            let inferred_return_type = try_opt!(f.body.as_ref().map(|body| child.visit_stmts_for_return(
                span.with_ctxt(SyntaxContext::empty()),
                is_async,
                is_generator,
                &body.stmts
            )));

            let mut inferred_return_type = match inferred_return_type {
                Some(Some(inferred_return_type)) => {
                    let mut inferred_return_type = match inferred_return_type {
                        Type::Ref(ty) => Type::Ref(child.qualify_ref_type_args(ty.span, ty)?),
                        _ => inferred_return_type,
                    };

                    if let Some(declared) = &declared_ret_ty {
                        // Expand before assigning
                        let span = inferred_return_type.span();

                        if f.is_generator && declared.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
                            child
                                .storage
                                .report(ErrorKind::GeneratorCannotHaveVoidAsReturnType { span: declared.span() }.into())
                        }
                    } else {
                        if child.rule().no_implicit_any {
                            if child.is_implicitly_typed(&inferred_return_type) {
                                child.storage.report(ErrorKind::ImplicitReturnType { span }.into())
                            }
                        }

                        if child.may_generalize(&inferred_return_type) {
                            inferred_return_type = inferred_return_type.generalize_lit();
                        }
                    }

                    inferred_return_type
                }
                Some(None) => {
                    let mut span = f.span;

                    if let Some(ref declared) = declared_ret_ty {
                        span = declared.span();
                        let declared = child.normalize(Some(span), Cow::Borrowed(declared), Default::default())?;

                        match declared.normalize() {
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsNeverKeyword,
                                ..
                            }) => {}
                            _ => errors.push(ErrorKind::ReturnRequired { span }.into()),
                        }
                    }

                    // No return statement -> void
                    if f.return_type.is_none() {
                        if let Some(m) = &mut child.mutations {
                            if m.for_fns.entry(f.node_id).or_default().ret_ty.is_none() {
                                m.for_fns.entry(f.node_id).or_default().ret_ty = Some(Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsVoidKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }));
                            }
                        }
                    }
                    Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsVoidKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                }
                None => Type::any(f.span, Default::default()),
            };

            inferred_return_type.freeze();

            if f.return_type.is_none() {
                if let Some(m) = &mut child.mutations {
                    if m.for_fns.entry(f.node_id).or_default().ret_ty.is_none() {
                        m.for_fns.entry(f.node_id).or_default().ret_ty = Some(inferred_return_type.clone())
                    }
                }
            }

            child.storage.report_all(errors);

            Ok(ty::Function {
                span: f.span,
                type_params,
                params,
                ret_ty: Box::new(declared_ret_ty.unwrap_or(inferred_return_type)),
                metadata: Default::default(),
                tracker: Default::default(),
            })
        })
    }
}

impl Analyzer<'_, '_> {
    pub(crate) fn fn_to_type_element(&self, f: &Function) -> VResult<TypeElement> {
        Ok(TypeElement::Call(CallSignature {
            span: f.span.with_ctxt(SyntaxContext::empty()),
            params: f.params.clone(),
            type_params: f.type_params.clone(),
            ret_ty: Some(f.ret_ty.clone()),
        }))
    }

    /// Fill type arguments using default value.
    ///
    /// If the referred type has default type parameter, we have to include it
    /// in function type of output (.d.ts)
    fn qualify_ref_type_args(&mut self, span: Span, mut ty: Ref) -> VResult<Ref> {
        let actual_ty = self.type_of_ts_entity_name(span, &ty.type_name.clone().into(), ty.type_args.as_deref())?;

        // TODO(kdy1): PERF
        let type_params = match actual_ty.get_type_param_decl() {
            Some(type_params) => type_params.clone(),

            _ => return Ok(ty),
        };

        let arg_cnt = ty.type_args.as_ref().map(|v| v.params.len()).unwrap_or(0);
        if type_params.params.len() <= arg_cnt {
            return Ok(ty);
        }

        self.prevent_expansion(&mut ty);

        if let Some(args) = ty.type_args.as_mut() {
            for (span, default) in type_params
                .params
                .into_iter()
                .skip(arg_cnt)
                .map(|param| (param.span, param.default.map(|v| *v)))
            {
                if let Some(default) = default {
                    args.params.push(default);
                } else {
                    self.storage
                        .report(ErrorKind::ImplicitAny { span }.context("qualify_ref_type_args"));
                    args.params
                        .push(Type::any(span.with_ctxt(SyntaxContext::empty()), Default::default()));
                }
            }
        }

        Ok(ty)
    }

    /// TODO(kdy1): Handle recursive function
    fn visit_fn(&mut self, name: Option<&RIdent>, f: &RFunction, type_ann: Option<&Type>) -> Type {
        for (i, param) in f.params.clone().into_iter().enumerate() {
            if let RPat::Rest(r_pam) = param.pat {
                if i < f.params.len() - 1 {
                    self.storage
                        .report(ErrorKind::RestParamMustBeLast { span: r_pam.dot3_token }.into());
                }
            }
        }

        let fn_ty: Result<_, _> = try {
            let no_implicit_any_span = name.as_ref().map(|name| name.span);

            self.apply_fn_type_ann(f.span, f.node_id, f.params.iter().map(|p| &p.pat), type_ann);

            // if let Some(name) = name {
            //     // We use `typeof function` to infer recursive function's return type.
            //     match self.declare_var(
            //         f.span,
            //         VarDeclKind::Var,
            //         name.into(),
            //         Some(Type::Query(QueryType {
            //             span: f.span,
            //             expr: RTsEntityName::Ident(name.clone()).into(),
            //         })),
            //         // value is initialized
            //         true,
            //         // Allow overriding
            //         true,
            //     ) {
            //         Ok(()) => {}
            //         Err(err) => {
            //             self.storage.report(err);
            //         }
            //     }
            // }

            if let Some(name) = name {
                assert_eq!(self.scope.borrow().declaring_fn, None);
                self.scope.borrow().declaring_fn = Some(name.into());
            }

            let mut fn_ty: ty::Function = f.validate_with_args(self, name)?;
            // Handle type parameters in return type.
            fn_ty.ret_ty = fn_ty.ret_ty.fold_with(&mut TypeParamHandler {
                params: fn_ty.type_params.as_ref().map(|v| &*v.params),
            });
            let ty::Function { ref mut ret_ty, .. } = fn_ty;
            if let Type::Tuple(Tuple { ref mut elems, .. }) = **ret_ty {
                for element in elems.iter_mut() {
                    let span = element.span();

                    match element.ty.normalize() {
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            ..
                        })
                        | Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsNullKeyword,
                            ..
                        }) => {}
                        _ => continue,
                    }

                    //if child.rule.no_implicit_any
                    //    && child.span_allowed_implicit_any != f.span
                    //{
                    //    child.storage.report(Error::ImplicitAny {
                    //        span: no_implicit_any_span.unwrap_or(span),
                    //    });
                    //}

                    element.ty = Box::new(Type::any(
                        span,
                        KeywordTypeMetadata {
                            common: element.ty.metadata(),
                            ..Default::default()
                        },
                    ));
                }
            };

            if let Some(name) = name {
                self.scope.borrow().declaring_fn = None;
            }

            fn_ty
        };

        match fn_ty {
            Ok(ty) => Type::Function(ty).fixed().freezed(),
            Err(err) => {
                self.storage.report(err);
                Type::any(f.span, Default::default())
            }
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RParamOrTsParamProp) -> VResult<FnParam> {
        match p {
            RParamOrTsParamProp::TsParamProp(p) => p.validate_with(self),
            RParamOrTsParamProp::Param(p) => p.validate_with(self),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.fold_children_with(self)**
    fn validate(&mut self, f: &RFnDecl) {
        let ctx = Ctx {
            in_declare: self.ctx.in_declare || f.declare || f.function.body.is_none(),
            in_async: f.function.is_async,
            in_generator: f.function.is_generator,
            ..self.ctx
        };
        let fn_ty = self
            .with_ctx(ctx)
            .with_child(ScopeKind::Fn, Default::default(), |a: &mut Analyzer| {
                Ok(a.visit_fn(Some(&f.ident), &f.function, None).freezed())
            })?;

        let mut a = self.with_ctx(ctx);
        match a.declare_var(
            f.span(),
            VarKind::Fn,
            f.ident.clone().into(),
            Some(fn_ty),
            None,
            true,
            true,
            false,
            f.function.body.is_some(),
        ) {
            Ok(..) => {}
            Err(err) => {
                a.storage.report(err);
            }
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.fold_children_with(self)**
    fn validate(&mut self, f: &RFnExpr, type_ann: Option<&Type>) -> VResult<Type> {
        Ok(self.visit_fn(f.ident.as_ref(), &f.function, type_ann))
    }
}

struct TypeParamHandler<'a> {
    params: Option<&'a [TypeParam]>,
}

impl Fold<Type> for TypeParamHandler<'_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        if let Some(params) = self.params {
            // TODO(kdy1): PERF
            ty.normalize_mut();

            let ty: Type = ty.fold_children_with(self);

            match ty {
                Type::Ref(ref r) if r.type_args.is_none() => {
                    if let RTsEntityName::Ident(ref i) = r.type_name {
                        //
                        for param in params {
                            if param.name == i {
                                return Type::Param(param.clone());
                            }
                        }
                    }
                }

                _ => {}
            }

            ty
        } else {
            ty
        }
    }
}
