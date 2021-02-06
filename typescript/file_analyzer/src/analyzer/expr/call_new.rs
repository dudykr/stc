//! Handles new expressions and call expressions.
use super::super::Analyzer;
use super::IdCtx;
use crate::analyzer::assign::AssignOpts;
use crate::{
    analyzer::{
        expr::TypeOfMode,
        marks::MarkExt,
        util::{instantiate_class, ResultExt},
        Ctx, ScopeKind,
    },
    ty,
    ty::{
        CallSignature, ClassInstance, ConstructorSignature, FnParam, Method, MethodSignature, QueryExpr, QueryType,
        Type, TypeElement, TypeOrSpread, TypeParam, TypeParamInstantiation,
    },
    util::{is_str_lit_or_union, type_ext::TypeVecExt},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::FxHashMap;
use itertools::EitherOrBoth;
use itertools::Itertools;
use rnode::Fold;
use rnode::FoldWith;
use rnode::NodeId;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RCallExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSpread;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RInvalid;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RNewExpr;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTaggedTpl;
use stc_ts_ast_rnode::RTsAsExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsThisTypeOrIdent;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeParamInstantiation;
use stc_ts_ast_rnode::RTsTypeRef;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::debug::print_type;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::ClassProperty;
use stc_ts_types::Key;
use stc_ts_types::{Alias, Id, IndexedAccessType, Ref, Symbol, Union};
use stc_ts_utils::PatExt;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::SyntaxContext;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use ty::TypeExt;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExprOrSpread) -> ValidationResult<TypeOrSpread> {
        let span = node.span();
        Ok(TypeOrSpread {
            span,
            spread: node.spread,
            ty: node.expr.validate_with_default(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RCallExpr, type_ann: Option<&Type>) -> ValidationResult {
        self.record(e);

        let RCallExpr {
            span,
            ref callee,
            ref args,
            ref type_args,
            ..
        } = *e;

        let callee = match callee {
            RExprOrSuper::Super(..) => return Ok(Type::any(span)),
            RExprOrSuper::Expr(callee) => callee,
        };

        // TODO: validate children

        self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.extract_call_new_expr_member(span, callee, type_ann, ExtractKind::Call, args, type_args.as_ref())
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RNewExpr, type_ann: Option<&Type>) -> ValidationResult {
        self.record(e);

        let RNewExpr {
            span,
            ref callee,
            ref args,
            ref type_args,
            ..
        } = *e;

        // TODO: e.visit_children

        self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.extract_call_new_expr_member(
                span,
                callee,
                type_ann,
                ExtractKind::New,
                args.as_ref().map(|v| &**v).unwrap_or_else(|| &mut []),
                type_args.as_ref(),
            )
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTaggedTpl) -> ValidationResult {
        let span = e.span;

        let tpl_str_arg = {
            let span = span.with_ctxt(SyntaxContext::empty());
            RExprOrSpread {
                spread: None,
                expr: box RExpr::TsAs(RTsAsExpr {
                    node_id: NodeId::invalid(),
                    span,
                    expr: box RExpr::Invalid(RInvalid { span: DUMMY_SP }),
                    type_ann: box RTsType::TsTypeRef(RTsTypeRef {
                        node_id: NodeId::invalid(),
                        span,
                        type_name: RTsEntityName::Ident(RIdent::new("TemplateStringsArray".into(), span)),
                        type_params: None,
                    }),
                }),
            }
        };
        let mut args = vec![tpl_str_arg];

        args.extend(e.exprs.iter().cloned().map(|expr| RExprOrSpread { spread: None, expr }));

        let res = self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.extract_call_new_expr_member(
                span,
                &e.tag,
                Default::default(),
                ExtractKind::Call,
                args.as_ref(),
                Default::default(),
            )
        });

        // dbg!(&res);

        res
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExtractKind {
    New,
    Call,
}

impl Analyzer<'_, '_> {
    /// Calculates the return type of a new /call expression.
    ///
    /// This method check arguments
    fn extract_call_new_expr_member(
        &mut self,
        span: Span,
        callee: &RExpr,
        type_ann: Option<&Type>,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        type_args: Option<&RTsTypeParamInstantiation>,
    ) -> ValidationResult {
        debug_assert_eq!(self.scope.kind(), ScopeKind::Call);

        slog::debug!(self.logger, "extract_call_new_expr_member");

        let type_args = match type_args {
            Some(v) => {
                let mut type_args = v.validate_with(self)?;
                self.prevent_expansion(&mut type_args);
                Some(type_args)
            }
            None => None,
        };

        let arg_types = self.validate_args(args)?;
        let spread_arg_types = self.spread_args(&arg_types);

        match *callee {
            RExpr::Ident(ref i) if i.sym == js_word!("require") => {
                let id = args
                    .iter()
                    .cloned()
                    .map(|arg| match arg {
                        RExprOrSpread { spread: None, expr } => match *expr {
                            RExpr::Lit(RLit::Str(RStr { span, value, .. })) => RIdent::new(value.clone(), span).into(),
                            _ => unimplemented!("dynamic import: require()"),
                        },
                        _ => unimplemented!("error reporting: spread element in require()"),
                    })
                    .next()
                    .unwrap();
                if let Some(dep) = self.find_imported_var(&id)? {
                    let dep = dep.clone();
                    unimplemented!("dep: {:#?}", dep);
                }

                // if let Some(Type::Enum(ref e)) = self.scope.find_type(&i.into()) {
                //     return Ok(RTsType::TsTypeRef(RTsTypeRef {
                //         span,
                //         type_name: RTsEntityName::Ident(i.clone()),
                //         type_params: None,
                //     })
                //     .into());
                // }
                Err(Error::UndefinedSymbol {
                    sym: i.into(),
                    span: i.span(),
                })?
            }

            _ => {}
        }

        match *callee {
            RExpr::Ident(RIdent {
                sym: js_word!("Symbol"),
                ..
            }) => {
                // Symbol uses special type
                if !args.is_empty() {
                    unimplemented!("Error reporting for calling `Symbol` with arguments is not implemented yet")
                }

                return Ok(box Type::Symbol(Symbol {
                    span,
                    id: self.symbols.generate(),
                }));
            }

            RExpr::Member(RMemberExpr {
                obj: RExprOrSuper::Expr(ref obj),
                ref prop,
                computed,
                ..
            }) => {
                let prop = self.validate_key(prop, computed)?;
                {
                    // Handle toString()

                    if prop == js_word!("toString") {
                        return Ok(box Type::from(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }
                }

                // Handle member expression
                let obj_type = obj.validate_with_default(self)?.generalize_lit();

                let obj_type: Box<Type> = box self.expand_top_ref(span, Cow::Owned(*obj_type))?.into_owned();

                let obj_type = match *obj_type.normalize() {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => self
                        .env
                        .get_global_type(span, &js_word!("Number"))
                        .expect("Builtin type named 'Number' should exist"),
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) => self
                        .env
                        .get_global_type(span, &js_word!("String"))
                        .expect("Builtin type named 'String' should exist"),
                    _ => obj_type,
                };

                self.call_property(
                    span,
                    kind,
                    obj_type,
                    &prop,
                    type_args.as_ref(),
                    args,
                    &arg_types,
                    &spread_arg_types,
                )
            }
            _ => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_all: false,
                    ignore_expand_prevention_for_top: false,
                    ..self.ctx
                };

                self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
                    let ret_ty = match callee {
                        RExpr::Ident(i) if kind == ExtractKind::New => {
                            let mut ty = Type::Ref(Ref {
                                span: i.span,
                                ctxt: analyzer.ctx.module_id,
                                type_name: RTsEntityName::Ident(i.clone()),
                                type_args: Default::default(),
                            });
                            // It's specified by user
                            analyzer.prevent_expansion(&mut ty);
                            match ty {
                                Type::Ref(r) => Some(r),
                                _ => unreachable!(),
                            }
                        }
                        _ => None,
                    };

                    let mut callee_ty = {
                        let callee_ty = callee.validate_with_default(analyzer)?;
                        match *callee_ty.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                                ..
                            }) if type_args.is_some() => analyzer.storage.report(box Error::TS2347 { span }),
                            _ => {}
                        }
                        callee_ty
                    };

                    if let Some(type_args) = &type_args {
                        let type_params = match callee_ty.normalize() {
                            Type::Function(f) => f.type_params.as_ref(),
                            _ => None,
                        };
                        if let Some(type_param_decl) = type_params {
                            let mut params = FxHashMap::default();

                            for (type_param, ty) in type_param_decl.params.iter().zip(type_args.params.iter()) {
                                params.insert(type_param.name.clone(), ty.clone());
                            }

                            callee_ty = analyzer.expand_type_params(&params, callee_ty)?;
                        }
                    }

                    let ctx = Ctx {
                        preserve_params: true,
                        ..analyzer.ctx
                    };
                    callee_ty = analyzer.with_ctx(ctx).expand_fully(span, callee_ty, false)?;

                    let expanded_ty = analyzer.extract(
                        span,
                        &callee_ty,
                        kind,
                        args,
                        &arg_types,
                        &spread_arg_types,
                        type_args.as_ref(),
                    )?;
                    match *expanded_ty {
                        Type::ClassInstance(ClassInstance {
                            ty: box Type::Class(..),
                            type_args,
                            ..
                        }) if ret_ty.is_some() => {
                            return Ok(box Type::Ref(Ref {
                                type_args,
                                ..ret_ty.unwrap()
                            }));
                        }
                        _ => {}
                    }

                    return Ok(expanded_ty);
                })
            }
        }
    }

    /// TODO: Use Cow for `obj_type`
    pub(super) fn call_property(
        &mut self,
        span: Span,
        kind: ExtractKind,
        obj_type: Box<Type>,
        prop: &Key,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        let old_this = self.scope.this.take();
        self.scope.this = Some(obj_type.clone());

        let res = (|| {
            match obj_type.normalize() {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                }) => {
                    return Ok(Type::any(span));
                }

                Type::Ref(..) => {
                    let obj_type = box self
                        .expand_top_ref(span, Cow::Owned(*obj_type))
                        .context("tried to expand object to call property of it")?
                        .into_owned();

                    return self.call_property(
                        span,
                        kind,
                        obj_type,
                        prop,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                    );
                }

                Type::Interface(ref i) => {
                    // TODO: Check parent interface
                    return self.search_members_for_callable_prop(
                        kind,
                        span,
                        &obj_type,
                        &i.body,
                        &prop,
                        type_args,
                        args,
                        &arg_types,
                        &spread_arg_types,
                    );
                }

                Type::TypeLit(ref t) => {
                    return self.search_members_for_callable_prop(
                        kind,
                        span,
                        &obj_type,
                        &t.members,
                        &prop,
                        type_args,
                        args,
                        &arg_types,
                        &spread_arg_types,
                    );
                }

                Type::Class(ty::Class { body, super_class, .. }) => {
                    let mut candidates = vec![];
                    for member in body.iter() {
                        match member {
                            ty::ClassMember::Method(Method {
                                key,
                                ret_ty,
                                type_params,
                                params,
                                ..
                            }) if key.type_eq(&prop) => {
                                candidates.push((type_params, params, ret_ty));
                            }
                            ty::ClassMember::Property(ClassProperty { key, value, .. }) if key.type_eq(&prop) => {
                                // Check for properties with callable type.

                                // TODO: Change error message from no callable
                                // property to property exists but not callable.
                                if let Some(ty) = &value {
                                    return self.extract(span, ty, kind, args, arg_types, spread_arg_types, type_args);
                                }
                            }
                            _ => {}
                        }
                    }

                    candidates.sort_by_cached_key(|(type_params, params, _)| {
                        self.check_call_args(
                            span,
                            type_params.as_ref().map(|v| &*v.params),
                            params,
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                        )
                    });

                    for (type_params, params, ret_ty) in candidates {
                        return self.get_return_type(
                            span,
                            kind,
                            type_params.as_ref().map(|v| &*v.params),
                            &params,
                            ret_ty.clone(),
                            type_args,
                            args,
                            &arg_types,
                            &spread_arg_types,
                        );
                    }

                    if let Some(ty) = super_class {
                        if let Ok(ret_ty) = self.call_property(
                            span,
                            kind,
                            ty.clone(),
                            prop,
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                        ) {
                            return Ok(ret_ty);
                        }
                    }

                    return Err(match kind {
                        ExtractKind::Call => box Error::NoCallabelPropertyWithName {
                            span,
                            key: box prop.clone(),
                        },
                        ExtractKind::New => box Error::NoSuchConstructor {
                            span,
                            key: box prop.clone(),
                        },
                    });
                }
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                    ..
                }) => {
                    if let Ok(ty) = self.env.get_global_type(span, &js_word!("Symbol")) {
                        return Ok(ty);
                    }
                }

                _ => {}
            }

            let callee = self.access_property(span, obj_type, &prop, TypeOfMode::RValue, IdCtx::Var)?;

            let callee = box self.expand_top_ref(span, Cow::Owned(*callee))?.into_owned();

            self.get_best_return_type(span, callee, kind, type_args, args, &arg_types, &spread_arg_types)
        })();
        self.scope.this = old_this;
        res
    }

    fn check_type_element_for_call(
        &mut self,
        span: Span,
        kind: ExtractKind,
        candidates: &mut Vec<MethodSignature>,
        m: &TypeElement,
        prop: &Key,
    ) {
        match m {
            TypeElement::Method(m) if kind == ExtractKind::Call => {
                // We are interested only on methods named `prop`
                if let Ok(()) = self.assign(&m.key.ty(), &prop.ty(), span) {
                    candidates.push(m.clone());
                }
            }

            TypeElement::Property(p) if kind == ExtractKind::Call => {
                if let Ok(()) = self.assign(&p.key.ty(), &prop.ty(), span) {
                    // TODO: Remove useless clone
                    let ty = p.type_ann.as_ref().cloned().unwrap_or(Type::any(m.span()));

                    match ty.foldable() {
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => candidates.push(MethodSignature {
                            span: p.span,
                            readonly: p.readonly,
                            key: p.key.clone(),
                            optional: p.optional,
                            // TODO: Maybe we need Option<Vec<T>>.
                            params: Default::default(),
                            ret_ty: Default::default(),
                            type_params: Default::default(),
                        }),

                        Type::Function(f) => {
                            candidates.push(MethodSignature {
                                span: f.span,
                                readonly: p.readonly,
                                key: p.key.clone(),
                                optional: p.optional,
                                params: f.params,
                                ret_ty: Some(f.ret_ty),
                                type_params: f.type_params,
                            });
                        }

                        _ => {}
                    }
                }
            }

            _ => {}
        }
    }

    fn search_members_for_callable_prop(
        &mut self,
        kind: ExtractKind,
        span: Span,
        obj: &Type,
        members: &[TypeElement],
        prop: &Key,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        // Candidates of the method call.
        //
        // 4 is just an unscientific guess
        // TODO: Use smallvec
        let mut candidates = Vec::with_capacity(4);

        for m in members {
            self.check_type_element_for_call(span, kind, &mut candidates, m, prop);
        }

        // TODO: Move this to caller to prevent checking members of `Object` every time
        // we check parent interface.
        {
            // Handle methods from `interface Object`
            let i = self
                .env
                .get_global_type(span, &js_word!("Object"))
                .expect("`interface Object` is must");
            let methods = match i.normalize() {
                Type::Interface(i) => &*i.body,

                _ => &[],
            };

            // TODO: Remove clone
            for m in methods {
                self.check_type_element_for_call(span, kind, &mut candidates, m, prop);
            }
        }

        match candidates.len() {
            0 => Err(box Error::NoSuchProperty {
                span,
                obj: Some(box obj.clone()),
                prop: Some(box prop.clone()),
            }),
            1 => {
                // TODO:
                return self.check_method_call(
                    span,
                    &candidates.into_iter().next().unwrap(),
                    type_args,
                    args,
                    &arg_types,
                    spread_arg_types,
                );
            }
            _ => {
                //
                candidates.sort_by_cached_key(|method: &MethodSignature| {
                    self.check_call_args(
                        span,
                        method.type_params.as_ref().map(|v| &*v.params),
                        &method.params,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                    )
                });

                for c in candidates {
                    return self.check_method_call(span, &c, type_args, args, &arg_types, spread_arg_types);
                }

                unimplemented!("multiple methods with same name and same number of arguments")
            }
        }
    }

    /// Returns `()`
    fn spread_args<'a>(&mut self, arg_types: &'a [TypeOrSpread]) -> Cow<'a, [TypeOrSpread]> {
        let mut new_arg_types;

        if arg_types.iter().any(|arg| arg.spread.is_some()) {
            new_arg_types = vec![];
            for arg in arg_types {
                if arg.spread.is_some() {
                    match &*arg.ty {
                        Type::Tuple(arg_ty) => {
                            new_arg_types.extend(arg_ty.elems.iter().map(|element| &element.ty).cloned().map(|ty| {
                                TypeOrSpread {
                                    span: arg.spread.unwrap(),
                                    spread: None,
                                    ty,
                                }
                            }));
                        }

                        Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => {
                            self.scope.is_call_arg_count_unknown = true;
                            new_arg_types.push(TypeOrSpread {
                                span: *span,
                                spread: None,
                                ty: arg.ty.clone(),
                            });
                        }

                        Type::Array(arr) => {
                            self.scope.is_call_arg_count_unknown = true;
                            new_arg_types.push(TypeOrSpread {
                                span: arr.span,
                                spread: None,
                                ty: arr.elem_type.clone(),
                            });
                        }

                        _ => unimplemented!("spread argument with type other than tuple\nType: {:#?}", arg.ty),
                    }
                } else {
                    new_arg_types.push(arg.clone());
                }
            }

            return Cow::Owned(new_arg_types);
        } else {
            return Cow::Borrowed(arg_types);
        }
    }

    fn extract(
        &mut self,
        span: Span,
        ty: &Type,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        match ty.normalize() {
            Type::Ref(..) => {
                let ty = self.expand_top_ref(span, Cow::Borrowed(ty))?;
                return self.extract(span, &ty, kind, args, arg_types, spread_arg_types, type_args);
            }

            Type::Query(QueryType {
                expr: box QueryExpr::TsEntityName(name),
                ..
            }) => {
                let ty = self.resolve_typeof(span, name)?;
                return self.extract(span, &ty, kind, args, arg_types, spread_arg_types, type_args);
            }

            _ => {}
        }

        match kind {
            ExtractKind::New => match ty.normalize() {
                Type::Class(ref cls) => {
                    if cls.is_abstract {
                        self.storage
                            .report(box Error::CannotCreateInstanceOfAbstractClass { span })
                    }

                    if let Some(type_params) = &cls.type_params {
                        for param in &type_params.params {
                            self.register_type(param.name.clone(), box Type::Param(param.clone()));
                        }

                        // Infer type arguments using constructors.
                        let constructors = cls.body.iter().filter_map(|member| match member {
                            stc_ts_types::ClassMember::Constructor(c) => Some(c),
                            _ => None,
                        });

                        for constructor in constructors {
                            //
                            let inferred = self.infer_arg_types(
                                span,
                                type_args,
                                &type_params.params,
                                &constructor.params,
                                spread_arg_types,
                                Some(&Type::Keyword(RTsKeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                })),
                            )?;

                            for (id, ty) in &inferred {
                                print_type(&self.logger, &format!("{}", id), &self.cm, &ty);
                            }

                            let type_args = self.instantiate(span, &type_params.params, inferred)?;

                            return Ok(box Type::ClassInstance(ClassInstance {
                                span,
                                ty: box Type::Class(cls.clone()),
                                type_args: Some(box type_args),
                            }));
                        }
                    }

                    return Ok(box Type::ClassInstance(ClassInstance {
                        span,
                        ty: box Type::Class(cls.clone()),
                        type_args: type_args.cloned().map(Box::new),
                    }));
                }

                _ => {}
            },
            _ => {}
        }

        macro_rules! ret_err {
            () => {{
                dbg!();
                match kind {
                    ExtractKind::Call => {
                        return Err(box Error::NoCallSignature {
                            span,
                            callee: box ty.clone(),
                        })
                    }
                    ExtractKind::New => {
                        return Err(box Error::NoNewSignature {
                            span,
                            callee: box ty.clone(),
                        })
                    }
                }
            }};
        }

        match ty.normalize() {
            Type::Intersection(..) if kind == ExtractKind::New => {
                // TODO: Check if all types has constructor signature
                return Ok(box Type::ClassInstance(ClassInstance {
                    span,
                    ty: instantiate_class(self.ctx.module_id, box ty.clone()),
                    type_args: type_args.cloned().map(Box::new),
                }));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(Type::any(span)),

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(box Error::Unknown { span }),

            Type::Function(ref f) if kind == ExtractKind::Call => self.get_return_type(
                span,
                kind,
                f.type_params.as_ref().map(|v| &*v.params),
                &f.params,
                f.ret_ty.clone(),
                type_args,
                args,
                arg_types,
                spread_arg_types,
            ),

            // Type::Constructor(ty::Constructor {
            //     ref params,
            //     ref type_params,
            //     ref ret_ty,
            //     ..
            // }) if kind == ExtractKind::New => self.try_instantiate_simple(
            //     span,
            //     ty.span(),
            //     &ret_ty,
            //     params,
            //     type_params.as_ref(),
            //     args,
            //     type_args,
            // ),
            Type::Union(..) => {
                self.get_best_return_type(span, box ty.clone(), kind, type_args, args, spread_arg_types, arg_types)
            }

            Type::Interface(ref i) => {
                // Search for methods
                match self.search_members_for_extract(
                    span,
                    &ty,
                    &i.body,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                ) {
                    Ok(ty) => return Ok(ty.clone()),
                    Err(first_err) => {
                        //  Check parent interface
                        for parent in &i.extends {
                            let parent =
                                self.type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, type_args)?;

                            if let Ok(v) =
                                self.extract(span, &parent, kind, args, arg_types, spread_arg_types, type_args)
                            {
                                return Ok(v);
                            }
                        }
                        Err(first_err)?
                    }
                }
            }

            Type::TypeLit(ref l) => {
                return self.search_members_for_extract(
                    span,
                    &ty,
                    &l.members,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                );
            }

            Type::Class(ref cls) if kind == ExtractKind::New => {
                // TODO: Remove clone
                return Ok(box ClassInstance {
                    span,
                    ty: box Type::Class(cls.clone()),
                    type_args: type_args.cloned().map(Box::new),
                }
                .into());
            }

            _ => ret_err!(),
        }
    }

    /// Search for members and returns if there's a match
    #[inline(never)]
    fn search_members_for_extract(
        &mut self,
        span: Span,
        callee_ty: &Type,
        members: &[TypeElement],
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        let callee_span = callee_ty.span();

        let mut candidates = members
            .iter()
            .filter_map(|member| match member {
                TypeElement::Call(CallSignature {
                    span,
                    params,
                    type_params,
                    ret_ty,
                }) if kind == ExtractKind::Call => Some((span, params, type_params, ret_ty)),
                TypeElement::Constructor(ConstructorSignature {
                    span,
                    params,
                    ret_ty,
                    type_params,
                }) if kind == ExtractKind::New => Some((span, params, type_params, ret_ty)),
                _ => None,
            })
            .collect::<Vec<_>>();

        if candidates.is_empty() {
            return match kind {
                ExtractKind::Call => Err(box Error::NoCallSignature {
                    span,
                    callee: box callee_ty.clone(),
                }),
                ExtractKind::New => Err(box Error::NoNewSignature {
                    span,
                    callee: box callee_ty.clone(),
                }),
            };
        }

        candidates.sort_by_cached_key(|(_, params, type_params, _)| {
            self.check_call_args(
                span,
                type_params.as_ref().map(|v| &*v.params),
                params,
                type_args,
                args,
                arg_types,
                spread_arg_types,
            )
        });

        let (_, params, type_params, ret_ty) = candidates.into_iter().next().unwrap();

        return self.get_return_type(
            span,
            kind,
            type_params.as_ref().map(|v| &*v.params),
            params,
            ret_ty.clone().unwrap_or(Type::any(span)),
            type_args,
            args,
            arg_types,
            spread_arg_types,
        );
    }

    fn check_method_call(
        &mut self,
        span: Span,
        c: &MethodSignature,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        self.get_return_type(
            span,
            ExtractKind::Call,
            c.type_params.as_ref().map(|v| &*v.params),
            &c.params,
            c.ret_ty.clone().unwrap_or_else(|| Type::any(span)),
            type_args,
            args,
            arg_types,
            spread_arg_types,
        )
    }

    fn get_best_return_type(
        &mut self,
        span: Span,
        callee: Box<Type>,
        kind: ExtractKind,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        let has_spread = arg_types.len() != spread_arg_types.len();
        let cnt = callee.normalize().iter_union().count();

        if callee.is_any() {
            return Ok(Type::any(span));
        }

        slog::info!(self.logger, "get_best_return_type: {} candidates", cnt);

        // TODO: Calculate return type only if selected
        // This can be done by storing type params, return type, params in the
        // candidates.
        let mut candidates = vec![];

        for callee in callee.normalize().iter_union().flat_map(|ty| ty.iter_union()) {
            // TODO: Check if signature match.
            match callee.normalize() {
                Type::Intersection(ref i) => {
                    let types = i
                        .types
                        .iter()
                        .map(|callee| {
                            self.get_best_return_type(
                                span,
                                callee.clone(),
                                kind,
                                type_args.clone(),
                                args,
                                arg_types,
                                spread_arg_types,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut ok = true;
                    if types.len() >= 1 {
                        for ty in &types[1..] {
                            if !types[0].type_eq(&ty) {
                                ok = false;
                                break;
                            }
                        }

                        if ok {
                            return Ok(types.into_iter().next().unwrap());
                        }
                    }
                }

                Type::Function(ref f) => {
                    candidates.push((f.type_params.as_ref().map(|v| &*v.params), &f.params, f.ret_ty.clone()));
                }
                Type::Class(ref cls) if kind == ExtractKind::New => {
                    // TODO: Handle type parameters.
                    return Ok(box Type::ClassInstance(ClassInstance {
                        span,
                        ty: box Type::Class(cls.clone()),
                        type_args: type_args.cloned().map(Box::new),
                    }));
                }
                _ => {}
            }
        }

        if candidates.is_empty() {
            dbg!();

            return Err(if kind == ExtractKind::Call {
                box Error::NoCallSignature { span, callee }
            } else {
                box Error::NoNewSignature { span, callee }
            });
        }

        candidates.sort_by_cached_key(|(type_params, params, ret_ty)| {
            self.check_call_args(span, *type_params, params, type_args, args, arg_types, spread_arg_types)
        });

        let (type_params, params, ret_ty) = candidates.into_iter().next().unwrap();

        return self.get_return_type(
            span,
            kind,
            type_params,
            params,
            ret_ty,
            type_args,
            args,
            arg_types,
            spread_arg_types,
        );
    }

    fn validate_arg_count(
        &mut self,
        span: Span,
        params: &[FnParam],
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult<()> {
        let mut min_param = 0;
        let mut max_param = Some(params.len());
        for param in params {
            match param.pat {
                RPat::Rest(..) => {
                    max_param = None;
                }
                RPat::Ident(RIdent {
                    sym: js_word!("this"), ..
                }) => {
                    if let Some(max) = &mut max_param {
                        *max -= 1;
                    }
                    continue;
                }
                _ => {}
            }
            if param.required {
                min_param += 1;
            }
        }

        let has_spread = args.iter().any(|arg| arg.spread.is_some());
        if has_spread {
            // TODO
            Ok(())
        } else {
            if min_param <= args.len() {
                if let Some(max) = max_param {
                    if args.len() <= max {
                        return Ok(());
                    }
                } else {
                    return Ok(());
                }
            }
            return Err(box Error::ArgCountMismatch {
                span,
                min: min_param,
                max: max_param,
            });
        }
    }

    /// Returns the return type of function. This method should be called only
    /// for final step because it emits errors instead of returning them.
    fn get_return_type(
        &mut self,
        span: Span,
        kind: ExtractKind,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        mut ret_ty: Box<Type>,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        let logger = self.logger.clone();

        self.expand_this(&mut ret_ty);

        {
            let arg_check_res = self.validate_arg_count(span, params, args, arg_types, spread_arg_types);
            match arg_check_res {
                Err(..) => print_backtrace(),
                _ => {}
            }
            arg_check_res.report(&mut self.storage);
        }

        slog::debug!(
            logger,
            "get_return_type: \ntype_params = {:?}\nret_ty = {:?}",
            type_params,
            ret_ty
        );

        if let Some(type_params) = type_params {
            for param in type_params {
                slog::info!(self.logger, "({}) Defining {}", self.scope.depth(), param.name);

                self.register_type(param.name.clone(), box Type::Param(param.clone()));
            }

            let mut new_args = vec![];
            for (idx, (arg, param)) in args.into_iter().zip(params.iter()).enumerate() {
                let arg_ty = &arg_types[idx];
                let (type_param_decl, actual_params) = match &*param.ty {
                    Type::Function(f) => (&f.type_params, &f.params),
                    _ => {
                        new_args.push(arg_ty.clone());
                        continue;
                    }
                };

                if let Some(type_param_decl) = type_param_decl {
                    for param in &type_param_decl.params {
                        self.register_type(param.name.clone(), box Type::Param(param.clone()));
                    }
                }

                let mut patch_arg = |idx: usize, pat: &RPat| -> ValidationResult<()> {
                    let actual = &actual_params[idx];

                    let default_any_ty: Option<_> = try {
                        let node_id = pat.node_id()?;
                        self.mutations.as_ref()?.for_pats.get(&node_id)?.ty.clone()?
                    };
                    if let Some(ty) = default_any_ty {
                        match &*ty {
                            Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                            }) if self.is_implicitly_typed_span(*span) => {
                                // TODO: Make this eficient
                                let new_ty = RTsType::from(actual.ty.clone()).validate_with(self)?;
                                if let Some(node_id) = pat.node_id() {
                                    if let Some(m) = &mut self.mutations {
                                        m.for_pats.entry(node_id).or_default().ty = Some(new_ty);
                                    }
                                }
                                return Ok(());
                            }
                            _ => {}
                        }
                    }
                    Ok(())
                };

                let ty = match &*arg.expr {
                    RExpr::Arrow(arrow) => {
                        for (idx, pat) in arrow.params.iter().enumerate() {
                            patch_arg(idx, pat)?;
                        }

                        slog::info!(self.logger, "Inferring type of arrow expr with updated type");
                        box Type::Function(arrow.validate_with(self)?)
                    }
                    RExpr::Fn(fn_expr) => {
                        for (idx, param) in fn_expr.function.params.iter().enumerate() {
                            patch_arg(idx, &param.pat)?;
                        }

                        slog::info!(self.logger, "Inferring type of function expr with updated type");
                        box Type::Function(fn_expr.function.validate_with(self)?)
                    }
                    _ => arg_ty.ty.clone(),
                };
                print_type(&logger, "mapped", &self.cm, &ty);

                let new_arg = TypeOrSpread { ty, ..arg_ty.clone() };

                new_args.push(new_arg);
            }
            // if arg.len() > param.len(), we need to add all args
            if arg_types.len() > params.len() {
                new_args.extend(arg_types[params.len()..].iter().cloned());
            }

            // We have to recalculate types.
            let mut new_arg_types;
            let spread_arg_types = if new_args.iter().any(|arg| arg.spread.is_some()) {
                new_arg_types = vec![];
                for arg in &new_args {
                    if arg.spread.is_some() {
                        match &*arg.ty {
                            Type::Tuple(arg_ty) => {
                                new_arg_types.extend(arg_ty.elems.iter().map(|element| &element.ty).cloned().map(
                                    |ty| TypeOrSpread {
                                        span: arg.spread.unwrap(),
                                        spread: None,
                                        ty,
                                    },
                                ));
                            }
                            _ => unimplemented!("spread argument with type other than tuple\nType: {:#?}", arg.ty),
                        }
                    } else {
                        new_arg_types.push(arg.clone());
                    }
                }

                &*new_arg_types
            } else {
                &*new_args
            };

            let ctx = Ctx {
                preserve_params: true,
                preserve_ret_ty: true,
                ..self.ctx
            };
            let ret_ty = self.with_ctx(ctx).expand(span, ret_ty)?;

            let inferred = self.infer_arg_types(span, type_args, type_params, &params, &spread_arg_types, None)?;

            let expanded_param_types = params
                .into_iter()
                .cloned()
                .map(|v| -> ValidationResult<_> {
                    let ty = self.expand_type_params(&inferred, v.ty)?;

                    Ok(FnParam { ty, ..v })
                })
                .collect::<Result<Vec<_>, _>>()?;

            self.validate_arg_types(&expanded_param_types, &spread_arg_types);

            print_type(&logger, "Return", &self.cm, &ret_ty);
            let mut ty = self.expand_type_params(&inferred, ret_ty)?;
            print_type(&logger, "Return, expanded", &self.cm, &ty);

            ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });

            print_type(&logger, "Return, simplified", &self.cm, &ty);

            ty = self.simplify(ty);

            print_type(&logger, "Return, simplified again", &self.cm, &ty);

            ty = ty.fold_with(&mut ReturnTypeGeneralizer { analyzer: self });

            print_type(&logger, "Return, generalized", &self.cm, &ty);

            if kind == ExtractKind::Call {
                self.add_call_facts(params, &args, &mut ty);
            }

            ty.reposition(span);

            return Ok(ty);
        }

        self.validate_arg_types(params, &spread_arg_types);

        ret_ty.reposition(span);
        ret_ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });
        if kind == ExtractKind::Call {
            self.add_call_facts(params, &args, &mut ret_ty);
        }
        return Ok(ret_ty);
    }

    fn validate_arg_types(&mut self, params: &[FnParam], spread_arg_types: &[TypeOrSpread]) {
        for pair in params
            .iter()
            .filter(|param| match param.pat {
                RPat::Ident(RIdent {
                    sym: js_word!("this"), ..
                }) => false,
                _ => true,
            })
            .zip_longest(spread_arg_types)
        {
            match pair {
                EitherOrBoth::Both(param, arg) => {
                    match &param.pat {
                        RPat::Rest(..) => match &*param.ty {
                            Type::Array(arr) => {
                                // We should change type if the parameter is a rest parameter.
                                if let Ok(()) = self.assign(&arr.elem_type, &arg.ty, arg.span()) {
                                    continue;
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    }

                    if let Err(err) = self.assign(&param.ty, &arg.ty, arg.span()) {
                        self.storage.report(box Error::WrongArgType {
                            span: arg.span(),
                            inner: err,
                        })
                    }
                }
                _ => {}
            }
        }
    }

    /// Note:
    ///
    /// ```ts
    /// function isSubscriber(val: any): val is DummySubscriber;
    /// const observerOrNext: () => void | Subscriber;
    /// const subscriber = isSubscriber(observerOrNext) ? observerOrNext : new SafeSubscriber();
    /// ```
    ///
    /// should make type of `subscriber` `SafeSubscriber`, not `Subscriber`.
    /// I (kdy1) don't know why.
    fn add_call_facts(&mut self, params: &[FnParam], args: &[RExprOrSpread], ret_ty: &mut Type) {
        match ret_ty.normalize() {
            Type::Predicate(p) => {
                let ty = match &p.ty {
                    Some(v) => v.normalize(),
                    None => return,
                };

                match &p.param_name {
                    RTsThisTypeOrIdent::TsThisType(this) => {}
                    RTsThisTypeOrIdent::Ident(arg_id) => {
                        for (idx, param) in params.iter().enumerate() {
                            match &param.pat {
                                RPat::Ident(i) if i.sym == arg_id.sym => {
                                    // TODO: Check length of args.
                                    let arg = &args[idx];
                                    match &*arg.expr {
                                        RExpr::Ident(var_name) => {
                                            self.store_call_fact_for_var(var_name.span, var_name.into(), &ty);
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    #[extra_validator]
    fn store_call_fact_for_var(&mut self, span: Span, var_name: Id, new_ty: &Type) {
        if let Some(previous_types) = self.find_var_type(&var_name.clone().into()) {
            let mut new_types = vec![];

            let mut upcasted = false;
            for ty in previous_types.into_owned().iter_union().flat_map(|ty| ty.iter_union()) {
                match self.extends(&new_ty, &ty) {
                    Some(true) => {
                        upcasted = true;
                        new_types.push(box ty.clone());
                    }
                    _ => {}
                }
            }

            // TODO: Use super class instread of
            if !upcasted {
                new_types.push(box new_ty.clone());
            }

            new_types.dedup_type();
            let mut new_ty = Type::union(new_types);
            if upcasted {
                self.env
                    .shared()
                    .marks()
                    .prevent_converting_to_children
                    .apply_to_type(&mut new_ty);
            }
            self.add_type_fact(&var_name.into(), new_ty);
            return;
        }

        self.add_type_fact(&var_name.into(), box new_ty.clone());
    }

    pub(crate) fn validate_type_args_count(
        &mut self,
        span: Span,
        type_params: Option<&[TypeParam]>,
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult<()> {
        if let Some(type_params) = type_params {
            if let Some(type_args) = type_args {
                // TODO: Handle defaults of the type parameter (Change to range)
                if type_params.len() != type_args.params.len() {
                    return Err(box Error::TypeParameterCountMismatch {
                        span,
                        max: type_params.len(),
                        min: type_params.len(),
                        actual: type_args.params.len(),
                    });
                }
            }
        }

        Ok(())
    }

    /// This method return [Err] if call is invalid
    fn check_call_args(
        &mut self,
        span: Span,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ArgCheckResult {
        if self.validate_type_args_count(span, type_params, type_args).is_err() {
            return ArgCheckResult::NeverMatches;
        }

        if self
            .validate_arg_count(span, params, args, arg_types, spread_arg_types)
            .is_err()
        {
            return ArgCheckResult::NeverMatches;
        }

        self.with_scope_for_type_params(|analyzer: &mut Analyzer| {
            if let Some(type_params) = type_params {
                for param in type_params {
                    analyzer.register_type(param.name.clone(), box Type::Param(param.clone()));
                }
            }

            let mut exact = true;

            for (arg, param) in arg_types.iter().zip(params) {
                // match arg.ty.normalize() {
                //     Type::Union(..) => match param.ty.normalize() {
                //         Type::Keyword(..) => if self.assign(&param.ty, &arg.ty, span).is_ok()
                // {},         _ => {}
                //     },
                //     _ => {}
                // }

                match param.ty.normalize() {
                    Type::Param(..) => {}
                    _ => {
                        if analyzer
                            .assign_with_opts(
                                AssignOpts {
                                    span,
                                    allow_unknown_rhs: true,
                                },
                                &param.ty,
                                &arg.ty,
                            )
                            .is_err()
                        {
                            return ArgCheckResult::NeverMatches;
                        }
                        if analyzer.assign(&arg.ty, &param.ty, span).is_err() {
                            exact = false;
                        }
                    }
                }
            }

            if analyzer.scope.is_call_arg_count_unknown || !exact {
                return ArgCheckResult::MayBe;
            }

            ArgCheckResult::Exact
        })
    }

    fn validate_args(&mut self, args: &[RExprOrSpread]) -> Result<Vec<TypeOrSpread>, Error> {
        let ctx = Ctx {
            in_argument: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a: &mut Analyzer| {
            let args: Vec<_> = args
                .into_iter()
                .map(|arg| {
                    arg.validate_with(a)
                        .report(&mut a.storage)
                        .unwrap_or_else(|| TypeOrSpread {
                            span: arg.span(),
                            spread: arg.spread,
                            ty: Type::any(arg.expr.span()),
                        })
                })
                .collect();

            Ok(args)
        })
    }
}

struct ReturnTypeGeneralizer<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl Fold<Type> for ReturnTypeGeneralizer<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        if !self.analyzer.may_generalize(&ty) {
            return ty;
        }

        ty = ty.fold_children_with(self);

        *ty.generalize_lit()
    }
}

///
/// e.g.
///
/// - `any[string]` => `any`
/// - `Shape['name']` => `string`
struct ReturnTypeSimplifier<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl VisitMut<Union> for ReturnTypeSimplifier<'_, '_, '_> {
    fn visit_mut(&mut self, union: &mut Union) {
        let should_remove_null_and_undefined = union.types.iter().any(|ty| match ty.normalize() {
            Type::TypeLit(..) => true,
            Type::Ref(..) => true,
            _ => false,
        });

        if should_remove_null_and_undefined {
            union.types.retain(|ty| {
                if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) | ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                    return false;
                }

                true
            });
        }
    }
}

impl VisitMut<Type> for ReturnTypeSimplifier<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                    }),
                ..
            }) => {
                *ty = Type::Keyword(RTsKeywordType {
                    span: *span,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                });
                return;
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: ref obj_ty @ box Type::Ref(..),
                index_type,
                ..
            }) if is_str_lit_or_union(&index_type) => {
                let mut types: Vec<Box<Type>> = vec![];

                for index_ty in index_type.iter_union() {
                    let (lit_span, value) = match &*index_ty {
                        Type::Lit(RTsLitType {
                            span: lit_span,
                            lit: RTsLit::Str(RStr { value, .. }),
                            ..
                        }) => (*lit_span, value.clone()),
                        _ => return,
                    };

                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..self.analyzer.ctx
                    };
                    let mut a = self.analyzer.with_ctx(ctx);
                    let obj = a.expand_fully(*span, obj_ty.clone(), true).report(&mut a.storage);
                    if let Some(obj) = obj {
                        if let Some(actual_ty) = a
                            .access_property(
                                *span,
                                obj,
                                &Key::Normal {
                                    span: lit_span,
                                    sym: value.clone(),
                                },
                                TypeOfMode::RValue,
                                IdCtx::Type,
                            )
                            .report(&mut a.storage)
                        {
                            if types.iter().all(|prev_ty| !(**prev_ty).type_eq(&actual_ty)) {
                                types.push(actual_ty);
                            }
                        }
                    }
                }

                *ty = *Type::union(types);
                return;
            }

            Type::IndexedAccessType(ty) if is_str_lit_or_union(&ty.index_type) => {
                ty.span = self.analyzer.prevent_generalize_span(ty.span);
                self.analyzer.prevent_generalize(&mut ty.obj_type);
                self.analyzer.prevent_generalize(&mut ty.index_type);
            }

            // Boxified<A | B | C> => Boxified<A> | Boxified<B> | Boxified<C>
            Type::Ref(Ref {
                span,
                ctxt,
                type_name: RTsEntityName::Ident(i),
                type_args: Some(type_args),
            }) if type_args.params.len() == 1
                && type_args.params.iter().any(|ty| match &**ty {
                    Type::Union(..) => true,
                    _ => false,
                }) =>
            {
                // TODO: Replace .ok() with something better
                if let Some(types) = self.analyzer.find_type(*ctxt, &(&*i).into()).ok().flatten() {
                    for stored_ty in types {
                        match stored_ty.normalize() {
                            Type::Alias(Alias { ty: aliased_ty, .. }) => {
                                let mut types = vec![];

                                match &*type_args.params[0] {
                                    Type::Union(type_arg) => {
                                        for ty in &type_arg.types {
                                            types.push(box Type::Ref(Ref {
                                                span: *span,
                                                ctxt: *ctxt,
                                                type_name: RTsEntityName::Ident(i.clone()),
                                                type_args: Some(box TypeParamInstantiation {
                                                    span: type_args.span,
                                                    params: vec![ty.clone()],
                                                }),
                                            }))
                                        }
                                    }

                                    _ => unreachable!(),
                                }

                                *ty = *Type::union(types);
                                return;
                            }
                            _ => {}
                        }
                    }
                }
            }

            _ => {}
        }
    }
}

fn is_key_eq_prop(prop: &RExpr, computed: bool, e: &RExpr) -> bool {
    let tmp;
    let v = match *e {
        RExpr::Ident(ref i) => {
            tmp = Id::from(i);
            &tmp
        }
        RExpr::Lit(RLit::Str(ref s)) => {
            tmp = Id::word(s.value.clone());
            &tmp
        }
        _ => return false,
    };

    let p = match &*prop {
        RExpr::Ident(ref i) => &i.sym,
        RExpr::Lit(RLit::Str(ref s)) if computed => &s.value,
        _ => return false,
    };

    v.sym() == p
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
enum ArgCheckResult {
    Exact,
    MayBe,
    NeverMatches,
}

/// Ensure that sort work as expected.
#[test]
fn test_arg_check_result_order() {
    let mut v = vec![
        ArgCheckResult::Exact,
        ArgCheckResult::MayBe,
        ArgCheckResult::NeverMatches,
    ];
    let expected = v.clone();
    v.sort();

    assert_eq!(v, expected);
}
