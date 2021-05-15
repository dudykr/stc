//! Handles new expressions and call expressions.
use super::super::Analyzer;
use super::IdCtx;
use crate::analyzer::assign::AssignOpts;
use crate::{
    analyzer::{
        expr::TypeOfMode,
        marks::MarkExt,
        util::{make_instance_type, ResultExt},
        Ctx, ScopeKind,
    },
    ty,
    ty::{
        CallSignature, ConstructorSignature, FnParam, Method, MethodSignature, Type, TypeElement, TypeOrSpread,
        TypeParam, TypeParamInstantiation,
    },
    util::type_ext::TypeVecExt,
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
use rnode::VisitWith;
use stc_ts_ast_rnode::RBindingIdent;
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
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_ast_rnode::RTsThisTypeOrIdent;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeParamInstantiation;
use stc_ts_ast_rnode::RTsTypeRef;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::debug::print_type;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_generics::type_param::finder::TypeParamUsageFinder;
use stc_ts_type_ops::is_str_lit_or_union;
use stc_ts_type_ops::Fix;
use stc_ts_types::Array;
use stc_ts_types::Class;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassProperty;
use stc_ts_types::Instance;
use stc_ts_types::Interface;
use stc_ts_types::Key;
use stc_ts_types::ModuleId;
use stc_ts_types::{Alias, Id, IndexedAccessType, Ref, Symbol, Union};
use stc_ts_utils::PatExt;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::SyntaxContext;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;
use swc_common::{Span, Spanned};
use swc_ecma_ast::TsKeywordTypeKind;
use ty::TypeExt;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExprOrSpread) -> ValidationResult<TypeOrSpread> {
        let span = node.span();
        Ok(TypeOrSpread {
            span,
            spread: node.spread,
            ty: box node.expr.validate_with_default(self)?,
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
            RExprOrSuper::Super(..) => {
                self.report_error_for_super_refs_without_supers(span, true);
                self.report_error_for_super_reference_in_compute_keys(span, true);

                if type_args.is_some() {
                    // super<T>() is invalid.
                    self.storage.report(Error::SuperCannotUseTypeArgs { span })
                }

                return Ok(Type::any(span));
            }
            RExprOrSuper::Expr(callee) => callee,
        };

        let is_callee_iife = is_fn_expr(&callee);

        // TODO: validate children

        self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.ctx.is_calling_iife = is_callee_iife;

            analyzer.extract_call_new_expr_member(
                span,
                ReevalMode::Call(e),
                callee,
                ExtractKind::Call,
                args,
                type_args.as_ref(),
                type_ann,
            )
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
                ReevalMode::New(e),
                callee,
                ExtractKind::New,
                args.as_ref().map(|v| &**v).unwrap_or_else(|| &mut []),
                type_args.as_ref(),
                type_ann,
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

        args.extend(
            e.tpl
                .exprs
                .iter()
                .cloned()
                .map(|expr| RExprOrSpread { spread: None, expr }),
        );

        let res = self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.extract_call_new_expr_member(
                span,
                ReevalMode::NoReeval,
                &e.tag,
                ExtractKind::Call,
                args.as_ref(),
                Default::default(),
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
        expr: ReevalMode,
        callee: &RExpr,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        type_args: Option<&RTsTypeParamInstantiation>,
        type_ann: Option<&Type>,
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

        if self.ctx.in_computed_prop_name {
            if let Some(type_args) = &type_args {
                let mut v = TypeParamUsageFinder::default();
                type_args.visit_with(&mut v);
                self.report_error_for_usage_of_type_param_of_declaring_class(&v.params, span);
            }
        }

        let arg_types = self.validate_args(args)?;
        let spread_arg_types = self
            .spread_args(&arg_types)
            .context("tried to handle spreads in arguments")?;

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
                if kind == ExtractKind::New {
                    self.storage.report(Error::CannotCallWithNewNonVoidFunction { span })
                }

                // Symbol uses special type
                if !args.is_empty() {
                    unimplemented!("Error reporting for calling `Symbol` with arguments is not implemented yet")
                }

                return Ok(Type::Symbol(Symbol {
                    span,
                    id: self.symbols.generate(),
                }));
            }

            // Use general callee validation.
            RExpr::Member(RMemberExpr {
                prop: box RExpr::Lit(RLit::Num(..)),
                computed: true,
                ..
            }) => {}

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
                        return Ok(Type::from(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }
                }

                // Handle member expression
                let obj_type = obj.validate_with_default(self)?.generalize_lit();

                let mut obj_type = match *obj_type.normalize() {
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

                return self
                    .call_property(
                        span,
                        kind,
                        expr,
                        &obj_type,
                        &obj_type,
                        &prop,
                        type_args.as_ref(),
                        args,
                        &arg_types,
                        &spread_arg_types,
                        type_ann,
                    )
                    .map(|ty| ty.fixed());
            }
            _ => {}
        }

        let ctx = Ctx {
            preserve_ref: false,
            ignore_expand_prevention_for_all: false,
            ignore_expand_prevention_for_top: false,
            preserve_ret_ty: true,
            preserve_params: true,
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
                match callee_ty.normalize() {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) if type_args.is_some() => {
                        // If it's implicit any, we should postpone this check.
                        if !analyzer.is_implicitly_typed(&callee_ty) {
                            analyzer.storage.report(Error::AnyTypeUsedAsCalleeWithTypeArgs { span })
                        }
                    }
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
                expr,
                &callee_ty,
                kind,
                args,
                &arg_types,
                &spread_arg_types,
                type_args.as_ref(),
                type_ann,
            )?;

            return Ok(expanded_ty.fixed());
        })
    }

    /// TODO: Use Cow for `obj_type`
    ///
    /// ## Parameters
    ///
    ///  - `expr`: Can be default if argument does not include an arrow
    ///    expression nor a function expression.
    pub(super) fn call_property(
        &mut self,
        span: Span,
        kind: ExtractKind,
        expr: ReevalMode,
        this: &Type,
        obj_type: &Type,
        prop: &Key,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let old_this = self.scope.this.take();
        self.scope.this = Some(this.clone());

        let res = (|| {
            match obj_type.normalize() {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                }) => {
                    return Ok(Type::any(span));
                }

                Type::This(..) => {
                    if self.ctx.in_computed_prop_name {
                        self.storage
                            .report(Error::CannotReferenceThisInComputedPropName { span });
                        // Return any to prevent other errors
                        return Ok(Type::any(span));
                    }
                }

                Type::Array(obj) => {
                    let obj = Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new(
                            "Array".into(),
                            span.with_ctxt(self.marks().top_level_mark.as_ctxt()),
                        )),
                        type_args: Some(box TypeParamInstantiation {
                            span,
                            params: vec![*obj.elem_type.clone()],
                        }),
                    });
                    return self.call_property(
                        span,
                        kind,
                        expr,
                        this,
                        &obj,
                        prop,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                    );
                }

                Type::Intersection(obj) => {
                    let types = obj
                        .types
                        .iter()
                        .map(|obj| {
                            self.call_property(
                                span,
                                kind,
                                expr,
                                this,
                                obj,
                                prop,
                                type_args,
                                args,
                                arg_types,
                                spread_arg_types,
                                type_ann,
                            )
                        })
                        .filter_map(Result::ok)
                        .collect_vec();

                    if types.is_empty() {
                        if kind == ExtractKind::Call {
                            return Err(Error::NoCallabelPropertyWithName {
                                span,
                                key: box prop.clone(),
                            });
                        } else {
                            return Err(Error::NoSuchConstructor {
                                span,
                                key: box prop.clone(),
                            });
                        }
                    }

                    return Ok(Type::union(types));
                }

                Type::Ref(..) => {
                    let obj_type = self
                        .expand_top_ref(span, Cow::Borrowed(obj_type))
                        .context("tried to expand object to call property of it")?;

                    return self
                        .call_property(
                            span,
                            kind,
                            expr,
                            this,
                            &obj_type,
                            prop,
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                            type_ann,
                        )
                        .context("tried to call a property of expanded type");
                }

                Type::Interface(ref i) => {
                    // We check for body before parent to support overriding
                    let err = match self.call_property_of_type_elements(
                        kind,
                        expr,
                        span,
                        &obj_type,
                        &i.body,
                        &prop,
                        type_args,
                        args,
                        &arg_types,
                        &spread_arg_types,
                        type_ann,
                    ) {
                        Ok(v) => return Ok(v),
                        Err(err) => err,
                    };

                    // Check parent interface
                    for parent in &i.extends {
                        let parent = self
                            .type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, parent.type_args.as_deref())
                            .context("tried to check parent interface to call a property of it")?;
                        if let Ok(v) = self.call_property(
                            span,
                            kind,
                            expr,
                            this,
                            &parent,
                            prop,
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                            type_ann,
                        ) {
                            return Ok(v);
                        }
                    }

                    return Err(err);
                }

                Type::TypeLit(ref t) => {
                    return self.call_property_of_type_elements(
                        kind,
                        expr,
                        span,
                        &obj_type,
                        &t.members,
                        &prop,
                        type_args,
                        args,
                        &arg_types,
                        &spread_arg_types,
                        type_ann,
                    );
                }

                Type::ClassDef(cls) => {
                    if let Some(v) = self.call_property_of_class(
                        span,
                        expr,
                        kind,
                        this,
                        cls,
                        prop,
                        true,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                    )? {
                        return Ok(v);
                    }
                }

                Type::Class(ty::Class { def, .. }) => {
                    if let Some(v) = self.call_property_of_class(
                        span,
                        expr,
                        kind,
                        this,
                        def,
                        prop,
                        false,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                    )? {
                        return Ok(v);
                    }
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

            // Handle methods from `Object`.
            match obj_type.normalize() {
                Type::Interface(Interface { name, .. }) if *name.sym() == js_word!("Object") => {}
                _ => {
                    let obj_res = self.call_property(
                        span,
                        kind,
                        expr,
                        this,
                        &Type::Ref(Ref {
                            span: DUMMY_SP,
                            type_name: RTsEntityName::Ident(RIdent::new(
                                js_word!("Object"),
                                DUMMY_SP.with_ctxt(self.marks().top_level_mark.as_ctxt()),
                            )),
                            ctxt: ModuleId::builtin(),
                            type_args: None,
                        }),
                        prop,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                    );
                    match obj_res {
                        Ok(v) => return Ok(v),
                        Err(..) => {}
                    }
                }
            }

            // Use proper error.
            match obj_type.normalize() {
                Type::Class(..) => {
                    return Err(match kind {
                        ExtractKind::Call => Error::NoCallabelPropertyWithName {
                            span,
                            key: box prop.clone(),
                        },
                        ExtractKind::New => Error::NoSuchConstructor {
                            span,
                            key: box prop.clone(),
                        },
                    });
                }
                _ => {}
            }

            let ctx = Ctx {
                diallow_unknown_object_property: true,
                ..self.ctx
            };
            let callee = self
                .with_ctx(ctx)
                .access_property(span, obj_type, &prop, TypeOfMode::RValue, IdCtx::Var)?;

            let callee = self.expand_top_ref(span, Cow::Owned(callee))?.into_owned();

            match callee.normalize() {
                Type::ClassDef(cls) => {
                    if cls.is_abstract {
                        self.storage.report(Error::CannotCreateInstanceOfAbstractClass { span })
                    }
                }
                _ => {}
            }
            let callee_str = dump_type_as_string(&self.cm, &callee);

            self.get_best_return_type(
                span,
                expr,
                callee,
                kind,
                type_args,
                args,
                &arg_types,
                &spread_arg_types,
                type_ann,
            )
            .convert_err(|err| match err {
                Error::NoCallSignature { span, .. } => Error::NoCallabelPropertyWithName {
                    span,
                    key: box prop.clone(),
                },
                Error::NoNewSignature { span, .. } => Error::NoCallabelPropertyWithName {
                    span,
                    key: box prop.clone(),
                },
                _ => err,
            })
            .with_context(|| {
                format!(
                    "tried to call property by using access_property because the object type is not handled by \
                     call_property: \nobj = {}\ncallee = {}",
                    dump_type_as_string(&self.cm, &obj_type),
                    callee_str
                )
            })
        })();
        self.scope.this = old_this;
        res
    }

    fn call_property_of_class(
        &mut self,
        span: Span,
        expr: ReevalMode,
        kind: ExtractKind,
        this: &Type,
        c: &ClassDef,
        prop: &Key,
        is_static_call: bool,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> ValidationResult<Option<Type>> {
        let mut candidates: Vec<CallCandidate> = vec![];
        for member in c.body.iter() {
            match member {
                ty::ClassMember::Method(Method {
                    key,
                    ret_ty,
                    type_params,
                    params,
                    is_static,
                    ..
                }) if *is_static == is_static_call => {
                    if self.key_matches(span, key, prop, false) {
                        candidates.push(CallCandidate {
                            type_params: type_params.as_ref().map(|v| v.params.clone()),
                            params: params.clone(),
                            ret_ty: *ret_ty.clone(),
                        });
                    }
                }
                ty::ClassMember::Property(ClassProperty {
                    key, value, is_static, ..
                }) if *is_static == is_static_call => {
                    if self.key_matches(span, key, prop, false) {
                        // Check for properties with callable type.

                        // TODO: Change error message from no callable
                        // property to property exists but not callable.
                        if let Some(ty) = &value {
                            return self
                                .extract(
                                    span,
                                    expr,
                                    ty,
                                    kind,
                                    args,
                                    arg_types,
                                    spread_arg_types,
                                    type_args,
                                    type_ann,
                                )
                                .map(Some);
                        }
                    }
                }
                _ => {}
            }
        }

        if let Some(v) = self.select_and_invoke(
            span,
            kind,
            expr,
            &candidates,
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )? {
            return Ok(Some(v));
        }

        if let Some(ty) = &c.super_class {
            let ty = if is_static_call {
                *ty.clone()
            } else {
                self.instantiate_class(span, ty)
                    .context("tried to instantiate a class to call property of a super class")?
            };
            if let Ok(ret_ty) = self.call_property(
                span,
                kind,
                expr,
                this,
                &ty,
                prop,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
            ) {
                return Ok(Some(ret_ty));
            }
        }

        Ok(None)
    }

    fn check_type_element_for_call<'a>(
        &mut self,
        span: Span,
        kind: ExtractKind,
        candidates: &mut Vec<CallCandidate>,
        m: &'a TypeElement,
        prop: &Key,
    ) {
        match m {
            TypeElement::Method(m) if kind == ExtractKind::Call => {
                if self.ctx.disallow_optional_object_property && m.optional {
                    // See: for-of29.ts
                    // Optional properties cannot be called.
                    return;
                }

                // We are interested only on methods named `prop`
                if let Ok(()) = self.assign(&mut Default::default(), &m.key.ty(), &prop.ty(), span) {
                    candidates.push(CallCandidate {
                        type_params: m.type_params.as_ref().map(|v| v.params.clone()),
                        params: m.params.clone(),
                        ret_ty: m.ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(m.span)),
                    });
                }
            }

            TypeElement::Property(p) if kind == ExtractKind::Call => {
                if self.ctx.disallow_optional_object_property && p.optional {
                    // See: for-of29.ts
                    // Optional properties cannot be called.
                    return;
                }

                if let Ok(()) = self.assign(&mut Default::default(), &p.key.ty(), &prop.ty(), span) {
                    // TODO: Remove useless clone
                    let ty = *p.type_ann.as_ref().cloned().unwrap_or(box Type::any(m.span()));

                    match ty.foldable() {
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => candidates.push(CallCandidate {
                            // TODO: Maybe we need Option<Vec<T>>.
                            params: Default::default(),
                            ret_ty: Type::any(span),
                            type_params: Default::default(),
                        }),

                        Type::Function(f) => {
                            candidates.push(CallCandidate {
                                params: f.params,
                                ret_ty: *f.ret_ty,
                                type_params: f.type_params.clone().map(|v| v.params),
                            });
                        }

                        _ => {}
                    }
                }
            }

            _ => {}
        }
    }

    fn call_property_of_type_elements(
        &mut self,
        kind: ExtractKind,
        expr: ReevalMode,
        span: Span,
        obj: &Type,
        members: &[TypeElement],
        prop: &Key,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
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

        if let Some(v) = self.select_and_invoke(
            span,
            kind,
            expr,
            &candidates,
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )? {
            return Ok(v);
        }

        Err(Error::NoSuchProperty {
            span,
            obj: Some(box obj.clone()),
            prop: Some(box prop.clone()),
        })
    }

    /// Returns `()`
    fn spread_args<'a>(&mut self, arg_types: &'a [TypeOrSpread]) -> ValidationResult<Cow<'a, [TypeOrSpread]>> {
        let mut new_arg_types;

        if arg_types.iter().any(|arg| arg.spread.is_some()) {
            new_arg_types = vec![];
            for arg in arg_types {
                if arg.spread.is_some() {
                    let arg_ty = self
                        .expand_top_ref(arg.span(), Cow::Borrowed(&arg.ty))
                        .context("tried to expand ref to handle a spread argument")?;
                    match arg_ty.normalize() {
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
                                ty: box arg_ty.clone().into_owned(),
                            });
                        }

                        Type::Array(arr) => {
                            self.scope.is_call_arg_count_unknown = true;
                            new_arg_types.push(arg.clone());
                        }

                        _ => {
                            self.scope.is_call_arg_count_unknown = true;

                            let elem_type = self
                                .get_iterator_element_type(arg.span(), arg_ty, false)
                                .context("tried to get element type of an iterator for spread syntax in arguments")?;

                            new_arg_types.push(TypeOrSpread {
                                span: arg.span(),
                                spread: arg.spread,
                                ty: box elem_type.into_owned(),
                            });
                        }
                    }
                } else {
                    new_arg_types.push(arg.clone());
                }
            }

            return Ok(Cow::Owned(new_arg_types));
        } else {
            return Ok(Cow::Borrowed(arg_types));
        }
    }

    fn extract(
        &mut self,
        span: Span,
        expr: ReevalMode,
        ty: &Type,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        if !self.is_builtin {
            ty.assert_valid();
        }

        match ty.normalize() {
            Type::Ref(..) | Type::Query(..) => {
                let ty = self.normalize(None, Cow::Borrowed(ty), Default::default())?;
                return self.extract(
                    span,
                    expr,
                    &ty,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                    type_ann,
                );
            }

            _ => {}
        }

        slog::debug!(
            self.logger,
            "[exprs/call] Calling {}",
            dump_type_as_string(&self.cm, &ty)
        );

        match kind {
            ExtractKind::Call => match ty.normalize() {
                Type::Interface(i) if i.name == "Function" => return Ok(Type::any(span)),
                _ => {}
            },
            _ => {}
        }

        match kind {
            ExtractKind::New => match ty.normalize() {
                Type::ClassDef(ref cls) => {
                    if cls.is_abstract {
                        self.storage.report(Error::CannotCreateInstanceOfAbstractClass { span })
                    }

                    if let Some(type_params) = &cls.type_params {
                        for param in &type_params.params {
                            self.register_type(param.name.clone(), Type::Param(param.clone()));
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

                            let type_args = self.instantiate(span, &type_params.params, inferred)?;

                            return Ok(Type::Class(Class {
                                span,
                                def: box cls.clone(),
                            }));
                        }

                        let ret_ty = self.get_return_type(
                            span,
                            kind,
                            expr,
                            Some(&type_params.params),
                            &[],
                            Type::Class(Class {
                                span,
                                def: box cls.clone(),
                            }),
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                            type_ann,
                        )?;

                        return Ok(ret_ty);
                    }

                    return Ok(Type::Class(Class {
                        span,
                        def: box cls.clone(),
                    }));
                }

                Type::Constructor(c) => {
                    return self.get_return_type(
                        span,
                        kind,
                        expr,
                        c.type_params.as_ref().map(|v| &*v.params),
                        &c.params,
                        *c.type_ann.clone(),
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                    )
                }

                Type::This(..) => {
                    return Ok(Type::Instance(Instance {
                        span,
                        of: box Type::This(RTsThisType { span }),
                    }))
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
                        return Err(Error::NoCallSignature {
                            span,
                            callee: box ty.clone(),
                        })
                    }
                    ExtractKind::New => {
                        return Err(Error::NoNewSignature {
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
                return Ok(make_instance_type(self.ctx.module_id, ty.clone()));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(Type::any(span)),

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                debug_assert!(!span.is_dummy());
                return Err(Error::Unknown { span });
            }

            Type::Function(ref f) if kind == ExtractKind::Call => self.get_return_type(
                span,
                kind,
                expr,
                f.type_params.as_ref().map(|v| &*v.params),
                &f.params,
                *f.ret_ty.clone(),
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
            ),

            // new fn()
            Type::Function(f) => self.get_return_type(
                span,
                kind,
                expr,
                f.type_params.as_ref().map(|v| &*v.params),
                &f.params,
                Type::any(span),
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
            ),

            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => {
                return self.extract(
                    span,
                    expr,
                    constraint,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                    type_ann,
                )
            }

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
            Type::Union(..) => self.get_best_return_type(
                span,
                expr,
                ty.clone(),
                kind,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
            ),

            Type::Interface(ref i) => {
                if kind == ExtractKind::New && &**i.name.sym() == "ArrayConstructor" {
                    if let Some(type_args) = type_args {
                        if type_args.params.len() == 1 {
                            return Ok(Type::Array(Array {
                                span,
                                elem_type: box type_args.params.iter().cloned().next().unwrap(),
                            }));
                        }
                    }
                }

                // Search for methods
                match self.search_members_for_extract(
                    span,
                    expr,
                    &ty,
                    i.type_params.as_ref().map(|v| &*v.params),
                    &i.body,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                    type_ann,
                ) {
                    Ok(ty) => return Ok(ty.clone()),
                    Err(first_err) => {
                        //  Check parent interface
                        for parent in &i.extends {
                            let parent =
                                self.type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, type_args)?;

                            if let Ok(v) = self.extract(
                                span,
                                expr,
                                &parent,
                                kind,
                                args,
                                arg_types,
                                spread_arg_types,
                                type_args,
                                type_ann,
                            ) {
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
                    expr,
                    &ty,
                    None,
                    &l.members,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                    type_ann,
                );
            }

            Type::ClassDef(ref def) if kind == ExtractKind::New => {
                // TODO: Remove clone
                return Ok(Class {
                    span,
                    def: box def.clone(),
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
        expr: ReevalMode,
        callee_ty: &Type,
        type_params_of_type: Option<&[TypeParam]>,
        members: &[TypeElement],
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
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
                }) if kind == ExtractKind::Call => Some(CallCandidate {
                    params: params.clone(),
                    type_params: type_params
                        .clone()
                        .map(|v| v.params)
                        .or_else(|| type_params_of_type.map(|v| v.to_vec())),
                    ret_ty: ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(*span)),
                }),
                TypeElement::Constructor(ConstructorSignature {
                    span,
                    params,
                    ret_ty,
                    type_params,
                    ..
                }) if kind == ExtractKind::New => Some(CallCandidate {
                    params: params.clone(),
                    type_params: type_params
                        .clone()
                        .map(|v| v.params)
                        .or_else(|| type_params_of_type.clone().map(|v| v.to_vec())),
                    ret_ty: ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(*span)),
                }),
                _ => None,
            })
            .collect::<Vec<_>>();

        if let Some(v) = self.select_and_invoke(
            span,
            kind,
            expr,
            &candidates,
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )? {
            return Ok(v);
        }

        match kind {
            ExtractKind::Call => Err(Error::NoCallSignature {
                span,
                callee: box callee_ty.clone(),
            }),
            ExtractKind::New => Err(Error::NoNewSignature {
                span,
                callee: box callee_ty.clone(),
            }),
        }
    }

    fn check_method_call(
        &mut self,
        span: Span,
        expr: ReevalMode,
        c: &MethodSignature,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        self.get_return_type(
            span,
            ExtractKind::Call,
            expr,
            c.type_params.as_ref().map(|v| &*v.params),
            &c.params,
            c.ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(span)),
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )
    }

    fn extract_callee_candidates(
        &mut self,
        span: Span,
        kind: ExtractKind,
        callee: &Type,
    ) -> ValidationResult<Vec<CallCandidate>> {
        let callee = callee.normalize();

        // TODO: Check if signature match.
        match callee {
            Type::Ref(..) => {
                let callee = self.expand_top_ref(span, Cow::Borrowed(callee))?.into_owned();
                return Ok(self.extract_callee_candidates(span, kind, &callee)?);
            }

            Type::Intersection(i) => {
                let candidates = i
                    .types
                    .iter()
                    .map(|callee| self.extract_callee_candidates(span, kind, callee))
                    .filter_map(Result::ok)
                    .collect::<Vec<_>>();

                return Ok(candidates.into_iter().flatten().collect());
            }

            Type::Constructor(c) if kind == ExtractKind::New => {
                let candidate = CallCandidate {
                    type_params: c.type_params.clone().map(|v| v.params),
                    params: c.params.clone(),
                    ret_ty: *c.type_ann.clone(),
                };
                return Ok(vec![candidate]);
            }

            Type::Function(f) if kind == ExtractKind::Call => {
                let candidate = CallCandidate {
                    type_params: f.type_params.clone().map(|v| v.params),
                    params: f.params.clone(),
                    ret_ty: *f.ret_ty.clone(),
                };
                return Ok(vec![candidate]);
            }

            // Type::Union(ty) => {
            //     // TODO: We should select best one based on the arugment type and count.
            //     let mut types = ty
            //         .types
            //         .iter()
            //         .cloned()
            //         .map(|callee| {
            //             self.get_best_return_type(span, callee, kind, type_args, args, arg_types,
            // spread_arg_types)         })
            //         .collect::<Result<Vec<_>, _>>()?;

            //     types.dedup_type();
            //     return Ok(Type::union(types));
            // }
            Type::Union(ref u) => {
                let candidates = u
                    .types
                    .iter()
                    .map(|callee| self.extract_callee_candidates(span, kind, callee))
                    .collect::<Result<Vec<_>, _>>()?;

                return Ok(candidates.into_iter().flatten().collect());
            }

            Type::Interface(..) => {
                let callee = self
                    .type_to_type_lit(span, callee)?
                    .map(Cow::into_owned)
                    .map(Type::TypeLit);
                if let Some(callee) = callee {
                    return Ok(self.extract_callee_candidates(span, kind, &callee)?);
                }
            }

            Type::TypeLit(ty) => {
                let mut candidates = vec![];
                // Search for callable properties.
                for member in &ty.members {
                    match member {
                        TypeElement::Call(m) if kind == ExtractKind::Call => {
                            candidates.push(CallCandidate {
                                type_params: m.type_params.clone().map(|v| v.params),
                                params: m.params.clone(),
                                ret_ty: m.ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(m.span)),
                            });
                        }

                        TypeElement::Constructor(m) if kind == ExtractKind::New => {
                            candidates.push(CallCandidate {
                                type_params: m.type_params.clone().map(|v| v.params),
                                params: m.params.clone(),
                                ret_ty: m.ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(m.span)),
                            });
                        }
                        _ => {}
                    }
                }

                return Ok(candidates);
            }

            _ => {}
        }

        Ok(vec![])
    }

    fn get_best_return_type(
        &mut self,
        span: Span,
        expr: ReevalMode,
        callee: Type,
        kind: ExtractKind,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let has_spread = arg_types.len() != spread_arg_types.len();

        // TODO: Calculate return type only if selected
        // This can be done by storing type params, return type, params in the
        // candidates.
        let mut candidates = self.extract_callee_candidates(span, kind, &callee)?;

        slog::info!(self.logger, "get_best_return_type: {} candidates", candidates.len());

        if let Some(v) = self.select_and_invoke(
            span,
            kind,
            expr,
            &candidates,
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )? {
            return Ok(v);
        }

        dbg!();

        match callee.normalize() {
            Type::ClassDef(cls) if kind == ExtractKind::New => {
                let ret_ty = self.get_return_type(
                    span,
                    kind,
                    expr,
                    cls.type_params.as_ref().map(|v| &*v.params),
                    &[],
                    callee.clone(),
                    type_args,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_ann,
                )?;
                return Ok(ret_ty);
            }
            _ => {}
        }

        return Err(if kind == ExtractKind::Call {
            print_backtrace();
            Error::NoCallSignature {
                span,
                callee: box callee,
            }
        } else {
            Error::NoNewSignature {
                span,
                callee: box callee,
            }
        });
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
                RPat::Ident(RBindingIdent {
                    id: RIdent {
                        sym: js_word!("this"), ..
                    },
                    ..
                }) => {
                    if let Some(max) = &mut max_param {
                        *max -= 1;
                    }
                    continue;
                }
                _ => {}
            }
            if param.required {
                if !param.ty.is_any()
                    && self
                        .assign(
                            &mut Default::default(),
                            &param.ty,
                            &Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                            }),
                            span,
                        )
                        .is_ok()
                {
                    // Reduce min_params if the type of parameter accepts void.
                    continue;
                }

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

            // For iifes, not providing some arguemnts are allowed.
            if self.ctx.is_calling_iife {
                if let Some(max) = max_param {
                    if args.len() <= max {
                        return Ok(());
                    }
                }
            }

            if max_param.is_none() {
                return Err(Error::ExpectedAtLeastNArgsButGotM { span, min: min_param });
            }

            let span = args.get(min_param).map(|arg| arg.expr.span()).unwrap_or(span);

            return Err(Error::ExpectedNArgsButGotM {
                span,
                min: min_param,
                max: max_param,
            });
        }
    }

    /// Returns [None] if nothing matched.
    fn select_and_invoke(
        &mut self,
        span: Span,
        kind: ExtractKind,
        expr: ReevalMode,
        candidates: &[CallCandidate],
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> ValidationResult<Option<Type>> {
        let mut callable = candidates
            .iter()
            .map(|c| {
                let res = self.check_call_args(
                    span,
                    c.type_params.as_deref(),
                    &c.params,
                    type_args,
                    args,
                    arg_types,
                    spread_arg_types,
                );

                (c, res)
            })
            .collect::<Vec<_>>();
        callable.sort_by_key(|(_, res)| res.clone());

        if candidates.is_empty() {
            return Ok(None);
        }

        let (c, _) = callable.into_iter().next().unwrap();

        if candidates.len() == 1 {
            return self
                .get_return_type(
                    span,
                    kind,
                    expr,
                    c.type_params.as_deref(),
                    &c.params,
                    c.ret_ty.clone(),
                    type_args,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_ann,
                )
                .map(Some);
        }

        self.get_return_type(
            span,
            kind,
            expr,
            c.type_params.as_deref(),
            &c.params,
            c.ret_ty.clone(),
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )
        .map(Some)
    }

    /// Returns the return type of function. This method should be called only
    /// for final step because it emits errors instead of returning them.
    ///
    /// ## Note
    ///
    /// We should evaluate two time because of code like below.
    ///
    ///
    /// ```ts
    /// declare function getType<T>(arr: T[]): string;
    /// declare function getType(obj: { foo(n: number): number[] }): string;
    /// declare function wrap<A, B>(f: (a: A) => B): (a: A) => B;
    ///
    /// getType({
    ///    foo: wrap((a) => [a.toExponential()]),
    /// })
    /// ```
    ///
    /// In this example,
    ///
    ///  - we can't calculate the type of `a.toExponential()` because we don't
    ///    know the type of `a`
    ///  - we can't use type annotation because of `wrap`
    ///  - we can't determine the function to call before validating arguments
    ///  - we can't use type annotation of the function because we cannot
    ///    determine the function to call because of `wrap`
    ///
    /// To fix this problem, we evaluate calls twice.
    ///
    /// If then, the logic becomes simple.
    ///
    ///  1. We set type of `a` to `any`.
    ///  2. Type of `a.toExponential()` is `any`.
    ///  3. Type of the arrow function is `(a: any) => [any]`.
    ///  4. Type of the property `foo` is `<A, B>(a: A) => B` where A = `any`
    /// and B = `[any]`.
    ///  5. We select appropriate function to call.
    ///  6. Type of `a` is now number.
    ///  7. Type of `a.toExponential()` is `number`.
    ///  8. Type of the arrow function is `(a: number) => [number]`.
    ///  9. Type of the property `foo` is `<A, B>(a: A) => B` where A = `number`
    /// and B = `[number]`.
    fn get_return_type(
        &mut self,
        span: Span,
        kind: ExtractKind,
        expr: ReevalMode,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        mut ret_ty: Type,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let logger = self.logger.clone();

        // TODO: Optimize by skipping clone if `this type` is not used.
        let params = params
            .iter()
            .map(|param| {
                let mut ty = param.ty.clone();
                self.expand_this(&mut ty);
                FnParam { ty, ..param.clone() }
            })
            .collect_vec();
        self.expand_this(&mut ret_ty);

        {
            let arg_check_res = self.validate_arg_count(span, &params, args, arg_types, spread_arg_types);
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

                self.register_type(param.name.clone(), Type::Param(param.clone()));
            }

            let inferred_from_return_type = match type_ann {
                Some(type_ann) => self
                    .infer_type_with_types(span, type_params, &ret_ty, type_ann)
                    .map(Some)?,
                None => None,
            };

            let expanded_params;
            let params = if let Some(map) = &inferred_from_return_type {
                expanded_params = params
                    .into_iter()
                    .map(|v| -> ValidationResult<_> {
                        let mut ty = box self.expand_type_params(&map, *v.ty)?;
                        ty.fix();

                        Ok(FnParam { ty, ..v })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                expanded_params
            } else {
                params
            };

            slog::debug!(self.logger, "Inferring arg types for a call");
            let inferred = self.infer_arg_types(span, type_args, type_params, &params, &spread_arg_types, None)?;

            let expanded_param_types = params
                .into_iter()
                .map(|v| -> ValidationResult<_> {
                    let mut ty = box self.expand_type_params(&inferred, *v.ty)?;
                    ty.fix();
                    ty.assert_valid();

                    Ok(FnParam { ty, ..v })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let ctx = Ctx {
                in_argument: true,
                reevaluating_argument: true,
                ..self.ctx
            };
            let mut new_args = vec![];
            for (idx, (arg, param)) in args.into_iter().zip(expanded_param_types.iter()).enumerate() {
                let arg_ty = &arg_types[idx];
                print_type(
                    &self.logger,
                    &format!("Expanded parameter at {}", idx),
                    &self.cm,
                    &param.ty,
                );
                print_type(
                    &self.logger,
                    &format!("Original argument at {}", idx),
                    &self.cm,
                    &arg_ty.ty,
                );

                let (type_param_decl, actual_params) = match &*param.ty {
                    Type::Function(f) => (&f.type_params, &f.params),
                    _ => {
                        new_args.push(arg_ty.clone());
                        continue;
                    }
                };

                if let Some(type_param_decl) = type_param_decl {
                    for param in &type_param_decl.params {
                        self.register_type(param.name.clone(), Type::Param(param.clone()));
                    }
                }

                let mut patch_arg = |idx: usize, pat: &RPat| -> ValidationResult<()> {
                    if actual_params.len() <= idx {
                        return Ok(());
                    }
                    let actual = &actual_params[idx];

                    let default_any_ty: Option<_> = try {
                        let node_id = pat.node_id()?;
                        self.mutations.as_ref()?.for_pats.get(&node_id)?.ty.clone()?
                    };

                    if let Some(ty) = default_any_ty {
                        match &ty {
                            Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                            }) if self.is_implicitly_typed_span(*span) => {
                                // let new_ty = RTsType::from(actual.ty.clone()).validate_with(self)?;
                                // if let Some(node_id) = pat.node_id() {
                                //     if let Some(m) = &mut self.mutations {
                                //         m.for_pats.entry(node_id).or_default().ty = Some(new_ty);
                                //     }
                                // }
                                let mut new_ty = *actual.ty.clone();
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
                        // It's okay to use default as we have patched parameters.
                        let mut ty = box Type::Function(arrow.validate_with_default(&mut *self.with_ctx(ctx))?);
                        self.add_required_type_params(&mut ty);
                        ty
                    }
                    RExpr::Fn(fn_expr) => {
                        for (idx, param) in fn_expr.function.params.iter().enumerate() {
                            patch_arg(idx, &param.pat)?;
                        }

                        slog::info!(self.logger, "Inferring type of function expr with updated type");
                        let mut ty = box Type::Function(
                            fn_expr
                                .function
                                .validate_with_args(&mut *self.with_ctx(ctx), fn_expr.ident.as_ref())?,
                        );
                        self.add_required_type_params(&mut ty);
                        ty
                    }
                    _ => arg_ty.ty.clone(),
                };
                print_type(
                    &self.logger,
                    &format!("Mapped argument at {}", idx),
                    &self.cm,
                    &arg_ty.ty,
                );

                let new_arg = TypeOrSpread { ty, ..arg_ty.clone() };

                new_args.push(new_arg);
            }

            if !self.ctx.reevaluating_call_or_new {
                slog::debug!(self.logger, "Reevaluating a call");
                let ctx = Ctx {
                    reevaluating_call_or_new: true,
                    ..self.ctx
                };
                match expr {
                    ReevalMode::Call(e) => {
                        return e.validate_with_default(&mut *self.with_ctx(ctx));
                    }
                    ReevalMode::New(e) => {
                        return e.validate_with_default(&mut *self.with_ctx(ctx));
                    }
                    _ => {}
                }
            }

            // if arg.len() > param.len(), we need to add all args
            if arg_types.len() > expanded_param_types.len() {
                for idx in expanded_param_types.len()..arg_types.len() {
                    let ty = &arg_types[idx].ty;
                    print_type(&self.logger, &format!("Expanded param type at {}", idx), &self.cm, &ty);
                }
                new_args.extend(arg_types[expanded_param_types.len()..].iter().cloned());
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
                            _ => {
                                new_arg_types.push(arg.clone());
                            }
                        }
                    } else {
                        new_arg_types.push(arg.clone());
                    }
                }

                new_arg_types.fix();

                &*new_arg_types
            } else {
                new_args.fix();

                &*new_args
            };

            let ctx = Ctx {
                preserve_params: true,
                preserve_ret_ty: true,
                ..self.ctx
            };
            let ret_ty = self.with_ctx(ctx).expand(span, ret_ty)?;

            for item in &expanded_param_types {
                item.ty.assert_valid();
            }

            for item in spread_arg_types {
                item.ty.assert_valid();
            }

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

            self.add_required_type_params(&mut ty);

            if kind == ExtractKind::Call {
                self.add_call_facts(&expanded_param_types, &args, &mut ty);
            }

            ty.reposition(span);

            return Ok(ty);
        }

        self.validate_arg_types(&params, &spread_arg_types);

        ret_ty.reposition(span);
        ret_ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });
        self.add_required_type_params(&mut ret_ty);

        if kind == ExtractKind::Call {
            self.add_call_facts(&params, &args, &mut ret_ty);
        }

        return Ok(ret_ty);
    }

    fn validate_arg_types(&mut self, params: &[FnParam], spread_arg_types: &[TypeOrSpread]) {
        slog::info!(self.logger, "[exprs] Validating arguments");

        let rest_idx = {
            let mut rest_idx = None;
            let mut shift = 0;

            for (idx, param) in params.iter().enumerate() {
                match param.pat {
                    RPat::Rest(..) => {
                        rest_idx = Some(idx - shift);
                    }
                    _ => {
                        if !param.required {
                            shift += 1;
                        }
                    }
                }
            }

            rest_idx
        };

        for (idx, arg) in spread_arg_types.iter().enumerate() {
            if arg.spread.is_some() {
                if let Some(rest_idx) = rest_idx {
                    if idx < rest_idx {
                        self.storage.report(Error::ExpectedAtLeastNArgsButGotMOrMore {
                            span: arg.span(),
                            min: rest_idx - 1,
                        })
                    }
                }
            }
        }

        for pair in params
            .iter()
            .filter(|param| match param.pat {
                RPat::Ident(RBindingIdent {
                    id: RIdent {
                        sym: js_word!("this"), ..
                    },
                    ..
                }) => false,
                _ => true,
            })
            .zip_longest(spread_arg_types)
        {
            match pair {
                EitherOrBoth::Both(param, arg) => {
                    match &param.pat {
                        RPat::Rest(..) => match param.ty.normalize() {
                            Type::Array(arr) => {
                                // We should change type if the parameter is a rest parameter.
                                let res = self.assign(&mut Default::default(), &arr.elem_type, &arg.ty, arg.span());
                                let err = match res {
                                    Ok(()) => continue,
                                    Err(err) => err,
                                };

                                let err = err.convert(|err| Error::WrongArgType {
                                    span: arg.span(),
                                    inner: box err,
                                });
                                self.storage.report(err);
                                continue;
                            }
                            _ => {
                                if let Ok(()) = self.assign_with_opts(
                                    &mut Default::default(),
                                    AssignOpts {
                                        span: arg.span(),
                                        allow_iterable_on_rhs: true,
                                        ..Default::default()
                                    },
                                    &param.ty,
                                    &arg.ty,
                                ) {
                                    continue;
                                }
                            }
                        },
                        _ => {}
                    }

                    if arg.spread.is_some() {
                        match arg.ty.normalize() {
                            Type::Array(arg) => {
                                // We should change type if the parameter is a rest parameter.
                                if let Ok(()) =
                                    self.assign(&mut Default::default(), &param.ty, &arg.elem_type, arg.span())
                                {
                                    continue;
                                }
                            }
                            _ => {}
                        }
                    } else {
                        let mut allow_unknown_rhs = match arg.ty.normalize() {
                            Type::TypeLit(..) => false,
                            _ => true,
                        };
                        if let Err(err) = self.assign_with_opts(
                            &mut Default::default(),
                            AssignOpts {
                                span: arg.span(),
                                allow_unknown_rhs,
                                ..Default::default()
                            },
                            &param.ty,
                            &arg.ty,
                        ) {
                            let err = err.convert(|err| {
                                match err {
                                    Error::TupleAssignError { span, errors } => return Error::Errors { span, errors },
                                    Error::ObjectAssignFailed { span, errors } => {
                                        return Error::Errors { span, errors }
                                    }
                                    Error::Errors { span, ref errors } => {
                                        if errors.iter().all(|err| match err.actual() {
                                            Error::UnknownPropertyInObjectLiteralAssignment { span } => true,
                                            _ => false,
                                        }) {
                                            return Error::Errors {
                                                span,
                                                errors: errors
                                                    .iter()
                                                    .map(|err| Error::WrongArgType {
                                                        span: err.span(),
                                                        inner: box err.clone(),
                                                    })
                                                    .collect(),
                                            };
                                        }
                                    }
                                    _ => {}
                                }

                                Error::WrongArgType {
                                    span: arg.span(),
                                    inner: box err,
                                }
                            });
                            self.storage.report(err);
                        }
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
                                RPat::Ident(i) if i.id.sym == arg_id.sym => {
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

    fn narrow_with_predicate(&mut self, span: Span, orig_ty: &Type, new_ty: Type) -> ValidationResult {
        match new_ty.normalize() {
            Type::Ref(..) => {
                let new_ty = self
                    .expand_top_ref(span, Cow::Owned(new_ty))
                    .context("tried to expand ref type in new_ty to narrow type with predicate")?
                    .into_owned();
                return self.narrow_with_predicate(span, orig_ty, new_ty);
            }

            Type::Keyword(..) | Type::Lit(..) => {}
            _ => {
                match orig_ty.normalize() {
                    Type::Union(..) | Type::Interface(..) => {}
                    Type::Ref(..) => {
                        let orig_ty = self.expand_top_ref(span, Cow::Borrowed(orig_ty))?;
                        return self.narrow_with_predicate(span, &orig_ty, new_ty);
                    }
                    _ => {
                        if let Some(v) = self.extends(span, Default::default(), orig_ty, &new_ty) {
                            if v {
                                match orig_ty.normalize() {
                                    Type::ClassDef(def) => {
                                        return Ok(Type::Class(Class {
                                            span,
                                            def: box def.clone(),
                                        }))
                                    }
                                    _ => {}
                                }
                                return Ok(orig_ty.clone());
                            }
                        }

                        return Ok(new_ty);
                    }
                }

                let mut new_types = vec![];

                let mut upcasted = false;
                for ty in orig_ty.iter_union().flat_map(|ty| ty.iter_union()) {
                    match self.extends(span, Default::default(), &new_ty, &ty) {
                        Some(true) => {
                            upcasted = true;
                            new_types.push(ty.clone());
                        }
                        _ => {}
                    }
                }

                // TODO: Use super class instread of
                if !upcasted {
                    new_types.push(new_ty.clone());
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
                return Ok(new_ty);
            }
        }

        match new_ty.normalize() {
            Type::ClassDef(def) => {
                return Ok(Type::Class(Class {
                    span,
                    def: box def.clone(),
                }))
            }
            _ => {}
        }

        Ok(new_ty)
    }

    #[extra_validator]
    fn store_call_fact_for_var(&mut self, span: Span, var_name: Id, new_ty: &Type) {
        match new_ty.normalize() {
            Type::Keyword(..) | Type::Lit(..) => {}
            _ => {
                if let Some(previous_types) = self
                    .find_var_type(&var_name.clone().into(), TypeOfMode::RValue)
                    .map(Cow::into_owned)
                {
                    let new_ty = self.narrow_with_predicate(span, &previous_types, new_ty.clone())?;

                    self.add_type_fact(&var_name.into(), new_ty);
                    return;
                }
            }
        }

        self.add_type_fact(&var_name.into(), new_ty.clone());
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
                    return Err(Error::TypeParameterCountMismatch {
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

    fn is_subtype_in_fn_call(&mut self, span: Span, arg: &Type, param: &Type) -> bool {
        if arg.type_eq(param) {
            return true;
        }

        if arg.is_any() {
            return false;
        }

        if param.is_any() {
            return true;
        }

        self.assign(&mut Default::default(), &arg, &param, span).is_ok()
    }

    /// This method return [Err] if call is invalid
    ///
    ///
    /// # Implementation notes
    ///
    /// `anyAssignabilityInInheritance.ts` says `any, not a subtype of number so
    /// it skips that overload, is a subtype of itself so it picks second (if
    /// truly ambiguous it would pick first overload)`
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
            return ArgCheckResult::WrongArgCount;
        }

        if self
            .validate_arg_count(span, params, args, arg_types, spread_arg_types)
            .is_err()
        {
            return ArgCheckResult::WrongArgCount;
        }

        self.with_scope_for_type_params(|analyzer: &mut Analyzer| {
            if let Some(type_params) = type_params {
                for param in type_params {
                    analyzer.register_type(param.name.clone(), Type::Param(param.clone()));
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
                                &mut Default::default(),
                                AssignOpts {
                                    span,
                                    allow_unknown_rhs: true,
                                    allow_assignment_to_param: true,
                                    ..Default::default()
                                },
                                &param.ty,
                                &arg.ty,
                            )
                            .is_err()
                        {
                            return ArgCheckResult::ArgTypeMismatch;
                        }

                        if !analyzer.is_subtype_in_fn_call(span, &arg.ty, &param.ty) {
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
                            ty: box Type::any(arg.expr.span()),
                        })
                })
                .collect();

            Ok(args)
        })
    }
}

/// Used for reevaluation.
#[derive(Clone, Copy)]
pub(crate) enum ReevalMode<'a> {
    Call(&'a RCallExpr),
    New(&'a RNewExpr),
    NoReeval,
}

impl Default for ReevalMode<'_> {
    fn default() -> Self {
        Self::NoReeval
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

        ty.generalize_lit()
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
                let mut types: Vec<Type> = vec![];

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
                    let obj = a.expand_fully(*span, *obj_ty.clone(), true).report(&mut a.storage);
                    if let Some(obj) = &obj {
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
                            if types.iter().all(|prev_ty| !(*prev_ty).type_eq(&actual_ty)) {
                                types.push(actual_ty);
                            }
                        }
                    }
                }

                *ty = Type::union(types);
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
                && type_args.params.iter().any(|ty| match ty.normalize() {
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

                                match &type_args.params[0] {
                                    Type::Union(type_arg) => {
                                        for ty in &type_arg.types {
                                            types.push(Type::Ref(Ref {
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

                                *ty = Type::union(types);
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

fn is_fn_expr(callee: &RExpr) -> bool {
    match callee {
        RExpr::Arrow(..) | RExpr::Fn(..) => true,
        RExpr::Paren(e) => is_fn_expr(&e.expr),
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
enum ArgCheckResult {
    Exact,
    MayBe,
    ArgTypeMismatch,
    WrongArgCount,
}

/// Ensure that sort work as expected.
#[test]
fn test_arg_check_result_order() {
    let mut v = vec![
        ArgCheckResult::Exact,
        ArgCheckResult::MayBe,
        ArgCheckResult::ArgTypeMismatch,
        ArgCheckResult::WrongArgCount,
    ];
    let expected = v.clone();
    v.sort();

    assert_eq!(v, expected);
}

/// TODO: Use cow
struct CallCandidate {
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FnParam>,
    pub ret_ty: Type,
}
