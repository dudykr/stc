//! Handles new expressions and call expressions.
use std::borrow::Cow;

use fxhash::FxHashMap;
use itertools::Itertools;
use rnode::{Fold, FoldWith, NodeId, VisitMut, VisitMutWith, VisitWith};
use stc_ts_ast_rnode::{
    RArrayPat, RBindingIdent, RCallExpr, RCallee, RComputedPropName, RExpr, RExprOrSpread, RIdent, RInvalid, RLit, RMemberExpr,
    RMemberProp, RNewExpr, RObjectPat, RPat, RRestPat, RStr, RTaggedTpl, RTsAsExpr, RTsEntityName, RTsKeywordType, RTsLit,
    RTsThisTypeOrIdent, RTsType, RTsTypeAnn, RTsTypeParamInstantiation, RTsTypeRef, RTsUnionOrIntersectionType, RTsUnionType,
};
use stc_ts_env::MarkExt;
use stc_ts_errors::{
    debug::{dump_type_as_string, dump_type_map, force_dump_type_as_string, print_type},
    DebugExt, ErrorKind,
};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_generics::type_param::finder::TypeParamUsageFinder;
use stc_ts_storage::ErrorStore;
use stc_ts_type_ops::{generalization::prevent_generalize, is_str_lit_or_union, Fix};
use stc_ts_types::{
    type_id::SymbolId, Alias, Array, Class, ClassDef, ClassMember, ClassProperty, CommonTypeMetadata, Id, IdCtx, IndexedAccessType,
    Instance, Interface, Intersection, Key, KeywordType, KeywordTypeMetadata, LitType, QueryExpr, QueryType, Ref, StaticThis, Symbol,
    TypeParamDecl, Union, UnionMetadata,
};
use stc_ts_utils::PatExt;
use stc_utils::{cache::Freeze, dev_span, ext::TypeVecExt};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::{Accessibility, TsKeywordTypeKind};
use tracing::{debug, info, warn};
use ty::TypeExt;

use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::TypeOfMode,
        generic::{ExtendsOpts, InferTypeOpts},
        types::NormalizeTypeOpts,
        util::{make_instance_type, ResultExt},
        Analyzer, Ctx, ScopeKind,
    },
    ty,
    ty::{
        CallSignature, ConstructorSignature, FnParam, Method, MethodSignature, Type, TypeElement, TypeOrSpread, TypeParam,
        TypeParamInstantiation,
    },
    validator,
    validator::ValidateWith,
    VResult,
};

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct CallOpts {
    pub disallow_invoking_implicit_constructors: bool,

    /// Optional properties cannot be called.
    ///
    /// See: for-of29.ts
    pub disallow_optional_object_property: bool,

    /// If false, private members are not allowed.
    pub allow_private_names: bool,

    /// Used to prevent infinite recursion.
    pub do_not_check_object: bool,

    pub do_not_use_any_for_computed_key: bool,
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExprOrSpread) -> VResult<TypeOrSpread> {
        let span = node.span();
        Ok(TypeOrSpread {
            span,
            spread: node.spread,
            ty: Box::new(node.expr.validate_with_default(self)?),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RCallExpr, type_ann: Option<&Type>) -> VResult<Type> {
        let RCallExpr {
            span,
            ref callee,
            ref args,
            ref type_args,
            ..
        } = *e;

        let mut type_ann = self.expand_type_ann(span, type_ann)?;
        type_ann.freeze();

        let callee = match callee {
            RCallee::Super(..) => {
                self.report_error_for_super_refs_without_supers(span, true);
                self.report_error_for_super_reference_in_compute_keys(span, true);

                if type_args.is_some() {
                    // super<T>() is invalid.
                    self.storage.report(ErrorKind::SuperCannotUseTypeArgs { span }.into())
                }

                self.validate_args(args).report(&mut self.storage);

                self.scope.mark_as_super_called();

                return Ok(Type::any(span, Default::default()));
            }
            RCallee::Expr(callee) => callee,
            RCallee::Import(callee) => {
                let base = self.storage.path(self.ctx.module_id);

                let src = args.iter().next();
                let src = match src {
                    Some(src) => src.expr.validate_with_default(self)?,
                    None => {
                        return Err(ErrorKind::Unimplemented {
                            span,
                            msg: "validation of dynamic import without an arg".to_string(),
                        }
                        .into())
                    }
                };
                let src = match src.normalize() {
                    Type::Lit(LitType { lit: RTsLit::Str(s), .. }) => s.value.clone(),
                    ty if !self.rule().strict_null_checks && ty.is_null_or_undefined() => {
                        return Ok(Type::any(callee.span, Default::default()))
                    }
                    ty if ty.is_any() || ty.is_str_like() => return Ok(Type::any(callee.span, Default::default())),
                    ty if ty.is_union_type() => {
                        let span = ty.span();
                        let types = ty.clone().expect_union_type().types;
                        if types.iter().all(|t| t.is_str_like()) {
                            return Ok(Type::Ref(Ref {
                                span,
                                type_name: RTsEntityName::Ident(RIdent::new("Promise".into(), span.with_ctxt(SyntaxContext::empty()))),
                                type_args: Some(Box::new(TypeParamInstantiation {
                                    span,
                                    params: vec![Type::any(span, Default::default())],
                                })),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }));
                        }
                        return Err(ErrorKind::NonStringDynamicImport { span: callee.span }.into());
                    }
                    _ => return Err(ErrorKind::NonStringDynamicImport { span: callee.span }.into()),
                };

                let dep_id = self.loader.module_id(&base, &src);

                if let Some(dep_id) = dep_id {
                    // We need to update the import map because of code like
                    //
                    // declare function bar(): boolean;
                    // const specify = bar() ? "./0" : undefined;
                    // let myModule = import(specify);

                    if let Some(dep) = self.data.imports.get(&(self.ctx.module_id, dep_id)) {
                        return Ok(Type::Ref(Ref {
                            span,
                            type_name: RTsEntityName::Ident(RIdent::new("Promise".into(), span.with_ctxt(SyntaxContext::empty()))),
                            type_args: Some(Box::new(TypeParamInstantiation {
                                span,
                                params: vec![dep.clone()],
                            })),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                    } else {
                        return self
                            .load_import_lazily(span, &base, dep_id, &src)
                            .context("tried to load a dynamic import lazily");
                    }
                }

                return Err(ErrorKind::Unimplemented {
                    span: e.span,
                    msg: "validation of dynamic import".to_string(),
                }
                .into());
            }
        };

        let is_callee_iife = is_fn_expr(callee);

        // TODO(kdy1): validate children

        self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.ctx.is_calling_iife = is_callee_iife;

            analyzer.extract_call_new_expr_member(
                span,
                ReEvalMode::Call(e),
                callee,
                ExtractKind::Call,
                args,
                type_args.as_deref(),
                type_ann.as_deref(),
            )
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RNewExpr, type_ann: Option<&Type>) -> VResult<Type> {
        let RNewExpr {
            span,
            ref callee,
            ref args,
            ref type_args,
            ..
        } = *e;

        let mut type_ann = self.expand_type_ann(span, type_ann)?;
        type_ann.freeze();

        // TODO(kdy1): e.visit_children

        self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.extract_call_new_expr_member(
                span,
                ReEvalMode::New(e),
                callee,
                ExtractKind::New,
                args.as_ref().map(|v| &**v).unwrap_or_else(|| &mut []),
                type_args.as_deref(),
                type_ann.as_deref(),
            )
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTaggedTpl) -> VResult<Type> {
        let span = e.span;

        let tpl_str_arg = {
            let span = span.with_ctxt(SyntaxContext::empty());
            RExprOrSpread {
                spread: None,
                expr: Box::new(RExpr::TsAs(RTsAsExpr {
                    node_id: NodeId::invalid(),
                    span,
                    expr: Box::new(RExpr::Invalid(RInvalid { span: DUMMY_SP })),
                    type_ann: Box::new(RTsType::TsTypeRef(RTsTypeRef {
                        node_id: NodeId::invalid(),
                        span,
                        type_name: RTsEntityName::Ident(RIdent::new("TemplateStringsArray".into(), span)),
                        type_params: None,
                    })),
                })),
            }
        };
        let mut args = vec![tpl_str_arg];

        args.extend(e.tpl.exprs.iter().cloned().map(|expr| RExprOrSpread { spread: None, expr }));

        self.with_child(ScopeKind::Call, Default::default(), |analyzer: &mut Analyzer| {
            analyzer.extract_call_new_expr_member(
                span,
                ReEvalMode::NoReEval,
                &e.tag,
                ExtractKind::Call,
                args.as_ref(),
                e.type_params.as_deref(),
                Default::default(),
            )
        })
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
        expr: ReEvalMode,
        callee: &RExpr,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        type_args: Option<&RTsTypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let _tracing = dev_span!("extract_call_new_expr_member");

        debug_assert_eq!(self.scope.kind(), ScopeKind::Call);

        let marks = self.marks();

        debug!("extract_call_new_expr_member");

        let type_args = match type_args {
            Some(v) => {
                let mut type_args = v.validate_with(self)?;
                self.prevent_expansion(&mut type_args);
                type_args.freeze();
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

        match *callee {
            RExpr::Ident(ref i) if i.sym == js_word!("require") => {
                let id = args
                    .iter()
                    .cloned()
                    .map(|arg| match arg {
                        RExprOrSpread { spread: None, expr } => match *expr {
                            RExpr::Lit(RLit::Str(RStr { span, value, .. })) => RIdent::new(value, span).into(),
                            _ => unimplemented!("dynamic import: require()"),
                        },
                        _ => unimplemented!("error reporting: spread element in require()"),
                    })
                    .next()
                    .unwrap();
                if let Some(dep) = self.find_imported_var(&id)? {
                    let dep = dep;
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
                if self.env.target().eq(&swc_ecma_ast::EsVersion::Es5) {
                    Err(ErrorKind::NoSuchType {
                        span: i.span(),
                        name: i.into(),
                    })?
                }

                Err(ErrorKind::UndefinedSymbol {
                    sym: i.into(),
                    span: i.span(),
                })?
            }

            _ => {}
        }

        match *callee {
            RExpr::Ident(RIdent {
                sym: js_word!("Symbol"), ..
            }) => {
                if kind == ExtractKind::New {
                    self.storage.report(ErrorKind::CannotCallWithNewNonVoidFunction { span }.into())
                }

                // Symbol uses special type
                if !args.is_empty() {
                    unimplemented!("Error reporting for calling `Symbol` with arguments is not implemented yet")
                }

                return Ok(Type::Symbol(Symbol {
                    span,
                    id: SymbolId::generate(),
                    metadata: Default::default(),
                    tracker: Default::default(),
                }));
            }

            // Use general callee validation.
            RExpr::Member(RMemberExpr {
                prop:
                    RMemberProp::Computed(RComputedPropName {
                        expr: box RExpr::Lit(RLit::Num(..)),
                        ..
                    }),
                ..
            }) => {}

            RExpr::Member(RMemberExpr { ref obj, ref prop, .. }) => {
                let prop = self.validate_key(
                    &match prop {
                        RMemberProp::Ident(i) => RExpr::Ident(i.clone()),
                        RMemberProp::Computed(c) => *c.expr.clone(),
                        RMemberProp::PrivateName(p) => RExpr::PrivateName(p.clone()),
                    },
                    matches!(prop, RMemberProp::Computed(..)),
                )?;

                // Validate object
                let mut obj_type = obj
                    .validate_with_default(self)
                    .unwrap_or_else(|err| {
                        self.storage.report(err);
                        Type::any(span, Default::default())
                    })
                    .generalize_lit();
                {
                    // Handle toString()

                    if prop == js_word!("toString") {
                        return Ok(Type::from(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                    }
                }

                // Handle member expression
                obj_type.freeze();

                let obj_type = match *obj_type.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => self
                        .env
                        .get_global_type(span, &js_word!("Number"))
                        .expect("Builtin type named 'Number' should exist"),
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) => self
                        .env
                        .get_global_type(span, &js_word!("String"))
                        .expect("Builtin type named 'String' should exist"),
                    _ => obj_type,
                };

                let mut arg_types = self.validate_args(args)?;
                arg_types.freeze();

                let spread_arg_types = self.spread_args(&arg_types).context("tried to handle spreads in arguments")?;

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
                        Default::default(),
                    )
                    .map(|ty| ty.fixed());
            }
            _ => {}
        }

        self.with(|analyzer: &mut Analyzer| {
            let ret_ty = match callee {
                RExpr::Ident(i) if kind == ExtractKind::New => {
                    let mut ty = Type::Ref(Ref {
                        span: i.span,
                        type_name: RTsEntityName::Ident(i.clone()),
                        type_args: Default::default(),
                        metadata: Default::default(),
                        tracker: Default::default(),
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
                let callee_ty = callee
                    .validate_with_args(analyzer, (TypeOfMode::RValue, type_args.as_ref(), None))
                    .unwrap_or_else(|err| {
                        analyzer.storage.report(err);
                        Type::any(
                            span,
                            KeywordTypeMetadata {
                                common: CommonTypeMetadata {
                                    implicit: true,
                                    ..Default::default()
                                },
                                ..Default::default()
                            },
                        )
                    });
                match callee_ty.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) if type_args.is_some() => {
                        // If it's implicit any, we should postpone this check.
                        if !analyzer.is_implicitly_typed(&callee_ty) {
                            analyzer.storage.report(ErrorKind::AnyTypeUsedAsCalleeWithTypeArgs { span }.into())
                        }
                    }
                    _ => {}
                }

                match callee_ty.normalize() {
                    Type::Union(u) => {
                        let types = u
                            .types
                            .iter()
                            .filter(|callee| !matches!(callee.normalize(), Type::Module(..) | Type::Namespace(..)))
                            .cloned()
                            .collect::<Vec<_>>();

                        match types.len() {
                            0 => Type::never(
                                u.span,
                                KeywordTypeMetadata {
                                    common: u.metadata.common,
                                    ..Default::default()
                                },
                            ),
                            1 => types.into_iter().next().unwrap(),
                            _ => Type::Union(Union { types, ..*u }),
                        }
                    }
                    _ => callee_ty,
                }
            };

            if let Some(type_args) = &type_args {
                let type_params = match callee_ty.normalize() {
                    Type::Function(f) => f.type_params.as_ref(),
                    _ => None,
                };
                if let Some(type_param_decl) = type_params {
                    let mut params = FxHashMap::default();

                    for (type_param, ty) in type_param_decl.params.iter().zip(type_args.params.iter()) {
                        params.insert(type_param.name.clone(), ty.clone().freezed());
                    }

                    callee_ty = analyzer.expand_type_params(&params, callee_ty, Default::default())?;
                }
            }

            callee_ty.freeze();

            analyzer.apply_type_ann_from_callee(span, kind, args, &callee_ty, type_args.as_ref(), type_ann)?;
            let mut arg_types = analyzer.validate_args(args)?;
            arg_types.freeze();

            let spread_arg_types = analyzer.spread_args(&arg_types).context("tried to handle spreads in arguments")?;

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
                Default::default(),
            )?;

            Ok(expanded_ty.fixed())
        })
    }

    /// TODO(kdy1): Use Cow for `obj_type`
    ///
    /// ## Parameters
    ///
    ///  - `expr`: Can be default if argument does not include an arrow
    ///    expression nor a function expression.
    pub(super) fn call_property(
        &mut self,
        span: Span,
        kind: ExtractKind,
        expr: ReEvalMode,
        this: &Type,
        obj_type: &Type,
        prop: &Key,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
        opts: CallOpts,
    ) -> VResult<Type> {
        let _tracing = dev_span!("call_property");

        obj_type.assert_valid();

        let span = span.with_ctxt(SyntaxContext::empty());

        let old_this = self.scope.this.take();
        self.scope.this = Some(this.clone());

        let res = (|| {
            let obj_type = self
                .normalize(
                    Some(span),
                    Cow::Borrowed(obj_type),
                    NormalizeTypeOpts {
                        preserve_intersection: true,
                        preserve_global_this: true,
                        ..Default::default()
                    },
                )
                .context("failed to normalize for call_property")?
                .freezed()
                .into_owned();

            match obj_type.normalize() {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                }) => {
                    return Ok(Type::any(span, Default::default()));
                }

                Type::Array(obj) => {
                    if let Key::Computed(key) = prop {
                        if let Type::Symbol(key_ty) = key.ty.normalize() {
                            if key_ty.id == SymbolId::iterator() {
                                return Ok(obj_type);
                            }
                        }
                    }

                    let obj = Type::Ref(Ref {
                        span,
                        type_name: RTsEntityName::Ident(RIdent::new(
                            "Array".into(),
                            span.with_ctxt(self.marks().unresolved_mark().as_ctxt()),
                        )),
                        type_args: Some(Box::new(TypeParamInstantiation {
                            span,
                            params: vec![*obj.elem_type.clone()],
                        })),
                        metadata: Default::default(),
                        tracker: Default::default(),
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
                        opts,
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
                                CallOpts {
                                    do_not_use_any_for_computed_key: true,
                                    ..opts
                                },
                            )
                        })
                        .filter_map(Result::ok)
                        .collect_vec();

                    if types.is_empty() {
                        if kind == ExtractKind::Call {
                            return Err(ErrorKind::NoCallablePropertyWithName {
                                span,
                                obj: Box::new(obj_type.clone()),
                                key: Box::new(prop.clone()),
                            }
                            .into());
                        } else {
                            return Err(ErrorKind::NoSuchConstructor {
                                span,
                                key: Box::new(prop.clone()),
                            }
                            .into());
                        }
                    }

                    return Ok(Type::new_union(span, types));
                }

                Type::Interface(ref i) => {
                    // We check for body before parent to support overriding
                    let err = match self.call_property_of_type_elements(
                        kind,
                        expr,
                        span,
                        &obj_type,
                        &i.body,
                        prop,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                        opts,
                    ) {
                        Ok(v) => return Ok(v),
                        Err(err) => err,
                    };

                    // Check parent interface
                    for parent in &i.extends {
                        let parent = self
                            .type_of_ts_entity_name(span, &parent.expr, parent.type_args.as_deref())
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
                            opts,
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
                        prop,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                        opts,
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
                        opts,
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
                        opts,
                    )? {
                        return Ok(v);
                    }
                }

                Type::Keyword(KeywordType {
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
                    if !opts.do_not_check_object {
                        let obj_res = self.call_property(
                            span,
                            kind,
                            expr,
                            this,
                            &Type::Ref(Ref {
                                span: DUMMY_SP,
                                type_name: RTsEntityName::Ident(RIdent::new(
                                    js_word!("Object"),
                                    DUMMY_SP.with_ctxt(self.marks().unresolved_mark().as_ctxt()),
                                )),
                                type_args: None,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                            prop,
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                            type_ann,
                            CallOpts {
                                do_not_check_object: true,
                                ..opts
                            },
                        );
                        if let Ok(v) = obj_res {
                            return Ok(v);
                        }
                    }
                }
            }

            // Use proper error.
            if let Type::Class(..) = obj_type.normalize() {
                return Err(match kind {
                    ExtractKind::Call => ErrorKind::NoCallablePropertyWithName {
                        span,
                        obj: Box::new(obj_type.clone()),
                        key: Box::new(prop.clone()),
                    }
                    .into(),
                    ExtractKind::New => ErrorKind::NoSuchConstructor {
                        span,
                        key: Box::new(prop.clone()),
                    }
                    .into(),
                });
            }

            let ctx = Ctx {
                disallow_unknown_object_property: true,
                ..self.ctx
            };

            let callee = self
                .with_ctx(ctx)
                .access_property(span, &obj_type, prop, TypeOfMode::RValue, IdCtx::Var, Default::default())
                .context("tried to access property to call it")?;

            let callee_before_expanding = force_dump_type_as_string(&callee);
            let callee = self
                .normalize(Some(span), Cow::Owned(callee), NormalizeTypeOpts { ..Default::default() })?
                .into_owned();

            if let Type::ClassDef(cls) = callee.normalize() {
                if cls.is_abstract {
                    self.storage.report(ErrorKind::CannotCreateInstanceOfAbstractClass { span }.into())
                }
            }
            let callee_str = force_dump_type_as_string(&callee);

            self.get_best_return_type(
                span,
                expr,
                callee,
                kind,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
                SelectOpts {
                    skip_check_for_overloads: true,
                    ..Default::default()
                },
            )
            .or_else(|err| {
                if obj_type.is_type_param() {
                    if prop.is_computed() {
                        return Ok(Type::any(span, Default::default()));
                    }
                }

                Err(err)
            })
            .convert_err(|err| {
                if obj_type.is_type_param() {
                    return ErrorKind::NoSuchProperty {
                        span,
                        obj: Some(Box::new(obj_type.clone())),
                        prop: Some(Box::new(prop.clone())),
                    };
                }

                match err {
                    ErrorKind::NoCallSignature { span, .. } => ErrorKind::NoCallablePropertyWithName {
                        span,
                        obj: Box::new(obj_type.clone()),
                        key: Box::new(prop.clone()),
                    },
                    ErrorKind::NoNewSignature { span, .. } => ErrorKind::NoConstructablePropertyWithName {
                        span,
                        obj: Box::new(obj_type.clone()),
                        key: Box::new(prop.clone()),
                    },
                    _ => err,
                }
            })
            .with_context(|| {
                format!(
                    "tried to call property by using access_property because the object type is not handled by call_property: \nobj = \
                     {}\ncallee = {}\ncallee (before expanding): {}",
                    force_dump_type_as_string(&obj_type),
                    callee_str,
                    callee_before_expanding,
                )
            })
        })()
        .with_context(|| format!("tried to call a property of an object ({})", force_dump_type_as_string(obj_type)));
        self.scope.this = old_this;
        res
    }

    #[allow(unused)]
    fn extract_callable_properties_of_class(
        &mut self,
        span: Span,
        kind: ExtractKind,
        c: &ClassDef,
        prop: &Key,
        is_static_call: bool,
    ) -> VResult<Vec<CallCandidate>> {
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
                            type_params: type_params.clone(),
                            params: params.clone(),
                            ret_ty: ret_ty.clone(),
                        });
                    }
                }
                ty::ClassMember::Property(ClassProperty { key, value, is_static, .. }) if *is_static == is_static_call => {
                    if self.key_matches(span, key, prop, false) {
                        // Check for properties with callable type.

                        // TODO(kdy1): Change error message from no callable
                        // property to property exists but not callable.

                        if let Some(prop_ty) = value.as_deref().map(Type::normalize) {
                            if let Ok(cs) = self.extract_callee_candidates(span, kind, prop_ty) {
                                candidates.extend(cs);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(candidates)
    }

    fn call_property_of_class(
        &mut self,
        span: Span,
        expr: ReEvalMode,
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
        opts: CallOpts,
    ) -> VResult<Option<Type>> {
        let _tracing = dev_span!("call_property_of_class");

        let candidates = {
            // TODO(kdy1): Deduplicate.
            // This is duplicated intentionally because of regressions.

            let mut candidates: Vec<CallCandidate> = vec![];
            for member in c.body.iter() {
                match member {
                    ClassMember::Method(Method {
                        key,
                        ret_ty,
                        type_params,
                        params,
                        is_static,
                        ..
                    }) if *is_static == is_static_call => {
                        if self.key_matches(span, key, prop, false) {
                            candidates.push(CallCandidate {
                                type_params: type_params.clone(),
                                params: params.clone(),
                                ret_ty: ret_ty.clone(),
                            });
                        }
                    }
                    ClassMember::Property(ClassProperty { key, value, is_static, .. }) if *is_static == is_static_call => {
                        if self.key_matches(span, key, prop, false) {
                            // Check for properties with callable type.

                            // TODO(kdy1): Change error message from no callable
                            // property to property exists but not callable.

                            if let Some(ty) = value.as_deref() {
                                return self
                                    .extract(span, expr, ty, kind, args, arg_types, spread_arg_types, type_args, type_ann, opts)
                                    .map(Some);
                            }
                        }
                    }
                    _ => {}
                }
            }

            candidates
        };

        if let Some(v) = self
            .select_and_invoke(
                span,
                kind,
                expr,
                &candidates,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
                SelectOpts { ..Default::default() },
            )
            .context("tried to select a callable property of a class")?
        {
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
                opts,
            ) {
                return Ok(Some(ret_ty));
            }
        }

        Ok(None)
    }

    fn check_type_element_for_call(
        &mut self,
        span: Span,
        kind: ExtractKind,
        candidates: &mut Vec<CallCandidate>,
        m: &TypeElement,
        prop: &Key,
        opts: CallOpts,
    ) {
        let span = span.with_ctxt(SyntaxContext::empty());

        match m {
            TypeElement::Method(m) if kind == ExtractKind::Call => {
                if opts.disallow_optional_object_property && m.optional {
                    return;
                }

                if !opts.allow_private_names {
                    if m.key.is_private() || prop.is_private() {
                        return;
                    }
                }

                // We are interested only on methods named `prop`
                if let Ok(()) = self.assign(span, &mut Default::default(), &m.key.ty(), &prop.ty()) {
                    candidates.push(CallCandidate {
                        type_params: m.type_params.clone(),
                        params: m.params.clone(),
                        ret_ty: m.ret_ty.clone().unwrap_or_else(|| Box::new(Type::any(m.span, Default::default()))),
                    });
                }
            }

            TypeElement::Property(p) => {
                if opts.disallow_optional_object_property && p.optional {
                    // See: for-of29.ts
                    // Optional properties cannot be called.
                    return;
                }

                if self.key_matches(span, &p.key, prop, false) {
                    // TODO(kdy1): Remove useless clone
                    let ty = *p.type_ann.clone().unwrap_or(Box::new(Type::any(m.span(), Default::default())));
                    let mut ty = self
                        .normalize(Some(span), Cow::Borrowed(&ty), Default::default())
                        .map(Cow::into_owned)
                        .unwrap_or_else(|_| ty);
                    ty.normalize_mut();

                    // TODO(kdy1): PERF

                    match ty {
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => {
                            let rest = FnParam {
                                span,
                                required: false,
                                pat: RPat::Rest(RRestPat {
                                    node_id: NodeId::invalid(),
                                    span,
                                    dot3_token: DUMMY_SP,
                                    arg: Box::new(RPat::Ident(RBindingIdent {
                                        node_id: NodeId::invalid(),
                                        id: RIdent::new("args".into(), span.with_ctxt(SyntaxContext::empty())),
                                        type_ann: Default::default(),
                                    })),
                                    type_ann: Default::default(),
                                }),
                                ty: Box::new(Type::any(span, Default::default())),
                            };
                            candidates.push(CallCandidate {
                                params: vec![rest],
                                ret_ty: Box::new(Type::any(span, Default::default())),
                                type_params: Default::default(),
                            });
                        }

                        Type::Function(f) if kind == ExtractKind::Call => {
                            candidates.push(CallCandidate {
                                params: f.params,
                                ret_ty: f.ret_ty,
                                type_params: f.type_params,
                            });
                        }

                        _ => {
                            if let Ok(cs) = self.extract_callee_candidates(span, kind, &ty) {
                                candidates.extend(cs);
                            }
                        }
                    }
                }
            }

            _ => {}
        }
    }

    fn call_property_of_type_elements(
        &mut self,
        kind: ExtractKind,
        expr: ReEvalMode,
        span: Span,
        obj: &Type,
        members: &[TypeElement],
        prop: &Key,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
        opts: CallOpts,
    ) -> VResult<Type> {
        let _tracing = dev_span!("call_property_of_type_elements");

        let span = span.with_ctxt(SyntaxContext::empty());

        // Candidates of the method call.
        //
        // 4 is just an unscientific guess
        // TODO(kdy1): Use smallvec
        let mut candidates = Vec::with_capacity(4);

        for m in members {
            self.check_type_element_for_call(span, kind, &mut candidates, m, prop, opts);
        }

        // TODO(kdy1): Move this to caller to prevent checking members of `Object` every
        // time we check parent interface.
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

            // TODO(kdy1): Remove clone
            for m in methods {
                self.check_type_element_for_call(span, kind, &mut candidates, m, prop, opts);
            }
        }

        if let Some(v) = self
            .select_and_invoke(
                span,
                kind,
                expr,
                &candidates,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
                SelectOpts { ..Default::default() },
            )
            .context("tried to select a callable property of type elements")?
        {
            return Ok(v);
        }

        if !opts.do_not_use_any_for_computed_key && prop.is_computed() {
            return Ok(Type::any(span, Default::default()));
        }

        Err(ErrorKind::NoSuchProperty {
            span,
            obj: Some(Box::new(obj.clone())),
            prop: Some(Box::new(prop.clone())),
        }
        .context("failed to call property of type elements"))
    }

    /// Returns `()`
    fn spread_args<'a>(&mut self, arg_types: &'a [TypeOrSpread]) -> VResult<Cow<'a, [TypeOrSpread]>> {
        let mut new_arg_types;

        if arg_types.iter().any(|arg| arg.spread.is_some()) {
            new_arg_types = vec![];
            for arg in arg_types {
                if arg.spread.is_some() {
                    let arg_ty = self
                        .normalize(
                            Some(arg.span()),
                            Cow::Borrowed(&arg.ty),
                            NormalizeTypeOpts {
                                preserve_global_this: true,
                                ..Default::default()
                            },
                        )
                        .context("tried to expand ref to handle a spread argument")?;

                    match arg_ty.normalize() {
                        Type::Tuple(arg_ty) => {
                            new_arg_types.extend(arg_ty.elems.iter().map(|element| &element.ty).cloned().map(|ty| TypeOrSpread {
                                span: arg.spread.unwrap(),
                                spread: None,
                                ty,
                            }));
                        }

                        Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => {
                            self.scope.is_call_arg_count_unknown = true;
                            new_arg_types.push(TypeOrSpread {
                                span: *span,
                                spread: None,
                                ty: Box::new(arg_ty.clone().into_owned()),
                            });
                        }

                        Type::Array(arr) => {
                            self.scope.is_call_arg_count_unknown = true;
                            new_arg_types.push(arg.clone());
                        }

                        Type::Param(..) => {
                            new_arg_types.push(arg.clone());
                        }

                        _ => {
                            self.scope.is_call_arg_count_unknown = true;

                            let elem_type = self
                                .get_iterator_element_type(arg.span(), arg_ty, false, Default::default())
                                .context("tried to get element type of an iterator for spread syntax in arguments")?;

                            new_arg_types.push(TypeOrSpread {
                                span: arg.span(),
                                spread: arg.spread,
                                ty: Box::new(elem_type.into_owned()),
                            });
                        }
                    }
                } else {
                    new_arg_types.push(arg.clone());
                }
            }

            new_arg_types.freeze();

            return Ok(Cow::Owned(new_arg_types));
        } else {
            return Ok(Cow::Borrowed(arg_types));
        }
    }

    fn extract(
        &mut self,
        span: Span,
        expr: ReEvalMode,
        ty: &Type,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
        opts: CallOpts,
    ) -> VResult<Type> {
        if !self.config.is_builtin {
            ty.assert_valid();
        }

        let span = span.with_ctxt(SyntaxContext::empty());
        match ty.normalize() {
            Type::Ref(..) | Type::Query(..) | Type::Instance(..) => {
                let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default())?;
                return self.extract(span, expr, &ty, kind, args, arg_types, spread_arg_types, type_args, type_ann, opts);
            }

            _ => {}
        }

        debug!("[exprs/call] Calling {}", dump_type_as_string(ty));

        if let ExtractKind::Call = kind {
            match ty.normalize() {
                Type::Interface(i) if i.name == "Function" => {
                    if i.type_params.is_none() && type_args.is_some() {
                        self.storage
                            .report(ErrorKind::TypeParamsProvidedButCalleeIsNotGeneric { span }.into());
                    }

                    return Ok(Type::any(span, Default::default()));
                }
                _ => {}
            }
        }

        if let ExtractKind::New = kind {
            match ty.normalize() {
                Type::ClassDef(ref cls) => {
                    self.scope.this = Some(Type::Class(Class {
                        span,
                        def: cls.clone(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));

                    if cls.is_abstract {
                        if opts.disallow_invoking_implicit_constructors {
                            return Err(ErrorKind::NoNewSignature {
                                span,
                                callee: Box::new(ty.clone()),
                            }
                            .into());
                        }

                        self.storage.report(ErrorKind::CannotCreateInstanceOfAbstractClass { span }.into());
                        // The test classAbstractInstantiation1.ts says
                        //
                        //  new A(1); // should report 1 error
                        //
                        return Ok(Type::Class(Class {
                            span,
                            def: cls.clone(),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                    }

                    if let Some(type_params) = &cls.type_params {
                        for (i, param) in type_params.params.iter().enumerate() {
                            if let Some(constraint) = &param.constraint {
                                if let Some(type_args) = type_args {
                                    if let Some(type_arg) = type_args.params.get(i) {
                                        if let Err(err) = self.assign_with_opts(
                                            &mut Default::default(),
                                            constraint,
                                            type_arg,
                                            AssignOpts {
                                                span,
                                                allow_assignment_to_param_constraint: true,
                                                ..Default::default()
                                            },
                                        ) {
                                            return Err(ErrorKind::NotSatisfyConstraint {
                                                span,
                                                left: constraint.clone(),
                                                right: Box::new(type_arg.clone()),
                                            }
                                            .into());
                                        }
                                    }
                                }
                            };
                            self.register_type(param.name.clone(), Type::Param(param.clone()));
                        }
                    }

                    // Infer type arguments using constructors.
                    let mut constructors = cls
                        .body
                        .iter()
                        .filter_map(|member| match member {
                            ClassMember::Constructor(c) => Some(c),
                            _ => None,
                        })
                        .collect_vec();

                    constructors.sort_by_cached_key(|c| {
                        self.check_call_args(
                            span,
                            c.type_params.as_ref().map(|v| &*v.params),
                            &c.params,
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                        )
                    });

                    if let Some(constructor) = constructors.first() {
                        if matches!(constructor.accessibility, Some(Accessibility::Private))
                            || (matches!(constructor.accessibility, Some(Accessibility::Protected)) && !self.ctx.in_class_with_super)
                        {
                            let err = match constructor.accessibility {
                                Some(Accessibility::Private) => ErrorKind::ClassConstructorPrivate { span },
                                Some(Accessibility::Protected) => ErrorKind::ClassConstructorProtected { span },
                                _ => unreachable!(),
                            };
                            self.storage.report(err.into());

                            return Ok(Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                                span,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }));
                        };
                        let type_params = constructor.type_params.as_ref().or(cls.type_params.as_deref()).map(|v| &*v.params);
                        // TODO(kdy1): Constructor's return type.

                        return self
                            .get_return_type(
                                span,
                                kind,
                                expr,
                                type_params,
                                &constructor.params,
                                Type::Class(Class {
                                    span,
                                    def: cls.clone(),
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                                type_args,
                                args,
                                arg_types,
                                spread_arg_types,
                                type_ann,
                            )
                            .context("tried to instantiate a class using constructor");
                    }

                    // Check for constructors declared in the super class.
                    if let Some(super_class) = &cls.super_class {
                        //

                        if let Ok(v) = self.extract(
                            span,
                            expr,
                            super_class,
                            kind,
                            args,
                            arg_types,
                            spread_arg_types,
                            type_args,
                            type_ann,
                            CallOpts {
                                disallow_invoking_implicit_constructors: true,
                                ..opts
                            },
                        ) {
                            return Ok(v);
                        }
                    }

                    if opts.disallow_invoking_implicit_constructors {
                        return Err(ErrorKind::NoNewSignature {
                            span,
                            callee: Box::new(ty.clone()),
                        }
                        .into());
                    }

                    let ctx = Ctx {
                        is_instantiating_class: true,
                        ..self.ctx
                    };
                    return self
                        .with_ctx(ctx)
                        .get_return_type(
                            span,
                            kind,
                            expr,
                            cls.type_params.as_ref().map(|v| &*v.params),
                            &[],
                            Type::Class(Class {
                                span,
                                def: cls.clone(),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                            type_args,
                            args,
                            arg_types,
                            spread_arg_types,
                            type_ann,
                        )
                        .context("tried to instantiate a class without any constructor with call");
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

                Type::StaticThis(..) => {
                    if let Some(class_name) = self.scope.this_class_name() {
                        return Ok(Type::Instance(Instance {
                            span,
                            ty: Box::new(Type::Query(QueryType {
                                span,
                                expr: Box::new(QueryExpr::TsEntityName(RTsEntityName::Ident(class_name.into()))),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                    }
                    return Ok(Type::Instance(Instance {
                        span,
                        ty: Box::new(Type::StaticThis(StaticThis {
                            span,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }

                Type::Function(..) if self.rule().no_implicit_any => {
                    return Err(ErrorKind::TargetLacksConstructSignature { span }.into());
                }

                _ => {}
            }
        }

        macro_rules! ret_err {
            () => {{
                dbg!();
                match kind {
                    ExtractKind::Call => {
                        return Err(ErrorKind::NoCallSignature {
                            span,
                            callee: Box::new(ty.clone()),
                        }
                        .into())
                    }
                    ExtractKind::New => {
                        return Err(ErrorKind::NoNewSignature {
                            span,
                            callee: Box::new(ty.clone()),
                        }
                        .into())
                    }
                }
            }};
        }

        match ty.normalize() {
            Type::Intersection(..) if kind == ExtractKind::New => {
                // TODO(kdy1): Check if all types has constructor signature
                Ok(make_instance_type(ty.clone()))
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => Ok(Type::any(span, Default::default())),

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                debug_assert!(!span.is_dummy());
                Err(ErrorKind::IsTypeUnknown { span }.into())
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
                Type::any(span, Default::default()),
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
            ),

            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => self.extract(
                span,
                expr,
                constraint,
                kind,
                args,
                arg_types,
                spread_arg_types,
                type_args,
                type_ann,
                opts,
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
            Type::Union(u) => self.get_best_return_type(
                span,
                expr,
                ty.clone(),
                kind,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
                SelectOpts {
                    skip_check_for_overloads: u.types.iter().any(|ty| match ty.normalize_instance() {
                        Type::TypeLit(lit) => {
                            //
                            match kind {
                                ExtractKind::New => lit.members.iter().filter(|m| matches!(m, TypeElement::Constructor(..))).count() <= 1,
                                ExtractKind::Call => lit.members.iter().filter(|m| matches!(m, TypeElement::Call(..))).count() <= 1,
                            }
                        }
                        Type::Function(..) if kind == ExtractKind::Call => false,
                        Type::Constructor(..) if kind == ExtractKind::New => false,
                        _ => true,
                    }),
                    ..Default::default()
                },
            ),

            Type::Interface(ref i) => {
                if kind == ExtractKind::New && &**i.name.sym() == "ArrayConstructor" {
                    if let Some(type_args) = type_args {
                        if type_args.params.len() == 1 {
                            return Ok(Type::Array(Array {
                                span,
                                elem_type: Box::new(type_args.params.first().cloned().unwrap()),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }));
                        }
                    }
                }

                // Search for methods
                match self.call_type_element(
                    span,
                    expr,
                    ty,
                    i.type_params.as_deref(),
                    &i.body,
                    kind,
                    args,
                    arg_types,
                    spread_arg_types,
                    type_args,
                    type_ann,
                ) {
                    Ok(ty) => Ok(ty),
                    Err(first_err) => {
                        //  Check parent interface
                        for parent in &i.extends {
                            let parent = self.type_of_ts_entity_name(span, &parent.expr, type_args)?;

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
                                opts,
                            ) {
                                return Ok(v);
                            }
                        }
                        Err(first_err)?
                    }
                }
            }

            Type::TypeLit(ref l) => self.call_type_element(
                span,
                expr,
                ty,
                None,
                &l.members,
                kind,
                args,
                arg_types,
                spread_arg_types,
                type_args,
                type_ann,
            ),

            Type::ClassDef(ref def) if kind == ExtractKind::New => {
                // TODO(kdy1): Remove clone
                Ok(Class {
                    span,
                    def: def.clone(),
                    metadata: Default::default(),
                    tracker: Default::default(),
                }
                .into())
            }

            Type::Intersection(i) => {
                // For intersection, we should select one element which matches
                // the signature

                let mut candidates = vec![];

                for ty in i.types.iter() {
                    candidates.extend(
                        self.extract_callee_candidates(span, kind, ty)
                            .context("tried to extract callable candidates from an intersection type")?,
                    );
                }

                if let Some(v) = self
                    .select_and_invoke(
                        span,
                        kind,
                        expr,
                        &candidates,
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                        type_ann,
                        SelectOpts {
                            skip_check_for_overloads: true,
                            ..Default::default()
                        },
                    )
                    .context("tried to extract")?
                {
                    return Ok(v);
                }

                ret_err!()
            }

            _ => ret_err!(),
        }
    }

    /// Search for members and returns if there's a match
    #[inline(never)]
    fn call_type_element(
        &mut self,
        span: Span,
        expr: ReEvalMode,
        callee_ty: &Type,
        type_params_of_type: Option<&TypeParamDecl>,
        members: &[TypeElement],
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let _tracing = dev_span!("call_type_element");

        let callee_span = callee_ty.span();

        let candidates = members
            .iter()
            .filter_map(|member| match member {
                TypeElement::Call(CallSignature {
                    span,
                    params,
                    type_params,
                    ret_ty,
                }) if kind == ExtractKind::Call => Some(CallCandidate {
                    params: params.clone(),
                    type_params: type_params.clone().or_else(|| type_params_of_type.cloned()),
                    ret_ty: ret_ty.clone().unwrap_or_else(|| Box::new(Type::any(*span, Default::default()))),
                }),
                TypeElement::Constructor(ConstructorSignature {
                    span,
                    params,
                    ret_ty,
                    type_params,
                    ..
                }) if kind == ExtractKind::New => Some(CallCandidate {
                    params: params.clone(),
                    type_params: type_params.clone().or_else(|| type_params_of_type.cloned()),
                    ret_ty: ret_ty.clone().unwrap_or_else(|| Box::new(Type::any(*span, Default::default()))),
                }),
                _ => None,
            })
            .collect::<Vec<_>>();

        if type_params_of_type.is_none()
            && type_args.is_some()
            && members.iter().all(|v| match v {
                TypeElement::Call(c) => c.type_params.is_none(),
                TypeElement::Constructor(c) => c.type_params.is_none(),
                TypeElement::Method(met) => met.type_params.is_none(),
                TypeElement::Property(prop) => prop.type_params.is_none(),
                TypeElement::Index(ind) => true,
            })
        {
            if let Some(type_args) = type_args {
                return Err(ErrorKind::TypeParameterCountMismatch {
                    span,
                    min: 0,
                    max: 0,
                    actual: type_args.params.len(),
                }
                .into());
            }
        }

        if let Some(v) = self
            .select_and_invoke(
                span,
                kind,
                expr,
                &candidates,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
                SelectOpts { ..Default::default() },
            )
            .or_else(|err| {
                // If user selected type arguments, we should not report no matching overload.
                if type_args.is_some() && matches!(&*err, ErrorKind::NoMatchingOverload { .. }) {
                    Ok(Some(Type::any(span, Default::default())))
                } else {
                    Err(err)
                }
            })
            .context("tried to call a type element")?
        {
            return Ok(v);
        }

        match kind {
            ExtractKind::Call => Err(ErrorKind::NoCallSignature {
                span,
                callee: Box::new(callee_ty.clone()),
            }
            .context("failed to select the element to invoke")),
            ExtractKind::New => Err(ErrorKind::NoNewSignature {
                span,
                callee: Box::new(callee_ty.clone()),
            }
            .context("failed to select the element to invoke")),
        }
    }

    #[allow(unused)]
    fn check_method_call(
        &mut self,
        span: Span,
        expr: ReEvalMode,
        c: &MethodSignature,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        self.get_return_type(
            span,
            ExtractKind::Call,
            expr,
            c.type_params.as_ref().map(|v| &*v.params),
            &c.params,
            c.ret_ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(span, Default::default())),
            type_args,
            args,
            arg_types,
            spread_arg_types,
            type_ann,
        )
    }

    pub(super) fn extract_callee_candidates(&mut self, span: Span, kind: ExtractKind, callee: &Type) -> VResult<Vec<CallCandidate>> {
        let span = span.with_ctxt(SyntaxContext::empty());

        let callee = self
            .normalize(Some(span), Cow::Borrowed(callee), Default::default())
            .context("tried to normalize to extract callee")?;

        // TODO(kdy1): Check if signature match.
        match callee.normalize_instance() {
            Type::Intersection(i) => {
                return Ok(i
                    .types
                    .iter()
                    .map(|callee| self.extract_callee_candidates(span, kind, callee))
                    .filter_map(Result::ok)
                    .flatten()
                    .collect());
            }

            Type::Constructor(c) if kind == ExtractKind::New => {
                let candidate = CallCandidate {
                    type_params: c.type_params.clone(),
                    params: c.params.clone(),
                    ret_ty: c.type_ann.clone(),
                };
                return Ok(vec![candidate]);
            }

            Type::Function(f) if kind == ExtractKind::Call => {
                let candidate = CallCandidate {
                    type_params: f.type_params.clone(),
                    params: f.params.clone(),
                    ret_ty: f.ret_ty.clone(),
                };
                return Ok(vec![candidate]);
            }

            Type::Function(f) => {
                let candidate = CallCandidate {
                    type_params: f.type_params.clone(),
                    params: f.params.clone(),
                    ret_ty: Box::new(Type::any(span, Default::default())),
                };
                return Ok(vec![candidate]);
            }

            // Type::Union(ty) => {
            //     // TODO(kdy1): We should select best one based on the argument type and count.
            //     let mut types = ty
            //         .types
            //         .iter()
            //         .cloned()
            //         .map(|callee| {
            //             self.get_best_return_type(span, callee, kind, type_args, args, arg_types,
            // spread_arg_types)         })
            //         .collect::<Result<Vec<_>, _>>()?;

            //     types.dedup_type();
            //     return Ok(Type::new_union(span, types));
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
                    .convert_type_to_type_lit(span, callee, Default::default())?
                    .map(Cow::into_owned)
                    .map(Type::TypeLit);
                if let Some(callee) = callee {
                    return self.extract_callee_candidates(span, kind, &callee);
                }
            }

            Type::TypeLit(ty) => {
                let mut candidates = vec![];
                // Search for callable properties.
                for member in &ty.members {
                    match member {
                        TypeElement::Call(m) if kind == ExtractKind::Call => {
                            candidates.push(CallCandidate {
                                type_params: m.type_params.clone(),
                                params: m.params.clone(),
                                ret_ty: m.ret_ty.clone().unwrap_or_else(|| Box::new(Type::any(m.span, Default::default()))),
                            });
                        }

                        TypeElement::Constructor(m) if kind == ExtractKind::New => {
                            candidates.push(CallCandidate {
                                type_params: m.type_params.clone(),
                                params: m.params.clone(),
                                ret_ty: m.ret_ty.clone().unwrap_or_else(|| Box::new(Type::any(m.span, Default::default()))),
                            });
                        }
                        _ => {}
                    }
                }

                return Ok(candidates);
            }

            Type::ClassDef(cls) => {
                if kind == ExtractKind::Call {
                    return Ok(vec![]);
                }

                let mut candidates = vec![];
                for body in &cls.body {
                    if let ClassMember::Constructor(c) = body {
                        candidates.push(CallCandidate {
                            type_params: c.type_params.clone(),
                            params: c.params.clone(),
                            ret_ty: c.ret_ty.clone().unwrap_or_else(|| {
                                Box::new(Type::Class(Class {
                                    span,
                                    def: cls.clone(),
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }))
                            }),
                        });
                    }
                }

                if candidates.is_empty() {
                    if let Some(sc) = &cls.super_class {
                        candidates.extend(self.extract_callee_candidates(span, kind, sc)?);
                    }
                }

                if candidates.is_empty() {
                    candidates.push(CallCandidate {
                        type_params: Default::default(),
                        params: Default::default(),
                        ret_ty: Box::new(Type::Class(Class {
                            span,
                            def: cls.clone(),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })),
                    });
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
        expr: ReEvalMode,
        callee: Type,
        kind: ExtractKind,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
        opts: SelectOpts,
    ) -> VResult<Type> {
        let span = span.with_ctxt(SyntaxContext::empty());

        let has_spread = arg_types.len() != spread_arg_types.len();

        // TODO(kdy1): Calculate return type only if selected
        // This can be done by storing type params, return type, params in the
        // candidates.
        let candidates = self.extract_callee_candidates(span, kind, &callee)?;

        info!("get_best_return_type: {} candidates", candidates.len());

        if let Some(v) = self
            .select_and_invoke(
                span,
                kind,
                expr,
                &candidates,
                type_args,
                args,
                arg_types,
                spread_arg_types,
                type_ann,
                opts,
            )
            .context("tried to get a best return type")?
        {
            return Ok(v);
        }

        if callee.is_any() {
            return Ok(Type::any(span, Default::default()));
        }

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

        Err(if kind == ExtractKind::Call {
            ErrorKind::NoCallSignature {
                span,
                callee: Box::new(callee),
            }
            .context("tried to calculate return type")
        } else {
            ErrorKind::NoNewSignature {
                span,
                callee: Box::new(callee),
            }
            .context("tried to calculate return type")
        })
    }

    fn validate_arg_count(
        &mut self,
        span: Span,
        params: &[FnParam],
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> VResult<()> {
        /// Count required parameter count.
        fn count_required_pat(p: &RPat) -> usize {
            match p {
                RPat::Rest(p) => {
                    if p.type_ann.is_some() {
                        return 0;
                    }

                    match &*p.arg {
                        RPat::Array(arr) => arr
                            .elems
                            .iter()
                            .map(|v| {
                                v.as_ref()
                                    .map(|pat| match pat {
                                        RPat::Array(RArrayPat { optional: false, .. })
                                        | RPat::Object(RObjectPat { optional: false, .. }) => 1,

                                        RPat::Ident(..) => 0,

                                        _ => 0,
                                    })
                                    .unwrap_or(1)
                            })
                            .sum(),
                        _ => 0,
                    }
                }
                RPat::Ident(RBindingIdent {
                    id: RIdent { sym: js_word!("this"), .. },
                    ..
                }) => 0,
                RPat::Ident(RBindingIdent {
                    type_ann:
                        Some(box RTsTypeAnn {
                            type_ann:
                                box RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsUnionType(RTsUnionType { types, .. })),
                            ..
                        }),
                    id,
                    ..
                }) => usize::from(
                    !(id.optional
                        || types.iter().any(|p| {
                            matches!(
                                *p,
                                box RTsType::TsKeywordType(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                    ..
                                })
                            )
                        })),
                ),
                RPat::Ident(v) => usize::from(!v.id.optional),
                RPat::Array(v) => usize::from(!v.optional),
                RPat::Object(v) => usize::from(!v.optional),
                RPat::Assign(..) | RPat::Invalid(_) | RPat::Expr(_) => 0,
            }
        }

        // Assertion about deep clone
        if cfg!(debug_assertions) {
            let _p = params.to_vec();
            let _a = arg_types.to_vec();
            let _s = spread_arg_types.to_vec();
        }

        let span = span.with_ctxt(SyntaxContext::empty());

        let min_param: usize = params
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.ty.contains_void() {
                    let next_params = &params[i + 1..params.len()];

                    if next_params.is_empty() || next_params.iter().all(|v| v.ty.contains_void()) {
                        return None;
                    }
                }

                Some(&v.pat)
            })
            .map(count_required_pat)
            .sum();

        let mut max_param = Some(params.len());

        for (index, param) in params.iter().enumerate() {
            match &param.pat {
                RPat::Rest(..) => match param.ty.normalize_instance() {
                    Type::Tuple(param_ty) => {
                        for elem in &param_ty.elems {
                            match elem.ty.normalize() {
                                Type::Rest(..) => {
                                    max_param = None;
                                    break;
                                }
                                Type::Optional(..) => {}
                                _ => {
                                    if let Some(max) = &mut max_param {
                                        *max += 1;
                                    }
                                }
                            }
                        }
                        if let Some(max) = &mut max_param {
                            *max -= 1;
                        }
                        continue;
                    }
                    _ => {
                        max_param = None;
                    }
                },
                RPat::Ident(RBindingIdent {
                    id: RIdent { sym: js_word!("this"), .. },
                    ..
                }) => {
                    if let Some(max) = &mut max_param {
                        *max -= 1;
                    }
                    continue;
                }
                _ => {}
            }
        }

        let has_spread = args.iter().any(|arg| arg.spread.is_some());
        if has_spread {
            // TODO: current implementation far from perfect
            match self.validate_arg_count_spread(span, args, arg_types, params, min_param, max_param) {
                Ok(_) => Ok(()),
                Err(err) => Err(err),
            }
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

            // For iife, not providing some arguments are allowed.
            if self.ctx.is_calling_iife {
                if let Some(max) = max_param {
                    if args.len() <= max {
                        return Ok(());
                    }
                }
            }

            if max_param.is_none() {
                if let RPat::Ident(b_id) = &params[args.len()].pat {
                    return Err(ErrorKind::ExpectedAtLeastNArgsButGotM {
                        span,
                        min: min_param,
                        param_name: b_id.clone().id.sym,
                    }
                    .into());
                }
            }

            // function foo(a) {}
            // foo(1, 2, 3)
            //        ^^^^
            let span = args
                .get(min_param)
                .map(|arg| match args.last() {
                    Some(to) => arg.expr.span().to(to.expr.span()),
                    None => arg.expr.span(),
                })
                .unwrap_or(span);

            Err(ErrorKind::ExpectedNArgsButGotM {
                span,
                min: min_param,
                max: max_param,
            }
            .into())
        }
    }

    fn validate_arg_count_spread(
        &mut self,
        span: Span,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        params: &[FnParam],
        min_param: usize,
        max_param: Option<usize>,
    ) -> VResult<()> {
        let mut real_idx = 0;
        let has_rest_param = !params.is_empty() && matches!(params[params.len() - 1].pat, RPat::Rest(_));
        // non required params can be pushed into
        let no_longer_required = |index: usize| !params.is_empty() && index < params.len() && !params[index].required;

        for arg_type in arg_types.iter() {
            let is_spread = arg_type.spread.is_some();
            if !is_spread {
                real_idx += 1;
                continue;
            }
            match arg_type.ty.normalize() {
                Type::Tuple(tuple) => real_idx += tuple.elems.len(),
                _ => {
                    // rest params are always at the end so we can check if it
                    // was passed at least at the end and their must be a rest param
                    if (real_idx < min_param || !has_rest_param) && !no_longer_required(real_idx) {
                        return Err(ErrorKind::SpreadMustBeTupleOrPassedToRest { span: arg_type.span }.into());
                    }
                    real_idx += 1
                }
            }
        }

        if real_idx < min_param {
            if max_param.is_some() {
                return Err(ErrorKind::ExpectedNArgsButGotM {
                    span,
                    min: min_param,
                    max: max_param,
                }
                .into());
            } else {
                if let RPat::Ident(b_id) = &params[real_idx].pat {
                    return Err(ErrorKind::ExpectedAtLeastNArgsButGotM {
                        span,
                        min: min_param,
                        param_name: b_id.clone().id.sym,
                    }
                    .into());
                }

                // This is only useful when the rest parameter isn't last in the function
                // function fn(a: string, ...b: string[], c: string) (this is also an error)
                if let RPat::Rest(r_pat) = &params[real_idx].pat {
                    return Err(ErrorKind::ExpectedNArgsButGotM {
                        span,
                        min: min_param,
                        max: max_param,
                    }
                    .into());
                }
            }
        }

        Ok(())
    }

    /// Returns [None] if nothing matched.
    fn select_and_invoke(
        &mut self,
        span: Span,
        kind: ExtractKind,
        expr: ReEvalMode,
        candidates: &[CallCandidate],
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
        opts: SelectOpts,
    ) -> VResult<Option<Type>> {
        let _tracing = dev_span!("select_and_invoke");

        let span = span.with_ctxt(SyntaxContext::empty());

        let mut callable = candidates
            .iter()
            .map(|c| {
                let res = self.check_call_args(
                    span,
                    c.type_params.as_ref().map(|v| &*v.params),
                    &c.params,
                    type_args,
                    args,
                    arg_types,
                    spread_arg_types,
                );

                (c, res)
            })
            .collect::<Vec<_>>();
        callable.sort_by_key(|(_, res)| *res);

        if candidates.is_empty() {
            return Ok(None);
        }
        if callable.iter().all(|(_, x)| matches!(x, ArgCheckResult::WrongArgCount)) {
            callable.sort_by_key(|(x, _)| {
                x.params
                    .iter()
                    .fold(0, |acc, param| acc + if let RPat::Rest(..) = param.pat { -1 } else { -10 })
            });
        }

        // Check if all candidates are failed.
        if !args.is_empty()
            && type_args.is_none()
            && !opts.skip_check_for_overloads
            && callable.len() > 1
            && callable
                .iter()
                .all(|(_, res)| matches!(res, ArgCheckResult::WrongArgCount | ArgCheckResult::ArgTypeMismatch))
        {
            return Err(ErrorKind::NoMatchingOverload { span }.context("tried to select a call candidate"));
        }

        let (c, _) = callable.into_iter().next().unwrap();

        if candidates.len() == 1 {
            return self
                .get_return_type(
                    span,
                    kind,
                    expr,
                    c.type_params.as_ref().map(|v| &*v.params),
                    &c.params,
                    *c.ret_ty.clone(),
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
            c.type_params.as_ref().map(|v| &*v.params),
            &c.params,
            *c.ret_ty.clone(),
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
    fn get_return_type(
        &mut self,
        span: Span,
        kind: ExtractKind,
        expr: ReEvalMode,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        mut ret_ty: Type,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let _tracing = dev_span!("get_return_type");

        let span = span.with_ctxt(SyntaxContext::empty());
        ret_ty.freeze();
        let type_ann = self.expand_type_ann(span, type_ann)?.freezed();

        // TODO(kdy1): Optimize by skipping clone if `this type` is not used.
        let params = params
            .iter()
            .map(|param| {
                let mut ty = param.ty.clone();
                self.expand_this_in_type(&mut ty);
                ty.freeze();
                FnParam { ty, ..param.clone() }
            })
            .collect_vec();
        self.expand_this_in_type(&mut ret_ty);
        ret_ty.fix();
        ret_ty.freeze();

        let is_type_arg_count_fine = {
            let type_arg_check_res;

            if let ReEvalMode::New(..) = expr {
                if let Type::Class(cls) = ret_ty.normalize() {
                    type_arg_check_res = self.validate_type_args_count(span, cls.def.type_params.as_ref().map(|v| &*v.params), type_args);
                } else {
                    type_arg_check_res = self.validate_type_args_count(span, type_params, type_args);
                }
            } else {
                type_arg_check_res = self.validate_type_args_count(span, type_params, type_args);
            }

            type_arg_check_res.report(&mut self.storage) == Some(())
        };

        let passed_arity_checks;
        let mut is_overload = false;

        if let Type::Class(cls) = ret_ty.normalize() {
            let constructors = &cls.def.body.iter().filter(|m| m.is_constructor()).collect::<Vec<&ClassMember>>();

            if constructors.len() > 1 {
                is_overload = true;

                let valid_arity_constructors = constructors
                    .iter()
                    .filter_map(|c| {
                        if let ClassMember::Constructor(constr) = c {
                            if self
                                .validate_arg_count(constr.span, &constr.params, args, arg_types, spread_arg_types)
                                .is_ok()
                            {
                                Some(constr)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<&ConstructorSignature>>();

                passed_arity_checks = is_type_arg_count_fine && !valid_arity_constructors.is_empty();

                if !passed_arity_checks && is_type_arg_count_fine {
                    self.validate_arg_count(span, &params, args, arg_types, spread_arg_types)
                        .report(&mut self.storage);
                } else {
                    let errors = valid_arity_constructors
                        .clone()
                        .into_iter()
                        .filter_map(|cn| {
                            if let Err(e) = self.validate_arg_types(&cn.params, spread_arg_types, false, true) {
                                Some(e)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<stc_ts_errors::Error>>();

                    if errors.len() >= valid_arity_constructors.len() {
                        if valid_arity_constructors.len() > 1 {
                            self.storage.report(ErrorKind::NoMatchingOverload { span }.into());
                        } else {
                            self.storage.report(errors.last().unwrap().clone());
                        }
                    }
                }
            } else {
                passed_arity_checks = is_type_arg_count_fine
                    && self
                        .validate_arg_count(span, &params, args, arg_types, spread_arg_types)
                        .report(&mut self.storage)
                        .is_some();
            }
        } else {
            passed_arity_checks = is_type_arg_count_fine
                && self
                    .validate_arg_count(span, &params, args, arg_types, spread_arg_types)
                    .report(&mut self.storage)
                    .is_some();
        }

        debug!("get_return_type: \ntype_params = {:?}\nret_ty = {:?}", type_params, ret_ty);

        if let Some(type_params) = type_params {
            for param in type_params {
                info!("({}) Defining {}", self.scope.depth(), param.name);

                self.register_type(param.name.clone(), Type::Param(param.clone()));
            }

            // Assert deep clone
            if cfg!(debug_assertions) {
                let _ = type_args.cloned();
                let _ = type_params.to_vec();
                let _ = params.clone();
                let _ = spread_arg_types.to_vec();
                ret_ty.assert_clone_cheap();
                let _ = type_ann.clone().map(Cow::into_owned);
            }

            debug!("Inferring arg types for a call");
            let mut inferred = self.infer_arg_types(
                span,
                type_args,
                type_params,
                &params,
                spread_arg_types,
                None,
                Some(&ret_ty),
                type_ann.as_deref(),
                InferTypeOpts {
                    is_type_ann: type_ann.is_some(),
                    ..Default::default()
                },
            )?;
            debug!("Inferred types:\n{}", dump_type_map(&inferred.types));
            warn!("Failed to infer types of {:?}", inferred.errored);

            let expanded_param_types = params
                .into_iter()
                .map(|v| -> VResult<_> {
                    let ty = Box::new(self.expand_type_params(&inferred.types, *v.ty, Default::default())?);

                    Ok(FnParam { ty, ..v })
                })
                .collect::<Result<Vec<_>, _>>()?
                .freezed();

            let ctx = Ctx {
                in_argument: true,
                reevaluating_argument: true,
                ..self.ctx
            };
            let mut new_args = vec![];

            for (idx, (arg, param)) in args.iter().zip(expanded_param_types.iter()).enumerate() {
                let arg_ty = &arg_types[idx];
                print_type(&format!("Expanded parameter at {}", idx), &param.ty);
                print_type(&format!("Original argument at {}", idx), &arg_ty.ty);

                let (type_param_decl, actual_params) = match param.ty.normalize() {
                    Type::Function(f) => (&f.type_params, &f.params),
                    _ => {
                        new_args.push(arg_ty.clone());
                        // let ty = box arg.expr.validate_with_args(self, (TypeOfMode::RValue, None,
                        // Some(&param.ty)))?;

                        // new_args.push(TypeOrSpread {
                        //     span: arg.span(),
                        //     spread: arg.spread,
                        //     ty,
                        // });
                        continue;
                    }
                };

                if let Some(type_param_decl) = type_param_decl {
                    for param in &type_param_decl.params {
                        self.register_type(param.name.clone(), Type::Param(param.clone()));
                    }
                }

                // TODO: Use apply_fn_type_ann instead
                let mut patch_arg = |idx: usize, pat: &RPat| -> VResult<()> {
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
                            Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                                metadata,
                                ..
                            }) if metadata.common.implicit => {
                                // let new_ty =
                                // RTsType::from(actual.ty.clone()).validate_with(self)?;
                                // if let Some(node_id) = pat.node_id() {
                                //     if let Some(m) = &mut self.mutations {
                                //         m.for_pats.entry(node_id).or_default().ty = Some(new_ty);
                                //     }
                                // }
                                let new_ty = *actual.ty.clone();
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

                        info!("Inferring type of arrow expr with updated type");
                        // It's okay to use default as we have patched parameters.
                        let mut ty = Box::new(Type::Function(arrow.validate_with_default(&mut *self.with_ctx(ctx))?));
                        self.add_required_type_params(&mut ty);
                        ty
                    }
                    RExpr::Fn(fn_expr) => {
                        for (idx, param) in fn_expr.function.params.iter().enumerate() {
                            patch_arg(idx, &param.pat)?;
                        }

                        info!("Inferring type of function expr with updated type");
                        let mut ty = Box::new(Type::Function(
                            fn_expr
                                .function
                                .validate_with_args(&mut *self.with_ctx(ctx), fn_expr.ident.as_ref())?,
                        ));
                        self.add_required_type_params(&mut ty);
                        ty
                    }
                    _ => arg_ty.ty.clone(),
                };
                print_type(&format!("Mapped argument at {}", idx), &arg_ty.ty);

                let new_arg = TypeOrSpread { ty, ..arg_ty.clone() };

                new_args.push(new_arg);
            }

            if !self.ctx.reevaluating_call_or_new && type_ann.is_none() {
                debug!("Reevaluating a call");
                let ctx = Ctx {
                    reevaluating_call_or_new: true,
                    ..self.ctx
                };
                match expr {
                    ReEvalMode::Call(e) => {
                        return e.validate_with_args(&mut *self.with_ctx(ctx), None);
                    }
                    ReEvalMode::New(e) => {
                        return e.validate_with_args(&mut *self.with_ctx(ctx), None);
                    }
                    _ => {}
                }
            }

            // if arg.len() > param.len(), we need to add all args
            if arg_types.len() > expanded_param_types.len() {
                #[allow(clippy::needless_range_loop)]
                for idx in expanded_param_types.len()..arg_types.len() {
                    let ty = &arg_types[idx].ty;
                    print_type(&format!("Expanded param type at {}", idx), ty);
                }
                new_args.extend(arg_types[expanded_param_types.len()..].iter().cloned());
            }

            // We have to recalculate types.
            let mut new_arg_types;
            let spread_arg_types = if new_args.iter().any(|arg| arg.spread.is_some()) {
                new_arg_types = vec![];
                for arg in &new_args {
                    if arg.spread.is_some() {
                        match arg.ty.normalize() {
                            Type::Tuple(arg_ty) => {
                                new_arg_types.extend(arg_ty.elems.iter().map(|element| &element.ty).cloned().map(|ty| TypeOrSpread {
                                    span: arg.spread.unwrap(),
                                    spread: None,
                                    ty,
                                }));
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
                new_arg_types.freeze();

                &*new_arg_types
            } else {
                new_args.fix();
                new_args.freeze();

                &*new_args
            };

            ret_ty.fix();
            let ret_ty = self.expand(span, ret_ty, Default::default())?;

            for item in &expanded_param_types {
                item.ty.assert_valid();

                // Assertion for deep clones
                if cfg!(debug_assertions) {
                    let _ = item.ty.clone();
                }
            }

            for item in spread_arg_types {
                item.ty.assert_valid();

                if cfg!(debug_assertions) {
                    let _ = item.ty.clone();
                }
            }

            if passed_arity_checks {
                if !is_overload {
                    self.validate_arg_types(&expanded_param_types, spread_arg_types, true, false)?;
                }
            }

            if self.ctx.is_instantiating_class {
                for tp in type_params.iter() {
                    if !inferred.types.contains_key(&tp.name) {
                        inferred.types.insert(
                            tp.name.clone(),
                            Type::Keyword(KeywordType {
                                span: tp.span,
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                metadata: KeywordTypeMetadata {
                                    common: tp.metadata.common,
                                    ..Default::default()
                                },
                                tracker: Default::default(),
                            }),
                        );
                    }
                }
            }

            for id in &inferred.errored {
                inferred.types.insert(
                    id.clone(),
                    Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        metadata: KeywordTypeMetadata { ..Default::default() },
                        tracker: Default::default(),
                    }),
                );
            }

            print_type("Return", &ret_ty);
            let mut ty = self.expand_type_params(&inferred.types, ret_ty, Default::default())?.freezed();
            print_type("Return, expanded", &ty);

            ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });

            if ty.is_conditional() {
                if let Ok(new) = self.normalize(Some(span), Cow::Borrowed(&ty), Default::default()) {
                    ty = new.into_owned();
                }
            }

            print_type("Return, simplified", &ty);

            ty = self.simplify(ty);

            print_type("Return, simplified again", &ty);

            if type_ann.is_none() && (ty.is_class() || ty.is_class_def()) {
                ty = ty.fold_with(&mut ReturnTypeGeneralizer { analyzer: self });

                print_type("Return, generalized", &ty);
            }

            self.add_required_type_params(&mut ty);

            print_type("Return, after adding type params", &ty);

            ty.reposition(span);
            ty.freeze();

            if kind == ExtractKind::Call {
                self.add_call_facts(&expanded_param_types, args, &mut ty);
            }

            return Ok(ty);
        }

        if passed_arity_checks && !is_overload {
            self.validate_arg_types(&params, spread_arg_types, type_params.is_some(), false)?;
        }

        print_type("Return", &ret_ty);

        ret_ty.reposition(span);
        ret_ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });

        print_type("Return, simplified", &ret_ty);

        self.add_required_type_params(&mut ret_ty);
        ret_ty.freeze();

        if kind == ExtractKind::Call {
            self.add_call_facts(&params, args, &mut ret_ty);
        }

        Ok(ret_ty)
    }

    fn validate_arg_types(
        &mut self,
        params: &[FnParam],
        spread_arg_types: &[TypeOrSpread],
        is_generic: bool,
        is_overload: bool,
    ) -> VResult<()> {
        info!("[exprs] Validating arguments");

        macro_rules! report_err {
            ($err:expr) => {{
                if is_overload {
                    return Err($err.into());
                } else {
                    self.storage.report($err.context("tried to validate an argument"));
                }

                if is_generic {
                    return Ok(());
                }
            }};
        }

        let marks = self.marks();

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
                        match arg.ty.normalize() {
                            Type::Tuple(..) => {
                                report_err!(ErrorKind::ExpectedAtLeastNArgsButGotMOrMore {
                                    span: arg.span(),
                                    min: rest_idx - 1,
                                });
                                return Ok(());
                            }

                            _ => {
                                report_err!(ErrorKind::SpreadMustBeTupleOrPassedToRest { span: arg.span() });
                                return Ok(());
                            }
                        }
                    }
                }
            }
        }

        let mut params_iter = params.iter().filter(|param| {
            !matches!(
                param.pat,
                RPat::Ident(RBindingIdent {
                    id: RIdent { sym: js_word!("this"), .. },
                    ..
                })
            )
        });
        let mut args_iter = spread_arg_types.iter();

        while let (Some(param), Some(arg)) = (params_iter.next(), args_iter.next()) {
            if let RPat::Rest(..) = &param.pat {
                let param_ty = self.normalize(Some(arg.span()), Cow::Borrowed(&param.ty), Default::default());

                let param_ty = match param_ty {
                    Ok(v) => v,
                    Err(err) => {
                        report_err!(err);
                        return Ok(());
                    }
                }
                .freezed();

                // Handle
                //
                //   param: (...x: [boolean, sting, ...number])
                //   arg: (true, 'str')
                //      or
                //   arg: (true, 'str', 10)
                if arg.spread.is_none() {
                    match param_ty.normalize() {
                        Type::Tuple(param_ty) if !param_ty.elems.is_empty() => {
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
                                        inner: Box::new(err),
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
                                        inner: Box::new(err.into()),
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
                        _ => {}
                    }
                }

                if arg.spread.is_some() {
                    if let Ok(()) = self.assign_with_opts(
                        &mut Default::default(),
                        &param.ty,
                        &arg.ty,
                        AssignOpts {
                            span: arg.span(),
                            allow_iterable_on_rhs: true,
                            ..Default::default()
                        },
                    ) {
                        continue;
                    }
                }

                match param_ty.normalize() {
                    Type::Array(arr) => {
                        // We should change type if the parameter is a rest parameter.
                        let res = self.assign(arg.span(), &mut Default::default(), &arr.elem_type, &arg.ty);
                        let err = match res {
                            Ok(()) => continue,
                            Err(err) => err,
                        };

                        let err = err
                            .convert(|err| ErrorKind::WrongArgType {
                                span: arg.span(),
                                inner: Box::new(err.into()),
                            })
                            .context("tried assigning elem type of an array because parameter is declared as a rest pattern");
                        report_err!(err);
                        return Ok(());
                    }
                    _ => {
                        if let Ok(()) = self.assign_with_opts(
                            &mut Default::default(),
                            &param.ty,
                            &arg.ty,
                            AssignOpts {
                                span: arg.span(),
                                allow_iterable_on_rhs: true,
                                ..Default::default()
                            },
                        ) {
                            continue;
                        }
                    }
                }
            }

            if arg.spread.is_some() {
                let res = self.get_iterator_element_type(arg.span(), Cow::Borrowed(&arg.ty), false, Default::default());
                match res {
                    Ok(arg_elem_ty) => {
                        // We should change type if the parameter is a rest parameter.
                        if let Ok(()) = self.assign(arg.span(), &mut Default::default(), &param.ty, &arg_elem_ty) {
                            continue;
                        }
                    }
                    Err(err) => {
                        if let ErrorKind::MustHaveSymbolIteratorThatReturnsIterator { span } = &*err {
                            report_err!(ErrorKind::SpreadMustBeTupleOrPassedToRest { span: *span });
                            return Ok(());
                        }
                    }
                }

                let res = self
                    .assign_with_opts(
                        &mut Default::default(),
                        &param.ty,
                        &arg.ty,
                        AssignOpts {
                            span: arg.span(),
                            ..Default::default()
                        },
                    )
                    .convert_err(|err| {
                        // Once a param is not required no further params are required
                        // Which means you just need to type check the spread
                        if matches!(param.pat, RPat::Rest(..)) || !param.required {
                            ErrorKind::WrongArgType {
                                span: arg.span(),
                                inner: Box::new(err.into()),
                            }
                        } else {
                            ErrorKind::SpreadMustBeTupleOrPassedToRest { span: arg.span() }
                        }
                    })
                    .context("arg is spread");
                if let Err(err) = res {
                    report_err!(err);
                    return Ok(());
                }
            } else {
                let allow_unknown_rhs = arg.ty.metadata().resolved_from_var || !matches!(arg.ty.normalize(), Type::TypeLit(..));

                let mut p = &param.ty.clone();
                let binding = &Box::new(Type::unknown(param.ty.span(), Default::default()));

                if let Type::Param(t) = param.ty.normalize() {
                    if let Some(constr) = &t.constraint {
                        p = constr;
                    } else {
                        p = binding;
                    }
                }

                if let Err(err) = self.assign_with_opts(
                    &mut Default::default(),
                    p,
                    &arg.ty,
                    AssignOpts {
                        span: arg.span(),
                        allow_unknown_rhs: Some(allow_unknown_rhs),
                        use_missing_fields_for_class: true,
                        allow_assignment_to_void: false,
                        do_not_use_single_error_for_tuple_with_rest: true,
                        ..Default::default()
                    },
                ) {
                    let err = err.convert(|err| {
                        match err {
                            ErrorKind::TupleAssignError { span, errors } if !arg.ty.metadata().resolved_from_var => {
                                return ErrorKind::Errors { span, errors }
                            }
                            ErrorKind::ObjectAssignFailed { span, errors } if !arg.ty.metadata().resolved_from_var => {
                                return ErrorKind::Errors { span, errors }
                            }
                            ErrorKind::Errors { span, ref errors } => {
                                if errors
                                    .iter()
                                    .all(|err| matches!(&**err, ErrorKind::UnknownPropertyInObjectLiteralAssignment { span }))
                                {
                                    return ErrorKind::Errors {
                                        span,
                                        errors: errors
                                            .iter()
                                            .map(|err| {
                                                ErrorKind::WrongArgType {
                                                    span: err.span(),
                                                    inner: Box::new(err.clone()),
                                                }
                                                .into()
                                            })
                                            .collect(),
                                    };
                                }
                            }
                            _ => {}
                        }

                        if let RPat::Array(array_pat) = &param.pat {
                            if param.ty.is_array() && array_pat.type_ann.is_none() && !arg.ty.is_array() {
                                return ErrorKind::NotArrayType { span: param.span() };
                            }
                        }

                        ErrorKind::WrongArgType {
                            span: arg.span(),
                            inner: Box::new(err.into()),
                        }
                    });

                    report_err!(err);
                    return Ok(());
                }
            }
        }
        Ok(())
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
    #[allow(clippy::needless_pass_by_ref_mut)]
    fn add_call_facts(&mut self, params: &[FnParam], args: &[RExprOrSpread], ret_ty: &mut Type) {
        if !self.ctx.in_cond {
            return;
        }

        if let Type::Predicate(p) = ret_ty.normalize() {
            let ctx = Ctx {
                is_type_predicate: true,
                ..self.ctx
            };

            let ty = match &p.ty {
                Some(v) => v,
                None => return,
            };

            match &p.param_name {
                RTsThisTypeOrIdent::TsThisType(this) => {
                    //
                    self.store_call_fact_for_var(this.span, Id::word("this".into()), &ty.clone().freezed());
                }
                RTsThisTypeOrIdent::Ident(arg_id) => {
                    for (idx, param) in params.iter().enumerate() {
                        match &param.pat {
                            RPat::Ident(i) if i.id.sym == arg_id.sym => {
                                // TODO(kdy1): Check length of args.
                                let arg = &args[idx];
                                if let RExpr::Ident(var_name) = &*arg.expr {
                                    let ty = ty.clone().freezed();
                                    self.with_ctx(ctx).store_call_fact_for_var(var_name.span, var_name.into(), &ty);
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    fn narrow_with_predicate(&mut self, span: Span, orig_ty: &Type, new_ty: Type) -> VResult<Type> {
        let _tracing = dev_span!("narrow_with_predicate");

        let span = span.with_ctxt(SyntaxContext::empty());

        let orig_ty = self
            .normalize(Some(span), Cow::Borrowed(orig_ty), Default::default())
            .context("tried to normalize original type")?
            .freezed();

        let new_ty = self
            .normalize(Some(span), Cow::Owned(new_ty), Default::default())
            .context("tried to normalize new type")?
            .freezed();

        let use_simple_intersection = (|| {
            if let (Type::Interface(orig), Type::Interface(new)) = (orig_ty.normalize(), new_ty.normalize()) {
                if orig.extends.is_empty() && new.extends.is_empty() {
                    return true;
                }
            }

            false
        })();

        if use_simple_intersection {
            return Ok(Type::Intersection(Intersection {
                span,
                types: vec![orig_ty.into_owned(), new_ty.into_owned()],
                metadata: Default::default(),
                tracker: Default::default(),
            }));
        }

        match new_ty.normalize() {
            Type::Keyword(keyword) => {
                if let TsKeywordTypeKind::TsObjectKeyword = keyword.kind {
                    if orig_ty.is_any() {
                        return Ok(orig_ty.into_owned());
                    }
                }
            }

            Type::Lit(..) => {}
            _ => {
                match orig_ty.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        if new_ty.is_fn_type() {
                            return Ok(orig_ty.into_owned());
                        }

                        return Ok(new_ty.into_owned());
                    }
                    Type::Union(..) | Type::Interface(..) => {}

                    _ => {
                        if let Some(v) = self.extends(
                            span,
                            &orig_ty,
                            &new_ty,
                            ExtendsOpts {
                                allow_missing_fields: true,
                                ..Default::default()
                            },
                        ) {
                            if v {
                                if let Type::ClassDef(def) = orig_ty.normalize() {
                                    return Ok(Type::Class(Class {
                                        span,
                                        def: def.clone(),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }));
                                }
                                return Ok(orig_ty.into_owned());
                            }
                        }

                        return Ok(new_ty.into_owned());
                    }
                }

                let mut new_types = vec![];

                let mut did_upcast = false;
                for ty in orig_ty.iter_union() {
                    if let Some(true) = self.extends(span, &new_ty, ty, Default::default()) {
                        did_upcast = true;
                        new_types.push(new_ty.clone().into_owned());
                    } else if let Some(true) = self.extends(span, ty, &new_ty, Default::default()) {
                        new_types.push(ty.clone());
                    }
                }

                // TODO(kdy1): Use super class instead of
                if !did_upcast && new_types.is_empty() {
                    new_types.push(new_ty.clone().into_owned());
                }

                new_types.dedup_type();
                let mut new_ty = Type::new_union_without_dedup(span, new_types);
                if did_upcast {
                    new_ty.metadata_mut().prevent_converting_to_children = true;
                }
                return Ok(new_ty);
            }
        }

        if let Type::ClassDef(def) = new_ty.normalize() {
            return Ok(Type::Class(Class {
                span,
                def: def.clone(),
                metadata: Default::default(),
                tracker: Default::default(),
            }));
        }

        Ok(new_ty.into_owned())
    }

    #[extra_validator]
    fn store_call_fact_for_var(&mut self, span: Span, var_name: Id, new_ty: &Type) {
        match new_ty.normalize() {
            Type::Keyword(..) | Type::Lit(..) => {}
            _ => {
                if let Some(previous_types) = self.find_var_type(&var_name.clone(), TypeOfMode::RValue).map(Cow::into_owned) {
                    let narrowed_ty = self.narrow_with_predicate(span, &previous_types, new_ty.clone())?.fixed().freezed();
                    self.add_type_fact(&var_name, narrowed_ty, new_ty.clone());
                    return;
                }
            }
        }

        let new_ty = new_ty.clone().freezed();
        self.add_type_fact(&var_name, new_ty.clone(), new_ty);
    }

    pub(crate) fn validate_type_args_count(
        &mut self,
        span: Span,
        type_params: Option<&[TypeParam]>,
        type_args: Option<&TypeParamInstantiation>,
    ) -> VResult<()> {
        match (type_params, type_args) {
            (Some(type_params), Some(type_args)) => {
                // TODO(kdy1): Handle defaults of the type parameter (Change to range)
                if type_params.len() != type_args.params.len() {
                    return Err(ErrorKind::TypeParameterCountMismatch {
                        span,
                        max: type_params.len(),
                        min: type_params.len(),
                        actual: type_args.params.len(),
                    }
                    .into());
                }
            }
            (None, Some(type_args)) => {
                return Err(ErrorKind::TypeParameterCountMismatch {
                    span,
                    max: 0,
                    min: 0,
                    actual: type_args.params.len(),
                }
                .into());
            }
            _ => {}
        };

        Ok(())
    }

    fn is_subtype_in_fn_call(&mut self, span: Span, arg: &Type, param: &Type) -> bool {
        let _tracing = dev_span!("is_subtype_in_fn_call");

        if arg.type_eq(param) {
            return true;
        }

        if param.is_any() {
            return true;
        }

        if arg.is_any() {
            return false;
        }

        let res = self.assign_with_opts(
            &mut Default::default(),
            arg,
            param,
            AssignOpts {
                span,
                for_castablity: true,
                ..Default::default()
            },
        );
        res.is_ok()
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
        let _tracing = dev_span!("check_call_args");

        if self.validate_type_args_count(span, type_params, type_args).is_err() {
            return ArgCheckResult::WrongArgCount;
        }

        if self.validate_arg_count(span, params, args, arg_types, spread_arg_types).is_err() {
            return ArgCheckResult::WrongArgCount;
        }

        self.with_scope_for_type_params(|analyzer: &mut Analyzer| {
            if let Some(type_params) = type_params {
                for param in type_params {
                    analyzer.register_type(param.name.clone(), Type::Param(param.clone()));
                }
            }

            let mut exact = true;

            if let (Some(arg), Some(param)) = (type_args, type_params) {
                for (t_arg, t_param) in arg.params.iter().zip(param) {
                    if let Some(t_param) = &t_param.constraint {
                        if !analyzer.is_subtype_in_fn_call(span, t_arg, t_param) {
                            return ArgCheckResult::TypeParamMismatch;
                        }
                    }
                }
            }

            for (arg, param) in spread_arg_types.iter().zip(params) {
                if matches!(param.pat, RPat::Rest(..)) && !arg.ty.is_array() {
                    continue;
                }

                if analyzer
                    .assign_with_opts(
                        &mut Default::default(),
                        &param.ty,
                        &arg.ty,
                        AssignOpts {
                            span,
                            allow_unknown_rhs: Some(true),
                            allow_assignment_to_param: true,
                            allow_assignment_to_param_constraint: true,
                            ..Default::default()
                        },
                    )
                    .is_err()
                {
                    return ArgCheckResult::ArgTypeMismatch;
                }

                if !analyzer.is_subtype_in_fn_call(span, &arg.ty, &param.ty) {
                    exact = false;
                }
            }

            if analyzer.scope.is_call_arg_count_unknown || !exact {
                return ArgCheckResult::MayBe;
            }
            ArgCheckResult::Exact
        })
    }

    fn apply_type_ann_from_callee(
        &mut self,
        span: Span,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        callee: &Type,
        type_args: Option<&TypeParamInstantiation>,
        type_ann_for_return_type: Option<&Type>,
    ) -> VResult<()> {
        let c = self.extract_callee_candidates(span, kind, callee)?;

        if c.len() != 1 {
            return Ok(());
        }

        let c = c.into_iter().next().unwrap();

        let params = match (&c.type_params, type_args, type_ann_for_return_type) {
            (Some(type_params), Some(type_args), _) => {
                let mut map = FxHashMap::default();

                for (param, arg) in type_params.params.iter().zip(type_args.params.iter()) {
                    map.insert(param.name.clone(), arg.clone());
                }

                let params = c.params.clone().freezed();

                let mut params = self.expand_type_params(&map, params, Default::default())?;

                for param in &mut params {
                    self.add_required_type_params(&mut param.ty);
                }

                params.freeze();

                Cow::Owned(params)
            }

            (Some(type_params), None, Some(type_ann)) => {
                let map = self.infer_type_with_types(span, &type_params.params, &c.ret_ty, type_ann, Default::default())?;
                let params = c.params.clone().freezed();

                let mut params = self.expand_type_params(&map, params, Default::default())?;

                for param in &mut params {
                    self.add_required_type_params(&mut param.ty);
                }

                params.freeze();

                Cow::Owned(params)
            }

            (Some(..), _, None) => {
                return Ok(());
            }

            _ => Cow::Borrowed(&c.params),
        };

        for (arg, param) in args.iter().zip(params.iter()) {
            // TODO(kdy1):  Handle rest
            if arg.spread.is_some() || matches!(param.pat, RPat::Rest(..)) {
                break;
            }

            self.apply_type_ann_to_expr(&arg.expr, &param.ty)?;
        }

        Ok(())
    }

    pub(crate) fn apply_type_ann_to_expr(&mut self, arg: &RExpr, type_ann: &Type) -> VResult<()> {
        type_ann.assert_clone_cheap();

        match arg {
            RExpr::Paren(arg) => return self.apply_type_ann_to_expr(&arg.expr, type_ann),
            RExpr::Fn(arg) => {
                self.apply_fn_type_ann(
                    arg.span(),
                    arg.function.node_id,
                    arg.function.params.iter().map(|v| &v.pat),
                    Some(type_ann),
                );
            }
            RExpr::Arrow(arg) => {
                self.apply_fn_type_ann(arg.span(), arg.node_id, arg.params.iter(), Some(type_ann));
            }
            _ => {
                if let Some(mutations) = &mut self.mutations {
                    if let Some(node_id) = arg.node_id() {
                        if !node_id.is_invalid() {
                            mutations.for_exprs.entry(node_id).or_default().type_ann = Some(type_ann.clone());
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn validate_args(&mut self, args: &[RExprOrSpread]) -> VResult<Vec<TypeOrSpread>> {
        let ctx = Ctx {
            in_argument: true,
            should_store_truthy_for_access: false,
            prefer_tuple_for_array_lit: true,
            array_lit_cannot_be_tuple: false,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|this: &mut Analyzer| {
            let args: Vec<_> = args
                .iter()
                .map(|arg| {
                    arg.validate_with(this).report(&mut this.storage).unwrap_or_else(|| TypeOrSpread {
                        span: arg.span(),
                        spread: arg.spread,
                        ty: Box::new(Type::any(arg.expr.span(), Default::default())),
                    })
                })
                .collect();

            Ok(args)
        })
    }
}

/// Used for reevaluation.
#[derive(Clone, Copy)]
pub(crate) enum ReEvalMode<'a> {
    Call(&'a RCallExpr),
    New(&'a RNewExpr),
    NoReEval,
}

impl Default for ReEvalMode<'_> {
    fn default() -> Self {
        Self::NoReEval
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

        // TODO(kdy1): PERF
        ty.normalize_mut();

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

impl VisitMut<Type> for ReturnTypeSimplifier<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty.visit_mut_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        metadata,
                        ..
                    }),
                ..
            }) => {
                *ty = Type::Keyword(KeywordType {
                    span: *span,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    metadata: *metadata,
                    tracker: Default::default(),
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: ref obj_ty @ box Type::Ref(..),
                index_type,
                metadata,
                ..
            }) if is_str_lit_or_union(index_type) => {
                let mut types: Vec<Type> = vec![];

                for index_ty in index_type.iter_union() {
                    let (lit_span, value) = match index_ty.normalize() {
                        Type::Lit(LitType {
                            span: lit_span,
                            lit: RTsLit::Str(RStr { value, .. }),
                            ..
                        }) => (*lit_span, value.clone()),
                        _ => return,
                    };

                    if let Some(actual_ty) = self
                        .analyzer
                        .access_property(
                            *span,
                            obj_ty,
                            &Key::Normal {
                                span: lit_span,
                                sym: value.clone(),
                            },
                            TypeOfMode::RValue,
                            IdCtx::Type,
                            Default::default(),
                        )
                        .context("tried to access property to simplify return type")
                        .report(&mut self.analyzer.storage)
                    {
                        if types.iter().all(|prev_ty| !(*prev_ty).type_eq(&actual_ty)) {
                            types.push(actual_ty);
                        }
                    }
                }

                *ty = Type::Union(Union {
                    span: *span,
                    types,
                    metadata: UnionMetadata {
                        common: metadata.common,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
                .fixed();
            }

            Type::IndexedAccessType(iat) if is_str_lit_or_union(&iat.index_type) => {
                prevent_generalize(ty);
            }

            // Boxed<A | B | C> => Boxed<A> | Boxed<B> | Boxed<C>
            Type::Ref(Ref {
                span,
                type_name: RTsEntityName::Ident(i),
                type_args: Some(type_args),
                metadata,
                ..
            }) if type_args.params.len() == 1 && type_args.params.iter().any(|ty| matches!(ty.normalize(), Type::Union(..))) => {
                // TODO(kdy1): Replace .ok() with something better
                if let Some(types) = self.analyzer.find_type(&(&*i).into()).ok().flatten() {
                    type_args.freeze();

                    for stored_ty in types {
                        if let Type::Alias(Alias { ty: aliased_ty, .. }) = stored_ty.normalize() {
                            let mut types = vec![];

                            if let Type::Union(type_arg) = &type_args.params[0].normalize() {
                                for ty in &type_arg.types {
                                    types.push(Type::Ref(Ref {
                                        span: *span,
                                        type_name: RTsEntityName::Ident(i.clone()),
                                        type_args: Some(Box::new(TypeParamInstantiation {
                                            span: type_args.span,
                                            params: vec![ty.clone()],
                                        })),
                                        metadata: *metadata,
                                        tracker: Default::default(),
                                    }))
                                }
                            } else {
                                unreachable!()
                            }

                            *ty = Type::new_union(*span, types);
                            return;
                        }
                    }
                }
            }

            _ => {}
        }
    }
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
    TypeParamMismatch,
    WrongArgCount,
}

#[derive(Debug, Default, Clone, Copy)]
struct SelectOpts {
    /// Defaults to false.
    skip_check_for_overloads: bool,
}

/// Ensure that sort work as expected.
#[test]
fn test_arg_check_result_order() {
    let mut v = vec![
        ArgCheckResult::Exact,
        ArgCheckResult::MayBe,
        ArgCheckResult::ArgTypeMismatch,
        ArgCheckResult::TypeParamMismatch,
        ArgCheckResult::WrongArgCount,
    ];
    let expected = v.clone();
    v.sort();

    assert_eq!(v, expected);
}

/// TODO(kdy1): Use cow
#[derive(Debug)]
pub(super) struct CallCandidate {
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    pub ret_ty: Box<Type>,
}
