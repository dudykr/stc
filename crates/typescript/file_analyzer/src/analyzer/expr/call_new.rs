//! Handles new expressions and call expressions.
use super::super::Analyzer;
use crate::{
    analyzer::{
        expr::TypeOfMode,
        marks::MarkExt,
        util::{instantiate_class, ResultExt},
        Ctx, ScopeKind,
    },
    ty,
    ty::{
        CallSignature, ClassInstance, ConstructorSignature, FnParam, Method, MethodSignature,
        QueryExpr, QueryType, Type, TypeElement, TypeOrSpread, TypeParam, TypeParamInstantiation,
    },
    util::{is_str_lit_or_union, type_ext::TypeVecExt},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::FxHashMap;
use rnode::Fold;
use rnode::FoldWith;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RCallExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSpread;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RNewExpr;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsThisTypeOrIdent;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeParamInstantiation;
use stc_ts_errors::debug::print_type;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::rprop_name_to_expr;
use stc_ts_types::{Alias, Id, IndexedAccessType, Ref, Symbol, TypeLit, Union};
use stc_ts_utils::PatExt;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;
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

        self.with_child(
            ScopeKind::Call,
            Default::default(),
            |analyzer: &mut Analyzer| {
                analyzer.extract_call_new_expr_member(
                    callee,
                    type_ann,
                    ExtractKind::Call,
                    args,
                    type_args.as_ref(),
                )
            },
        )
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

        self.with_child(
            ScopeKind::Call,
            Default::default(),
            |analyzer: &mut Analyzer| {
                analyzer.extract_call_new_expr_member(
                    callee,
                    type_ann,
                    ExtractKind::New,
                    args.as_ref().map(|v| &**v).unwrap_or_else(|| &mut []),
                    type_args.as_ref(),
                )
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExtractKind {
    New,
    Call,
}

impl Analyzer<'_, '_> {
    /// Calculates the return type of a new /call expression.
    ///
    /// This method check arguments
    fn extract_call_new_expr_member(
        &mut self,
        callee: &RExpr,
        type_ann: Option<&Type>,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        type_args: Option<&RTsTypeParamInstantiation>,
    ) -> ValidationResult {
        debug_assert_eq!(self.scope.kind(), ScopeKind::Call);

        let span = callee.span();

        slog::debug!(self.logger, "extract_call_new_expr_member");

        match *callee {
            RExpr::Ident(ref i) if i.sym == js_word!("require") => {
                let id = args
                    .iter()
                    .cloned()
                    .map(|arg| match arg {
                        RExprOrSpread { spread: None, expr } => match *expr {
                            RExpr::Lit(RLit::Str(RStr { span, value, .. })) => {
                                RIdent::new(value.clone(), span).into()
                            }
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
                    unimplemented!(
                        "Error reporting for calling `Symbol` with arguments is not implemented \
                         yet"
                    )
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
                {
                    // Handle toString()
                    macro_rules! handle {
                        () => {{
                            return Ok(box Type::from(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                            }));
                        }};
                    }
                    match **prop {
                        RExpr::Ident(RIdent {
                            sym: js_word!("toString"),
                            ..
                        }) if !computed => handle!(),
                        RExpr::Lit(RLit::Str(RStr {
                            value: js_word!("toString"),
                            ..
                        })) => handle!(),

                        _ => {}
                    }
                }

                // Handle member expression
                let obj_type = obj.validate_with_default(self)?.generalize_lit();

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

                match *obj_type.normalize() {
                    Type::Function(ref f) if kind == ExtractKind::Call => {
                        //
                        return Ok(f.ret_ty.clone());
                    }

                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        return Ok(Type::any(span));
                    }

                    Type::Interface(ref i) => {
                        if self.scope.is_calling() {
                            if let Some(type_params) = &i.type_params {
                                for param in &type_params.params {
                                    self.scope
                                        .types
                                        .entry(param.name.clone())
                                        .or_default()
                                        .push(Type::Param(param.clone()).cheap());
                                }
                            }
                        }
                        // TODO: Check parent interface
                        return self.search_members_for_callable_prop(
                            kind, span, &i.body, prop, computed, args,
                        );
                    }

                    Type::TypeLit(ref t) => {
                        return self.search_members_for_callable_prop(
                            kind, span, &t.members, prop, computed, args,
                        );
                    }

                    Type::Class(ty::Class { ref body, .. }) => {
                        for member in body.iter() {
                            match *member {
                                ty::ClassMember::Method(Method {
                                    ref key,
                                    ref ret_ty,
                                    ..
                                }) => {
                                    if rprop_name_to_expr(key.clone()).eq_ignore_span(&*prop) {
                                        return Ok(ret_ty.clone());
                                    }
                                }
                                _ => {}
                            }
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

                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };
                let obj_type = self.with_ctx(ctx).expand_fully(span, obj_type, true)?;

                let callee =
                    self.access_property(span, obj_type, prop, computed, TypeOfMode::RValue)?;
                let callee = self.with_ctx(ctx).expand_fully(span, callee, true)?;

                let type_args = match type_args {
                    None => None,
                    Some(v) => {
                        let mut type_args = v.validate_with(self)?;
                        // If user specified type in call / new, preserve it.
                        self.prevent_expansion(&mut type_args);

                        Some(type_args)
                    }
                };

                let arg_types = self.validate_args(args)?;
                let spread_arg_types = self.spread_args(&arg_types);

                self.get_best_return_type(
                    span,
                    callee,
                    kind,
                    type_args,
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
                            }) if type_args.is_some() => {
                                analyzer.storage.report(Error::TS2347 { span })
                            }
                            _ => {}
                        }
                        callee_ty
                    };

                    let type_args: Option<TypeParamInstantiation> = match type_args {
                        None => None,
                        Some(type_args) => {
                            let mut type_args = type_args.validate_with(analyzer)?;
                            // Preserve types specified by user
                            analyzer.prevent_expansion(&mut type_args);
                            Some(type_args)
                        }
                    };

                    if let Some(type_args) = &type_args {
                        let type_params = match callee_ty.normalize() {
                            Type::Function(f) => f.type_params.as_ref(),
                            _ => None,
                        };
                        if let Some(type_param_decl) = type_params {
                            let mut params = FxHashMap::default();

                            for (type_param, ty) in
                                type_param_decl.params.iter().zip(type_args.params.iter())
                            {
                                params.insert(type_param.name.clone(), ty.clone());
                            }

                            callee_ty = analyzer.expand_type_params(&params, callee_ty)?;
                        }
                    }

                    let ctx = Ctx {
                        preserve_params: true,
                        ..analyzer.ctx
                    };
                    callee_ty = analyzer
                        .with_ctx(ctx)
                        .expand_fully(span, callee_ty, false)?;

                    let arg_types = analyzer.validate_args(args)?;
                    let spread_arg_types = analyzer.spread_args(&arg_types);

                    let expanded_ty = analyzer.extract(
                        span,
                        callee_ty,
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

    fn check_type_element_for_call(
        &mut self,
        kind: ExtractKind,
        candidates: &mut Vec<MethodSignature>,
        m: &TypeElement,
        prop: &RExpr,
        computed: bool,
    ) {
        match m {
            TypeElement::Method(m) if kind == ExtractKind::Call => {
                // We are interested only on methods named `prop`
                if is_key_eq_prop(prop, computed, &m.key) {
                    candidates.push(m.clone());
                }
            }

            TypeElement::Property(p) if kind == ExtractKind::Call => {
                if is_key_eq_prop(prop, computed, &p.key) {
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
                            computed: p.computed,
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
                                computed,
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
        members: &[TypeElement],
        prop: &RExpr,
        computed: bool,
        args: &[RExprOrSpread],
    ) -> ValidationResult {
        // Candidates of the method call.
        //
        // 4 is just an unscientific guess
        // TODO: Use smallvec
        let mut candidates = Vec::with_capacity(4);

        for m in members {
            self.check_type_element_for_call(kind, &mut candidates, m, prop, computed);
        }

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
                self.check_type_element_for_call(kind, &mut candidates, m, prop, computed);
            }
        }

        match candidates.len() {
            0 => Err(Error::Unimplemented {
                span,
                msg: format!(
                    "no method with same name\nMembers: {:?}\nKey: {:?}",
                    members, prop
                ),
            }),
            1 => {
                let arg_types = self.validate_args(args)?;
                // TODO:
                return self.check_method_call(
                    span,
                    &candidates.into_iter().next().unwrap(),
                    args,
                    &arg_types,
                );
            }
            _ => {
                let arg_types = self.validate_args(args)?;

                //
                for c in candidates {
                    if c.params.len() == args.len() {
                        return self.check_method_call(span, &c, args, &arg_types);
                    }
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
                            new_arg_types.extend(
                                arg_ty
                                    .elems
                                    .iter()
                                    .map(|element| &element.ty)
                                    .cloned()
                                    .map(|ty| TypeOrSpread {
                                        span: arg.spread.unwrap(),
                                        spread: None,
                                        ty,
                                    }),
                            );
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

                        _ => unimplemented!(
                            "spread argument with type other than tuple\nType: {:#?}",
                            arg.ty
                        ),
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
        ty: Box<Type>,
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        match ty.normalize() {
            Type::Ref(..) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };
                let ty = self.with_ctx(ctx).expand_fully(span, ty, true)?;
                return self.extract(span, ty, kind, args, arg_types, spread_arg_types, type_args);
            }
            _ => {}
        }

        match kind {
            ExtractKind::New => match ty.normalize() {
                Type::Class(ref cls) => {
                    if let Some(type_params) = &cls.type_params {
                        for param in &type_params.params {
                            self.scope
                                .types
                                .entry(param.name.clone())
                                .or_default()
                                .push(Type::Param(param.clone()).cheap());
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
                                &Type::Keyword(RTsKeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                }),
                            )?;

                            for (id, ty) in &inferred {
                                print_type(&self.logger, &format!("{}", id), &self.cm, &ty);
                            }

                            let type_args =
                                self.instantiate(span, &type_params.params, inferred)?;

                            return Ok(box Type::ClassInstance(ClassInstance {
                                span,
                                ty: box Type::Class(cls.clone()),
                                type_args: Some(type_args),
                            }));
                        }
                    }

                    return Ok(box Type::ClassInstance(ClassInstance {
                        span,
                        ty: box Type::Class(cls.clone()),
                        type_args: type_args.cloned(),
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
                    ExtractKind::Call => return Err(Error::NoCallSignature { span, callee: ty }),
                    ExtractKind::New => return Err(Error::NoNewSignature { span, callee: ty }),
                }
            }};
        }

        match ty.normalize() {
            Type::Intersection(..) if kind == ExtractKind::New => {
                // TODO: Check if all types has constructor signature
                return Ok(box Type::ClassInstance(ClassInstance {
                    span,
                    ty: instantiate_class(self.ctx.module_id, ty),
                    type_args: type_args.cloned(),
                }));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(Type::any(span)),

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span }),

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
            Type::Union(..) => self.get_best_return_type(
                span,
                ty,
                kind,
                type_args.cloned(),
                args,
                spread_arg_types,
                arg_types,
            ),

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
                            let parent = self.type_of_ts_entity_name(
                                span,
                                self.ctx.module_id,
                                &parent.expr,
                                type_args.cloned(),
                            )?;

                            if let Ok(v) = self.extract(
                                span,
                                parent,
                                kind,
                                args,
                                arg_types,
                                spread_arg_types,
                                type_args,
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
                    type_args: type_args.cloned(),
                }
                .into());
            }

            Type::Query(QueryType {
                expr: QueryExpr::TsEntityName(RTsEntityName::Ident(RIdent { ref sym, .. })),
                ..
            }) => {
                //if self.scope.find_declaring_fn(sym) {
                //    return Ok(Type::any(span));
                //}

                ret_err!();
            }

            _ => ret_err!(),
        }
    }

    /// Search for members and returns if there's a match
    #[inline(never)]
    fn search_members_for_extract(
        &mut self,
        span: Span,
        ty: &Type,
        members: &[TypeElement],
        kind: ExtractKind,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        let ty_span = ty.span();

        for member in members {
            match *member {
                TypeElement::Call(CallSignature {
                    ref params,
                    ref type_params,
                    ref ret_ty,
                    ..
                }) if kind == ExtractKind::Call => {
                    //
                    match self.get_return_type(
                        span,
                        kind,
                        type_params.as_ref().map(|v| &*v.params),
                        params,
                        ret_ty.clone().unwrap_or(Type::any(span)),
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                    ) {
                        Ok(v) => return Ok(v),
                        Err(..) => {}
                    };
                }

                TypeElement::Constructor(ConstructorSignature {
                    ref params,
                    ref type_params,
                    ref ret_ty,
                    ..
                }) if kind == ExtractKind::New => {
                    match self.get_return_type(
                        span,
                        kind,
                        type_params.as_ref().map(|v| &*v.params),
                        params,
                        ret_ty.clone().unwrap_or(Type::any(span)),
                        type_args,
                        args,
                        arg_types,
                        spread_arg_types,
                    ) {
                        Ok(v) => return Ok(v),
                        Err(..) => {
                            // TODO: Handle error
                        }
                    }
                }
                _ => {}
            }
        }

        dbg!();

        match kind {
            ExtractKind::Call => Err(Error::NoCallSignature {
                span,
                callee: box ty.clone(),
            }),
            ExtractKind::New => Err(Error::NoNewSignature {
                span,
                callee: box ty.clone(),
            }),
        }
    }

    fn check_method_call(
        &mut self,
        span: Span,
        c: &MethodSignature,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        // Validate arguments
        for (i, p) in c.params.iter().enumerate() {
            // TODO: Handle spread
            // TODO: Validate optional parameters
            if arg_types.len() > i {
                let args_ty = &arg_types[i].ty;
                if let Err(..) = self.assign(&p.ty, &args_ty, arg_types[i].span()) {
                    continue;
                }
            }
        }

        return Ok(c.ret_ty.clone().unwrap_or_else(|| Type::any(span)));
    }

    fn get_best_return_type(
        &mut self,
        span: Span,
        callee: Box<Type>,
        kind: ExtractKind,
        type_args: Option<TypeParamInstantiation>,
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

        for callee in callee.normalize().iter_union() {
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
                    let type_params = f.type_params.as_ref().map(|v| &*v.params);
                    if !has_spread || cnt == 1 {
                        if cnt == 1
                            || match self.is_exact_call(
                                span,
                                type_params,
                                &f.params,
                                type_args.as_ref(),
                                &arg_types,
                            ) {
                                Ok(true) => true,
                                _ => false,
                            }
                        {
                            let ret_ty = self.get_return_type(
                                span,
                                kind,
                                type_params,
                                &f.params,
                                f.ret_ty.clone(),
                                type_args.as_ref(),
                                args,
                                &arg_types,
                                spread_arg_types,
                            )?;
                            return Ok(ret_ty);
                        }
                    }

                    candidates.push((
                        span,
                        f.type_params.as_ref().map(|v| &*v.params),
                        &f.params,
                        f.ret_ty.clone(),
                        type_args.as_ref(),
                        &arg_types,
                    ));
                }
                Type::Class(ref cls) if kind == ExtractKind::New => {
                    // TODO: Handle type parameters.
                    return Ok(box Type::ClassInstance(ClassInstance {
                        span,
                        ty: box Type::Class(cls.clone()),
                        type_args,
                    }));
                }
                _ => {}
            }
        }

        if candidates.is_empty() {
            dbg!();

            return Err(if kind == ExtractKind::Call {
                Error::NoCallSignature { span, callee }
            } else {
                Error::NoNewSignature { span, callee }
            });
        }

        if has_spread {
            if let Some((span, type_params, params, ret_ty, type_args, arg_types)) = candidates
                .iter()
                .find(|(_, _, params, _, _, _)| {
                    params.iter().any(|param| match param.pat {
                        RPat::Rest(..) => true,
                        _ => false,
                    })
                })
                .cloned()
            {
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
        }

        let (span, type_params, params, ret_ty, type_args, arg_types) =
            candidates.into_iter().next().unwrap();

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

    /// Returns the return type of function.
    fn get_return_type(
        &mut self,
        span: Span,
        kind: ExtractKind,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        ret_ty: Box<Type>,
        type_args: Option<&TypeParamInstantiation>,
        args: &[RExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        let logger = self.logger.clone();

        slog::debug!(
            logger,
            "get_return_type: \ntype_params = {:?}\nret_ty = {:?}",
            type_params,
            ret_ty
        );

        if let Some(type_params) = type_params {
            let analyzer = self;

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

                for param in type_params {
                    slog::info!(
                        analyzer.logger,
                        "({}) Defining {}",
                        analyzer.scope.depth(),
                        param.name
                    );

                    analyzer
                        .scope
                        .types
                        .entry(param.name.clone())
                        .or_default()
                        .push(Type::Param(param.clone()).cheap());
                }

                if let Some(type_param_decl) = type_param_decl {
                    for param in &type_param_decl.params {
                        analyzer
                            .scope
                            .types
                            .entry(param.name.clone())
                            .or_default()
                            .push(Type::Param(param.clone()).cheap());
                    }
                }

                let mut patch_arg = |idx: usize, pat: &RPat| -> ValidationResult<()> {
                    let actual = &actual_params[idx];

                    let default_any_ty: Option<_> = try {
                        let node_id = pat.node_id()?;
                        analyzer
                            .mutations
                            .as_ref()?
                            .for_pats
                            .get(&node_id)?
                            .ty
                            .clone()?
                    };
                    if let Some(ty) = default_any_ty {
                        match &*ty {
                            Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                            }) if analyzer.is_implicitly_typed_span(*span) => {
                                // TODO: Make this eficient
                                let new_ty =
                                    RTsType::from(actual.ty.clone()).validate_with(analyzer)?;
                                if let Some(node_id) = pat.node_id() {
                                    if let Some(m) = &mut analyzer.mutations {
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

                        slog::info!(
                            analyzer.logger,
                            "Inferring type of arrow expr with updated type"
                        );
                        box Type::Function(arrow.validate_with(analyzer)?)
                    }
                    RExpr::Fn(fn_expr) => {
                        for (idx, param) in fn_expr.function.params.iter().enumerate() {
                            patch_arg(idx, &param.pat)?;
                        }

                        slog::info!(
                            analyzer.logger,
                            "Inferring type of function expr with updated type"
                        );
                        box Type::Function(fn_expr.function.validate_with(analyzer)?)
                    }
                    _ => arg_ty.ty.clone(),
                };
                print_type(&logger, "mapped", &analyzer.cm, &ty);

                let new_arg = TypeOrSpread {
                    ty,
                    ..arg_ty.clone()
                };

                new_args.push(new_arg);
            }
            // if arg.len() > param.len(), we need to add all args
            if arg_types.len() > params.len() {
                new_args.extend(arg_types[params.len()..].iter().cloned());
            }

            let mut new_arg_types;
            let spread_arg_types = if new_args.iter().any(|arg| arg.spread.is_some()) {
                new_arg_types = vec![];
                for arg in &new_args {
                    if arg.spread.is_some() {
                        match &*arg.ty {
                            Type::Tuple(arg_ty) => {
                                new_arg_types.extend(
                                    arg_ty.elems.iter().map(|element| &element.ty).cloned().map(
                                        |ty| TypeOrSpread {
                                            span: arg.spread.unwrap(),
                                            spread: None,
                                            ty,
                                        },
                                    ),
                                );
                            }
                            _ => unimplemented!(
                                "spread argument with type other than tuple\nType: {:#?}",
                                arg.ty
                            ),
                        }
                    } else {
                        new_arg_types.push(arg.clone());
                    }
                }

                &*new_arg_types
            } else {
                &*new_args
            };

            let ret_ty = analyzer.expand(span, ret_ty)?;

            let inferred = analyzer.infer_arg_types(
                span,
                type_args,
                type_params,
                &params,
                &spread_arg_types,
                &Type::TypeLit(TypeLit {
                    span,
                    members: vec![],
                }),
            )?;

            print_type(&logger, "Return", &analyzer.cm, &ret_ty);
            let mut ty = analyzer.expand_type_params(&inferred, ret_ty)?;
            print_type(&logger, "Return, expanded", &analyzer.cm, &ty);

            ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer });

            print_type(&logger, "Return, simplified", &analyzer.cm, &ty);

            ty = analyzer.simplify(ty);

            print_type(&logger, "Return, simplified again", &analyzer.cm, &ty);

            ty = ty.fold_with(&mut ReturnTypeGeneralizer { analyzer });

            print_type(&logger, "Return, generalized", &analyzer.cm, &ty);

            if kind == ExtractKind::Call {
                analyzer.add_call_facts(params, &args, &mut ty);
            }

            return Ok(ty);
        }

        let mut ret_ty = ret_ty.clone();
        ret_ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });
        if kind == ExtractKind::Call {
            self.add_call_facts(params, &args, &mut ret_ty);
        }
        return Ok(ret_ty);
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
                                            self.store_call_fact_for_var(
                                                var_name.span,
                                                var_name.into(),
                                                &ty,
                                            );
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
            for ty in previous_types
                .into_owned()
                .iter_union()
                .flat_map(|ty| ty.iter_union())
            {
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

    /// This method return [Err] if call is invalid
    fn is_exact_call(
        &self,
        span: Span,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        type_args: Option<&TypeParamInstantiation>,
        arg_types: &[TypeOrSpread],
    ) -> ValidationResult<bool> {
        if self.scope.is_call_arg_count_unknown {
            return Ok(false);
        }

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

        // TODO: Handle spread

        let params_min = params.iter().filter(|param| param.required).count();
        let params_max = params.len();

        if arg_types.len() < params_min || params_max < arg_types.len() {
            return Err(Error::ParameterCountMismatch {
                span,
                min: params_min,
                max: params_max,
                actual: arg_types.len(),
            });
        }

        let mut exact = true;

        for (arg, param) in arg_types.iter().zip(params) {
            match arg.ty.normalize() {
                Type::Union(..) => match param.ty.normalize() {
                    Type::Keyword(..) => {
                        if self.assign(&param.ty, &arg.ty, span).is_ok() {
                            return Ok(true);
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            match param.ty.normalize() {
                Type::Param(..) => {}
                _ => {
                    self.assign(&param.ty, &arg.ty, span)?;
                    if self.assign(&arg.ty, &param.ty, span).is_err() {
                        exact = false;
                    }
                }
            }
        }

        Ok(exact)
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
                if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                    | ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                {
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
                    let obj = a
                        .expand_fully(*span, obj_ty.clone(), true)
                        .report(&mut a.storage);
                    if let Some(obj) = obj {
                        if let Some(actual_ty) = a
                            .access_property(
                                *span,
                                obj,
                                &mut RExpr::Lit(RLit::Str(RStr {
                                    span: lit_span,
                                    value: value.clone(),
                                    has_escape: false,
                                    kind: Default::default(),
                                })),
                                true,
                                TypeOfMode::RValue,
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
                                                type_args: Some(TypeParamInstantiation {
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
