//! Handles new expressions and call expressions.
use super::super::Analyzer;
use crate::{
    analyzer::{
        expr::TypeOfMode,
        props::prop_name_to_expr,
        util::{instantiate_class, ResultExt},
        Ctx, ScopeKind,
    },
    builtin_types,
    debug::{print_backtrace, print_type},
    errors::Error,
    ty,
    ty::{
        CallSignature, ClassInstance, ConstructorSignature, FnParam, Method, MethodSignature,
        QueryExpr, QueryType, Static, Type, TypeElement, TypeOrSpread, TypeParam,
        TypeParamInstantiation,
    },
    util::{is_str_lit_or_union, EqIgnoreSpan, PatExt, TypeEq},
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use fxhash::FxHashMap;
use swc_atoms::js_word;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use swc_ts_types::{
    FoldWith as _, Id, IndexedAccessType, Ref, Symbol, TypeLit, VisitMut, VisitMutWith,
};
use ty::TypeExt;

impl Validate<ExprOrSpread> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeOrSpread>;

    fn validate(&mut self, node: &mut ExprOrSpread) -> Self::Output {
        let span = node.span();
        Ok(TypeOrSpread {
            span,
            spread: node.spread,
            ty: node.expr.validate_with(self)?,
        })
    }
}

impl Validate<CallExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &mut CallExpr) -> ValidationResult {
        self.record(e);

        let CallExpr {
            span,
            ref mut callee,
            ref mut args,
            ref mut type_args,
        } = *e;

        let callee = match callee {
            ExprOrSuper::Super(..) => return Ok(Type::any(span)),
            ExprOrSuper::Expr(callee) => callee,
        };

        // TODO: validate children

        self.with_child(
            ScopeKind::Call,
            Default::default(),
            |analyzer: &mut Analyzer| {
                analyzer.extract_call_new_expr_member(
                    callee,
                    ExtractKind::Call,
                    args,
                    type_args.as_mut(),
                )
            },
        )
    }
}

impl Validate<NewExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &mut NewExpr) -> ValidationResult {
        self.record(e);

        let NewExpr {
            span,
            ref mut callee,
            ref mut args,
            ref mut type_args,
        } = *e;

        // TODO: e.visit_children

        self.with_child(
            ScopeKind::Call,
            Default::default(),
            |analyzer: &mut Analyzer| {
                analyzer.extract_call_new_expr_member(
                    callee,
                    ExtractKind::New,
                    args.as_mut().map(|v| &mut **v).unwrap_or_else(|| &mut []),
                    type_args.as_mut(),
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
        callee: &mut Expr,
        kind: ExtractKind,
        args: &mut [ExprOrSpread],
        type_args: Option<&mut TsTypeParamInstantiation>,
    ) -> ValidationResult {
        let span = callee.span();

        log::debug!("extract_call_new_expr_member");

        match *callee {
            Expr::Ident(ref i) if i.sym == js_word!("require") => {
                if let Some(dep) = self.resolved_import_vars.get(
                    &args
                        .iter()
                        .cloned()
                        .map(|arg| match arg {
                            ExprOrSpread { spread: None, expr } => match *expr {
                                Expr::Lit(Lit::Str(Str { span, value, .. })) => {
                                    Ident::new(value.clone(), span).into()
                                }
                                _ => unimplemented!("dynamic import: require()"),
                            },
                            _ => unimplemented!("error reporting: spread element in require()"),
                        })
                        .next()
                        .unwrap(),
                ) {
                    let dep = dep.clone();
                    unimplemented!("dep: {:#?}", dep);
                }

                // if let Some(Type::Enum(ref e)) = self.scope.find_type(&i.into()) {
                //     return Ok(TsType::TsTypeRef(TsTypeRef {
                //         span,
                //         type_name: TsEntityName::Ident(i.clone()),
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

        let arg_types = self.validate_args(args)?;
        let mut new_arg_types;
        let spread_arg_types =
            if arg_types.iter().any(|arg| arg.spread.is_some()) {
                new_arg_types = vec![];
                for arg in &arg_types {
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
                &*arg_types
            };

        match *callee {
            Expr::Ident(Ident {
                sym: js_word!("Symbol"),
                ..
            }) => {
                // Symbol uses special type
                if !arg_types.is_empty() {
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

            Expr::Member(MemberExpr {
                obj: ExprOrSuper::Expr(ref mut obj),
                ref mut prop,
                computed,
                ..
            }) => {
                let is_key_eq_prop = |e: &Expr| {
                    let tmp;
                    let v = match *e {
                        Expr::Ident(ref i) => {
                            tmp = Id::from(i);
                            &tmp
                        }
                        Expr::Lit(Lit::Str(ref s)) => {
                            tmp = Id::word(s.value.clone());
                            &tmp
                        }
                        _ => return false,
                    };

                    let p = match **prop {
                        Expr::Ident(ref i) => &i.sym,
                        Expr::Lit(Lit::Str(ref s)) if computed => &s.value,
                        _ => return false,
                    };

                    v.as_str() == &**p
                };

                macro_rules! search_members_for_prop {
                    ($members:expr) => {{
                        // Candidates of the method call.
                        //
                        // 4 is just an unscientific guess
                        // TODO: Use smallvec
                        let mut candidates = Vec::with_capacity(4);

                        macro_rules! check {
                            ($m:expr) => {{
                                match $m {
                                    TypeElement::Method(ref m) if kind == ExtractKind::Call => {
                                        // We are interested only on methods named `prop`
                                        if is_key_eq_prop(&m.key) {
                                            candidates.push(m.clone());
                                        }
                                    }

                                    _ => {}
                                }
                            }};
                        }

                        for m in $members {
                            check!(m);
                        }

                        {
                            // Handle methods from `interface Object`
                            let i = builtin_types::get_type(
                                self.libs,
                                span,
                                &Id::word(js_word!("Object")),
                            )
                            .expect("`interface Object` is must");
                            let methods = match *i {
                                Type::Static(Static {
                                    ty: Type::Interface(i),
                                    ..
                                }) => &*i.body,

                                _ => &[],
                            };

                            // TODO: Remove clone
                            for m in methods.into_iter().map(|v| v.clone()) {
                                check!(m);
                            }
                        }

                        match candidates.len() {
                            0 => {
                                unimplemented!("no method with same name\nMembers: {:?}", $members)
                            }
                            1 => {
                                // TODO:
                                return self.check_method_call(
                                    span,
                                    &candidates.into_iter().next().unwrap(),
                                    args,
                                    &arg_types,
                                );
                            }
                            _ => {
                                //
                                for c in candidates {
                                    if c.params.len() == args.len() {
                                        return self.check_method_call(span, &c, args, &arg_types);
                                    }
                                }

                                unimplemented!(
                                    "multiple methods with same name and same number of arguments"
                                )
                            }
                        }
                    }};
                }

                {
                    // Handle toString()
                    macro_rules! handle {
                        () => {{
                            return Ok(box Type::from(TsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                            }));
                        }};
                    }
                    match **prop {
                        Expr::Ident(Ident {
                            sym: js_word!("toString"),
                            ..
                        }) if !computed => handle!(),
                        Expr::Lit(Lit::Str(Str {
                            value: js_word!("toString"),
                            ..
                        })) => handle!(),

                        _ => {}
                    }
                }

                // Handle member expression
                let obj_type = self.validate(obj)?.generalize_lit().into_owned();

                let obj_type = match *obj_type.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => builtin_types::get_type(self.libs, span, &Id::word(js_word!("Number")))
                        .expect("Builtin type named 'Number' should exist"),
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) => builtin_types::get_type(self.libs, span, &Id::word(js_word!("String")))
                        .expect("Builtin type named 'String' should exist"),
                    _ => box obj_type,
                };

                match *obj_type.normalize() {
                    Type::Function(ref f) if kind == ExtractKind::Call => {
                        //
                        return Ok(f.ret_ty.clone());
                    }

                    Type::Keyword(TsKeywordType {
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
                                        .push(box Type::Param(param.clone()));
                                }
                            }
                        }
                        // TODO: Check parent interface
                        search_members_for_prop!(i.body.iter());
                    }

                    Type::TypeLit(ref t) => {
                        search_members_for_prop!(t.members.iter());
                    }

                    Type::Class(ty::Class { ref body, .. }) => {
                        for member in body.iter() {
                            match *member {
                                ty::ClassMember::Method(Method {
                                    ref key,
                                    ref ret_ty,
                                    ..
                                }) => {
                                    if prop_name_to_expr(key).eq_ignore_span(&*prop) {
                                        return Ok(ret_ty.clone());
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    }) => {
                        if let Ok(ty) =
                            builtin_types::get_type(self.libs, span, &Id::word(js_word!("Symbol")))
                        {
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

                let type_args = match type_args {
                    None => None,
                    Some(v) => {
                        let mut type_args = v.validate_with(self)?;
                        // If user specified type in call / new, preserve it.
                        self.prevent_expansion(&mut type_args);

                        Some(type_args)
                    }
                };

                self.get_best_return_type(
                    span,
                    callee,
                    kind,
                    type_args,
                    args,
                    &arg_types,
                    spread_arg_types,
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
                        Expr::Ident(i) if kind == ExtractKind::New => {
                            let mut ty = Type::Ref(Ref {
                                span: i.span,
                                type_name: TsEntityName::Ident(i.clone()),
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
                        let callee_ty = callee.validate_with(analyzer)?;
                        match *callee_ty.normalize() {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                                ..
                            }) if type_args.is_some() => {
                                analyzer.info.errors.push(Error::TS2347 { span })
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
                            Type::Method(m) => m.type_params.as_ref(),
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

                    let expanded_ty = analyzer.extract(
                        span,
                        callee_ty,
                        kind,
                        args,
                        &arg_types,
                        spread_arg_types,
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

    fn extract(
        &mut self,
        span: Span,
        ty: Box<Type>,
        kind: ExtractKind,
        args: &mut [ExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        match kind {
            ExtractKind::New => match ty.normalize() {
                Type::Class(ref cls) => {
                    if let Some(type_params) = &cls.type_params {
                        for param in &type_params.params {
                            self.scope
                                .types
                                .entry(param.name.clone())
                                .or_default()
                                .push(box Type::Param(param.clone()));
                        }

                        // Infer type arguments using constructors.
                        let constructors = cls.body.iter().filter_map(|member| match member {
                            swc_ts_types::ClassMember::Constructor(c) => Some(c),
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
                                &Type::Keyword(TsKeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                }),
                            )?;

                            for (id, ty) in &inferred {
                                print_type(&format!("{}", id), &self.cm, &ty);
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
                    ty: instantiate_class(ty),
                    type_args: type_args.cloned(),
                }));
            }

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(Type::any(span)),

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span }),

            Type::Function(ref f) if kind == ExtractKind::Call => self.get_return_type(
                span,
                f.type_params.as_ref().map(|v| &*v.params),
                &f.params,
                f.ret_ty.clone(),
                type_args,
                args,
                arg_types,
                spread_arg_types,
            ),

            Type::Method(ref f) if kind == ExtractKind::Call => self.get_return_type(
                span,
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
                    Ok(ty) => return Ok(ty),
                    Err(err) => {
                        // TODO: Check parent interface
                        Err(err)?
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
                expr: QueryExpr::TsEntityName(TsEntityName::Ident(Ident { ref sym, .. })),
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
        args: &mut [ExprOrSpread],
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
        args: &mut [ExprOrSpread],
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
        args: &mut [ExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        let cnt = callee.normalize().iter_union().count();

        log::info!("get_best_return_type: {} candidates", cnt);

        // TODO: Calculate return type only if selected
        // This can be done by storing type params, return type, params in the
        // candidates.
        let mut candidates = vec![];

        macro_rules! check {
            ($m:expr) => {{
                let m = $m;

                let type_params = m.type_params.as_ref().map(|v| &*v.params);
                if cnt == 1
                    || match self.is_exact_call(
                        span,
                        type_params,
                        &m.params,
                        type_args.as_ref(),
                        &arg_types,
                    ) {
                        Ok(true) => true,
                        _ => false,
                    }
                {
                    let ret_ty = self.get_return_type(
                        span,
                        type_params,
                        &m.params,
                        m.ret_ty.clone(),
                        type_args.as_ref(),
                        args,
                        &arg_types,
                        spread_arg_types,
                    )?;
                    return Ok(ret_ty);
                }

                candidates.push((
                    span,
                    m.type_params.as_ref().map(|v| &*v.params),
                    &m.params,
                    m.ret_ty.clone(),
                    type_args.as_ref(),
                    &arg_types,
                ));
            }};
        }

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
                Type::Method(ref m) => {
                    check!(m);
                }
                Type::Function(ref f) => {
                    check!(f);
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

        if !candidates.is_empty() {
            let (span, type_params, params, ret_ty, type_args, arg_types) =
                candidates.into_iter().next().unwrap();

            return self.get_return_type(
                span,
                type_params,
                params,
                ret_ty,
                type_args,
                args,
                arg_types,
                spread_arg_types,
            );
        }

        dbg!();

        Err(if kind == ExtractKind::Call {
            Error::NoCallSignature { span, callee }
        } else {
            Error::NoNewSignature { span, callee }
        })
    }

    /// Returns the return type of function.
    fn get_return_type(
        &mut self,
        span: Span,
        type_params: Option<&[TypeParam]>,
        params: &[FnParam],
        ret_ty: Box<Type>,
        type_args: Option<&TypeParamInstantiation>,
        args: &mut [ExprOrSpread],
        arg_types: &[TypeOrSpread],
        spread_arg_types: &[TypeOrSpread],
    ) -> ValidationResult {
        assert_eq!(args.len(), arg_types.len());
        log::debug!(
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
                    Type::Method(f) => (&f.type_params, &f.params),
                    _ => {
                        new_args.push(arg_ty.clone());
                        continue;
                    }
                };

                for param in type_params {
                    log::info!("({}) Defining {}", analyzer.scope.depth(), param.name);

                    analyzer
                        .scope
                        .types
                        .entry(param.name.clone())
                        .or_default()
                        .push(box Type::Param(param.clone()));
                }

                if let Some(type_param_decl) = type_param_decl {
                    for param in &type_param_decl.params {
                        analyzer
                            .scope
                            .types
                            .entry(param.name.clone())
                            .or_default()
                            .push(box Type::Param(param.clone()));
                    }
                }

                let patch_arg = |idx: usize, pat: &mut Pat| {
                    let actual = &actual_params[idx];

                    let ty = pat.get_mut_ty();
                    if let Some(ty) = ty {
                        match ty {
                            TsType::TsKeywordType(TsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                            }) if analyzer.is_implicitly_typed_span(*span) => {
                                *ty = actual.ty.clone().into();
                                return;
                            }
                            _ => {}
                        }
                    }
                };

                let ty = match &mut *arg.expr {
                    Expr::Arrow(arrow) => {
                        for (idx, pat) in arrow.params.iter_mut().enumerate() {
                            patch_arg(idx, pat);
                        }

                        log::info!("Inferring type of arrow expr with updated type");
                        box Type::Function(arrow.validate_with(analyzer)?)
                    }
                    Expr::Fn(fn_expr) => {
                        for (idx, param) in fn_expr.function.params.iter_mut().enumerate() {
                            patch_arg(idx, &mut param.pat)
                        }

                        log::info!("Inferring type of function expr with updated type");
                        box Type::Function(fn_expr.function.validate_with(analyzer)?)
                    }
                    _ => arg_ty.ty.clone(),
                };
                print_type("mapped", &analyzer.cm, &ty);

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

            print_type("Return", &analyzer.cm, &ret_ty);
            let mut ty = analyzer.expand_type_params(&inferred, ret_ty)?;
            print_type("Return, expanded", &analyzer.cm, &ty);

            ty = analyzer.simplify(ty);

            print_type("Return, simplified", &analyzer.cm, &ty);

            ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer });

            print_type("Return, simplified again", &analyzer.cm, &ty);

            ty = ty.fold_with(&mut ReturnTypeGeneralizer { analyzer });

            print_type("Return, generalized", &analyzer.cm, &ty);

            return Ok(ty);
        }

        let mut ret_ty = ret_ty.clone();
        ret_ty.visit_mut_with(&mut ReturnTypeSimplifier { analyzer: self });
        return Ok(ret_ty);
    }

    pub(crate) fn generalize_ret_ty(&self, ty: Box<Type>) -> Box<Type> {
        if self.ctx.generalize_ret_ty {
            ty.generalize_lit()
        } else {
            ty
        }
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
            self.assign(&param.ty, &arg.ty, span)?;
            if self.assign(&arg.ty, &param.ty, span).is_err() {
                exact = false;
            }
        }

        Ok(exact)
    }

    fn validate_args(&mut self, args: &mut [ExprOrSpread]) -> Result<Vec<TypeOrSpread>, Error> {
        let args: Vec<_> = args
            .into_iter()
            .map(|arg| {
                self.validate(arg)
                    .store(&mut self.info.errors)
                    .unwrap_or_else(|| TypeOrSpread {
                        span: arg.span(),
                        spread: arg.spread,
                        ty: Type::any(arg.expr.span()),
                    })
            })
            .collect();

        Ok(args)
    }
}

struct ReturnTypeGeneralizer<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl ty::Fold for ReturnTypeGeneralizer<'_, '_, '_> {
    fn fold_type(&mut self, mut ty: Type) -> Type {
        if !self.analyzer.may_generalize(&ty) {
            return ty;
        }

        ty = ty.fold_children_with(self);

        ty.generalize_lit().into_owned()
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

impl VisitMut for ReturnTypeSimplifier<'_, '_, '_> {
    fn visit_mut_type(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Keyword(TsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                    }),
                ..
            }) => {
                *ty = Type::Keyword(TsKeywordType {
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
                        Type::Lit(TsLitType {
                            span: lit_span,
                            lit: TsLit::Str(Str { value, .. }),
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
                        .store(&mut a.info.errors);
                    if let Some(obj) = obj {
                        if let Some(actual_ty) = a
                            .access_property(
                                *span,
                                obj,
                                &mut Expr::Ident(Ident::new(value.clone(), lit_span)),
                                true,
                                TypeOfMode::RValue,
                            )
                            .store(&mut a.info.errors)
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

            _ => {}
        }
    }
}
