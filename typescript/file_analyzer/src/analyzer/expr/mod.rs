use self::bin::extract_name_for_assignment;
use super::{marks::MarkExt, Analyzer};
use crate::analyzer::util::ResultExt;
use crate::util::type_ext::TypeVecExt;
use crate::util::RemoveTypes;
use crate::{
    analyzer::{pat::PatMode, Ctx},
    ty,
    ty::{
        Array, EnumVariant, IndexSignature, IndexedAccessType, Interface, Intersection, Ref, Tuple, Type, TypeElement,
        TypeLit, TypeParam, TypeParamInstantiation, Union,
    },
    type_facts::TypeFacts,
    util::is_str_lit_or_union,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use optional_chaining::is_obj_opt_chaining;
use rnode::NodeId;
use rnode::VisitWith;
use stc_ts_ast_rnode::RAssignExpr;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RCallExpr;
use stc_ts_ast_rnode::RClassExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RFnExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RNull;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RParenExpr;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPatOrExpr;
use stc_ts_ast_rnode::RSeqExpr;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RSuper;
use stc_ts_ast_rnode::RThisExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsEnumMemberId;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsNonNullExpr;
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_ast_rnode::RUnaryExpr;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_types::name::Name;
use stc_ts_types::Alias;
use stc_ts_types::Class;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassMember;
use stc_ts_types::ComputedKey;
use stc_ts_types::Key;
use stc_ts_types::PropertySignature;
use stc_ts_types::{ClassProperty, Id, Method, ModuleId, Operator, QueryExpr, QueryType, StaticThis};
use std::borrow::Cow;
use std::convert::TryFrom;
use std::convert::TryInto;
use swc_atoms::js_word;
use swc_common::SyntaxContext;
use swc_common::TypeEq;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::op;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::TsTypeOperatorOp;
use ty::TypeExt;

mod array;
mod await_expr;
mod bin;
mod call_new;
mod constraint_reducer;
mod function;
mod object;
mod optional_chaining;
mod type_cast;
mod unary;
mod update;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdCtx {
    Var,
    Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOfMode {
    /// Used for l-values.
    ///
    /// This is used to allow
    ///
    /// ```ts
    /// type Num = { '0': string } | { [n: number]: number }
    /// declare var num: Num
    /// num[0] = 1
    /// num['0'] = 'ok'
    /// ```
    LValue,
    /// Use for r-values.
    RValue,
}

impl Default for TypeOfMode {
    fn default() -> Self {
        Self::RValue
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RExpr,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        self.record(e);

        let span = e.span();
        let need_type_param_handling = match e {
            RExpr::Member(..) => true,
            RExpr::Call(..) | RExpr::New(..) if self.ctx.in_argument => false,
            RExpr::Call(..) | RExpr::New(..) => true,
            _ => false,
        };

        let mut ty = (|| -> ValidationResult {
            match e {
                // super() returns any
                RExpr::Call(RCallExpr {
                    callee: RExprOrSuper::Super(..),
                    ..
                }) => Ok(Type::any(span)),

                RExpr::TaggedTpl(e) => e.validate_with(self),

                RExpr::Bin(e) => e.validate_with_args(self, type_ann),
                RExpr::Cond(e) => e.validate_with_args(self, (mode, type_ann)),
                RExpr::Seq(e) => e.validate_with_args(self, (mode, type_ann)),
                RExpr::Update(e) => e.validate_with(self),
                RExpr::New(e) => e.validate_with_args(self, type_ann),
                RExpr::Call(e) => e.validate_with_args(self, type_ann),
                RExpr::TsAs(e) => e.validate_with_args(self, (mode, type_args, type_ann)),
                RExpr::TsTypeAssertion(e) => e.validate_with_args(self, (mode, type_args, type_ann)),
                RExpr::Assign(e) => e.validate_with_args(self, (mode, type_ann)),
                RExpr::Unary(e) => e.validate_with(self),

                RExpr::This(RThisExpr { span, .. }) => {
                    if !self.scope.is_this_defined() {
                        return Ok(Type::Keyword(RTsKeywordType {
                            span: *span,
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        }));
                    }
                    let span = *span;
                    if let Some(ty) = self.scope.this() {
                        return Ok(ty.into_owned());
                    }
                    return Ok(Type::from(RTsThisType { span }));
                }

                RExpr::Ident(ref i) => {
                    if i.sym == js_word!("undefined") {
                        return Ok(Type::Keyword(RTsKeywordType {
                            span: i.span.with_ctxt(SyntaxContext::empty()),
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        }));
                    }
                    let ty = self.type_of_var(i, mode, type_args)?;
                    if self.ctx.should_store_truthy_for_access && mode == TypeOfMode::RValue {
                        // `i` is truthy
                        self.cur_facts.true_facts.facts.insert(i.into(), TypeFacts::Truthy);
                        self.cur_facts.false_facts.facts.insert(i.into(), TypeFacts::Falsy);
                    }

                    Ok(ty)
                }

                RExpr::Array(arr) => return arr.validate_with_args(self, (mode, type_args, type_ann)),

                RExpr::Lit(RLit::Bool(v)) => {
                    return Ok(Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: v.span,
                        lit: RTsLit::Bool(v.clone()),
                    }));
                }
                RExpr::Lit(RLit::Str(ref v)) => {
                    return Ok(Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: v.span,
                        lit: RTsLit::Str(v.clone()),
                    }));
                }
                RExpr::Lit(RLit::Num(v)) => {
                    return Ok(Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: v.span,
                        lit: RTsLit::Number(v.clone()),
                    }));
                }
                RExpr::Lit(RLit::Null(RNull { span })) => {
                    if self.ctx.in_export_default_expr {
                        // TODO: strict mode
                        return Ok(Type::Keyword(RTsKeywordType {
                            span: *span,
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                        }));
                    }

                    return Ok(Type::Keyword(RTsKeywordType {
                        span: *span,
                        kind: TsKeywordTypeKind::TsNullKeyword,
                    }));
                }
                RExpr::Lit(RLit::Regex(..)) => {
                    return Ok(Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent {
                            node_id: NodeId::invalid(),
                            span,
                            sym: js_word!("RegExp"),
                            optional: false,
                        }),
                        type_args: None,
                    }));
                }

                RExpr::Paren(RParenExpr { ref expr, .. }) => expr.validate_with_args(self, (mode, type_args, type_ann)),

                RExpr::Tpl(ref t) => {
                    // Check if tpl is constant. If it is, it's type is string literal.
                    if t.exprs.is_empty() {
                        return Ok(Type::Lit(RTsLitType {
                            node_id: NodeId::invalid(),
                            span: t.span(),
                            lit: RTsLit::Str(t.quasis[0].cooked.clone().unwrap_or_else(|| t.quasis[0].raw.clone())),
                        }));
                    }

                    return Ok(Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                    }));
                }

                RExpr::TsNonNull(RTsNonNullExpr { ref expr, .. }) => Ok(expr
                    .validate_with_args(self, (mode, type_args, type_ann))?
                    .remove_falsy()),

                RExpr::Object(e) => {
                    return e.validate_with_args(self, type_ann);
                }

                // https://github.com/Microsoft/TypeScript/issues/26959
                RExpr::Yield(..) => {
                    e.visit_children_with(self);
                    return Ok(Type::any(span));
                }

                RExpr::Await(e) => e.validate_with(self),

                RExpr::Class(RClassExpr {
                    ref ident, ref class, ..
                }) => {
                    self.scope.this_class_name = ident.as_ref().map(|i| i.into());
                    return Ok(class.validate_with(self)?.into());
                }

                RExpr::Arrow(ref e) => return Ok(e.validate_with_args(self, type_ann)?.into()),

                RExpr::Fn(RFnExpr { ref function, .. }) => {
                    return Ok(function.validate_with(self)?.into());
                }

                RExpr::Member(ref expr) => {
                    // Foo.a
                    if self.ctx.should_store_truthy_for_access {
                        if let Ok(name) = Name::try_from(&*expr) {
                            self.cur_facts.true_facts.facts.insert(name, TypeFacts::Truthy);
                        }
                    }

                    return self.type_of_member_expr(expr, mode);
                }

                RExpr::MetaProp(..) => unimplemented!("typeof(MetaProp)"),

                RExpr::Invalid(ref i) => return Ok(Type::any(i.span())),

                RExpr::OptChain(expr) => expr.validate_with_args(self, type_ann),

                RExpr::TsConstAssertion(expr) => {
                    if mode == TypeOfMode::RValue {
                        return expr.expr.validate_with_args(self, (mode, None, type_ann));
                    } else {
                        return Err(Error::Unimplemented {
                            span,
                            msg: format!(
                                "Proper error reporting for using const assertion expression in left hand side of an \
                                 assignment expression"
                            ),
                        });
                    }
                }

                _ => unimplemented!("typeof ({:?})", e),
            }
        })()?;

        if need_type_param_handling {
            self.replace_invalid_type_params(&mut ty);
        }

        // Exclude literals
        if !span.is_dummy()
            & match e {
                RExpr::Lit(..) => false,
                _ => true,
            }
        {
            if let Some(debugger) = &self.debugger {
                debugger.dump_type(span, &ty);
            }
        }

        Ok(ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RExprOrSuper) -> ValidationResult {
        match e {
            RExprOrSuper::Expr(e) => e.validate_with_default(self),
            RExprOrSuper::Super(s) => Ok(Type::any(s.span)),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RParenExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> ValidationResult {
        e.expr.validate_with_args(self, (mode, None, type_ann))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAssignExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> ValidationResult {
        let ctx = Ctx {
            pat_mode: PatMode::Assign,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
            let span = e.span();

            let ty_of_left;
            let (any_span, type_ann) = match e.left {
                RPatOrExpr::Pat(box RPat::Ident(RBindingIdent { id: ref i, .. }))
                | RPatOrExpr::Expr(box RExpr::Ident(ref i)) => {
                    // Type is any if self.declaring contains ident
                    let any_span = if analyzer.scope.declaring.contains(&i.into()) {
                        Some(span)
                    } else {
                        None
                    };

                    ty_of_left = analyzer
                        .type_of_var(i, TypeOfMode::LValue, None)
                        .context("tried to get type of lhs of an assignment")?;

                    (any_span, Some(&ty_of_left))
                }

                _ => (None, type_ann),
            };

            let mut errors = Errors::default();

            match &e.left {
                RPatOrExpr::Pat(box RPat::Ident(i)) => {
                    // TODO: Implemennt this
                    let rhs_is_always_true = true;

                    // TODO: Deny changing type of const
                    if rhs_is_always_true {
                        analyzer.mark_var_as_truthy(Id::from(&i.id))?;
                    }
                }
                _ => e.left.visit_with(analyzer),
            }

            let rhs_ty = match {
                let ctx = Ctx {
                    in_assign_rhs: true,
                    ..analyzer.ctx
                };
                let mut analyzer = analyzer.with_ctx(ctx);
                e.right.validate_with_args(&mut *analyzer, (mode, None, type_ann))
            } {
                Ok(rhs_ty) => {
                    analyzer.check_rvalue(span, &rhs_ty);

                    Ok(rhs_ty)
                }
                Err(err) => {
                    errors.push(err);
                    Err(())
                }
            };

            analyzer.storage.report_all(errors);

            let rhs_ty = match rhs_ty {
                Ok(v) => v,
                Err(()) => Type::any(span),
            };

            let ctx = Ctx {
                preserve_params: true,
                preserve_ret_ty: true,
                ..analyzer.ctx
            };
            let rhs_ty = analyzer.with_ctx(ctx).expand_fully(span, rhs_ty.clone(), true)?;
            analyzer.try_assign(span, e.op, &e.left, &rhs_ty);

            if let Some(span) = any_span {
                return Ok(Type::any(span));
            }

            Ok(rhs_ty)
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RSeqExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> ValidationResult {
        let RSeqExpr { span, ref exprs, .. } = *e;

        assert!(exprs.len() >= 1);

        let first_span = exprs[0].span();
        let len = exprs.len();

        let mut is_any = false;
        for (i, e) in exprs.iter().enumerate() {
            let is_last = i == len - 1;

            if !is_last {
                match **e {
                    RExpr::Ident(..)
                    | RExpr::Lit(..)
                    | RExpr::Arrow(..)
                    | RExpr::Unary(RUnaryExpr {
                        op: op!(unary, "-"), ..
                    })
                    | RExpr::Unary(RUnaryExpr {
                        op: op!(unary, "+"), ..
                    })
                    | RExpr::Unary(RUnaryExpr { op: op!("!"), .. })
                    | RExpr::Unary(RUnaryExpr { op: op!("typeof"), .. })
                        if !self.rule().allow_unreachable_code =>
                    {
                        self.storage.report(Error::UselessSeqExpr {
                            span: span.with_lo(first_span.lo()),
                        });
                    }

                    _ => {}
                }
            }
            match **e {
                RExpr::Ident(ref i) => {
                    if self.scope.declaring.contains(&i.into()) {
                        is_any = true;
                    }
                }
                _ => {}
            }

            if !is_last {
                match e.validate_with_default(self) {
                    Ok(..) => {}
                    Err(Error::ReferencedInInit { .. }) => {
                        is_any = true;
                    }
                    Err(..) => {}
                }
            }
        }
        if is_any {
            return Ok(Type::any(span));
        }

        return exprs.last().unwrap().validate_with_args(self, (mode, None, type_ann));
    }
}

impl Analyzer<'_, '_> {
    pub(crate) fn validate_key(&mut self, prop: &RExpr, computed: bool) -> ValidationResult<Key> {
        if computed {
            match prop {
                RExpr::Lit(RLit::Str(s)) => {
                    return Ok(Key::Normal {
                        span: s.span,
                        sym: s.value.clone(),
                    })
                }
                RExpr::Lit(RLit::Num(n)) => return Ok(Key::Num(n.clone())),
                _ => {}
            }

            prop.validate_with_default(self)
                .and_then(|ty| self.expand_top_ref(ty.span(), Cow::Owned(ty)).map(Cow::into_owned))
                .and_then(|ty| self.expand_enum(ty))
                .and_then(|ty| self.expand_enum_variant(ty))
                .map(|ty| ComputedKey {
                    span: prop.span(),
                    ty: box ty,
                    expr: box prop.clone(),
                })
                .map(Key::Computed)
        } else {
            match prop {
                RExpr::Ident(RIdent { span, sym, .. }) => Ok(Key::Normal {
                    span: *span,
                    sym: sym.clone(),
                }),

                // The code below is valid typescript.
                //
                // interface AbortSignalEventMap {
                //     "abort": Event;
                // }
                RExpr::Lit(RLit::Str(s)) => Ok(Key::Normal {
                    span: s.span,
                    sym: s.value.clone(),
                }),

                RExpr::Lit(RLit::Num(n)) => Ok(Key::Num(n.clone())),
                RExpr::Lit(RLit::BigInt(n)) => Ok(Key::BigInt(n.clone())),
                _ => unreachable!("non-computed-key: {:?}", prop),
            }
        }
    }

    /// Check if key matches.
    ///
    /// # Parameters
    ///
    /// - `declared`: Key of declared property.
    pub(crate) fn key_matches(&mut self, span: Span, declared: &Key, cur: &Key, allow_union: bool) -> bool {
        match (declared, cur) {
            (
                Key::Num(RNumber {
                    value: declared_value, ..
                }),
                Key::Num(RNumber { value, .. }),
            ) => {
                if declared_value == value {
                    return true;
                }
            }
            (Key::Num(RNumber { value, .. }), Key::Normal { sym, .. }) => {
                if value.is_infinite() {
                    if *sym == *"Infinity" {
                        return true;
                    }
                    let parsed = sym.parse::<f64>();
                    if let Ok(v) = parsed {
                        if v.is_infinite() {
                            return true;
                        }
                    }
                }
                if *sym == *value.to_string() {
                    return true;
                }
            }
            _ => {}
        }

        match cur {
            Key::Computed(cur) => {
                if self.check_if_type_matches_key(span, declared, &cur.ty, true) {
                    return true;
                }
            }
            _ => {}
        }

        self.assign(&declared.ty(), &cur.ty(), span).is_ok()
    }

    fn check_if_type_matches_key(&mut self, span: Span, declared: &Key, key_ty: &Type, allow_union: bool) -> bool {
        let key_ty = key_ty.normalize();

        if declared.ty().type_eq(key_ty) {
            return true;
        }

        match key_ty {
            Type::Ref(..) => {
                let cur = self.expand_top_ref(span, Cow::Borrowed(key_ty));
                if let Ok(cur) = cur {
                    return self.check_if_type_matches_key(span, declared, &cur, allow_union);
                }
            }
            Type::Enum(e) if allow_union => {
                //
                for m in &e.members {
                    match &*m.val {
                        RExpr::Lit(RLit::Str(s)) => {
                            if self.key_matches(
                                span,
                                declared,
                                &Key::Normal {
                                    span: s.span,
                                    sym: s.value.clone(),
                                },
                                true,
                            ) {
                                return true;
                            }
                        }
                        _ => {}
                    }
                }
            }
            Type::Union(u) if allow_union => {
                return u
                    .types
                    .iter()
                    .any(|ty| self.check_if_type_matches_key(span, declared, &ty, true))
            }

            _ => {}
        }

        false
    }

    fn access_property_of_type_elements(
        &mut self,
        span: Span,
        obj: &Type,
        prop: &Key,
        type_mode: TypeOfMode,
        members: &[TypeElement],
    ) -> ValidationResult<Option<Type>> {
        let mut matching_elements = vec![];
        for el in members.iter() {
            if let Some(key) = el.key() {
                if self.key_matches(span, key, prop, true) {
                    match el {
                        TypeElement::Property(ref p) => {
                            if type_mode == TypeOfMode::LValue && p.readonly {
                                return Err(Error::ReadOnly { span });
                            }

                            if let Some(ref type_ann) = p.type_ann {
                                if p.optional {
                                    let mut types = vec![Type::undefined(span), *type_ann.clone()];
                                    types.dedup_type();
                                    matching_elements.push(Type::union(types));
                                } else {
                                    matching_elements.push(*type_ann.clone());
                                }
                                continue;
                            }

                            // TODO: no implicit any?
                            matching_elements.push(Type::any(span));
                            continue;
                        }

                        TypeElement::Method(ref m) => {
                            //
                            matching_elements.push(Type::Function(ty::Function {
                                span,
                                type_params: m.type_params.clone(),
                                params: m.params.clone(),
                                ret_ty: m.ret_ty.clone().unwrap_or_else(|| box Type::any(span)),
                            }));
                            continue;
                        }

                        _ => unimplemented!("TypeElement {:?}", el),
                    }
                }
            }
        }

        let is_callable = members.iter().any(|element| match element {
            TypeElement::Call(_) => true,
            _ => false,
        });

        if is_callable {
            // Handle funciton-like interfaces
            // Example of code handled by this block is `Error.call`

            let obj = self.env.get_global_type(span, &js_word!("Function"))?;

            if let Ok(v) = self.access_property(span, obj, prop, type_mode, IdCtx::Var) {
                return Ok(Some(v));
            }
        }

        if matching_elements.len() == 1 {
            return Ok(matching_elements.pop());
        }

        for el in members.iter() {
            if prop.is_computed() {}
            match el {
                TypeElement::Index(IndexSignature {
                    ref params,
                    ref type_ann,
                    readonly,
                    ..
                }) => {
                    if params.len() != 1 {
                        unimplemented!("Index signature with multiple parameters")
                    }

                    let index_ty = &params[0].ty;

                    let prop_ty = prop.ty();

                    // Don't know exact reason, but you can index `{ [x: string]: boolean }`
                    // with number type.
                    //
                    // I guess it's because javascript work in that way.
                    let indexed = (index_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                        && prop_ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword))
                        || self.assign(&index_ty, &prop_ty, span).is_ok();

                    if indexed {
                        if let Some(ref type_ann) = type_ann {
                            let ty = self.expand_top_ref(span, Cow::Borrowed(type_ann))?;
                            return Ok(Some(ty.into_owned()));
                        }

                        return Ok(Some(Type::any(span)));
                    }

                    match prop_ty.normalize() {
                        // TODO: Only string or number
                        Type::EnumVariant(..) => {
                            matching_elements.extend(type_ann.clone().map(|v| *v));
                            continue;
                        }

                        _ => {}
                    }

                    // This check exists to prefer a specific property over generic index signature.
                    if prop.is_computed() || matching_elements.is_empty() {
                        let ty = Type::IndexedAccessType(IndexedAccessType {
                            span,
                            obj_type: box obj.clone(),
                            index_type: box prop_ty.into_owned(),
                            readonly: *readonly,
                        });

                        return Ok(Some(ty));
                    }
                }
                _ => {}
            }
        }

        if matching_elements.len() == 0 {
            return Ok(None);
        }

        Ok(Some(Type::union(matching_elements)))
    }

    pub(super) fn access_property(
        &mut self,
        span: Span,
        obj: Type,
        prop: &Key,
        type_mode: TypeOfMode,
        id_ctx: IdCtx,
    ) -> ValidationResult {
        if !self.is_builtin {
            debug_assert_ne!(span, DUMMY_SP, "access_property: called with a dummy span");
        }

        // We use child scope to store type parameters.
        let mut ty = self.with_scope_for_type_params(|analyzer: &mut Analyzer| -> ValidationResult<_> {
            let mut ty = analyzer.access_property_inner(span, obj, prop, type_mode, id_ctx)?;
            ty = analyzer.expand_type_params_using_scope(ty)?;
            Ok(ty)
        })?;

        if !self.is_builtin && ty.span().is_dummy() && !span.is_dummy() {
            ty.reposition(span);
        }

        Ok(ty)
    }
    fn access_property_inner(
        &mut self,
        span: Span,
        obj: Type,
        prop: &Key,
        type_mode: TypeOfMode,
        id_ctx: IdCtx,
    ) -> ValidationResult {
        if !self.is_builtin {
            debug_assert!(!span.is_dummy());

            slog::debug!(&self.logger, "access_property");
        }

        let computed = prop.is_computed();

        if id_ctx == IdCtx::Var {
            // Recursive method call
            if !computed
                && obj.is_this()
                && (self.scope.is_this_ref_to_object_lit() || self.scope.is_this_ref_to_class())
            {
                if let Some(declaring) = &self.scope.declaring_prop() {
                    if prop == declaring.sym() {
                        return Ok(Type::any(span));
                    }
                }
            }

            match &obj {
                Type::This(this) if self.scope.is_this_ref_to_object_lit() => {
                    if let Key::Computed(prop) = prop {
                        //
                        match &*prop.expr {
                            RExpr::Cond(..) => {
                                return Ok(Type::any(span));
                            }
                            _ => {}
                        }
                    }

                    // TODO: Remove clone
                    let members = self.scope.object_lit_members().to_vec();
                    if let Some(mut v) = self.access_property_of_type_elements(span, &obj, prop, type_mode, &members)? {
                        self.marks().infected_by_this_in_object_literal.apply_to_type(&mut v);
                        return Ok(v);
                    }
                }

                Type::This(this) if self.scope.is_this_ref_to_class() => {
                    if !computed {
                        // We are currently declaring a class.
                        for (_, member) in self.scope.class_members() {
                            match member {
                                // No-op, as constructor parameter properties are handled by
                                // Validate<Class>.
                                ClassMember::Constructor(_) => {}

                                ClassMember::Method(member @ Method { is_static: false, .. })
                                    if member.key.type_eq(prop) =>
                                {
                                    return Ok(Type::Function(ty::Function {
                                        span: member.span,
                                        type_params: member.type_params.clone(),
                                        params: member.params.clone(),
                                        ret_ty: member.ret_ty.clone(),
                                    }));
                                }

                                ClassMember::Property(member @ ClassProperty { is_static: false, .. }) => {
                                    if member.key.type_eq(prop) {
                                        return Ok(*member.value.clone().unwrap_or_else(|| box Type::any(span)));
                                    }
                                }

                                ClassMember::Property(..) | ClassMember::Method(..) => {}

                                ClassMember::IndexSignature(_) => {
                                    unimplemented!("class -> this.foo where an `IndexSignature` exists")
                                }
                            }
                        }

                        if let Some(super_class) = self.scope.get_super_class() {
                            let super_class = super_class.clone();
                            let ctx = Ctx {
                                preserve_ref: false,
                                ignore_expand_prevention_for_top: true,
                                ..self.ctx
                            };
                            let super_class = self.with_ctx(ctx).expand_fully(span, super_class, true)?;

                            if let Ok(v) = self.access_property(span, super_class, prop, type_mode, IdCtx::Var) {
                                return Ok(v);
                            }
                        }

                        return Err(Error::NoSuchPropertyInClass {
                            span,
                            class_name: self.scope.get_this_class_name(),
                            prop: prop.clone(),
                        });
                    }

                    if let Some(super_class) = self.scope.get_super_class() {
                        let super_class = super_class.clone();
                        let ctx = Ctx {
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ..self.ctx
                        };
                        let super_class = self.with_ctx(ctx).expand_fully(span, super_class, true)?;

                        if let Ok(v) = self.access_property(span, super_class, prop, type_mode, IdCtx::Var) {
                            return Ok(v);
                        }
                    }

                    let prop_ty = prop.clone().computed().unwrap().ty;

                    // TODO: Handle string literals like
                    //
                    // `this['props']`
                    return Ok(Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly: false,
                        obj_type: box Type::This(this.clone()),
                        index_type: prop_ty,
                    }));
                }

                Type::StaticThis(StaticThis { span }) => {
                    // Handle static access to class itself while *declaring* the class.
                    for (_, member) in self.scope.class_members() {
                        match member {
                            stc_ts_types::ClassMember::Method(member @ Method { is_static: true, .. }) => {
                                if member.key.type_eq(prop) {
                                    return Ok(Type::Function(ty::Function {
                                        span: member.span,
                                        type_params: member.type_params.clone(),
                                        params: member.params.clone(),
                                        ret_ty: member.ret_ty.clone(),
                                    }));
                                }
                            }

                            stc_ts_types::ClassMember::Property(property @ ClassProperty { is_static: true, .. }) => {
                                if property.key.type_eq(prop) {
                                    return Ok(*property.value.clone().unwrap_or_else(|| box Type::any(span.clone())));
                                }
                            }

                            _ => {}
                        }
                    }

                    dbg!();

                    return Err(Error::NoSuchProperty {
                        span: *span,
                        obj: Some(box obj.clone()),
                        prop: Some(box prop.clone()),
                    });
                }

                _ => {}
            }
        }

        let ctx = Ctx {
            preserve_ref: false,
            ignore_expand_prevention_for_top: true,
            ignore_expand_prevention_for_all: false,
            preserve_params: true,
            ..self.ctx
        };
        let obj = self.with_ctx(ctx).expand(span, obj)?.generalize_lit();

        match obj.normalize() {
            Type::Lit(..) => unreachable!(),

            Type::Enum(ref e) => {
                // TODO: Check if variant exists.

                match prop {
                    Key::Normal { sym, .. } => {
                        let has_such_member = e.members.iter().any(|m| match &m.id {
                            RTsEnumMemberId::Ident(i) => i.sym == *sym,
                            RTsEnumMemberId::Str(s) => s.value == *sym,
                        });
                        if !has_such_member {
                            return Err(Error::NoSuchEnumVariant {
                                span,
                                name: sym.clone(),
                            });
                        }

                        // Computed values are not permitted in an enum with string valued members.
                        if e.is_const && type_mode == TypeOfMode::RValue {
                            // for m in &e.members {
                            //     match m.id {
                            //         TsEnumMemberId::Ident(Ident { ref sym, ..
                            // })
                            //         | TsEnumMemberId::Str(Str { value: ref
                            // sym, .. }) => {
                            //             if sym == $sym {
                            //                 return Ok(Type::Lit(RTsLitType {
                            //                     span: m.span(),
                            //                     lit: match m.val.clone() {
                            //                         RExpr::Lit(RLit::Str(s))
                            // => RTsLit::Str(s),
                            // RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                            // _ => unreachable!(),                     },
                            //                 }));
                            //             }
                            //         }
                            //     }
                            // }
                        }

                        if e.is_const && computed {
                            // return Err(Error::ConstEnumNonIndexAccess { span:
                            // prop.span() });
                        }

                        if type_mode == TypeOfMode::LValue {
                            return Err(Error::EnumCannotBeLValue { span: prop.span() });
                        }

                        return Ok(Type::EnumVariant(EnumVariant {
                            span: match type_mode {
                                TypeOfMode::LValue => prop.span(),
                                TypeOfMode::RValue => span,
                            },
                            ctxt: self.ctx.module_id,
                            enum_name: e.id.clone().into(),
                            name: sym.clone(),
                        }));
                    }
                    Key::Num(RNumber { value, .. }) => {
                        let idx = value.round() as usize;
                        if e.members.len() > idx {
                            let v = &e.members[idx];
                            if match *v.val {
                                RExpr::Lit(RLit::Str(..)) | RExpr::Lit(RLit::Num(..)) => true,
                                _ => false,
                            } {
                                let new_obj_ty = Type::Lit(RTsLitType {
                                    node_id: NodeId::invalid(),
                                    span,
                                    lit: match *v.val.clone() {
                                        RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                        RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                                        _ => unreachable!(),
                                    },
                                });
                                return self.access_property(span, new_obj_ty, prop, type_mode, id_ctx);
                            }
                        }
                        return Ok(Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }

                    _ => {
                        if e.is_const {
                            return Err(Error::ConstEnumNonIndexAccess { span: prop.span() });
                        }

                        // TODO: Validate type of enum

                        // enumBasics.ts says
                        //
                        // Reverse mapping of enum returns string name of property
                        return Ok(Type::Keyword(RTsKeywordType {
                            span: prop.span(),
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }
                }
            }

            // enum Foo { A }
            //
            // Foo.A.toString()
            Type::EnumVariant(EnumVariant {
                ctxt,
                ref enum_name,
                ref name,
                span,
                ..
            }) => match self.find_type(*ctxt, enum_name)? {
                Some(types) => {
                    //
                    for ty in types {
                        match ty.normalize() {
                            Type::Enum(ref e) => {
                                for v in e.members.iter() {
                                    if match *v.val {
                                        RExpr::Lit(RLit::Str(..)) | RExpr::Lit(RLit::Num(..)) => true,
                                        _ => false,
                                    } {
                                        let new_obj_ty = Type::Lit(RTsLitType {
                                            node_id: NodeId::invalid(),
                                            span: *span,
                                            lit: match *v.val.clone() {
                                                RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                                RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                                                _ => unreachable!(),
                                            },
                                        });
                                        return self.access_property(*span, new_obj_ty, prop, type_mode, id_ctx);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => unreachable!("Enum named {} does not exist", enum_name),
            },

            Type::Class(ref c) => {
                for v in c.def.body.iter() {
                    match v {
                        ClassMember::Property(ref class_prop) => {
                            if let Some(declaring) = self.scope.declaring_prop.as_ref() {
                                if class_prop.key == *declaring.sym() {
                                    return Err(Error::ReferencedInInit { span });
                                }
                            }

                            //
                            if self.key_matches(span, &class_prop.key, &prop, false) {
                                return Ok(match class_prop.value {
                                    Some(ref ty) => *ty.clone(),
                                    None => Type::any(span),
                                });
                            }
                        }
                        ClassMember::Method(ref mtd) => {
                            if self.key_matches(span, &mtd.key, prop, false) {
                                return Ok(Type::Function(stc_ts_types::Function {
                                    span: mtd.span,
                                    type_params: mtd.type_params.clone(),
                                    params: mtd.params.clone(),
                                    ret_ty: mtd.ret_ty.clone(),
                                }));
                            }
                        }

                        ClassMember::Constructor(ref cons) => {
                            if prop == "constructor" {
                                return Ok(Type::Constructor(ty::Constructor {
                                    span,
                                    type_params: cons.type_params.clone(),
                                    params: cons.params.clone(),
                                    type_ann: cons.ret_ty.clone().unwrap_or_else(|| box obj.clone()),
                                    is_abstract: false,
                                }));
                            }
                        }

                        ClassMember::IndexSignature(index) => {
                            if index.params.len() == 1 {
                                if let Ok(()) = self.assign(&index.params[0].ty, &prop.ty(), span) {
                                    return Ok(index.type_ann.clone().map(|v| *v).unwrap_or_else(|| Type::any(span)));
                                }
                            }
                        }
                    }
                }

                // check for super class
                if let Some(super_ty) = &c.def.super_class {
                    let super_ty = self.instantiate_class(span, &super_ty)?;

                    if let Ok(v) = self.access_property(span, super_ty, prop, type_mode, id_ctx) {
                        return Ok(v);
                    }
                }

                return Err(Error::NoSuchPropertyInClass {
                    span,
                    class_name: c.def.name.clone(),
                    prop: prop.clone(),
                });
            }

            Type::Param(TypeParam {
                span: p_span,
                name,
                constraint,
                ..
            }) => {
                {
                    // Check for `T extends { a: { x: any } }`
                    match constraint {
                        Some(constraint) => {
                            let ctx = Ctx {
                                ignore_errors: true,
                                ..self.ctx
                            };
                            if let Ok(ty) =
                                self.with_ctx(ctx)
                                    .access_property(span, *constraint.clone(), prop, type_mode, id_ctx)
                            {
                                return Ok(ty);
                            }
                        }
                        _ => {}
                    }
                }

                let mut prop_ty = match prop {
                    Key::Computed(key) => key.ty.clone(),
                    Key::Normal { span, sym } => box Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: *span,
                        lit: RTsLit::Str(RStr {
                            span: *span,
                            value: sym.clone(),
                            has_escape: false,
                            kind: Default::default(),
                        }),
                    }),
                    Key::Num(n) => box Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: n.span,
                        lit: RTsLit::Number(n.clone()),
                    }),
                    Key::BigInt(n) => box Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: n.span,
                        lit: RTsLit::BigInt(n.clone()),
                    }),
                    Key::Private(..) => {
                        unreachable!()
                    }
                };

                if is_str_lit_or_union(&prop_ty) {
                    self.prevent_generalize(&mut prop_ty);
                }

                return Ok(Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly: false,
                    obj_type: box obj,
                    index_type: prop_ty,
                }));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => {
                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                }));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                debug_assert!(!span.is_dummy());
                return Err(Error::Unknown { span });
            }

            Type::Keyword(RTsKeywordType { kind, .. }) if !self.is_builtin => {
                let word = match kind {
                    TsKeywordTypeKind::TsStringKeyword => js_word!("String"),
                    TsKeywordTypeKind::TsNumberKeyword => js_word!("Number"),
                    TsKeywordTypeKind::TsBooleanKeyword => js_word!("Boolean"),
                    TsKeywordTypeKind::TsObjectKeyword => js_word!("Object"),
                    TsKeywordTypeKind::TsSymbolKeyword => js_word!("Symbol"),
                    _ => {
                        dbg!();

                        return Err(Error::NoSuchProperty {
                            span: prop.span(),
                            obj: Some(box obj),
                            prop: Some(box prop.clone()),
                        });
                    }
                };
                let interface = self.env.get_global_type(span, &word)?;

                let err = match self.access_property(span, interface, prop, type_mode, id_ctx) {
                    Ok(v) => return Ok(v),
                    Err(err) => err,
                };
                if *kind == TsKeywordTypeKind::TsObjectKeyword {
                    return Ok(Type::any(span));
                }

                return Err(err);
            }

            Type::Array(Array { elem_type, .. }) => {
                let elem_type = elem_type.clone().cheap();
                if self.scope.should_store_type_params() {
                    self.scope.store_type_param(Id::word("T".into()), elem_type.clone());
                }

                if let Key::Computed(prop) = prop {
                    match prop.ty.normalize() {
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Lit(RTsLitType {
                            lit: RTsLit::Number(..),
                            ..
                        }) => return Ok(elem_type.clone()),

                        _ => {}
                    }
                }
                if let Key::Num(n) = prop {
                    return Ok(elem_type.clone());
                }

                let array_ty = self.env.get_global_type(span, &js_word!("Array"))?;

                return self.access_property(span, array_ty, prop, type_mode, id_ctx);
            }

            Type::Interface(Interface { ref body, extends, .. }) => {
                if let Ok(Some(v)) = self.access_property_of_type_elements(span, &obj, prop, type_mode, body) {
                    return Ok(v);
                }

                for super_ty in extends {
                    let obj = self.type_of_ts_entity_name(
                        span,
                        self.ctx.module_id,
                        &super_ty.expr,
                        super_ty.type_args.as_deref(),
                    )?;

                    // TODO: Check if multiple interface has same property.
                    if let Ok(ty) = self.access_property(span, obj, prop, type_mode, id_ctx) {
                        return Ok(ty);
                    }
                }

                // TODO: Check parent interfaces

                return Err(Error::NoSuchProperty {
                    span,
                    obj: Some(box obj),
                    prop: Some(box prop.clone()),
                });
            }

            Type::TypeLit(TypeLit { ref members, .. }) => {
                if let Some(v) = self.access_property_of_type_elements(span, &obj, prop, type_mode, members)? {
                    return Ok(v);
                }

                dbg!();
                return Err(Error::NoSuchProperty {
                    span,
                    obj: Some(box obj),
                    prop: Some(box prop.clone()),
                });
            }

            Type::Union(ty::Union { types, .. }) => {
                debug_assert!(types.len() >= 1);

                let mut tys = vec![];
                let mut errors = Vec::with_capacity(types.len());

                let has_null = types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsNullKeyword));
                let has_undefined = types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword));

                // tsc is crazy. It uses different error code for these errors.
                if !self.ctx.in_opt_chain && self.rule().strict_null_checks {
                    if has_null && has_undefined {
                        return Err(Error::ObjectIsPossiblyNullOrUndefined { span });
                    }

                    if has_null {
                        return Err(Error::ObjectIsPossiblyNull { span });
                    }

                    if has_undefined {
                        return Err(Error::ObjectIsPossiblyUndefined { span });
                    }
                }

                for ty in types {
                    if !self.rule().strict_null_checks || self.ctx.in_opt_chain {
                        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                            || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                        {
                            continue;
                        }
                    }

                    match self.access_property(span, ty.clone(), prop, type_mode, id_ctx) {
                        Ok(ty) => tys.push(ty),
                        Err(err) => errors.push(err),
                    }
                }

                if type_mode == TypeOfMode::LValue {
                    if !errors.is_empty() {
                        assert_ne!(errors.len(), 0);
                        return Err(Error::UnionError { span, errors });
                    }
                } else {
                    if !errors.is_empty() {
                        print_backtrace();
                        return Err(Error::NoSuchProperty {
                            span,
                            obj: Some(box obj),
                            prop: Some(box prop.clone()),
                        });
                    }
                }

                // TODO: Validate that the ty has same type instead of returning union.
                return Ok(Type::union(tys));
            }

            Type::Tuple(Tuple { ref elems, .. }) => match prop {
                Key::Num(n) => {
                    let v = n.value.round() as i64;

                    if v < 0 {
                        return Err(Error::TupleIndexError {
                            span: n.span(),
                            index: v,
                            len: elems.len() as u64,
                        });
                    }

                    if v as usize >= elems.len() {
                        match elems.last() {
                            Some(elem) => match elem.ty.normalize() {
                                Type::Rest(rest_ty) => {
                                    // debug_assert!(rest_ty.ty.is_clone_cheap());
                                    return Ok(*rest_ty.ty.clone());
                                }
                                _ => {}
                            },
                            _ => {}
                        }

                        return Err(Error::TupleIndexError {
                            span: n.span(),
                            index: v,
                            len: elems.len() as u64,
                        });
                    }

                    return Ok(*elems[v as usize].ty.clone());
                }
                _ => {
                    let mut types = elems.iter().map(|e| *e.ty.clone()).collect::<Vec<_>>();
                    types.dedup_type();
                    let obj = Type::Array(Array {
                        span,
                        elem_type: box Type::union(types),
                    });

                    return self.access_property(span, obj, prop, type_mode, id_ctx);
                }
            },

            Type::ClassDef(cls) => {
                match prop {
                    Key::Normal { sym, .. } if *sym == *"prototype" => {
                        return self.create_prototype_of_class_def(cls).map(Type::TypeLit)
                    }
                    _ => {}
                }

                //
                for m in &cls.body {
                    //
                    match *m {
                        ClassMember::Property(ref p) => {
                            if !p.is_static {
                                continue;
                            }
                            // TODO: normalized string / ident
                            if self.key_matches(span, &p.key, prop, false) {
                                if let Some(ref ty) = p.value {
                                    return Ok(*ty.clone());
                                }

                                return Ok(Type::any(p.key.span()));
                            }
                        }

                        ClassMember::Method(ref m) => {
                            if !m.is_static {
                                continue;
                            }

                            if self.key_matches(span, &m.key, prop, false) {
                                return Ok(Type::Function(ty::Function {
                                    span,
                                    type_params: m.type_params.clone(),
                                    params: m.params.clone(),
                                    ret_ty: m.ret_ty.clone(),
                                }));
                            }
                        }
                        _ => {}
                    }
                }

                if let Some(super_ty) = &cls.super_class {
                    if let Ok(v) = self.access_property(span, *super_ty.clone(), prop, type_mode, id_ctx) {
                        return Ok(v);
                    }
                }

                return Err(Error::NoSuchPropertyInClass {
                    span,
                    class_name: cls.name.clone(),
                    prop: prop.clone(),
                });
            }

            Type::Module(ty::Module { ref exports, .. }) => {
                match id_ctx {
                    IdCtx::Type => {
                        if let Key::Normal { sym, .. } = prop {
                            if let Some(types) = exports.types.get(sym).cloned() {
                                if types.len() == 1 {
                                    return Ok(types.into_iter().next().unwrap());
                                }
                                return Ok(Type::Intersection(Intersection { span, types }));
                            }
                        }
                    }
                    IdCtx::Var => {
                        if let Key::Normal { sym, .. } = prop {
                            if let Some(item) = exports.vars.get(sym) {
                                return Ok(item.clone());
                            }
                        }
                    }
                }

                print_backtrace();
                // No property found
                return Err(Error::NoSuchPropertyInModule { span });
            }

            Type::This(..) => {
                if let Some(this) = self.scope.this().map(|this| this.into_owned()) {
                    return self.access_property(span, this, prop, type_mode, id_ctx);
                } else if self.ctx.in_argument {
                    // We will adjust `this` using information from callee.
                    return Ok(Type::any(span));
                }
            }

            Type::Intersection(Intersection { ref types, .. }) => {
                // TODO: Verify if multiple type has field
                let mut new = vec![];
                for ty in types {
                    let ty = self.expand_top_ref(span, Cow::Borrowed(ty))?.into_owned();
                    if let Some(v) = self.access_property(span, ty, prop, type_mode, id_ctx).ok() {
                        new.push(v);
                    }
                }
                // Exclude accesses to type params.
                if new.len() >= 2 {
                    new.retain(|prop_ty| match prop_ty.normalize() {
                        Type::IndexedAccessType(iat) => match iat.obj_type.normalize() {
                            Type::Param(..) => false,
                            _ => true,
                        },
                        _ => true,
                    });
                }

                // print_backtrace();
                if new.len() == 1 {
                    return Ok(new.into_iter().next().unwrap());
                }

                return Ok(Type::Union(Union { span, types: new }));
            }

            Type::Mapped(m) => {
                //
                let constraint = self::constraint_reducer::reduce(m);
                // If type of prop is equal to the type of index signature, it's
                // index access.

                match constraint.as_ref().map(Type::normalize) {
                    Some(Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: box Type::Array(..),
                        ..
                    })) => {
                        if let Ok(obj) = self.env.get_global_type(span, &js_word!("Array")) {
                            return self.access_property(span, obj, prop, type_mode, id_ctx);
                        }
                    }

                    Some(Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ..
                    })) => {}

                    Some(index @ Type::Keyword(..)) | Some(index @ Type::Param(..)) => {
                        // {
                        //     [P in string]: number;
                        // };
                        if let Ok(()) = self.assign(&index, &prop.ty(), span) {
                            return Ok(m.ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(span)));
                        }
                    }

                    _ => {}
                }

                let obj = self
                    .expand_mapped(span, m)
                    .context("tried to expand a mapped type to access property")?;

                return self.access_property_inner(span, obj, prop, type_mode, id_ctx);
            }

            Type::Ref(r) => {
                if let Key::Computed(prop) = prop {
                    match obj.normalize() {
                        Type::Param(..) => {
                            let index_type = prop.ty.clone();
                            // Return something like SimpleDBRecord<Flag>[Flag];
                            return Ok(Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly: false,
                                obj_type: box obj,
                                index_type,
                            }));
                        }
                        _ => {}
                    }
                } else {
                    match &r.type_name {
                        RTsEntityName::TsQualifiedName(_) => {}
                        RTsEntityName::Ident(i) => {
                            if let Some(class) = self.scope.get_this_class_name() {
                                if class == *i {
                                    return self.access_property(
                                        span,
                                        Type::StaticThis(StaticThis { span }),
                                        prop,
                                        type_mode,
                                        id_ctx,
                                    );
                                }
                            }
                        }
                    }
                }

                let obj = self
                    .expand_top_ref(span, Cow::Borrowed(&obj))
                    .context("tried to expand reference to access property")?
                    .into_owned();

                return self.access_property(span, obj, prop, type_mode, id_ctx);
            }

            Type::IndexedAccessType(..) => {
                let index_type = match prop {
                    Key::Normal { sym, .. } => {
                        let mut prop_ty = box Type::Lit(RTsLitType {
                            node_id: NodeId::invalid(),
                            span: DUMMY_SP,
                            lit: RTsLit::Str(RStr {
                                span: DUMMY_SP,
                                value: sym.clone(),
                                has_escape: false,
                                kind: Default::default(),
                            }),
                        });
                        self.prevent_generalize(&mut prop_ty);

                        prop_ty
                    }
                    Key::Computed(c) => c.ty.clone(),
                    _ => unreachable!(),
                };

                let ty = Type::IndexedAccessType(IndexedAccessType {
                    span,
                    obj_type: box obj,
                    readonly: false,
                    index_type,
                });
                return Ok(ty);
            }

            Type::Query(QueryType {
                expr: box QueryExpr::TsEntityName(name),
                ..
            }) => {
                let obj = self.type_of_ts_entity_name(span, self.ctx.module_id, name, None)?;
                return self.access_property(span, obj, prop, type_mode, id_ctx);
            }

            Type::Function(f) if type_mode == TypeOfMode::RValue => {
                // Use builtin type `Function`
                let interface = self.env.get_global_type(f.span, &js_word!("Function"))?;

                return self.access_property(span, interface, prop, type_mode, id_ctx);
            }

            _ => {}
        }

        print_backtrace();
        unimplemented!("access_property(MemberExpr):\nObject: {:?}\nProp: {:?}", obj, prop);
    }

    fn type_to_query_if_required(&mut self, span: Span, i: &RIdent, ty: Type) -> Type {
        if self.scope.is_in_call() {
            return ty;
        }

        match ty.normalize() {
            Type::Union(ref union_ty) => {
                let is_all_fn = union_ty.types.iter().all(|v| match v.normalize() {
                    Type::Function(f) => true,
                    _ => false,
                });
                if is_all_fn {
                    // We should return typeof function name
                    return Type::Query(QueryType {
                        span,
                        expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(i.clone())),
                    });
                }
                return ty;
            }
            _ => return ty,
        }
    }

    /// Returned type reflects conditional type facts.
    pub(super) fn type_of_var(
        &mut self,
        i: &RIdent,
        type_mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        let span = i.span();
        let id: Id = i.into();
        let name: Name = i.into();

        if self.scope.is_declaring_fn(&id) {
            // We will expand this type query to proper type while calculating returns types
            // of a function.
            return Ok(Type::Query(QueryType {
                span,
                expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(id.into())),
            }));
        }

        let mut modules = vec![];
        let mut ty = self.type_of_raw_var(i, type_mode, type_args)?;
        let mut need_intersection = true;

        // TODO: Change return type of type_of_raw_var to Option and inject module from
        // here.
        match ty.normalize() {
            Type::Module(..) => {
                need_intersection = false;
            }
            Type::Intersection(i) => {
                for ty in &i.types {
                    match ty.normalize() {
                        Type::Module(..) => {
                            need_intersection = false;
                            break;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        if self.ctx.allow_module_var && need_intersection {
            if let Some(types) = self.find_type(self.ctx.module_id, &id)? {
                for ty in types {
                    debug_assert!(ty.is_clone_cheap(), "{:?}", ty);

                    match ty.normalize() {
                        Type::Module(..) => modules.push(ty.clone().into_owned()),
                        Type::Intersection(intersection) => {
                            for ty in &intersection.types {
                                debug_assert!(ty.is_clone_cheap());

                                match ty.normalize() {
                                    Type::Module(..) => modules.push(ty.clone()),
                                    _ => {}
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        ty = self.apply_type_facts(&name, ty);

        ty = self.type_to_query_if_required(span, i, ty);

        self.exclude_types_using_fact(&name, &mut ty);

        if !modules.is_empty() {
            modules.push(ty);
            ty = Type::Intersection(Intersection {
                span: i.span,
                types: modules,
            });
            ty.make_cheap();
        }

        slog::debug!(self.logger, "type_of_var({:?}): {:?}", id, ty);

        Ok(ty)
    }

    /// Returned type does not reflects conditional type facts. (like Truthy /
    /// exclusion)
    fn type_of_raw_var(
        &mut self,
        i: &RIdent,
        type_mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        slog::info!(self.logger, "({}) type_of_raw_var({})", self.scope.depth(), Id::from(i));

        // See documentation on Analyzer.cur_module_name to understand what we are doing
        // here.
        if let Some(cur_module) = self.scope.current_module_name() {
            let ty = self.find_type(self.ctx.module_id, &cur_module)?;
            if let Some(ty) = ty {
                for ty in ty {
                    match ty.normalize() {
                        Type::Module(module) => {
                            //
                            if let Some(var_ty) = module.exports.vars.get(&i.sym).cloned() {
                                return Ok(var_ty);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        let span = i.span();

        if let Some(ref cls_name) = self.scope.this_class_name {
            if *cls_name == i {
                slog::warn!(
                    self.logger,
                    "Creating ref because we are currently defining a class: {}",
                    i.sym
                );
                return Ok(Type::Ref(Ref {
                    span,
                    ctxt: self.ctx.module_id,
                    type_name: RTsEntityName::Ident(i.clone()),
                    type_args: None,
                }));
            }
        }

        match i.sym {
            js_word!("arguments") => return Ok(Type::any(span)),
            js_word!("undefined") => return Ok(Type::undefined(span)),
            js_word!("void") => return Ok(Type::any(span)),
            js_word!("eval") => match type_mode {
                TypeOfMode::LValue => return Err(Error::CannotAssignToNonVariable { span }),
                TypeOfMode::RValue => {
                    return Ok(Type::Function(ty::Function {
                        span,
                        params: vec![],
                        ret_ty: box Type::any(span),
                        type_params: None,
                    }));
                }
            },
            _ => {}
        }

        if i.sym == js_word!("require") {
            unreachable!("typeof(require('...'))");
        }

        if let Some(ty) = self.find_imported_var(&i.into())? {
            println!("({}) type_of({}): resolved import", self.scope.depth(), Id::from(i));
            return Ok(ty.clone());
        }

        if let Some(v) = self.scope.vars.get(&i.into()) {
            slog::debug!(self.logger, "found var with name");
            match type_mode {
                TypeOfMode::LValue => {
                    if let Some(ty) = &v.ty {
                        slog::debug!(self.logger, "Type of var: {:?}", ty);
                        return Ok(ty.clone());
                    }
                }

                TypeOfMode::RValue => {
                    if let Some(ty) = &v.actual_ty {
                        slog::debug!(self.logger, "Type of var: {:?}", ty);
                        return Ok(ty.clone());
                    }
                }
            }
        }

        // Check `declaring` before checking variables.
        if self.scope.declaring.contains(&i.into()) {
            println!("({}) reference in initialization: {}", self.scope.depth(), i.sym);

            if self.ctx.allow_ref_declaring {
                return Ok(Type::any(span));
            } else {
                return Err(Error::ReferencedInInit { span });
            }
        }

        if let Some(ty) = self.find_var_type(&i.into(), type_mode) {
            slog::debug!(self.logger, "find_var_type returned a type");
            let mut span = span;
            let mut ty = ty.into_owned();
            if self.scope.kind().allows_respanning() {
                if self.is_implicitly_typed(&ty) {
                    span.ctxt = span.ctxt.apply_mark(self.marks().implicit_type_mark);
                }
                if !self.may_generalize(&ty) {
                    span = self.prevent_generalize_span(span);
                }
                ty.respan(span);
            }
            slog::debug!(self.logger, "{:?}", ty);
            return Ok(ty);
        }

        if let Some(_var) = self.find_var(&i.into()) {
            // TODO: Infer type or use type hint to handle
            //
            // let id: (x: Foo) => Foo = x => x;
            //
            return Ok(Type::any(span));
        }

        if !self.is_builtin {
            if let Ok(ty) = self.env.get_global_var(span, &i.sym) {
                return Ok(ty);
            }
        }

        if let Ok(Some(types)) = self.find_type(self.ctx.module_id, &i.into()) {
            for ty in types {
                debug_assert!(ty.is_clone_cheap());

                match ty.normalize() {
                    Type::Module(..) => return Ok(ty.clone().into_owned()),
                    _ => {}
                }
            }
            Err(Error::TypeUsedAsVar {
                span,
                name: i.clone().into(),
            })
        } else {
            if self.this_has_property_named(&i.clone().into()) {
                dbg!();
                Err(Error::NoSuchVarButThisHasSuchProperty {
                    span,
                    name: i.clone().into(),
                })
            } else {
                dbg!();
                Err(Error::NoSuchVar {
                    span,
                    name: i.clone().into(),
                })
            }
        }
    }

    pub(crate) fn type_of_ts_entity_name(
        &mut self,
        span: Span,
        ctxt: ModuleId,
        n: &RTsEntityName,
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult {
        match *n {
            RTsEntityName::Ident(ref i) => {
                if i.sym == js_word!("Array") {
                    if let Some(type_args) = type_args {
                        // TODO: Validate number of args.
                        return Ok(Type::Array(Array {
                            span,
                            // TODO: Check length (After implementing error recovery for the parser)
                            elem_type: box type_args.clone().params.into_iter().next().unwrap(),
                        }));
                    }
                }

                if let Some(types) = self.find_type(ctxt, &i.into())? {
                    for ty in types {
                        match ty.normalize() {
                            Type::Instance(..)
                            | Type::Interface(_)
                            | Type::Class(_)
                            | Type::ClassDef(_)
                            | Type::Enum(_)
                            | Type::EnumVariant(_)
                            | Type::This(_)
                            | Type::StaticThis(_)
                            | Type::Param(_)
                            | Type::Constructor(_)
                            | Type::Function(_)
                            | Type::TypeLit(_)
                            | Type::Keyword(_)
                            | Type::Optional(_)
                            | Type::Rest(_)
                            | Type::Lit(_) => {
                                let mut ty = ty.into_owned().clone();
                                let mut params = None;
                                if let Some(type_args) = type_args {
                                    match ty.normalize() {
                                        Type::Interface(Interface {
                                            type_params: Some(type_params),
                                            ..
                                        })
                                        | Type::Alias(Alias {
                                            type_params: Some(type_params),
                                            ..
                                        })
                                        | Type::Class(Class {
                                            def:
                                                box ClassDef {
                                                    type_params: Some(type_params),
                                                    ..
                                                },
                                            ..
                                        })
                                        | Type::ClassDef(ClassDef {
                                            type_params: Some(type_params),
                                            ..
                                        }) => {
                                            params = self
                                                .instantiate_type_params_using_args(span, type_params, type_args)
                                                .map(Some)?;
                                        }
                                        _ => {
                                            unimplemented!(
                                                "Error reporting for type arguments for types without type \
                                                 parameters: {:#?}",
                                                ty
                                            )
                                        }
                                    }
                                }
                                if let Some(params) = params {
                                    ty = self.expand_type_params(&params, ty)?;
                                }
                                ty.respan(span);
                                return Ok(ty);
                            }

                            Type::Symbol(_) => {}
                            Type::Query(_) => {}
                            Type::Infer(_) => {}
                            Type::Import(_) => {}
                            Type::Predicate(_) => {}
                            Type::IndexedAccessType(_) => {}
                            Type::Ref(_) => {}

                            Type::Conditional(_) => {}
                            Type::Tuple(_) => {}
                            Type::Array(_) => {}
                            Type::Union(_) => {}
                            Type::Intersection(_) => {}
                            Type::Operator(_) => {}
                            Type::Mapped(_) => {}
                            Type::Alias(_) => {}
                            Type::Namespace(_) => {}
                            Type::Module(_) => {}
                            Type::Arc(_) => {}
                        }
                    }
                }

                slog::warn!(self.logger, "Creating Type::Ref: {:?}", i);

                Ok(Type::Ref(Ref {
                    span,
                    ctxt: self.ctx.module_id,
                    type_name: RTsEntityName::Ident(i.clone()),
                    type_args: type_args.cloned().map(Box::new),
                }))
            }
            RTsEntityName::TsQualifiedName(ref qname) => {
                let obj_ty = self.type_of_ts_entity_name(span, ctxt, &qname.left, None)?;

                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };
                let obj_ty = self.with_ctx(ctx).expand_fully(span, obj_ty, true)?;

                self.access_property(
                    span,
                    obj_ty,
                    &Key::Normal {
                        span: qname.right.span,
                        sym: qname.right.sym.clone(),
                    },
                    TypeOfMode::RValue,
                    IdCtx::Type,
                )
            }
        }
    }

    fn type_of_member_expr(&mut self, expr: &RMemberExpr, type_mode: TypeOfMode) -> ValidationResult {
        let RMemberExpr {
            ref obj,
            computed,
            ref prop,
            span,
            ..
        } = *expr;

        let mut errors = Errors::default();

        let mut is_obj_opt_chain = false;
        let mut should_be_optional = false;
        let obj_ty = match *obj {
            RExprOrSuper::Expr(ref obj) => {
                is_obj_opt_chain = is_obj_opt_chaining(&obj);

                let obj_ctx = Ctx {
                    allow_module_var: true,
                    in_opt_chain: is_obj_opt_chain,
                    should_store_truthy_for_access: self.ctx.in_cond && !is_obj_opt_chain,
                    ..self.ctx
                };

                let obj_ty = match obj.validate_with_default(&mut *self.with_ctx(obj_ctx)) {
                    Ok(ty) => ty,
                    Err(err) => {
                        // Recover error if possible.
                        if computed {
                            errors.push(err);
                            Type::any(span)
                        } else {
                            return Err(err);
                        }
                    }
                };

                if is_obj_opt_chain {
                    should_be_optional = self.is_obj_optional(&obj_ty)?;
                }

                obj_ty
            }

            RExprOrSuper::Super(RSuper { span, .. }) => {
                if let Some(v) = self.scope.get_super_class() {
                    v.clone()
                } else {
                    self.storage.report(Error::SuperInClassWithoutSuper { span });
                    Type::any(span)
                }
            }
        };
        self.storage.report_all(errors);

        let prop = self.validate_key(prop, computed)?;

        let prop_access_ctx = Ctx {
            in_opt_chain: self.ctx.in_opt_chain || is_obj_opt_chain,
            ..self.ctx
        };

        let ty = if computed {
            self.with_ctx(prop_access_ctx)
                .access_property(span, obj_ty, &prop, type_mode, IdCtx::Var)?
        } else {
            let mut ty = self
                .with_ctx(prop_access_ctx)
                .access_property(span, obj_ty.clone(), &prop, type_mode, IdCtx::Var)
                .context(
                    "tried to access property of an object to calculate type of a non-computed member expression",
                )?;

            let name: Option<Name> = expr.try_into().ok();
            if let Some(name) = name {
                ty = self.apply_type_facts(&name, ty);

                self.exclude_types_using_fact(&name, &mut ty);
            }

            if self.ctx.in_cond && self.ctx.should_store_truthy_for_access {
                // Add type facts.
                match obj {
                    RExprOrSuper::Expr(obj) => {
                        if let Some(name) = extract_name_for_assignment(obj) {
                            let next_ty = self
                                .filter_types_with_property(
                                    &obj_ty,
                                    match &prop {
                                        Key::Normal { sym, .. } => sym,
                                        _ => unreachable!(),
                                    },
                                    Some(TypeFacts::Truthy),
                                )
                                .report(&mut self.storage)
                                .map(|ty| ty.cheap());
                            if let Some(next_ty) = next_ty {
                                self.cur_facts
                                    .false_facts
                                    .excludes
                                    .entry(name.clone())
                                    .or_default()
                                    .push(next_ty.clone());

                                self.add_deep_type_fact(name, next_ty, true);
                            }
                        }
                    }
                    _ => {}
                }
            }

            ty
        };

        if should_be_optional {
            Ok(Type::union(vec![Type::undefined(span), ty]))
        } else {
            Ok(ty)
        }
    }

    fn prefer_tuple(&mut self, type_ann: Option<&Type>) -> bool {
        let ty = match type_ann {
            Some(ty) => ty.normalize(),
            None => return false,
        };

        match ty {
            Type::Ref(Ref { span, .. }) => {
                let ctx = Ctx {
                    ignore_expand_prevention_for_top: true,
                    preserve_ref: false,
                    ..self.ctx
                };
                let ty = self.with_ctx(ctx).expand_fully(*span, ty.clone(), true);
                let ty = match ty {
                    Ok(v) => v,
                    Err(..) => return false,
                };
                return self.prefer_tuple(Some(&ty));
            }
            Type::Tuple(..) => true,
            Type::TypeLit(ty) => self.prefer_tuple_type_elements(&ty.members),
            Type::Interface(ty) => {
                if !self.prefer_tuple_type_elements(&ty.body) {
                    return false;
                }

                for parent in &ty.extends {
                    let parent_ty = self.type_of_ts_entity_name(
                        parent.span,
                        self.ctx.module_id,
                        &parent.expr,
                        parent.type_args.as_deref(),
                    );

                    let parent_ty = match parent_ty {
                        Ok(v) => v,
                        Err(..) => return false,
                    };
                    if !self.prefer_tuple(Some(&parent_ty)) {
                        return false;
                    }
                }

                true
                //
            }
            _ => false,
        }
    }

    fn prefer_tuple_type_elements(&mut self, elems: &[TypeElement]) -> bool {
        // If a type literal contains [number]: T, it should be treated as an array.
        if elems.iter().any(|el| match el {
            TypeElement::Index(el) => {
                if el.params.is_empty() {
                    return false;
                }
                match el.params[0].ty.normalize() {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => true,
                    _ => false,
                }
            }
            _ => false,
        }) {
            return false;
        }

        // Check for numeric keys like '0', '1', '2'.
        elems.iter().any(|el| match el {
            TypeElement::Property(PropertySignature { key: Key::Num(..), .. }) => true,
            _ => false,
        })
    }
}
