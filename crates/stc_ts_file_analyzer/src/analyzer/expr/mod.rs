use std::{
    borrow::Cow,
    collections::HashMap,
    convert::{TryFrom, TryInto},
    time::{Duration, Instant},
};

use optional_chaining::is_obj_opt_chaining;
use rnode::{NodeId, VisitWith};
use stc_ts_ast_rnode::{
    RAssignExpr, RBindingIdent, RClassExpr, RExpr, RIdent, RInvalid, RLit, RMemberExpr, RMemberProp, RNull, RNumber, ROptChainBase,
    ROptChainExpr, RParenExpr, RPat, RPatOrExpr, RSeqExpr, RStr, RSuper, RSuperProp, RSuperPropExpr, RThisExpr, RTpl, RTsEntityName,
    RTsEnumMemberId, RTsLit, RTsNonNullExpr, RUnaryExpr,
};
use stc_ts_base_type_ops::bindings::BindingKind;
use stc_ts_errors::{
    debug::{dump_type_as_string, print_backtrace},
    DebugExt, Error, Errors,
};
use stc_ts_generics::ExpandGenericOpts;
use stc_ts_type_ops::{generalization::prevent_generalize, is_str_lit_or_union, Fix};
pub use stc_ts_types::IdCtx;
use stc_ts_types::{
    name::Name, Alias, Class, ClassDef, ClassMember, ClassProperty, CommonTypeMetadata, ComputedKey, Id, Key, KeywordType,
    KeywordTypeMetadata, LitType, LitTypeMetadata, Method, ModuleId, Operator, OptionalType, PropertySignature, QueryExpr, QueryType,
    QueryTypeMetadata, StaticThis, ThisType, TplType, TplTypeMetadata,
};
use stc_utils::{cache::Freeze, debug_ctx, ext::TypeVecExt, stack};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::{op, EsVersion, TruePlusMinus, TsKeywordTypeKind, TsTypeOperatorOp, VarDeclKind};
use tracing::{debug, info, warn, Level};
use ty::TypeExt;

use self::bin::extract_name_for_assignment;
pub(crate) use self::{array::GetIteratorOpts, call_new::CallOpts};
use crate::{
    analyzer::{
        assign::AssignOpts,
        pat::PatMode,
        scope::{ExpandOpts, ScopeKind, VarKind},
        util::ResultExt,
        Analyzer, Ctx,
    },
    ty,
    ty::{
        Array, EnumVariant, IndexSignature, IndexedAccessType, Interface, Intersection, Ref, Tuple, Type, TypeElement, TypeLit, TypeParam,
        TypeParamInstantiation,
    },
    type_facts::TypeFacts,
    util::RemoveTypes,
    validator,
    validator::ValidateWith,
    VResult,
};

mod array;
mod await_expr;
mod bin;
mod call_new;
mod const_assertion;
mod constraint_reducer;
mod function;
mod jsx;
mod meta_prop;
mod misc;
mod object;
pub(crate) mod optional_chaining;
mod type_cast;
mod unary;
mod update;

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
    ) -> VResult<Type> {
        self.record(e);

        let _stack = stack::start(64);
        let _ctx = debug_ctx!(format!("validate\nExpr: {:?}", e));

        let span = e.span();
        let need_type_param_handling = match e {
            RExpr::Member(..) => true,
            RExpr::Call(..) | RExpr::New(..) if self.ctx.in_argument => false,
            RExpr::Call(..) | RExpr::New(..) => true,
            _ => false,
        };

        let mut ty = (|| -> VResult<Type> {
            match e {
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
                    let span = *span;

                    if self.ctx.in_static_property_initializer {
                        self.storage.report(Error::ThisInStaticPropertyInitializer { span })
                    } else if self.ctx.in_constructor_param {
                        self.storage.report(Error::ThisInConstructorParam { span })
                    }

                    if self.scope.cannot_use_this_because_super_not_called() {
                        self.storage.report(Error::ThisUsedBeforeCallingSuper { span })
                    }

                    let is_ref_to_module = match self.scope.kind() {
                        ScopeKind::Module => true,
                        _ => false,
                    } || (self.ctx.in_computed_prop_name
                        && match self.scope.scope_of_computed_props().map(|s| s.kind()) {
                            Some(ScopeKind::Module) => true,
                            _ => false,
                        });
                    if is_ref_to_module {
                        self.storage.report(Error::ThisRefToModuleOrNamespace { span })
                    }

                    // Use globalThis
                    if !self.scope.is_this_defined() {
                        return Ok(Type::Query(QueryType {
                            span,
                            expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(RIdent::new(
                                "globalThis".into(),
                                span.with_ctxt(SyntaxContext::empty()),
                            ))),
                            metadata: Default::default(),
                        }));
                    }

                    let scope = if self.ctx.in_computed_prop_name {
                        self.scope.scope_of_computed_props()
                    } else {
                        Some(&self.scope)
                    };
                    if let Some(scope) = scope {
                        if let Some(ty) = scope.this() {
                            return Ok(ty.into_owned());
                        }
                    }
                    return Ok(Type::from(ThisType {
                        span,
                        metadata: Default::default(),
                    }));
                }

                RExpr::Ident(ref i) => {
                    if i.sym == js_word!("undefined") {
                        return Ok(Type::Keyword(KeywordType {
                            span: i.span.with_ctxt(SyntaxContext::empty()),
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            metadata: Default::default(),
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
                    return Ok(Type::Lit(LitType {
                        span: v.span,
                        lit: RTsLit::Bool(v.clone()),
                        metadata: Default::default(),
                    }));
                }
                RExpr::Lit(RLit::Str(ref v)) => {
                    return Ok(Type::Lit(LitType {
                        span: v.span,
                        lit: RTsLit::Str(v.clone()),
                        metadata: Default::default(),
                    }));
                }
                RExpr::Lit(RLit::Num(v)) => {
                    return Ok(Type::Lit(LitType {
                        span: v.span,
                        lit: RTsLit::Number(v.clone()),
                        metadata: Default::default(),
                    }));
                }
                RExpr::Lit(RLit::BigInt(v)) => {
                    return Ok(Type::Lit(LitType {
                        span: v.span,
                        lit: RTsLit::BigInt(v.clone()),
                        metadata: Default::default(),
                    }));
                }
                RExpr::Lit(RLit::Null(RNull { span })) => {
                    if self.ctx.in_export_default_expr {
                        // TODO(kdy1): strict mode
                        return Ok(Type::Keyword(KeywordType {
                            span: *span,
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            metadata: Default::default(),
                        }));
                    }

                    return Ok(Type::Keyword(KeywordType {
                        span: *span,
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        metadata: Default::default(),
                    }));
                }
                RExpr::Lit(RLit::Regex(..)) => {
                    return Ok(Type::Ref(Ref {
                        span,
                        type_name: RTsEntityName::Ident(RIdent {
                            node_id: NodeId::invalid(),
                            span,
                            sym: js_word!("RegExp"),
                            optional: false,
                        }),
                        type_args: None,
                        metadata: Default::default(),
                    }));
                }

                RExpr::Paren(RParenExpr { ref expr, .. }) => expr.validate_with_args(self, (mode, type_args, type_ann)),

                RExpr::Tpl(ref e) => {
                    return e.validate_with_args(self, type_ann);
                }

                RExpr::TsNonNull(RTsNonNullExpr { span, ref expr, .. }) => {
                    let mut ty = expr.validate_with_args(self, (mode, type_args, type_ann))?.remove_falsy();
                    ty.reposition(*span);
                    Ok(ty)
                }

                RExpr::Object(e) => {
                    return e.validate_with_args(self, type_ann);
                }

                // https://github.com/Microsoft/TypeScript/issues/26959
                RExpr::Yield(..) => {
                    e.visit_children_with(self);
                    return Ok(Type::any(span, Default::default()));
                }

                RExpr::Await(e) => e.validate_with_args(self, type_ann),

                RExpr::Class(RClassExpr { ref ident, ref class, .. }) => {
                    self.scope.this_class_name = ident.as_ref().map(|i| i.into());
                    return Ok(class.validate_with(self)?.into());
                }

                RExpr::Arrow(ref e) => return Ok(e.validate_with_args(self, type_ann)?.into()),

                RExpr::Fn(f) => {
                    return Ok(f.validate_with_args(self, type_ann)?.into());
                }

                RExpr::Member(ref expr) => {
                    // Foo.a
                    if self.ctx.should_store_truthy_for_access {
                        if let Ok(name) = Name::try_from(&*expr) {
                            self.cur_facts.true_facts.facts.insert(name.clone(), TypeFacts::Truthy);
                            self.cur_facts.false_facts.facts.insert(name, TypeFacts::Falsy);
                        }
                    }

                    return self.type_of_member_expr(expr, mode);
                }

                RExpr::SuperProp(ref expr) => {
                    return self.type_of_super_prop_expr(expr, mode);
                }

                RExpr::MetaProp(e) => return e.validate_with(self),

                RExpr::Invalid(ref i) => return Ok(Type::any(i.span(), Default::default())),

                RExpr::OptChain(expr) => expr.validate_with_args(self, type_ann),

                RExpr::TsConstAssertion(expr) => expr.validate_with_args(self, (mode, None, type_ann)),

                RExpr::TsSatisfies(expr) => expr.validate_with_args(self, (mode, None, type_ann)),

                RExpr::TsInstantiation(expr) => expr.validate_with_args(self, (mode, None, type_ann)),

                _ => unimplemented!("typeof ({:?})", e),
            }
        })()?;

        if self.is_builtin {
            // `Symbol.iterator` is defined multiple times, and it results in union of
            // `symbol`s.
            match &mut ty {
                Type::Union(u) => {
                    u.types.dedup_type();
                }
                _ => {}
            }
        }

        if !self.is_builtin {
            // TODO(kdy1): Normalize?
            if ty.is_never() {
                self.ctx.in_unreachable = true;
            }
        }

        ty.assert_valid();

        if type_ann.is_none() && need_type_param_handling {
            self.replace_invalid_type_params(&mut ty);
            ty.fix();
        }

        self.cur_facts.assert_clone_cheap();

        if !self.is_builtin {
            debug_assert_ne!(
                ty.span(),
                DUMMY_SP,
                "Found a type with dummy span generated by validating an expression:\n{:?}",
                ty
            )
        }

        // Exclude literals
        if !span.is_dummy()
            & match e {
                RExpr::Lit(..) => false,
                _ => true,
            }
        {
            self.dump_type(span, &ty);
        }

        Ok(ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RParenExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> VResult<Type> {
        e.expr.validate_with_args(self, (mode, None, type_ann))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAssignExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> VResult<Type> {
        let ctx = Ctx {
            pat_mode: PatMode::Assign,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
            let span = e.span();
            let mut mark_var_as_truthy = false;
            let mut skip_right = false;

            let mut left_i = None;
            let ty_of_left;
            let (any_span, type_ann) = match e.left {
                RPatOrExpr::Pat(box RPat::Ident(RBindingIdent { id: ref i, .. })) | RPatOrExpr::Expr(box RExpr::Ident(ref i)) => {
                    // Type is any if self.declaring contains ident
                    let any_span = if analyzer.scope.declaring.contains(&i.into()) {
                        Some(span)
                    } else {
                        None
                    };

                    left_i = Some(i.clone());

                    ty_of_left = analyzer
                        .type_of_var(i, TypeOfMode::LValue, None)
                        .context("tried to get type of lhs of an assignment")
                        .map(|ty| {
                            mark_var_as_truthy = true;
                            ty
                        })
                        .convert_err(|err| {
                            skip_right = true;
                            match err.actual() {
                                Error::CannotAssignToNonVariable { ty, .. } | Error::NotVariable { ty: Some(ty), .. } => {
                                    match ty.normalize() {
                                        Type::Module(..) => Error::CannotAssignToNamespace { span },
                                        Type::Namespace(..) => Error::CannotAssignToModule { span },
                                        Type::ClassDef(..) => Error::CannotAssignToClass { span },
                                        Type::Enum(..) => Error::CannotAssignToEnum { span },
                                        Type::Function(..) => Error::CannotAssignToFunction { span },
                                        _ => err,
                                    }
                                }
                                Error::CannotAssignToNamespace { .. }
                                | Error::CannotAssignToModule { .. }
                                | Error::CannotAssignToClass { .. }
                                | Error::CannotAssignToEnum { .. }
                                | Error::CannotAssignToFunction { .. } => err,
                                _ => {
                                    skip_right = false;
                                    err
                                }
                            }
                        })
                        .report(&mut analyzer.storage);

                    (any_span, ty_of_left.as_ref())
                }

                RPatOrExpr::Pat(box RPat::Expr(ref e)) | RPatOrExpr::Expr(ref e) => {
                    ty_of_left = e
                        .validate_with_args(analyzer, (TypeOfMode::LValue, None, None))
                        .report(&mut analyzer.storage);

                    (None, ty_of_left.as_ref())
                }

                _ => {
                    // TODO(kdy1): This is wrong and commented out because of the evaluation order.
                    //
                    // e.left.visit_with(analyzer);
                    (None, type_ann)
                }
            };

            is_valid_lhs(&e.left).report(&mut analyzer.storage);

            let mut errors = Errors::default();

            let rhs_ty = match {
                if !skip_right {
                    let cannot_be_tuple = match &e.left {
                        RPatOrExpr::Pat(pat) => !analyzer.can_rhs_be_tuple(&pat),
                        _ => false,
                    };

                    let ctx = Ctx {
                        in_assign_rhs: true,
                        cannot_be_tuple,
                        ..analyzer.ctx
                    };
                    let mut analyzer = analyzer.with_ctx(ctx);
                    let result: Result<_, _> = e
                        .right
                        .validate_with_args(&mut *analyzer, (mode, None, type_ann))
                        .context("tried to validate rhs an assign expr");

                    match result {
                        Ok(v) => Some(v),
                        Err(err) => {
                            errors.push(err);
                            None
                        }
                    }
                } else {
                    None
                }
            } {
                Some(rhs_ty) => {
                    let lhs;
                    analyzer.report_error_for_invalid_rvalue(
                        span,
                        match &e.left {
                            RPatOrExpr::Expr(_) => {
                                lhs = RPat::Invalid(RInvalid { span: DUMMY_SP });
                                &lhs
                            }
                            RPatOrExpr::Pat(p) => &**p,
                        },
                        &rhs_ty,
                    );

                    Ok(rhs_ty)
                }
                None => Err(()),
            };

            // TODO(kdy1): Deny changing type of const
            if mark_var_as_truthy {
                if let Some(i) = left_i {
                    analyzer.mark_var_as_truthy(Id::from(i))?;
                }
            }

            analyzer.storage.report_all(errors);

            let mut rhs_ty = match rhs_ty {
                Ok(v) => v,
                Err(()) => Type::any(span, Default::default()),
            };
            rhs_ty.respan(e.right.span());
            rhs_ty.make_cheap();

            analyzer.try_assign(span, e.op, &e.left, &rhs_ty);

            if let Some(span) = any_span {
                return Ok(Type::any(span, Default::default()));
            }

            Ok(rhs_ty)
        })
    }
}

/// All fields defaults to default value of the type. (`false` for [bool]).
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct AccessPropertyOpts {
    pub do_not_validate_type_of_computed_prop: bool,

    pub disallow_indexing_array_with_string: bool,

    /// If `true`, `access_property` will not produce types like `Array['b']`
    pub disallow_creating_indexed_type_from_ty_els: bool,

    pub disallow_indexing_class_with_computed: bool,

    /// Note: If it's in l-value context, `access_property` will return
    /// undefined even if this field is `false`.
    pub use_undefined_for_tuple_index_error: bool,

    pub for_validation_of_indexed_access_type: bool,

    /// If `true`, `access_property` will not return undefined for object
    /// literal types created with a spread.
    ///
    /// This is true for destructuring variable declarations.
    pub disallow_inexact: bool,
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RSeqExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> VResult<Type> {
        let RSeqExpr { span, ref exprs, .. } = *e;

        assert!(exprs.len() >= 1);

        let len = exprs.len();

        let mut is_any = false;
        for (i, e) in exprs.iter().enumerate() {
            let span = e.span();
            let is_last = i == len - 1;

            if !is_last {
                match **e {
                    RExpr::Arrow(..) if !self.rule().allow_unreachable_code => {
                        self.storage.report(Error::UselessSeqExpr { span });
                    }
                    RExpr::Ident(..)
                    | RExpr::Cond(..)
                    | RExpr::Lit(..)
                    | RExpr::Unary(RUnaryExpr { op: op!(unary, "-"), .. })
                    | RExpr::Unary(RUnaryExpr { op: op!(unary, "+"), .. })
                    | RExpr::Unary(RUnaryExpr { op: op!("!"), .. })
                    | RExpr::Unary(RUnaryExpr { op: op!("typeof"), .. })
                        if !self.rule().allow_unreachable_code =>
                    {
                        self.storage.report(Error::UselessSeqExpr { span });
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
                let ctx = Ctx {
                    in_useless_expr_for_seq: true,
                    ..self.ctx
                };
                let mut a = self.with_ctx(ctx);
                match e.validate_with_default(&mut *a) {
                    Ok(..) => {}
                    Err(Error::ReferencedInInit { .. }) => {
                        is_any = true;
                    }
                    Err(err) => a.storage.report(err),
                }
            }
        }
        if is_any {
            return Ok(Type::any(span, Default::default()));
        }

        return exprs.last().unwrap().validate_with_args(self, (mode, None, type_ann));
    }
}

impl Analyzer<'_, '_> {
    /// Returns `true` if a rhs expression of the assignment expression can be
    /// a tuple.
    fn can_rhs_be_tuple(&mut self, left: &RPat) -> bool {
        match left {
            RPat::Array(l) => {
                for elem in l.elems.iter() {
                    match elem {
                        Some(RPat::Rest(rest)) => match &*rest.arg {
                            RPat::Object(..) => {
                                return false;
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        true
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn validate_key(&mut self, prop: &RExpr, computed: bool) -> VResult<Key> {
        if computed {
            prop.validate_with_default(self)
                .and_then(|ty| {
                    self.expand_top_ref(ty.span(), Cow::Owned(ty), Default::default())
                        .map(Cow::into_owned)
                })
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

                RExpr::PrivateName(p) => Ok(Key::Private(p.clone().into())),

                _ => unreachable!("non-computed-key: {:?}", prop),
            }
        }
    }

    /// Check if key matches.
    ///
    /// # Parameters
    ///
    /// - `declared`: Key of declared property.
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn key_matches(&mut self, span: Span, declared: &Key, cur: &Key, allow_union: bool) -> bool {
        match declared {
            Key::Computed(..) => {}
            _ => {
                if declared.type_eq(cur) {
                    return true;
                }
            }
        }

        match (declared, cur) {
            (Key::Private(d), Key::Private(cur)) => {
                if *d.id.sym() == *cur.id.sym() {
                    return true;
                }
            }
            (Key::Private(..), _) | (_, Key::Private(..)) => return false,
            _ => {}
        }

        match (declared, cur) {
            (Key::Normal { sym, .. }, Key::Num(RNumber { value, .. })) => {
                if &**sym == value.to_string() {
                    return true;
                }
            }
            _ => {}
        }

        match (declared, cur) {
            (Key::Num(RNumber { value: declared_value, .. }), Key::Num(RNumber { value, .. })) => {
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

            Key::Normal { .. } => return false,
            _ => {}
        }

        self.assign(span, &mut Default::default(), &declared.ty(), &cur.ty()).is_ok()
    }

    fn check_if_type_matches_key(&mut self, span: Span, declared: &Key, key_ty: &Type, allow_union: bool) -> bool {
        let key_ty = key_ty.normalize();

        if declared.ty().type_eq(key_ty) {
            return true;
        }

        match key_ty {
            Type::Ref(..) => {
                let cur = self.expand_top_ref(span, Cow::Borrowed(key_ty), Default::default());
                if let Ok(cur) = cur {
                    return self.check_if_type_matches_key(span, declared, &cur, allow_union);
                }
            }

            Type::EnumVariant(EnumVariant { enum_name, name: None, .. }) => {
                if let Ok(Some(types)) = self.find_type(enum_name) {
                    for ty in types {
                        match ty.normalize() {
                            Type::Enum(e) => {
                                let e = e.clone();
                                return self.check_if_type_matches_key(span, declared, &Type::Enum(e), allow_union);
                            }
                            _ => {}
                        }
                    }
                }

                let cur = self.expand_top_ref(span, Cow::Borrowed(key_ty), Default::default());
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
            Type::Union(u) if allow_union => return u.types.iter().any(|ty| self.check_if_type_matches_key(span, declared, &ty, true)),

            _ => {}
        }

        false
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    fn access_property_of_type_elements(
        &mut self,
        span: Span,
        obj: &Type,
        prop: &Key,
        type_mode: TypeOfMode,
        members: &[TypeElement],
        opts: AccessPropertyOpts,
    ) -> VResult<Option<Type>> {
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
                                    let mut types = vec![Type::undefined(span, Default::default()), *type_ann.clone()];
                                    types.dedup_type();
                                    matching_elements.push(Type::union(types));
                                } else {
                                    matching_elements.push(*type_ann.clone());
                                }
                                continue;
                            }

                            // TODO(kdy1): no implicit any?
                            matching_elements.push(Type::any(span, Default::default()));
                            continue;
                        }

                        TypeElement::Method(ref m) => {
                            //
                            let prop_ty = Type::Function(ty::Function {
                                span,
                                type_params: m.type_params.clone(),
                                params: m.params.clone(),
                                ret_ty: m.ret_ty.clone().unwrap_or_else(|| box Type::any(span, Default::default())),
                                metadata: Default::default(),
                            });

                            if m.optional {
                                let mut types = vec![Type::undefined(span, Default::default()), prop_ty.clone()];
                                types.dedup_type();
                                matching_elements.push(Type::union(types));
                            } else {
                                matching_elements.push(prop_ty.clone());
                            }

                            continue;
                        }

                        _ => unimplemented!("TypeElement {:?}", el),
                    }
                }

                match (key, prop) {
                    (
                        Key::Num(key),
                        Key::Normal {
                            span: prop_span,
                            sym: prop_sym,
                        },
                    ) => {
                        // If we are accessing an object which has 0b11010 (26,
                        // binary number) and the accessor is "26" (string), it
                        // should be any.
                        //
                        //
                        if *prop_sym == js_word!("Infinity") {
                            return Ok(Some(Type::any(span, Default::default())));
                        } else if prop_sym.starts_with("0b") || prop_sym.starts_with("0B") {
                            let prop_num = lexical::parse_radix::<f64, _>(prop_sym[2..].as_bytes(), 2);

                            if let Ok(prop_num) = prop_num {
                                if key.value == prop_num {
                                    return Ok(Some(Type::any(span, Default::default())));
                                }
                            }
                        } else if prop_sym.starts_with("0o") || prop_sym.starts_with("0O") {
                            let prop_num = lexical::parse_radix::<f64, _>(prop_sym[2..].as_bytes(), 8);

                            if let Ok(prop_num) = prop_num {
                                if key.value == prop_num {
                                    return Ok(Some(Type::any(span, Default::default())));
                                }
                            }
                        } else {
                            let prop_num = lexical::parse_radix::<f64, _>(prop_sym.as_bytes(), 10);

                            if let Ok(prop_num) = prop_num {
                                if key.value == prop_num {
                                    return Ok(Some(Type::any(span, Default::default())));
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if matching_elements.len() == 1 {
            return Ok(matching_elements.pop());
        }

        let is_callable = members.iter().any(|element| match element {
            TypeElement::Call(_) => true,
            _ => false,
        });

        if is_callable {
            // Handle funciton-like interfaces
            // Example of code handled by this block is `Error.call`

            let obj = self.env.get_global_type(span, &js_word!("Function"))?;

            if let Ok(v) = self.access_property(span, &obj, prop, type_mode, IdCtx::Var, opts) {
                return Ok(Some(v));
            }
        }

        let mut has_index_signature = false;
        for el in members.iter().rev() {
            match el {
                TypeElement::Index(IndexSignature {
                    ref params,
                    ref type_ann,
                    readonly,
                    ..
                }) => {
                    has_index_signature = true;

                    if params.len() != 1 {
                        unimplemented!("Index signature with multiple parameters")
                    }

                    let index_ty = &params[0].ty;

                    let prop_ty = prop.ty();

                    // Don't know exact reason, but you can index `{ [x: string]: boolean }`
                    // with number type.
                    //
                    // Reverse also works, although it returns any
                    //
                    // I guess it's because javascript work in that way.

                    if index_ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword) && prop_ty.is_str() && prop.is_computed() {
                        return Ok(Some(Type::any(span, Default::default())));
                    }

                    let indexed = (index_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword) && prop_ty.is_num())
                        || self.assign(span, &mut Default::default(), &index_ty, &prop_ty).is_ok();

                    if indexed {
                        if let Some(ref type_ann) = type_ann {
                            let ty = self.expand_top_ref(span, Cow::Borrowed(type_ann), Default::default())?;
                            return Ok(Some(ty.into_owned()));
                        }

                        return Ok(Some(Type::any(span, Default::default())));
                    }

                    if (&**index_ty).type_eq(&*prop_ty) {
                        return Ok(Some(
                            type_ann.clone().map(|v| *v).unwrap_or_else(|| Type::any(span, Default::default())),
                        ));
                    }

                    match prop_ty.normalize() {
                        // TODO(kdy1): Only string or number
                        Type::EnumVariant(..) => {
                            matching_elements.extend(type_ann.clone().map(|v| *v));
                            continue;
                        }

                        _ => {}
                    }
                }
                _ => {}
            }
        }

        if has_index_signature && !opts.disallow_creating_indexed_type_from_ty_els {
            // This check exists to prefer a specific property over generic index signature.
            if prop.is_computed() || matching_elements.is_empty() {
                warn!("Creating a indexed access type from a type literal");
                let ty = Type::IndexedAccessType(IndexedAccessType {
                    span,
                    obj_type: box obj.clone(),
                    index_type: box prop.ty().into_owned(),
                    readonly: false,
                    metadata: Default::default(),
                });

                return Ok(Some(ty));
            }
        }

        if matching_elements.len() == 0 {
            return Ok(None);
        }

        matching_elements.dedup_type();

        Ok(Some(Type::union(matching_elements)))
    }

    pub(super) fn access_property(
        &mut self,
        span: Span,
        obj: &Type,
        prop: &Key,
        type_mode: TypeOfMode,
        id_ctx: IdCtx,
        opts: AccessPropertyOpts,
    ) -> VResult<Type> {
        if !self.is_builtin {
            debug_assert_ne!(span, DUMMY_SP, "access_property: called with a dummy span");
        }

        let _tracing = if cfg!(debug_assertions) {
            let obj = dump_type_as_string(&self.cm, &obj);
            // let prop_ty = dump_type_as_string(&self.cm, &prop.ty());

            Some(tracing::span!(Level::ERROR, "access_property", obj = &*obj).entered())
        } else {
            None
        };

        let span = span.with_ctxt(SyntaxContext::empty());

        let start = Instant::now();
        obj.assert_valid();

        // Try some easier assignments.
        if prop.is_computed() {
            if match obj.normalize() {
                Type::Tuple(..) => true,
                _ => false,
            } {
                // See if key is number.
                match prop.ty().normalize() {
                    Type::Lit(LitType {
                        lit: RTsLit::Number(prop), ..
                    }) => return self.access_property(span, obj, &Key::Num(prop.clone()), type_mode, id_ctx, opts),
                    _ => {}
                }
            }

            // See if key is string.
            match prop.ty().normalize() {
                Type::Lit(LitType {
                    lit: RTsLit::Str(prop), ..
                }) => {
                    let res = self
                        .access_property(
                            span,
                            obj,
                            &Key::Normal {
                                span: prop.span,
                                sym: prop.value.clone(),
                            },
                            type_mode,
                            id_ctx,
                            AccessPropertyOpts {
                                disallow_indexing_array_with_string: true,
                                disallow_creating_indexed_type_from_ty_els: true,
                                ..opts
                            },
                        )
                        .context("tired to access property using string as a key");
                    // As some types has rules about computed propeties, we use the result only if
                    // it sucesses.
                    if let Ok(ty) = res {
                        return Ok(ty);
                    }

                    match obj.normalize() {
                        Type::Enum(..) | Type::Symbol(..) => return res,
                        _ => {}
                    }
                }

                Type::Lit(LitType {
                    lit: RTsLit::Number(n), ..
                }) => {
                    // As some types has rules about computed propeties, we use the result only if
                    // it sucesses.
                    if let Ok(ty) = self.access_property(span, obj, &Key::Num(n.clone()), type_mode, id_ctx, opts) {
                        return Ok(ty);
                    }
                }

                Type::Param(TypeParam {
                    constraint: Some(constraint),
                    ..
                }) if opts.for_validation_of_indexed_access_type => match constraint.normalize() {
                    Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: constraint_ty,
                        ..
                    }) => {
                        //
                        if constraint_ty.as_ref().type_eq(&*obj) {
                            return Ok(Type::any(DUMMY_SP, Default::default()));
                        }
                    }

                    _ => {}
                },

                _ => {}
            }
        }

        let obj_str = dump_type_as_string(&self.cm, &obj);

        // We use child scope to store type parameters.
        let mut res = self.with_scope_for_type_params(|analyzer: &mut Analyzer| -> VResult<_> {
            let mut ty = analyzer.access_property_inner(span, obj, prop, type_mode, id_ctx, opts)?.fixed();
            ty.assert_valid();
            ty = analyzer.expand_type_params_using_scope(ty)?;
            ty.assert_valid();
            Ok(ty)
        });

        if !self.is_builtin {
            res = res.with_context(|| {
                format!(
                    "tried to access property of an object ({}, id_ctx = {:?})\nProp={:?}",
                    dump_type_as_string(&self.cm, &obj),
                    id_ctx,
                    prop
                )
            })
        }
        let end = Instant::now();
        let dur = end - start;

        if dur >= Duration::from_micros(100) {
            let line_col = self.line_col(span);
            debug!(
                kind = "perf",
                op = "access_property",
                "({}) access_property: (time = {:?})",
                line_col,
                end - start
            );
        }

        let mut ty = res?;

        ty.assert_valid();

        let ty_str = dump_type_as_string(&self.cm, &ty);

        debug!(
            "[expr] Accessed property:\nObject: {}\nResult: {}\n{:?}",
            obj_str, ty_str, type_mode
        );

        if !self.is_builtin && ty.span().is_dummy() && !span.is_dummy() {
            ty.reposition(span);
        }

        Ok(ty)
    }

    fn access_property_inner(
        &mut self,
        span: Span,
        obj: &Type,
        prop: &Key,
        type_mode: TypeOfMode,
        id_ctx: IdCtx,
        opts: AccessPropertyOpts,
    ) -> VResult<Type> {
        if !self.is_builtin {
            debug_assert!(!span.is_dummy());

            debug!("access_property: obj = {}", dump_type_as_string(&self.cm, &obj));
        }

        let _stack = stack::track(span)?;

        let marks = self.marks();

        let computed = prop.is_computed();

        if computed && !opts.do_not_validate_type_of_computed_prop {
            let res: VResult<_> = try {
                let key_ty = prop.ty();
                let key_ty = self.normalize(Some(span), key_ty, Default::default())?;

                if key_ty.is_fn_type() || key_ty.is_constructor() {
                    Err(Error::CannotUseTypeAsIndexIndex { span })?
                }
            };

            res.report(&mut self.storage);
        }

        if self.ctx.in_opt_chain {
            if let Key::Private(p) = prop {
                return Err(Error::OptionalChainCannotContainPrivateIdentifier { span: p.span });
            }
        }

        let opts = AccessPropertyOpts {
            do_not_validate_type_of_computed_prop: false,
            ..opts
        };

        if id_ctx == IdCtx::Var {
            // TODO(kdy1): Use parent scope

            // Recursive method call
            if !computed
                && obj.is_this()
                && !self.ctx.in_computed_prop_name
                && (self.scope.is_this_ref_to_object_lit() || self.scope.is_this_ref_to_class())
            {
                if let Some(declaring) = &self.scope.declaring_prop() {
                    if prop == declaring.sym() {
                        return Ok(Type::any(span, Default::default()));
                    }
                }
            }

            match &obj {
                Type::This(this) if !self.ctx.in_computed_prop_name && self.scope.is_this_ref_to_object_lit() => {
                    if let Key::Computed(prop) = prop {
                        //
                        match &*prop.expr {
                            RExpr::Cond(..) => {
                                return Ok(Type::any(span, Default::default()));
                            }
                            _ => {}
                        }
                    }

                    // TODO(kdy1): Remove clone
                    let members = self.scope.object_lit_members().to_vec();
                    if let Some(mut v) = self.access_property_of_type_elements(span, &obj, prop, type_mode, &members, opts)? {
                        v.metadata_mut().infected_by_this_in_object_literal = true;
                        return Ok(v);
                    }

                    return Ok(Type::any(span, Default::default()));
                }

                Type::This(this) if !self.ctx.in_computed_prop_name && self.scope.is_this_ref_to_class() => {
                    if !computed {
                        // We are currently declaring a class.
                        for (_, member) in self.scope.class_members() {
                            match member {
                                // No-op, as constructor parameter properties are handled by
                                // Validate<Class>.
                                ClassMember::Constructor(_) => {}

                                ClassMember::Method(member @ Method { is_static: false, .. }) if member.key.type_eq(prop) => {
                                    return Ok(Type::Function(ty::Function {
                                        span: member.span,
                                        type_params: member.type_params.clone(),
                                        params: member.params.clone(),
                                        ret_ty: member.ret_ty.clone(),
                                        metadata: Default::default(),
                                    }));
                                }

                                ClassMember::Property(member @ ClassProperty { is_static: false, .. }) => {
                                    if member.key.type_eq(prop) {
                                        let ty = *member.value.clone().unwrap_or_else(|| box Type::any(span, Default::default()));
                                        let ty = match self.expand_top_ref(span, Cow::Borrowed(&ty), Default::default()) {
                                            Ok(new_ty) => {
                                                if new_ty.is_any() {
                                                    new_ty.into_owned()
                                                } else {
                                                    ty
                                                }
                                            }
                                            Err(..) => Type::any(span, Default::default()),
                                        };

                                        return Ok(ty);
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
                            let super_class = self.with_ctx(ctx).expand(
                                span,
                                super_class,
                                ExpandOpts {
                                    full: true,
                                    expand_union: true,
                                    ..Default::default()
                                },
                            )?;

                            if let Ok(v) = self.access_property(span, &super_class, prop, type_mode, IdCtx::Var, opts) {
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
                        let super_class = self.with_ctx(ctx).expand(
                            span,
                            super_class,
                            ExpandOpts {
                                full: true,
                                expand_union: true,
                                ..Default::default()
                            },
                        )?;

                        if let Ok(v) = self.access_property(span, &super_class, prop, type_mode, IdCtx::Var, opts) {
                            return Ok(v);
                        }
                    }

                    let prop_ty = prop.clone().computed().unwrap().ty;

                    warn!("Creating an indexed access type with this as the object");

                    // TODO(kdy1): Handle string literals like
                    //
                    // `this['props']`
                    return Ok(Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly: false,
                        obj_type: box Type::This(this.clone()),
                        index_type: prop_ty,
                        metadata: Default::default(),
                    }));
                }

                Type::StaticThis(StaticThis { span, metadata }) => {
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
                                        metadata: Default::default(),
                                    }));
                                }
                            }

                            stc_ts_types::ClassMember::Property(property @ ClassProperty { is_static: true, .. }) => {
                                if property.key.type_eq(prop) {
                                    return Ok(*property.value.clone().unwrap_or_else(|| {
                                        box Type::any(
                                            span.clone(),
                                            KeywordTypeMetadata {
                                                common: metadata.common,
                                                ..Default::default()
                                            },
                                        )
                                    }));
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

        match obj.normalize() {
            Type::This(..) => {
                let scope = if self.ctx.in_computed_prop_name {
                    self.scope.scope_of_computed_props()
                } else {
                    Some(&self.scope)
                };
                if let Some(this) = scope.and_then(|scope| scope.this().map(Cow::into_owned)) {
                    if this.is_this() {
                        unreachable!("this() should not be `this`")
                    }

                    return self
                        .access_property(span, &this, prop, type_mode, id_ctx, opts)
                        .context("tried to access property of `this`");
                }
            }
            _ => {}
        }

        let ctx = Ctx {
            preserve_ref: false,
            ignore_expand_prevention_for_top: true,
            ignore_expand_prevention_for_all: false,
            preserve_params: true,
            ..self.ctx
        };
        let mut obj = match obj.normalize() {
            Type::Conditional(..) | Type::Instance(..) => self.normalize(None, Cow::Borrowed(obj), Default::default())?,
            _ => Cow::Borrowed(obj),
        };
        if !self.is_builtin {
            obj.make_clone_cheap();
        }
        let mut obj = self
            .with_ctx(ctx)
            .expand(span, obj.into_owned(), Default::default())?
            .generalize_lit();
        if !self.is_builtin {
            obj.make_clone_cheap();
        }

        match obj.normalize() {
            Type::Lit(obj) => {
                // Even if literal generalization is prevented, it should be
                // expanded in this case.

                return self
                    .access_property(
                        span,
                        &Type::Keyword(KeywordType {
                            span: obj.span,
                            kind: match &obj.lit {
                                RTsLit::BigInt(_) => TsKeywordTypeKind::TsBigIntKeyword,
                                RTsLit::Number(_) => TsKeywordTypeKind::TsNumberKeyword,
                                RTsLit::Str(_) => TsKeywordTypeKind::TsStringKeyword,
                                RTsLit::Bool(_) => TsKeywordTypeKind::TsBooleanKeyword,
                                RTsLit::Tpl(_) => {
                                    unreachable!()
                                }
                            },
                            metadata: KeywordTypeMetadata {
                                common: obj.metadata.common,
                                ..Default::default()
                            },
                        }),
                        prop,
                        type_mode,
                        id_ctx,
                        opts,
                    )
                    .context("tried to access property of a type generalized from a literal");
            }

            Type::Symbol(..) => {
                return Err(Error::NoSuchProperty {
                    span,
                    obj: Some(box obj.clone()),
                    prop: Some(box prop.clone()),
                })
            }

            Type::Enum(ref e) => {
                // TODO(kdy1): Check if variant exists.

                match prop {
                    Key::Normal { sym, .. } => {
                        let has_such_member = e.members.iter().any(|m| match &m.id {
                            RTsEnumMemberId::Ident(i) => i.sym == *sym,
                            RTsEnumMemberId::Str(s) => s.value == *sym,
                        });
                        if !has_such_member {
                            return Err(Error::NoSuchEnumVariant { span, name: sym.clone() });
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
                            //                 return Ok(Type::Lit(LitType {
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
                            enum_name: e.id.clone().into(),
                            name: Some(sym.clone()),
                            metadata: Default::default(),
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
                                let new_obj_ty = Type::Lit(LitType {
                                    span,
                                    lit: match *v.val.clone() {
                                        RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                        RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                                        _ => unreachable!(),
                                    },
                                    metadata: Default::default(),
                                });
                                return self.access_property(span, &new_obj_ty, prop, type_mode, id_ctx, opts);
                            }
                        }
                        return Ok(Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            metadata: Default::default(),
                        }));
                    }

                    _ => {
                        if e.is_const {
                            return Err(Error::ConstEnumNonIndexAccess { span: prop.span() });
                        }

                        // TODO(kdy1): Validate type of enum

                        // enumBasics.ts says
                        //
                        // Reverse mapping of enum returns string name of property
                        return Ok(Type::Keyword(KeywordType {
                            span: prop.span().with_ctxt(SyntaxContext::empty()),
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            metadata: Default::default(),
                        }));
                    }
                }
            }

            // enum Foo { A }
            //
            // Foo.A.toString()
            Type::EnumVariant(EnumVariant {
                ref enum_name,
                ref name,
                span,
                metadata,
                ..
            }) => match self.find_type(enum_name)? {
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
                                        let new_obj_ty = Type::Lit(LitType {
                                            span: *span,
                                            lit: match *v.val.clone() {
                                                RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                                RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                                                _ => unreachable!(),
                                            },
                                            metadata: LitTypeMetadata {
                                                common: metadata.common,
                                                ..Default::default()
                                            },
                                        });
                                        return self.access_property(*span, &new_obj_ty, prop, type_mode, id_ctx, opts);
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
                        ClassMember::Property(ref class_prop @ ClassProperty { is_static: false, .. }) => {
                            if class_prop.key.is_private() {
                                self.storage.report(Error::CannotAccessPrivatePropertyFromOutside { span });
                                return Ok(Type::any(span, Default::default()));
                            }

                            if let Some(declaring) = self.scope.declaring_prop.as_ref() {
                                if class_prop.key == *declaring.sym() {
                                    return Err(Error::ReferencedInInit { span });
                                }
                            }

                            //
                            if self.key_matches(span, &class_prop.key, &prop, false) {
                                return Ok(match class_prop.value {
                                    Some(ref ty) => *ty.clone(),
                                    None => Type::any(span, Default::default()),
                                });
                            }
                        }
                        ClassMember::Method(ref mtd @ Method { is_static: false, .. }) => {
                            if mtd.key.is_private() {
                                self.storage.report(Error::CannotAccessPrivatePropertyFromOutside { span });
                                return Ok(Type::any(span, Default::default()));
                            }

                            if self.key_matches(span, &mtd.key, prop, false) {
                                if mtd.is_abstract {
                                    self.storage.report(Error::CannotAccessAbstractMember { span });
                                    return Ok(Type::any(span, Default::default()));
                                }

                                return Ok(Type::Function(stc_ts_types::Function {
                                    span: mtd.span,
                                    type_params: mtd.type_params.clone(),
                                    params: mtd.params.clone(),
                                    ret_ty: mtd.ret_ty.clone(),
                                    metadata: Default::default(),
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
                                    metadata: Default::default(),
                                }));
                            }
                        }

                        ClassMember::IndexSignature(index) => {
                            if index.params.len() == 1 {
                                // `[s: string]: boolean` can be indexed with a number.

                                let index_ty = &index.params[0].ty;

                                let prop_ty = prop.ty();

                                let indexed = (index_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword) && prop_ty.is_num())
                                    || self.assign(span, &mut Default::default(), &index_ty, &prop_ty).is_ok();

                                if indexed {
                                    return Ok(index
                                        .type_ann
                                        .clone()
                                        .map(|v| *v)
                                        .unwrap_or_else(|| Type::any(span, Default::default())));
                                }
                            }
                        }

                        _ => {}
                    }
                }

                // check for super class
                if let Some(super_ty) = &c.def.super_class {
                    let super_ty = self.instantiate_class(span, &super_ty)?;

                    if let Ok(v) = self.access_property(span, &super_ty, prop, type_mode, id_ctx, opts) {
                        return Ok(v);
                    }
                }

                let has_better_default = !opts.disallow_indexing_class_with_computed
                    && prop.is_computed()
                    && match prop.ty().normalize() {
                        // newWithSpreadES5.ts contains
                        //
                        //
                        // var i: C[][];
                        // new i["a-b"][1](1, 2, "string");
                        // new i["a-b"][1](1, 2, ...a);
                        // new i["a-b"][1](1, 2, ...a, "string");
                        //
                        //
                        // and it's not error.
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            ..
                        })
                        | Type::Lit(LitType { lit: RTsLit::Str(..), .. }) => true,
                        _ => false,
                    };

                if has_better_default {
                    return Ok(Type::any(span, Default::default()));
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
                            if let Ok(ty) = self.with_ctx(ctx).access_property(span, &constraint, prop, type_mode, id_ctx, opts) {
                                return Ok(ty);
                            }
                        }
                        _ => {}
                    }
                }

                let mut prop_ty = match prop {
                    Key::Computed(key) => key.ty.clone(),
                    Key::Normal { span, sym } => box Type::Lit(LitType {
                        span: span.with_ctxt(SyntaxContext::empty()),
                        lit: RTsLit::Str(RStr {
                            span: *span,
                            value: sym.clone(),
                            raw: None,
                        }),
                        metadata: Default::default(),
                    }),
                    Key::Num(n) => box Type::Lit(LitType {
                        span: n.span.with_ctxt(SyntaxContext::empty()),
                        lit: RTsLit::Number(n.clone()),
                        metadata: Default::default(),
                    }),
                    Key::BigInt(n) => box Type::Lit(LitType {
                        span: n.span.with_ctxt(SyntaxContext::empty()),
                        lit: RTsLit::BigInt(n.clone()),
                        metadata: Default::default(),
                    }),
                    Key::Private(..) => {
                        unreachable!()
                    }
                };

                if is_str_lit_or_union(&prop_ty) {
                    prevent_generalize(&mut prop_ty);
                }

                warn!("Creating an indexed access type with type parameter as the object");

                return Ok(Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly: false,
                    obj_type: box obj,
                    index_type: prop_ty,
                    metadata: Default::default(),
                }));
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => {
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    metadata: Default::default(),
                }));
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                debug_assert!(!span.is_dummy());
                return Err(Error::Unknown { span });
            }

            Type::Keyword(KeywordType { kind, .. }) if !self.is_builtin => {
                match prop {
                    Key::Computed(prop) => match (*kind, prop.ty.normalize()) {
                        (
                            TsKeywordTypeKind::TsObjectKeyword,
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                ..
                            }),
                        ) => {
                            if self.rule().no_implicit_any && !self.rule().suppress_implicit_any_index_errors {
                                self.storage.report(Error::ImplicitAnyBecauseIndexTypeIsWrong { span });
                            }

                            return Ok(Type::any(span, Default::default()));
                        }
                        _ => {}
                    },
                    _ => {}
                }

                let word = match kind {
                    TsKeywordTypeKind::TsStringKeyword => js_word!("String"),
                    TsKeywordTypeKind::TsNumberKeyword => js_word!("Number"),
                    TsKeywordTypeKind::TsBooleanKeyword => js_word!("Boolean"),
                    TsKeywordTypeKind::TsObjectKeyword => js_word!("Object"),
                    TsKeywordTypeKind::TsSymbolKeyword => js_word!("Symbol"),
                    _ => {
                        return Err(Error::NoSuchProperty {
                            span: prop.span(),
                            obj: Some(box obj),
                            prop: Some(box prop.clone()),
                        });
                    }
                };
                let interface = self.env.get_global_type(span, &word)?;

                let err = match self.access_property(span, &interface, prop, type_mode, id_ctx, opts) {
                    Ok(v) => return Ok(v),
                    Err(err) => err,
                };
                if *kind == TsKeywordTypeKind::TsObjectKeyword && !self.ctx.disallow_unknown_object_property {
                    return Ok(Type::any(span.with_ctxt(SyntaxContext::empty()), Default::default()));
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
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Lit(LitType {
                            lit: RTsLit::Number(..), ..
                        }) => return Ok(elem_type.clone()),

                        _ => {}
                    }
                }
                if let Key::Num(n) = prop {
                    return Ok(elem_type.clone());
                }

                let array_ty = self.env.get_global_type(span, &js_word!("Array"))?;

                let has_better_default = !opts.disallow_indexing_array_with_string
                    && match prop.ty().normalize() {
                        // newWithSpreadES5.ts contains
                        //
                        //
                        // var i: C[][];
                        // new i["a-b"][1](1, 2, "string");
                        // new i["a-b"][1](1, 2, ...a);
                        // new i["a-b"][1](1, 2, ...a, "string");
                        //
                        //
                        // and it's not error.
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            ..
                        })
                        | Type::Lit(LitType { lit: RTsLit::Str(..), .. }) => true,
                        _ => false,
                    };

                return self
                    .with_ctx(ctx)
                    .access_property(
                        span,
                        &array_ty,
                        prop,
                        type_mode,
                        id_ctx,
                        AccessPropertyOpts {
                            disallow_creating_indexed_type_from_ty_els: opts.disallow_creating_indexed_type_from_ty_els
                                || has_better_default,
                            ..opts
                        },
                    )
                    .or_else(|err| {
                        if !has_better_default {
                            return Err(err);
                        }
                        match prop.ty().normalize() {
                            // newWithSpreadES5.ts contains
                            //
                            //
                            // var i: C[][];
                            // new i["a-b"][1](1, 2, "string");
                            // new i["a-b"][1](1, 2, ...a);
                            // new i["a-b"][1](1, 2, ...a, "string");
                            //
                            //
                            // and it's not error.
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                ..
                            })
                            | Type::Lit(LitType { lit: RTsLit::Str(..), .. }) => Ok(Type::any(span, Default::default())),
                            _ => Err(err),
                        }
                    });
            }

            Type::Interface(Interface { ref body, extends, .. }) => {
                if let Ok(Some(v)) = self.access_property_of_type_elements(span, &obj, prop, type_mode, body, opts) {
                    return Ok(v);
                }

                for super_ty in extends {
                    let obj = self.type_of_ts_entity_name(span, self.ctx.module_id, &super_ty.expr, super_ty.type_args.as_deref())?;

                    let obj = self
                        .instantiate_class(span, &obj)
                        .context("tried to instantiate parents of an interface to access property")?;

                    // TODO(kdy1): Check if multiple interface has same property.
                    if let Ok(ty) = self.access_property(span, &obj, prop, type_mode, id_ctx, opts) {
                        return Ok(ty);
                    }
                }

                // TODO(kdy1): Check parent interfaces

                if body.iter().any(|el| el.is_constructor()) {
                    // Constructor extends prototype of `Function` (global interface)
                    if let Ok(ty) = self.access_property(
                        span,
                        &Type::Ref(Ref {
                            span: span.with_ctxt(Default::default()),
                            type_name: RTsEntityName::Ident(RIdent::new(js_word!("Function"), DUMMY_SP)),
                            type_args: None,
                            metadata: Default::default(),
                        }),
                        prop,
                        type_mode,
                        id_ctx,
                        opts,
                    ) {
                        return Ok(ty);
                    }
                }

                if prop.is_computed() {
                    return Ok(Type::any(span, Default::default()));
                }

                return Err(Error::NoSuchProperty {
                    span,
                    obj: Some(box obj),
                    prop: Some(box prop.clone()),
                });
            }

            Type::TypeLit(TypeLit { ref members, metadata, .. }) => {
                if let Some(v) = self.access_property_of_type_elements(span, &obj, prop, type_mode, members, opts)? {
                    return Ok(v);
                }

                if members.iter().any(|el| el.is_constructor()) {
                    // Constructor extends prototype of `Function` (global interface)
                    if let Ok(ty) = self.access_property(
                        span,
                        &Type::Ref(Ref {
                            span: span.with_ctxt(Default::default()),
                            type_name: RTsEntityName::Ident(RIdent::new(js_word!("Function"), DUMMY_SP)),
                            type_args: None,
                            metadata: Default::default(),
                        }),
                        prop,
                        type_mode,
                        id_ctx,
                        opts,
                    ) {
                        return Ok(ty);
                    }
                }

                if type_mode == TypeOfMode::LValue {
                    return Ok(Type::any(span, Default::default()));
                }

                if members.iter().any(|e| e.is_call()) {
                    let obj = self.env.get_global_type(span, &js_word!("Function"))?;
                    if let Ok(v) = self.access_property(span, &obj, prop, type_mode, IdCtx::Var, opts) {
                        return Ok(v);
                    }
                }

                {
                    let obj = self.env.get_global_type(span, &js_word!("Object"))?;
                    if let Ok(v) = self.access_property(span, &obj, prop, type_mode, IdCtx::Var, opts) {
                        return Ok(v);
                    }
                }

                if !opts.disallow_inexact && metadata.inexact {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        metadata: Default::default(),
                    }));
                }

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

                let is_all_tuple = types.iter().all(|ty| ty.is_tuple());
                let use_undefined_for_tuple_index_error =
                    opts.use_undefined_for_tuple_index_error || (type_mode == TypeOfMode::LValue && is_all_tuple);

                for ty in types {
                    if !self.rule().strict_null_checks || self.ctx.in_opt_chain {
                        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                            continue;
                        }
                    }

                    match self.access_property(
                        span,
                        ty,
                        prop,
                        type_mode,
                        id_ctx,
                        AccessPropertyOpts {
                            use_undefined_for_tuple_index_error,
                            ..opts
                        },
                    ) {
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
                        if is_all_tuple && errors.len() != types.len() {
                            tys.push(Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                metadata: Default::default(),
                            }));
                            tys.dedup_type();
                            let ty = Type::union(tys);
                            ty.assert_valid();
                            return Ok(ty);
                        }

                        print_backtrace();
                        return Err(Error::NoSuchProperty {
                            span,
                            obj: Some(box obj),
                            prop: Some(box prop.clone()),
                        });
                    }
                }

                tys.dedup_type();

                // TODO(kdy1): Validate that the ty has same type instead of returning union.
                let ty = Type::union(tys);
                ty.assert_valid();
                return Ok(ty);
            }

            Type::Tuple(Tuple { ref elems, .. }) => {
                match prop {
                    Key::Num(n) => {
                        let v = n.value.round() as i64;

                        if v < 0 {
                            return Err(Error::NegativeTupleIndex {
                                span: n.span(),
                                index: v,
                                len: elems.len() as u64,
                            });
                        }

                        if (v as usize) + 1 >= elems.len() {
                            match elems.last() {
                                Some(elem) => {
                                    match elem.ty.normalize() {
                                        Type::Rest(rest_ty) => {
                                            if let Ok(ty) = self.access_property(
                                                span,
                                                &rest_ty.ty,
                                                &Key::Num(RNumber {
                                                    span: n.span,
                                                    value: (v + 1i64 - (elems.len() as i64)) as _,
                                                    raw: None,
                                                }),
                                                type_mode,
                                                id_ctx,
                                                opts,
                                            ) {
                                                return Ok(ty);
                                            }

                                            // debug_assert!(rest_ty.ty.is_clone_cheap());
                                            return Ok(*rest_ty.ty.clone());
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }

                        if v as usize >= elems.len() {
                            if opts.use_undefined_for_tuple_index_error {
                                return Ok(Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                    metadata: Default::default(),
                                }));
                            }

                            if let TypeOfMode::LValue = type_mode {
                                self.storage.report(
                                    Error::TupleIndexError {
                                        span: n.span(),
                                        index: v,
                                        len: elems.len() as u64,
                                    }
                                    .context("returning undefined because it's l-value context"),
                                );
                                return Ok(Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                    metadata: Default::default(),
                                }));
                            }

                            return Err(Error::TupleIndexError {
                                span: n.span(),
                                index: v,
                                len: elems.len() as u64,
                            }
                            .context("r-value context"));
                        }

                        return Ok(*elems[v as usize].ty.clone());
                    }

                    Key::Normal {
                        sym: js_word!("length"), ..
                    } => {
                        if elems.iter().any(|el| el.ty.is_rest()) {
                            return Ok(Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                metadata: Default::default(),
                            }));
                        }

                        return Ok(Type::Lit(LitType {
                            span,
                            lit: RTsLit::Number(RNumber {
                                span,
                                value: elems.len() as _,
                                raw: None,
                            }),
                            metadata: Default::default(),
                        }));
                    }

                    _ => {}
                }

                let mut types = elems.iter().map(|e| *e.ty.clone()).collect::<Vec<_>>();
                types.dedup_type();
                let obj = Type::Array(Array {
                    span,
                    elem_type: box Type::union(types),
                    metadata: Default::default(),
                });

                return self.access_property(span, &obj, prop, type_mode, id_ctx, opts);
            }

            Type::ClassDef(cls) => {
                match prop {
                    Key::Normal { sym, .. } if *sym == *"prototype" => return self.create_prototype_of_class_def(cls).map(Type::TypeLit),
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
                            // TODO(kdy1): normalized string / ident
                            if self.key_matches(span, &p.key, prop, false) {
                                if let Some(ref ty) = p.value {
                                    return Ok(*ty.clone());
                                }

                                return Ok(Type::any(p.key.span().with_ctxt(SyntaxContext::empty()), Default::default()));
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
                                    metadata: Default::default(),
                                }));
                            }
                        }
                        _ => {}
                    }
                }

                if let Some(super_ty) = &cls.super_class {
                    if let Ok(v) = self.access_property(span, &super_ty, prop, type_mode, id_ctx, opts) {
                        return Ok(v);
                    }
                }

                // Classes extends prototype of `Function` (global interface)
                if let Ok(ty) = self.access_property(
                    span,
                    &Type::Ref(Ref {
                        span: span.with_ctxt(Default::default()),
                        type_name: RTsEntityName::Ident(RIdent::new(js_word!("Function"), DUMMY_SP)),
                        type_args: None,
                        metadata: Default::default(),
                    }),
                    prop,
                    type_mode,
                    id_ctx,
                    opts,
                ) {
                    return Ok(ty);
                }

                return Err(Error::NoSuchPropertyInClass {
                    span,
                    class_name: cls.name.clone(),
                    prop: prop.clone(),
                });
            }

            Type::Module(ty::Module { name, ref exports, .. }) => {
                match id_ctx {
                    IdCtx::Type => {
                        if let Key::Normal { sym, .. } = prop {
                            if let Some(types) = exports.types.get(sym).cloned() {
                                if types.len() == 1 {
                                    return Ok(types.into_iter().next().unwrap());
                                }
                                return Ok(Type::Intersection(Intersection {
                                    span,
                                    types,
                                    metadata: Default::default(),
                                }));
                            }
                        }
                    }
                    IdCtx::Var => {
                        if let Key::Normal { sym, .. } = prop {
                            if let Some(item) = exports.vars.get(sym) {
                                return Ok(item.clone());
                            }
                        }

                        if let Key::Normal { sym, .. } = prop {
                            if let Some(types) = exports.types.get(sym) {
                                for ty in types.iter() {
                                    if ty.is_module() {
                                        return Ok(ty.clone());
                                    }
                                }
                            }
                        }
                    }
                }

                print_backtrace();
                // No property found
                return Err(Error::NoSuchPropertyInModule {
                    span,
                    name: box name.clone(),
                });
            }

            Type::This(..) => {
                // TODO(kdy1): Use parent scope in computed property names.
                if let Some(this) = self.scope.this().map(|this| this.into_owned()) {
                    if self.ctx.in_computed_prop_name {
                        self.storage.report(Error::CannotReferenceThisInComputedPropName { span });
                        // Return any to prevent other errors
                        return Ok(Type::any(span, Default::default()));
                    }

                    if this.normalize_instance().is_this() {
                        unreachable!("this() should not be `this`")
                    }

                    return self.access_property(span, &this, prop, type_mode, id_ctx, opts);
                } else if self.ctx.in_argument {
                    // We will adjust `this` using information from callee.
                    return Ok(Type::any(span, Default::default()));
                }

                let scope = if self.ctx.in_computed_prop_name {
                    self.scope.scope_of_computed_props()
                } else {
                    self.scope.first(|scope| match scope.kind() {
                        ScopeKind::TypeParams | ScopeKind::Flow => false,
                        _ => true,
                    })
                };

                match scope.map(|scope| scope.kind()) {
                    Some(ScopeKind::Fn) => {
                        // TODO
                        return Ok(Type::any(span, Default::default()));
                    }
                    None => {
                        // Global this
                        return Ok(Type::any(span, Default::default()));
                    }
                    kind => {
                        unimplemented!("access property of this to {:?}", kind)
                    }
                }
            }

            Type::Intersection(Intersection { ref types, .. }) => {
                // TODO(kdy1): Verify if multiple type has field
                let mut new = vec![];
                for ty in types {
                    let ty = self.expand_top_ref(span, Cow::Borrowed(ty), Default::default())?;
                    if let Some(v) = self.access_property(span, &ty, prop, type_mode, id_ctx, opts).ok() {
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
                    let mut ty = new.into_iter().next().unwrap();
                    ty.respan(span);
                    return Ok(ty);
                }

                new.dedup_type();

                let ty = Type::Intersection(Intersection {
                    span,
                    types: new,
                    metadata: Default::default(),
                })
                .fixed();
                // ty.respan(span);
                return Ok(ty);
            }

            Type::Mapped(m) => {
                //
                let mut constraint = self::constraint_reducer::reduce(m);
                constraint.make_clone_cheap();
                // If type of prop is equal to the type of index signature, it's
                // index access.

                match constraint.as_ref().map(Type::normalize) {
                    Some(Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: box Type::Array(..),
                        ..
                    })) => {
                        if let Ok(obj) = self.env.get_global_type(span, &js_word!("Array")) {
                            return self.access_property(span, &obj, prop, type_mode, id_ctx, opts);
                        }
                    }

                    Some(index @ Type::Keyword(..)) | Some(index @ Type::Param(..)) => {
                        // {
                        //     [P in string]: number;
                        // };
                        if let Ok(()) = self.assign(span, &mut Default::default(), &index, &prop.ty()) {
                            // We handle `Partial<string>` at here.
                            let ty = m.ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(span, Default::default()));

                            let ty = match m.optional {
                                Some(TruePlusMinus::Plus) | Some(TruePlusMinus::True) => {
                                    let undefined = Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                        metadata: Default::default(),
                                    });
                                    let mut types = vec![undefined, ty];
                                    types.dedup_type();

                                    Type::union(types)
                                }
                                Some(TruePlusMinus::Minus) => self.apply_type_facts_to_type(TypeFacts::NEUndefined | TypeFacts::NENull, ty),
                                _ => ty,
                            };
                            return Ok(ty);
                        }
                    }

                    _ => {}
                }

                let expanded = self
                    .expand_mapped(span, m)
                    .context("tried to expand a mapped type to access property")?;

                if let Some(obj) = &expanded {
                    return self.access_property(span, obj, prop, type_mode, id_ctx, opts);
                }

                match constraint.as_ref().map(Type::normalize) {
                    Some(Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty,
                        ..
                    })) => {
                        // Check if we can index the object with given key.
                        if let Ok(index_type) = self.keyof(span, &ty) {
                            if let Ok(()) = self.assign_with_opts(
                                &mut Default::default(),
                                &index_type,
                                &prop.ty(),
                                AssignOpts {
                                    span,
                                    ..Default::default()
                                },
                            ) {
                                return Ok(m.ty.clone().map(|v| *v).unwrap_or_else(|| Type::any(span, Default::default())));
                            }
                        }
                    }
                    _ => {}
                }

                warn!("Creating an indexed access type with mapped type as the object");

                return Ok(Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly: false,
                    obj_type: box obj,
                    index_type: box prop.ty().into_owned(),
                    metadata: Default::default(),
                }));
            }

            Type::Ref(r) => {
                if let Key::Computed(computed) = prop {
                    match obj.normalize() {
                        Type::Param(..) => {
                            let index_type = computed.ty.clone();

                            warn!("Creating an indexed access type with a type parameter as the object");

                            // Return something like SimpleDBRecord<Flag>[Flag];
                            return Ok(Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly: false,
                                obj_type: box obj,
                                index_type,
                                metadata: Default::default(),
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
                                        &Type::StaticThis(StaticThis {
                                            span,
                                            metadata: Default::default(),
                                        }),
                                        prop,
                                        type_mode,
                                        id_ctx,
                                        opts,
                                    );
                                }
                            }
                        }
                    }
                }

                let expand_opts = ExpandOpts {
                    generic: ExpandGenericOpts { ..Default::default() },
                    ..Default::default()
                };

                let obj = self
                    .expand_top_ref(span, Cow::Borrowed(&obj), expand_opts)
                    .context("tried to expand reference to access property")?;

                return self.access_property(span, &obj, prop, type_mode, id_ctx, opts);
            }

            Type::IndexedAccessType(..) => {
                let index_type = match prop {
                    Key::Computed(c) => c.ty.clone(),
                    _ => {
                        let mut prop_ty = box prop.ty().into_owned();
                        prevent_generalize(&mut prop_ty);

                        prop_ty
                    }
                };

                let ty = Type::IndexedAccessType(IndexedAccessType {
                    span,
                    obj_type: box obj,
                    readonly: false,
                    index_type,
                    metadata: Default::default(),
                });
                return Ok(ty);
            }

            Type::Query(QueryType {
                expr: box QueryExpr::TsEntityName(name),
                ..
            }) => {
                let obj = self.type_of_ts_entity_name(span, self.ctx.module_id, &name.clone().into(), None)?;
                return self.access_property(span, &obj, prop, type_mode, id_ctx, opts);
            }

            Type::Function(f) if type_mode == TypeOfMode::RValue => {
                // Use builtin type `Function`
                let interface = self.env.get_global_type(f.span, &js_word!("Function"))?;
                print_backtrace();
                return self.access_property(span, &interface, prop, type_mode, id_ctx, opts);
            }

            Type::Constructor(c) => match prop {
                Key::Num(_) | Key::BigInt(_) => return Ok(Type::any(span, Default::default())),
                _ => {
                    return self
                        .access_property(span, &c.type_ann, prop, type_mode, id_ctx, opts)
                        .context("tried to access property of the return type of a constructor")
                }
            },

            Type::Conditional(..) => match prop {
                Key::Num(..) => {
                    return Ok(Type::TypeLit(TypeLit {
                        span,
                        members: Default::default(),
                        metadata: Default::default(),
                    }))
                }
                _ => {}
            },

            Type::Rest(rest) => {
                // I'm not sure if this impl is correct, so let's print a log for debugging.
                warn!(
                    "[expr] accessing property of rest type({})",
                    dump_type_as_string(&self.cm, &rest.ty)
                );
                return self
                    .access_property(span, &rest.ty, prop, type_mode, id_ctx, opts)
                    .context("tried to access property of a rest type");
            }

            Type::Function(..) => {
                // Classes extends prototype of `Function` (global interface)
                if let Ok(ty) = self.access_property(
                    span,
                    &Type::Ref(Ref {
                        span: span.with_ctxt(Default::default()),
                        type_name: RTsEntityName::Ident(RIdent::new(js_word!("Function"), DUMMY_SP)),
                        type_args: None,
                        metadata: Default::default(),
                    }),
                    prop,
                    type_mode,
                    id_ctx,
                    opts,
                ) {
                    return Ok(ty);
                }

                // Function does not have information about types of properties.
                match type_mode {
                    TypeOfMode::LValue => return Ok(Type::any(span, Default::default())),
                    TypeOfMode::RValue => {}
                }
            }

            Type::Operator(Operator {
                op: TsTypeOperatorOp::ReadOnly,
                ty,
                ..
            }) => {
                if let TypeOfMode::RValue = type_mode {
                    return self.access_property(span, &ty, prop, type_mode, id_ctx, opts);
                }
            }

            Type::Optional(OptionalType { ty, .. }) => {
                if self.rule().strict_null_checks {
                    return Err(Error::ObjectIsPossiblyUndefined { span });
                }

                return self.access_property(span, &ty, prop, type_mode, id_ctx, opts);
            }

            _ => {}
        }

        print_backtrace();
        unimplemented!(
            "access_property(MemberExpr):\nObject: {:?}\nProp: {:?}\nPath: {}",
            obj,
            prop,
            self.storage.path(self.ctx.module_id)
        );
    }

    /// TODO(kdy1): Clarify this.
    fn type_to_query_if_required(&mut self, span: Span, i: &RIdent, ty: Type) -> Type {
        if self.scope.is_in_call() {
            return ty;
        }

        let span = span.with_ctxt(SyntaxContext::empty());

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
                        metadata: Default::default(),
                    });
                }
                return ty;
            }
            _ => return ty,
        }
    }

    /// Expand type paramters using `type_args`.
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn expand_generics_with_type_args(&mut self, span: Span, ty: Type, type_args: &TypeParamInstantiation) -> VResult<Type> {
        match ty.normalize() {
            Type::Interface(Interface { type_params, .. })
            | Type::Alias(Alias { type_params, .. })
            | Type::Class(Class {
                def: box ClassDef { type_params, .. },
                ..
            })
            | Type::ClassDef(ClassDef { type_params, .. }) => {
                if let Some(type_params) = type_params {
                    let mut params = HashMap::default();

                    for (param, arg) in type_params.params.iter().zip(type_args.params.iter()) {
                        params.insert(param.name.clone(), arg.clone());
                    }

                    return self.expand_type_params(&params, ty.clone(), Default::default());
                }
            }
            _ => {
                // TODO(kdy1): Report an error.
            }
        }

        Ok(ty)
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn type_of_name(
        &mut self,
        span: Span,
        name: &[Id],
        type_mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
    ) -> VResult<Type> {
        assert!(name.len() > 0, "Cannot determine type of empty name");

        let mut id: RIdent = name[0].clone().into();
        id.span.lo = span.lo;
        id.span.hi = span.hi;

        match name.len() {
            1 => {
                return self
                    .type_of_var(&id, TypeOfMode::RValue, None)
                    .context("tried to get type of a name with len == 1");
            }

            _ => {
                let last_id = name.last().unwrap();

                let obj = self
                    .type_of_name(span, &name[..name.len() - 1], type_mode, type_args)
                    .context("tried to get type of &names[..-1]")?;

                let ty = self
                    .access_property(
                        span,
                        &obj,
                        &Key::Normal {
                            span: id.span,
                            sym: last_id.sym().clone(),
                        },
                        type_mode,
                        IdCtx::Var,
                        AccessPropertyOpts { ..Default::default() },
                    )
                    .context("tried to access property to calculate type of name")?;

                Ok(ty)
            }
        }
    }

    /// Returned type reflects conditional type facts.
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn type_of_var(&mut self, i: &RIdent, type_mode: TypeOfMode, type_args: Option<&TypeParamInstantiation>) -> VResult<Type> {
        let span = i.span();
        let id: Id = i.into();
        let name: Name = i.into();

        if self.scope.is_declaring_fn(&id) {
            // We will expand this type query to proper type while calculating returns types
            // of a function.
            return Ok(Type::Query(QueryType {
                // TODO(kdy1): This is a regession.
                span: span.with_ctxt(SyntaxContext::empty()),
                expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(id.into())),
                metadata: Default::default(),
            }));
        }

        let mut modules = vec![];
        let mut ty = self.type_of_raw_var(i, type_mode)?;
        if type_mode == TypeOfMode::LValue && (ty.is_class_def() || ty.is_enum_type()) {
            if ty.is_enum_type() {
                return Err(Error::CannotAssignToEnum { span });
            }
            if ty.is_class_def() {
                return Err(Error::CannotAssignToClass { span });
            }

            return Err(Error::NotVariable {
                span,
                left: span,
                ty: Some(box ty.clone()),
            });
        }
        ty.assert_valid();
        if let Some(type_args) = type_args {
            ty = self.expand_generics_with_type_args(span, ty, type_args)?;
            ty.fix();
        }
        let mut need_intersection = true;

        // TODO(kdy1): Change return type of type_of_raw_var to Option and inject module
        // from here.
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

        if let TypeOfMode::LValue = type_mode {
            if let Some(types) = self.find_type(&id)? {
                for ty in types {
                    match ty.normalize() {
                        Type::Module(..) => {
                            return Err(Error::NotVariable {
                                span,
                                left: span,
                                ty: Some(box ty.normalize().clone()),
                            })
                        }
                        _ => {}
                    }
                }
            }
        }

        if self.ctx.allow_module_var && need_intersection {
            if let Some(types) = self.find_type(&id)? {
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
        if !self.is_builtin {
            ty = self.apply_type_facts(&name, ty);
        }

        ty.assert_valid();

        ty = self.type_to_query_if_required(span, i, ty);

        ty.assert_valid();

        if !self.is_builtin {
            self.exclude_types_using_fact(span, &name, &mut ty);
        }

        ty.assert_valid();

        if !modules.is_empty() {
            modules.push(ty);
            ty = Type::Intersection(Intersection {
                span: i.span.with_ctxt(SyntaxContext::empty()),
                types: modules,
                metadata: Default::default(),
            });
            ty.fix();
            ty.make_cheap();
        }

        debug!("type_of_var({:?}): {:?}", id, ty);

        ty.reposition(i.span);

        {
            ty.metadata_mut().resolved_from_var = true;
        }

        Ok(ty)
    }

    /// Returned type does not reflects conditional type facts. (like Truthy /
    /// exclusion)
    fn type_of_raw_var(&mut self, i: &RIdent, type_mode: TypeOfMode) -> VResult<Type> {
        info!("({}) type_of_raw_var({})", self.scope.depth(), Id::from(i));

        // See documentation on Analyzer.cur_module_name to understand what we are doing
        // here.
        if let Some(cur_module) = self.scope.current_module_name() {
            let ty = self.find_type(&cur_module)?;
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

        let span = i.span().with_ctxt(SyntaxContext::empty());

        if let Some(ref cls_name) = self.scope.this_class_name {
            if *cls_name == i {
                warn!("Creating ref because we are currently defining a class: {}", i.sym);
                return Ok(Type::StaticThis(StaticThis {
                    span,
                    metadata: Default::default(),
                }));
            }
        }

        // At here, it cannot be a declared variable.
        if self.env.target() <= EsVersion::Es5 {
            match i.sym {
                js_word!("arguments") => {
                    // `arguments` cannot be used as implicit variable if target <= ES5
                    let arguments_point_to_arrow = Some(true)
                        == self.scope.matches(|scope| {
                            if scope.is_root() {
                                return Some(false);
                            }

                            match scope.kind() {
                                ScopeKind::ArrowFn => Some(true),
                                ScopeKind::Fn | ScopeKind::Constructor | ScopeKind::Method { .. } => Some(false),
                                _ => None,
                            }
                        });
                    let arguments_points_async_fn = Some(true) == {
                        let ctx = self.ctx;

                        self.scope.matches(|scope| {
                            if scope.is_root() {
                                return Some(false);
                            }

                            match scope.kind() {
                                ScopeKind::Fn => Some(ctx.in_async),
                                ScopeKind::ArrowFn | ScopeKind::Constructor | ScopeKind::Method { .. } => Some(false),
                                _ => None,
                            }
                        })
                    };

                    let is_argument_defined_in_current_scope = self.scope.vars.contains_key(&i.clone().into());

                    if !self.scope.is_arguments_implicitly_defined() {
                        self.storage.report(Error::InvalidUseOfArgumentsInEs3OrEs5 { span })
                    } else if arguments_point_to_arrow && !is_argument_defined_in_current_scope {
                        self.storage.report(Error::InvalidUseOfArgumentsInEs3OrEs5 { span });
                        return Ok(Type::any(span, Default::default()));
                    } else if arguments_points_async_fn && !is_argument_defined_in_current_scope {
                        self.storage.report(Error::ArgumentsCannotBeUsedInAsyncFnInEs3OrEs5 { span });
                        return Ok(Type::any(span, Default::default()));
                    }
                }
                _ => {}
            }
        }

        match i.sym {
            js_word!("undefined") => {
                match type_mode {
                    TypeOfMode::LValue => self.storage.report(Error::NotVariable {
                        span,
                        left: span,
                        ty: None,
                    }),
                    TypeOfMode::RValue => {}
                }
                return Ok(Type::undefined(span, Default::default()));
            }
            js_word!("void") => return Ok(Type::any(span, Default::default())),
            js_word!("eval") => match type_mode {
                TypeOfMode::LValue => return Err(Error::CannotAssignToFunction { span }),
                _ => {}
            },
            _ => {}
        }

        if i.sym == js_word!("require") {
            unreachable!("typeof(require('...'))");
        }

        if let Some(ty) = self.find_imported_var(&i.into())? {
            debug!("({}) type_of({}): resolved import", self.scope.depth(), Id::from(i));
            return Ok(ty.clone());
        }

        if let Some(v) = self.scope.vars.get(&i.into()) {
            if let VarKind::Fn = v.kind {
                if let TypeOfMode::LValue = type_mode {
                    return Err(Error::CannotAssignToFunction { span });
                }
            }

            debug!("found var with name");
            match type_mode {
                TypeOfMode::LValue => {
                    if let Some(ty) = &v.ty {
                        ty.assert_valid();

                        debug!("Type of var: {:?}", ty);
                        return Ok(ty.clone());
                    }
                }

                TypeOfMode::RValue => {
                    if let Some(ty) = &v.actual_ty {
                        ty.assert_valid();

                        debug!("Type of var: {:?}", ty);
                        return Ok(ty.clone());
                    }
                }
            }
        }

        if let Some(ty) = self.find_var_type(&i.into(), type_mode) {
            ty.assert_valid();

            let ty_str = dump_type_as_string(&self.cm, &ty);
            debug!("find_var_type returned a type: {}", ty_str);
            let mut ty = ty.into_owned();
            if self.scope.kind().allows_respanning() {
                if self.is_implicitly_typed(&ty) {
                    ty.metadata_mut().implicit = true;
                }
                if !self.may_generalize(&ty) {
                    ty.metadata_mut().prevent_generalization = true;
                }
                ty.respan(span);
            }
            debug!("{:?}", ty);
            return Ok(ty);
        }

        if let Some(_var) = self.find_var(&i.into()) {
            // TODO(kdy1): Infer type or use type hint to handle
            //
            // let id: (x: Foo) => Foo = x => x;
            //
            return Ok(Type::any(span, Default::default()));
        }

        if !self.is_builtin {
            if let Ok(ty) = self.env.get_global_var(span, &i.sym) {
                if self.ctx.report_error_for_non_local_vars {
                    self.storage.report(Error::CannotExportNonLocalVar { span: i.span });
                }

                return Ok(ty);
            }
        }

        // Check `declaring` before checking variables.
        if self.scope.is_declaring(&i.into()) {
            debug!("({}) reference in initialization: {}", self.scope.depth(), i.sym);

            // Report an error if a variable is used before initialization.
            (|| {
                match self.ctx.var_kind {
                    VarDeclKind::Let | VarDeclKind::Const => {}
                    _ => return,
                }

                // If a method or a function scope exists before the scope declaring the
                // variable, it means current statement will be evaluated after the variable is
                // initialized.
                if let Some(scope) = self.scope.first(|scope| {
                    if scope.declaring.contains(&i.into()) {
                        return true;
                    }

                    match scope.kind() {
                        ScopeKind::Method { .. } | ScopeKind::Fn | ScopeKind::ArrowFn | ScopeKind::Constructor => {
                            return true;
                        }
                        _ => false,
                    }
                }) {
                    if !scope.is_root() {
                        match scope.kind() {
                            ScopeKind::Method { .. } | ScopeKind::Fn | ScopeKind::ArrowFn | ScopeKind::Constructor => return,
                            _ => {}
                        }
                    }
                }

                self.storage.report(Error::BlockScopedVarUsedBeforeInit { span })
            })();

            if self.ctx.allow_ref_declaring {
                if self.rule().no_implicit_any {
                    self.storage.report(Error::ImplicitAnyBecauseOfSelfRef { span });
                }

                return Ok(Type::any(span, Default::default()));
            } else {
                return Err(Error::ReferencedInInit { span });
            }
        }

        // At here, it cannot be a declared variable.
        match i.sym {
            js_word!("arguments") => {
                if self.env.target() <= EsVersion::Es5 {
                    // `arguments` cannot be used as implicit variable if target <= ES5
                    let arguments_point_to_arrow = Some(true)
                        == self.scope.matches(|scope| {
                            if scope.is_root() {
                                return Some(false);
                            }

                            match scope.kind() {
                                ScopeKind::ArrowFn => Some(true),
                                ScopeKind::Fn | ScopeKind::Constructor | ScopeKind::Method { .. } => Some(false),
                                _ => None,
                            }
                        });

                    if !self.scope.is_arguments_implicitly_defined() || arguments_point_to_arrow {
                        self.storage.report(Error::InvalidUseOfArgumentsInEs3OrEs5 { span })
                    }
                } else {
                    if !self.scope.is_arguments_implicitly_defined() {
                        self.storage.report(Error::NoSuchVar { span, name: i.into() })
                    }
                }

                return Ok(Type::any(span, Default::default()));
            }
            _ => {}
        }

        if let Ok(Some(types)) = self.find_type(self.ctx.module_id, &i.into()) {
            for ty in types {
                debug_assert!(ty.is_clone_cheap());
                ty.assert_valid();

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
            if let Some(scope) = self.scope.first_kind(|kind| match kind {
                ScopeKind::Class | ScopeKind::ObjectLit => true,
                _ => false,
            }) {
                if let ScopeKind::ObjectLit = scope.kind() {
                    if let Some(declaring_prop) = self.scope.declaring_prop() {
                        if *declaring_prop.sym() == i.sym {
                            return Err(Error::NoSuchVar {
                                span,
                                name: i.clone().into(),
                            });
                        }
                    }
                }
            }

            if let Some(kinds) = self.data.bindings.all.get(&i.into()) {
                if kinds
                    .iter()
                    .any(|kind| matches!(kind, BindingKind::Namespace | BindingKind::TsModule))
                {
                    return Ok(Type::Query(QueryType {
                        span,
                        expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(i.clone())),
                        metadata: QueryTypeMetadata {
                            common: CommonTypeMetadata {
                                resolved_from_var: true,
                                ..Default::default()
                            },
                        },
                    }));
                }
            }

            if !self.ctx.disallow_suggesting_property_on_no_var && self.this_has_property_named(&i.clone().into()) {
                Err(Error::NoSuchVarButThisHasSuchProperty {
                    span,
                    name: i.clone().into(),
                })
            } else {
                if self.ctx.in_shorthand {
                    Err(Error::NoSuchVarForShorthand {
                        span,
                        name: i.clone().into(),
                    })
                } else {
                    Err(Error::NoSuchVar {
                        span,
                        name: i.clone().into(),
                    })
                }
            }
        }
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn type_of_ts_entity_name(
        &mut self,
        span: Span,
        ctxt: ModuleId,
        n: &RExpr,
        type_args: Option<&TypeParamInstantiation>,
    ) -> VResult<Type> {
        self.type_of_ts_entity_name_inner(span, ctxt, n, type_args)
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    fn type_of_ts_entity_name_inner(
        &mut self,
        span: Span,
        ctxt: ModuleId,
        n: &RExpr,
        type_args: Option<&TypeParamInstantiation>,
    ) -> VResult<Type> {
        let span = span.with_ctxt(SyntaxContext::empty());
        {
            let res = self.report_error_for_unresolve_type(span, &n, type_args);
            match res {
                Ok(()) => {}
                Err(err) => {
                    self.storage.report(err);
                    return Ok(Type::any(span, Default::default()));
                }
            }
        }

        match &*n {
            RExpr::Ident(ref i) => {
                if i.sym == js_word!("Array") {
                    if let Some(type_args) = type_args {
                        // TODO(kdy1): Validate number of args.
                        return Ok(Type::Array(Array {
                            span,
                            // TODO(kdy1): Check length (After implementing error recovery for the
                            // parser)
                            elem_type: box type_args.clone().params.into_iter().next().unwrap(),
                            metadata: Default::default(),
                        }));
                    }
                }

                if let Some(types) = self.find_type(&i.into())? {
                    for ty in types {
                        match ty.normalize() {
                            Type::Namespace(_)
                            | Type::Module(_)
                            | Type::Instance(..)
                            | Type::Enum(_)
                            | Type::EnumVariant(_)
                            | Type::This(_)
                            | Type::StaticThis(_)
                            | Type::Param(_)
                            | Type::Constructor(_)
                            | Type::Rest(_)
                            | Type::Lit(_)
                            | Type::Optional(_)
                            | Type::Keyword(_)
                            | Type::Function(_)
                            | Type::TypeLit(_)
                            | Type::Tpl(_) => {
                                let mut ty = ty.into_owned().clone();
                                ty.respan(span);
                                return Ok(ty);
                            }

                            Type::Interface(_) | Type::Class(_) | Type::ClassDef(_) | Type::Alias(_) => {
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
                                            params = self.instantiate_type_params_using_args(span, type_params, type_args).map(Some)?;
                                        }
                                        _ => self.storage.report(Error::TypeParamsProvidedButCalleeIsNotGeneric { span }),
                                    }
                                }
                                if let Some(params) = params {
                                    ty = self.expand_type_params(&params, ty, Default::default())?;
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
                            Type::Intrinsic(_) => {}

                            Type::Conditional(_) => {}
                            Type::Tuple(_) => {}
                            Type::Array(_) => {}
                            Type::Union(ty) => {
                                // TODO(kdy1): Expand types
                                if !self.is_builtin {
                                    if cfg!(debug_assertions) {
                                        dbg!(&ty);
                                    }
                                }
                            }
                            Type::Intersection(ty) => {
                                // TODO(kdy1): Expand types
                                if !self.is_builtin {
                                    if cfg!(debug_assertions) {
                                        dbg!(&ty);
                                    }
                                }
                            }
                            Type::Operator(_) => {}
                            Type::Mapped(_) => {}
                            Type::Arc(_) => {}
                        }
                    }
                }

                if cfg!(debug_assertions) {
                    warn!("Creating Type::Ref: {:?}", i);
                }

                Ok(Type::Ref(Ref {
                    span,
                    ctxt: self.ctx.module_id,
                    type_name: RTsEntityName::Ident(i.clone()),
                    type_args: type_args.cloned().map(Box::new),
                    metadata: Default::default(),
                }))
            }
            RExpr::Member(RMemberExpr {
                obj,
                prop: RMemberProp::Ident(right),
                ..
            }) => {
                let obj_ty = self.type_of_ts_entity_name(span, ctxt, &obj, None)?;
                obj_ty.assert_valid();

                self.access_property(
                    span,
                    &obj_ty,
                    &Key::Normal {
                        span: right.span,
                        sym: right.sym.clone(),
                    },
                    TypeOfMode::RValue,
                    IdCtx::Type,
                    Default::default(),
                )
                .context("tried to resolve type from a ts entity name")
            }
            RExpr::OptChain(ROptChainExpr {
                base:
                    ROptChainBase::Member(RMemberExpr {
                        obj,
                        prop: RMemberProp::Ident(right),
                        ..
                    }),
                ..
            }) => {
                let obj_ty = self.type_of_ts_entity_name(span, ctxt, &obj, None)?;
                obj_ty.assert_valid();

                let ty = self
                    .access_property(
                        span,
                        &obj_ty,
                        &Key::Normal {
                            span: right.span,
                            sym: right.sym.clone(),
                        },
                        TypeOfMode::RValue,
                        IdCtx::Type,
                        Default::default(),
                    )
                    .context("tried to resolve type from an optional ts entity name")?;

                Ok(Type::union(vec![
                    ty,
                    Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        metadata: Default::default(),
                    }),
                ])
                .cheap())
            }

            _ => {
                todo!("type_of_ts_entity_name: {:?}", n)
            }
        }
    }

    /// TODO(kdy1): Expand type arguments if provided.
    fn type_of_member_expr(&mut self, expr: &RMemberExpr, type_mode: TypeOfMode) -> VResult<Type> {
        let RMemberExpr {
            ref obj, ref prop, span, ..
        } = *expr;
        let computed = matches!(prop, RMemberProp::Computed(_));

        let name: Option<Name> = expr.try_into().ok();

        if let TypeOfMode::RValue = type_mode {
            if let Some(name) = &name {
                if let Some(ty) = self.scope.get_type_from_name(name) {
                    return Ok(ty);
                }
            }
        }

        let mut errors = Errors::default();

        let mut is_obj_opt_chain = false;
        let mut should_be_optional = false;
        let mut obj_ty = match *obj {
            ref obj => {
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
                            Type::any(span, Default::default())
                        } else {
                            return Err(err);
                        }
                    }
                };

                obj_ty.assert_valid();

                if is_obj_opt_chain {
                    should_be_optional = self.is_obj_optional(&obj_ty)?;
                }

                obj_ty
            }
        };
        obj_ty.make_clone_cheap();

        self.storage.report_all(errors);

        let mut prop = self
            .validate_key(
                &match prop {
                    RMemberProp::Ident(i) => RExpr::Ident(i.clone()),
                    RMemberProp::Computed(c) => *c.expr.clone(),
                    RMemberProp::PrivateName(p) => RExpr::PrivateName(p.clone()),
                },
                computed,
            )
            .report(&mut self.storage)
            .unwrap_or_else(|| {
                let span = prop.span().with_ctxt(SyntaxContext::empty());
                Key::Computed(ComputedKey {
                    span,
                    expr: box RExpr::Invalid(RInvalid { span }),
                    ty: box Type::any(span, Default::default()),
                })
            });
        prop.make_clone_cheap();

        let prop_access_ctx = Ctx {
            in_opt_chain: self.ctx.in_opt_chain || is_obj_opt_chain,
            ..self.ctx
        };

        let mut ty = self
            .with_ctx(prop_access_ctx)
            .access_property(span, &obj_ty, &prop, type_mode, IdCtx::Var, Default::default())
            .context("tried to access property of an object to calculate type of a member expression")?;

        if !self.is_builtin {
            if let Some(name) = name {
                ty = self.apply_type_facts(&name, ty);

                self.exclude_types_using_fact(span, &name, &mut ty);
            }
        }

        let ty = if computed {
            ty
        } else {
            if self.ctx.in_cond && self.ctx.should_store_truthy_for_access {
                // Add type facts.
                if let Some(name) = extract_name_for_assignment(obj, false) {
                    let next_ty = self
                        .filter_types_with_property(
                            span,
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

                        self.add_deep_type_fact(span, name, next_ty, true);
                    }
                }
            }

            ty
        };

        if should_be_optional {
            Ok(Type::union(vec![Type::undefined(span, Default::default()), ty]))
        } else {
            Ok(ty)
        }
    }

    /// TODO(kdy1): Expand type arguments if provided.
    fn type_of_super_prop_expr(&mut self, expr: &RSuperPropExpr, type_mode: TypeOfMode) -> VResult<Type> {
        let RSuperPropExpr {
            ref obj, ref prop, span, ..
        } = *expr;
        let computed = matches!(prop, RSuperProp::Computed(_));

        let mut errors = Errors::default();

        let mut is_obj_opt_chain = false;
        let mut should_be_optional = false;
        let mut obj_ty = match *obj {
            RSuper { span, .. } => {
                if self.scope.cannot_use_this_because_super_not_called() {
                    self.storage.report(Error::SuperUsedBeforeCallingSuper { span })
                }

                self.report_error_for_super_reference_in_compute_keys(span, false);

                if let Some(v) = self.scope.get_super_class() {
                    v.clone()
                } else {
                    self.storage.report(Error::SuperInClassWithoutSuper { span });
                    Type::any(span, Default::default())
                }
            }
        };
        obj_ty.make_clone_cheap();

        self.storage.report_all(errors);

        let mut prop = self
            .validate_key(
                &match prop {
                    RSuperProp::Ident(i) => RExpr::Ident(i.clone()),
                    RSuperProp::Computed(c) => *c.expr.clone(),
                },
                computed,
            )
            .report(&mut self.storage)
            .unwrap_or_else(|| {
                let span = prop.span().with_ctxt(SyntaxContext::empty());
                Key::Computed(ComputedKey {
                    span,
                    expr: box RExpr::Invalid(RInvalid { span }),
                    ty: box Type::any(span, Default::default()),
                })
            });
        prop.make_clone_cheap();

        let prop_access_ctx = Ctx {
            in_opt_chain: self.ctx.in_opt_chain || is_obj_opt_chain,
            ..self.ctx
        };

        let ty = self
            .with_ctx(prop_access_ctx)
            .access_property(span, &obj_ty, &prop, type_mode, IdCtx::Var, Default::default())
            .context("tried to access property of an object to calculate type of a member expression")?;

        if should_be_optional {
            Ok(Type::union(vec![Type::undefined(span, Default::default()), ty]))
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
                let ty = self.with_ctx(ctx).expand(
                    *span,
                    ty.clone(),
                    ExpandOpts {
                        full: true,
                        expand_union: true,
                        ..Default::default()
                    },
                );
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
                    let parent_ty = self.type_of_ts_entity_name(parent.span, self.ctx.module_id, &parent.expr, parent.type_args.as_deref());

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

    pub(crate) fn report_error_for_super_refs_without_supers(&mut self, span: Span, is_super_call: bool) {
        let res: VResult<_> = try {
            if !self.ctx.in_class_with_super && self.ctx.super_references_super_class {
                Err(Error::SuperInClassWithoutSuper { span })?
            }
        };

        res.report(&mut self.storage);
    }

    pub(crate) fn report_error_for_super_reference_in_compute_keys(&mut self, span: Span, is_super_call: bool) {
        if !self.ctx.in_computed_prop_name {
            return;
        }

        match self
            .scope
            .first_kind(|kind| match kind {
                ScopeKind::TypeParams
                | ScopeKind::Flow
                | ScopeKind::Call
                | ScopeKind::Block
                | ScopeKind::LoopBody { .. }
                | ScopeKind::ObjectLit => false,
                ScopeKind::Fn
                | ScopeKind::Method { .. }
                | ScopeKind::Class
                | ScopeKind::Module
                | ScopeKind::Constructor
                | ScopeKind::ArrowFn => true,
            })
            .map(|scope| scope.kind())
        {
            Some(ScopeKind::Class) => {
                // Using proerties of super class in class property names are not allowed.
                self.storage.report(Error::CannotReferenceSuperInComputedPropName { span })
            }

            Some(ScopeKind::ArrowFn) => {
                if !is_super_call {
                    return;
                }

                self.storage.report(Error::CannotReferenceSuperInComputedPropName { span })
            }

            kind => {
                dbg!(kind);
            }
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
                    Type::Keyword(KeywordType {
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
        elems.is_empty()
            || elems.iter().any(|el| match el {
                TypeElement::Property(PropertySignature { key: Key::Num(..), .. }) => true,
                _ => false,
            })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTpl, type_ann: Option<&Type>) -> VResult<Type> {
        e.exprs.visit_with(self);

        if e.exprs.is_empty() {
            return Ok(Type::Lit(LitType {
                span: e.span,
                lit: RTsLit::Str(RStr {
                    span: e.span,
                    value: (&*e.quasis[0].cooked.clone().unwrap_or_else(|| e.quasis[0].raw.clone())).into(),
                    raw: None,
                }),
                metadata: Default::default(),
            }));
        }

        if let Some(type_ann) = type_ann {
            match self
                .normalize(None, Cow::Borrowed(type_ann.normalize_instance()), Default::default())?
                .as_ref()
                .normalize()
            {
                Type::Tpl(TplType { span, quasis, types, .. }) => {
                    return Ok(Type::Tpl(TplType {
                        span: *span,
                        quasis: quasis.clone(),
                        types: types.clone(),
                        metadata: TplTypeMetadata {
                            common: CommonTypeMetadata {
                                prevent_generalization: true,
                                ..Default::default()
                            },
                        },
                    }))
                }
                _ => {}
            }
        }

        Ok(Type::Keyword(KeywordType {
            span: e.span,
            kind: TsKeywordTypeKind::TsStringKeyword,
            metadata: Default::default(),
        }))
    }
}

fn is_valid_lhs(l: &RPatOrExpr) -> Result<(), Error> {
    fn is_valid_lhs_expr(e: &RExpr) -> Result<(), Error> {
        // obj?.a["b"] += 1;
        if is_obj_opt_chaining(&e) {
            return Err(Error::InvalidLhsOfAssignOptionalProp { span: e.span() });
        }
        match e {
            RExpr::Ident(..) | RExpr::Member(..) | RExpr::SuperProp(..) => Ok(()),
            RExpr::Paren(e) => is_valid_lhs_expr(&e.expr),
            _ => Err(Error::InvalidLhsOfAssign { span: e.span() }),
        }
    }

    match l {
        RPatOrExpr::Pat(pat) => match &**pat {
            RPat::Expr(e) => is_valid_lhs_expr(&e),
            _ => Ok(()),
        },
        RPatOrExpr::Expr(e) => is_valid_lhs_expr(&e),
    }
}
