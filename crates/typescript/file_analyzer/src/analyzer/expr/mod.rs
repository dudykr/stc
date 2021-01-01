use super::{marks::MarkExt, Analyzer};
use crate::analyzer::util::ResultExt;
use crate::errors::Errors;
use crate::util::type_ext::TypeVecExt;
use crate::util::RemoveTypes;
use crate::{
    analyzer::{pat::PatMode, Ctx, ScopeKind},
    debug::print_backtrace,
    errors::Error,
    name::Name,
    ty,
    ty::{
        Array, ClassInstance, EnumVariant, IndexSignature, IndexedAccessType, Interface,
        Intersection, Ref, Tuple, Type, TypeElement, TypeLit, TypeParam, TypeParamInstantiation,
        Union,
    },
    type_facts::TypeFacts,
    util::is_str_lit_or_union,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::NodeId;
use rnode::VisitWith;
use stc_ts_ast_rnode::RArrayLit;
use stc_ts_ast_rnode::RArrowExpr;
use stc_ts_ast_rnode::RAssignExpr;
use stc_ts_ast_rnode::RAwaitExpr;
use stc_ts_ast_rnode::RBlockStmtOrExpr;
use stc_ts_ast_rnode::RCallExpr;
use stc_ts_ast_rnode::RClassExpr;
use stc_ts_ast_rnode::RComputedPropName;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSpread;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RFnExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RNull;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RObjectLit;
use stc_ts_ast_rnode::RParenExpr;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPatOrExpr;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RPropOrSpread;
use stc_ts_ast_rnode::RSeqExpr;
use stc_ts_ast_rnode::RSpreadElement;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RThisExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsNonNullExpr;
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_ast_rnode::RUnaryExpr;
use stc_ts_ast_rnode::RUpdateExpr;
use stc_ts_types::rprop_name_to_expr;
use stc_ts_types::{
    ClassProperty, Id, Method, ModuleId, Operator, QueryExpr, QueryType, StaticThis, TupleElement,
};
use std::{convert::TryFrom, mem::take};
use swc_atoms::js_word;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use ty::TypeExt;

mod bin;
mod call_new;
mod constraint_reducer;
mod optional_chaining;
mod type_cast;
mod unary;

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

        match e {
            // super() returns any
            RExpr::Call(RCallExpr {
                callee: RExprOrSuper::Super(..),
                ..
            }) => Ok(Type::any(span)),

            RExpr::Bin(e) => e.validate_with(self),
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
                    return Ok(box Type::Keyword(RTsKeywordType {
                        span: *span,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    }));
                }
                let span = *span;
                if let Some(ty) = self.scope.this() {
                    return Ok(ty.into_owned());
                }
                return Ok(box Type::from(RTsThisType { span }));
            }

            RExpr::Ident(ref i) => {
                let ty = self.type_of_var(i, mode, type_args)?;
                if mode == TypeOfMode::RValue {
                    // `i` is truthy
                    self.cur_facts
                        .true_facts
                        .facts
                        .insert(i.into(), TypeFacts::Truthy);
                    self.cur_facts
                        .false_facts
                        .facts
                        .insert(i.into(), TypeFacts::Falsy);
                }

                Ok(ty)
            }

            RExpr::Array(RArrayLit { ref elems, .. }) => {
                let mut can_be_tuple = true;
                let mut elements = Vec::with_capacity(elems.len());

                for elem in elems.iter() {
                    let span = elem.span();
                    let ty = match elem {
                        Some(RExprOrSpread {
                            spread: None,
                            ref expr,
                        }) => {
                            let ty = expr.validate_with_default(self)?;
                            ty
                        }
                        Some(RExprOrSpread {
                            spread: Some(..),
                            expr,
                        }) => {
                            let element_type = expr.validate_with_default(self)?;
                            let element_type = box element_type.foldable();

                            match *element_type {
                                Type::Array(array) => {
                                    can_be_tuple = false;
                                    elements.push(TupleElement {
                                        span,
                                        label: None,
                                        ty: array.elem_type,
                                    });
                                }
                                Type::Tuple(tuple) => {
                                    can_be_tuple = false;
                                    elements.extend(tuple.elems);
                                }
                                Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) => {
                                    can_be_tuple = false;
                                    elements.push(TupleElement {
                                        span,
                                        label: None,
                                        ty: element_type.clone(),
                                    });
                                }
                                _ => unimplemented!("type of array spread: {:?}", element_type),
                            }
                            continue;
                        }
                        None => {
                            let ty = Type::undefined(span);
                            ty
                        }
                    };
                    elements.push(TupleElement {
                        span,
                        label: None,
                        ty,
                    });
                }

                if self.ctx.in_export_default_expr && elements.is_empty() {
                    return Ok(box Type::Array(Array {
                        span,
                        elem_type: Type::any(span),
                    }));
                }

                if !can_be_tuple {
                    let mut types: Vec<_> =
                        elements.into_iter().map(|element| element.ty).collect();
                    types.dedup_type();

                    return Ok(box Type::Array(Array {
                        span,
                        elem_type: Type::union(types),
                    }));
                }

                return Ok(box Type::Tuple(Tuple {
                    span,
                    elems: elements,
                }));
            }

            RExpr::Lit(RLit::Bool(v)) => {
                return Ok(box Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    span: v.span,
                    lit: RTsLit::Bool(v.clone()),
                }));
            }
            RExpr::Lit(RLit::Str(ref v)) => {
                return Ok(box Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    span: v.span,
                    lit: RTsLit::Str(v.clone()),
                }));
            }
            RExpr::Lit(RLit::Num(v)) => {
                return Ok(box Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    span: v.span,
                    lit: RTsLit::Number(v.clone()),
                }));
            }
            RExpr::Lit(RLit::Null(RNull { span })) => {
                if self.ctx.in_export_default_expr {
                    // TODO: strict mode
                    return Ok(box Type::Keyword(RTsKeywordType {
                        span: *span,
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                    }));
                }

                return Ok(box Type::Keyword(RTsKeywordType {
                    span: *span,
                    kind: TsKeywordTypeKind::TsNullKeyword,
                }));
            }
            RExpr::Lit(RLit::Regex(..)) => {
                return Ok(box Type::Ref(Ref {
                    span,
                    ctxt: ModuleId::builtin(),
                    type_name: RTsEntityName::Ident(RIdent {
                        node_id: NodeId::invalid(),
                        span,
                        sym: js_word!("RegExp"),
                        optional: false,
                        type_ann: None,
                    }),
                    type_args: None,
                }));
            }

            RExpr::Paren(RParenExpr { ref expr, .. }) => {
                expr.validate_with_args(self, (mode, type_args, type_ann))
            }

            RExpr::Tpl(ref t) => {
                // Check if tpl is constant. If it is, it's type is string literal.
                if t.exprs.is_empty() {
                    return Ok(box Type::Lit(RTsLitType {
                        node_id: NodeId::invalid(),
                        span: t.span(),
                        lit: RTsLit::Str(
                            t.quasis[0]
                                .cooked
                                .clone()
                                .unwrap_or_else(|| t.quasis[0].raw.clone()),
                        ),
                    }));
                }

                return Ok(box Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                }));
            }

            RExpr::TsNonNull(RTsNonNullExpr { ref expr, .. }) => Ok(box expr
                .validate_with_args(self, (mode, type_args, type_ann))?
                .remove_falsy()),

            RExpr::Object(e) => {
                return e.validate_with(self);
            }

            // https://github.com/Microsoft/TypeScript/issues/26959
            RExpr::Yield(..) => {
                e.visit_children_with(self);
                return Ok(Type::any(span));
            }

            RExpr::Await(RAwaitExpr { .. }) => unimplemented!("typeof(AwaitExpr)"),

            RExpr::Class(RClassExpr {
                ref ident,
                ref class,
                ..
            }) => {
                self.scope.this_class_name = ident.as_ref().map(|i| i.into());
                return Ok(box class.validate_with(self)?.into());
            }

            RExpr::Arrow(ref e) => return Ok(box e.validate_with(self)?.into()),

            RExpr::Fn(RFnExpr { ref function, .. }) => {
                return Ok(box function.validate_with(self)?.into());
            }

            RExpr::Member(ref expr) => {
                // Foo.a
                if let Ok(name) = Name::try_from(&*expr) {
                    self.cur_facts
                        .true_facts
                        .facts
                        .insert(name, TypeFacts::Truthy);
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
                            "Proper error reporting for using const assertion expression in left \
                             hand side of an assignment expression"
                        ),
                    });
                }
            }

            _ => unimplemented!("typeof ({:?})", e),
        }
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
    fn validate(
        &mut self,
        e: &RParenExpr,
        mode: TypeOfMode,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        e.expr.validate_with_args(self, (mode, None, type_ann))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RAssignExpr,
        mode: TypeOfMode,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let ctx = Ctx {
            pat_mode: PatMode::Assign,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
            let span = e.span();

            let any_span = match e.left {
                RPatOrExpr::Pat(box RPat::Ident(ref i))
                | RPatOrExpr::Expr(box RExpr::Ident(ref i)) => {
                    // Type is any if self.declaring contains ident
                    if analyzer.scope.declaring.contains(&i.into()) {
                        Some(span)
                    } else {
                        None
                    }
                }

                _ => None,
            };

            let mut errors = Errors::default();

            let rhs_ty = match e.right.validate_with_args(analyzer, (mode, None, type_ann)) {
                Ok(rhs_ty) => {
                    analyzer.check_rvalue(&rhs_ty);

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

            match &e.left {
                RPatOrExpr::Pat(box RPat::Ident(i)) => {
                    // TODO: Implemennt this
                    let rhs_is_always_true = true;

                    // TODO: Deny changing type of const
                    if rhs_is_always_true {
                        analyzer.mark_var_as_truthy(Id::from(&*i))?;
                    }
                }
                _ => e.left.visit_with(analyzer),
            }

            if e.op == op!("=") {
                let rhs_ty = analyzer.expand_fully(span, rhs_ty.clone(), true)?;
                analyzer.try_assign(span, &e.left, &rhs_ty);
            }

            if let Some(span) = any_span {
                return Ok(Type::any(span));
            }

            Ok(rhs_ty)
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RUpdateExpr) -> ValidationResult {
        let span = e.span;

        let ty = e
            .arg
            .validate_with_args(self, (TypeOfMode::LValue, None, None))
            .and_then(|ty| match *ty.normalize() {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Str(..),
                    ..
                })
                | Type::Array(..) => Err(Error::TS2356 { span: e.arg.span() }),

                _ => Ok(ty),
            })
            .report(&mut self.storage);

        if let Some(ty) = ty {
            if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                self.storage.report(Error::UpdateOpToSymbol {
                    span: e.arg.span(),
                    op: e.op,
                })
            }
        }

        Ok(box Type::Keyword(RTsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RSeqExpr,
        mode: TypeOfMode,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let RSeqExpr {
            span, ref exprs, ..
        } = *e;

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
                        op: op!(unary, "-"),
                        ..
                    })
                    | RExpr::Unary(RUnaryExpr {
                        op: op!(unary, "+"),
                        ..
                    })
                    | RExpr::Unary(RUnaryExpr { op: op!("!"), .. })
                    | RExpr::Unary(RUnaryExpr {
                        op: op!("typeof"), ..
                    }) if !self.rule().allow_unreachable_code => {
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

        return exprs
            .last()
            .unwrap()
            .validate_with_args(self, (mode, None, type_ann));
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn access_property(
        &mut self,
        span: Span,
        obj: Box<Type>,
        prop: &RExpr,
        computed: bool,
        type_mode: TypeOfMode,
    ) -> ValidationResult {
        #[inline(never)]
        fn handle_type_elements(
            a: &mut Analyzer,
            span: Span,
            obj: &Type,
            prop: &RExpr,
            computed: bool,
            type_mode: TypeOfMode,
            members: &[TypeElement],
        ) -> ValidationResult<Option<Box<Type>>> {
            let prop_ty = if computed {
                prop.validate_with_default(a)?
            } else {
                match prop {
                    RExpr::Ident(..) => box Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        span,
                    }),
                    _ => unreachable!(),
                }
            };

            let mut candidates = vec![];
            for el in members.iter() {
                if let Some(key) = el.key() {
                    let is_el_computed = match *el {
                        TypeElement::Property(ref p) => p.computed,
                        _ => false,
                    };
                    let is_eq = match prop {
                        RExpr::Ident(RIdent { sym: ref value, .. }) if !computed => match &*key {
                            RExpr::Ident(RIdent {
                                sym: ref r_value, ..
                            }) => value == r_value,

                            RExpr::Lit(RLit::Str(RStr {
                                value: ref r_value, ..
                            })) => value == r_value,
                            _ => false,
                        },

                        RExpr::Lit(RLit::Str(RStr { ref value, .. })) if computed => match &*key {
                            RExpr::Ident(RIdent {
                                sym: ref r_value, ..
                            }) => value == r_value,

                            RExpr::Lit(RLit::Str(RStr {
                                value: ref r_value, ..
                            })) => value == r_value,
                            _ => false,
                        },
                        _ => false,
                    };

                    if is_eq || key.eq_ignore_span(prop) {
                        match el {
                            TypeElement::Property(ref p) => {
                                if type_mode == TypeOfMode::LValue && p.readonly {
                                    return Err(Error::ReadOnly { span });
                                }

                                if let Some(ref type_ann) = p.type_ann {
                                    candidates.push(type_ann.clone());
                                    continue;
                                }

                                // TODO: no implicit any?
                                candidates.push(Type::any(span));
                                continue;
                            }

                            TypeElement::Method(ref m) => {
                                //
                                candidates.push(box Type::Function(ty::Function {
                                    span,
                                    type_params: m.type_params.clone(),
                                    params: m.params.clone(),
                                    ret_ty: m.ret_ty.clone().unwrap_or_else(|| Type::any(span)),
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

                let obj = a.env.get_global_type(span, &js_word!("Function"))?;

                if let Ok(v) = a.access_property(span, obj, prop, computed, type_mode) {
                    return Ok(Some(v));
                }
            }

            for el in members.iter() {
                if computed {
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

                            // Don't know exact reason, but you can index `{ [x: string]: boolean }`
                            // with number type.
                            //
                            // I guess it's because javascript work in that way.
                            let indexed = (index_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                                && prop_ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword))
                                || index_ty.type_eq(&prop_ty);

                            if indexed {
                                if let Some(ref type_ann) = type_ann {
                                    return Ok(Some(type_ann.clone()));
                                }

                                return Ok(Some(Type::any(span)));
                            }

                            match prop_ty.normalize() {
                                // TODO: Only string or number
                                Type::EnumVariant(..) => {
                                    candidates.extend(type_ann.clone());
                                    continue;
                                }

                                _ => {}
                            }

                            let ty = box Type::IndexedAccessType(IndexedAccessType {
                                span,
                                obj_type: box obj.clone(),
                                index_type: prop_ty,
                                readonly: *readonly,
                            });

                            return Ok(Some(ty));
                        }
                        _ => {}
                    }
                }
            }

            if candidates.len() == 0 {
                return Ok(None);
            }

            if candidates.len() == 1 {
                return Ok(candidates.pop());
            }

            Ok(Some(Type::union(candidates)))
        }

        if !self.is_builtin {
            debug_assert!(!span.is_dummy());

            slog::debug!(&self.logger, "access_property");
        }

        // Recursive method call
        if !computed
            && obj.is_this()
            && (self.scope.is_this_ref_to_object_lit() || self.scope.is_this_ref_to_class())
        {
            if let Some(declaring) = &self.scope.declaring_prop() {
                match prop {
                    RExpr::Ident(key) => {
                        if key.sym == *declaring.sym() {
                            return Ok(Type::any(span));
                        }
                    }
                    _ => {}
                }
            }
        }

        match &*obj {
            Type::This(this) if self.scope.is_this_ref_to_object_lit() => {
                if computed
                    && match prop {
                        RExpr::Cond(..) => true,
                        _ => false,
                    }
                {
                    return Ok(Type::any(span));
                }

                // TODO: Remove clone
                let members = self.scope.object_lit_members().to_vec();
                if let Some(mut v) =
                    handle_type_elements(self, span, &obj, prop, computed, type_mode, &members)?
                {
                    self.marks()
                        .infected_by_this_in_object_literal
                        .apply_to_type(&mut v);
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
                            ty::ClassMember::Constructor(_) => {}

                            ty::ClassMember::Method(
                                member
                                @
                                Method {
                                    is_static: false, ..
                                },
                            ) => match &member.key {
                                RPropName::Ident(key) => {
                                    //
                                    match prop {
                                        RExpr::Ident(i) if i.sym == key.sym => {
                                            return Ok(box Type::Function(ty::Function {
                                                span: member.span,
                                                type_params: member.type_params.clone(),
                                                params: member.params.clone(),
                                                ret_ty: member.ret_ty.clone(),
                                            }));
                                        }

                                        _ => {}
                                    }
                                }
                                RPropName::Str(_) => {}
                                RPropName::Num(_) => {}
                                RPropName::BigInt(_) => {}

                                RPropName::Computed(key) => {
                                    if (*key.expr).type_eq(&*prop) {
                                        return Ok(box Type::Function(ty::Function {
                                            span: member.span,
                                            type_params: member.type_params.clone(),
                                            params: member.params.clone(),
                                            ret_ty: member.ret_ty.clone(),
                                        }));
                                    }
                                }
                            },

                            ty::ClassMember::Property(
                                member
                                @
                                ClassProperty {
                                    is_static: false, ..
                                },
                            ) => {
                                match &*member.key {
                                    RExpr::Ident(member_key) => match &*prop {
                                        RExpr::Ident(prop) => {
                                            if prop.sym == member_key.sym {
                                                return Ok(member
                                                    .value
                                                    .clone()
                                                    .unwrap_or_else(|| Type::any(span)));
                                            }
                                        }
                                        _ => {}
                                    },
                                    _ => {}
                                }
                                if (&*member.key).type_eq(&*prop) {
                                    return Ok(member
                                        .value
                                        .clone()
                                        .unwrap_or_else(|| Type::any(span)));
                                }
                            }

                            ty::ClassMember::Property(..) | ty::ClassMember::Method(..) => {}

                            ty::ClassMember::IndexSignature(_) => {
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
                        let super_class =
                            self.with_ctx(ctx).expand_fully(span, super_class, true)?;

                        if let Ok(v) =
                            self.access_property(span, super_class, prop, computed, type_mode)
                        {
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

                    if let Ok(v) =
                        self.access_property(span, super_class, prop, computed, type_mode)
                    {
                        return Ok(v);
                    }
                }

                let prop_ty = prop.validate_with_default(self)?;
                // TODO: Handle string literals like
                //
                // `this['props']`
                return Ok(box Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly: false,
                    obj_type: box Type::This(this.clone()),
                    index_type: prop_ty,
                }));
            }

            Type::StaticThis(StaticThis { span }) => {
                // Handle static access to class itself while *decalring* the class.
                for (_, member) in self.scope.class_members() {
                    match member {
                        stc_ts_types::ClassMember::Method(
                            member
                            @
                            Method {
                                is_static: true, ..
                            },
                        ) => {
                            match &member.key {
                                RPropName::Ident(key) => {
                                    //
                                    match prop {
                                        RExpr::Ident(i) if i.sym == key.sym => {
                                            return Ok(box Type::Function(ty::Function {
                                                span: member.span,
                                                type_params: member.type_params.clone(),
                                                params: member.params.clone(),
                                                ret_ty: member.ret_ty.clone(),
                                            }));
                                        }

                                        _ => {}
                                    }
                                }
                                RPropName::Str(_) => {}
                                RPropName::Num(_) => {}
                                RPropName::BigInt(_) => {}

                                RPropName::Computed(key) => {
                                    if (*key.expr).type_eq(&*prop) {
                                        return Ok(box Type::Function(ty::Function {
                                            span: member.span,
                                            type_params: member.type_params.clone(),
                                            params: member.params.clone(),
                                            ret_ty: member.ret_ty.clone(),
                                        }));
                                    }
                                }
                            }
                        }

                        stc_ts_types::ClassMember::Property(
                            property
                            @
                            ClassProperty {
                                is_static: true, ..
                            },
                        ) => {
                            if (*property.key).type_eq(&*prop) {
                                return Ok(property
                                    .value
                                    .clone()
                                    .unwrap_or_else(|| Type::any(span.clone())));
                            }
                        }

                        _ => {}
                    }
                }

                dbg!();

                return Err(Error::NoSuchProperty {
                    span: *span,
                    obj: Some(obj.clone()),
                    prop: Some(prop.clone()),
                    prop_ty: None,
                });
            }

            _ => {}
        }

        let ctx = Ctx {
            preserve_ref: false,
            ignore_expand_prevention_for_top: true,
            ..self.ctx
        };
        let obj = self.with_ctx(ctx).expand(span, obj)?.generalize_lit();

        match obj.normalize() {
            Type::Lit(..) => unreachable!(),

            Type::Enum(ref e) => {
                // TODO: Check if variant exists.
                macro_rules! ret {
                    ($sym:expr) => {{
                        // Computed values are not permitted in an enum with string valued members.
                        if e.is_const && type_mode == TypeOfMode::RValue {
                            // for m in &e.members {
                            //     match m.id {
                            //         TsEnumMemberId::Ident(Ident { ref sym, .. })
                            //         | TsEnumMemberId::Str(Str { value: ref sym, .. }) => {
                            //             if sym == $sym {
                            //                 return Ok(Type::Lit(RTsLitType {
                            //                     span: m.span(),
                            //                     lit: match m.val.clone() {
                            //                         RExpr::Lit(RLit::Str(s)) =>
                            // RTsLit::Str(s),
                            // RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                            // _ => unreachable!(),                     },
                            //                 }));
                            //             }
                            //         }
                            //     }
                            // }
                        }

                        if e.is_const && computed {
                            // return Err(Error::ConstEnumNonIndexAccess { span: prop.span() });
                        }

                        if e.is_const && type_mode == TypeOfMode::LValue {
                            return Err(Error::InvalidLValue { span: prop.span() });
                        }

                        debug_assert_ne!(span, prop.span());
                        return Ok(box Type::EnumVariant(EnumVariant {
                            span: match type_mode {
                                TypeOfMode::LValue => prop.span(),
                                TypeOfMode::RValue => span,
                            },
                            ctxt: self.ctx.module_id,
                            enum_name: e.id.clone().into(),
                            name: $sym.clone(),
                        }));
                    }};
                }
                match *prop {
                    RExpr::Ident(RIdent { ref sym, .. }) if !computed => {
                        ret!(sym);
                    }
                    RExpr::Lit(RLit::Str(RStr { value: ref sym, .. })) => {
                        ret!(sym);
                    }
                    RExpr::Lit(RLit::Num(RNumber { value, .. })) => {
                        let idx = value.round() as usize;
                        if e.members.len() > idx {
                            let v = &e.members[idx];
                            if match *v.val {
                                RExpr::Lit(RLit::Str(..)) | RExpr::Lit(RLit::Num(..)) => true,
                                _ => false,
                            } {
                                let new_obj_ty = box Type::Lit(RTsLitType {
                                    node_id: NodeId::invalid(),
                                    span,
                                    lit: match *v.val.clone() {
                                        RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                        RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                                        _ => unreachable!(),
                                    },
                                });
                                return self
                                    .access_property(span, new_obj_ty, prop, computed, type_mode);
                            }
                        }
                        return Ok(box Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }

                    _ => {
                        if e.is_const {
                            return Err(Error::ConstEnumNonIndexAccess { span: prop.span() });
                        }
                        return Err(Error::Unimplemented {
                            span,
                            msg: format!("access_property\nProp: {:?}", prop),
                        });
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
                                        RExpr::Lit(RLit::Str(..)) | RExpr::Lit(RLit::Num(..)) => {
                                            true
                                        }
                                        _ => false,
                                    } {
                                        let new_obj_ty = box Type::Lit(RTsLitType {
                                            node_id: NodeId::invalid(),
                                            span: *span,
                                            lit: match *v.val.clone() {
                                                RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                                RExpr::Lit(RLit::Num(v)) => RTsLit::Number(v),
                                                _ => unreachable!(),
                                            },
                                        });
                                        return self.access_property(
                                            *span, new_obj_ty, prop, computed, type_mode,
                                        );
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
                for v in c.body.iter() {
                    match v {
                        ty::ClassMember::Property(ref class_prop) => {
                            match &*class_prop.key {
                                RExpr::Ident(i) if !computed => {
                                    if self.scope.declaring_prop.as_ref() == Some(&i.into()) {
                                        return Err(Error::ReferencedInInit { span });
                                    }
                                }

                                _ => {}
                            }

                            let has_same_key = match &*prop {
                                RExpr::Ident(prop) => match &*class_prop.key {
                                    RExpr::Ident(key) if !computed => {
                                        // ctxt is ignored because syntax context of property in a
                                        // member expression is always empty.
                                        prop.sym == key.sym
                                    }
                                    _ => false,
                                },
                                RExpr::Lit(RLit::Str(v)) if computed => match &*class_prop.key {
                                    RExpr::Ident(key) => v.value == key.sym,
                                    _ => false,
                                },
                                _ => false,
                            } || (*class_prop.key).eq_ignore_span(&*prop);
                            //
                            if has_same_key {
                                return Ok(match class_prop.value {
                                    Some(ref ty) => ty.clone(),
                                    None => Type::any(span),
                                });
                            }
                        }
                        ty::ClassMember::Method(ref mtd) => {
                            let mtd_key = rprop_name_to_expr(mtd.key.clone());
                            // TODO: Check if this is correct.

                            let has_same_key = match &*prop {
                                RExpr::Ident(prop) => match &mtd_key {
                                    RExpr::Ident(key) => {
                                        prop.span.ctxt == key.span.ctxt && prop.sym == key.sym
                                    }
                                    _ => false,
                                },
                                _ => false,
                            } || mtd_key.eq_ignore_span(prop);

                            if has_same_key {
                                return Ok(box Type::Function(stc_ts_types::Function {
                                    span: mtd.span,
                                    type_params: mtd.type_params.clone(),
                                    params: mtd.params.clone(),
                                    ret_ty: mtd.ret_ty.clone(),
                                }));
                            }
                        }

                        ty::ClassMember::Constructor(ref cons) => match prop {
                            RExpr::Ident(ref i) if i.sym == *"constructor" => {
                                return Ok(box Type::Constructor(ty::Constructor {
                                    span,
                                    type_params: cons.type_params.clone(),
                                    params: cons.params.clone(),
                                    type_ann: cons.ret_ty.clone().unwrap_or_else(|| obj.clone()),
                                }))
                            }
                            _ => {}
                        },

                        ref member => unimplemented!(
                            "propert access to class member: {:?}\nprop: {:?}",
                            member,
                            prop
                        ),
                    }
                }

                // check for super class
                if let Some(super_ty) = &c.super_class {
                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..self.ctx
                    };
                    let super_ty = self.with_ctx(ctx).expand_fully(
                        span,
                        box super_ty.normalize().clone(),
                        true,
                    )?;
                    if let Ok(v) = self.access_property(span, super_ty, prop, computed, type_mode) {
                        return Ok(v);
                    }
                }

                return Err(Error::NoSuchPropertyInClass {
                    span,
                    class_name: c.name.clone(),
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
                            if let Ok(ty) = self.with_ctx(ctx).access_property(
                                span,
                                constraint.clone(),
                                prop,
                                computed,
                                type_mode,
                            ) {
                                return Ok(ty);
                            }
                        }
                        _ => {}
                    }
                }

                let mut prop_ty = if computed {
                    prop.validate_with_default(self)?
                } else {
                    match prop {
                        RExpr::Ident(i) => box Type::Lit(RTsLitType {
                            node_id: NodeId::invalid(),
                            span: i.span,
                            lit: RTsLit::Str(RStr {
                                span: i.span,
                                value: i.sym.clone(),
                                has_escape: false,
                                kind: Default::default(),
                            }),
                        }),
                        _ => unreachable!(),
                    }
                };
                if is_str_lit_or_union(&prop_ty) {
                    self.prevent_generalize(&mut prop_ty);
                }

                return Ok(box Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly: false,
                    obj_type: obj,
                    index_type: prop_ty,
                }));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => {
                return Ok(box Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                }));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span: obj.span() }),

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
                            obj: Some(obj),
                            prop: Some(prop.clone()),
                            prop_ty: None,
                        });
                    }
                };
                let interface = self.env.get_global_type(span, &word)?;

                return self.access_property(span, interface, prop, computed, type_mode);
            }

            Type::Array(Array { elem_type, .. }) => {
                let array_ty = self.env.get_global_type(span, &js_word!("Array"))?;

                if self.scope.is_calling() {
                    self.scope
                        .types
                        .entry(Id::word("T".into()))
                        .or_default()
                        .push(elem_type.clone().cheap());
                }

                if computed {
                    match prop.validate_with_default(self) {
                        Ok(ty) => match ty.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                ..
                            })
                            | Type::Lit(RTsLitType {
                                lit: RTsLit::Number(..),
                                ..
                            }) => return Ok(elem_type.clone()),

                            _ => {}
                        },
                        _ => {}
                    }
                }

                return self.access_property(span, array_ty, prop, computed, type_mode);
            }

            Type::Interface(Interface {
                ref body, extends, ..
            }) => {
                if let Ok(Some(v)) =
                    handle_type_elements(self, span, &obj, prop, computed, type_mode, body)
                {
                    return Ok(v);
                }

                for super_ty in extends {
                    let obj = self.type_of_ts_entity_name(
                        span,
                        self.ctx.module_id,
                        &super_ty.expr,
                        super_ty.type_args.clone(),
                    )?;

                    // TODO: Check if multiple interface has same property.
                    if let Ok(ty) = self.access_property(span, obj, prop, computed, type_mode) {
                        return Ok(ty);
                    }
                }

                // TODO: Check parent interfaces

                if computed {
                    let prop_ty = Some(prop.validate_with_default(self)?);
                    dbg!();
                    return Err(Error::NoSuchProperty {
                        span,
                        obj: Some(obj),
                        prop: Some(prop.clone()),
                        prop_ty,
                    });
                } else {
                    dbg!();
                    return Err(Error::NoSuchProperty {
                        span,
                        obj: Some(obj),
                        prop: Some(prop.clone()),
                        prop_ty: None,
                    });
                };
            }

            Type::TypeLit(TypeLit { ref members, .. }) => {
                if let Some(v) =
                    handle_type_elements(self, span, &obj, prop, computed, type_mode, members)?
                {
                    return Ok(v);
                }

                if computed {
                    let prop_ty = Some(prop.validate_with_default(self)?);
                    dbg!();
                    return Err(Error::NoSuchProperty {
                        span,
                        obj: Some(obj),
                        prop: Some(prop.clone()),
                        prop_ty,
                    });
                } else {
                    dbg!();
                    return Err(Error::NoSuchProperty {
                        span,
                        obj: Some(obj),
                        prop: Some(prop.clone()),
                        prop_ty: None,
                    });
                };
            }

            Type::Union(ty::Union { types, .. }) => {
                debug_assert!(types.len() >= 1);

                let mut tys = vec![];
                let mut errors = Vec::with_capacity(types.len());

                for ty in types {
                    let ty = ty.clone();
                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..self.ctx
                    };
                    let ty = self.with_ctx(ctx).expand_fully(span, ty, true)?;

                    match self.access_property(span, ty.clone(), prop, computed, type_mode) {
                        Ok(ty) => tys.push(ty),
                        Err(err) => errors.push(err),
                    }
                }

                if type_mode != TypeOfMode::LValue {
                    if !errors.is_empty() {
                        return Err(Error::UnionError { span, errors });
                    }
                } else {
                    // In l-value context, it's success if one of types matches it.
                    let is_err = errors.iter().any(|err| match *err {
                        Error::ReadOnly { .. } => true,
                        _ => false,
                    });
                    if tys.is_empty() || is_err {
                        assert_ne!(errors.len(), 0);
                        return Err(Error::UnionError { span, errors });
                    }
                }

                // TODO: Validate that the ty has same type instead of returning union.
                return Ok(Type::union(tys));
            }

            Type::Tuple(Tuple { ref elems, .. }) => match *prop {
                RExpr::Lit(RLit::Num(ref n)) => {
                    let v = n.value.round() as i64;
                    if v < 0 || elems.len() <= v as usize {
                        return Err(Error::TupleIndexError {
                            span: n.span(),
                            index: v,
                            len: elems.len() as u64,
                        });
                    }

                    return Ok(elems[v as usize].ty.clone());
                }
                _ => {
                    let mut types = elems.iter().map(|e| e.ty.clone()).collect::<Vec<_>>();
                    types.dedup_type();
                    let obj = box Type::Array(Array {
                        span,
                        elem_type: Type::union(types),
                    });

                    return self.access_property(span, obj, prop, computed, type_mode);
                }
            },

            Type::ClassInstance(ClassInstance {
                ty: box Type::Class(ref cls),
                ..
            }) => {
                //
                for m in &cls.body {
                    //
                    match *m {
                        ty::ClassMember::Property(ref p) => {
                            // TODO: normalized string / ident
                            if (&*p.key).eq_ignore_span(&prop) {
                                if let Some(ref ty) = p.value {
                                    return Ok(ty.clone());
                                }

                                return Ok(Type::any(p.key.span()));
                            }
                        }

                        ty::ClassMember::Method(ref m) => {
                            // TODO: normalized string / ident
                            match &m.key {
                                RPropName::Computed(RComputedPropName { expr, .. }) => {
                                    if (&**expr).eq_ignore_span(&prop) {
                                        return Ok(box Type::Function(ty::Function {
                                            span,
                                            type_params: m.type_params.clone(),
                                            params: m.params.clone(),
                                            ret_ty: m.ret_ty.clone(),
                                        }));
                                    }
                                }
                                // TODO: Merge code
                                RPropName::Ident(method_key) => match &*prop {
                                    RExpr::Ident(prop) => {
                                        if prop.sym == method_key.sym
                                            && prop.span.ctxt == method_key.span.ctxt
                                        {
                                            return Ok(box Type::Function(ty::Function {
                                                span,
                                                type_params: m.type_params.clone(),
                                                params: m.params.clone(),
                                                ret_ty: m.ret_ty.clone(),
                                            }));
                                        }
                                    }
                                    _ => {}
                                },

                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }

                return Err(Error::NoSuchPropertyInClass {
                    span,
                    class_name: cls.name.clone(),
                    prop: prop.clone(),
                });
            }

            Type::ClassInstance(ClassInstance { ty, .. }) => {
                return self.access_property(span, ty.clone(), prop, computed, type_mode)
            }

            Type::Module(ty::Module { ref exports, .. }) => {
                match prop {
                    RExpr::Ident(ref i) => {
                        if let Some(item) = exports.vars.get(&i.into()) {
                            return Ok(item.clone());
                        }
                        //                        if let Some(item) =
                        // exports.types.get(sym) {
                        //                            return Ok(item.clone());
                        //                        }
                    }
                    _ => {}
                }
                // No property found
                return Err(Error::NoSuchPropertyInModule { span });
            }

            Type::This(..) => {
                if let Some(this) = self.scope.this().map(|this| this.into_owned()) {
                    return self.access_property(span, this, prop, computed, type_mode);
                } else if self.ctx.in_argument {
                    // We will adjust `this` using information from callee.
                    return Ok(Type::any(span));
                }
            }

            Type::Intersection(Intersection { ref types, .. }) => {
                // TODO: Verify if multiple type has field
                let mut new = vec![];
                for ty in types {
                    if let Some(v) = self
                        .access_property(span, ty.clone(), prop, computed, type_mode)
                        .ok()
                    {
                        new.push(v);
                    }
                }
                // print_backtrace();
                if new.len() == 1 {
                    return Ok(new.into_iter().next().unwrap());
                }

                return Ok(box Type::Union(Union { span, types: new }));
            }

            Type::Mapped(m) => {
                //
                let constraint = self::constraint_reducer::reduce(m);
                // If type of prop is equal to the type of index signature, it's
                // index access.

                match constraint {
                    Some(box Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: box Type::Array(..),
                        ..
                    })) => {
                        if let Ok(obj) = self.env.get_global_type(span, &js_word!("Array")) {
                            return self.access_property(span, obj, prop, computed, type_mode);
                        }
                    }
                    _ => {}
                }

                return Ok(m.ty.as_ref().cloned().unwrap_or_else(|| Type::any(span)));
            }

            Type::Ref(r) => {
                if computed {
                    let index_type = prop.validate_with_default(self)?;
                    // Return something like SimpleDBRecord<Flag>[Flag];
                    return Ok(box Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly: false,
                        obj_type: obj,
                        index_type,
                    }));
                } else {
                    match &r.type_name {
                        RTsEntityName::TsQualifiedName(_) => {}
                        RTsEntityName::Ident(i) => {
                            if let Some(class) = self.scope.get_this_class_name() {
                                if class == *i {
                                    return self.access_property(
                                        span,
                                        box Type::StaticThis(StaticThis { span }),
                                        prop,
                                        computed,
                                        type_mode,
                                    );
                                }
                            }
                        }
                    }

                    print_backtrace();
                    unreachable!(
                        "access_property: object type should be expanded before calling \
                         this\n:Object: {:#?}\nProperty: {:#?}",
                        obj, prop
                    )
                }
            }

            Type::IndexedAccessType(..) => {
                // TODO: Verify input type (obj.index_type)
                let ty = box Type::IndexedAccessType(IndexedAccessType {
                    span,
                    obj_type: obj,
                    readonly: false,
                    index_type: match prop {
                        RExpr::Ident(i) if !computed => {
                            let mut prop_ty = box Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span: DUMMY_SP,
                                lit: RTsLit::Str(RStr {
                                    span: DUMMY_SP,
                                    value: i.sym.clone(),
                                    has_escape: false,
                                    kind: Default::default(),
                                }),
                            });
                            self.prevent_generalize(&mut prop_ty);

                            prop_ty
                        }
                        _ => prop.validate_with_default(self)?,
                    },
                });
                return Ok(ty);
            }

            Type::Query(QueryType {
                expr: QueryExpr::TsEntityName(name),
                ..
            }) => {
                let obj = self.type_of_ts_entity_name(span, self.ctx.module_id, name, None)?;
                return self.access_property(span, obj, prop, computed, type_mode);
            }

            Type::Function(f) if type_mode == TypeOfMode::RValue => {
                // Use builtin type `Function`
                let interface = self.env.get_global_type(f.span, &js_word!("Function"))?;

                return self.access_property(span, interface, prop, computed, type_mode);
            }

            _ => {}
        }

        print_backtrace();
        unimplemented!(
            "access_property(MemberExpr):\nObject: {:?}\nProp: {:?}",
            obj,
            prop
        );
    }

    fn type_to_query_if_required(&mut self, span: Span, i: &RIdent, ty: Box<Type>) -> Box<Type> {
        if self.scope.is_calling() {
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
                    return box Type::Query(QueryType {
                        span,
                        expr: QueryExpr::TsEntityName(RTsEntityName::Ident(i.clone())),
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
        let id = i.into();

        let mut ty = self.type_of_raw_var(i, type_mode, type_args)?;

        let type_facts = self.scope.get_type_facts(&id)
            | self
                .cur_facts
                .true_facts
                .facts
                .get(&id)
                .copied()
                .unwrap_or(TypeFacts::None);

        ty = ty.apply_type_facts(type_facts);

        ty = self.type_to_query_if_required(span, i, ty);

        {
            fn exclude_types(ty: &mut Type, excludes: Option<&Vec<Box<Type>>>) {
                let excludes = match excludes {
                    Some(v) => v,
                    None => return,
                };
                let ty = ty.normalize_mut();

                for excluded in excludes {
                    exclude_type(ty, &excluded);
                }
            }

            let mut s = Some(&self.scope);

            while let Some(scope) = s {
                exclude_types(&mut ty, scope.facts.excludes.get(&id));
                s = scope.parent();
            }

            exclude_types(&mut ty, self.cur_facts.true_facts.excludes.get(&id));
        }

        slog::debug!(self.logger, "Type: {:?}", ty);

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
        slog::info!(
            self.logger,
            "({}) type_of_raw_var({})",
            self.scope.depth(),
            Id::from(i)
        );

        let span = i.span();

        if let Some(ref cls_name) = self.scope.this_class_name {
            if *cls_name == i {
                slog::warn!(
                    self.logger,
                    "Creating ref because we are currently defining a class: {}",
                    i.sym
                );
                return Ok(box Type::Ref(Ref {
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
                    return Ok(box Type::Function(ty::Function {
                        span,
                        params: vec![],
                        ret_ty: Type::any(span),
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
            println!(
                "({}) type_of({}): resolved import",
                self.scope.depth(),
                Id::from(i)
            );
            return Ok(ty.clone());
        }

        if let Some(v) = self.scope.vars.get(&i.into()) {
            slog::debug!(self.logger, "found var with name");
            if let Some(ty) = &v.ty {
                slog::debug!(self.logger, "Type of var: {:?}", ty);
                return Ok(ty.clone());
            }
        }

        // Check `declaring` before checking variables.
        if self.scope.declaring.contains(&i.into()) {
            println!(
                "({}) reference in initialization: {}",
                self.scope.depth(),
                i.sym
            );

            if self.ctx.allow_ref_declaring {
                return Ok(Type::any(span));
            } else {
                return Err(Error::ReferencedInInit { span });
            }
        }

        if let Some(ty) = self.find_var_type(&i.into()) {
            slog::debug!(self.logger, "find_var_type returned a type");
            let mut ty = ty.into_owned();
            if self.scope.kind().allows_respanning() {
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

        if i.sym == js_word!("Symbol") && !self.is_builtin {
            return Ok(self.env.get_global_var(i.span, &js_word!("Symbol"))?);
        }

        slog::error!(
            self.logger,
            "({}) Creating ref because we failed to resolve {}",
            self.scope.depth(),
            i.sym
        );
        // print_backtrace();

        Ok(box Type::Ref(Ref {
            span,
            ctxt: self.ctx.module_id,
            type_name: RTsEntityName::Ident(i.clone()),
            type_args: None,
        }))
    }

    pub(crate) fn type_of_ts_entity_name(
        &mut self,
        span: Span,
        ctxt: ModuleId,
        n: &RTsEntityName,
        type_args: Option<TypeParamInstantiation>,
    ) -> ValidationResult {
        match *n {
            RTsEntityName::Ident(ref i) => {
                if let Some(types) = self.find_type(ctxt, &i.into())? {
                    for ty in types {
                        match ty.normalize() {
                            Type::Interface(_)
                            | Type::Class(_)
                            | Type::ClassInstance(_)
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
                                let mut ty = box ty.into_owned().clone();
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

                Ok(box Type::Ref(Ref {
                    span,
                    ctxt: self.ctx.module_id,
                    type_name: RTsEntityName::Ident(i.clone()),
                    type_args,
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
                    &mut RExpr::Ident(qname.right.clone()),
                    false,
                    TypeOfMode::RValue,
                )
            }
        }
    }

    fn type_of_member_expr(
        &mut self,
        expr: &RMemberExpr,
        type_mode: TypeOfMode,
    ) -> ValidationResult {
        let RMemberExpr {
            ref obj,
            computed,
            ref prop,
            span,
            ..
        } = *expr;

        let mut errors = vec![];
        let obj_ty = match *obj {
            RExprOrSuper::Expr(ref obj) => {
                let obj_ty = match obj.validate_with_default(self) {
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

                obj_ty
            }

            RExprOrSuper::Super(..) => {
                if let Some(v) = self.scope.get_super_class() {
                    v.clone()
                } else {
                    unimplemented!("error reporting accessing super in a class without super class")
                }
            }
        };

        if computed {
            let obj_ty = self.expand_fully(span, obj_ty, true)?;
            let ty = match self.access_property(span, obj_ty, prop, computed, type_mode) {
                Ok(v) => Ok(v),
                Err(err) => {
                    errors.push(err);

                    Err(())
                }
            };
            if errors.is_empty() {
                return Ok(ty.unwrap());
            } else {
                // TODO: Remove duplicate: prop
                match prop.validate_with_default(self) {
                    Ok(..) => match ty {
                        Ok(ty) => {
                            if errors.is_empty() {
                                return Ok(ty);
                            } else {
                                return Err(Error::Errors { span, errors });
                            }
                        }
                        Err(()) => return Err(Error::Errors { span, errors }),
                    },
                    Err(err) => errors.push(err),
                }
            }
        } else {
            let ctx = Ctx {
                preserve_ref: false,
                ignore_expand_prevention_for_top: true,
                ..self.ctx
            };
            let obj_ty = self.with_ctx(ctx).expand_fully(span, obj_ty, true)?;

            match self.access_property(span, obj_ty, prop, computed, type_mode) {
                Ok(v) => return Ok(v),
                Err(err) => {
                    errors.push(err);
                    return Err(Error::Errors { span, errors });
                }
            }
        }

        if errors.len() == 1 {
            return Err(errors.remove(0));
        }
        return Err(Error::Errors { span, errors });
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, f: &RArrowExpr) -> ValidationResult<ty::Function> {
        self.record(f);

        self.with_child(
            ScopeKind::ArrowFn,
            Default::default(),
            |child: &mut Analyzer| {
                let type_params = try_opt!(f.type_params.validate_with(child));

                let params = {
                    let ctx = Ctx {
                        pat_mode: PatMode::Decl,
                        allow_ref_declaring: false,
                        ..child.ctx
                    };

                    for p in &f.params {
                        child.default_any_pat(p);
                    }

                    f.params.validate_with(&mut *child.with_ctx(ctx))?
                };

                let declared_ret_ty = match f.return_type.validate_with(child) {
                    Some(Ok(ty)) => Some(ty),
                    Some(Err(err)) => {
                        child.storage.report(err);
                        Some(Type::any(f.span))
                    }
                    None => None,
                };
                let declared_ret_ty = match declared_ret_ty {
                    Some(ty) => {
                        let span = ty.span();
                        Some(match *ty {
                            Type::Class(cls) => box Type::ClassInstance(ClassInstance {
                                span,
                                ty: box Type::Class(cls),
                                type_args: None,
                            }),
                            _ => ty,
                        })
                    }
                    None => None,
                };

                let inferred_return_type = {
                    match f.body {
                        RBlockStmtOrExpr::Expr(ref e) => Some({
                            let ty = e.validate_with_default(child)?;
                            ty.generalize_lit()
                        }),
                        RBlockStmtOrExpr::BlockStmt(ref s) => child.visit_stmts_for_return(
                            f.span,
                            f.is_async,
                            f.is_generator,
                            &s.stmts,
                        )?,
                    }
                };

                // Remove void from inferred return type.
                let inferred_return_type = inferred_return_type.map(|mut ty| {
                    match &mut *ty {
                        Type::Union(ty) => {
                            ty.types.retain(|ty| match &**ty {
                                Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsVoidKeyword,
                                    ..
                                }) => false,
                                _ => true,
                            });
                        }
                        _ => {}
                    }

                    ty
                });

                if let Some(ref declared) = declared_ret_ty {
                    let span = inferred_return_type.span();
                    if let Some(ref inferred) = inferred_return_type {
                        child.assign(declared, inferred, span)?;
                    }
                }

                Ok(ty::Function {
                    span: f.span,
                    params,
                    type_params,
                    ret_ty: declared_ret_ty.unwrap_or_else(|| {
                        inferred_return_type.unwrap_or_else(|| Type::void(f.span))
                    }),
                })
            },
        )
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit) -> ValidationResult {
        self.with_child(
            ScopeKind::ObjectLit,
            Default::default(),
            |a: &mut Analyzer| {
                let mut special_type = None;

                // TODO: Change order

                for prop in node.props.iter() {
                    match *prop {
                        RPropOrSpread::Prop(ref prop) => {
                            let p: TypeElement = prop.validate_with(a)?;
                            if let Some(key) = p.key() {
                                if a.scope.this_object_members.iter_mut().any(|element| {
                                    match element {
                                        ty::TypeElement::Property(prop)
                                            if (*prop.key).eq_ignore_span(&*key) =>
                                        {
                                            prop.readonly = false;
                                            true
                                        }
                                        _ => false,
                                    }
                                }) {
                                    continue;
                                }
                            }

                            a.scope.this_object_members.push(p);
                        }
                        RPropOrSpread::Spread(RSpreadElement { ref expr, .. }) => {
                            match *expr.validate_with_default(a)? {
                                Type::TypeLit(TypeLit {
                                    members: spread_members,
                                    ..
                                }) => {
                                    a.scope.this_object_members.extend(spread_members);
                                }

                                // Use last type on ...any or ...unknown
                                ty
                                @
                                Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })
                                | ty
                                @
                                Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) => special_type = Some(ty),

                                ty => unimplemented!("spread with non-type-lit: {:#?}", ty),
                            }
                        }
                    }
                }

                if let Some(ty) = special_type {
                    return Ok(box ty);
                }

                Ok(box Type::TypeLit(TypeLit {
                    span: node.span,
                    members: take(&mut a.scope.this_object_members),
                }))
            },
        )
    }
}

/// Exclude `excluded` from `ty`
fn exclude_type(ty: &mut Type, excluded: &Type) {
    match excluded {
        Type::Union(excluded) => {
            //
            for excluded in &excluded.types {
                exclude_type(ty, &excluded)
            }

            return;
        }
        _ => {}
    }

    match &mut *ty {
        Type::Union(ty) => {
            ty.types.retain(|element| !excluded.type_eq(element));
        }
        _ => {}
    }
}
