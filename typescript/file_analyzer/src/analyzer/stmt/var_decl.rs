use super::super::{pat::PatMode, Analyzer, Ctx};
use crate::analyzer::assign::AssignOpts;
use crate::analyzer::scope::VarKind;
use crate::util::should_instantiate_type_ann;
use crate::{
    analyzer::{
        expr::TypeOfMode,
        util::{Generalizer, ResultExt},
    },
    ty::{self, Tuple, Type, TypeParam},
    util::RemoveTypes,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::FoldWith;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RCallExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RTsAsExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsTypeAssertion;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_type_ops::Fix;
use stc_ts_types::QueryExpr;
use stc_ts_types::QueryType;
use stc_ts_types::{Array, Id, Operator, Symbol};
use stc_ts_types::{EnumVariant, Instance};
use stc_ts_utils::find_ids_in_pat;
use stc_ts_utils::PatExt;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::Spanned;
use swc_ecma_ast::*;
use ty::TypeExt;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, var: &RVarDecl) {
        self.record(&*var);

        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            var_kind: var.kind,
            in_declare: self.ctx.in_declare || var.declare,
            allow_ref_declaring: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a| {
            var.decls.visit_with(a);
        });

        // Set type of tuples.
        for decl in &var.decls {
            match &decl.name {
                RPat::Array(RArrayPat {
                    span, elems, node_id, ..
                }) => {
                    if let Some(m) = &self.mutations {
                        if let Some(Type::Tuple(tuple)) = m.for_pats.get(&node_id).map(|m| &m.ty).cloned().flatten() {
                            for (i, elem) in elems.iter().enumerate() {
                                match elem {
                                    Some(pat) => {
                                        //
                                        if i < tuple.elems.len() {
                                            let ty = &tuple.elems[i].ty;
                                            if let Some(node_id) = pat.node_id() {
                                                if let Some(m) = &mut self.mutations {
                                                    m.for_pats.entry(node_id).or_default().ty = Some(*ty.clone());
                                                }
                                            }
                                        }
                                    }
                                    None => {}
                                }
                            }
                        }
                    }
                    //
                }
                // TODO
                //  RPat::Object(obj) => {}
                _ => {}
            }
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, v: &RVarDeclarator) {
        self.record(v);

        let marks = self.marks();

        let kind = self.ctx.var_kind;
        let node_id = v.node_id;

        let res: Result<_, _> = try {
            let v_span = v.span();
            if !self.is_builtin {
                debug_assert!(!v_span.is_dummy());
            }

            let debug_declaring = if cfg!(debug_assertions) {
                Some(self.scope.declaring.clone())
            } else {
                None
            };
            let ids: Vec<Id> = find_ids_in_pat(&v.name);
            let prev_declaring_len = self.scope.declaring.len();
            self.scope.declaring.extend(ids);

            macro_rules! inject_any {
                () => {
                    // Declare variable with type any
                    match self.declare_complex_vars(
                        kind,
                        &v.name,
                        Type::any(v_span),
                        Some(Type::any(v_span)),
                        None,
                    ) {
                        Ok(()) => {}
                        Err(err) => {
                            self.storage.report(err);
                        }
                    }
                };
            }

            let forced_type_ann = {
                // let a = {} as Foo
                match &v.init {
                    Some(box RExpr::TsAs(RTsAsExpr { type_ann, .. })) => Some(type_ann.validate_with(self)?),

                    Some(box RExpr::TsTypeAssertion(RTsTypeAssertion { type_ann, .. })) => {
                        Some(type_ann.validate_with(self)?)
                    }
                    _ => None,
                }
            };

            // If user specified type, value should be removed.
            let should_remove_value = v.name.get_ty().is_some();

            macro_rules! remove_declaring {
                () => {{
                    if should_remove_value {
                        if let Some(m) = &mut self.mutations {
                            m.for_var_decls.entry(node_id).or_default().remove_init = true;
                        }
                    }
                    self.scope.declaring.drain(prev_declaring_len..);
                    debug_assert_eq!(Some(self.scope.declaring.clone()), debug_declaring);
                }};
            }

            if let Some(ref init) = v.init {
                let span = init.span();
                let is_symbol_call = match &**init {
                    RExpr::Call(RCallExpr {
                        callee:
                            RExprOrSuper::Expr(box RExpr::Ident(RIdent {
                                sym: js_word!("Symbol"),
                                ..
                            })),
                        ..
                    }) => true,
                    _ => false,
                };

                // Set `this` in
                //
                // export let p1: Point = {
                //     x: 10,
                //     y: 20,
                //     moveBy(dx, dy, dz) {
                //         this.x += dx;
                //         this.y += dy;
                //         if (this.z && dz) {
                //             this.z += dz;
                //         }
                //     }
                // };
                let creates_new_this = match &**init {
                    RExpr::Object(..) => true,
                    _ => false,
                };

                let old_this = if creates_new_this { self.scope.this.take() } else { None };

                let declared_ty = v.name.get_ty();
                if declared_ty.is_some() {
                    //TODO:
                    // self.span_allowed_implicit_any = span;
                }

                macro_rules! get_value_ty {
                    ($ty:expr) => {{
                        match init.validate_with_args(self, (TypeOfMode::RValue, None, $ty)) {
                            Ok(ty) => {
                                if creates_new_this {
                                    self.scope.this = old_this;
                                }
                                ty
                            }
                            Err(err) => {
                                if creates_new_this {
                                    self.scope.this = old_this;
                                }
                                if self.is_builtin {
                                    unreachable!("failed to assign builtin: \nError: {:?}", err)
                                } else {
                                    self.storage.report(err);
                                }
                                inject_any!();
                                remove_declaring!();
                                return Ok(());
                            }
                        }
                    }};
                }

                debug_assert_eq!(self.ctx.allow_ref_declaring, true);

                //  Check if v_ty is assignable to ty
                match declared_ty {
                    Some(ty) => {
                        slog::debug!(self.logger, "var: user declared type");
                        let ty = match ty.validate_with(self) {
                            Ok(ty) => ty,
                            Err(err) => {
                                self.storage.report(err);
                                remove_declaring!();
                                return Ok(());
                            }
                        };
                        let ty = self.expand(span, ty)?;
                        ty.assert_valid();
                        let ty = (|| {
                            if !should_instantiate_type_ann(&ty) {
                                return ty;
                            }

                            Type::Instance(Instance {
                                span: ty.span(),
                                ty: box ty,
                            })
                        })();
                        ty.assert_valid();
                        self.check_rvalue(span, &v.name, &ty);

                        self.scope.this = Some(ty.clone().remove_falsy());
                        let mut value_ty = get_value_ty!(Some(&ty));
                        value_ty.assert_valid();
                        value_ty = self.expand(span, value_ty)?;
                        value_ty.assert_valid();
                        value_ty = self.rename_type_params(span, value_ty, Some(&ty))?;
                        value_ty.assert_valid();

                        let opts = AssignOpts {
                            span: v_span,
                            allow_unknown_rhs: match &**init {
                                RExpr::Ident(..)
                                | RExpr::Member(..)
                                | RExpr::MetaProp(..)
                                | RExpr::New(..)
                                | RExpr::Call(..) => true,
                                _ => false,
                            },
                            ..Default::default()
                        };

                        match self
                            .assign_with_opts(&mut Default::default(), opts, &ty, &value_ty)
                            .context("tried to assign from var decl")
                        {
                            Ok(()) => {
                                let mut ty = ty.cheap();
                                self.prevent_generalize(&mut ty);

                                let actual_ty = self.narrowed_type_of_assignment(span, ty.clone(), &value_ty)?;

                                actual_ty.assert_valid();

                                // let ty = ty.fold_with(&mut Generalizer::default());
                                match self.declare_complex_vars(kind, &v.name, ty, Some(actual_ty), None) {
                                    Ok(()) => {}
                                    Err(err) => {
                                        self.storage.report(err);
                                    }
                                }
                                remove_declaring!();
                                return Ok(());
                            }
                            Err(err) => {
                                self.storage.report(err);

                                match self.declare_complex_vars(kind, &v.name, ty, None, None) {
                                    Ok(()) => {}
                                    Err(err) => {
                                        self.storage.report(err);
                                    }
                                }

                                Some(init)
                            }
                        }
                    }
                    None => {
                        let value_ty = get_value_ty!(None);

                        // infer type from value.
                        let mut ty = (|| -> ValidationResult<_> {
                            match value_ty.normalize() {
                                Type::TypeLit(..) | Type::Function(..) | Type::Query(..) => {
                                    if let Some(m) = &mut self.mutations {
                                        m.for_var_decls.entry(v.node_id).or_default().remove_init = true;
                                    }
                                }
                                _ => {}
                            }

                            Ok(value_ty)
                        })()?;

                        let should_generalize_fully = self.may_generalize(&ty) && !contains_type_param(&ty);

                        slog::debug!(self.logger, "var: user did not declare type");
                        let mut ty = self.rename_type_params(span, ty, None)?;
                        ty.fix();
                        ty.assert_valid();

                        if !(self.ctx.var_kind == VarDeclKind::Const && ty.is_lit()) {
                            if self.may_generalize(&ty) {
                                // Vars behave differently based on the context.
                                if self.ctx.can_generalize_literals() {
                                    ty = ty.generalize_lit(marks);
                                } else {
                                    ty = ty.fold_with(&mut Generalizer { force: false, marks });
                                }
                            }
                        }

                        ty.assert_valid();

                        slog::debug!(
                            self.logger,
                            "[vars]: Type after generalization: {}",
                            dump_type_as_string(&self.cm, &ty)
                        );

                        if should_generalize_fully {
                            self.normalize_tuples(&mut ty);
                            ty.assert_valid();
                            ty = match ty.normalize() {
                                Type::Function(f) => {
                                    let ret_ty = box f.ret_ty.clone().generalize_lit(marks);
                                    Type::Function(stc_ts_types::Function { ret_ty, ..f.clone() })
                                }

                                _ => ty,
                            };
                        }

                        slog::debug!(
                            self.logger,
                            "[vars]: Type after normalization: {}",
                            dump_type_as_string(&self.cm, &ty)
                        );

                        match ty.normalize() {
                            Type::Ref(..) => {
                                let ctx = Ctx {
                                    preserve_ref: true,
                                    ignore_expand_prevention_for_all: false,
                                    ignore_expand_prevention_for_top: false,
                                    ..self.ctx
                                };
                                ty = self.with_ctx(ctx).expand(span, ty)?;
                                ty.assert_valid();

                                slog::debug!(
                                    self.logger,
                                    "[vars]: Type after expansion: {}",
                                    dump_type_as_string(&self.cm, &ty)
                                );
                            }
                            _ => {}
                        }

                        ty.assert_valid();

                        if self.scope.is_root() {
                            let ty = Some(forced_type_ann.unwrap_or_else(|| {
                                let ty = ty.clone();

                                // Normalize unresolved parameters
                                let ty = match ty.normalize() {
                                    Type::Param(TypeParam {
                                        constraint: Some(ty), ..
                                    }) => *ty.clone(),
                                    _ => ty,
                                };

                                let ty = match ty.normalize() {
                                    // `err is Error` => boolean
                                    Type::Predicate(..) => Type::Keyword(RTsKeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                                    }),

                                    Type::Keyword(RTsKeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                                    })
                                    | Type::Operator(Operator {
                                        span,
                                        op: TsTypeOperatorOp::Unique,
                                        ty:
                                            box Type::Keyword(RTsKeywordType {
                                                kind: TsKeywordTypeKind::TsSymbolKeyword,
                                                ..
                                            }),
                                    })
                                    | Type::Symbol(Symbol { span, .. }) => {
                                        match self.ctx.var_kind {
                                            // It's `uniqute symbol` only if it's `Symbol()`
                                            VarDeclKind::Const if is_symbol_call => Type::Operator(Operator {
                                                span: *span,
                                                op: TsTypeOperatorOp::Unique,
                                                ty: box Type::Keyword(RTsKeywordType {
                                                    span: *span,
                                                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                                                }),
                                            }),

                                            _ => Type::Keyword(RTsKeywordType {
                                                span: *span,
                                                kind: TsKeywordTypeKind::TsSymbolKeyword,
                                            }),
                                        }
                                    }

                                    Type::Array(Array {
                                        span,
                                        elem_type:
                                            box Type::Param(TypeParam {
                                                span: elem_span,
                                                constraint,
                                                ..
                                            }),
                                        ..
                                    }) => {
                                        Type::Array(Array {
                                            span: *span,
                                            elem_type: match constraint {
                                                Some(_constraint) => {
                                                    // TODO: We need something smarter
                                                    box Type::Keyword(RTsKeywordType {
                                                        span: *elem_span,
                                                        kind: TsKeywordTypeKind::TsAnyKeyword,
                                                    })
                                                }
                                                None => box Type::Keyword(RTsKeywordType {
                                                    span: *elem_span,
                                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                                }),
                                            },
                                        })
                                    }

                                    // We failed to infer type of the type parameter.
                                    Type::Param(TypeParam { span, .. }) => Type::Keyword(RTsKeywordType {
                                        span: *span,
                                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    }),

                                    _ => ty,
                                };

                                ty
                            }));

                            if let Some(box RExpr::Ident(ref alias)) = &v.init {
                                if let RPat::Ident(ref i) = v.name {
                                    if let Some(m) = &mut self.mutations {
                                        m.for_pats.entry(i.node_id).or_default().ty = Some(Type::Query(QueryType {
                                            span,
                                            expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(alias.clone())),
                                        }));
                                    }
                                }
                            }
                            if !should_remove_value {
                                let node_id = v.name.node_id();
                                if let Some(node_id) = node_id {
                                    if let Some(m) = &mut self.mutations {
                                        m.for_pats.entry(node_id).or_default().ty = ty;
                                    }
                                }
                            }
                        }
                        match ty.normalize() {
                            Type::Ref(..) => {}
                            _ => {
                                let ctx = Ctx {
                                    preserve_ref: true,
                                    ignore_expand_prevention_for_all: false,
                                    ignore_expand_prevention_for_top: false,
                                    preserve_params: true,
                                    preserve_ret_ty: true,
                                    ..self.ctx
                                };
                                ty = self.with_ctx(ctx).expand(span, ty)?;
                            }
                        }
                        ty.assert_valid();

                        self.check_rvalue(span, &v.name, &ty);

                        let mut type_errors = Errors::default();

                        // Handle implicit any

                        match ty.normalize_mut() {
                            Type::Tuple(Tuple { ref mut elems, .. }) if !elems.is_empty() => {
                                for (i, element) in elems.iter_mut().enumerate() {
                                    let span = element.span();

                                    match *element.ty.normalize() {
                                        Type::Keyword(RTsKeywordType {
                                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                            ..
                                        })
                                        | Type::Keyword(RTsKeywordType {
                                            kind: TsKeywordTypeKind::TsNullKeyword,
                                            ..
                                        }) => {}
                                        _ => {
                                            continue;
                                        }
                                    }
                                    // Widen tuple types
                                    element.ty = box Type::any(span);

                                    if self.rule().no_implicit_any {
                                        match v.name {
                                            RPat::Ident(ref i) => {
                                                let span = i.id.span;
                                                type_errors
                                                    .push(Error::ImplicitAny { span }.context("tuple type widenning"));
                                                break;
                                            }
                                            RPat::Array(RArrayPat { ref elems, .. }) => {
                                                let span = elems[i].span();
                                                type_errors
                                                    .push(Error::ImplicitAny { span }.context("tuple type widenning"));
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }

                        ty.assert_valid();

                        if !type_errors.is_empty() {
                            self.storage.report_all(type_errors);
                            remove_declaring!();
                            return Ok(());
                        }

                        let var_ty = (|| -> ValidationResult<_> {
                            match ty.normalize() {
                                Type::EnumVariant(ref v) => {
                                    if let Some(items) = self.find_type(self.ctx.module_id, &v.enum_name)? {
                                        for ty in items {
                                            if let Type::Enum(ref e) = ty.normalize() {
                                                return Ok(Type::EnumVariant(EnumVariant {
                                                    name: None,
                                                    ..v.clone()
                                                }));
                                            }
                                        }
                                    }
                                    unreachable!("Failed to found enum named `{}`", v.enum_name)
                                }
                                _ => Ok(ty),
                            }
                        })()?
                        .cheap();

                        self.declare_complex_vars(kind, &v.name, var_ty.clone(), None, None)
                            .report(&mut self.storage);
                        remove_declaring!();
                        return Ok(());
                    }
                }
            } else {
                let var_ty = self
                    .mutations
                    .as_ref()
                    .and_then(|m| m.for_pats.get(&v.node_id))
                    .and_then(|v| v.ty.as_ref())
                    .cloned();

                if let Some(var_ty) = var_ty {
                    self.declare_complex_vars(kind, &v.name, var_ty, None, None)
                        .report(&mut self.storage);
                    remove_declaring!();
                    return Ok(());
                }

                match v.name {
                    RPat::Ident(ref i) => {
                        //
                        let sym: Id = (&i.id).into();
                        let mut ty = try_opt!(i.type_ann.validate_with(self));
                        ty.fix();
                        ty = ty.map(|ty| {
                            if !should_instantiate_type_ann(&ty) {
                                return ty;
                            }

                            Type::Instance(Instance {
                                span: i.id.span,
                                ty: box ty,
                            })
                        });
                        match ty {
                            Some(ref mut ty) => {
                                self.prevent_expansion(&mut *ty);
                            }
                            _ => {}
                        }

                        if !self.is_builtin {
                            // Report error if type is not found.
                            if let Some(ty) = &ty {
                                self.normalize(Some(i.id.span), Cow::Borrowed(ty), Default::default())
                                    .report(&mut self.storage);
                            }
                        }

                        match self.declare_var(
                            i.id.span,
                            VarKind::Decl(kind),
                            sym,
                            ty,
                            None,
                            // initialized
                            false,
                            // allow_multiple
                            kind == VarDeclKind::Var,
                            false,
                        ) {
                            Ok(()) => {}
                            Err(err) => {
                                self.storage.report(err);
                            }
                        };
                    }
                    _ => {
                        // For ambient contexts and loops, we add variables to the scope.

                        match self.declare_vars(kind, &v.name) {
                            Ok(()) => {}
                            Err(err) => {
                                self.storage.report(err);
                            }
                        }
                    }
                };
                remove_declaring!();
                return Ok(());
            };

            debug_assert_eq!(self.ctx.allow_ref_declaring, true);
            if v.name.get_ty().is_none() {
                self.declare_vars(kind, &v.name).report(&mut self.storage);
            }

            remove_declaring!();
        };

        res.report(&mut self.storage);

        Ok(())
    }
}

struct TypeParamFinder {
    found: bool,
}

impl Visit<TypeParam> for TypeParamFinder {
    fn visit(&mut self, _: &TypeParam) {
        self.found = true;
    }
}

fn contains_type_param<T>(node: &T) -> bool
where
    T: VisitWith<TypeParamFinder>,
{
    let mut v = TypeParamFinder { found: false };

    node.visit_with(&mut v);

    v.found
}
