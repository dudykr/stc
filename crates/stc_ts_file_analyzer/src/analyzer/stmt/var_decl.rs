use std::borrow::Cow;

use rnode::{FoldWith, Visit, VisitWith};
use stc_ts_ast_rnode::{
    RArrayPat, RArrowExpr, RBindingIdent, RCallExpr, RCallee, RExpr, RIdent, RPat, RTsAsExpr, RTsEntityName, RTsFnOrConstructorType,
    RTsFnParam, RTsFnType, RTsQualifiedName, RTsType, RTsTypeAssertion, RVarDecl, RVarDeclarator,
};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt, ErrorKind, Errors};
use stc_ts_type_ops::{generalization::prevent_generalize, Fix};
use stc_ts_types::{
    Array, EnumVariant, Id, Instance, InstanceMetadata, KeywordType, KeywordTypeMetadata, OperatorMetadata, QueryExpr, QueryType, Symbol,
    SymbolMetadata, ThisType, Unique,
};
use stc_ts_utils::{find_ids_in_pat, PatExt};
use stc_utils::cache::Freeze;
use swc_atoms::js_word;
use swc_common::{Spanned, SyntaxContext};
use swc_ecma_ast::*;
use tracing::debug;
use ty::TypeExt;

use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::TypeOfMode,
        pat::PatMode,
        scope::{vars::DeclareVarsOpts, ExpandOpts, VarKind},
        types::NormalizeTypeOpts,
        util::{Generalizer, ResultExt},
        Analyzer, Ctx, ScopeKind,
    },
    ty::{self, Tuple, Type, TypeParam},
    util::{should_instantiate_type_ann, RemoveTypes},
    validator,
    validator::ValidateWith,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, var: &RVarDecl) {
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
            if let RPat::Array(RArrayPat { span, elems, node_id, .. }) = &decl.name {
                if let Some(m) = &self.mutations {
                    if let Some(Type::Tuple(tuple)) = m.for_pats.get(node_id).map(|m| &m.ty).cloned().flatten() {
                        for (i, elem) in elems.iter().enumerate() {
                            if let Some(pat) = elem {
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
                        }
                    }
                }
                //
            }
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, v: &RVarDeclarator) {
        let marks = self.marks();

        let kind = self.ctx.var_kind;
        let node_id = v.node_id;

        let res: Result<_, _> = try {
            let v_span = v.span();
            if !self.config.is_builtin {
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
                        VarKind::Var(kind),
                        &v.name,
                        Type::any(v_span, Default::default()),
                        Some(Type::any(v_span, Default::default())),
                        None,
                    ) {
                        Ok(..) => {}
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

                    Some(box RExpr::TsTypeAssertion(RTsTypeAssertion { type_ann, .. })) => Some(type_ann.validate_with(self)?),
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
                let is_symbol_call = matches!(
                    &**init,
                    RExpr::Call(RCallExpr {
                        callee: RCallee::Expr(box RExpr::Ident(RIdent {
                            sym: js_word!("Symbol"),
                            ..
                        })),
                        ..
                    })
                );

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
                let creates_new_this = matches!(&**init, RExpr::Object(..));

                let old_this = if creates_new_this { self.scope.this.take() } else { None };

                let declared_ty = v.name.get_ty();
                let declared_ty_include_this =
                    if let Some(RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsFnType(RTsFnType { params, .. }))) = declared_ty {
                        if let [RTsFnParam::Ident(RBindingIdent { id, .. }), ..] = &params[..] {
                            id.sym == js_word!("this")
                        } else {
                            false
                        }
                    } else {
                        false
                    };
                if declared_ty.is_some() {
                    //TODO(kdy1):
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
                                if self.config.is_builtin {
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

                debug_assert!(self.ctx.allow_ref_declaring);

                //  Check if v_ty is assignable to ty
                match declared_ty {
                    Some(ty) => {
                        debug!("var: user declared type");
                        let mut ty = match ty.validate_with(self) {
                            Ok(ty) => ty,
                            Err(err) => {
                                self.storage.report(err);
                                remove_declaring!();
                                return Ok(());
                            }
                        };

                        if declared_ty_include_this {
                            if let box RExpr::Arrow(RArrowExpr { .. }) = init {
                                if let Type::Function(stc_ts_types::Function { params, .. }) = ty.normalize_mut() {
                                    *params = params[1..].to_vec();
                                }
                            }
                        }

                        let ty = self.expand(span, ty, Default::default())?;
                        let error_for_null_or_undefined = if matches!(v.name, RPat::Array(..) | RPat::Object(..)) {
                            self.deny_null_or_undefined(span, &ty)
                        } else {
                            Ok(())
                        };

                        ty.assert_valid();
                        let mut ty = (|| {
                            if !should_instantiate_type_ann(&ty) {
                                return ty;
                            }

                            Type::Instance(Instance {
                                span: ty.span(),
                                metadata: InstanceMetadata { common: ty.metadata() },
                                ty: box ty,
                                tracker: Default::default(),
                            })
                        })();
                        ty.assert_valid();
                        ty.freeze();
                        self.report_error_for_invalid_rvalue(span, &v.name, &ty);

                        self.scope.this = Some(
                            if matches!(
                                ty.normalize(),
                                Type::Query(QueryType {
                                    expr: box QueryExpr::TsEntityName(RTsEntityName::TsQualifiedName(box RTsQualifiedName {
                                        left: RTsEntityName::Ident(RIdent { sym: js_word!("this"), .. }),
                                        ..
                                    })),
                                    ..
                                })
                            ) || self.ctx.in_class_member
                            {
                                if self.scope.this().is_some() {
                                    self.scope.this().unwrap().into_owned()
                                } else {
                                    if matches!(self.scope.kind(), ScopeKind::ArrowFn) {
                                        // ```ts
                                        // const Test8 = () => {
                                        //     let x: typeof this.no = 1;
                                        //   };
                                        // ```
                                        Type::Query(QueryType {
                                            span,
                                            expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(RIdent::new(
                                                "globalThis".into(),
                                                span.with_ctxt(SyntaxContext::empty()),
                                            ))),
                                            metadata: Default::default(),
                                            tracker: Default::default(),
                                        })
                                    } else {
                                        // ```ts
                                        // class Test {
                                        //     data = {};
                                        //     constructor() {
                                        //       var copy: typeof this.data = {};
                                        //     }
                                        //   }
                                        // ```
                                        Type::This(ThisType {
                                            span,
                                            metadata: Default::default(),
                                            tracker: Default::default(),
                                        })
                                    }
                                }
                            } else {
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
                                ty.clone().remove_falsy()
                            },
                        );

                        let mut value_ty = get_value_ty!(Some(&ty));
                        value_ty.assert_valid();
                        value_ty = self.expand(span, value_ty, Default::default())?;
                        value_ty.assert_valid();
                        value_ty.freeze();

                        let opts = AssignOpts {
                            span: v_span,
                            allow_unknown_rhs: match &**init {
                                RExpr::Ident(..) | RExpr::Member(..) | RExpr::MetaProp(..) | RExpr::New(..) | RExpr::Call(..) => Some(true),
                                _ => None,
                            },
                            ..Default::default()
                        };

                        let res = match error_for_null_or_undefined {
                            Ok(()) => self
                                .assign_with_opts(&mut Default::default(), &ty, &value_ty, opts)
                                .context("tried to assign from var decl"),
                            Err(err) => Err(err),
                        };
                        match res {
                            Ok(()) => {
                                let mut ty = ty;
                                prevent_generalize(&mut ty);
                                ty.freeze();

                                let actual_ty = self.narrowed_type_of_assignment(span, ty.clone(), &value_ty)?.freezed();

                                actual_ty.assert_valid();

                                // let ty = ty.fold_with(&mut Generalizer::default());
                                match self.declare_complex_vars(VarKind::Var(kind), &v.name, ty, Some(actual_ty), None) {
                                    Ok(..) => {}
                                    Err(err) => {
                                        self.storage.report(err);
                                    }
                                }
                                remove_declaring!();
                                return Ok(());
                            }
                            Err(err) => {
                                self.storage.report(err);

                                match self.declare_complex_vars(VarKind::Var(kind), &v.name, ty, None, None) {
                                    Ok(..) => {}
                                    Err(err) => {
                                        self.storage.report(err);
                                    }
                                }

                                Some(init)
                            }
                        }
                    }
                    None => {
                        self.ctx.prefer_tuple_for_array_lit = matches!(v.name, RPat::Array(_) | RPat::Object(..));
                        let value_ty = get_value_ty!(None);

                        // infer type from value.
                        let ty = {
                            match value_ty.normalize() {
                                Type::TypeLit(..) | Type::Function(..) | Type::Query(..) => {
                                    if let Some(m) = &mut self.mutations {
                                        m.for_var_decls.entry(v.node_id).or_default().remove_init = true;
                                    }
                                }
                                _ => {}
                            }

                            value_ty
                        };

                        let should_generalize_fully =
                            !matches!(v.name, RPat::Array(_) | RPat::Object(..)) && self.may_generalize(&ty) && !contains_type_param(&ty);

                        debug!("var: user did not declare type");
                        let mut ty = self.rename_type_params(span, ty, None)?;
                        ty.fix();
                        ty.assert_valid();

                        if self.ctx.var_kind == VarDeclKind::Const && ty.is_lit() {
                            prevent_generalize(&mut ty);
                        }

                        #[allow(clippy::nonminimal_bool)]
                        if !(self.ctx.var_kind == VarDeclKind::Const && ty.is_lit()) && !matches!(v.name, RPat::Array(_) | RPat::Object(..))
                        {
                            if self.may_generalize(&ty) {
                                // Vars behave differently based on the context.
                                if self.ctx.can_generalize_literals() {
                                    ty = ty.generalize_lit();
                                } else {
                                    ty = ty.fold_with(&mut Generalizer { force: false });
                                }
                            }
                        }

                        ty.assert_valid();

                        debug!("[vars]: Type after generalization: {}", dump_type_as_string(&ty));

                        if should_generalize_fully {
                            match v.name {
                                RPat::Array(_) => {}
                                _ => {
                                    self.normalize_tuples(&mut ty);
                                }
                            }
                            ty.assert_valid();
                            ty.freeze();
                            ty = match ty.normalize() {
                                Type::Function(f) => {
                                    let ret_ty = box f.ret_ty.clone().generalize_lit();
                                    Type::Function(stc_ts_types::Function { ret_ty, ..f.clone() })
                                }

                                _ => ty,
                            };
                        }

                        debug!("[vars]: Type after normalization: {}", dump_type_as_string(&ty));

                        if let Type::Ref(..) = ty.normalize() {
                            ty = self.expand(
                                span,
                                ty,
                                ExpandOpts {
                                    preserve_ref: true,
                                    ..Default::default()
                                },
                            )?;
                            ty.assert_valid();

                            debug!("[vars]: Type after expansion: {}", dump_type_as_string(&ty));
                        }

                        ty.assert_valid();
                        ty.freeze();

                        if self.scope.is_root() {
                            let ty = Some(forced_type_ann.unwrap_or_else(|| {
                                let ty = ty.clone();

                                // Normalize unresolved parameters
                                let ty = match ty.normalize() {
                                    Type::Param(TypeParam { constraint: Some(ty), .. }) => *ty.clone(),
                                    _ => ty,
                                };

                                let ty = match ty.normalize() {
                                    // `err is Error` => boolean
                                    Type::Predicate(..) => Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),

                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                                        metadata: KeywordTypeMetadata { common, .. },
                                        ..
                                    })
                                    | Type::Unique(Unique {
                                        span,
                                        ty:
                                            box Type::Keyword(KeywordType {
                                                kind: TsKeywordTypeKind::TsSymbolKeyword,
                                                ..
                                            }),
                                        metadata: OperatorMetadata { common, .. },
                                        ..
                                    })
                                    | Type::Symbol(Symbol {
                                        span,
                                        metadata: SymbolMetadata { common, .. },
                                        ..
                                    }) => {
                                        match self.ctx.var_kind {
                                            // It's `unique symbol` only if it's `Symbol()`
                                            VarDeclKind::Const if is_symbol_call => Type::Unique(Unique {
                                                span: *span,
                                                ty: box Type::Keyword(KeywordType {
                                                    span: *span,
                                                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                                                    metadata: KeywordTypeMetadata {
                                                        common: *common,
                                                        ..Default::default()
                                                    },
                                                    tracker: Default::default(),
                                                }),
                                                metadata: OperatorMetadata {
                                                    common: *common,
                                                    ..Default::default()
                                                },
                                                tracker: Default::default(),
                                            }),

                                            _ => Type::Keyword(KeywordType {
                                                span: *span,
                                                kind: TsKeywordTypeKind::TsSymbolKeyword,
                                                metadata: KeywordTypeMetadata {
                                                    common: *common,
                                                    ..Default::default()
                                                },
                                                tracker: Default::default(),
                                            }),
                                        }
                                    }

                                    Type::Array(Array {
                                        span,
                                        elem_type:
                                            box Type::Param(TypeParam {
                                                span: elem_span,
                                                constraint,
                                                metadata: elem_metadata,
                                                ..
                                            }),
                                        metadata,
                                        ..
                                    }) => {
                                        Type::Array(Array {
                                            span: *span,
                                            elem_type: match constraint {
                                                Some(_constraint) => {
                                                    // TODO(kdy1): We need something smarter
                                                    box Type::Keyword(KeywordType {
                                                        span: *elem_span,
                                                        kind: TsKeywordTypeKind::TsAnyKeyword,
                                                        metadata: KeywordTypeMetadata {
                                                            common: elem_metadata.common,
                                                            ..Default::default()
                                                        },
                                                        tracker: Default::default(),
                                                    })
                                                }
                                                None => box Type::Keyword(KeywordType {
                                                    span: *elem_span,
                                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                                    metadata: KeywordTypeMetadata {
                                                        common: elem_metadata.common,
                                                        ..Default::default()
                                                    },
                                                    tracker: Default::default(),
                                                }),
                                            },
                                            metadata: *metadata,
                                            tracker: Default::default(),
                                        })
                                    }

                                    // We failed to infer type of the type parameter.
                                    Type::Param(TypeParam { span, metadata, .. }) => Type::Keyword(KeywordType {
                                        span: *span,
                                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                                        metadata: KeywordTypeMetadata {
                                            common: metadata.common,
                                            ..Default::default()
                                        },
                                        tracker: Default::default(),
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
                                            metadata: Default::default(),
                                            tracker: Default::default(),
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
                                ty = self.expand(
                                    span,
                                    ty,
                                    ExpandOpts {
                                        preserve_ref: true,
                                        ..Default::default()
                                    },
                                )?;
                            }
                        }
                        ty.assert_valid();

                        self.report_error_for_invalid_rvalue(span, &v.name, &ty);

                        let mut type_errors = Errors::default();

                        // Handle implicit any

                        // TODO(kdy1): PERF
                        match ty.normalize_mut() {
                            Type::Tuple(Tuple { ref mut elems, .. }) if !elems.is_empty() => {
                                for (i, element) in elems.iter_mut().enumerate() {
                                    let span = element.span();

                                    match *element.ty.normalize() {
                                        Type::Keyword(KeywordType {
                                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                            ..
                                        })
                                        | Type::Keyword(KeywordType {
                                            kind: TsKeywordTypeKind::TsNullKeyword,
                                            ..
                                        }) => {}
                                        _ => {
                                            continue;
                                        }
                                    }
                                    // Widen tuple types
                                    element.ty = box Type::any(
                                        span,
                                        KeywordTypeMetadata {
                                            common: element.ty.metadata(),
                                            ..Default::default()
                                        },
                                    );

                                    if self.rule().no_implicit_any {
                                        match v.name {
                                            RPat::Ident(ref i) => {
                                                let span = i.id.span;
                                                type_errors.push(ErrorKind::ImplicitAny { span }.context("tuple type widening"));
                                                break;
                                            }
                                            RPat::Array(RArrayPat { ref elems, .. }) => {
                                                let span = elems[i].span();
                                                type_errors.push(ErrorKind::ImplicitAny { span }.context("tuple type widening"));
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

                        let var_ty = match ty.normalize() {
                            Type::EnumVariant(ref v) => Type::EnumVariant(EnumVariant { name: None, ..v.clone() }),
                            _ => ty,
                        }
                        .freezed();

                        self.declare_complex_vars(VarKind::Var(kind), &v.name, var_ty, None, None)
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
                    .cloned()
                    .freezed();

                if let Some(var_ty) = var_ty {
                    self.declare_complex_vars(VarKind::Var(kind), &v.name, var_ty, None, None)
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
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                        });
                        if let Some(ref mut ty) = ty {
                            self.prevent_expansion(&mut *ty);
                        }

                        ty.freeze();

                        if !self.config.is_builtin {
                            // Report error if type is not found.
                            if let Some(ty) = &ty {
                                self.normalize(
                                    Some(i.id.span),
                                    Cow::Borrowed(ty),
                                    NormalizeTypeOpts {
                                        preserve_global_this: true,
                                        ..Default::default()
                                    },
                                )
                                .report(&mut self.storage);
                            }
                        }

                        match self.declare_var(
                            i.id.span,
                            VarKind::Var(kind),
                            sym,
                            ty,
                            None,
                            // initialized
                            false,
                            // allow_multiple
                            kind == VarDeclKind::Var,
                            false,
                            false,
                        ) {
                            Ok(..) => {}
                            Err(err) => {
                                self.storage.report(err);
                            }
                        };
                    }
                    _ => {
                        // For ambient contexts and loops, we add variables to the scope.

                        match self.add_vars(
                            &v.name,
                            None,
                            None,
                            None,
                            DeclareVarsOpts {
                                kind: VarKind::Var(kind),
                                use_iterator_for_array: false,
                            },
                        ) {
                            Ok(..) => {}
                            Err(err) => {
                                self.storage.report(err);
                            }
                        }
                    }
                };
                remove_declaring!();
                return Ok(());
            };

            debug_assert!(self.ctx.allow_ref_declaring);
            if v.name.get_ty().is_none() {
                self.add_vars(
                    &v.name,
                    None,
                    None,
                    None,
                    DeclareVarsOpts {
                        kind: VarKind::Var(kind),
                        use_iterator_for_array: false,
                    },
                )
                .report(&mut self.storage);
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
