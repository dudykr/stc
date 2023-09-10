#![allow(clippy::if_same_then_else)]

use std::{borrow::Cow, mem::take, ops::AddAssign};

use rnode::{Fold, FoldWith, Visit, VisitWith};
use stc_ts_ast_rnode::{RBreakStmt, RIdent, RReturnStmt, RStmt, RStr, RThrowStmt, RTsEntityName, RTsLit, RYieldExpr};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_simple_ast_validations::yield_check::YieldValueUsageFinder;
use stc_ts_types::{
    CommonTypeMetadata, Index, IndexedAccessType, Instance, Key, KeywordType, KeywordTypeMetadata, LitType, MethodSignature,
    PropertySignature, Ref, RefMetadata, TypeElement, TypeParamInstantiation,
};
use stc_utils::{
    cache::Freeze,
    dev_span,
    ext::{SpanExt, TypeVecExt},
};
use swc_common::{Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;
use tracing::debug;

use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::{GetIteratorOpts, TypeOfMode},
        scope::ExpandOpts,
        util::ResultExt,
        Analyzer, Ctx,
    },
    ty::{Array, Type, TypeExt},
    validator,
    validator::ValidateWith,
    VResult,
};

#[derive(Debug, Default)]
pub(in crate::analyzer) struct ReturnValues {
    /// If all cases are handled, `return literal` like `return 5` and never
    /// type is used, we should not generalize the return value.
    should_generalize: bool,
    pub return_types: Vec<Type>,
    yield_types: Vec<Type>,
    /// Are we in if or switch statement?
    pub(super) in_conditional: bool,
}

impl AddAssign for ReturnValues {
    #[allow(clippy::suspicious_op_assign_impl)]
    fn add_assign(&mut self, rhs: Self) {
        self.should_generalize |= rhs.should_generalize;

        self.return_types.extend(rhs.return_types);
        self.yield_types.extend(rhs.yield_types);
    }
}

impl Analyzer<'_, '_> {
    /// This method returns `Generator` if `yield` is found.
    pub(in crate::analyzer) fn visit_stmts_for_return(
        &mut self,
        span: Span,
        is_async: bool,
        is_generator: bool,
        stmts: &Vec<RStmt>,
    ) -> VResult<Option<Type>> {
        let _tracing = dev_span!("visit_stmts_for_return");

        let marks = self.marks();

        debug_assert_eq!(span.ctxt, SyntaxContext::empty());
        debug!("visit_stmts_for_return()");
        debug_assert!(!self.config.is_builtin, "builtin: visit_stmts_for_return should not be called");

        let mut unconditional_throw = None;
        for stmt in stmts {
            if let RStmt::Throw(throws) = stmt {
                unconditional_throw = Some(throws.span);
                break;
            }
        }

        let cannot_fallback_to_iterable_iterator = self.rule().strict_null_checks && {
            let mut v = YieldValueUsageFinder::default();

            stmts.visit_with(&mut v);

            v.found
        };

        // let mut old_ret_tys = self.scope.return_types.take();

        let mut is_unreachable = false;
        let mut ret_ty = (|| -> VResult<_> {
            let mut values: ReturnValues = {
                let ctx = Ctx {
                    cannot_fallback_to_iterable_iterator,
                    ..self.ctx
                };
                self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
                    analyzer.validate_stmts_and_collect(&stmts.iter().collect::<Vec<_>>());
                    is_unreachable = analyzer.ctx.in_unreachable;
                    take(&mut analyzer.scope.get_mut().return_values)
                })
            };

            {
                //  Expand return types if no element references a type parameter
                let can_expand = !values.return_types.iter().any(should_preserve_ref);

                if can_expand {
                    values.return_types = values
                        .return_types
                        .into_iter()
                        .map(|ty| {
                            debug_assert_ne!(ty.span(), DUMMY_SP);

                            self.expand(
                                ty.span(),
                                ty,
                                ExpandOpts {
                                    full: true,
                                    expand_union: true,
                                    preserve_ref: true,
                                    ..Default::default()
                                },
                            )
                        })
                        .collect::<Result<_, _>>()
                        .report(&mut self.storage)
                        .unwrap_or_default();

                    values.yield_types = values
                        .yield_types
                        .into_iter()
                        .map(|ty| {
                            self.expand(
                                ty.span(),
                                ty,
                                ExpandOpts {
                                    full: true,
                                    expand_union: true,
                                    ..Default::default()
                                },
                            )
                        })
                        .collect::<Result<_, _>>()
                        .report(&mut self.storage)
                        .unwrap_or_default();
                }
            }

            {
                if let Some(span) = unconditional_throw {
                    values.return_types.push(Type::never(span, Default::default()));
                }
            }

            debug!("visit_stmts_for_return: types.len() = {}", values.return_types.len());

            let mut actual = Vec::with_capacity(values.return_types.len());
            for mut ty in values.return_types {
                ty = ty.fold_with(&mut KeyInliner { analyzer: self });
                if values.should_generalize {
                    ty = ty.generalize_lit();
                }

                actual.push(ty);
            }

            if is_generator {
                let mut types = Vec::with_capacity(values.yield_types.len());

                let is_all_null_or_undefined = values.yield_types.iter().all(|ty| ty.is_null_or_undefined());

                for ty in values.yield_types {
                    let ty = self.simplify(ty);
                    types.push(ty);
                }

                if is_all_null_or_undefined {
                    types.clear();
                }

                if types.is_empty() {
                    if let Some(declared) = self.scope.borrow().declared_return_type().cloned() {
                        // TODO(kdy1): Change this to `get_iterable_element_type`
                        if let Ok(el_ty) = self.get_iterator_element_type(span, Cow::Owned(declared), true, Default::default()) {
                            types.push(el_ty.into_owned());
                        }
                    }
                }

                let yield_ty = if types.is_empty() {
                    Type::any(
                        DUMMY_SP,
                        KeywordTypeMetadata {
                            common: CommonTypeMetadata {
                                implicit: true,
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                    )
                } else {
                    Type::new_union(span, types)
                };

                let ret_ty = if actual.is_empty() {
                    Type::void(span, Default::default())
                } else {
                    self.simplify(Type::new_union(span, actual))
                };

                let mut metadata = yield_ty.metadata();

                return Ok(Some(Type::Ref(Ref {
                    span: yield_ty.span().or_else(|| {
                        metadata = ret_ty.metadata();
                        ret_ty.span()
                    }),
                    type_name: if is_async {
                        RTsEntityName::Ident(RIdent::new("AsyncGenerator".into(), DUMMY_SP))
                    } else {
                        if cannot_fallback_to_iterable_iterator || self.env.get_global_type(span, &"Generator".into()).is_ok() {
                            RTsEntityName::Ident(RIdent::new("Generator".into(), DUMMY_SP))
                        } else {
                            RTsEntityName::Ident(RIdent::new("IterableIterator".into(), DUMMY_SP))
                        }
                    },
                    type_args: Some(Box::new(TypeParamInstantiation {
                        span,
                        params: vec![
                            yield_ty,
                            ret_ty,
                            Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                        ],
                    })),
                    metadata: RefMetadata {
                        common: metadata,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })));
            }

            if is_async {
                let ret_ty = if actual.is_empty() {
                    Type::void(span, Default::default())
                } else {
                    self.simplify(Type::new_union(span, actual))
                };

                return Ok(Some(Type::Ref(Ref {
                    span,
                    type_name: RTsEntityName::Ident(RIdent::new("Promise".into(), DUMMY_SP)),
                    type_args: Some(Box::new(TypeParamInstantiation {
                        span,
                        params: vec![ret_ty],
                    })),
                    metadata: Default::default(),
                    tracker: Default::default(),
                })));
            }

            let is_all_null_or_undefined = actual.iter().all(|ty| ty.is_null_or_undefined());

            if !actual.is_empty() && is_all_null_or_undefined {
                return Ok(Some(Type::any(span, Default::default())));
            }

            if actual.is_empty() {
                return Ok(None);
            }

            actual.dedup_type();

            if actual.len() == 1 {
                return Ok(actual.pop());
            }

            let ty = Type::new_union(span, actual);
            let ty = self.simplify(ty);

            // print_type("Return",  &ty);

            Ok(Some(ty))
        })()?;
        ret_ty.freeze();

        if self.config.is_builtin {
            return Ok(ret_ty);
        }

        if let Some(declared) = self.scope.borrow().declared_return_type().cloned() {
            if !is_async && !is_generator {
                if ret_ty.is_none() && !is_unreachable {
                    if let Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNeverKeyword,
                        span,
                        ..
                    }) = declared
                    {
                        self.storage.report(ErrorKind::CannotFunctionReturningNever { span }.into());
                    }
                }
                // Noop
            } else if is_generator && declared.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
                // We use different error code
            } else if let Some(ret_ty) = &ret_ty {
                let declared = Type::Instance(Instance {
                    span: declared.span(),
                    ty: Box::new(declared),
                    metadata: Default::default(),
                    tracker: Default::default(),
                });

                let ret_ty = Type::Instance(Instance {
                    span: ret_ty.span(),
                    ty: Box::new(ret_ty.clone()),
                    metadata: Default::default(),
                    tracker: Default::default(),
                });

                self.assign_with_opts(
                    &mut Default::default(),
                    &declared,
                    &ret_ty,
                    AssignOpts {
                        span,
                        allow_unknown_rhs: Some(true),
                        may_unwrap_promise: is_async,
                        ..Default::default()
                    },
                )
                .context("tried to assign return type")
                .report(&mut self.storage);
            }
        }

        Ok(ret_ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RReturnStmt) {
        debug_assert!(!self.config.is_builtin, "builtin: return statement is not supported");
        debug_assert_ne!(node.span, DUMMY_SP, "return statement should have valid span");

        let mut ty = if let Some(res) = {
            let ctx = Ctx {
                in_return_arg: true,
                ..self.ctx
            };
            let mut a = self.with_ctx(ctx);

            let type_ann = a.scope.borrow().declared_return_type().cloned();
            node.arg.validate_with_args(&mut *a, (TypeOfMode::RValue, None, type_ann.as_ref()))
        } {
            res?
        } else {
            Type::Keyword(KeywordType {
                span: node.span,
                kind: TsKeywordTypeKind::TsVoidKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            })
        };
        debug_assert_ne!(ty.span(), DUMMY_SP, "{:?}", ty);
        ty.freeze();

        if let Some(declared) = self.scope.borrow().declared_return_type().cloned() {
            let declared = Type::Instance(Instance {
                span: declared.span(),
                ty: Box::new(declared),
                metadata: Default::default(),
                tracker: Default::default(),
            });

            match (self.ctx.in_async, self.ctx.in_generator) {
                // AsyncGenerator
                (true, true) => {
                    self.assign_with_opts(
                        &mut Default::default(),
                        &declared,
                        &Type::Ref(Ref {
                            span: node.span,
                            type_name: RTsEntityName::Ident(RIdent::new("AsyncGenerator".into(), node.span)),
                            type_args: Some(Box::new(TypeParamInstantiation {
                                span: node.span,
                                params: vec![Type::any(DUMMY_SP, Default::default()), ty.clone()],
                            })),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }),
                        AssignOpts {
                            span: node.span,
                            allow_unknown_rhs: Some(true),
                            allow_assignment_of_void: Some(!self.rule().strict_null_checks),
                            may_unwrap_promise: true,
                            ..Default::default()
                        },
                    )
                    .context("tried to validate the return type of an async generator")
                    .report(&mut self.storage);
                }

                // Promise
                (true, false) => {
                    self.assign_with_opts(
                        &mut Default::default(),
                        &declared,
                        &ty,
                        AssignOpts {
                            span: node.span,
                            allow_unknown_rhs: Some(true),
                            allow_assignment_of_void: Some(!self.rule().strict_null_checks),
                            may_unwrap_promise: true,
                            ..Default::default()
                        },
                    )
                    .context("tried to validate the return type of an async function")
                    .report(&mut self.storage);
                }

                // Generator
                (false, true) => {
                    let name = if self.ctx.cannot_fallback_to_iterable_iterator
                        || self.env.get_global_type(node.span, &"Generator".into()).is_ok()
                    {
                        "Generator"
                    } else {
                        "IterableIterator"
                    };

                    self.assign_with_opts(
                        &mut Default::default(),
                        &declared,
                        &Type::Ref(Ref {
                            span: node.span,
                            type_name: RTsEntityName::Ident(RIdent::new(name.into(), node.span)),
                            type_args: Some(Box::new(TypeParamInstantiation {
                                span: node.span,
                                params: vec![Type::any(DUMMY_SP, Default::default()), ty.clone()],
                            })),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }),
                        AssignOpts {
                            span: node.span,
                            allow_unknown_rhs: Some(true),
                            allow_assignment_of_void: Some(!self.rule().strict_null_checks),
                            ..Default::default()
                        },
                    )
                    .context("tried to validate the return type of a generator")
                    .report(&mut self.storage);
                }

                (false, false) => {
                    self.assign_with_opts(
                        &mut Default::default(),
                        &declared,
                        &ty,
                        AssignOpts {
                            span: node.span,
                            allow_unknown_rhs: Some(true),
                            allow_assignment_of_void: Some(!self.rule().strict_null_checks),

                            ..Default::default()
                        },
                    )
                    .context("tried to validate the return type of a function")
                    .report(&mut self.storage);
                }
            }
        }

        self.scope.borrow_mut().return_values.return_types.push(ty);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RYieldExpr) -> VResult<Type> {
        let span = e.span;

        if let Some(res) = e.arg.validate_with_default(self) {
            let ty = res?;

            let item_ty = if e.delegate {
                if self.ctx.in_async {
                    self.get_async_iterator_element_type(e.span, Cow::Owned(ty), false)
                        .context("tried to convert argument as an async iterator for delegating yield")?
                        .into_owned()
                } else {
                    self.get_iterator_element_type(e.span, Cow::Owned(ty), false, GetIteratorOpts { ..Default::default() })
                        .context("tried to convert argument as an iterator for delegating yield")?
                        .into_owned()
                }
            } else {
                ty
            }
            .freezed();

            if let Some(declared) = self.scope.borrow().declared_return_type().cloned() {
                match if self.ctx.in_async {
                    self.get_async_iterator_element_type(e.span, Cow::Owned(declared), true)
                        .context("tried to get an element type from an async iterator for normal yield")
                } else {
                    self.get_iterator_element_type(e.span, Cow::Owned(declared), true, GetIteratorOpts { ..Default::default() })
                        .context("tried to get an element type from an iterator for normal yield")
                }
                .map(Cow::into_owned)
                .map(Freeze::freezed)
                {
                    Ok(declared) => {
                        let declared = Type::Instance(Instance {
                            span: declared.span(),
                            ty: Box::new(declared),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        });

                        match self.assign_with_opts(
                            &mut Default::default(),
                            &declared,
                            &item_ty,
                            AssignOpts {
                                span: e.span,
                                allow_unknown_rhs: Some(true),
                                use_missing_fields_for_class: true,
                                may_unwrap_promise: true,
                                ..Default::default()
                            },
                        ) {
                            Ok(()) => {}
                            Err(err) => {
                                self.storage.report(err);
                                return Ok(Type::any(span, Default::default()));
                            }
                        }
                    }
                    Err(err) => {
                        self.storage.report(
                            ErrorKind::SimpleAssignFailed {
                                span,
                                cause: Some(Box::new(err)),
                            }
                            .into(),
                        );
                        return Ok(Type::any(span, Default::default()));
                    }
                }
            }

            self.scope.borrow_mut().return_values.yield_types.push(item_ty);
        } else {
            self.scope.borrow_mut().return_values.yield_types.push(Type::Keyword(KeywordType {
                span: e.span,
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            }));
        }

        Ok(Type::any(e.span, Default::default()))
    }
}

pub(super) struct LoopBreakerFinder {
    pub found: bool,
}

impl Visit<RBreakStmt> for LoopBreakerFinder {
    fn visit(&mut self, _: &RBreakStmt) {
        self.found = true;
    }
}

impl Visit<RThrowStmt> for LoopBreakerFinder {
    fn visit(&mut self, _: &RThrowStmt) {
        self.found = true;
    }
}

impl Visit<RReturnStmt> for LoopBreakerFinder {
    fn visit(&mut self, _: &RReturnStmt) {
        self.found = true;
    }
}

fn should_preserve_ref(ty: &Type) -> bool {
    match ty {
        Type::IndexedAccessType(..) => true,
        Type::Array(Array { elem_type, .. }) => should_preserve_ref(elem_type),
        // TODO(kdy1): More work
        _ => false,
    }
}

/// # Example
///
/// ```ts
/// declare type S2 = {
///     a: string;
///     b: string;
/// };
/// T[keyof S2];
///
/// // becomes
///
/// T["a" | "b"]
/// ```
struct KeyInliner<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl Fold<Type> for KeyInliner<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty = ty.fold_children_with(self);

        if let Type::IndexedAccessType(IndexedAccessType {
            span,
            readonly,
            ref obj_type,
            index_type:
                box Type::Index(Index {
                    span: op_span,
                    ty: ref index_type,
                    metadata: op_metadata,
                    ..
                }),
            metadata,
            ..
        }) = ty
        {
            // TODO(kdy1): Handle error.
            let index_ty = self
                .analyzer
                .expand(
                    span,
                    *index_type.clone(),
                    ExpandOpts {
                        full: true,
                        expand_union: true,
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..Default::default()
                    },
                )
                .unwrap_or_else(|_| *index_type.clone());

            if obj_type.type_eq(index_type) {
                // declare type S2 = {
                //     a: string;
                //     b: string;
                // };
                // `S2[keyof S2]`;

                // TODO(kdy1): PERF
                if let Type::TypeLit(obj) = index_ty.foldable() {
                    let mut types: Vec<Type> = vec![];
                    for member in obj.members {
                        match member {
                            TypeElement::Call(_) => {
                                unimplemented!("Call signature in S[keyof S]")
                            }
                            TypeElement::Constructor(_) => {
                                unimplemented!("Constructor signature in S[keyof S]")
                            }
                            TypeElement::Property(p) => {
                                if p.key.is_computed() {
                                    unimplemented!("Computed key mixed with S[keyof S]")
                                }

                                if let Some(ty) = p.type_ann {
                                    if types.iter().all(|previous| !previous.type_eq(&ty)) {
                                        types.push(*ty);
                                    }
                                }
                            }
                            TypeElement::Method(_) => {
                                unimplemented!("Method property in S[keyof S]")
                            }
                            TypeElement::Index(_) => {
                                unimplemented!("Index signature in S[keyof S]")
                            }
                        }
                    }
                    let ty = Type::new_union(span, types);

                    return ty;
                }
            } else {
                // declare type S2 = {
                //     a: string;
                //     b: string;
                // };
                // `T[keyof S2]`;
                // =>
                // `T["a" | "b"]`

                // TODO(kdy1): PERF
                if let Some(obj) = index_ty.type_lit() {
                    let mut types: Vec<Type> = vec![];
                    for member in obj.members {
                        match member {
                            TypeElement::Call(_) => {
                                unimplemented!("Call signature in T[keyof S]")
                            }
                            TypeElement::Constructor(_) => {
                                unimplemented!("Constructor signature in T[keyof S]")
                            }

                            TypeElement::Index(_) => {
                                unimplemented!("Index signature in T[keyof S]")
                            }

                            TypeElement::Property(PropertySignature { key, .. }) | TypeElement::Method(MethodSignature { key, .. }) => {
                                if key.is_computed() {
                                    unimplemented!("Computed key mixed with T[keyof S]");
                                }

                                if let Key::Normal { span: i_span, sym: key } = key {
                                    debug_assert_eq!(i_span.ctxt, SyntaxContext::empty());
                                    let ty = Type::Lit(LitType {
                                        span: i_span,
                                        lit: RTsLit::Str(RStr {
                                            span: i_span,
                                            value: key.clone(),
                                            raw: None,
                                        }),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    });

                                    if types.iter().all(|previous| !previous.type_eq(&ty)) {
                                        types.push(ty);
                                    }
                                }
                            }
                        }
                    }
                    return Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        obj_type: obj_type.clone(),
                        index_type: Box::new(Type::new_union(span, types)),
                        metadata,
                        tracker: Default::default(),
                    });
                }
            }
        }

        ty
    }
}
