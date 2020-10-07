use crate::{
    analyzer::{util::ResultExt, Analyzer, Ctx},
    debug::print_type,
    errors::Error,
    ty::{Array, Type, TypeExt},
    util::TypeEq,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::{ExprExt, Value::Known};
use swc_ecma_visit::{Node, Visit, VisitMut, VisitMutWith, VisitWith};
use swc_ts_types::{
    Fold, FoldWith, IndexedAccessType, MethodSignature, Operator, PropertySignature, Ref,
    TypeElement, TypeParamInstantiation,
};

impl Analyzer<'_, '_> {
    /// This method returns `Generator` if `yield` is found.
    ///
    /// TODO: Handle yield
    /// TODO: prevent visiting children (to improve performance greatly)
    pub(in crate::analyzer) fn visit_stmts_for_return(
        &mut self,
        span: Span,
        is_async: bool,
        is_generator: bool,
        stmts: &mut [Stmt],
    ) -> Result<Option<Box<Type>>, Error> {
        log::debug!("visit_stmts_for_return()");

        // let mut old_ret_tys = self.scope.return_types.take();

        let (mut return_types, mut yield_types) = {
            let order = self.reorder_stmts(&*stmts);
            assert_eq!(order.len(), stmts.len());

            let ctx = Ctx {
                preserve_ref: true,
                ..self.ctx
            };
            self.with_ctx(ctx).with(|analyzer| {
                let mut v = ReturnTypeCollector {
                    analyzer,
                    return_types: Default::default(),
                    yield_types: Default::default(),
                    in_conditional: false,
                    forced_never: false,
                };

                for idx in order {
                    stmts[idx].visit_mut_with(&mut v);
                }

                //  Expand return types if no element references a type parameter

                (v.return_types, v.yield_types)
            })
        };

        {
            let can_expand = return_types.iter().all(|ty| match ty {
                Ok(ty) => {
                    if should_preserve_ref(ty) {
                        return false;
                    }

                    true
                }
                _ => false,
            });

            if can_expand {
                return_types = return_types
                    .into_iter()
                    .map(|res| match res {
                        Ok(ty) => {
                            let ctx = Ctx {
                                preserve_ref: true,
                                ignore_expand_prevention_for_top: false,
                                ignore_expand_prevention_for_all: false,
                                ..self.ctx
                            };
                            let ty = self.with_ctx(ctx).expand_fully(ty.span(), ty, true)?;
                            Ok(ty)
                        }
                        Err(e) => Err(e),
                    })
                    .collect();

                yield_types = yield_types
                    .into_iter()
                    .map(|res| match res {
                        Ok(ty) => self.expand_fully(ty.span(), ty, true),
                        Err(e) => Err(e),
                    })
                    .collect();
            }
        }

        log::debug!(
            "visit_stmts_for_return: types.len() = {}",
            return_types.len()
        );

        let mut actual = Vec::with_capacity(return_types.len());
        for ty in return_types {
            let ty = ty
                .map(|ty| ty.fold_with(&mut KeyInliner { analyzer: self }))
                .map(|ty| {
                    if is_async || is_generator {
                        ty.generalize_lit()
                    } else {
                        print_type("Inferred", &self.cm, &ty);
                        self.generalize_ret_ty(ty)
                    }
                })
                .map(|ty| self.simplify(ty));

            actual.extend(ty.store(&mut self.info.errors));
        }

        if is_generator {
            let mut types = Vec::with_capacity(yield_types.len());
            for ty in yield_types {
                let ty = ty.map(|ty| ty.generalize_lit()).map(|ty| self.simplify(ty));
                types.extend(ty.store(&mut self.info.errors));
            }

            let yield_ty = if types.is_empty() {
                Type::void(span)
            } else {
                Type::union(types)
            };

            let ret_ty = if actual.is_empty() {
                Type::void(span)
            } else {
                Type::union(actual)
            };

            return Ok(Some(box Type::Ref(Ref {
                span,
                type_name: if is_async {
                    TsEntityName::Ident(Ident::new("AsyncGenerator".into(), DUMMY_SP))
                } else {
                    TsEntityName::Ident(Ident::new("Generator".into(), DUMMY_SP))
                },
                type_args: Some(TypeParamInstantiation {
                    span,
                    params: vec![
                        yield_ty,
                        ret_ty,
                        box Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsUnknownKeyword,
                        }),
                    ],
                }),
            })));
        }

        if is_async {
            let ret_ty = if actual.is_empty() {
                Type::void(span)
            } else {
                Type::union(actual)
            };

            return Ok(Some(box Type::Ref(Ref {
                span,
                type_name: TsEntityName::Ident(Ident::new("Promise".into(), DUMMY_SP)),
                type_args: Some(TypeParamInstantiation {
                    span,
                    params: vec![ret_ty],
                }),
            })));
        }

        if actual.is_empty() {
            return Ok(None);
        }
        let ty = Type::union(actual);

        // print_type("Return", &self.cm, &ty);

        Ok(Some(ty))
    }
}

trait MyVisitor: Validate<Expr, Output = ValidationResult> + VisitMut {}
impl MyVisitor for Analyzer<'_, '_> {}

struct ReturnTypeCollector<'a, A>
where
    A: MyVisitor,
{
    pub analyzer: &'a mut A,
    pub return_types: Vec<Result<Box<Type>, Error>>,
    pub yield_types: Vec<Result<Box<Type>, Error>>,
    /// Are we in if or switch statement?
    pub in_conditional: bool,
    pub forced_never: bool,
}

impl<A> ReturnTypeCollector<'_, A>
where
    A: MyVisitor,
{
    fn is_always_true(&mut self, e: &mut Expr) -> bool {
        if let (_, Known(v)) = e.as_bool() {
            return v;
        }

        match self.analyzer.validate(e) {
            Ok(ty) => {
                if let Known(v) = ty.as_bool() {
                    return v;
                }
            }
            Err(err) => {
                self.return_types.push(Err(err));
                return false;
            }
        }

        false
    }
}

macro_rules! simple {
    ($name:ident, $T:ty) => {
        fn $name(&mut self, node: &mut $T) {
            // TODO: Prevent recursion of analyzer
            node.visit_mut_with(self.analyzer);
            node.visit_mut_children_with(self);
        }
    };
}

impl<A> VisitMut for ReturnTypeCollector<'_, A>
where
    A: MyVisitor,
{
    simple!(visit_mut_block_stmt, BlockStmt);
    simple!(visit_mut_labeled_stmt, LabeledStmt);
    simple!(visit_mut_break_stmt, BreakStmt);
    simple!(visit_mut_continue_stmt, ContinueStmt);
    simple!(visit_mut_if_stmt, IfStmt);
    simple!(visit_mut_switch_stmt, SwitchStmt);
    simple!(visit_mut_throw_stmt, ThrowStmt);
    simple!(visit_mut_try_stmt, TryStmt);
    simple!(visit_mut_while_stmt, WhileStmt);
    simple!(visit_mut_do_while_stmt, DoWhileStmt);
    simple!(visit_mut_for_stmt, ForStmt);
    simple!(visit_mut_for_in_stmt, ForInStmt);
    simple!(visit_mut_for_of_stmt, ForOfStmt);
    simple!(visit_mut_decl, Decl);
    simple!(visit_mut_expr_stmt, ExprStmt);

    fn visit_mut_expr(&mut self, e: &mut Expr) {
        match e {
            Expr::Yield(..) => {
                e.visit_mut_children_with(self);
                return;
            }
            _ => {}
        }

        let ty: Result<_, Error> = try {
            let ty = e.validate_with(self.analyzer)?;
            match ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                    ..
                }) => {
                    log::info!("found never type");
                    self.return_types.push(Ok(ty))
                }
                _ => {}
            }

            ()
        };

        match ty {
            Ok(()) => {}
            Err(err) => self.return_types.push(Err(err)),
        }
    }

    fn visit_mut_yield_expr(&mut self, e: &mut YieldExpr) {
        if let Some(res) = e.arg.validate_with(self.analyzer) {
            self.yield_types.push(res);
        } else {
            self.return_types.push(Ok(box Type::Keyword(TsKeywordType {
                span: e.span,
                kind: TsKeywordTypeKind::TsVoidKeyword,
            })));
        }
    }

    fn visit_mut_return_stmt(&mut self, s: &mut ReturnStmt) {
        if let Some(res) = s.arg.validate_with(self.analyzer) {
            self.return_types.push(res)
        } else {
            self.return_types.push(Ok(box Type::Keyword(TsKeywordType {
                span: s.span,
                kind: TsKeywordTypeKind::TsVoidKeyword,
            })));
        }
    }

    fn visit_mut_stmt(&mut self, s: &mut Stmt) {
        let old_in_conditional = self.in_conditional;
        self.in_conditional |= match s {
            Stmt::If(_) => true,
            Stmt::Switch(_) => true,
            _ => false,
        };

        s.visit_mut_children_with(self);

        // Of `s` is always executed and we enter infinite loop, return type should be
        // never
        if !self.in_conditional {
            log::trace!("Checking for infinite loop");
            let mut v = LoopBreakerFinder { found: false };
            s.visit_with(&Invalid { span: DUMMY_SP }, &mut v);
            let has_break = v.found;
            if !has_break {
                match s {
                    Stmt::While(s) => {
                        if self.is_always_true(&mut s.test) {
                            self.forced_never = true;
                        }
                    }
                    Stmt::DoWhile(s) => {
                        if self.is_always_true(&mut s.test) {
                            self.forced_never = true;
                        }
                    }
                    Stmt::For(s) => {
                        if let Some(test) = &mut s.test {
                            if self.is_always_true(test) {
                                self.forced_never = true;
                            }
                        } else {
                            self.forced_never = true;
                        }
                    }
                    _ => {}
                }
            }
        }

        self.in_conditional = old_in_conditional;
    }

    /// no-op
    fn visit_mut_arrow_expr(&mut self, _: &mut ArrowExpr) {}

    /// no-op
    fn visit_mut_function(&mut self, _: &mut Function) {}
}

struct LoopBreakerFinder {
    found: bool,
}

impl Visit for LoopBreakerFinder {
    fn visit_break_stmt(&mut self, _: &BreakStmt, _: &dyn Node) {
        self.found = true;
    }

    fn visit_throw_stmt(&mut self, _: &ThrowStmt, _: &dyn Node) {
        self.found = true;
    }

    fn visit_return_stmt(&mut self, _: &ReturnStmt, _: &dyn Node) {
        self.found = true;
    }
}

fn should_preserve_ref(ty: &Type) -> bool {
    match ty {
        Type::IndexedAccessType(..) => true,
        Type::Array(Array { elem_type, .. }) => should_preserve_ref(&elem_type),
        // TODO: More work
        _ => false,
    }
}

struct KeyInliner<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl Fold for KeyInliner<'_, '_, '_> {
    fn fold_type(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                span,
                readonly,
                ref obj_type,
                index_type:
                    box Type::Operator(Operator {
                        span: op_span,
                        op: TsTypeOperatorOp::KeyOf,
                        ty: ref index_type,
                    }),
            }) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.analyzer.ctx
                };

                // TODO: Handle error.
                let index_ty = self
                    .analyzer
                    .with_ctx(ctx)
                    .expand_fully(span, index_type.clone(), true)
                    .unwrap_or_else(|_| index_type.clone());

                if obj_type.type_eq(&index_type) {
                    // declare type S2 = {
                    //     a: string;
                    //     b: string;
                    // };
                    // `S2[keyof S2]`;

                    match *index_ty {
                        Type::TypeLit(obj) => {
                            let mut types: Vec<Box<Type>> = vec![];
                            for member in obj.members {
                                match member {
                                    TypeElement::Call(_) => {
                                        unimplemented!("Call signature in S[keyof S]")
                                    }
                                    TypeElement::Constructor(_) => {
                                        unimplemented!("Constructor signature in S[keyof S]")
                                    }
                                    TypeElement::Property(p) => {
                                        if p.computed {
                                            unimplemented!("Computed key mixed with S[keyof S]")
                                        }

                                        if let Some(ty) = p.type_ann {
                                            if types.iter().all(|previous| !previous.type_eq(&ty)) {
                                                types.push(ty);
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
                            let ty = *Type::union(types);

                            return ty;
                        }
                        _ => {}
                    }
                } else {
                    // declare type S2 = {
                    //     a: string;
                    //     b: string;
                    // };
                    // `T[keyof S2]`;
                    // =>
                    // `T["a" | "b"]`

                    match *index_ty {
                        Type::TypeLit(obj) => {
                            let mut types: Vec<Box<Type>> = vec![];
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

                                    TypeElement::Property(PropertySignature {
                                        key,
                                        computed,
                                        ..
                                    })
                                    | TypeElement::Method(MethodSignature {
                                        key, computed, ..
                                    }) => {
                                        if computed {
                                            unimplemented!("Computed key mixed with T[keyof S]");
                                        }

                                        match &*key {
                                            Expr::Ident(i) => {
                                                let ty = box Type::Lit(TsLitType {
                                                    span: i.span,
                                                    lit: TsLit::Str(Str {
                                                        span: i.span,
                                                        value: i.sym.clone(),
                                                        has_escape: false,
                                                    }),
                                                });

                                                if types
                                                    .iter()
                                                    .all(|previous| !previous.type_eq(&ty))
                                                {
                                                    types.push(ty);
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                            return Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly,
                                obj_type: obj_type.clone(),
                                index_type: Type::union(types),
                            });
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        ty
    }
}
