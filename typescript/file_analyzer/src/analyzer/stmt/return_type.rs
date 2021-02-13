use crate::analyzer::util::ResultExt;
use crate::util::type_ext::TypeVecExt;
use crate::{
    analyzer::{Analyzer, Ctx},
    ty::{Array, Type, TypeExt},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::Fold;
use rnode::FoldWith;
use rnode::NodeId;
use rnode::Visit;
use stc_ts_ast_rnode::RBreakStmt;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RReturnStmt;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RThrowStmt;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RYieldExpr;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::Key;
use stc_ts_types::ModuleId;
use stc_ts_types::{
    IndexedAccessType, MethodSignature, Operator, PropertySignature, Ref, TypeElement, TypeParamInstantiation,
};
use std::borrow::Cow;
use std::{mem::take, ops::AddAssign};
use swc_common::TypeEq;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

#[derive(Debug, Default)]
pub(in crate::analyzer) struct ReturnValues {
    /// If all cases are handled, `return literal` like `return 5` and never
    /// type is used, we should not generalize the return value.
    should_generalize: bool,
    pub return_types: Vec<Box<Type>>,
    yield_types: Vec<Box<Type>>,
    /// Are we in if or switch statement?
    pub(super) in_conditional: bool,
    pub(super) forced_never: bool,
}

impl AddAssign for ReturnValues {
    fn add_assign(&mut self, rhs: Self) {
        self.should_generalize |= rhs.should_generalize;

        self.return_types.extend(rhs.return_types);
        self.yield_types.extend(rhs.yield_types);

        self.forced_never |= rhs.forced_never;
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
    ) -> Result<Option<Box<Type>>, Error> {
        slog::debug!(self.logger, "visit_stmts_for_return()");
        debug_assert!(!self.is_builtin, "builtin: visit_stmts_for_return should not be called");

        // let mut old_ret_tys = self.scope.return_types.take();

        let mut values: ReturnValues = {
            let ctx = Ctx {
                preserve_ref: true,
                ..self.ctx
            };
            self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
                analyzer.validate_stmts_and_collect(&stmts.iter().collect::<Vec<_>>());

                take(&mut analyzer.scope.return_values)
            })
        };

        {
            //  Expand return types if no element references a type parameter
            let can_expand = values.return_types.iter().all(|ty| {
                if should_preserve_ref(ty) {
                    return false;
                }

                true
            });

            if can_expand {
                values.return_types = values
                    .return_types
                    .into_iter()
                    .map(|ty| {
                        debug_assert_ne!(ty.span(), DUMMY_SP);
                        let ctx = Ctx {
                            preserve_ref: true,
                            ignore_expand_prevention_for_top: false,
                            ignore_expand_prevention_for_all: false,
                            ..self.ctx
                        };
                        self.with_ctx(ctx).expand_fully(ty.span(), ty, true)
                    })
                    .collect::<Result<_, _>>()
                    .report(&mut self.storage)
                    .unwrap_or_default();

                values.yield_types = values
                    .yield_types
                    .into_iter()
                    .map(|ty| self.expand_fully(ty.span(), ty, true))
                    .collect::<Result<_, _>>()
                    .report(&mut self.storage)
                    .unwrap_or_default();
            }
        }

        slog::debug!(
            self.logger,
            "visit_stmts_for_return: types.len() = {}",
            values.return_types.len()
        );

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
            for ty in values.yield_types {
                let ty = self.simplify(ty);
                types.push(ty);
            }

            let yield_ty = if types.is_empty() {
                Type::void(span)
            } else {
                Type::union(types)
            };

            let ret_ty = if actual.is_empty() {
                Type::void(span)
            } else {
                self.simplify(Type::union(actual))
            };

            return Ok(Some(box Type::Ref(Ref {
                span,
                ctxt: ModuleId::builtin(),
                type_name: if is_async {
                    RTsEntityName::Ident(RIdent::new("AsyncGenerator".into(), DUMMY_SP))
                } else {
                    RTsEntityName::Ident(RIdent::new("Generator".into(), DUMMY_SP))
                },
                type_args: Some(box TypeParamInstantiation {
                    span,
                    params: vec![
                        yield_ty,
                        ret_ty,
                        box Type::Keyword(RTsKeywordType {
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
                self.simplify(Type::union(actual))
            };

            return Ok(Some(box Type::Ref(Ref {
                span,
                ctxt: ModuleId::builtin(),
                type_name: RTsEntityName::Ident(RIdent::new("Promise".into(), DUMMY_SP)),
                type_args: Some(box TypeParamInstantiation {
                    span,
                    params: vec![ret_ty],
                }),
            })));
        }

        if actual.is_empty() {
            return Ok(None);
        }

        actual.dedup_type();
        let ty = Type::union(actual);
        let ty = self.simplify(ty);

        // print_type("Return", &self.cm, &ty);

        Ok(Some(ty))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RReturnStmt) {
        debug_assert!(!self.is_builtin, "builtin: return statement is not supported");
        debug_assert_ne!(node.span, DUMMY_SP, "return statement should have valid span");

        let ty = if let Some(res) = {
            let ctx = Ctx {
                in_return_arg: true,
                ..self.ctx
            };
            let mut a = self.with_ctx(ctx);
            node.arg.validate_with_default(&mut *a)
        } {
            res?
        } else {
            box Type::Keyword(RTsKeywordType {
                span: node.span,
                kind: TsKeywordTypeKind::TsVoidKeyword,
            })
        };
        debug_assert_ne!(ty.span(), DUMMY_SP, "{:?}", ty);

        self.scope.return_values.return_types.push(ty);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RYieldExpr) -> ValidationResult {
        if let Some(res) = e.arg.validate_with_default(self) {
            let ty = res?;

            if e.delegate {
                // TODO: Use correct symbol. (need proper symbol handling)
                let item_ty = box self
                    .convert_to_iterator(e.span, Cow::Owned(*ty))
                    .context("tried to convert argument as an interator for delegating yield")?
                    .into_owned();

                self.scope.return_values.yield_types.push(item_ty);
            } else {
                self.scope.return_values.yield_types.push(ty);
            }
        } else {
            self.scope
                .return_values
                .yield_types
                .push(box Type::Keyword(RTsKeywordType {
                    span: e.span,
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                }));
        }

        Ok(Type::any(e.span))
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
        Type::Array(Array { elem_type, .. }) => should_preserve_ref(&elem_type),
        // TODO: More work
        _ => false,
    }
}

struct KeyInliner<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl Fold<Type> for KeyInliner<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
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
                                        if p.key.is_computed() {
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

                                    TypeElement::Property(PropertySignature { key, .. })
                                    | TypeElement::Method(MethodSignature { key, .. }) => {
                                        if key.is_computed() {
                                            unimplemented!("Computed key mixed with T[keyof S]");
                                        }

                                        match key {
                                            Key::Normal { span: i_span, sym: key } => {
                                                let ty = box Type::Lit(RTsLitType {
                                                    node_id: NodeId::invalid(),
                                                    span: i_span,
                                                    lit: RTsLit::Str(RStr {
                                                        span: i_span,
                                                        value: key.clone(),
                                                        has_escape: false,
                                                        kind: Default::default(),
                                                    }),
                                                });

                                                if types.iter().all(|previous| !previous.type_eq(&ty)) {
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
