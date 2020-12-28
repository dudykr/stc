use super::super::{pat::PatMode, Analyzer, Ctx};
use crate::errors::Errors;
use crate::{
    analyzer::{
        expr::TypeOfMode,
        util::{Generalizer, ResultExt},
    },
    debug::print_type,
    errors::Error,
    ty::{self, Tuple, Type, TypeParam},
    util::{PatExt, RemoveTypes},
    validator,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use rnode::FoldWith;
use rnode::Visit;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ast_rnode::RArrayPat;
use stc_ast_rnode::RCallExpr;
use stc_ast_rnode::RExpr;
use stc_ast_rnode::RExprOrSuper;
use stc_ast_rnode::RIdent;
use stc_ast_rnode::RPat;
use stc_ast_rnode::RTsArrayType;
use stc_ast_rnode::RTsAsExpr;
use stc_ast_rnode::RTsEntityName;
use stc_ast_rnode::RTsKeywordType;
use stc_ast_rnode::RTsType;
use stc_ast_rnode::RTsTypeAnn;
use stc_ast_rnode::RTsTypeCastExpr;
use stc_ast_rnode::RTsTypeOperator;
use stc_ast_rnode::RTsTypeQuery;
use stc_ast_rnode::RTsTypeQueryExpr;
use stc_ast_rnode::RVarDecl;
use stc_ast_rnode::RVarDeclarator;
use stc_types::{Array, Id, Operator, Symbol};
use std::mem::take;
use swc_atoms::js_word;
use swc_common::{Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use ty::TypeExt;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, var: &mut RVarDecl) {
        self.record(&*var);

        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            var_kind: var.kind,
            in_declare: self.ctx.in_declare || var.declare,
            allow_ref_declaring: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a| {
            var.decls.visit_mut_with(a);
        });

        // Flatten var declarations
        for mut decl in take(&mut var.decls) {
            match decl.name {
                RPat::Array(RArrayPat {
                    span,
                    mut elems,
                    type_ann:
                        Some(RTsTypeAnn {
                            type_ann: box RTsType::TsTupleType(tuple),
                            ..
                        }),
                    ..
                }) => {
                    //
                    for (i, elem) in elems.into_iter().enumerate() {
                        match elem {
                            Some(mut pat) => {
                                //
                                if i < tuple.elem_types.len() {
                                    let ty = box tuple.elem_types[i].ty.clone();
                                    pat.set_ty(Some(ty));
                                }

                                var.decls.push(RVarDeclarator {
                                    span,
                                    name: pat,
                                    init: None,
                                    definite: false,
                                })
                            }
                            None => {}
                        }
                    }
                }
                // TODO
                //  RPat::Object(obj) => {}
                _ => var.decls.push(decl),
            }
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, v: &mut RVarDeclarator) {
        self.record(v);

        let kind = self.ctx.var_kind;

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

            macro_rules! inject_any {
                () => {
                    // Declare variable with type any
                    match self.declare_complex_vars(kind, &v.name, Type::any(v_span)) {
                        Ok(()) => {}
                        Err(err) => {
                            self.storage.report(err);
                        }
                    }
                };
            }

            let forced_type_ann = {
                // let a = {} as Foo
                match v.init {
                    Some(box RExpr::TsAs(RTsAsExpr { ref type_ann, .. }))
                    | Some(box RExpr::TsTypeCast(RTsTypeCastExpr {
                        type_ann: RTsTypeAnn { ref type_ann, .. },
                        ..
                    })) => Some(type_ann.clone()),
                    _ => None,
                }
            };

            // If user specified type, value should be removed.
            let should_remove_value = v.name.get_ty().is_some();

            macro_rules! remove_declaring {
                () => {{
                    if should_remove_value {
                        v.init = None;
                    }

                    debug_assert_eq!(Some(self.scope.declaring.clone()), debug_declaring);
                }};
            }

            if let Some(ref mut init) = v.init {
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

                let old_this = if creates_new_this {
                    self.scope.this.take()
                } else {
                    None
                };

                let declared_ty = v.name.get_mut_ty();
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
                        self.check_rvalue(&ty);

                        self.scope.this = Some(box ty.clone().remove_falsy());
                        let mut value_ty = get_value_ty!(Some(&ty));
                        value_ty = self.expand(span, value_ty)?;
                        value_ty = self.rename_type_params(span, value_ty, Some(&ty))?;

                        let ty_for_assignment = self.expand_fully(span, ty.clone(), true)?;
                        let value_ty_for_assignment =
                            self.expand_fully(span, value_ty.clone(), true)?;
                        match self.assign(&ty_for_assignment, &value_ty_for_assignment, v_span) {
                            Ok(()) => {
                                let mut ty = ty;
                                self.prevent_generalize(&mut ty);

                                // let ty = ty.fold_with(&mut Generalizer::default());
                                match self.declare_complex_vars(kind, &v.name, ty) {
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

                                match self.declare_complex_vars(kind, &v.name, ty) {
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
                                Type::EnumVariant(ref v) => {
                                    if let Some(items) =
                                        self.find_type(self.ctx.module_id, &v.enum_name)?
                                    {
                                        for ty in items {
                                            if let Type::Enum(ref e) = ty.normalize() {
                                                return Ok(box Type::Enum(e.clone()));
                                            }
                                        }
                                    }
                                }
                                Type::TypeLit(..) | Type::Function(..) | Type::Query(..) => {
                                    v.init = None;
                                }
                                _ => {}
                            }

                            Ok(value_ty)
                        })()?;

                        let should_generalize_fully =
                            self.may_generalize(&ty) && !contains_type_param(&ty);

                        slog::debug!(self.logger, "var: user did not declare type");
                        let mut ty = self.rename_type_params(span, ty, None)?;
                        if self.may_generalize(&ty) {
                            ty = ty.fold_with(&mut Generalizer::default());
                        }

                        if should_generalize_fully {
                            ty = match ty.normalize() {
                                Type::Function(f) => {
                                    let ret_ty = f.ret_ty.clone().generalize_lit();
                                    box Type::Function(stc_types::Function {
                                        ret_ty,
                                        ..f.clone()
                                    })
                                }
                                _ => ty,
                            };
                        }

                        match ty.normalize() {
                            Type::Ref(..) => {
                                let ctx = Ctx {
                                    preserve_ref: true,
                                    ignore_expand_prevention_for_all: false,
                                    ignore_expand_prevention_for_top: false,
                                    ..self.ctx
                                };
                                ty = self.with_ctx(ctx).expand(span, ty)?;
                            }
                            _ => {}
                        }

                        if self.scope.is_root() {
                            if let Some(box RExpr::Ident(ref alias)) = &v.init {
                                if let RPat::Ident(ref mut i) = v.name {
                                    i.type_ann = Some(RTsTypeAnn {
                                        span: DUMMY_SP,
                                        type_ann: box RTsType::TsTypeQuery(RTsTypeQuery {
                                            span,
                                            expr_name: RTsTypeQueryExpr::TsEntityName(
                                                RTsEntityName::Ident(alias.clone()),
                                            ),
                                        }),
                                    });
                                }
                            }
                            if !should_remove_value {
                                v.name.set_ty(Some(forced_type_ann.unwrap_or_else(|| {
                                    let ty = ty.clone();

                                    // Normalize unresolved parameters
                                    let ty = match ty.normalize() {
                                        Type::Param(TypeParam {
                                            constraint: Some(ty),
                                            ..
                                        }) => ty.clone(),
                                        _ => ty,
                                    };

                                    let ty = match ty.normalize() {
                                        Type::ClassInstance(c) => box c.ty.clone().into(),

                                        // `err is Error` => boolean
                                        Type::Predicate(..) => {
                                            box RTsType::TsKeywordType(RTsKeywordType {
                                                span,
                                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                            })
                                        }

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
                                            VarDeclKind::Const if is_symbol_call => {
                                                box RTsType::TsTypeOperator(RTsTypeOperator {
                                                    span:*span,
                                                    op: TsTypeOperatorOp::Unique,
                                                    type_ann: box RTsType::TsKeywordType(
                                                        RTsKeywordType {
                                                            span:*span,
                                                            kind:
                                                                TsKeywordTypeKind::TsSymbolKeyword,
                                                        },
                                                    ),
                                                })
                                            }

                                            _ => box RTsType::TsKeywordType(RTsKeywordType {
                                                span:*span,
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
                                            box RTsType::TsArrayType(RTsArrayType {
                                                span: *span,
                                                elem_type: match constraint {
                                                    Some(_constraint) => {
                                                        // TODO: We need something smarter
                                                        box RTsType::TsKeywordType(RTsKeywordType {
                                                            span: *elem_span,
                                                            kind: TsKeywordTypeKind::TsAnyKeyword,
                                                        })
                                                    }
                                                    None => {
                                                        box RTsType::TsKeywordType(RTsKeywordType {
                                                            span: *elem_span,
                                                            kind: TsKeywordTypeKind::TsAnyKeyword,
                                                        })
                                                    }
                                                },
                                            })
                                        }

                                        // We failed to infer type of the type parameter.
                                        Type::Param(TypeParam { span, .. }) => {
                                            box RTsType::TsKeywordType(RTsKeywordType {
                                                span: *span,
                                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                            })
                                        }
                                        _ => ty.into(),
                                    };

                                    ty.into()
                                })));
                            }
                        }
                        match ty.normalize() {
                            Type::Ref(..) => {}
                            _ => {
                                ty = self.expand(span, ty)?;
                            }
                        }
                        self.check_rvalue(&ty);

                        let mut type_errors = Errors::default();

                        // Handle implicit any

                        match ty.normalize_mut() {
                            Type::Tuple(Tuple { ref mut elems, .. }) => {
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
                                    element.ty = Type::any(span);

                                    if self.rule().no_implicit_any {
                                        match v.name {
                                            RPat::Ident(ref i) => {
                                                let span = i.span;
                                                type_errors.push(Error::ImplicitAny { span });
                                                break;
                                            }
                                            RPat::Array(RArrayPat { ref elems, .. }) => {
                                                let span = elems[i].span();
                                                type_errors.push(Error::ImplicitAny { span });
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }

                        if !type_errors.is_empty() {
                            self.storage.report_all(type_errors);
                            remove_declaring!();
                            return Ok(());
                        }

                        self.declare_complex_vars(kind, &v.name, ty)
                            .report(&mut self.storage);
                        remove_declaring!();
                        return Ok(());
                    }
                }
            } else {
                match v.name {
                    RPat::Ident(ref mut i) => {
                        //
                        let sym: Id = (&*i).into();
                        let mut ty = try_opt!(i.type_ann.validate_with(self));
                        match ty {
                            Some(ref mut ty) => {
                                self.prevent_expansion(&mut *ty);
                            }
                            _ => {}
                        }

                        match self.declare_var(
                            i.span,
                            kind,
                            sym,
                            ty,
                            // initialized
                            false,
                            // allow_multiple
                            kind == VarDeclKind::Var,
                        ) {
                            Ok(()) => {}
                            Err(err) => {
                                self.storage.report(err);
                            }
                        };
                    }
                    _ => {
                        // assert!(
                        //     var.declare,
                        //     "complex pattern without initializer is invalid syntax and
                        // parser \      should handle it"
                        //  );

                        if self.ctx.in_declare {
                            match self.declare_vars(kind, &mut v.name) {
                                Ok(()) => {}
                                Err(err) => {
                                    self.storage.report(err);
                                }
                            };
                        } else {
                            // This is parsing error
                        }
                    }
                };
                remove_declaring!();
                return Ok(());
            };

            debug_assert_eq!(self.ctx.allow_ref_declaring, true);
            if v.name.get_ty().is_none() {
                self.declare_vars(kind, &mut v.name)
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
