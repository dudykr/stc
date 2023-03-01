use std::{borrow::Cow, collections::HashMap};

use itertools::Itertools;
use rnode::{NodeId, VisitWith};
use stc_ts_ast_rnode::{
    RArrayPat, RAssignPatProp, RBindingIdent, RComputedPropName, RExpr, RIdent, RInvalid, RObjectPat, RObjectPatProp, RPat, RTsArrayType,
    RTsCallSignatureDecl, RTsConditionalType, RTsConstructSignatureDecl, RTsConstructorType, RTsEntityName, RTsExprWithTypeArgs,
    RTsFnOrConstructorType, RTsFnParam, RTsFnType, RTsGetterSignature, RTsImportType, RTsIndexSignature, RTsIndexedAccessType,
    RTsInferType, RTsInterfaceBody, RTsInterfaceDecl, RTsIntersectionType, RTsKeywordType, RTsLit, RTsMappedType, RTsMethodSignature,
    RTsOptionalType, RTsParenthesizedType, RTsPropertySignature, RTsRestType, RTsSetterSignature, RTsTplLitType, RTsTupleElement,
    RTsTupleType, RTsType, RTsTypeAliasDecl, RTsTypeAnn, RTsTypeElement, RTsTypeLit, RTsTypeOperator, RTsTypeParam, RTsTypeParamDecl,
    RTsTypeParamInstantiation, RTsTypePredicate, RTsTypeQuery, RTsTypeQueryExpr, RTsTypeRef, RTsUnionOrIntersectionType, RTsUnionType,
};
use stc_ts_errors::ErrorKind;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::{
    type_id::SymbolId, Accessor, Alias, AliasMetadata, ArcCowType, Array, CallSignature, CommonTypeMetadata, ComputedKey, Conditional,
    ConstructorSignature, FnParam, Id, IdCtx, ImportType, IndexSignature, IndexedAccessType, InferType, InferTypeMetadata, Interface,
    IntrinsicKind, Key, KeywordType, KeywordTypeMetadata, LitType, LitTypeMetadata, Mapped, MethodSignature, Operator, OptionalType,
    Predicate, PropertySignature, QueryExpr, QueryType, Ref, RefMetadata, RestType, StringMapping, Symbol, ThisType, TplElem, TplType,
    TsExpr, Tuple, TupleElement, TupleMetadata, Type, TypeElement, TypeLit, TypeLitMetadata, TypeParam, TypeParamDecl,
    TypeParamInstantiation,
};
use stc_ts_utils::{find_ids_in_pat, PatExt};
use stc_utils::{cache::Freeze, dev_span, AHashSet};
use swc_atoms::js_word;
use swc_common::{Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::{TsKeywordTypeKind, TsTypeOperatorOp};
use tracing::warn;

use crate::{
    analyzer::{
        expr::{AccessPropertyOpts, TypeOfMode},
        props::ComputedPropMode,
        scope::VarKind,
        util::ResultExt,
        Analyzer, Ctx, ScopeKind,
    },
    util::contains_infer_type,
    validator,
    validator::ValidateWith,
    VResult,
};

mod interface;

/// We analyze dependencies between type parameters, and fold parameter in
/// topological order.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsTypeParamDecl) -> VResult<TypeParamDecl> {
        if self.config.is_builtin {
            Ok(TypeParamDecl {
                span: decl.span,
                params: decl.params.validate_with(self)?,
                tracker: Default::default(),
            })
        } else {
            {
                // Check for duplicates
                let names = decl.params.iter().map(|param| param.name.clone()).collect::<Vec<_>>();
                let mut found = AHashSet::default();

                for name in names {
                    if !found.insert(name.sym.clone()) {
                        self.storage.report(
                            ErrorKind::DuplicateName {
                                span: name.span,
                                name: name.into(),
                            }
                            .context("tried to validate duplicate entries of a type parameter declaration"),
                        );
                    }
                }
                //
            }

            for param in &decl.params {
                let name: Id = param.name.clone().into();
                self.register_type(
                    name.clone(),
                    Type::Param(TypeParam {
                        span: param.span,
                        name,
                        constraint: None,
                        default: None,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                    .freezed(),
                );
            }

            let params: Vec<TypeParam> = decl.params.validate_with(self)?;

            let ctxt = self.ctx.module_id;
            let mut map = HashMap::default();
            for param in &params {
                let ty = self.find_type(&param.name).unwrap().unwrap().next().unwrap();

                map.entry(param.name.clone()).or_insert_with(|| ty.into_owned());
            }

            // Resolve constraints
            let mut params = self.expand_type_params(&map, params, Default::default())?;
            params.freeze();

            for param in &params {
                self.register_type(param.name.clone(), Type::Param(param.clone()));
            }

            Ok(TypeParamDecl {
                span: decl.span,
                params,
                tracker: Default::default(),
            })
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RTsTypeParam) -> VResult<TypeParam> {
        let ctx = Ctx {
            in_actual_type: true,
            ..self.ctx
        };
        let constraint = try_opt!(p.constraint.validate_with(&mut *self.with_ctx(ctx))).freezed();
        let default = try_opt!(p.default.validate_with(&mut *self.with_ctx(ctx))).freezed();

        let has_constraint = constraint.is_some();

        let param = TypeParam {
            span: p.span,
            name: p.name.clone().into(),
            constraint,
            default,
            metadata: Default::default(),
            tracker: Default::default(),
        };
        self.register_type(param.name.clone(), param.clone().into());

        if cfg!(debug_assertions) && has_constraint {
            if let Ok(types) = self.find_type(&p.name.clone().into()) {
                let types = types.expect("should be stored").collect_vec();

                debug_assert_eq!(types.len(), 1, "Types: {:?}", types);

                match types[0] {
                    Type::Param(p) => {
                        assert!(p.constraint.is_some(), "should store constraint");
                    }
                    _ => {
                        unreachable!()
                    }
                }
            }
        }

        Ok(param)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    #[inline]
    fn validate(&mut self, ann: &RTsTypeAnn) -> VResult<ArcCowType> {
        let ctx = Ctx {
            in_actual_type: true,
            ..self.ctx
        };

        ann.type_ann.validate_with(&mut *self.with_ctx(ctx))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsTypeAliasDecl) -> VResult<ArcCowType> {
        let span = d.span;

        let alias = {
            self.with_child(ScopeKind::Flow, Default::default(), |child: &mut Analyzer| -> VResult<_> {
                let type_params = try_opt!(d.type_params.validate_with(child)).map(From::from);

                let mut ty = match &*d.type_ann {
                    RTsType::TsKeywordType(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsIntrinsicKeyword,
                    }) if !child.config.is_builtin => {
                        let span = *span;
                        child.storage.report(ErrorKind::IntrinsicIsBuiltinOnly { span }.into());
                        Type::any(span.with_ctxt(SyntaxContext::empty()), Default::default())
                    }

                    RTsType::TsKeywordType(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsIntrinsicKeyword,
                    }) => Type::StringMapping(StringMapping {
                        span: d.span,
                        kind: IntrinsicKind::from(&*d.id.sym),
                        type_args: TypeParamInstantiation {
                            span: d.span,
                            params: type_params
                                .clone()
                                .unwrap()
                                .params
                                .into_iter()
                                .map(|v| {
                                    Type::Param(TypeParam {
                                        span: DUMMY_SP,
                                        name: v.name,
                                        constraint: Default::default(),
                                        default: Default::default(),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    })
                                })
                                .collect(),
                        },
                        metadata: Default::default(),
                    }),

                    _ => d.type_ann.validate_with(child)?,
                };

                let contains_infer_type = contains_infer_type(&ty);

                // If infer type exists, it should be expanded to remove infer type.
                if contains_infer_type {
                    child.mark_type_as_infer_type_container(&mut ty);
                } else {
                    child.prevent_expansion(&mut ty);
                }
                ty.freeze();
                let alias = Type::Alias(Alias {
                    span: span.with_ctxt(SyntaxContext::empty()),
                    ty: box ty,
                    type_params,
                    metadata: AliasMetadata {
                        common: CommonTypeMetadata {
                            contains_infer_type,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
                .freezed();
                Ok(alias)
            })?
        };
        self.register_type(d.id.clone().into(), alias.clone());

        self.store_unmergable_type_span(d.id.clone().into(), d.id.span);

        Ok(alias)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsInterfaceDecl) -> VResult<ArcCowType> {
        let ty = self.with_child(ScopeKind::Flow, Default::default(), |child: &mut Analyzer| -> VResult<_> {
            match &*d.id.sym {
                "any" | "void" | "never" | "unknown" | "string" | "number" | "bigint" | "boolean" | "null" | "undefined" | "symbol" => {
                    child.storage.report(ErrorKind::InvalidInterfaceName { span: d.id.span }.into());
                }
                _ => {}
            }

            let mut ty = Interface {
                span: d.span,
                name: d.id.clone().into(),
                type_params: try_opt!(d.type_params.validate_with(&mut *child).map(|v| v.map(From::from))),
                extends: d.extends.validate_with(child)?.freezed(),
                body: d.body.validate_with(child)?,
                metadata: Default::default(),
                tracker: Default::default(),
            };
            child.prevent_expansion(&mut ty.body);
            ty.body.freeze();

            child.resolve_parent_interfaces(&d.extends, true);
            child.report_error_for_conflicting_parents(d.id.span, &ty.extends);
            child.report_error_for_wrong_interface_inheritance(d.id.span, &ty.body, &ty.extends);

            let ty = Type::Interface(ty).freezed();

            Ok(ty)
        })?;

        // TODO(kdy1): Recover
        self.register_type(d.id.clone().into(), ty.clone());

        Ok(ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsInterfaceBody) -> VResult<Vec<TypeElement>> {
        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Interface,
            ..self.ctx
        };

        let members = node.body.validate_with(&mut *self.with_ctx(ctx))?;

        self.report_error_for_duplicate_type_elements(&members);

        Ok(members)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, lit: &RTsTypeLit) -> VResult<TypeLit> {
        let members = lit.members.validate_with(self)?;

        self.report_error_for_duplicate_type_elements(&members);
        self.report_errors_for_mixed_optional_method_signatures(&members);

        Ok(TypeLit {
            span: lit.span,
            members,
            metadata: TypeLitMetadata {
                specified: true,
                ..Default::default()
            },
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTsTypeElement) -> VResult<TypeElement> {
        Ok(match e {
            RTsTypeElement::TsCallSignatureDecl(d) => TypeElement::Call(d.validate_with(self)?),
            RTsTypeElement::TsConstructSignatureDecl(d) => TypeElement::Constructor(d.validate_with(self)?),
            RTsTypeElement::TsIndexSignature(d) => TypeElement::Index(d.validate_with(self)?),
            RTsTypeElement::TsMethodSignature(d) => TypeElement::Method(d.validate_with(self)?),
            RTsTypeElement::TsPropertySignature(d) => TypeElement::Property(d.validate_with(self)?),
            RTsTypeElement::TsGetterSignature(d) => TypeElement::Property(d.validate_with(self)?),
            RTsTypeElement::TsSetterSignature(d) => TypeElement::Property(d.validate_with(self)?),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsConstructSignatureDecl) -> VResult<ConstructorSignature> {
        let type_params = try_opt!(d.type_params.validate_with(self));
        Ok(ConstructorSignature {
            accessibility: None,
            span: d.span,
            params: d.params.validate_with(self)?,
            type_params,
            ret_ty: try_opt!(d.type_ann.validate_with(self)).map(From::from),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsCallSignatureDecl) -> VResult<CallSignature> {
        let type_params = try_opt!(d.type_params.validate_with(self));
        let params: Vec<FnParam> = d.params.validate_with(self)?;
        let ret_ty = try_opt!(d.type_ann.validate_with(self)).map(From::from);

        self.report_error_for_duplicate_params(&params);

        Ok(CallSignature {
            span: d.span,
            params,
            type_params,
            ret_ty,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsMethodSignature) -> VResult<MethodSignature> {
        self.with_child(ScopeKind::Fn, Default::default(), |child: &mut Analyzer| {
            let type_params = try_opt!(d.type_params.validate_with(child));

            let key = child.validate_key(&d.key, d.computed)?;

            if d.computed {
                child.validate_computed_prop_key(d.span(), &d.key).report(&mut child.storage);
            }

            let params = d.params.validate_with(child)?;
            child.report_error_for_duplicate_params(&params);

            Ok(MethodSignature {
                accessibility: None,
                span: d.span,
                readonly: d.readonly,
                key,
                optional: d.optional,
                type_params,
                params,
                ret_ty: try_opt!(d.type_ann.validate_with(child)).map(From::from),
                metadata: Default::default(),
            })
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsIndexSignature) -> VResult<IndexSignature> {
        Ok(IndexSignature {
            span: d.span,
            params: d.params.validate_with(self)?,
            readonly: d.readonly,
            type_ann: try_opt!(d.type_ann.validate_with(self)).map(From::from),
            is_static: d.is_static,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsPropertySignature) -> VResult<PropertySignature> {
        let type_params = try_opt!(d.type_params.validate_with(self));

        let key = self.validate_key(&d.key, d.computed)?;
        if !self.config.is_builtin && d.computed {
            RComputedPropName {
                node_id: NodeId::invalid(),
                span: d.key.span(),
                expr: d.key.clone(),
            }
            .visit_with(self);
        }

        let params = d.params.validate_with(self)?;

        let type_ann = {
            // TODO(kdy1): implicit any
            match d.type_ann.validate_with(self) {
                Some(v) => match v {
                    Ok(mut ty) => {
                        // Handle some symbol types.
                        if self.config.is_builtin {
                            if ty.is_unique_symbol() || ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                                let key = match &key {
                                    Key::Normal { sym, .. } => sym,
                                    _ => {
                                        unreachable!("builtin: non-string key for symbol type")
                                    }
                                };
                                ty = Type::Symbol(Symbol {
                                    span: DUMMY_SP,
                                    id: SymbolId::known(key),
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                })
                                .into();
                            }
                        }

                        Some(ty)
                    }
                    Err(e) => {
                        self.storage.report(e);
                        Some(Type::any(d.span, Default::default()).into())
                    }
                },
                None => Some(Type::any(d.span, Default::default().into())),
            }
        };

        Ok(PropertySignature {
            accessibility: None,
            span: d.span,
            key,
            optional: d.optional,
            params,
            readonly: d.readonly,
            type_ann,
            type_params,
            metadata: Default::default(),
            accessor: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsGetterSignature) -> VResult<PropertySignature> {
        let key = self.validate_key(&d.key, d.computed)?;
        let type_ann = {
            match d.type_ann.validate_with(self) {
                Some(v) => match v {
                    Ok(ty) => Some(ty),
                    Err(e) => {
                        self.storage.report(e);
                        Some(Type::any(d.span, Default::default()).into())
                    }
                },
                None => Some(Type::any(d.span, Default::default()).into()),
            }
        };
        Ok(PropertySignature {
            accessibility: None,
            span: d.span,
            key,
            optional: d.optional,
            params: Default::default(),
            readonly: d.readonly,
            type_ann,
            type_params: Default::default(),
            metadata: Default::default(),
            accessor: Accessor {
                getter: true,
                setter: false,
            },
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsSetterSignature) -> VResult<PropertySignature> {
        let key = self.validate_key(&d.key, d.computed)?;
        let params = vec![d.param.validate_with(self)?];
        Ok(PropertySignature {
            accessibility: None,
            span: d.span,
            key,
            optional: d.optional,
            params,
            readonly: d.readonly,
            type_ann: Default::default(),
            type_params: Default::default(),
            metadata: Default::default(),
            accessor: Accessor {
                getter: false,
                setter: true,
            },
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTsExprWithTypeArgs) -> VResult<TsExpr> {
        Ok(TsExpr {
            span: e.span,
            expr: e.expr.clone(),
            type_args: try_opt!(e.type_args.validate_with(self)).map(From::from),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, i: &RTsTypeParamInstantiation) -> VResult<TypeParamInstantiation> {
        let params = {
            let ctx = Ctx {
                in_actual_type: true,
                ..self.ctx
            };
            i.params.validate_with(&mut *self.with_ctx(ctx))?
        };

        Ok(TypeParamInstantiation { span: i.span, params })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTupleType) -> VResult<Tuple> {
        let marks = self.marks();

        let span = t.span;

        Ok(Tuple {
            span,
            elems: t.elem_types.validate_with(self)?,
            metadata: TupleMetadata {
                prevent_tuple_to_array: true,
                ..Default::default()
            },
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsTupleElement) -> VResult<TupleElement> {
        Ok(TupleElement {
            span: node.span,
            label: node.label.clone(),
            ty: node.ty.validate_with(self)?,
            tracker: Default::default(),
        })
    }
}

/// Order of evaluation is important to handle infer types correctly.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsConditionalType) -> VResult<Conditional> {
        let check_type = t.check_type.validate_with(self)?;
        let extends_type = t.extends_type.validate_with(self)?;
        let true_type = t.true_type.validate_with(self)?;
        let false_type = t.false_type.validate_with(self)?;

        Ok(Conditional {
            span: t.span,
            check_type,
            extends_type,
            true_type,
            false_type,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsMappedType) -> VResult<Mapped> {
        let type_param = box ty.type_param.validate_with(self)?;

        Ok(Mapped {
            span: ty.span,
            readonly: ty.readonly,
            optional: ty.optional,
            name_type: try_opt!(ty.name_type.validate_with(self)).map(From::from),
            type_param,
            ty: try_opt!(ty.type_ann.validate_with(self)).map(From::from),
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsTypeOperator) -> VResult<Operator> {
        Ok(Operator {
            span: ty.span,
            op: ty.op,
            ty: ty.type_ann.validate_with(self)?,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsArrayType) -> VResult<Array> {
        Ok(Array {
            span: node.span,
            elem_type: node.elem_type.validate_with(self)?,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &RTsUnionType) -> VResult<ArcCowType> {
        let types = u.types.validate_with(self)?;

        Ok(Type::new_union(u.span, types))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &RTsIntersectionType) -> VResult<ArcCowType> {
        let types = u.types.validate_with(self)?;

        Ok(Type::new_intersection(u.span, types))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsFnType) -> VResult<stc_ts_types::Function> {
        let ctx = Ctx {
            in_ts_fn_type: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with_scope_for_type_params(|child: &mut Analyzer| {
            let type_params = try_opt!(t.type_params.validate_with(child));

            for param in &t.params {
                child.default_any_param(param);
            }

            let mut params: Vec<_> = t.params.validate_with(child)?;
            params.freeze();

            let mut ret_ty = t.type_ann.validate_with(child)?;

            if !child.config.is_builtin {
                for param in params.iter() {
                    child
                        .declare_complex_vars(VarKind::Param, &param.pat, *param.ty.clone(), None, None)
                        .report(&mut child.storage);
                }
            }

            child.expand_return_type_of_fn(&mut ret_ty).report(&mut child.storage);

            Ok(stc_ts_types::Function {
                span: t.span,
                type_params,
                params,
                ret_ty,
                metadata: Default::default(),
                tracker: Default::default(),
            })
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsConstructorType) -> VResult<stc_ts_types::Constructor> {
        let type_params = try_opt!(t.type_params.validate_with(self));

        for param in &t.params {
            self.default_any_param(param);
        }

        Ok(stc_ts_types::Constructor {
            span: t.span,
            type_params,
            params: t.params.validate_with(self)?,
            type_ann: t.type_ann.validate_with(self).map(From::from)?,
            is_abstract: t.is_abstract,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsParenthesizedType) -> VResult<ArcCowType> {
        t.type_ann.validate_with(self)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeRef) -> VResult<ArcCowType> {
        let span = t.span;
        let type_args = try_opt!(t.type_params.validate_with(self)).freezed();
        let mut contains_infer = false;

        let mut reported_type_not_found = false;

        match t.type_name {
            RTsEntityName::Ident(ref i) if i.sym == js_word!("Array") && type_args.is_some() => {
                if type_args.as_ref().unwrap().params.len() == 1 {
                    return Ok(Type::Array(Array {
                        span: t.span,
                        elem_type: box type_args.unwrap().params.into_iter().next().unwrap(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
            }
            RTsEntityName::Ident(ref i) if i.sym == js_word!("ReadonlyArray") && type_args.is_some() => {
                if type_args.as_ref().unwrap().params.len() == 1 {
                    return Ok(Type::Operator(Operator {
                        span,
                        op: TsTypeOperatorOp::ReadOnly,
                        ty: box Type::Array(Array {
                            span: t.span,
                            elem_type: box type_args.unwrap().params.into_iter().next().unwrap(),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
            }

            RTsEntityName::Ident(ref i) => {
                self.report_error_for_type_param_usages_in_static_members(i);

                if let Some(types) = self.find_type(&i.into())? {
                    let mut found = false;
                    for ty in types {
                        found = true;

                        if contains_infer_type(&*ty) {
                            contains_infer = true;
                        }
                        // We use type param instead of reference type if possible.
                        if let Type::Param(..) = ty {
                            return Ok(ty.into_owned());
                        }
                    }

                    if !self.config.is_builtin && !found && self.ctx.in_actual_type {
                        if let Some(..) = self.scope.get_var(&i.into()) {
                            self.storage
                                .report(ErrorKind::NoSuchTypeButVarExists { span, name: i.into() }.into());
                            reported_type_not_found = true;
                        }
                    }
                } else {
                    if !self.config.is_builtin && self.ctx.in_actual_type {
                        if let Some(..) = self.scope.get_var(&i.into()) {
                            self.storage
                                .report(ErrorKind::NoSuchTypeButVarExists { span, name: i.into() }.into());
                            reported_type_not_found = true;
                        }
                    }
                }
            }

            _ => {}
        }

        if !self.config.is_builtin {
            if !cfg!(feature = "profile") {
                warn!("Creating a ref from TsTypeRef: {:?}", t.type_name);
            }

            if !reported_type_not_found {
                self.report_error_for_unresolved_type(t.span, &t.type_name.clone().into(), type_args.as_deref())
                    .report(&mut self.storage);
            }
        }

        Ok(Type::Ref(Ref {
            span: t.span.with_ctxt(SyntaxContext::empty()),
            type_name: t.type_name.clone(),
            type_args,
            metadata: RefMetadata {
                common: CommonTypeMetadata {
                    contains_infer_type: contains_infer,
                    ..Default::default()
                },
                ..Default::default()
            },
            tracker: Default::default(),
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsInferType) -> VResult<InferType> {
        Ok(InferType {
            span: t.span,
            type_param: t.type_param.validate_with(self)?,
            metadata: InferTypeMetadata {
                common: CommonTypeMetadata {
                    contains_infer_type: true,
                    ..Default::default()
                },
                ..Default::default()
            },
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsImportType) -> VResult<ImportType> {
        Ok(ImportType {
            span: t.span,
            arg: t.arg.clone(),
            qualifier: t.qualifier.clone(),
            type_params: try_opt!(t.type_args.validate_with(self)).map(From::from),
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeQueryExpr) -> VResult<QueryExpr> {
        let span = t.span();

        Ok(match t {
            RTsTypeQueryExpr::TsEntityName(t) => t.clone().into(),
            RTsTypeQueryExpr::Import(i) => i.validate_with(self)?.into(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsRestType) -> VResult<RestType> {
        Ok(RestType {
            span: t.span,
            ty: box t.type_ann.validate_with(self)?,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsOptionalType) -> VResult<OptionalType> {
        Ok(OptionalType {
            span: t.span,
            ty: box t.type_ann.validate_with(self)?,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeQuery) -> VResult<QueryType> {
        Ok(QueryType {
            span: t.span,
            expr: box t.expr_name.validate_with(self)?,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypePredicate) -> VResult<Predicate> {
        let mut ty = try_opt!(t.type_ann.validate_with(self)).map(From::from);
        match &mut ty {
            Some(ty) => {
                self.prevent_expansion(ty);
            }
            None => {}
        }

        Ok(Predicate {
            span: t.span,
            param_name: t.param_name.clone(),
            asserts: t.asserts,
            ty,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsIndexedAccessType) -> VResult<ArcCowType> {
        let span = t.span;

        let obj_type = box t.obj_type.validate_with(self)?;
        let index_type = box t.index_type.validate_with(self)?.freezed();

        if !self.config.is_builtin {
            let ctx = Ctx {
                disallow_unknown_object_property: true,
                ..self.ctx
            };
            let prop_ty = self.with_ctx(ctx).access_property(
                span,
                &obj_type,
                &Key::Computed(ComputedKey {
                    span,
                    expr: box RExpr::Invalid(RInvalid { span }),
                    ty: index_type.clone(),
                }),
                TypeOfMode::RValue,
                IdCtx::Type,
                AccessPropertyOpts {
                    for_validation_of_indexed_access_type: true,
                    ..Default::default()
                },
            );

            prop_ty.report(&mut self.storage);
        }

        Ok(Type::IndexedAccessType(IndexedAccessType {
            span,
            readonly: t.readonly,
            obj_type,
            index_type,
            metadata: Default::default(),
            tracker: Default::default(),
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTplLitType) -> VResult<TplType> {
        let types = t
            .types
            .iter()
            .map(|ty| ty.validate_with(self).map(From::from))
            .collect::<Result<_, _>>()?;

        Ok(TplType {
            span: t.span,
            quasis: t
                .quasis
                .iter()
                .map(|v| TplElem {
                    span: v.span,
                    value: v.cooked.clone().unwrap(),
                })
                .collect(),
            types,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsType) -> VResult<ArcCowType> {
        let is_topmost_type = !self.ctx.is_not_topmost_type;
        let ctx = Ctx {
            is_not_topmost_type: true,
            ..self.ctx
        };
        let ty = self.with_ctx(ctx).with(|a| {
            let ty = match ty {
                RTsType::TsThisType(this) => Type::This(ThisType {
                    span: this.span,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }),
                RTsType::TsLitType(ty) => {
                    if let RTsLit::Tpl(t) = &ty.lit {
                        return Ok(t.validate_with(a)?.into());
                    }

                    Type::Lit(LitType {
                        span: ty.span,
                        lit: ty.lit.clone(),
                        metadata: LitTypeMetadata {
                            common: CommonTypeMetadata {
                                prevent_generalization: true,
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                        tracker: Default::default(),
                    })
                }
                RTsType::TsKeywordType(ty) => {
                    if let TsKeywordTypeKind::TsIntrinsicKeyword = ty.kind {
                        if !a.config.is_builtin {
                            let span = ty.span;

                            a.storage.report(
                                ErrorKind::NoSuchType {
                                    span,
                                    name: Id::word("intrinsic".into()),
                                }
                                .into(),
                            );
                            return Ok(Type::any(span.with_ctxt(SyntaxContext::empty()), Default::default()));
                        }
                    }
                    Type::Keyword(KeywordType {
                        span: ty.span,
                        kind: ty.kind,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                }
                RTsType::TsTupleType(ty) => Type::Tuple(ty.validate_with(a)?),
                RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsUnionType(u)) => u.validate_with(a)?,
                RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsIntersectionType(i)) => i.validate_with(a)?,
                RTsType::TsArrayType(arr) => Type::Array(arr.validate_with(a)?),
                RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsFnType(f)) => Type::Function(f.validate_with(a)?),
                RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsConstructorType(c)) => Type::Constructor(c.validate_with(a)?),
                RTsType::TsTypeLit(lit) => Type::TypeLit(lit.validate_with(a)?),
                RTsType::TsConditionalType(cond) => Type::Conditional(cond.validate_with(a)?),
                RTsType::TsMappedType(ty) => Type::Mapped(ty.validate_with(a)?),
                RTsType::TsTypeOperator(ty) => Type::Operator(ty.validate_with(a)?),
                RTsType::TsParenthesizedType(ty) => return ty.validate_with(a),
                RTsType::TsTypeRef(ty) => ty.validate_with(a)?,
                RTsType::TsTypeQuery(ty) => Type::Query(ty.validate_with(a)?),
                RTsType::TsOptionalType(ty) => Type::Optional(ty.validate_with(a)?),
                RTsType::TsRestType(ty) => Type::Rest(ty.validate_with(a)?),
                RTsType::TsInferType(ty) => Type::Infer(ty.validate_with(a)?),
                RTsType::TsIndexedAccessType(ty) => ty.validate_with(a)?,
                RTsType::TsTypePredicate(ty) => Type::Predicate(ty.validate_with(a)?),
                RTsType::TsImportType(ty) => Type::Import(ty.validate_with(a)?),
            };

            ty.assert_valid();

            Ok(ty)
        })?;

        if is_topmost_type {
            Ok(ty.freezed())
        } else {
            Ok(ty)
        }
    }
}

impl Analyzer<'_, '_> {
    fn report_error_for_duplicate_type_elements(&mut self, elems: &[TypeElement]) {
        let _tracing = dev_span!("report_error_for_duplicate_type_elements");

        if self.config.is_builtin {
            return;
        }

        let mut prev_keys: Vec<Cow<_>> = vec![];

        for elem in elems {
            if let TypeElement::Property(PropertySignature {
                accessor:
                    Accessor {
                        getter: false,
                        setter: false,
                        ..
                    },
                ..
            }) = elem
            {
                if let Some(key) = elem.key() {
                    let key = key;
                    let key_ty = key.ty();

                    if key_ty.is_symbol() {
                        continue;
                    }
                    if let Some(prev) = prev_keys.iter().find(|prev_key| key.type_eq(prev_key)) {
                        self.storage
                            .report(ErrorKind::DuplicateNameWithoutName { span: prev.span() }.into());
                        self.storage.report(ErrorKind::DuplicateNameWithoutName { span: key.span() }.into());
                    } else {
                        prev_keys.push(key);
                    }
                }
            }
        }
    }

    fn report_error_for_duplicate_params(&mut self, params: &[FnParam]) {
        let _tracing = dev_span!("report_error_for_duplicate_params");

        if self.config.is_builtin {
            return;
        }

        let mut prev_ids: Vec<RIdent> = vec![];
        for param in params {
            let ids: Vec<RIdent> = find_ids_in_pat(&param.pat);

            for id in ids {
                if let Some(prev) = prev_ids.iter().find(|v| v.sym == id.sym) {
                    self.storage.report(
                        ErrorKind::DuplicateName {
                            span: prev.span,
                            name: prev.into(),
                        }
                        .context("report_error_for_duplicate_params"),
                    );
                    self.storage.report(
                        ErrorKind::DuplicateName {
                            span: id.span,
                            name: id.into(),
                        }
                        .context("report_error_for_duplicate_params"),
                    );
                } else {
                    prev_ids.push(id);
                }
            }
        }
    }

    #[extra_validator]
    fn report_error_for_type_param_usages_in_static_members(&mut self, i: &RIdent) {
        let _tracing = dev_span!("report_error_for_type_param_usages_in_static_members");

        let span = i.span;
        let id = i.into();
        let static_method = self.scope.first(|scope| {
            let parent = scope.parent();
            let parent = match parent {
                Some(v) => v,
                None => return false,
            };
            if parent.kind() != ScopeKind::Class {
                return false;
            }
            if !parent.declaring_type_params.contains(&id) {
                return false;
            }

            matches!(scope.kind(), ScopeKind::Method { is_static: true, .. })
        });

        if static_method.is_some() {
            self.storage
                .report(ErrorKind::StaticMemberCannotUseTypeParamOfClass { span }.into())
        }
    }

    /// Handle implicit defaults.
    pub(crate) fn default_any_pat(&mut self, p: &RPat) {
        match p {
            RPat::Ident(i) => self.default_any_ident(i),
            RPat::Array(arr) => self.default_any_array_pat(arr),
            RPat::Object(obj) => self.default_any_object(obj),
            _ => {}
        }
    }

    /// Handle implicit defaults.
    pub(crate) fn default_any_ident(&mut self, i: &RBindingIdent) {
        if i.type_ann.is_some() {
            return;
        }

        if let Some(m) = &mut self.mutations {
            if m.for_pats.entry(i.node_id).or_default().ty.is_some() {
                return;
            }
        }

        #[allow(clippy::nonminimal_bool)]
        if !self.ctx.is_calling_iife && self.env.rule().no_implicit_any {
            let no_type_ann =
                !self.ctx.in_argument && !(self.ctx.in_return_arg && self.ctx.in_fn_with_return_type) && !self.ctx.in_assign_rhs;
            if no_type_ann || self.ctx.in_useless_expr_for_seq || self.ctx.check_for_implicit_any {
                self.storage
                    .report(ErrorKind::ImplicitAny { span: i.id.span }.context("default type"));
            }
        }

        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(i.node_id).or_default().ty.get_or_insert_with(|| {
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
            });
        }
    }

    /// Handle implicit defaults.
    pub(crate) fn default_any_array_pat(&mut self, arr: &RArrayPat) {
        if arr.type_ann.is_some() {
            return;
        }
        let cnt = arr.elems.len();

        let ty = Type::Tuple(Tuple {
            span: DUMMY_SP,
            elems: arr
                .elems
                .iter()
                .map(|elem| {
                    let span = elem.span();
                    // any
                    let ty = match elem {
                        Some(RPat::Array(ref arr)) => {
                            self.default_any_array_pat(arr);
                            if let Some(m) = &mut self.mutations {
                                m.for_pats.entry(arr.node_id).or_default().ty.take().unwrap()
                            } else {
                                unreachable!();
                            }
                        }
                        Some(RPat::Object(ref obj)) => {
                            self.default_any_object(obj);

                            if let Some(m) = &mut self.mutations {
                                m.for_pats.entry(obj.node_id).or_default().ty.take().unwrap()
                            } else {
                                unreachable!();
                            }
                        }

                        Some(RPat::Rest(pat)) => {
                            self.default_any_pat(&pat.arg);

                            let elem_ty = if let Some(m) = &mut self.mutations {
                                m.for_pats.entry(pat.arg.node_id().unwrap()).or_default().ty.take().unwrap()
                            } else {
                                unreachable!();
                            };
                            Type::Rest(RestType {
                                span,
                                ty: elem_ty,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                            .into()
                        }

                        _ => Type::any(DUMMY_SP, Default::default()).into(),
                    };

                    TupleElement {
                        span,
                        // TODO?
                        label: None,
                        ty,
                        tracker: Default::default(),
                    }
                })
                .collect(),
            metadata: Default::default(),
            tracker: Default::default(),
        });
        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(arr.node_id).or_default().ty.get_or_insert(ty);
        }
    }

    /// Handle implicit defaults.
    #[extra_validator]
    pub(crate) fn default_any_object(&mut self, obj: &RObjectPat) {
        if obj.type_ann.is_some() {
            return;
        }

        let mut members = Vec::with_capacity(obj.props.len());

        for props in &obj.props {
            match props {
                RObjectPatProp::KeyValue(p) => {
                    let key = p.key.validate_with(self)?;
                    match *p.value {
                        RPat::Array(_) | RPat::Object(_) => {
                            self.default_any_pat(&p.value);
                        }
                        _ => {}
                    }
                    let ty = if let Some(value_node_id) = p.value.node_id() {
                        if let Some(m) = &mut self.mutations {
                            m.for_pats.entry(value_node_id).or_default().ty.take().map(From::from)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    members.push(TypeElement::Property(PropertySignature {
                        span: DUMMY_SP,
                        accessibility: None,
                        readonly: false,
                        key,
                        optional: false,
                        params: vec![],
                        type_ann: ty,
                        type_params: None,
                        metadata: Default::default(),
                        accessor: Default::default(),
                    }))
                }
                RObjectPatProp::Assign(RAssignPatProp { key, value, .. }) => {
                    let key = Key::Normal {
                        span: key.span,
                        sym: key.sym.clone(),
                    };
                    members.push(TypeElement::Property(PropertySignature {
                        span: DUMMY_SP,
                        accessibility: None,
                        readonly: false,
                        key,
                        optional: value.is_some(),
                        params: vec![],
                        type_ann: None,
                        type_params: None,
                        metadata: Default::default(),
                        accessor: Default::default(),
                    }))
                }
                RObjectPatProp::Rest(..) => {}
            }
        }

        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(obj.node_id).or_default().ty.get_or_insert_with(|| {
                Type::TypeLit(TypeLit {
                    span: DUMMY_SP,
                    members,
                    metadata: TypeLitMetadata {
                        common: CommonTypeMetadata {
                            implicit: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
            });
        }
    }

    /// Handle implicit defaults.
    pub(crate) fn default_any_param(&mut self, p: &RTsFnParam) {
        match p {
            RTsFnParam::Ident(i) => self.default_any_ident(i),
            RTsFnParam::Array(arr) => self.default_any_array_pat(arr),
            RTsFnParam::Rest(rest) => {}
            RTsFnParam::Object(obj) => self.default_any_object(obj),
        }
    }
}
