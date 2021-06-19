use super::marks::MarkExt;
use super::props::ComputedPropMode;
use super::Analyzer;
use super::Ctx;
use super::ScopeKind;
use crate::analyzer::util::ResultExt;
use crate::util::contains_infer_type;
use crate::util::type_ext::TypeVecExt;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use rnode::NodeId;
use rnode::VisitWith;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RAssignPatProp;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RComputedPropName;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RTsArrayType;
use stc_ts_ast_rnode::RTsCallSignatureDecl;
use stc_ts_ast_rnode::RTsConditionalType;
use stc_ts_ast_rnode::RTsConstructSignatureDecl;
use stc_ts_ast_rnode::RTsConstructorType;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsExprWithTypeArgs;
use stc_ts_ast_rnode::RTsFnOrConstructorType;
use stc_ts_ast_rnode::RTsFnParam;
use stc_ts_ast_rnode::RTsFnType;
use stc_ts_ast_rnode::RTsImportType;
use stc_ts_ast_rnode::RTsIndexSignature;
use stc_ts_ast_rnode::RTsIndexedAccessType;
use stc_ts_ast_rnode::RTsInferType;
use stc_ts_ast_rnode::RTsInterfaceBody;
use stc_ts_ast_rnode::RTsInterfaceDecl;
use stc_ts_ast_rnode::RTsIntersectionType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsMappedType;
use stc_ts_ast_rnode::RTsMethodSignature;
use stc_ts_ast_rnode::RTsOptionalType;
use stc_ts_ast_rnode::RTsParenthesizedType;
use stc_ts_ast_rnode::RTsPropertySignature;
use stc_ts_ast_rnode::RTsRestType;
use stc_ts_ast_rnode::RTsTplLitType;
use stc_ts_ast_rnode::RTsTupleElement;
use stc_ts_ast_rnode::RTsTupleType;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeAliasDecl;
use stc_ts_ast_rnode::RTsTypeAnn;
use stc_ts_ast_rnode::RTsTypeElement;
use stc_ts_ast_rnode::RTsTypeLit;
use stc_ts_ast_rnode::RTsTypeOperator;
use stc_ts_ast_rnode::RTsTypeParam;
use stc_ts_ast_rnode::RTsTypeParamDecl;
use stc_ts_ast_rnode::RTsTypeParamInstantiation;
use stc_ts_ast_rnode::RTsTypePredicate;
use stc_ts_ast_rnode::RTsTypeQuery;
use stc_ts_ast_rnode::RTsTypeQueryExpr;
use stc_ts_ast_rnode::RTsTypeRef;
use stc_ts_ast_rnode::RTsUnionOrIntersectionType;
use stc_ts_ast_rnode::RTsUnionType;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_type_ops::Fix;
use stc_ts_types::Alias;
use stc_ts_types::Array;
use stc_ts_types::CallSignature;
use stc_ts_types::Conditional;
use stc_ts_types::ConstructorSignature;
use stc_ts_types::Id;
use stc_ts_types::ImportType;
use stc_ts_types::IndexSignature;
use stc_ts_types::IndexedAccessType;
use stc_ts_types::InferType;
use stc_ts_types::Interface;
use stc_ts_types::Intersection;
use stc_ts_types::Key;
use stc_ts_types::Mapped;
use stc_ts_types::MethodSignature;
use stc_ts_types::Operator;
use stc_ts_types::OptionalType;
use stc_ts_types::Predicate;
use stc_ts_types::PropertySignature;
use stc_ts_types::QueryExpr;
use stc_ts_types::QueryType;
use stc_ts_types::Ref;
use stc_ts_types::RestType;
use stc_ts_types::Symbol;
use stc_ts_types::SymbolId;
use stc_ts_types::TplType;
use stc_ts_types::TsExpr;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeLitMetadata;
use stc_ts_types::TypeParam;
use stc_ts_types::TypeParamDecl;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::Union;
use stc_ts_utils::OptionExt;
use stc_ts_utils::PatExt;
use stc_utils::error;
use stc_utils::AHashSet;
use swc_atoms::js_word;
use swc_common::EqIgnoreSpan;
use swc_common::Spanned;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::VarDeclKind;

/// We analyze dependencies between type parameters, and fold parameter in
/// topological order.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsTypeParamDecl) -> ValidationResult<TypeParamDecl> {
        self.record(decl);

        if self.is_builtin {
            Ok(TypeParamDecl {
                span: decl.span,
                params: decl.params.validate_with(self)?,
            })
        } else {
            {
                // Check for duplicates
                let mut names = decl.params.iter().map(|param| param.name.clone()).collect::<Vec<_>>();
                let mut found = AHashSet::default();

                for name in names {
                    if !found.insert(name.sym.clone()) {
                        self.storage.report(
                            Error::DuplicateName {
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
                    })
                    .cheap(),
                );
            }

            let params = decl.params.validate_with(self)?;

            Ok(TypeParamDecl {
                span: decl.span,
                params,
            })
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RTsTypeParam) -> ValidationResult<TypeParam> {
        self.record(p);

        let ctx = Ctx {
            in_actual_type: true,
            ..self.ctx
        };
        let constraint = try_opt!(p.constraint.validate_with(&mut *self.with_ctx(ctx))).map(Box::new);
        let default = try_opt!(p.default.validate_with(&mut *self.with_ctx(ctx))).map(Box::new);

        let param = TypeParam {
            span: p.span,
            name: p.name.clone().into(),
            constraint,
            default,
        };
        self.register_type(param.name.clone().into(), param.clone().into());

        Ok(param)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    #[inline]
    fn validate(&mut self, ann: &RTsTypeAnn) -> ValidationResult {
        self.record(ann);

        ann.type_ann.validate_with(self).map(|ty: Type| ty.fixed())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsTypeAliasDecl) -> ValidationResult<Alias> {
        self.record(d);
        let mut span = d.span;

        let alias = {
            self.with_child(
                ScopeKind::Flow,
                Default::default(),
                |child: &mut Analyzer| -> ValidationResult<_> {
                    let type_params = try_opt!(d.type_params.validate_with(child));

                    let mut ty = d.type_ann.validate_with(child)?;
                    // If infer type exists, it should be expanded to remove infer type.
                    if contains_infer_type(&ty) || child.contains_infer_type(&ty) {
                        span = child.mark_as_infer_type_container(span);
                        child.mark_type_as_infer_type_container(&mut ty);
                    } else {
                        child.prevent_expansion(&mut ty);
                    }
                    ty.make_cheap();
                    let alias = Alias {
                        span,
                        ty: box ty,
                        type_params,
                    };
                    Ok(alias)
                },
            )?
        };
        self.register_type(d.id.clone().into(), Type::Alias(alias.clone()));

        self.store_unmergedable_type_span(d.id.clone().into(), d.id.span);

        Ok(alias)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsInterfaceDecl) -> ValidationResult<Interface> {
        let ty: Interface = self.with_child(ScopeKind::Flow, Default::default(), |child| -> ValidationResult<_> {
            match &*d.id.sym {
                "any" | "void" | "never" | "string" | "number" | "boolean" | "null" | "undefined" | "symbol" => {
                    child.storage.report(Error::InvalidInterfaceName { span: d.id.span });
                }
                _ => {}
            }

            let mut ty = Interface {
                span: d.span,
                name: d.id.clone().into(),
                type_params: try_opt!(d.type_params.validate_with(&mut *child)),
                extends: d.extends.validate_with(child)?,
                body: d.body.validate_with(child)?,
            };
            child.prevent_expansion(&mut ty.body);

            child.resolve_parent_interfaces(&d.extends);

            Ok(ty)
        })?;

        // TODO: Recover
        self.register_type(d.id.clone().into(), Type::Interface(ty.clone()).cheap());

        Ok(ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsInterfaceBody) -> ValidationResult<Vec<TypeElement>> {
        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Interface,
            ..self.ctx
        };

        Ok(node.body.validate_with(&mut *self.with_ctx(ctx))?)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, lit: &RTsTypeLit) -> ValidationResult<TypeLit> {
        let members = lit.members.validate_with(self)?;

        let mut keys: Vec<Key> = vec![];

        for member in &members {
            match member {
                TypeElement::Method(..) => continue,
                _ => {}
            }
            if let Some(key) = member.key() {
                for prev in &keys {
                    if prev.eq_ignore_span(key) {
                        self.storage.report(Error::DuplicateName {
                            span: prev.span(),
                            name: Id::word("".into()),
                        });

                        self.storage.report(Error::DuplicateName {
                            span: key.span(),
                            name: Id::word("".into()),
                        });
                    }
                }

                keys.push(key.clone());
            }
        }

        Ok(TypeLit {
            span: lit.span,
            members,
            metadata: TypeLitMetadata {
                specified: true,
                ..Default::default()
            },
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTsTypeElement) -> ValidationResult<TypeElement> {
        Ok(match e {
            RTsTypeElement::TsCallSignatureDecl(d) => TypeElement::Call(d.validate_with(self)?),
            RTsTypeElement::TsConstructSignatureDecl(d) => TypeElement::Constructor(d.validate_with(self)?),
            RTsTypeElement::TsIndexSignature(d) => TypeElement::Index(d.validate_with(self)?),
            RTsTypeElement::TsMethodSignature(d) => TypeElement::Method(d.validate_with(self)?),
            RTsTypeElement::TsPropertySignature(d) => TypeElement::Property(d.validate_with(self)?),
            RTsTypeElement::TsGetterSignature(_) => {
                unimplemented!()
            }
            RTsTypeElement::TsSetterSignature(_) => {
                unimplemented!()
            }
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsConstructSignatureDecl) -> ValidationResult<ConstructorSignature> {
        let type_params = try_opt!(d.type_params.validate_with(self));
        Ok(ConstructorSignature {
            accessibility: None,
            span: d.span,
            params: d.params.validate_with(self)?,
            type_params,
            ret_ty: try_opt!(d.type_ann.validate_with(self)).map(Box::new),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsCallSignatureDecl) -> ValidationResult<CallSignature> {
        let type_params = try_opt!(d.type_params.validate_with(self));

        Ok(CallSignature {
            span: d.span,
            params: d.params.validate_with(self)?,
            type_params,
            ret_ty: try_opt!(d.type_ann.validate_with(self)).map(Box::new),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsMethodSignature) -> ValidationResult<MethodSignature> {
        self.with_child(ScopeKind::Fn, Default::default(), |child: &mut Analyzer| {
            let type_params = try_opt!(d.type_params.validate_with(child));

            let key = child.validate_key(&d.key, d.computed)?;

            if d.computed {
                child.validate_computed_prop_key(d.span(), &d.key);
            }

            Ok(MethodSignature {
                accessibility: None,
                span: d.span,
                readonly: d.readonly,
                key,
                optional: d.optional,
                type_params,
                params: d.params.validate_with(child)?,
                ret_ty: try_opt!(d.type_ann.validate_with(child)).map(Box::new),
                metadata: Default::default(),
            })
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsIndexSignature) -> ValidationResult<IndexSignature> {
        Ok(IndexSignature {
            span: d.span,
            params: d.params.validate_with(self)?,
            readonly: d.readonly,
            type_ann: try_opt!(d.type_ann.validate_with(self)).map(Box::new),
            is_static: d.is_static,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsPropertySignature) -> ValidationResult<PropertySignature> {
        let type_params = try_opt!(d.type_params.validate_with(self));

        let key = self.validate_key(&d.key, d.computed)?;
        if !self.is_builtin && d.computed {
            RComputedPropName {
                node_id: NodeId::invalid(),
                span: d.key.span(),
                expr: d.key.clone(),
            }
            .visit_with(self);
        }

        let params = d.params.validate_with(self)?;

        let type_ann = {
            // TODO: implicit any
            match d.type_ann.validate_with(self) {
                Some(v) => match v {
                    Ok(mut ty) => {
                        // Handle some symbol types.
                        if self.is_builtin {
                            if ty.is_unique_symbol() || ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                                let key = match &key {
                                    Key::Normal { sym, .. } => sym,
                                    _ => {
                                        unreachable!("builtin: non-string key for symbol type")
                                    }
                                };
                                ty = Type::Symbol(Symbol {
                                    span: DUMMY_SP,
                                    id: SymbolId::known(&key),
                                });
                            }
                        }

                        Some(box ty)
                    }
                    Err(e) => {
                        self.storage.report(e);
                        Some(box Type::any(d.span))
                    }
                },
                None => Some(box Type::any(d.span)),
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
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTsExprWithTypeArgs) -> ValidationResult<TsExpr> {
        Ok(TsExpr {
            span: e.span,
            expr: e.expr.clone(),
            type_args: try_opt!(e.type_args.validate_with(self)).map(Box::new),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, i: &RTsTypeParamInstantiation) -> ValidationResult<TypeParamInstantiation> {
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
    fn validate(&mut self, t: &RTsTupleType) -> ValidationResult<Tuple> {
        let marks = self.marks();

        let span = t.span;
        let span = marks.prevent_tuple_to_array.apply_to_span(span);

        Ok(Tuple {
            span,
            elems: t.elem_types.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsTupleElement) -> ValidationResult<TupleElement> {
        Ok(TupleElement {
            span: node.span,
            label: node.label.clone(),
            ty: box node.ty.validate_with(self)?,
        })
    }
}

/// Order of evaluation is important to handle infer types correctly.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsConditionalType) -> ValidationResult<Conditional> {
        let check_type = box t.check_type.validate_with(self)?;
        let extends_type = box t.extends_type.validate_with(self)?;
        let true_type = box t.true_type.validate_with(self)?;
        let false_type = box t.false_type.validate_with(self)?;

        Ok(Conditional {
            span: t.span,
            check_type,
            extends_type,
            true_type,
            false_type,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsMappedType) -> ValidationResult<Mapped> {
        let type_param = ty.type_param.validate_with(self)?;

        Ok(Mapped {
            span: ty.span,
            readonly: ty.readonly,
            optional: ty.optional,
            name_type: try_opt!(ty.name_type.validate_with(self)).map(Box::new),
            type_param,
            ty: try_opt!(ty.type_ann.validate_with(self)).map(Box::new),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsTypeOperator) -> ValidationResult<Operator> {
        Ok(Operator {
            span: ty.span,
            op: ty.op,
            ty: box ty.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsArrayType) -> ValidationResult<Array> {
        Ok(Array {
            span: node.span,
            elem_type: box node.elem_type.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &RTsUnionType) -> ValidationResult<Union> {
        let mut types = u.types.validate_with(self)?;

        types.dedup_type();

        Ok(Union { span: u.span, types })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &RTsIntersectionType) -> ValidationResult<Intersection> {
        Ok(Intersection {
            span: u.span,
            types: u.types.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsFnType) -> ValidationResult<stc_ts_types::Function> {
        let ctx = Ctx {
            in_ts_fn_type: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with_scope_for_type_params(|child: &mut Analyzer| {
            let type_params = try_opt!(t.type_params.validate_with(child));

            for param in &t.params {
                child.default_any_param(&param);
            }

            let mut params: Vec<_> = t.params.validate_with(child)?;

            let mut ret_ty = box t.type_ann.validate_with(child)?;

            if !child.is_builtin {
                for param in params.iter() {
                    child
                        .declare_complex_vars(VarDeclKind::Let, &param.pat, *param.ty.clone(), None, None)
                        .report(&mut child.storage);
                }
            }

            child.expand_return_type_of_fn(&mut ret_ty).report(&mut child.storage);

            Ok(stc_ts_types::Function {
                span: t.span,
                type_params,
                params,
                ret_ty,
            })
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsConstructorType) -> ValidationResult<stc_ts_types::Constructor> {
        let type_params = try_opt!(t.type_params.validate_with(self));

        for param in &t.params {
            self.default_any_param(param);
        }

        Ok(stc_ts_types::Constructor {
            span: t.span,
            type_params,
            params: t.params.validate_with(self)?,
            type_ann: t.type_ann.validate_with(self).map(Box::new)?,
            is_abstract: t.is_abstract,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsParenthesizedType) -> ValidationResult {
        t.type_ann.validate_with(self)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeRef) -> ValidationResult {
        self.record(t);

        let span = t.span;
        let type_args = try_opt!(t.type_params.validate_with(self)).map(Box::new);
        let mut contains_infer = false;

        let mut reported_type_not_founf = false;

        match t.type_name {
            RTsEntityName::Ident(ref i) if i.sym == js_word!("Array") && type_args.is_some() => {
                if type_args.as_ref().unwrap().params.len() == 1 {
                    return Ok(Type::Array(Array {
                        span: t.span,
                        elem_type: box type_args.unwrap().params.into_iter().next().unwrap(),
                    }));
                }
            }

            RTsEntityName::Ident(ref i) => {
                self.report_error_for_type_param_usages_in_static_members(&i);

                if let Some(types) = self.find_type(self.ctx.module_id, &i.into())? {
                    let mut found = false;
                    for ty in types {
                        found = true;

                        if contains_infer_type(&ty) || self.contains_infer_type(&*ty) {
                            contains_infer = true;
                        }
                        // We use type param instead of reference type if possible.
                        match ty.normalize() {
                            Type::Param(..) => return Ok(ty.into_owned()),
                            _ => {}
                        }
                    }

                    if !self.is_builtin && !found && self.ctx.in_actual_type {
                        if let Some(..) = self.scope.get_var(&i.into()) {
                            self.storage
                                .report(Error::NoSuchTypeButVarExists { span, name: i.into() });
                            reported_type_not_founf = true;
                        }
                    }
                } else {
                    if !self.is_builtin && self.ctx.in_actual_type {
                        if let Some(..) = self.scope.get_var(&i.into()) {
                            self.storage
                                .report(Error::NoSuchTypeButVarExists { span, name: i.into() });
                            reported_type_not_founf = true;
                        }
                    }
                }
            }

            _ => {}
        }

        if !self.is_builtin {
            slog::warn!(self.logger, "Crating a ref from TsTypeRef: {:?}", t.type_name);

            if !reported_type_not_founf {
                self.report_error_for_unresolve_type(t.span, &t.type_name, type_args.as_deref())
                    .report(&mut self.storage);
            }
        }
        let mut span = t.span;
        if contains_infer {
            span = self.mark_as_infer_type_container(span);
        }

        Ok(Type::Ref(Ref {
            span,
            ctxt: self.ctx.module_id,
            type_name: t.type_name.clone(),
            type_args,
        })
        .cheap())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsInferType) -> ValidationResult<InferType> {
        self.record(t);

        Ok(InferType {
            span: t.span,
            type_param: t.type_param.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsImportType) -> ValidationResult<ImportType> {
        self.record(t);

        Ok(ImportType {
            span: t.span,
            arg: t.arg.clone(),
            qualifier: t.qualifier.clone(),
            type_params: try_opt!(t.type_args.validate_with(self)).map(Box::new),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeQueryExpr) -> ValidationResult<QueryExpr> {
        self.record(t);

        let span = t.span();

        Ok(match t {
            RTsTypeQueryExpr::TsEntityName(t) => t.clone().into(),
            RTsTypeQueryExpr::Import(i) => i.validate_with(self)?.into(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsRestType) -> ValidationResult<RestType> {
        self.record(t);

        Ok(RestType {
            span: t.span,
            ty: box t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsOptionalType) -> ValidationResult<OptionalType> {
        self.record(t);

        Ok(OptionalType {
            span: t.span,
            ty: box t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeQuery) -> ValidationResult<QueryType> {
        self.record(t);

        Ok(QueryType {
            span: t.span,
            expr: box t.expr_name.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypePredicate) -> ValidationResult<Predicate> {
        self.record(t);
        let mut ty = try_opt!(t.type_ann.validate_with(self)).map(Box::new);
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
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsIndexedAccessType) -> ValidationResult<IndexedAccessType> {
        self.record(t);

        Ok(IndexedAccessType {
            span: t.span,
            readonly: t.readonly,
            obj_type: box t.obj_type.validate_with(self)?,
            index_type: box t.index_type.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTplLitType) -> ValidationResult<TplType> {
        let types = t
            .types
            .iter()
            .map(|ty| ty.validate_with(self))
            .collect::<Result<_, _>>()?;

        Ok(TplType {
            span: t.span,
            quasis: t.quasis.clone(),
            types,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsType) -> ValidationResult {
        self.record(ty);

        let _ctx = error::context(format!("validate\nTsType: {:?}", ty));

        let ty = match ty {
            RTsType::TsThisType(this) => Type::This(this.clone()),
            RTsType::TsLitType(ty) => {
                match &ty.lit {
                    RTsLit::Tpl(t) => return Ok(t.validate_with(self)?.into()),
                    _ => {}
                }
                let mut ty = Type::Lit(ty.clone());
                self.prevent_generalize(&mut ty);
                ty
            }
            RTsType::TsKeywordType(ty) => Type::Keyword(ty.clone()),
            RTsType::TsTupleType(ty) => Type::Tuple(ty.validate_with(self)?),
            RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsUnionType(u)) => {
                Type::Union(u.validate_with(self)?).fixed()
            }
            RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsIntersectionType(i)) => {
                Type::Intersection(i.validate_with(self)?).fixed()
            }
            RTsType::TsArrayType(arr) => Type::Array(arr.validate_with(self)?),
            RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsFnType(f)) => {
                Type::Function(f.validate_with(self)?)
            }
            RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsConstructorType(c)) => {
                Type::Constructor(c.validate_with(self)?)
            }
            RTsType::TsTypeLit(lit) => Type::TypeLit(lit.validate_with(self)?),
            RTsType::TsConditionalType(cond) => Type::Conditional(cond.validate_with(self)?),
            RTsType::TsMappedType(ty) => Type::Mapped(ty.validate_with(self)?),
            RTsType::TsTypeOperator(ty) => Type::Operator(ty.validate_with(self)?),
            RTsType::TsParenthesizedType(ty) => return ty.validate_with(self),
            RTsType::TsTypeRef(ty) => ty.validate_with(self)?,
            RTsType::TsTypeQuery(ty) => Type::Query(ty.validate_with(self)?),
            RTsType::TsOptionalType(ty) => Type::Optional(ty.validate_with(self)?),
            RTsType::TsRestType(ty) => Type::Rest(ty.validate_with(self)?),
            RTsType::TsInferType(ty) => Type::Infer(ty.validate_with(self)?),
            RTsType::TsIndexedAccessType(ty) => Type::IndexedAccessType(ty.validate_with(self)?),
            RTsType::TsTypePredicate(ty) => Type::Predicate(ty.validate_with(self)?),
            RTsType::TsImportType(ty) => Type::Import(ty.validate_with(self)?),
        };

        ty.assert_valid();

        Ok(ty.cheap())
    }
}

impl Analyzer<'_, '_> {
    #[extra_validator]
    fn report_error_for_type_param_usages_in_static_members(&mut self, i: &RIdent) {
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

            match scope.kind() {
                ScopeKind::Method { is_static: true, .. } => true,
                _ => false,
            }
        });

        if static_method.is_some() {
            self.storage
                .report(Error::StaticMemberCannotUseTypeParamOfClass { span })
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

        if self.env.rule().no_implicit_any {
            let no_type_ann = !self.ctx.in_argument
                && !(self.ctx.in_return_arg && self.ctx.in_fn_with_return_type)
                && !self.ctx.in_assign_rhs;
            if no_type_ann || self.ctx.in_useless_expr_for_seq || self.ctx.check_for_implicit_any {
                self.storage
                    .report(Error::ImplicitAny { span: i.id.span }.context("default type"));
            }
        }
        let implicit_type_mark = self.marks().implicit_type_mark;

        if let Some(m) = &mut self.mutations {
            m.for_pats
                .entry(i.node_id)
                .or_default()
                .ty
                .fill_with(|| Type::any(DUMMY_SP.apply_mark(implicit_type_mark)));
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

                        _ => Type::any(DUMMY_SP),
                    };

                    TupleElement {
                        span,
                        // TODO?
                        label: None,
                        ty: box ty,
                    }
                })
                .collect(),
        });
        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(arr.node_id).or_default().ty.get_or_insert_with(|| ty);
        }
    }

    /// Handle implicit defaults.
    #[extra_validator]
    pub(crate) fn default_any_object(&mut self, obj: &RObjectPat) {
        if obj.type_ann.is_some() {
            return;
        }

        let implicit_type_mark = self.marks().implicit_type_mark;

        let mut members = Vec::with_capacity(obj.props.len());

        for props in &obj.props {
            match props {
                RObjectPatProp::KeyValue(p) => {
                    let key = p.key.validate_with(self)?;
                    match *p.value {
                        RPat::Array(_) | RPat::Object(_) => {
                            self.default_any_pat(&*p.value);
                        }
                        _ => {}
                    }
                    let ty = if let Some(value_node_id) = p.value.node_id() {
                        if let Some(m) = &mut self.mutations {
                            m.for_pats.entry(value_node_id).or_default().ty.take().map(Box::new)
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
                    }))
                }
                RObjectPatProp::Assign(RAssignPatProp { key, .. }) => {
                    let key = Key::Normal {
                        span: key.span,
                        sym: key.sym.clone(),
                    };
                    members.push(TypeElement::Property(PropertySignature {
                        span: DUMMY_SP,
                        accessibility: None,
                        readonly: false,
                        key,
                        optional: false,
                        params: vec![],
                        type_ann: None,
                        type_params: None,
                        metadata: Default::default(),
                    }))
                }
                RObjectPatProp::Rest(..) => {}
            }
        }

        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(obj.node_id).or_default().ty.fill_with(|| {
                Type::TypeLit(TypeLit {
                    span: DUMMY_SP.apply_mark(implicit_type_mark),
                    members,
                    metadata: Default::default(),
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
