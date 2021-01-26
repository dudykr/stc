use super::props::ComputedPropMode;
use super::Analyzer;
use super::Ctx;
use super::ScopeKind;
use crate::util::contains_infer_type;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use rnode::NodeId;
use rnode::VisitWith;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RAssignPatProp;
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
use stc_ts_ast_rnode::RTsMappedType;
use stc_ts_ast_rnode::RTsMethodSignature;
use stc_ts_ast_rnode::RTsOptionalType;
use stc_ts_ast_rnode::RTsParenthesizedType;
use stc_ts_ast_rnode::RTsPropertySignature;
use stc_ts_ast_rnode::RTsRestType;
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
use stc_ts_types::TsExpr;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeParam;
use stc_ts_types::TypeParamDecl;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::Union;
use stc_ts_utils::OptionExt;
use stc_ts_utils::PatExt;
use swc_atoms::js_word;
use swc_common::Spanned;
use swc_common::DUMMY_SP;

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

        let param = TypeParam {
            span: p.span,
            name: p.name.clone().into(),
            constraint: try_opt!(p.constraint.validate_with(self)),
            default: try_opt!(p.default.validate_with(self)),
        };
        self.register_type(param.name.clone().into(), box param.clone().into());

        Ok(param)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    #[inline]
    fn validate(&mut self, ann: &RTsTypeAnn) -> ValidationResult {
        self.record(ann);

        ann.type_ann.validate_with(self)
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
                    let alias = Alias { span, ty, type_params };
                    Ok(alias)
                },
            )?
        };
        self.register_type(d.id.clone().into(), box Type::Alias(alias.clone()));

        Ok(alias)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsInterfaceDecl) -> ValidationResult<Interface> {
        let ty: Interface = self.with_child(ScopeKind::Flow, Default::default(), |child| -> ValidationResult<_> {
            let mut ty = Interface {
                span: d.span,
                name: d.id.clone().into(),
                type_params: try_opt!(d.type_params.validate_with(&mut *child)),
                extends: d.extends.validate_with(child)?,
                body: d.body.validate_with(child)?,
            };
            child.prevent_expansion(&mut ty.body);

            child.register_type(d.id.clone().into(), box ty.clone().into());

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
        Ok(TypeLit {
            span: lit.span,
            members: lit.members.validate_with(self)?,
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
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsConstructSignatureDecl) -> ValidationResult<ConstructorSignature> {
        Ok(ConstructorSignature {
            span: d.span,
            params: d.params.validate_with(self)?,
            type_params: try_opt!(d.type_params.validate_with(self)),
            ret_ty: try_opt!(d.type_ann.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsCallSignatureDecl) -> ValidationResult<CallSignature> {
        Ok(CallSignature {
            span: d.span,
            params: d.params.validate_with(self)?,
            type_params: try_opt!(d.type_params.validate_with(self)),
            ret_ty: try_opt!(d.type_ann.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsMethodSignature) -> ValidationResult<MethodSignature> {
        self.with_child(ScopeKind::Fn, Default::default(), |child: &mut Analyzer| {
            let key = child.validate_key(&d.key, d.computed)?;

            if d.computed {
                child.validate_computed_prop_key(d.span(), &d.key);
            }

            Ok(MethodSignature {
                span: d.span,
                readonly: d.readonly,
                key,
                optional: d.optional,
                type_params: try_opt!(d.type_params.validate_with(child)),
                params: d.params.validate_with(child)?,
                ret_ty: try_opt!(d.type_ann.validate_with(child)),
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
            type_ann: try_opt!(d.type_ann.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RTsPropertySignature) -> ValidationResult<PropertySignature> {
        let key = self.validate_key(&d.key, d.computed)?;
        if !self.is_builtin && d.computed {
            RComputedPropName {
                node_id: NodeId::invalid(),
                span: d.key.span(),
                expr: d.key.clone(),
            }
            .visit_with(self);
        }

        Ok(PropertySignature {
            span: d.span,
            key,
            optional: d.optional,
            params: d.params.validate_with(self)?,
            readonly: d.readonly,
            type_ann: {
                // TODO: implicit any
                match d.type_ann.validate_with(self) {
                    Some(v) => match v {
                        Ok(v) => Some(v),
                        Err(e) => {
                            self.storage.report(e);
                            Some(Type::any(d.span))
                        }
                    },
                    None => Some(Type::any(d.span)),
                }
            },
            type_params: try_opt!(d.type_params.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RTsExprWithTypeArgs) -> ValidationResult<TsExpr> {
        Ok(TsExpr {
            span: e.span,
            expr: e.expr.clone(),
            type_args: try_opt!(e.type_args.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, i: &RTsTypeParamInstantiation) -> ValidationResult<TypeParamInstantiation> {
        let params = i.params.validate_with(self)?;

        Ok(TypeParamInstantiation { span: i.span, params })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTupleType) -> ValidationResult<Tuple> {
        Ok(Tuple {
            span: t.span,
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
            ty: node.ty.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsConditionalType) -> ValidationResult<Conditional> {
        Ok(Conditional {
            span: t.span,
            check_type: t.check_type.validate_with(self)?,
            extends_type: t.extends_type.validate_with(self)?,
            true_type: t.true_type.validate_with(self)?,
            false_type: t.false_type.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsMappedType) -> ValidationResult<Mapped> {
        Ok(Mapped {
            span: ty.span,
            readonly: ty.readonly,
            optional: ty.optional,
            name_type: try_opt!(ty.name_type.validate_with(self)),
            type_param: ty.type_param.validate_with(self)?,
            ty: try_opt!(ty.type_ann.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsTypeOperator) -> ValidationResult<Operator> {
        Ok(Operator {
            span: ty.span,
            op: ty.op,
            ty: ty.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsArrayType) -> ValidationResult<Array> {
        Ok(Array {
            span: node.span,
            elem_type: node.elem_type.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &RTsUnionType) -> ValidationResult<Union> {
        Ok(Union {
            span: u.span,
            types: u.types.validate_with(self)?,
        })
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
        let type_params = try_opt!(t.type_params.validate_with(self));

        for param in &t.params {
            self.default_any_param(&param);
        }

        let mut params: Vec<_> = t.params.validate_with(self)?;

        let ret_ty = t.type_ann.validate_with(self)?;

        Ok(stc_ts_types::Function {
            span: t.span,
            type_params,
            params,
            ret_ty,
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
            type_ann: t.type_ann.validate_with(self)?,
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

        let type_args = try_opt!(t.type_params.validate_with(self));
        let mut contains_infer = false;

        match t.type_name {
            RTsEntityName::Ident(ref i) if i.sym == js_word!("Array") && type_args.is_some() => {
                if type_args.as_ref().unwrap().params.len() == 1 {
                    return Ok(box Type::Array(Array {
                        span: t.span,
                        elem_type: type_args.unwrap().params.into_iter().next().unwrap(),
                    }));
                }
            }

            RTsEntityName::Ident(ref i) => {
                if let Some(types) = self.find_type(self.ctx.module_id, &i.into())? {
                    for ty in types {
                        if contains_infer_type(&ty) || self.contains_infer_type(&*ty) {
                            contains_infer = true;
                        }
                        // We use type param instead of reference type if possible.
                        match ty.normalize() {
                            Type::Param(..) => return Ok(box ty.into_owned()),
                            _ => {}
                        }
                    }
                }
            }

            _ => {}
        }

        if !self.is_builtin {
            slog::warn!(self.logger, "Crating a ref from TsTypeRef: {:?}", t.type_name);
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
            type_params: try_opt!(t.type_args.validate_with(self)),
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
            ty: t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsOptionalType) -> ValidationResult<OptionalType> {
        self.record(t);

        Ok(OptionalType {
            span: t.span,
            ty: t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypeQuery) -> ValidationResult<QueryType> {
        self.record(t);

        Ok(QueryType {
            span: t.span,
            expr: t.expr_name.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &RTsTypePredicate) -> ValidationResult<Predicate> {
        self.record(t);
        let mut ty = try_opt!(t.type_ann.validate_with(self));
        match &mut ty {
            Some(ty) => {
                self.prevent_expansion(&mut **ty);
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
            obj_type: t.obj_type.validate_with(self)?,
            index_type: t.index_type.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, ty: &RTsType) -> ValidationResult {
        self.record(ty);

        let ty = match ty {
            RTsType::TsThisType(this) => Type::This(this.clone()),
            RTsType::TsLitType(ty) => {
                let mut ty = Type::Lit(ty.clone());
                self.prevent_generalize(&mut ty);
                ty
            }
            RTsType::TsKeywordType(ty) => Type::Keyword(ty.clone()),
            RTsType::TsTupleType(ty) => Type::Tuple(ty.validate_with(self)?),
            RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsUnionType(u)) => {
                Type::Union(u.validate_with(self)?)
            }
            RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsIntersectionType(i)) => {
                Type::Intersection(i.validate_with(self)?)
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
            RTsType::TsTypeRef(ty) => *ty.validate_with(self)?,
            RTsType::TsTypeQuery(ty) => Type::Query(ty.validate_with(self)?),
            RTsType::TsOptionalType(ty) => Type::Optional(ty.validate_with(self)?),
            RTsType::TsRestType(ty) => Type::Rest(ty.validate_with(self)?),
            RTsType::TsInferType(ty) => Type::Infer(ty.validate_with(self)?),
            RTsType::TsIndexedAccessType(ty) => Type::IndexedAccessType(ty.validate_with(self)?),
            RTsType::TsTypePredicate(ty) => Type::Predicate(ty.validate_with(self)?),
            RTsType::TsImportType(ty) => Type::Import(ty.validate_with(self)?),
        };
        Ok(ty.cheap())
    }
}

impl Analyzer<'_, '_> {
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
    pub(crate) fn default_any_ident(&mut self, i: &RIdent) {
        if i.type_ann.is_some() {
            return;
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

        let ty = box Type::Tuple(Tuple {
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
                                Type::any(DUMMY_SP)
                            }
                        }
                        Some(RPat::Object(ref obj)) => {
                            self.default_any_object(obj);

                            if let Some(m) = &mut self.mutations {
                                m.for_pats.entry(obj.node_id).or_default().ty.take().unwrap()
                            } else {
                                Type::any(DUMMY_SP)
                            }
                        }

                        _ => Type::any(DUMMY_SP),
                    };

                    TupleElement {
                        span,
                        // TODO?
                        label: None,
                        ty,
                    }
                })
                .collect(),
        });
        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(arr.node_id).or_default().ty.fill_with(|| ty);
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
                            m.for_pats.entry(value_node_id).or_default().ty.take()
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    members.push(TypeElement::Property(PropertySignature {
                        span: DUMMY_SP,
                        readonly: false,
                        key,
                        optional: false,
                        params: vec![],
                        type_ann: ty,
                        type_params: None,
                    }))
                }
                RObjectPatProp::Assign(RAssignPatProp { key, .. }) => {
                    let key = Key::Normal {
                        span: key.span,
                        sym: key.sym.clone(),
                    };
                    members.push(TypeElement::Property(PropertySignature {
                        span: DUMMY_SP,
                        readonly: false,
                        key,
                        optional: false,
                        params: vec![],
                        type_ann: None,
                        type_params: None,
                    }))
                }
                RObjectPatProp::Rest(..) => {}
            }
        }

        if let Some(m) = &mut self.mutations {
            m.for_pats.entry(obj.node_id).or_default().ty.fill_with(|| {
                box Type::TypeLit(TypeLit {
                    span: DUMMY_SP.apply_mark(implicit_type_mark),
                    members,
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
