use super::props::ComputedPropMode;
use super::util::ResultExt;
use super::Analyzer;
use super::Ctx;
use super::ScopeKind;
use crate::util::contains_infer_type;
use crate::util::type_ext::TypeExt;
use crate::util::PatExt;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_types::Alias;
use stc_types::Array;
use stc_types::CallSignature;
use stc_types::Conditional;
use stc_types::ConstructorSignature;
use stc_types::Id;
use stc_types::ImportType;
use stc_types::IndexSignature;
use stc_types::IndexedAccessType;
use stc_types::InferType;
use stc_types::Interface;
use stc_types::Intersection;
use stc_types::Mapped;
use stc_types::MethodSignature;
use stc_types::Operator;
use stc_types::OptionalType;
use stc_types::Predicate;
use stc_types::PropertySignature;
use stc_types::QueryExpr;
use stc_types::QueryType;
use stc_types::Ref;
use stc_types::RestType;
use stc_types::TsExpr;
use stc_types::Tuple;
use stc_types::TupleElement;
use stc_types::Type;
use stc_types::TypeElement;
use stc_types::TypeLit;
use stc_types::TypeParam;
use stc_types::TypeParamDecl;
use stc_types::TypeParamInstantiation;
use stc_types::Union;
use swc_atoms::js_word;
use swc_common::Mark;
use swc_common::Spanned;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::prop_name_to_expr;
use swc_ecma_visit::VisitMutWith;

/// We analyze dependencies between type parameters, and fold parameter in
/// topological order.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &mut TsTypeParamDecl) -> ValidationResult<TypeParamDecl> {
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
                    box Type::Param(TypeParam {
                        span: param.span,
                        name,
                        constraint: None,
                        default: None,
                    }),
                )?;
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
    fn validate(&mut self, p: &mut TsTypeParam) -> ValidationResult<TypeParam> {
        self.record(p);

        let param = TypeParam {
            span: p.span,
            name: p.name.clone().into(),
            constraint: try_opt!(p.constraint.validate_with(self)),
            default: try_opt!(p.default.validate_with(self)),
        };
        self.register_type(param.name.clone().into(), box param.clone().into())?;

        Ok(param)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    #[inline]
    fn validate(&mut self, ann: &mut TsTypeAnn) -> ValidationResult {
        self.record(ann);

        ann.type_ann.validate_with(self)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &mut TsTypeAliasDecl) -> ValidationResult<Alias> {
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
                    let alias = Alias {
                        span,
                        ty,
                        type_params,
                    };
                    Ok(alias)
                },
            )?
        };
        self.register_type(d.id.clone().into(), box Type::Alias(alias.clone()))?;

        Ok(alias)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &mut TsInterfaceDecl) -> ValidationResult<Interface> {
        let ty: Interface = self.with_child(
            ScopeKind::Flow,
            Default::default(),
            |child| -> ValidationResult<_> {
                let mut ty = Interface {
                    span: d.span,
                    name: d.id.clone().into(),
                    type_params: try_opt!(d.type_params.validate_with(&mut *child)),
                    extends: d.extends.validate_with(child)?,
                    body: d.body.validate_with(child)?,
                };
                child.prevent_expansion(&mut ty.body);

                child
                    .register_type(d.id.clone().into(), box ty.clone().into())
                    .report(&mut child.storage);

                child.resolve_parent_interfaces(&mut d.extends);

                Ok(ty)
            },
        )?;

        // TODO: Recover
        self.register_type(d.id.clone().into(), Type::Interface(ty.clone()).cheap())?;

        Ok(ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut TsInterfaceBody) -> ValidationResult<Vec<TypeElement>> {
        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Interface,
            ..self.ctx
        };

        Ok(node.body.validate_with(&mut *self.with_ctx(ctx))?)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, lit: &mut TsTypeLit) -> ValidationResult<TypeLit> {
        Ok(TypeLit {
            span: lit.span,
            members: lit.members.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &mut TsTypeElement) -> ValidationResult<TypeElement> {
        Ok(match e {
            TsTypeElement::TsCallSignatureDecl(d) => TypeElement::Call(d.validate_with(self)?),
            TsTypeElement::TsConstructSignatureDecl(d) => {
                TypeElement::Constructor(d.validate_with(self)?)
            }
            TsTypeElement::TsIndexSignature(d) => TypeElement::Index(d.validate_with(self)?),
            TsTypeElement::TsMethodSignature(d) => TypeElement::Method(d.validate_with(self)?),
            TsTypeElement::TsPropertySignature(d) => TypeElement::Property(d.validate_with(self)?),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        d: &mut TsConstructSignatureDecl,
    ) -> ValidationResult<ConstructorSignature> {
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
    fn validate(&mut self, d: &mut TsCallSignatureDecl) -> ValidationResult<CallSignature> {
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
    fn validate(&mut self, d: &mut TsMethodSignature) -> ValidationResult<MethodSignature> {
        self.with_child(ScopeKind::Fn, Default::default(), |child: &mut Analyzer| {
            if d.computed {
                child.validate_computed_prop_key(d.span(), &mut d.key);
            }

            Ok(MethodSignature {
                span: d.span,
                readonly: d.readonly,
                key: d.key.clone(),
                computed: d.computed,
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
    fn validate(&mut self, d: &mut TsIndexSignature) -> ValidationResult<IndexSignature> {
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
    fn validate(&mut self, d: &mut TsPropertySignature) -> ValidationResult<PropertySignature> {
        if !self.is_builtin && d.computed {
            ComputedPropName {
                span: d.key.span(),
                expr: d.key.clone(),
            }
            .visit_mut_with(self);
        }

        Ok(PropertySignature {
            span: d.span,
            computed: d.computed,
            key: d.key.clone(),
            optional: d.optional,
            params: d.params.validate_with(self)?,
            readonly: d.readonly,
            type_ann: {
                // TODO: implicit any
                match d.type_ann.validate_with(self) {
                    Some(v) => match v {
                        Ok(v) => Some(v),
                        Err(e) => {
                            d.type_ann = Some(Type::any(d.span).into());

                            self.storage.report(e);
                            Some(Type::any(d.span))
                        }
                    },
                    None => {
                        d.type_ann = Some(Type::any(d.span).into());

                        Some(Type::any(d.span))
                    }
                }
            },
            type_params: try_opt!(d.type_params.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &mut TsExprWithTypeArgs) -> ValidationResult<TsExpr> {
        Ok(TsExpr {
            span: e.span,
            expr: e.expr.clone(),
            type_args: try_opt!(e.type_args.validate_with(self)),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        i: &mut TsTypeParamInstantiation,
    ) -> ValidationResult<TypeParamInstantiation> {
        let params = i.params.validate_with(self)?;

        Ok(TypeParamInstantiation {
            span: i.span,
            params,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsTupleType) -> ValidationResult<Tuple> {
        Ok(Tuple {
            span: t.span,
            elems: t.elem_types.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut TsTupleElement) -> ValidationResult<TupleElement> {
        Ok(TupleElement {
            span: node.span,
            label: node.label.clone(),
            ty: node.ty.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsConditionalType) -> ValidationResult<Conditional> {
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
    fn validate(&mut self, ty: &mut TsMappedType) -> ValidationResult<Mapped> {
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
    fn validate(&mut self, ty: &mut TsTypeOperator) -> ValidationResult<Operator> {
        Ok(Operator {
            span: ty.span,
            op: ty.op,
            ty: ty.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut TsArrayType) -> ValidationResult<Array> {
        Ok(Array {
            span: node.span,
            elem_type: node.elem_type.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &mut TsUnionType) -> ValidationResult<Union> {
        Ok(Union {
            span: u.span,
            types: u.types.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, u: &mut TsIntersectionType) -> ValidationResult<Intersection> {
        Ok(Intersection {
            span: u.span,
            types: u.types.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsFnType) -> ValidationResult<stc_types::Function> {
        let type_params = try_opt!(t.type_params.validate_with(self));

        for param in &mut t.params {
            default_any_param(self.marks().implicit_type_mark, param);
        }

        let mut params: Vec<_> = t.params.validate_with(self)?;

        let ret_ty = t.type_ann.validate_with(self)?;

        Ok(stc_types::Function {
            span: t.span,
            type_params,
            params,
            ret_ty,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsConstructorType) -> ValidationResult<stc_types::Constructor> {
        let type_params = try_opt!(t.type_params.validate_with(self));

        for param in &mut t.params {
            default_any_param(self.marks().implicit_type_mark, param);
        }

        Ok(stc_types::Constructor {
            span: t.span,
            type_params,
            params: t.params.validate_with(self)?,
            type_ann: t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsParenthesizedType) -> ValidationResult {
        t.type_ann.validate_with(self)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsTypeRef) -> ValidationResult {
        self.record(t);

        let type_args = try_opt!(t.type_params.validate_with(self));
        let mut contains_infer = false;

        match t.type_name {
            TsEntityName::Ident(ref i) if i.sym == js_word!("Array") && type_args.is_some() => {
                if type_args.as_ref().unwrap().params.len() == 1 {
                    return Ok(box Type::Array(Array {
                        span: t.span,
                        elem_type: type_args.unwrap().params.into_iter().next().unwrap(),
                    }));
                }
            }

            TsEntityName::Ident(ref i) => {
                if let Some(types) = self.find_type(self.ctx.module_id, &i.into())? {
                    for ty in types {
                        if contains_infer_type(&ty) || self.contains_infer_type(&*ty) {
                            contains_infer = true;
                        }
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
            slog::warn!(
                self.logger,
                "Crating a ref from TsTypeRef: {:?}",
                t.type_name
            );
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
    fn validate(&mut self, t: &mut TsInferType) -> ValidationResult<InferType> {
        self.record(t);

        Ok(InferType {
            span: t.span,
            type_param: t.type_param.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsImportType) -> ValidationResult<ImportType> {
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
    fn validate(&mut self, t: &mut TsTypeQueryExpr) -> ValidationResult<QueryExpr> {
        self.record(t);

        let span = t.span();

        Ok(match t {
            TsTypeQueryExpr::TsEntityName(t) => t.clone().into(),
            TsTypeQueryExpr::Import(i) => i.validate_with(self)?.into(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsRestType) -> ValidationResult<RestType> {
        self.record(t);

        Ok(RestType {
            span: t.span,
            ty: t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsOptionalType) -> ValidationResult<OptionalType> {
        self.record(t);

        Ok(OptionalType {
            span: t.span,
            ty: t.type_ann.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsTypeQuery) -> ValidationResult<QueryType> {
        self.record(t);

        Ok(QueryType {
            span: t.span,
            expr: t.expr_name.validate_with(self)?,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, t: &mut TsTypePredicate) -> ValidationResult<Predicate> {
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
    fn validate(&mut self, t: &mut TsIndexedAccessType) -> ValidationResult<IndexedAccessType> {
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
    fn validate(&mut self, ty: &mut TsType) -> ValidationResult {
        self.record(ty);

        let ty = match ty {
            TsType::TsThisType(this) => Type::This(this.clone()),
            TsType::TsLitType(ty) => {
                let mut ty = Type::Lit(ty.clone());
                self.prevent_generalize(&mut ty);
                ty
            }
            TsType::TsKeywordType(ty) => Type::Keyword(ty.clone()),
            TsType::TsTupleType(ty) => Type::Tuple(ty.validate_with(self)?),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(u)) => {
                Type::Union(u.validate_with(self)?)
            }
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(i)) => {
                Type::Intersection(i.validate_with(self)?)
            }
            TsType::TsArrayType(arr) => Type::Array(arr.validate_with(self)?),
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(f)) => {
                Type::Function(f.validate_with(self)?)
            }
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(c)) => {
                Type::Constructor(c.validate_with(self)?)
            }
            TsType::TsTypeLit(lit) => Type::TypeLit(lit.validate_with(self)?),
            TsType::TsConditionalType(cond) => Type::Conditional(cond.validate_with(self)?),
            TsType::TsMappedType(ty) => Type::Mapped(ty.validate_with(self)?),
            TsType::TsTypeOperator(ty) => Type::Operator(ty.validate_with(self)?),
            TsType::TsParenthesizedType(ty) => return ty.validate_with(self),
            TsType::TsTypeRef(ty) => *ty.validate_with(self)?,
            TsType::TsTypeQuery(ty) => Type::Query(ty.validate_with(self)?),
            TsType::TsOptionalType(ty) => Type::Optional(ty.validate_with(self)?),
            TsType::TsRestType(ty) => Type::Rest(ty.validate_with(self)?),
            TsType::TsInferType(ty) => Type::Infer(ty.validate_with(self)?),
            TsType::TsIndexedAccessType(ty) => Type::IndexedAccessType(ty.validate_with(self)?),
            TsType::TsTypePredicate(ty) => Type::Predicate(ty.validate_with(self)?),
            TsType::TsImportType(ty) => Type::Import(ty.validate_with(self)?),
        };
        Ok(ty.cheap())
    }
}

pub(crate) fn default_any_pat(implicit_type_mark: Mark, p: &mut Pat) {
    match p {
        Pat::Ident(i) => default_any_ident(implicit_type_mark, i),
        Pat::Array(arr) => default_any_array_pat(implicit_type_mark, arr),
        Pat::Object(obj) => default_any_object(implicit_type_mark, obj),
        _ => {}
    }
}

pub(crate) fn default_any_ident(implicit_type_mark: Mark, i: &mut Ident) {
    if i.type_ann.is_some() {
        return;
    }

    i.type_ann = Some(TsTypeAnn {
        span: DUMMY_SP.apply_mark(implicit_type_mark),
        type_ann: box TsType::TsKeywordType(TsKeywordType {
            span: DUMMY_SP.apply_mark(implicit_type_mark),
            kind: TsKeywordTypeKind::TsAnyKeyword,
        }),
    });
}

pub(crate) fn default_any_array_pat(implicit_type_mark: Mark, arr: &mut ArrayPat) {
    if arr.type_ann.is_some() {
        return;
    }
    let cnt = arr.elems.len();

    arr.type_ann = Some(TsTypeAnn {
        span: arr.span,
        type_ann: box TsType::TsTupleType(TsTupleType {
            span: DUMMY_SP,
            elem_types: arr
                .elems
                .iter_mut()
                .map(|elem| {
                    let span = elem.span();
                    // any
                    let ty = match elem {
                        Some(Pat::Array(ref mut arr)) => {
                            default_any_array_pat(implicit_type_mark, arr);
                            arr.type_ann.take().unwrap().type_ann
                        }
                        Some(Pat::Object(ref mut obj)) => {
                            default_any_object(implicit_type_mark, obj);
                            obj.type_ann.take().unwrap().type_ann
                        }

                        _ => box TsType::TsKeywordType(TsKeywordType {
                            span: DUMMY_SP,
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                        }),
                    };

                    TsTupleElement {
                        span,
                        // TODO?
                        label: None,
                        ty: *ty,
                    }
                })
                .collect(),
        }),
    })
}

pub(crate) fn default_any_object(implicit_type_mark: Mark, obj: &mut ObjectPat) {
    if obj.type_ann.is_some() {
        return;
    }

    let mut members = Vec::with_capacity(obj.props.len());

    for props in &mut obj.props {
        match props {
            ObjectPatProp::KeyValue(p) => {
                match *p.value {
                    Pat::Array(_) | Pat::Object(_) => {
                        default_any_pat(implicit_type_mark, &mut *p.value);
                    }
                    _ => {}
                }

                members.push(TsTypeElement::TsPropertySignature(TsPropertySignature {
                    span: DUMMY_SP,
                    readonly: false,
                    key: box prop_name_to_expr(p.key.clone()),
                    computed: false,
                    optional: false,
                    init: None,
                    params: vec![],
                    type_ann: {
                        let type_ann = p.value.get_mut_ty().take().cloned().map(|ty| TsTypeAnn {
                            span: DUMMY_SP,
                            type_ann: box ty,
                        });
                        p.value.set_ty(None);
                        type_ann
                    },
                    type_params: None,
                }))
            }
            ObjectPatProp::Assign(AssignPatProp { key, .. }) => {
                members.push(TsTypeElement::TsPropertySignature(TsPropertySignature {
                    span: DUMMY_SP,
                    readonly: false,
                    key: box Expr::Ident(key.clone()),
                    computed: false,
                    optional: false,
                    init: None,
                    params: vec![],
                    type_ann: None,
                    type_params: None,
                }))
            }
            ObjectPatProp::Rest(..) => {}
        }
    }

    obj.type_ann = Some(TsTypeAnn {
        span: DUMMY_SP.apply_mark(implicit_type_mark),
        type_ann: box TsType::TsTypeLit(TsTypeLit {
            span: DUMMY_SP,
            members,
        }),
    })
}

pub(crate) fn default_any_param(implicit_type_mark: Mark, p: &mut TsFnParam) {
    match p {
        TsFnParam::Ident(i) => default_any_ident(implicit_type_mark, i),
        TsFnParam::Array(arr) => default_any_array_pat(implicit_type_mark, arr),
        TsFnParam::Rest(rest) => {}
        TsFnParam::Object(obj) => default_any_object(implicit_type_mark, obj),
    }
}
