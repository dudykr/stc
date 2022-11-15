use rnode::NodeId;
use stc_ts_ast_rnode::{
    RArrayPat, RBindingIdent, RExpr, RIdent, RLit, RObjectPat, RPat, RPrivateName, RPropName, RRestPat, RTsArrayType, RTsCallSignatureDecl,
    RTsConditionalType, RTsConstructSignatureDecl, RTsConstructorType, RTsEntityName, RTsFnOrConstructorType, RTsFnParam, RTsFnType,
    RTsImportType, RTsIndexSignature, RTsIndexedAccessType, RTsInferType, RTsIntersectionType, RTsKeywordType, RTsLit, RTsLitType,
    RTsMappedType, RTsMethodSignature, RTsModuleName, RTsOptionalType, RTsParenthesizedType, RTsPropertySignature, RTsQualifiedName,
    RTsRestType, RTsThisType, RTsTplLitType, RTsTupleElement, RTsTupleType, RTsType, RTsTypeAnn, RTsTypeElement, RTsTypeLit,
    RTsTypeOperator, RTsTypeParam, RTsTypeParamDecl, RTsTypeParamInstantiation, RTsTypePredicate, RTsTypeQuery, RTsTypeQueryExpr,
    RTsTypeRef, RTsUnionOrIntersectionType, RTsUnionType,
};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

use crate::{
    Alias, Array, ClassDef, Conditional, Enum, EnumVariant, FnParam, Function, Id, ImportType, IndexedAccessType, InferType, Interface,
    Intersection, Intrinsic, Key, KeywordType, LitType, Operator, OptionalType, Predicate, QueryExpr, QueryType, Ref, RestType, StaticThis,
    Symbol, ThisType, TplType, Tuple, TupleElement, Type, TypeElement, TypeLit, TypeParam, TypeParamDecl, TypeParamInstantiation, Union,
};

impl From<Box<Type>> for RTsType {
    fn from(ty: Box<Type>) -> Self {
        (*ty).into()
    }
}

impl From<Type> for RTsType {
    fn from(t: Type) -> Self {
        match t {
            Type::Instance(t) => t.ty.into(),
            Type::This(t) => t.into(),
            Type::Lit(t) => t.into(),
            Type::Query(t) => t.into(),
            Type::Infer(t) => t.into(),
            Type::Import(t) => t.into(),
            Type::Predicate(t) => t.into(),
            Type::IndexedAccessType(t) => t.into(),
            Type::Ref(t) => t.into(),
            Type::TypeLit(t) => t.into(),
            Type::Keyword(t) => t.into(),
            Type::Conditional(t) => t.into(),
            Type::Tuple(t) => t.into(),
            Type::Array(t) => t.into(),
            Type::Union(t) => t.into(),
            Type::Intersection(t) => t.into(),
            Type::Function(t) => t.into(),
            Type::Constructor(t) => t.into(),
            Type::Operator(t) => t.into(),
            Type::Param(t) => t.into(),
            Type::EnumVariant(t) => t.into(),
            Type::Interface(t) => t.into(),
            Type::Enum(t) => t.into(),
            Type::Mapped(t) => t.into(),
            Type::Alias(t) => t.into(),
            Type::Namespace(..) => {
                unreachable!("TsNamespaceDecl should be handled before converting to RTsType")
            }
            Type::Module(t) => t.into(),
            Type::Class(t) => t.into(),
            Type::ClassDef(t) => t.into(),
            Type::Arc(t) => (*t.ty).clone().into(),
            Type::Optional(t) => t.into(),
            Type::Rest(t) => t.into(),
            Type::Symbol(t) => t.into(),
            Type::StaticThis(t) => t.into(),
            Type::Tpl(t) => t.into(),
            Type::Intrinsic(t) => t.into(),
        }
    }
}

impl From<KeywordType> for RTsType {
    fn from(ty: KeywordType) -> Self {
        RTsType::TsKeywordType(RTsKeywordType {
            span: ty.span,
            kind: ty.kind,
        })
    }
}

impl From<LitType> for RTsType {
    fn from(ty: LitType) -> Self {
        RTsType::TsLitType(RTsLitType {
            node_id: NodeId::invalid(),
            span: ty.span,
            lit: ty.lit,
        })
    }
}

impl From<ThisType> for RTsType {
    fn from(ty: ThisType) -> Self {
        RTsType::TsThisType(RTsThisType { span: ty.span })
    }
}

impl From<Symbol> for RTsType {
    fn from(ty: Symbol) -> Self {
        RTsType::TsKeywordType(RTsKeywordType {
            span: ty.span,
            kind: TsKeywordTypeKind::TsSymbolKeyword,
        })
    }
}

impl From<RestType> for RTsType {
    fn from(ty: RestType) -> Self {
        RTsType::from(RTsRestType::from(ty))
    }
}

impl From<RestType> for RTsRestType {
    fn from(ty: RestType) -> Self {
        RTsRestType {
            node_id: NodeId::invalid(),
            span: ty.span,
            type_ann: ty.ty.into(),
        }
    }
}

impl From<OptionalType> for RTsType {
    fn from(ty: OptionalType) -> Self {
        RTsType::TsOptionalType(RTsOptionalType::from(ty))
    }
}

impl From<OptionalType> for RTsOptionalType {
    fn from(ty: OptionalType) -> Self {
        RTsOptionalType {
            node_id: NodeId::invalid(),
            span: ty.span,
            type_ann: ty.ty.into(),
        }
    }
}

impl From<QueryType> for RTsType {
    fn from(t: QueryType) -> Self {
        RTsType::TsTypeQuery(RTsTypeQuery {
            node_id: NodeId::invalid(),
            span: t.span,
            expr_name: (*t.expr).into(),
            // TODO
            type_args: None,
        })
    }
}

impl From<QueryExpr> for RTsTypeQueryExpr {
    fn from(t: QueryExpr) -> Self {
        match t {
            QueryExpr::TsEntityName(t) => RTsTypeQueryExpr::TsEntityName(t),
            QueryExpr::Import(t) => RTsTypeQueryExpr::Import(t.into()),
        }
    }
}

impl From<ImportType> for RTsImportType {
    fn from(t: ImportType) -> Self {
        RTsImportType {
            node_id: NodeId::invalid(),
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.map(|v| *v).map(From::from).map(Box::new),
        }
    }
}

impl From<InferType> for RTsType {
    fn from(t: InferType) -> Self {
        RTsType::TsInferType(RTsInferType {
            node_id: NodeId::invalid(),
            span: t.span,
            type_param: t.type_param.into(),
        })
    }
}

impl From<ImportType> for RTsType {
    fn from(t: ImportType) -> Self {
        RTsType::TsImportType(RTsImportType {
            node_id: NodeId::invalid(),
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.map(|v| *v).map(From::from).map(Box::new),
        })
    }
}

impl From<Predicate> for RTsType {
    fn from(t: Predicate) -> Self {
        RTsType::TsTypePredicate(RTsTypePredicate {
            node_id: NodeId::invalid(),
            span: t.span,
            asserts: t.asserts,
            param_name: t.param_name,
            type_ann: t.ty.map(From::from).map(Box::new),
        })
    }
}

impl From<IndexedAccessType> for RTsType {
    fn from(t: IndexedAccessType) -> Self {
        let obj_type = match t.obj_type.normalize() {
            Type::Intersection(..) | Type::Union(..) => box RTsType::TsParenthesizedType(RTsParenthesizedType {
                node_id: NodeId::invalid(),
                span: t.obj_type.span(),
                type_ann: t.obj_type.into(),
            }),
            _ => t.obj_type.into(),
        };

        RTsType::TsIndexedAccessType(RTsIndexedAccessType {
            node_id: NodeId::invalid(),
            span: t.span,
            readonly: t.readonly,
            obj_type,
            index_type: t.index_type.into(),
        })
    }
}

impl From<Ref> for RTsType {
    fn from(t: Ref) -> Self {
        RTsType::TsTypeRef(RTsTypeRef {
            node_id: NodeId::invalid(),
            span: t.span,
            type_name: t.type_name,
            type_params: t.type_args.map(|v| *v).map(From::from).map(Box::new),
        })
    }
}

impl From<TypeLit> for RTsType {
    fn from(t: TypeLit) -> Self {
        RTsType::TsTypeLit(RTsTypeLit {
            node_id: NodeId::invalid(),
            span: t.span,
            members: t.members.into_iter().map(From::from).collect(),
        })
    }
}

impl From<Conditional> for RTsType {
    fn from(t: Conditional) -> Self {
        RTsType::TsConditionalType(RTsConditionalType {
            node_id: NodeId::invalid(),
            span: t.span,
            check_type: box (*t.check_type).into(),
            extends_type: box (*t.extends_type).into(),
            true_type: box (*t.true_type).into(),
            false_type: box (*t.false_type).into(),
        })
    }
}

impl From<Tuple> for RTsType {
    fn from(t: Tuple) -> Self {
        RTsType::TsTupleType(RTsTupleType {
            node_id: NodeId::invalid(),
            span: t.span,
            elem_types: t.elems.into_iter().map(From::from).collect(),
        })
    }
}

impl From<TupleElement> for RTsTupleElement {
    fn from(e: TupleElement) -> Self {
        RTsTupleElement {
            node_id: NodeId::invalid(),
            span: e.span,
            label: e.label,
            ty: e.ty.into(),
        }
    }
}

impl From<Array> for RTsType {
    fn from(t: Array) -> Self {
        match t.elem_type.normalize() {
            Type::Union(..) | Type::Intersection(..) => {
                return RTsType::TsArrayType(RTsArrayType {
                    node_id: NodeId::invalid(),
                    span: t.span,
                    elem_type: box RTsType::TsParenthesizedType(RTsParenthesizedType {
                        node_id: NodeId::invalid(),
                        span: t.elem_type.span(),
                        type_ann: box t.elem_type.into(),
                    }),
                })
            }
            _ => {}
        }
        RTsType::TsArrayType(RTsArrayType {
            node_id: NodeId::invalid(),
            span: t.span,
            elem_type: box (*t.elem_type).into(),
        })
    }
}

impl From<Union> for RTsType {
    fn from(t: Union) -> Self {
        RTsType::TsParenthesizedType(RTsParenthesizedType {
            node_id: NodeId::invalid(),
            span: t.span,
            type_ann: box RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsUnionType(RTsUnionType {
                node_id: NodeId::invalid(),
                span: t.span,
                types: t.types.into_iter().map(From::from).collect(),
            })),
        })
    }
}

impl From<Intersection> for RTsType {
    fn from(t: Intersection) -> Self {
        RTsType::TsParenthesizedType(RTsParenthesizedType {
            node_id: NodeId::invalid(),
            span: t.span,
            type_ann: box RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsIntersectionType(RTsIntersectionType {
                node_id: NodeId::invalid(),
                span: t.span,
                types: t.types.into_iter().map(From::from).collect(),
            })),
        })
    }
}

impl From<Function> for RTsType {
    fn from(t: Function) -> Self {
        RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsFnType(RTsFnType {
            node_id: NodeId::invalid(),
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from).map(Box::new),
            type_ann: box t.ret_ty.into(),
        }))
    }
}

impl From<super::Constructor> for RTsType {
    fn from(t: super::Constructor) -> Self {
        RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsConstructorType(RTsConstructorType {
            node_id: NodeId::invalid(),
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from).map(Box::new),
            type_ann: box t.type_ann.into(),
            is_abstract: t.is_abstract,
        }))
    }
}

impl From<TypeParamDecl> for RTsTypeParamDecl {
    fn from(t: TypeParamDecl) -> Self {
        RTsTypeParamDecl {
            node_id: NodeId::invalid(),
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
        }
    }
}

impl From<Type> for RTsTypeAnn {
    fn from(t: Type) -> Self {
        RTsTypeAnn {
            node_id: NodeId::invalid(),
            span: t.span(),
            type_ann: box t.into(),
        }
    }
}

impl From<Box<Type>> for RTsTypeAnn {
    fn from(t: Box<Type>) -> Self {
        (*t).into()
    }
}

impl From<Box<Type>> for Box<RTsType> {
    fn from(t: Box<Type>) -> Self {
        box (*t).into()
    }
}

impl From<TypeParam> for RTsTypeParam {
    fn from(t: TypeParam) -> Self {
        RTsTypeParam {
            node_id: NodeId::invalid(),
            span: t.span,
            // TODO
            name: t.name.into(),
            constraint: t.constraint.map(From::from),
            default: t.default.map(From::from),

            // TODO
            is_in: false,
            is_out: false,
        }
    }
}

impl From<Operator> for RTsType {
    fn from(t: Operator) -> Self {
        RTsTypeOperator {
            node_id: NodeId::invalid(),
            span: t.span,
            op: t.op,
            type_ann: t.ty.into(),
        }
        .into()
    }
}

impl From<TypeParam> for RTsType {
    fn from(t: TypeParam) -> Self {
        RTsType::TsTypeRef(RTsTypeRef {
            node_id: NodeId::invalid(),
            span: t.span,
            // TODO
            type_name: t.name.into(),
            type_params: None,
        })
    }
}

impl From<EnumVariant> for RTsType {
    fn from(t: EnumVariant) -> Self {
        match t.name {
            Some(name) => RTsType::TsTypeRef(RTsTypeRef {
                node_id: NodeId::invalid(),
                span: t.span,
                type_name: RTsEntityName::TsQualifiedName(box RTsQualifiedName {
                    node_id: NodeId::invalid(),
                    left: t.enum_name.into(),
                    right: RIdent::new(name, DUMMY_SP),
                }),
                type_params: None,
            }),
            None => RTsType::TsTypeRef(RTsTypeRef {
                node_id: NodeId::invalid(),
                span: t.span,
                type_name: RTsEntityName::Ident(t.enum_name.into()),
                type_params: None,
            }),
        }
    }
}

impl From<Enum> for RTsType {
    fn from(t: Enum) -> Self {
        RTsType::TsTypeRef(RTsTypeRef {
            node_id: NodeId::invalid(),
            span: t.span,
            // TODO
            type_name: t.id.into(),
            type_params: None,
        })
    }
}

impl From<Interface> for RTsType {
    fn from(t: Interface) -> Self {
        RTsTypeRef {
            node_id: NodeId::invalid(),
            span: t.span,
            // TODO
            type_name: RTsEntityName::Ident(t.name.into()),
            type_params: None,
        }
        .into()
    }
}

impl From<super::Mapped> for RTsType {
    fn from(t: super::Mapped) -> Self {
        RTsMappedType {
            node_id: NodeId::invalid(),
            span: t.span,

            name_type: t.name_type.map(From::from),

            readonly: t.readonly,
            type_param: t.type_param.into(),
            optional: t.optional.map(From::from),
            type_ann: t.ty.map(From::from),
        }
        .into()
    }
}

impl From<Alias> for RTsType {
    fn from(t: Alias) -> Self {
        (*t.ty).into()
    }
}

impl From<super::Module> for RTsType {
    fn from(m: super::Module) -> Self {
        RTsType::TsTypeRef(RTsTypeRef {
            node_id: NodeId::invalid(),
            span: m.span,
            type_params: None,
            type_name: RTsEntityName::Ident(match m.name {
                RTsModuleName::Ident(i) => i,
                RTsModuleName::Str(..) => {
                    unimplemented!("converting stringly-named module type to ast")
                }
            }),
        })
    }
}

impl From<TypeParamInstantiation> for RTsTypeParamInstantiation {
    fn from(t: TypeParamInstantiation) -> Self {
        RTsTypeParamInstantiation {
            node_id: NodeId::invalid(),
            span: t.span,
            params: t.params.into_iter().map(|v| box v.into()).collect(),
        }
    }
}

impl From<Operator> for RTsTypeOperator {
    fn from(t: Operator) -> Self {
        RTsTypeOperator {
            node_id: NodeId::invalid(),
            span: t.span,
            op: t.op,
            type_ann: t.ty.into(),
        }
    }
}

impl From<super::Class> for RTsType {
    fn from(t: super::Class) -> Self {
        RTsTypeRef {
            node_id: NodeId::invalid(),
            span: t.span,
            type_name: RTsEntityName::Ident(t.def.name.unwrap_or(Id::word("anonymous class".into())).into()),
            type_params: None,
        }
        .into()
    }
}

impl From<ClassDef> for RTsTypeQuery {
    fn from(c: ClassDef) -> Self {
        RTsTypeQuery {
            span: c.span,
            node_id: NodeId::invalid(),
            expr_name: RTsTypeQueryExpr::TsEntityName(RTsEntityName::Ident(
                c.name.unwrap_or_else(|| Id::word("anonymous class".into())).into(),
            )),
            // TODO
            type_args: None,
        }
    }
}

impl From<ClassDef> for RTsType {
    fn from(c: ClassDef) -> Self {
        RTsTypeQuery::from(c).into()
    }
}

impl From<super::ClassMember> for RTsTypeElement {
    fn from(m: super::ClassMember) -> Self {
        match m {
            super::ClassMember::Constructor(c) => RTsTypeElement::TsConstructSignatureDecl(RTsConstructSignatureDecl {
                node_id: NodeId::invalid(),
                span: c.span,
                params: c.params.into_iter().map(From::from).collect(),
                type_ann: c.ret_ty.map(From::from).map(Box::new),
                type_params: c.type_params.map(From::from).map(Box::new),
            }),
            super::ClassMember::Method(m) => RTsTypeElement::TsMethodSignature(RTsMethodSignature {
                node_id: NodeId::invalid(),
                span: m.span,
                readonly: false,
                computed: match &m.key {
                    Key::Computed(_) => true,
                    _ => false,
                },
                key: m.key.into_expr(),
                optional: m.is_optional,
                params: m.params.into_iter().map(From::from).collect(),
                type_ann: Some(box RTsTypeAnn {
                    node_id: NodeId::invalid(),
                    span: DUMMY_SP,
                    type_ann: box (*m.ret_ty).into(),
                }),
                type_params: m.type_params.map(From::from).map(Box::new),
            }),
            super::ClassMember::Property(p) => RTsTypeElement::TsPropertySignature(RTsPropertySignature {
                node_id: NodeId::invalid(),
                span: p.span,
                readonly: p.readonly,
                computed: p.key.is_computed(),
                key: p.key.into_expr(),
                optional: p.is_optional,
                init: None,
                params: vec![],
                type_ann: p.value.map(|ty| box RTsTypeAnn {
                    node_id: NodeId::invalid(),
                    span: DUMMY_SP,
                    type_ann: box ty.into(),
                }),
                type_params: None,
            }),
            super::ClassMember::IndexSignature(s) => RTsTypeElement::TsIndexSignature(RTsIndexSignature {
                node_id: NodeId::invalid(),
                span: s.span,
                params: s.params.into_iter().map(From::from).collect(),
                type_ann: s.type_ann.map(|ty| box RTsTypeAnn {
                    node_id: NodeId::invalid(),
                    span: DUMMY_SP,
                    type_ann: box ty.into(),
                }),
                readonly: s.readonly,
                is_static: s.is_static,
            }),
        }
    }
}

impl From<TypeElement> for RTsTypeElement {
    fn from(e: TypeElement) -> Self {
        match e {
            TypeElement::Call(e) => RTsTypeElement::TsCallSignatureDecl(RTsCallSignatureDecl {
                node_id: NodeId::invalid(),
                span: e.span,
                params: e.params.into_iter().map(|v| v.into()).collect(),
                type_ann: e.ret_ty.map(From::from).map(Box::new),
                type_params: e.type_params.map(From::from).map(Box::new),
            }),
            TypeElement::Constructor(e) => RTsTypeElement::TsConstructSignatureDecl(RTsConstructSignatureDecl {
                node_id: NodeId::invalid(),
                span: e.span,
                params: e.params.into_iter().map(|v| v.into()).collect(),
                type_ann: e.ret_ty.map(From::from).map(Box::new),
                type_params: e.type_params.map(From::from).map(Box::new),
            }),
            TypeElement::Property(e) => RTsTypeElement::TsPropertySignature(RTsPropertySignature {
                node_id: NodeId::invalid(),
                span: e.span,
                readonly: e.readonly,
                computed: e.key.is_computed(),
                key: e.key.into_expr(),
                optional: e.optional,
                init: None,
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.type_ann.map(From::from).map(Box::new),
                type_params: e.type_params.map(From::from).map(Box::new),
            }),
            TypeElement::Method(e) => RTsTypeElement::TsMethodSignature(RTsMethodSignature {
                node_id: NodeId::invalid(),
                span: e.span,
                readonly: e.readonly,
                computed: e.key.is_computed(),
                key: e.key.into_expr(),
                optional: e.optional,
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.ret_ty.map(From::from).map(Box::new),
                type_params: e.type_params.map(From::from).map(Box::new),
            }),
            TypeElement::Index(e) => RTsTypeElement::TsIndexSignature(RTsIndexSignature {
                node_id: NodeId::invalid(),
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.type_ann.map(From::from).map(Box::new),
                readonly: e.readonly,
                span: e.span,
                is_static: e.is_static,
            }),
        }
    }
}

impl From<FnParam> for RTsFnParam {
    fn from(t: FnParam) -> Self {
        let ty = t.ty;
        let type_ann = Some(RTsTypeAnn {
            node_id: NodeId::invalid(),
            span: DUMMY_SP,
            type_ann: box ty.into(),
        });

        fn convert(span: Span, type_ann: Option<RTsTypeAnn>, pat: RPat, optional: bool) -> RTsFnParam {
            match pat {
                RPat::Ident(i) => RTsFnParam::Ident(RBindingIdent {
                    node_id: NodeId::invalid(),
                    type_ann: type_ann.map(Box::new),
                    id: RIdent {
                        node_id: NodeId::invalid(),
                        span,
                        sym: i.id.sym,
                        optional,
                    },
                }),
                RPat::Array(a) => RTsFnParam::Array(RArrayPat {
                    node_id: NodeId::invalid(),
                    span,
                    type_ann: type_ann.map(Box::new),
                    elems: a.elems,
                    optional,
                }),
                RPat::Rest(r) => RTsFnParam::Rest(RRestPat {
                    node_id: NodeId::invalid(),
                    span,
                    dot3_token: r.dot3_token,
                    arg: r.arg,
                    type_ann: type_ann.map(Box::new),
                }),
                RPat::Object(o) => RTsFnParam::Object(RObjectPat {
                    node_id: NodeId::invalid(),
                    span,
                    type_ann: type_ann.map(Box::new),
                    props: o.props,
                    optional: o.optional,
                }),
                RPat::Assign(pat) => convert(span, type_ann, *pat.left, optional),
                _ => unimplemented!("From<super::FnParam> for TsFnParam with pat: {:?}", pat),
            }
        }

        convert(t.span, type_ann, t.pat, !t.required)
    }
}

impl From<Type> for Box<RTsType> {
    fn from(t: Type) -> Self {
        box t.into()
    }
}

impl From<StaticThis> for RTsThisType {
    fn from(t: StaticThis) -> Self {
        RTsThisType { span: t.span }
    }
}

impl From<StaticThis> for RTsType {
    fn from(t: StaticThis) -> Self {
        RTsType::TsThisType(t.into())
    }
}

impl From<TplType> for RTsType {
    fn from(t: TplType) -> Self {
        RTsType::TsLitType(t.into())
    }
}

impl From<TplType> for RTsLitType {
    fn from(t: TplType) -> Self {
        RTsLitType {
            node_id: NodeId::invalid(),
            span: t.span,
            lit: RTsLit::Tpl(t.into()),
        }
    }
}

impl From<TplType> for RTsTplLitType {
    fn from(t: TplType) -> Self {
        RTsTplLitType {
            node_id: NodeId::invalid(),
            span: t.span,
            quasis: t.quasis,
            types: t.types.into_iter().map(From::from).collect(),
        }
    }
}

impl From<Intrinsic> for RTsType {
    fn from(t: Intrinsic) -> Self {
        RTsType::TsKeywordType(t.into())
    }
}

impl From<Intrinsic> for RTsKeywordType {
    fn from(t: Intrinsic) -> Self {
        RTsKeywordType {
            span: t.span,
            kind: TsKeywordTypeKind::TsIntrinsicKeyword,
        }
    }
}

/// This function should be used for keys.
pub fn rprop_name_to_expr(p: RPropName) -> RExpr {
    match p {
        RPropName::Ident(i) => RExpr::Ident(i),
        RPropName::Str(s) => RExpr::Lit(RLit::Str(s)),
        RPropName::Num(n) => RExpr::Lit(RLit::Num(n)),
        RPropName::BigInt(b) => RExpr::Lit(RLit::BigInt(b)),
        RPropName::Computed(c) => *c.expr,
    }
}

impl Key {
    pub(crate) fn into_expr(self) -> Box<RExpr> {
        match self {
            Key::Computed(v) => v.expr,
            Key::Normal { span, sym } => box RExpr::Ident(RIdent {
                node_id: NodeId::invalid(),
                span,
                sym,
                optional: false,
            }),
            Key::Private(name) => box RExpr::PrivateName(RPrivateName {
                span: name.span,
                node_id: NodeId::invalid(),
                id: name.id.into(),
            }),
            Key::Num(n) => box RExpr::Lit(RLit::Num(n)),
            Key::BigInt(i) => box RExpr::Lit(RLit::BigInt(i)),
        }
    }
}
