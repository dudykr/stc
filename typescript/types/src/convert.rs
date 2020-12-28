use super::Alias;
use super::Array;
use super::ClassInstance;
use super::Conditional;
use super::Enum;
use super::EnumVariant;
use super::FnParam;
use super::Function;
use super::ImportType;
use super::IndexedAccessType;
use super::InferType;
use super::Interface;
use super::Intersection;
use super::Operator;
use super::Predicate;
use super::QueryExpr;
use super::QueryType;
use super::Ref;
use super::Tuple;
use super::TupleElement;
use super::Type;
use super::TypeElement;
use super::TypeLit;
use super::TypeParam;
use super::TypeParamDecl;
use super::TypeParamInstantiation;
use super::Union;
use crate::{OptionalType, RestType, StaticThis, Symbol};
use stc_ast_rnode::RArrayPat;
use stc_ast_rnode::RExpr;
use stc_ast_rnode::RIdent;
use stc_ast_rnode::RLit;
use stc_ast_rnode::RObjectPat;
use stc_ast_rnode::RPat;
use stc_ast_rnode::RPropName;
use stc_ast_rnode::RRestPat;
use stc_ast_rnode::RTsArrayType;
use stc_ast_rnode::RTsCallSignatureDecl;
use stc_ast_rnode::RTsConditionalType;
use stc_ast_rnode::RTsConstructSignatureDecl;
use stc_ast_rnode::RTsConstructorType;
use stc_ast_rnode::RTsEntityName;
use stc_ast_rnode::RTsFnOrConstructorType;
use stc_ast_rnode::RTsFnParam;
use stc_ast_rnode::RTsFnType;
use stc_ast_rnode::RTsImportType;
use stc_ast_rnode::RTsIndexSignature;
use stc_ast_rnode::RTsIndexedAccessType;
use stc_ast_rnode::RTsInferType;
use stc_ast_rnode::RTsIntersectionType;
use stc_ast_rnode::RTsKeywordType;
use stc_ast_rnode::RTsMappedType;
use stc_ast_rnode::RTsMethodSignature;
use stc_ast_rnode::RTsOptionalType;
use stc_ast_rnode::RTsParenthesizedType;
use stc_ast_rnode::RTsPropertySignature;
use stc_ast_rnode::RTsQualifiedName;
use stc_ast_rnode::RTsRestType;
use stc_ast_rnode::RTsThisType;
use stc_ast_rnode::RTsTupleElement;
use stc_ast_rnode::RTsTupleType;
use stc_ast_rnode::RTsType;
use stc_ast_rnode::RTsTypeAnn;
use stc_ast_rnode::RTsTypeElement;
use stc_ast_rnode::RTsTypeLit;
use stc_ast_rnode::RTsTypeOperator;
use stc_ast_rnode::RTsTypeParam;
use stc_ast_rnode::RTsTypeParamDecl;
use stc_ast_rnode::RTsTypeParamInstantiation;
use stc_ast_rnode::RTsTypePredicate;
use stc_ast_rnode::RTsTypeQuery;
use stc_ast_rnode::RTsTypeQueryExpr;
use stc_ast_rnode::RTsTypeRef;
use stc_ast_rnode::RTsUnionOrIntersectionType;
use stc_ast_rnode::RTsUnionType;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

impl From<Box<Type>> for RTsType {
    fn from(ty: Box<Type>) -> Self {
        (*ty).into()
    }
}

impl From<Type> for RTsType {
    fn from(t: Type) -> Self {
        match t {
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
            Type::ClassInstance(t) => t.into(),
            Type::Arc(t) => (*t.ty).clone().into(),
            Type::Optional(t) => t.into(),
            Type::Rest(t) => t.into(),
            Type::Symbol(t) => t.into(),
            Type::StaticThis(t) => t.into(),
        }
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
            span: ty.span,
            type_ann: ty.ty.into(),
        }
    }
}

impl From<QueryType> for RTsType {
    fn from(t: QueryType) -> Self {
        RTsType::TsTypeQuery(RTsTypeQuery {
            span: t.span,
            expr_name: t.expr.into(),
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
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.map(From::from),
        }
    }
}

impl From<InferType> for RTsType {
    fn from(t: InferType) -> Self {
        RTsType::TsInferType(RTsInferType {
            span: t.span,
            type_param: t.type_param.into(),
        })
    }
}

impl From<ImportType> for RTsType {
    fn from(t: ImportType) -> Self {
        RTsType::TsImportType(RTsImportType {
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.map(From::from),
        })
    }
}

impl From<Predicate> for RTsType {
    fn from(t: Predicate) -> Self {
        RTsType::TsTypePredicate(RTsTypePredicate {
            span: t.span,
            asserts: t.asserts,
            param_name: t.param_name,
            type_ann: t.ty.map(From::from),
        })
    }
}

impl From<IndexedAccessType> for RTsType {
    fn from(t: IndexedAccessType) -> Self {
        let obj_type = match t.obj_type.normalize() {
            Type::Intersection(..) | Type::Union(..) => {
                box RTsType::TsParenthesizedType(RTsParenthesizedType {
                    span: t.obj_type.span(),
                    type_ann: t.obj_type.into(),
                })
            }
            _ => t.obj_type.into(),
        };

        RTsType::TsIndexedAccessType(RTsIndexedAccessType {
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
            span: t.span,
            type_name: t.type_name,
            type_params: t.type_args.map(From::from),
        })
    }
}

impl From<TypeLit> for RTsType {
    fn from(t: TypeLit) -> Self {
        RTsType::TsTypeLit(RTsTypeLit {
            span: t.span,
            members: t.members.into_iter().map(From::from).collect(),
        })
    }
}

impl From<Conditional> for RTsType {
    fn from(t: Conditional) -> Self {
        RTsType::TsConditionalType(RTsConditionalType {
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
            span: t.span,
            elem_types: t.elems.into_iter().map(From::from).collect(),
        })
    }
}

impl From<TupleElement> for RTsTupleElement {
    fn from(e: TupleElement) -> Self {
        RTsTupleElement {
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
                    span: t.span,
                    elem_type: box RTsType::TsParenthesizedType(RTsParenthesizedType {
                        span: t.elem_type.span(),
                        type_ann: box t.elem_type.into(),
                    }),
                })
            }
            _ => {}
        }
        RTsType::TsArrayType(RTsArrayType {
            span: t.span,
            elem_type: box (*t.elem_type).into(),
        })
    }
}

impl From<Union> for RTsType {
    fn from(t: Union) -> Self {
        RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsUnionType(RTsUnionType {
            span: t.span,
            types: t.types.into_iter().map(From::from).collect(),
        }))
    }
}

impl From<Intersection> for RTsType {
    fn from(t: Intersection) -> Self {
        RTsType::TsUnionOrIntersectionType(RTsUnionOrIntersectionType::TsIntersectionType(
            RTsIntersectionType {
                span: t.span,
                types: t.types.into_iter().map(From::from).collect(),
            },
        ))
    }
}

impl From<Function> for RTsType {
    fn from(t: Function) -> Self {
        RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsFnType(RTsFnType {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from),
            type_ann: t.ret_ty.into(),
        }))
    }
}

impl From<super::Constructor> for RTsType {
    fn from(t: super::Constructor) -> Self {
        RTsType::TsFnOrConstructorType(RTsFnOrConstructorType::TsConstructorType(
            RTsConstructorType {
                span: t.span,
                params: t.params.into_iter().map(From::from).collect(),
                type_params: t.type_params.map(From::from),
                type_ann: t.type_ann.into(),
            },
        ))
    }
}

impl From<TypeParamDecl> for RTsTypeParamDecl {
    fn from(t: TypeParamDecl) -> Self {
        RTsTypeParamDecl {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
        }
    }
}

impl From<Type> for RTsTypeAnn {
    fn from(t: Type) -> Self {
        RTsTypeAnn {
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
            span: t.span,
            // TODO
            name: t.name.into(),
            constraint: t.constraint.map(From::from),
            default: t.default.map(From::from),
        }
    }
}

impl From<Operator> for RTsType {
    fn from(t: Operator) -> Self {
        RTsTypeOperator {
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
            span: t.span,
            // TODO
            type_name: t.name.into(),
            type_params: None,
        })
    }
}

impl From<EnumVariant> for RTsType {
    fn from(t: EnumVariant) -> Self {
        RTsType::TsTypeRef(RTsTypeRef {
            span: t.span,
            type_name: RTsEntityName::TsQualifiedName(box RTsQualifiedName {
                left: t.enum_name.into(),
                right: RIdent::new(t.name, DUMMY_SP),
            }),
            type_params: None,
        })
    }
}

impl From<Enum> for RTsType {
    fn from(t: Enum) -> Self {
        RTsType::TsTypeRef(RTsTypeRef {
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
    fn from(_: super::Module) -> Self {
        unreachable!("super::Module should be handled before converting to RTsType")
    }
}

impl From<TypeParamInstantiation> for RTsTypeParamInstantiation {
    fn from(t: TypeParamInstantiation) -> Self {
        RTsTypeParamInstantiation {
            span: t.span,
            params: t.params.into_iter().map(|v| box v.into()).collect(),
        }
    }
}

impl From<Operator> for RTsTypeOperator {
    fn from(t: Operator) -> Self {
        RTsTypeOperator {
            span: t.span,
            op: t.op,
            type_ann: t.ty.into(),
        }
    }
}

impl From<super::Class> for RTsType {
    fn from(t: super::Class) -> Self {
        // TODO: Handle generics
        RTsTypeLit {
            span: t.span,
            members: t.body.into_iter().map(From::from).collect(),
        }
        .into()
    }
}

impl From<ClassInstance> for RTsType {
    fn from(c: ClassInstance) -> Self {
        c.ty.into()
    }
}

impl From<super::ClassMember> for RTsTypeElement {
    fn from(m: super::ClassMember) -> Self {
        match m {
            super::ClassMember::Constructor(c) => {
                RTsTypeElement::TsConstructSignatureDecl(RTsConstructSignatureDecl {
                    span: c.span,
                    params: c.params.into_iter().map(From::from).collect(),
                    type_ann: c.ret_ty.map(From::from),
                    type_params: c.type_params.map(From::from),
                })
            }
            super::ClassMember::Method(m) => {
                RTsTypeElement::TsMethodSignature(RTsMethodSignature {
                    span: m.span,
                    readonly: false,
                    computed: match &m.key {
                        RPropName::Computed(_) => true,
                        _ => false,
                    },
                    key: box rprop_name_to_expr(m.key),
                    optional: m.is_optional,
                    params: m.params.into_iter().map(From::from).collect(),
                    type_ann: Some(RTsTypeAnn {
                        span: DUMMY_SP,
                        type_ann: box (*m.ret_ty).into(),
                    }),
                    type_params: m.type_params.map(From::from),
                })
            }
            super::ClassMember::Property(p) => {
                RTsTypeElement::TsPropertySignature(RTsPropertySignature {
                    span: p.span,
                    readonly: p.readonly,
                    key: p.key,
                    computed: p.computed,
                    optional: p.is_optional,
                    init: None,
                    params: vec![],
                    type_ann: p.value.map(|ty| RTsTypeAnn {
                        span: DUMMY_SP,
                        type_ann: box ty.into(),
                    }),
                    type_params: None,
                })
            }
            super::ClassMember::IndexSignature(s) => {
                RTsTypeElement::TsIndexSignature(RTsIndexSignature {
                    span: s.span,
                    params: s.params.into_iter().map(From::from).collect(),
                    type_ann: s.type_ann.map(|ty| RTsTypeAnn {
                        span: DUMMY_SP,
                        type_ann: box ty.into(),
                    }),
                    readonly: s.readonly,
                })
            }
        }
    }
}

impl From<TypeElement> for RTsTypeElement {
    fn from(e: TypeElement) -> Self {
        match e {
            TypeElement::Call(e) => RTsTypeElement::TsCallSignatureDecl(RTsCallSignatureDecl {
                span: e.span,
                params: e.params.into_iter().map(|v| v.into()).collect(),
                type_ann: e.ret_ty.map(From::from),
                type_params: e.type_params.map(From::from),
            }),
            TypeElement::Constructor(e) => {
                RTsTypeElement::TsConstructSignatureDecl(RTsConstructSignatureDecl {
                    span: e.span,
                    params: e.params.into_iter().map(|v| v.into()).collect(),
                    type_ann: e.ret_ty.map(From::from),
                    type_params: e.type_params.map(From::from),
                })
            }
            TypeElement::Property(e) => RTsTypeElement::TsPropertySignature(RTsPropertySignature {
                span: e.span,
                readonly: e.readonly,
                key: e.key,
                computed: e.computed,
                optional: e.optional,
                init: None,
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.type_ann.map(From::from),
                type_params: e.type_params.map(From::from),
            }),
            TypeElement::Method(e) => RTsTypeElement::TsMethodSignature(RTsMethodSignature {
                span: e.span,
                readonly: e.readonly,
                key: e.key,
                computed: e.computed,
                optional: e.optional,
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.ret_ty.map(From::from),
                type_params: e.type_params.map(From::from),
            }),
            TypeElement::Index(e) => RTsTypeElement::TsIndexSignature(RTsIndexSignature {
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.type_ann.map(From::from),
                readonly: e.readonly,
                span: e.span,
            }),
        }
    }
}

impl From<FnParam> for RTsFnParam {
    fn from(t: FnParam) -> Self {
        let ty = t.ty;
        let type_ann = Some(RTsTypeAnn {
            span: DUMMY_SP,
            type_ann: box ty.into(),
        });

        fn convert(
            span: Span,
            type_ann: Option<RTsTypeAnn>,
            pat: RPat,
            optional: bool,
        ) -> RTsFnParam {
            match pat {
                RPat::Ident(i) => RTsFnParam::Ident(RIdent {
                    span,
                    sym: i.sym,
                    type_ann: type_ann.into(),
                    optional,
                }),
                RPat::Array(a) => RTsFnParam::Array(RArrayPat {
                    span,
                    type_ann: type_ann.into(),
                    elems: a.elems,
                    optional,
                }),
                RPat::Rest(r) => RTsFnParam::Rest(RRestPat {
                    span,
                    dot3_token: r.dot3_token,
                    arg: r.arg,
                    type_ann: type_ann.into(),
                }),
                RPat::Object(o) => RTsFnParam::Object(RObjectPat {
                    span,
                    type_ann: type_ann.into(),
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

/// This function shoulod be used for keys.
pub fn rprop_name_to_expr(p: RPropName) -> RExpr {
    match p {
        RPropName::Ident(i) => RExpr::Ident(i),
        RPropName::Str(s) => RExpr::Lit(RLit::Str(s)),
        RPropName::Num(n) => RExpr::Lit(RLit::Num(n)),
        RPropName::BigInt(b) => RExpr::Lit(RLit::BigInt(b)),
        RPropName::Computed(c) => *c.expr,
    }
}
