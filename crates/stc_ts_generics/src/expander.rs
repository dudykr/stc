use std::sync::Arc;

use fxhash::{FxHashMap, FxHashSet};
use rnode::{Fold, FoldWith, Visit, VisitWith};
use stc_ts_ast_rnode::{RExpr, RInvalid, RTsEntityName, RTsLit};
use stc_ts_base_type_ops::{apply_mapped_flags, fix::Fix};
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_types::{
    Array, ArrayMetadata, CallSignature, ClassProperty, ComputedKey, ConstructorSignature,
    Function, Id, IndexSignature, IndexedAccessType, Key, KeywordType, KeywordTypeMetadata,
    LitType, Mapped, Method, MethodSignature, Operator, PropertySignature, Ref, Type, TypeElement,
    TypeLit, TypeParam,
};
use stc_utils::{cache::Freeze, debug_ctx, stack};
use swc_atoms::js_word;
use swc_common::{SourceMap, Spanned, DUMMY_SP};
use swc_ecma_ast::{TsKeywordTypeKind, TsTypeOperatorOp};
use tracing::{debug, error, info, warn};

use crate::{type_param::finder::TypeParamNameUsageFinder, ExpandGenericOpts};

#[derive(Debug)]
pub struct InferTypeResult {
    pub types: FxHashMap<Id, Type>,
    pub errored: FxHashSet<Id>,
}

/// This struct does not expands ref to other thpe. See Analyzer.expand to do
/// such operation.
pub struct GenericExpander<'a> {
    pub cm: Arc<SourceMap>,
    pub params: &'a FxHashMap<Id, Type>,
    /// Expand fully?
    pub fully: bool,
    pub dejavu: FxHashSet<Id>,
    pub opts: ExpandGenericOpts,
}

impl GenericExpander<'_> {
    fn fold_type(&mut self, mut ty: Type) -> Type {
        let span = ty.span();

        {
            let mut checker = GenericChecker {
                params: &self.params,
                found: false,
            };
            ty.visit_with(&mut checker);
            if !checker.found {
                return ty;
            }
        }

        match ty.n() {
            Type::StaticThis(..) | Type::Intrinsic(..) | Type::Symbol(..) => return ty,

            Type::Param(param) => {
                if !self.dejavu.contains(&param.name) {
                    if let Some(ty) = self.params.get(&param.name) {
                        info!(
                            "generic_expand: Expanding type parameter `{}` => {}",
                            param.name,
                            dump_type_as_string(&self.cm, &ty)
                        );

                        // If it's not self-referential, we fold it again.

                        self.dejavu.insert(param.name.clone());
                        let ty = ty.clone().fold_with(self);
                        self.dejavu.remove(&param.name);
                        return ty;
                    }
                }
            }

            _ => {}
        }

        ty.nm();

        match ty {
            Type::Ref(Ref {
                span,
                type_name: RTsEntityName::Ident(ref i),
                ref type_args,
                metadata,
                ..
            }) => {
                if i.sym == js_word!("Array") {
                    return Type::Array(Array {
                        span,
                        elem_type: box type_args
                            .as_ref()
                            .and_then(|args| args.params.iter().next().cloned())
                            .unwrap_or_else(|| {
                                Type::any(
                                    span,
                                    KeywordTypeMetadata {
                                        common: metadata.common,
                                        ..Default::default()
                                    },
                                )
                            }),
                        metadata: ArrayMetadata {
                            common: metadata.common,
                            ..Default::default()
                        },
                    });
                }

                if self.dejavu.contains(&i.into()) {
                    debug!("Dejavu: {}", i.sym);
                    return ty;
                }

                info!("Generic expander: Ref: {}", Id::from(i));

                return ty.fold_children_with(self);
            }

            Type::Instance(..) | Type::Ref(..) => return ty.fold_children_with(self),

            Type::Param(mut param) => {
                param = param.fold_with(self);

                if !self.dejavu.contains(&param.name) {
                    warn!(
                        "generic_expand: Failed to found type parameter instantiation: {}",
                        param.name,
                    );
                }

                return Type::Param(param);
            }

            // Alias returns other than self.
            Type::Alias(mut alias) => {
                alias = alias.fold_with(self);

                return *alias.ty;
            }

            Type::Interface(mut i) => {
                i = i.fold_with(self);

                return Type::Interface(i);
            }
            Type::Class(mut c) => {
                c = c.fold_with(self);

                return Type::Class(c);
            }

            Type::Conditional(mut c) => {
                c = c.fold_with(self);

                // if let Some(v) = self.analyzer.extends(&c.check_type, &c.extends_type) {
                //     return if v { *c.true_type } else { *c.false_type };
                // }

                return Type::Conditional(c);
            }

            Type::Mapped(mut m @ Mapped { ty: Some(..), .. }) => {
                m.make_clone_cheap();

                match &m.type_param.constraint {
                    Some(constraint) => {
                        match constraint.n() {
                            Type::Operator(
                                operator @ Operator {
                                    op: TsTypeOperatorOp::KeyOf,
                                    ..
                                },
                            ) => match operator.ty.n() {
                                Type::Param(param) if self.params.contains_key(&param.name) => {
                                    let ty = self.params.get(&param.name).unwrap();
                                    match ty.n() {
                                        Type::TypeLit(ty)
                                            if ty.members.iter().all(|element| match element {
                                                TypeElement::Property(..)
                                                | TypeElement::Method(..) => true,
                                                _ => false,
                                            }) =>
                                        {
                                            let mut members = vec![];

                                            for member in &ty.members {
                                                match member {
                                                TypeElement::Property(p) => {
                                                    members.push(TypeElement::Property(PropertySignature {
                                                        type_ann: m.ty.clone().fold_with(&mut MappedHandler {
                                                            key: &p.key,
                                                            param_name: &param.name,
                                                            prop_ty: &*p.type_ann.clone().unwrap_or_else(|| {
                                                                box Type::any(p.span, Default::default())
                                                            }),
                                                        }),
                                                        ..p.clone()
                                                    }))
                                                }
                                                TypeElement::Method(method) => {
                                                    members.push(TypeElement::Property(PropertySignature {
                                                        span: method.span,
                                                        accessibility: None,
                                                        readonly: method.readonly,
                                                        key: method.key.clone(),
                                                        optional: method.optional,
                                                        params: Default::default(),
                                                        type_ann: m.ty.clone().fold_with(&mut MappedHandler {
                                                            key: &method.key,
                                                            param_name: &param.name,
                                                            prop_ty: &Type::Function(Function {
                                                                span: method.span,
                                                                type_params: method.type_params.clone(),
                                                                params: method.params.clone(),
                                                                ret_ty: method.ret_ty.clone().unwrap_or_else(|| {
                                                                    box Type::any(method.span, Default::default())
                                                                }),
                                                                metadata: Default::default(),
                                                            }),
                                                        }),
                                                        type_params: Default::default(),
                                                        metadata: Default::default(),
                                                        accessor: Default::default(),
                                                    }))
                                                }
                                                _ => {}
                                            }
                                            }

                                            for member in &mut members {
                                                apply_mapped_flags(member, m.optional, m.readonly);
                                            }

                                            return Type::TypeLit(TypeLit {
                                                span: ty.span,
                                                members,
                                                metadata: ty.metadata,
                                            });
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                    _ => {}
                }

                // let m_ty = m.clone();

                // m.ty = m.ty.fold_with(&mut MappedHandler {
                //     analyzer: self.analyzer,
                //     ty: &m_ty,
                //     params: self.params,
                // });

                m = m.fold_with(self);

                match m.type_param.constraint {
                    Some(box Type::TypeLit(lit)) => {
                        let ty = m.ty.clone();

                        let mut members = lit
                            .members
                            .into_iter()
                            .map(|mut v| match v {
                                TypeElement::Property(ref mut p) => {
                                    p.type_ann = ty.clone();

                                    v
                                }
                                _ => todo!("type element other than property in a mapped type"),
                            })
                            .collect();

                        for member in &mut members {
                            apply_mapped_flags(member, m.optional, m.readonly);
                        }

                        return Type::TypeLit(TypeLit {
                            span,
                            members,
                            metadata: lit.metadata,
                        });
                    }

                    Some(box Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: box Type::Union(ref u),
                        ..
                    })) => {
                        error!("Union!");
                    }
                    _ => {}
                }

                // TODO(kdy1): PERF
                if let Some(ty) = &mut m.ty {
                    ty.nm();
                }
                m.ty = match m.ty {
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        mut obj_type,
                        index_type,
                        metadata,
                    })) => {
                        obj_type.nm();
                        // TODO(kdy1): PERF
                        match *obj_type {
                            Type::TypeLit(TypeLit {
                                span,
                                members,
                                metadata,
                                ..
                            }) if members.iter().all(|m| match m {
                                TypeElement::Property(_) => true,
                                TypeElement::Method(_) => true,
                                _ => false,
                            }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for m in members {
                                    match m {
                                        TypeElement::Property(..) | TypeElement::Method(..) => {
                                            new_members.push(m);
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                for member in &mut new_members {
                                    apply_mapped_flags(member, m.optional, m.readonly);
                                }

                                return Type::TypeLit(TypeLit {
                                    span,
                                    members: new_members,
                                    metadata,
                                });
                            }

                            _ => Some(box Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly,
                                obj_type,
                                index_type,
                                metadata,
                            })),
                        }
                    }
                    _ => m.ty,
                };

                if let Some(constraint) = &m.type_param.constraint {
                    match &**constraint {
                        Type::Operator(Operator {
                            span,
                            op: TsTypeOperatorOp::KeyOf,
                            ty,
                            ..
                        }) => match ty.n() {
                            Type::Keyword(..) if m.optional == None && m.readonly == None => {
                                return *ty.clone()
                            }
                            Type::TypeLit(TypeLit {
                                span,
                                members,
                                metadata: ty_metadata,
                                ..
                            }) if members.iter().all(|m| match m {
                                TypeElement::Property(_) => true,
                                TypeElement::Method(_) => true,
                                _ => false,
                            }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for member in members {
                                    match member {
                                        TypeElement::Method(method) => {
                                            new_members.push(TypeElement::Property(
                                                PropertySignature {
                                                    span: method.span,
                                                    accessibility: None,
                                                    readonly: method.readonly,
                                                    key: method.key.clone(),
                                                    optional: method.optional,
                                                    params: vec![],
                                                    type_ann: m.ty.clone().map(|v| v),
                                                    type_params: None,
                                                    metadata: Default::default(),
                                                    accessor: Default::default(),
                                                },
                                            ));
                                        }
                                        TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(ty) = &m.ty {
                                                p.type_ann = Some(ty.clone());
                                            }
                                            //
                                            new_members.push(TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                for member in &mut new_members {
                                    apply_mapped_flags(member, m.optional, m.readonly);
                                }

                                return Type::TypeLit(TypeLit {
                                    span: *span,
                                    members: new_members,
                                    metadata: *ty_metadata,
                                });
                            }
                            _ => {}
                        },

                        _ => {}
                    }
                }

                return Type::Mapped(m);
            }

            Type::This(..) | Type::Keyword(..) | Type::TypeLit(..) | Type::Lit(..) => {
                return ty.fold_children_with(self)
            }

            Type::IndexedAccessType(ty) => {
                let mut ty = ty.fold_with(self);
                ty.obj_type.fix();

                let key = match ty.index_type.n() {
                    Type::Lit(LitType {
                        lit: RTsLit::Str(s),
                        ..
                    }) => Some(Key::Normal {
                        span: s.span,
                        sym: s.value.clone(),
                    }),
                    Type::Lit(LitType {
                        lit: RTsLit::Number(v),
                        ..
                    }) => Some(Key::Num(v.clone())),

                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    })
                    | Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                        ..
                    }) => Some(Key::Computed(ComputedKey {
                        span: ty.index_type.span(),
                        expr: box RExpr::Invalid(RInvalid { span: DUMMY_SP }),
                        ty: ty.index_type.clone(),
                    })),
                    _ => None,
                };

                Type::IndexedAccessType(ty)
            }

            Type::Query(..)
            | Type::Operator(..)
            | Type::Tuple(..)
            | Type::Infer(..)
            | Type::Import(..)
            | Type::Predicate(..)
            | Type::Array(..)
            | Type::Union(..)
            | Type::Intersection(..)
            | Type::Function(..)
            | Type::Constructor(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Namespace(..)
            | Type::Module(..)
            | Type::ClassDef(..)
            | Type::Optional(..)
            | Type::Rest(..)
            | Type::Mapped(..)
            | Type::Tpl(..) => return ty.fold_children_with(self),

            _ => ty,
        }
    }
}

impl Fold<Type> for GenericExpander<'_> {
    fn fold(&mut self, ty: Type) -> Type {
        let _stack = match stack::track(ty.span()) {
            Ok(v) => v,
            _ => {
                error!(
                    "[generic/expander] Stack overflow: {}",
                    dump_type_as_string(&self.cm, &ty)
                );
                return ty;
            }
        };
        let _context = debug_ctx!(format!(
            "Expanding generics of {}",
            dump_type_as_string(&self.cm, &ty)
        ));

        let old_fully = self.fully;
        self.fully |= match ty.n() {
            Type::Mapped(..) => true,
            _ => false,
        };

        {
            let mut v = TypeParamNameUsageFinder::default();
            ty.visit_with(&mut v);
            let will_expand = v
                .params
                .iter()
                .any(|param| self.params.contains_key(&param));
            if !will_expand {
                return ty;
            }
        }

        let start = dump_type_as_string(&self.cm, &ty);
        let ty = self.fold_type(ty);
        let expanded = dump_type_as_string(&self.cm, &ty);

        debug!(op = "generic:expand", "Expanded {} => {}", start, expanded,);

        ty
    }
}

impl Fold<CallSignature> for GenericExpander<'_> {
    fn fold(&mut self, n: CallSignature) -> CallSignature {
        if self.opts.ignore_values {
            return n;
        }
        n.fold_children_with(self)
    }
}

impl Fold<ConstructorSignature> for GenericExpander<'_> {
    fn fold(&mut self, n: ConstructorSignature) -> ConstructorSignature {
        if self.opts.ignore_values {
            return n;
        }
        n.fold_children_with(self)
    }
}

impl Fold<PropertySignature> for GenericExpander<'_> {
    fn fold(&mut self, n: PropertySignature) -> PropertySignature {
        if self.opts.ignore_values {
            return PropertySignature {
                key: n.key.fold_with(self),
                ..n
            };
        }
        n.fold_children_with(self)
    }
}

impl Fold<MethodSignature> for GenericExpander<'_> {
    fn fold(&mut self, n: MethodSignature) -> MethodSignature {
        if self.opts.ignore_values {
            return MethodSignature {
                key: n.key.fold_with(self),
                ..n
            };
        }
        n.fold_children_with(self)
    }
}

impl Fold<IndexSignature> for GenericExpander<'_> {
    fn fold(&mut self, n: IndexSignature) -> IndexSignature {
        if self.opts.ignore_values {
            return IndexSignature {
                params: n.params.fold_with(self),
                ..n
            };
        }
        n.fold_children_with(self)
    }
}

impl Fold<ClassProperty> for GenericExpander<'_> {
    fn fold(&mut self, n: ClassProperty) -> ClassProperty {
        if self.opts.ignore_values {
            return ClassProperty {
                key: n.key.fold_with(self),
                ..n
            };
        }
        n.fold_children_with(self)
    }
}

impl Fold<Method> for GenericExpander<'_> {
    fn fold(&mut self, n: Method) -> Method {
        if self.opts.ignore_values {
            return Method {
                key: n.key.fold_with(self),
                ..n
            };
        }
        n.fold_children_with(self)
    }
}

/// This [Visit] implementation is used to check if one of the type parameters
/// are used.
struct GenericChecker<'a> {
    params: &'a FxHashMap<Id, Type>,
    found: bool,
}

impl Visit<Type> for GenericChecker<'_> {
    fn visit(&mut self, ty: &Type) {
        match ty.normalize() {
            Type::Param(p) => {
                if self.params.contains_key(&p.name) {
                    self.found = true;
                    return;
                }
            }
            _ => {}
        }

        ty.visit_children_with(self);
    }
}

struct MappedHandler<'d> {
    param_name: &'d Id,
    prop_ty: &'d Type,
    key: &'d Key,
}

impl Fold<Type> for MappedHandler<'_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        match ty.normalize() {
            Type::IndexedAccessType(ty) => match ty.obj_type.normalize() {
                Type::Param(TypeParam {
                    name: obj_param_name,
                    ..
                }) => match ty.index_type.normalize() {
                    Type::Param(TypeParam {
                        name: index_param_name,
                        constraint: Some(index_type_constraint),
                        ..
                    }) => match index_type_constraint.normalize() {
                        Type::Operator(
                            operator @ Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ..
                            },
                        ) => match operator.ty.normalize() {
                            Type::Param(constraint_param) => {
                                if *obj_param_name == constraint_param.name
                                    && *self.param_name == *obj_param_name
                                {
                                    return self.prop_ty.clone();
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }

        // TODO(kdy1): PERF
        ty.normalize_mut();
        ty = ty.fold_children_with(self);

        ty
    }
}
