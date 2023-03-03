use std::sync::Arc;

use fxhash::{FxHashMap, FxHashSet};
use rnode::{Fold, FoldWith, Visit, VisitWith};
use stc_ts_ast_rnode::{RExpr, RInvalid, RTsEntityName, RTsLit};
use stc_ts_base_type_ops::{apply_mapped_flags, fix::Fix};
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_types::{
    Array, ArrayMetadata, CallSignature, ClassProperty, ComputedKey, ConstructorSignature, Function, Id, Index, IndexSignature,
    IndexedAccessType, InferType, Key, KeywordType, KeywordTypeMetadata, LitType, Mapped, Method, MethodSignature, PropertySignature, Ref,
    Type, TypeElement, TypeLit, TypeParam,
};
use stc_utils::{cache::Freeze, stack};
use stc_visit::visit_cache;
use swc_atoms::js_word;
use swc_common::{SourceMap, Spanned, DUMMY_SP};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::{debug, error, info, warn};

use crate::{type_param::finder::TypeParamNameUsageFinder, ExpandGenericOpts};

#[derive(Debug)]
pub struct InferTypeResult {
    pub types: FxHashMap<Id, Type>,
    pub errored: FxHashSet<Id>,
}

/// This struct does not expands ref to other type. See Analyzer.expand to do
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

        match ty.normalize() {
            Type::StaticThis(..) | Type::Symbol(..) => return ty,

            Type::Param(param) | Type::Infer(InferType { type_param: param, .. }) => {
                if !self.dejavu.contains(&param.name) {
                    if let Some(ty) = self.params.get(&param.name) {
                        info!(
                            "generic_expand: Expanding type parameter `{}` => {}",
                            param.name,
                            dump_type_as_string(ty)
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

        ty.normalize_mut();

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
                        elem_type: box type_args.as_ref().and_then(|args| args.params.first().cloned()).unwrap_or_else(|| {
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
                        tracker: Default::default(),
                    });
                }

                if self.dejavu.contains(&i.into()) {
                    debug!("Dejavu: {}", i.sym);
                    return ty;
                }

                info!("Generic expander: Ref: {}", Id::from(i));

                ty.fold_children_with(self)
            }

            Type::Instance(..) | Type::Ref(..) | Type::StringMapping(..) => ty.fold_children_with(self),

            Type::Param(mut param) => {
                param = param.fold_with(self);

                if !self.dejavu.contains(&param.name) {
                    warn!("generic_expand: Failed to found type parameter instantiation: {}", param.name,);
                }

                Type::Param(param)
            }

            // Alias returns other than self.
            Type::Alias(mut alias) => {
                alias = alias.fold_with(self);

                *alias.ty
            }

            Type::Interface(mut i) => {
                i = i.fold_with(self);

                Type::Interface(i)
            }
            Type::Class(mut c) => {
                c = c.fold_with(self);

                Type::Class(c)
            }

            Type::Conditional(mut c) => {
                c = c.fold_with(self);

                // if let Some(v) = self.analyzer.extends(&c.check_type, &c.extends_type) {
                //     return if v { *c.true_type } else { *c.false_type };
                // }

                Type::Conditional(c)
            }

            Type::Mapped(mut m @ Mapped { ty: Some(..), .. }) => {
                m.freeze();

                if let Some(constraint) = &m.type_param.constraint {
                    if let Type::Index(operator) = constraint.normalize() {
                        match operator.ty.normalize() {
                            Type::Param(param) if self.params.contains_key(&param.name) => {
                                let ty = self.params.get(&param.name).unwrap();
                                match ty.normalize() {
                                    Type::TypeLit(ty)
                                        if ty
                                            .members
                                            .iter()
                                            .all(|element| matches!(element, TypeElement::Property(..) | TypeElement::Method(..))) =>
                                    {
                                        let mut members = vec![];

                                        for member in &ty.members {
                                            match member {
                                                TypeElement::Property(p) => members.push(TypeElement::Property(PropertySignature {
                                                    type_ann: m.ty.clone().fold_with(&mut MappedHandler {
                                                        key: &p.key,
                                                        param_name: &param.name,
                                                        prop_ty: &p
                                                            .type_ann
                                                            .clone()
                                                            .unwrap_or_else(|| box Type::any(p.span, Default::default())),
                                                    }),
                                                    ..p.clone()
                                                })),
                                                TypeElement::Method(method) => members.push(TypeElement::Property(PropertySignature {
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
                                                            ret_ty: method
                                                                .ret_ty
                                                                .clone()
                                                                .unwrap_or_else(|| box Type::any(method.span, Default::default())),
                                                            metadata: Default::default(),
                                                            tracker: Default::default(),
                                                        }),
                                                    }),
                                                    type_params: Default::default(),
                                                    metadata: Default::default(),
                                                    accessor: Default::default(),
                                                })),
                                                _ => {}
                                            }
                                        }

                                        for member in &mut members {
                                            apply_mapped_flags(member, m.optional, m.readonly);
                                        }

                                        let members = members.fold_with(self);
                                        return Type::TypeLit(TypeLit {
                                            span: ty.span,
                                            members,
                                            metadata: ty.metadata,
                                            tracker: Default::default(),
                                        });
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                }

                // let m_ty = m.clone();

                // m.ty = m.ty.fold_with(&mut MappedHandler {
                //     analyzer: self.analyzer,
                //     ty: &m_ty,
                //     params: self.params,
                // });

                m = m.fold_with(self);

                if let Some(Type::TypeLit(lit)) = m.type_param.constraint.as_deref().map(Type::normalize) {
                    let ty = m.ty.clone();

                    let mut members = lit
                        .members
                        .iter()
                        .cloned()
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
                        tracker: Default::default(),
                    });
                }

                // TODO(kdy1): PERF
                if let Some(ty) = &mut m.ty {
                    ty.normalize_mut();
                }
                m.ty = match m.ty {
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        mut obj_type,
                        index_type,
                        metadata,
                        ..
                    })) => {
                        obj_type.normalize_mut();
                        // TODO(kdy1): PERF
                        match *obj_type {
                            Type::TypeLit(TypeLit {
                                span, members, metadata, ..
                            }) if members
                                .iter()
                                .all(|m| matches!(m, TypeElement::Property(_) | TypeElement::Method(_))) =>
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
                                    tracker: Default::default(),
                                });
                            }

                            _ => Some(box Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly,
                                obj_type,
                                index_type,
                                metadata,
                                tracker: Default::default(),
                            })),
                        }
                    }
                    _ => m.ty,
                };

                if let Some(constraint) = &m.type_param.constraint {
                    if let Type::Index(Index { span, ty, .. }) = constraint.normalize() {
                        match ty.normalize() {
                            Type::Keyword(..) if m.optional.is_none() && m.readonly.is_none() => return *ty.clone(),
                            Type::TypeLit(TypeLit {
                                span,
                                members,
                                metadata: ty_metadata,
                                ..
                            }) if members
                                .iter()
                                .all(|m| matches!(m, TypeElement::Property(_) | TypeElement::Method(_))) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for member in members {
                                    match member {
                                        TypeElement::Method(method) => {
                                            new_members.push(TypeElement::Property(PropertySignature {
                                                span: method.span,
                                                accessibility: None,
                                                readonly: method.readonly,
                                                key: method.key.clone(),
                                                optional: method.optional,
                                                params: vec![],
                                                type_ann: m.ty.clone(),
                                                type_params: None,
                                                metadata: Default::default(),
                                                accessor: Default::default(),
                                            }));
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
                                    tracker: Default::default(),
                                });
                            }
                            _ => {}
                        }
                    }
                }

                Type::Mapped(m)
            }

            Type::This(..) | Type::Keyword(..) | Type::TypeLit(..) | Type::Lit(..) => ty.fold_children_with(self),

            Type::IndexedAccessType(ty) => {
                let mut ty = ty.fold_with(self);
                ty.obj_type.fix();

                let key = match ty.index_type.normalize() {
                    Type::Lit(LitType { lit: RTsLit::Str(s), .. }) => Some(Key::Normal {
                        span: s.span,
                        sym: s.value.clone(),
                    }),
                    Type::Lit(LitType {
                        lit: RTsLit::Number(v), ..
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
            | Type::Index(..)
            | Type::Readonly(..)
            | Type::Unique(..)
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
            | Type::Tpl(..) => ty.fold_children_with(self),

            _ => ty,
        }
    }
}

visit_cache!(pub static GENERIC_CACHE: bool);

impl Fold<Type> for GenericExpander<'_> {
    fn fold(&mut self, ty: Type) -> Type {
        let _stack = match stack::track(ty.span()) {
            Ok(v) => v,
            _ => {
                error!("[generic/expander] Stack overflow: {}", dump_type_as_string(&ty));
                return ty;
            }
        };

        let old_fully = self.fully;
        self.fully |= matches!(ty.normalize(), Type::Mapped(..));

        {
            // TODO(kdy1): Remove this block, after fixing a regression of a mapped types.
            let mut v = TypeParamNameUsageFinder::default();
            ty.visit_with(&mut v);
            let will_expand = v.params.iter().any(|param| self.params.contains_key(param));
            if !will_expand {
                return ty;
            }
        }

        {
            let mut checker = GenericChecker {
                params: self.params,
                found: false,
            };
            ty.visit_with(&mut checker);
            if !checker.found {
                return ty;
            }
        }

        let start = dump_type_as_string(&ty);
        let ty = self.fold_type(ty).fixed();
        ty.assert_valid();
        let expanded = dump_type_as_string(&ty);

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
        if self.found {
            return;
        }

        let key = ty as *const Type as *const ();

        if let Some(v) = GENERIC_CACHE.get_copied(key) {
            self.found |= v;
            return;
        }

        ty.visit_children_with(self);

        GENERIC_CACHE.insert(key, self.found);
    }
}

impl Visit<TypeParam> for GenericChecker<'_> {
    fn visit(&mut self, ty: &TypeParam) {
        if self.params.contains_key(&ty.name) {
            self.found = true;
            return;
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
        if let Type::IndexedAccessType(ty) = ty.normalize() {
            if let Type::Param(TypeParam { name: obj_param_name, .. }) = ty.obj_type.normalize() {
                if let Type::Param(TypeParam {
                    name: index_param_name,
                    constraint: Some(index_type_constraint),
                    ..
                }) = ty.index_type.normalize()
                {
                    if let Type::Index(operator) = index_type_constraint.normalize() {
                        if let Type::Param(constraint_param) = operator.ty.normalize() {
                            if *obj_param_name == constraint_param.name && *self.param_name == *obj_param_name {
                                return self.prop_ty.clone();
                            }
                        }
                    }
                }
            }
        }

        // TODO(kdy1): PERF
        ty.normalize_mut();
        ty = ty.fold_children_with(self);

        ty
    }
}
