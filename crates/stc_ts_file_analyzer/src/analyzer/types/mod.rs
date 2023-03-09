use std::{borrow::Cow, fmt::Debug};

use fxhash::FxHashMap;
use itertools::Itertools;
use rnode::{NodeId, VisitWith};
use stc_ts_ast_rnode::{RBindingIdent, RExpr, RIdent, RInvalid, RNumber, RPat, RStr, RTsEntityName, RTsEnumMemberId, RTsLit};
use stc_ts_base_type_ops::{
    bindings::{collect_bindings, BindingCollector, KnownTypeVisitor},
    is_str_lit_or_union,
};
use stc_ts_errors::{
    debug::{dump_type_as_string, force_dump_type_as_string, print_backtrace},
    DebugExt, ErrorKind,
};
use stc_ts_generics::ExpandGenericOpts;
use stc_ts_type_ops::{tuple_normalization::normalize_tuples, Fix};
use stc_ts_types::{
    name::Name, Accessor, Array, Class, ClassDef, ClassMember, ClassMetadata, ComputedKey, Conditional, ConditionalMetadata,
    ConstructorSignature, EnumVariant, FnParam, Id, IdCtx, Index, IndexSignature, IndexedAccessType, Instance, InstanceMetadata,
    Intersection, IntrinsicKind, Key, KeywordType, KeywordTypeMetadata, LitType, LitTypeMetadata, MethodSignature, PropertySignature,
    QueryExpr, QueryType, Ref, StringMapping, ThisType, ThisTypeMetadata, TplElem, TplType, Type, TypeElement, TypeLit, TypeLitMetadata,
    TypeParam, TypeParamInstantiation, Union,
};
use stc_ts_utils::run;
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    dev_span,
    ext::{SpanExt, TypeVecExt},
    stack,
};
use swc_atoms::{js_word, Atom, JsWord};
use swc_common::{util::take::Take, Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::{debug, error};

use super::expr::AccessPropertyOpts;
use crate::{
    analyzer::{expr::TypeOfMode, generic::ExtendsOpts, scope::ExpandOpts, Analyzer, Ctx},
    type_facts::TypeFacts,
    VResult,
};

mod conditional;
mod index_signature;
mod keyof;
mod mapped;
mod narrowing;
mod type_param;

/// All fields defaults to false.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct NormalizeTypeOpts {
    pub preserve_mapped: bool,
    pub preserve_typeof: bool,
    /// Should we normalize keywords as interfaces?
    pub normalize_keywords: bool,

    /// Should we preserve `typeof globalThis`?
    pub preserve_global_this: bool,

    /// Should we preserve [Type::Intersection]?
    pub preserve_intersection: bool,

    /// Should we preserve [Type::Union]?
    pub preserve_union: bool,

    /// If `true`, `true | false` becomes `boolean` and `E.**` of enum will be
    /// merged as `E` if E and all variants are selected.
    pub merge_union_elements: bool,

    //// If `true`, we will not expand generics.
    pub process_only_key: bool,

    /// If `true`, [Type::Enum] will be expanded as a union of `string` and
    /// [Type::EnumVariant].
    pub expand_enum_def: bool,

    /// IF true, [Type::EnumVariant] with the variant name will be expanded as a
    /// literal.
    pub expand_enum_variant: bool,

    pub preserve_keyof: bool,
}

impl Analyzer<'_, '_> {
    /// This methods normalizes a type.
    ///
    /// # Changed types.
    ///
    ///  - [Type::Ref]
    ///  - [Type::Mapped]
    ///  - [Type::Alias]

    ///
    /// # Span
    ///
    /// If `span` is provided, it will be used for types **created** by the
    /// method. Otherwise the span of the original type is used.
    pub(crate) fn normalize<'a>(&mut self, span: Option<Span>, mut ty: Cow<'a, Type>, opts: NormalizeTypeOpts) -> VResult<Cow<'a, Type>> {
        let _tracing = if cfg!(debug_assertions) {
            let ty = force_dump_type_as_string(&ty);

            Some(dev_span!("normalize", ty = tracing::field::display(&ty)))
        } else {
            None
        };

        ty.assert_valid();

        let actual_span = span.unwrap_or_else(|| ty.span());
        if !self.config.is_builtin {
            debug_assert!(!actual_span.is_dummy(), "Cannot normalize a type with dummy span\n{:?}", ty);
        }

        match ty.normalize() {
            Type::Lit(..)
            | Type::TypeLit(..)
            | Type::Interface(..)
            | Type::Class(..)
            | Type::ClassDef(..)
            | Type::Function(..)
            | Type::Constructor(..)
            | Type::EnumVariant(..)
            | Type::Param(_)
            | Type::Module(_) => return Ok(ty),
            _ => {}
        }

        #[cfg(debug_assertions)]
        let input = dump_type_as_string(&ty);

        let res = (|| {
            let _stack = match stack::track(actual_span) {
                Ok(v) => v,
                Err(err) => {
                    // print_backtrace();
                    return Err(err.into());
                }
            };

            if matches!(&*ty, Type::Arc(..)) {
                let ty = self.normalize(span, Cow::Borrowed(ty.normalize()), opts)?.into_owned();

                return Ok(Cow::Owned(ty));
            }

            if matches!(
                ty.normalize(),
                Type::Conditional(..) | Type::Array(..) | Type::IndexedAccessType(..) | Type::Mapped(..) | Type::Union(..)
            ) {
                ty.freeze();
            }

            {
                match ty.normalize() {
                    Type::Ref(_) => {
                        let mut new_ty = self
                            .expand_top_ref(
                                actual_span,
                                Cow::Borrowed(&ty),
                                ExpandOpts {
                                    generic: ExpandGenericOpts {
                                        ignore_values: opts.process_only_key,
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                            )
                            .context("tried to expand a ref type as a part of normalization")?;

                        // We are declaring, and expand_top_ref returned Type::Ref
                        if new_ty.type_eq(&*ty) {
                            return Ok(ty);
                        }

                        new_ty.assert_valid();

                        new_ty.freeze();

                        return Ok(Cow::Owned(self.normalize(span, new_ty, opts)?.into_owned()));
                    }

                    Type::Keyword(k) => {
                        if opts.normalize_keywords {
                            let name = match k.kind {
                                TsKeywordTypeKind::TsNumberKeyword => Some(js_word!("Number")),
                                TsKeywordTypeKind::TsObjectKeyword => Some(js_word!("Object")),
                                TsKeywordTypeKind::TsBooleanKeyword => Some(js_word!("Boolean")),
                                TsKeywordTypeKind::TsStringKeyword => Some(js_word!("String")),
                                TsKeywordTypeKind::TsSymbolKeyword => Some(js_word!("Symbol")),
                                _ => None,
                            };

                            if let Some(name) = name {
                                let global = self.env.get_global_type(actual_span, &name)?;

                                global.assert_valid();

                                return Ok(Cow::Owned(global));
                            }
                        }
                    }

                    Type::Mapped(m) => {
                        if !opts.preserve_mapped {
                            let ty = self.expand_mapped(actual_span, m)?;
                            if let Some(ty) = ty {
                                return Ok(Cow::Owned(
                                    self.normalize(span, Cow::Owned(ty), opts)
                                        .context("tried to expand a mapped type as a part of normalization")?
                                        .into_owned(),
                                ));
                            }
                        }
                    }

                    Type::Alias(a) => {
                        // TODO(kdy1): Optimize
                        return Ok(Cow::Owned(self.normalize(span, Cow::Borrowed(&a.ty), opts)?.into_owned()));
                    }

                    Type::StringMapping(i) => {
                        let ty = self
                            .expand_intrinsic_types(actual_span, i)
                            .context("tried to expand intrinsic type as a part of normalization")?;

                        return Ok(Cow::Owned(ty));
                    }

                    // Leaf types.
                    Type::Array(arr) => {
                        // TODO(kdy1): Optimize
                        let elem_type = box self
                            .normalize(span, Cow::Borrowed(&arr.elem_type), opts)
                            .context("tried to normalize the type of the element of an array type")?
                            .into_owned();

                        elem_type.assert_valid();

                        return Ok(Cow::Owned(Type::Array(Array {
                            span: arr.span,
                            elem_type,
                            metadata: arr.metadata,
                            tracker: Default::default(),
                        })));
                    }

                    // Not normalizable.
                    Type::Infer(_) | Type::StaticThis(_) | Type::This(_) => {}

                    Type::Union(ty) => {
                        if opts.merge_union_elements {
                            let mut enum_counts = FxHashMap::<_, i32>::default();
                            let mut new_types = vec![];

                            for elem in ty.types.iter() {
                                // TODO(kdy1): Cache the result of normalize

                                let elem = self
                                    .normalize(
                                        span,
                                        Cow::Borrowed(elem),
                                        NormalizeTypeOpts {
                                            preserve_mapped: true,
                                            preserve_global_this: true,
                                            preserve_intersection: true,
                                            preserve_union: true,
                                            ..Default::default()
                                        },
                                    )?
                                    .into_owned();

                                if let Type::EnumVariant(EnumVariant { name: Some(..), def, .. }) = elem.normalize() {
                                    *enum_counts.entry(def.id.clone()).or_insert(0) += 1;
                                }
                            }

                            for (enum_name, cnt) in enum_counts.iter_mut() {
                                if let Some(types) = self.find_type(enum_name)? {
                                    for ty in types {
                                        if let Type::Enum(e) = ty.normalize() {
                                            *cnt -= e.members.len() as i32;

                                            if *cnt == 0 {
                                                new_types.push(Type::EnumVariant(EnumVariant {
                                                    span: e.span,
                                                    def: e.cheap_clone(),
                                                    name: None,
                                                    metadata: Default::default(),
                                                    tracker: Default::default(),
                                                }));
                                            }
                                        }
                                    }
                                }
                            }

                            // If the count is 0, it means all variants exist in
                            // the union.
                            if enum_counts.values().any(|count| *count == 0) {
                                for elem in ty.types.iter() {
                                    let elem = self
                                        .normalize(
                                            span,
                                            Cow::Borrowed(elem),
                                            NormalizeTypeOpts {
                                                preserve_mapped: true,
                                                preserve_global_this: true,
                                                preserve_intersection: true,
                                                preserve_union: true,
                                                ..Default::default()
                                            },
                                        )?
                                        .into_owned();

                                    if let Type::EnumVariant(EnumVariant {
                                        span, name: Some(..), def, ..
                                    }) = elem.normalize()
                                    {
                                        if let Some(0) = enum_counts.get(&def.id) {
                                            // This enum is going to be added to union directly, so we skip the variants.
                                            continue;
                                        }
                                    }

                                    new_types.push(elem);
                                }

                                return self.normalize(
                                    span,
                                    Cow::Owned(Type::new_union(actual_span, new_types)),
                                    NormalizeTypeOpts {
                                        merge_union_elements: false,
                                        ..opts
                                    },
                                );
                            }
                        }

                        if !opts.preserve_union {
                            let mut types = vec![];

                            for ty in ty.types.iter() {
                                let mut ty = self
                                    .normalize(span, Cow::Borrowed(ty), opts)
                                    .context("tried to normalize an element of a union type")?;
                                ty.freeze();
                                let mut ty = ty.into_owned();

                                if let Some(u) = ty.as_union_type_mut() {
                                    types.append(&mut u.types);
                                } else {
                                    types.push(ty);
                                }
                            }

                            types.dedup_type();
                            types.retain(|ty| !ty.is_never());

                            if types.is_empty() {
                                return Ok(Cow::Owned(Type::never(
                                    ty.span,
                                    KeywordTypeMetadata {
                                        common: ty.metadata.common,
                                    },
                                )));
                            }
                            if types.len() == 1 {
                                return Ok(Cow::Owned(types.into_iter().next().unwrap()));
                            }

                            let ty = Type::Union(Union { types, ..*ty }).freezed();

                            return Ok(Cow::Owned(ty));
                        }
                    }

                    Type::Intersection(ty) => {
                        if !opts.preserve_intersection {
                            if let Some(new_ty) = self
                                .normalize_intersection_types(span.unwrap_or(ty.span), &ty.types, opts)
                                .context("failed to normalize an intersection type")?
                            {
                                return Ok(Cow::Owned(new_ty));
                            }
                        }
                    }

                    Type::Conditional(c) => {
                        return self.normalize_conditional(actual_span, c.clone(), opts);
                    }

                    Type::Query(q) => {
                        if !opts.preserve_typeof {
                            if let QueryExpr::TsEntityName(e) = &*q.expr {
                                if let RTsEntityName::Ident(i) = e {
                                    //
                                    if &*i.sym == "globalThis" {
                                        if opts.preserve_global_this {
                                            return Ok(Cow::Owned(Type::Query(QueryType {
                                                span: actual_span,
                                                expr: box QueryExpr::TsEntityName(e.clone()),
                                                metadata: Default::default(),
                                                tracker: Default::default(),
                                            })));
                                        } else {
                                            print_backtrace()
                                        }
                                    }
                                }

                                let expanded_ty = self
                                    .resolve_typeof(actual_span, e)
                                    .with_context(|| "tried to resolve typeof as a part of normalization".into())?;

                                if expanded_ty.is_global_this() {
                                    return Ok(Cow::Owned(expanded_ty));
                                }

                                if ty.type_eq(&expanded_ty) {
                                    return Ok(Cow::Owned(Type::any(
                                        actual_span.with_ctxt(SyntaxContext::empty()),
                                        Default::default(),
                                    )));
                                }

                                if expanded_ty.is_query() {
                                    unreachable!(
                                        "normalize: resolve_typeof returned a query type: {}",
                                        dump_type_as_string(&expanded_ty)
                                    )
                                }

                                return self
                                    .normalize(span, Cow::Owned(expanded_ty), opts)
                                    .context("tried to normalize the type returned from typeof");
                            }
                        }

                        if let QueryExpr::Import(import) = &*q.expr {
                            let base = self.storage.path(self.ctx.module_id);

                            let dep_id = self.loader.module_id(&base, &import.arg.value);

                            if let Some(dep_id) = dep_id {
                                if let Some(dep) = self.data.imports.get(&(self.ctx.module_id, dep_id)) {
                                    dep.assert_clone_cheap();
                                    return Ok(Cow::Owned(dep.clone()));
                                } else {
                                    return Err(ErrorKind::ModuleNotFound { span: import.span }.into());
                                }
                            }
                        }
                        // TODO
                    }

                    Type::Instance(ty) => {
                        let ty = self
                            .instantiate_for_normalization(
                                span,
                                &ty.ty,
                                NormalizeTypeOpts {
                                    preserve_global_this: true,
                                    ..opts
                                },
                            )
                            .context("tried to instantiate for normalizations")?;
                        ty.assert_valid();

                        if ty.is_query() || ty.is_instance() || ty.is_ref_type() {
                            return Ok(Cow::Owned(ty));
                        }

                        let mut ty = self.normalize(
                            span,
                            Cow::Owned(ty),
                            NormalizeTypeOpts {
                                preserve_global_this: true,
                                ..opts
                            },
                        )?;
                        ty.freeze();
                        let ty = ty.into_owned();

                        return Ok(Cow::Owned(ty));
                    }

                    Type::Import(import) => {
                        let base = self.storage.path(self.ctx.module_id);

                        let dep_id = self.loader.module_id(&base, &import.arg.value);

                        if let Some(dep_id) = dep_id {
                            if let Some(dep) = self.data.imports.get(&(self.ctx.module_id, dep_id)) {
                                dep.assert_clone_cheap();
                                return Ok(Cow::Owned(dep.clone()));
                            } else {
                                return Err(ErrorKind::ModuleNotFound { span: import.span }.into());
                            }
                        }
                    }

                    Type::Predicate(_) => {
                        // TODO(kdy1): Add option for this.
                    }

                    Type::IndexedAccessType(iat) => {
                        let obj_ty = box self
                            .normalize(span, Cow::Borrowed(&iat.obj_type), opts)
                            .context("tried to normalize object type")?
                            .into_owned();

                        let index_ty = box self
                            .normalize(span, Cow::Borrowed(&iat.index_type), opts)
                            .context("tried to normalize index type")?
                            .into_owned()
                            .freezed();

                        let ctx = Ctx {
                            disallow_unknown_object_property: true,
                            ..self.ctx
                        };
                        let prop_ty = self.with_ctx(ctx).access_property(
                            actual_span,
                            &obj_ty,
                            &Key::Computed(ComputedKey {
                                span: actual_span,
                                expr: box RExpr::Invalid(RInvalid { span: actual_span }),
                                ty: index_ty.clone(),
                            }),
                            TypeOfMode::RValue,
                            IdCtx::Type,
                            AccessPropertyOpts {
                                disallow_creating_indexed_type_from_ty_els: true,
                                disallow_inexact: true,
                                do_not_use_any_for_object: true,
                                ..Default::default()
                            },
                        );

                        if let Ok(prop_ty) = prop_ty {
                            if ty.type_eq(&prop_ty) {
                                return Ok(ty);
                            }

                            let ty = self
                                .normalize(span, Cow::Owned(prop_ty), opts)
                                .context("tried to normalize the type of property")?
                                .into_owned();

                            return Ok(Cow::Owned(ty));
                        }
                        // TODO(kdy1):

                        return Ok(Cow::Owned(Type::IndexedAccessType(IndexedAccessType {
                            span: iat.span,
                            readonly: iat.readonly,
                            obj_type: obj_ty,
                            index_type: index_ty,
                            metadata: iat.metadata,
                            tracker: Default::default(),
                        })));
                    }

                    Type::Index(Index { ty, .. }) => {
                        if !opts.preserve_keyof {
                            let keys_ty = self
                                .keyof(actual_span, ty)
                                .context("tried to get keys of a type as a part of normalization")?;
                            keys_ty.assert_valid();
                            return Ok(Cow::Owned(keys_ty));
                        }
                    }

                    Type::Enum(e) => {
                        // E => { [k: string]: string | E }
                        if opts.expand_enum_def {
                            let actual_span = actual_span.with_ctxt(SyntaxContext::empty());
                            let string = Type::Keyword(KeywordType {
                                span: e.span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            });

                            let variant = Type::EnumVariant(EnumVariant {
                                span: e.span,
                                def: e.cheap_clone(),
                                name: None,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            });

                            let index_param = FnParam {
                                span: actual_span,
                                required: true,
                                pat: RPat::Ident(RBindingIdent {
                                    node_id: NodeId::invalid(),
                                    id: RIdent {
                                        node_id: NodeId::invalid(),
                                        span: actual_span,
                                        sym: "__v".into(),
                                        optional: false,
                                    },
                                    type_ann: None,
                                }),
                                ty: box Type::Keyword(KeywordType {
                                    span: e.span,
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                            };
                            let index = TypeElement::Index(IndexSignature {
                                span: actual_span,
                                params: vec![index_param],
                                type_ann: Some(box Type::new_union(actual_span, vec![string, variant])),
                                readonly: false,
                                is_static: false,
                            });
                            return Ok(Cow::Owned(Type::TypeLit(TypeLit {
                                span: actual_span,
                                members: vec![index],
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })));
                        }
                    }

                    Type::Tuple(tuple) => {}

                    Type::Tpl(tpl) => {
                        if tpl.quasis.len() == 2
                            && tpl.types.len() == 1
                            && tpl.quasis[0].value.is_empty()
                            && tpl.quasis[1].value.is_empty()
                            && is_str_lit_or_union(&tpl.types[0])
                        {
                            return Ok(Cow::Owned(tpl.types[0].clone()));
                        }
                    }

                    _ => {}
                }
            }

            Ok(ty)
        })();

        if let Ok(res) = &res {
            #[cfg(debug_assertions)]
            let output = dump_type_as_string(res);

            #[cfg(debug_assertions)]
            debug!("normalize: {}\n===== ===== ===== ===== =====\n{}", input, output);
        }

        res
    }

    fn reduce_conditional_type(
        &mut self,
        span: Span,
        check_type: &Type,
        check_type_constraint: &Type,
        extends_type: &Type,
        true_type: &Type,
        false_type: &Type,
        metadata: ConditionalMetadata,
    ) -> VResult<Option<Type>> {
        if !Self::has_type_param_for_conditional(check_type) {
            return Ok(None);
        }
        let span = span.with_ctxt(SyntaxContext::empty());
        let mut worked = false;

        let mut true_type = self.normalize(Some(span), Cow::Borrowed(true_type), Default::default())?;
        let mut false_type = self.normalize(Some(span), Cow::Borrowed(false_type), Default::default())?;

        if let Type::Conditional(c) = true_type.normalize() {
            if (*c.check_type).type_eq(check_type) {
                if let Some(ty) = self.reduce_conditional_type(
                    span,
                    check_type,
                    extends_type,
                    &c.extends_type,
                    &c.true_type,
                    &c.false_type,
                    c.metadata,
                )? {
                    worked = true;
                    true_type = Cow::Owned(ty)
                }
            }
        }

        if let Type::Conditional(c) = false_type.normalize() {
            if (*c.check_type).type_eq(check_type) {
                let mut check_type_constraint = check_type_constraint.clone();
                self.exclude_type(span, &mut check_type_constraint, extends_type);
                check_type_constraint.fix();

                if let Some(ty) = self.reduce_conditional_type(
                    span,
                    check_type,
                    &check_type_constraint,
                    &c.extends_type,
                    &c.true_type,
                    &c.false_type,
                    c.metadata,
                )? {
                    worked = true;
                    false_type = Cow::Owned(ty)
                }
            }
        }

        match check_type_constraint.normalize() {
            Type::Union(check_type_union) => {
                //
                let can_match = check_type_union.types.iter().any(|check_type_constraint| {
                    self.extends(span, check_type_constraint, extends_type, Default::default())
                        .unwrap_or(true)
                });

                if !can_match {
                    return Ok(Some(Type::never(span, Default::default())));
                }
            }
            _ => {
                //
                if let Some(extends) = self.extends(span, check_type_constraint, extends_type, ExtendsOpts { ..Default::default() }) {
                    if extends {
                        return Ok(Some(true_type.into_owned()));
                    } else {
                        return Ok(Some(false_type.into_owned()));
                    }
                }
            }
        }

        if worked {
            Ok(Some(Type::Conditional(Conditional {
                span,
                check_type: box check_type.clone(),
                extends_type: box extends_type.clone(),
                true_type: box true_type.into_owned(),
                false_type: box false_type.into_owned(),
                metadata,
                tracker: Default::default(),
            })))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn normalize_intersection_types(&mut self, span: Span, types: &[Type], opts: NormalizeTypeOpts) -> VResult<Option<Type>> {
        macro_rules! never {
            () => {{
                Ok(Some(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                    metadata: KeywordTypeMetadata { ..Default::default() },
                    tracker: Default::default(),
                })))
            }};
        }
        let mut normalized_types = vec![];
        // set normalize all
        for el in types.iter() {
            if let Ok(res) = self.normalize(
                Some(span),
                Cow::Borrowed(el),
                NormalizeTypeOpts {
                    preserve_global_this: true,
                    ..opts
                },
            ) {
                let result = res.into_owned();

                match &result.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {}
                    Type::Intersection(Intersection { types, .. }) => {
                        for ty in types {
                            normalized_types.push(ty.to_owned());
                        }
                    }
                    _ => {
                        normalized_types.push(result);
                    }
                }
            }
        }

        normalized_types.dedup_type();

        if normalized_types.len() == 1 {
            if let Some(ty) = normalized_types.pop() {
                return Ok(Some(ty));
            }
        }
        // has never; return never
        if normalized_types.iter().any(|ty| ty.is_never()) {
            return never!();
        }
        // has any, return any
        if normalized_types.iter().any(|ty| ty.is_any()) {
            return Ok(Some(Type::Keyword(KeywordType {
                span,
                kind: TsKeywordTypeKind::TsAnyKeyword,
                metadata: KeywordTypeMetadata { ..Default::default() },
                tracker: Default::default(),
            })));
        }

        let is_lit = normalized_types.iter().any(|ty| ty.is_lit());
        let is_symbol = normalized_types.iter().any(|ty| ty.is_symbol());
        let is_str = normalized_types.iter().any(|ty| ty.is_str());
        let is_num = normalized_types.iter().any(|ty| ty.is_num());
        let is_bool = normalized_types.iter().any(|ty| ty.is_bool());
        let is_null = normalized_types.iter().any(|ty| ty.is_null());
        let is_undefined = normalized_types.iter().any(|ty| ty.is_undefined());
        let is_void = normalized_types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword));
        let is_object = normalized_types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword));
        let is_function = normalized_types.iter().any(|ty| ty.is_fn_type());
        let is_type_lit = normalized_types.iter().any(|ty| ty.is_type_lit());

        if (is_null || is_undefined) && is_type_lit {
            return never!();
        }

        let sum = u32::from(is_symbol)
            + u32::from(is_str)
            + u32::from(is_num)
            + u32::from(is_bool)
            + u32::from(is_null)
            + u32::from(is_undefined)
            + u32::from(is_void)
            + u32::from(is_object)
            + u32::from(is_function);

        if sum >= 2 {
            if sum == 2 && is_undefined && is_void {
                return Ok(Some(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    metadata: KeywordTypeMetadata { ..Default::default() },
                    tracker: Default::default(),
                })));
            }
            return never!();
        }

        if normalized_types.len() == 2 {
            let (a, b) = (&normalized_types[0], &normalized_types[1]);
            if ((a.is_str_lit() && b.is_str_lit()) || (a.is_num_lit() && b.is_num_lit()) || (a.is_bool_lit() && b.is_bool_lit()))
                && !a.type_eq(b)
            {
                return never!();
            }
            if let (Type::Conditional(c), other) | (other, Type::Conditional(c)) = (a, b) {
                return Ok(Some(
                    Type::Conditional(Conditional {
                        span,
                        check_type: c.check_type.clone(),
                        extends_type: c.extends_type.clone(),
                        true_type: Box::new(Type::new_intersection(span, vec![*(c.true_type).clone(), other.clone()])),
                        false_type: Box::new(Type::new_intersection(span, vec![*(c.false_type.clone()), other.clone()])),
                        metadata: c.metadata,
                        tracker: c.tracker,
                    })
                    .freezed(),
                ));
            }
        }

        let enum_variant_iter = normalized_types.iter().filter(|&t| t.is_enum_variant()).collect::<Vec<&Type>>();
        let enum_variant_len = enum_variant_iter.len();

        if enum_variant_len > 0 {
            if normalized_types.iter().any(|ty| matches!(ty, Type::Lit(..))) {
                return never!();
            }
            if let Some(first_enum) = enum_variant_iter.first() {
                let mut enum_temp = first_enum.normalize();
                for elem in enum_variant_iter.into_iter() {
                    if let Type::EnumVariant(el) = elem.normalize() {
                        if let Type::EnumVariant(en) = enum_temp {
                            if let Type::EnumVariant(EnumVariant { name: None, .. }) = enum_temp {
                                enum_temp = elem;
                                continue;
                            } else if en.def.id != el.def.id {
                                return never!();
                            } else {
                                // eq two argument enum_name
                                if let Ok(el_lit) = self.expand_enum_variant(elem.clone()) {
                                    if let Ok(etl) = self.expand_enum_variant(enum_temp.clone()) {
                                        if !etl.type_eq(&el_lit) {
                                            return never!();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            for elem in normalized_types.iter() {
                if let Type::EnumVariant(ref ev) = elem.normalize() {
                    if let Some(variant_name) = &ev.name {
                        // enumVariant is enumMember
                        if enum_variant_len > 1 {
                            let mut en = ev.clone();
                            en.name = None;
                            return Ok(Some(Type::EnumVariant(en).freezed()));
                        }
                        if let Ok(Type::Lit(LitType { .. })) = self.expand_enum_variant(elem.clone()) {
                            return Ok(Some(elem.clone().freezed()));
                        }
                    } else {
                        // enumVariant is Enum

                        let mut str_lits = vec![];
                        let mut num_lits = vec![];
                        for v in ev.def.members.iter() {
                            let key = match &v.id {
                                RTsEnumMemberId::Ident(i) => i.clone(),
                                RTsEnumMemberId::Str(s) => RIdent::new(s.value.clone(), s.span),
                            };
                            match &*v.val {
                                Type::Lit(LitType { lit: RTsLit::Str(v), .. }) => str_lits.push(Type::EnumVariant(EnumVariant {
                                    span: v.span,
                                    def: ev.def.cheap_clone(),
                                    name: Some(key.sym),
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                })),
                                Type::Lit(LitType {
                                    lit: RTsLit::Number(v), ..
                                }) => num_lits.push(Type::EnumVariant(EnumVariant {
                                    span: v.span,
                                    def: ev.def.cheap_clone(),
                                    name: Some(key.sym),
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                })),
                                _ => {}
                            }
                        }

                        if str_lits.is_empty() && is_str {
                            return never!();
                        }
                        if num_lits.is_empty() && is_num {
                            return never!();
                        }
                        if str_lits.is_empty() && is_num || num_lits.is_empty() && is_str {
                            return Ok(Some(elem.clone().freezed()));
                        }

                        let mut ty = Type::new_union(
                            span,
                            if is_str {
                                str_lits
                            } else if is_num {
                                num_lits
                            } else {
                                return never!();
                            },
                        );

                        ty.reposition(ev.def.span);
                        return Ok(Some(ty).freezed());
                    }
                }
            }
        }

        let mut property_types = vec![];

        for elem in types.iter() {
            let elem = self
                .normalize(
                    Some(span),
                    Cow::Borrowed(elem),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        ..opts
                    },
                )
                .context("failed to normalize types while intersecting properties")?
                .freezed()
                .into_owned()
                .freezed();

            if let Type::TypeLit(elem_tl) = elem.normalize_instance() {
                if let Some(ty) = self.normalize_intersection_of_type_elements(span, &elem_tl.members, &mut property_types, opts)? {
                    return Ok(Some(ty));
                }
            }
        }

        {
            let normalized_len = normalized_types.len();
            normalized_types.freeze();
            let mut type_iter = normalized_types.clone().into_iter();
            let mut acc_type = type_iter
                .next()
                .unwrap_or_else(|| {
                    Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNeverKeyword,
                        metadata: KeywordTypeMetadata { ..Default::default() },
                        tracker: Default::default(),
                    })
                })
                .freezed();

            for elem in type_iter {
                let mut new_types = vec![];
                match (acc_type.normalize(), elem.normalize()) {
                    (
                        another @ Type::Param(TypeParam {
                            constraint:
                                Some(box Type::Keyword(KeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })),
                            ..
                        }),
                        other,
                    )
                    | (
                        other,
                        another @ Type::Param(TypeParam {
                            constraint:
                                Some(box Type::Keyword(KeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })),
                            ..
                        }),
                    ) => {
                        new_types.push(Type::new_intersection(span, [other.clone(), another.clone()]).freezed());
                    }

                    (
                        Type::Param(TypeParam {
                            constraint: Some(other), ..
                        }),
                        Type::Param(TypeParam {
                            constraint: Some(another), ..
                        }),
                    ) => {
                        let other = other.normalize();
                        let another = another.normalize();
                        let result =
                            self.normalize_intersection_types(span, &vec![other.to_owned(), another.to_owned()], Default::default())?;
                        if let Some(tp) = result {
                            new_types.push(tp);
                        }
                    }
                    (
                        Type::Param(TypeParam {
                            constraint: Some(another), ..
                        }),
                        other,
                    )
                    | (
                        other,
                        Type::Param(TypeParam {
                            constraint: Some(another), ..
                        }),
                    ) => {
                        let other = other.normalize();
                        let another = another.normalize();
                        let result =
                            self.normalize_intersection_types(span, &vec![other.to_owned(), another.to_owned()], Default::default())?;
                        if let Some(tp) = result {
                            new_types.push(tp);
                        }
                    }
                    (Type::Union(Union { types: a_types, .. }), Type::Union(Union { types: b_types, .. })) => {
                        for a_ty in a_types {
                            for b_ty in b_types {
                                let result =
                                    self.normalize_intersection_types(span, &vec![a_ty.to_owned(), b_ty.to_owned()], Default::default())?;
                                if let Some(tp) = result {
                                    new_types.push(tp);
                                }
                            }
                        }
                    }
                    (Type::Union(Union { types, .. }), other) | (other, Type::Union(Union { types, .. })) => {
                        for ty in types {
                            let result =
                                self.normalize_intersection_types(span, &vec![ty.to_owned(), other.to_owned()], Default::default())?;
                            if let Some(tp) = result {
                                new_types.push(tp);
                            }
                        }
                    }
                    (Type::Intersection(Intersection { types, .. }), other) | (other, Type::Intersection(Intersection { types, .. })) => {
                        let mut temp_vec = vec![];
                        temp_vec.append(&mut types.to_owned());
                        temp_vec.push(other.to_owned());

                        acc_type = Type::new_intersection(span, temp_vec).freezed();
                        continue;
                    }
                    (other, another) => {
                        acc_type = Type::new_intersection(span, vec![other.to_owned(), another.to_owned()]).freezed();
                        continue;
                    }
                };

                new_types.retain(|ty| !ty.is_never());
                acc_type = if new_types.is_empty() {
                    return never!();
                } else if new_types.len() == 1 {
                    if let Some(ty) = new_types.pop() {
                        ty
                    } else {
                        return never!();
                    }
                } else {
                    Type::new_union(span, new_types).freezed()
                }
            }
            if let Type::Union(Union { types: u_types, .. }) = acc_type.normalize() {
                if normalized_len < u_types.len() {
                    return Ok(Some(
                        Type::Intersection(Intersection {
                            span,
                            types: normalized_types,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })
                        .freezed(),
                    ));
                }
            }
            Ok(Some(acc_type))
        }
    }

    fn normalize_intersection_of_type_elements(
        &mut self,
        span: Span,
        elements: &[TypeElement],
        property_types: &mut Vec<TypeElement>,
        opts: NormalizeTypeOpts,
    ) -> VResult<Option<Type>> {
        macro_rules! never {
            () => {{
                Ok(Some(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                    metadata: KeywordTypeMetadata { ..Default::default() },
                    tracker: Default::default(),
                })))
            }};
        }

        // Intersect property types
        'outer: for e in elements.iter() {
            if let TypeElement::Property(p) = e {
                for prev in property_types.iter_mut() {
                    if let TypeElement::Property(prev) = prev {
                        if prev.key.type_eq(&p.key) {
                            let prev_type = prev
                                .type_ann
                                .clone()
                                .map(|v| *v)
                                .unwrap_or_else(|| Type::any(span, KeywordTypeMetadata { ..Default::default() }));
                            let other = p
                                .type_ann
                                .clone()
                                .map(|v| *v)
                                .unwrap_or_else(|| Type::any(span, KeywordTypeMetadata { ..Default::default() }));

                            let new = self.normalize_intersection_types(span, &[prev_type, other], opts)?;

                            if let Some(new) = new {
                                if new.is_never() {
                                    return never!();
                                }
                                prev.type_ann = Some(box new);
                                continue 'outer;
                            }
                        }
                    }
                }
            }

            property_types.push(e.clone());
        }

        Ok(None)
    }

    // This is part of normalization.
    fn instantiate_for_normalization(&mut self, span: Option<Span>, ty: &Type, opts: NormalizeTypeOpts) -> VResult<Type> {
        let _tracing = if cfg!(debug_assertions) {
            let ty_str = force_dump_type_as_string(ty);

            Some(dev_span!("instantiate_for_normalization", ty = &*ty_str))
        } else {
            None
        };

        let mut ty = self.normalize(
            span,
            Cow::Borrowed(ty),
            NormalizeTypeOpts {
                normalize_keywords: false,
                ..opts
            },
        )?;
        ty.freeze();
        let metadata = ty.metadata();
        let actual_span = ty.span();

        // TODO(kdy1): PERF
        let mut ty = ty.into_owned();
        ty.normalize_mut();

        Ok(match ty {
            // For self-references in classes, we preserve `instanceof` type.
            Type::Ref(..) | Type::Query(..) => Type::Instance(Instance {
                span: actual_span,
                ty: box ty,
                metadata: InstanceMetadata {
                    common: metadata,
                    ..Default::default()
                },
                tracker: Default::default(),
            }),

            Type::ClassDef(def) => Type::Class(Class {
                span: actual_span,
                def,
                metadata: ClassMetadata {
                    common: metadata,
                    ..Default::default()
                },
                tracker: Default::default(),
            }),

            Type::StaticThis(ty) => Type::This(ThisType {
                span: actual_span,
                metadata: ThisTypeMetadata {
                    common: metadata,
                    ..Default::default()
                },
                tracker: Default::default(),
            }),

            Type::Intersection(ty) if !opts.preserve_intersection => {
                let types = ty
                    .types
                    .into_iter()
                    .map(|ty| self.instantiate_for_normalization(span, &ty, opts))
                    .collect::<Result<_, _>>()?;

                Type::Intersection(Intersection { types, ..ty }).fixed()
            }

            Type::Union(ty) if !opts.preserve_union => {
                let types = ty
                    .types
                    .into_iter()
                    .map(|ty| self.instantiate_for_normalization(span, &ty, opts))
                    .collect::<Result<_, _>>()?;

                Type::Union(Union { types, ..ty }).fixed()
            }

            _ => ty,
        })
    }

    pub(crate) fn report_possibly_null_or_undefined(&mut self, span: Span, ty: &Type) -> VResult<()> {
        let ty = self
            .normalize(Some(span), Cow::Borrowed(ty), Default::default())
            .context("tried to normalize to see if it can be undefined")?;

        if ty.is_str() || ty.is_bool() || ty.is_num() || ty.is_lit() {
            return Ok(());
        }

        if ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
            return Err(ErrorKind::ObjectIsPossiblyUndefined { span }.into());
        }
        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
            return Err(ErrorKind::ObjectIsPossiblyNull { span }.into());
        }

        match &*ty {
            Type::Class(..)
            | Type::ClassDef(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Keyword(..)
            | Type::Lit(..)
            | Type::Interface(..)
            | Type::TypeLit(..)
            | Type::Param(..)
            | Type::Tpl(..) => Ok(()),
            Type::Union(ty) => {
                let has_null = ty.types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsNullKeyword));
                let has_undefined = ty
                    .types
                    .iter()
                    .any(|ty| ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword));

                // tsc is crazy. It uses different error code for these errors.
                if has_null && has_undefined {
                    return Err(ErrorKind::ObjectIsPossiblyNullOrUndefined { span }.into());
                }

                if has_null {
                    return Err(ErrorKind::ObjectIsPossiblyNull { span }.into());
                }

                if has_undefined {
                    return Err(ErrorKind::ObjectIsPossiblyUndefined { span }.into());
                }

                Ok(())
            }
            _ => {
                if !self.rule().strict_null_checks {
                    return Ok(());
                }
                Err(ErrorKind::ObjectIsPossiblyUndefinedWithType {
                    span,
                    ty: box ty.into_owned(),
                }
                .into())
            }
        }
    }

    pub(crate) fn can_be_undefined(&mut self, span: Span, ty: &Type, include_null: bool) -> VResult<bool> {
        let _tracing = dev_span!("can_be_undefined", include_null = include_null);

        let ty = self
            .normalize(Some(span), Cow::Borrowed(ty), Default::default())
            .context("tried to normalize to see if it can be undefined")?;

        if ty.is_str() || ty.is_bool() || ty.is_num() || ty.is_lit() {
            return Ok(false);
        }

        if include_null {
            if ty.is_null() {
                return Ok(true);
            }
        } else {
            if ty.is_null() {
                return Ok(false);
            }
        }

        if ty.is_any() || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
            return Ok(true);
        }

        Ok(match ty.normalize() {
            Type::Class(..)
            | Type::ClassDef(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Keyword(..)
            | Type::Lit(..)
            | Type::TypeLit(..) => false,
            Type::Union(ty) => {
                for ty in &ty.types {
                    if self.can_be_undefined(span, ty, include_null)? {
                        return Ok(true);
                    }
                }

                false
            }
            _ => true,
        })
    }

    pub(crate) fn expand_type_ann<'a>(&mut self, span: Span, ty: Option<&'a Type>) -> VResult<Option<Cow<'a, Type>>> {
        let _tracing = dev_span!("expand_type_ann");

        let ty = match ty {
            Some(v) => v,
            None => return Ok(None),
        };
        let span = span.with_ctxt(SyntaxContext::empty());

        let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default())?;

        Ok(Some(ty))
    }

    pub(crate) fn create_prototype_of_class_def(&mut self, def: &ClassDef) -> VResult<TypeLit> {
        let _tracing = dev_span!("create_prototype_of_class_def");

        let mut members = vec![];

        let type_params = def.type_params.as_ref().map(|decl| {
            let ty = Type::any(decl.span, Default::default());

            decl.params
                .iter()
                .map(|param| (param.name.clone(), ty.clone()))
                .collect::<FxHashMap<_, _>>()
        });

        for member in &def.body {
            match member {
                ClassMember::Constructor(_) => {}
                ClassMember::Method(_) => {}
                ClassMember::Property(p) => {
                    let mut type_ann = p.value.clone();
                    if let Some(type_params) = &type_params {
                        type_ann = type_ann
                            .map(|ty| self.expand_type_params(type_params, *ty, Default::default()).map(Box::new))
                            .transpose()?;
                    }
                    //
                    members.push(TypeElement::Property(PropertySignature {
                        span: p.span,
                        accessibility: None,
                        readonly: p.readonly,
                        key: p.key.clone(),
                        optional: p.is_optional,
                        params: Default::default(),
                        type_ann,
                        type_params: Default::default(),
                        metadata: Default::default(),
                        accessor: Default::default(),
                    }))
                }
                ClassMember::IndexSignature(_) => {}
            }
        }

        Ok(TypeLit {
            span: def.span,
            members,
            metadata: TypeLitMetadata {
                inexact: true,
                ..Default::default()
            },
            tracker: Default::default(),
        })
    }

    /// Exclude types from `ty` using type facts with key `name`, for
    /// the current scope.
    pub(crate) fn exclude_types_using_fact(&mut self, span: Span, name: &Name, ty: &mut Type) {
        let _tracing = dev_span!("exclude_types_using_fact");

        debug_assert!(!span.is_dummy(), "exclude_types should not be called with a dummy span");

        let mut types_to_exclude = vec![];
        let mut s = Some(&self.scope);

        while let Some(scope) = s {
            types_to_exclude.extend(scope.facts.excludes.get(name).cloned().into_iter().flatten());
            s = scope.parent();
        }

        types_to_exclude.extend(self.cur_facts.true_facts.excludes.get(name).cloned().into_iter().flatten());

        let before = dump_type_as_string(ty);
        self.exclude_types(span, ty, Some(types_to_exclude));
        let after = dump_type_as_string(ty);

        debug!("[types/facts] Excluded types: {} => {}", before, after);
    }

    pub(crate) fn apply_type_facts(&mut self, name: &Name, ty: Type) -> Type {
        let _tracing = dev_span!("apply_type_facts", name = tracing::field::debug(name));

        let type_facts = self.scope.get_type_facts(name) | self.cur_facts.true_facts.facts.get(name).copied().unwrap_or(TypeFacts::None);

        debug!("[types/fact] Facts for {:?} is {:?}", name, type_facts);

        self.apply_type_facts_to_type(type_facts, ty)
    }

    /// Collect all class members, including inherited members.
    ///
    /// # Parameters
    ///
    /// ## excluded
    ///
    /// Members of base class.
    pub(crate) fn collect_class_members(&mut self, excluded: &[&ClassMember], ty: &Type) -> VResult<Option<Vec<ClassMember>>> {
        if self.config.is_builtin {
            return Ok(None);
        }
        let _tracing = dev_span!("collect_class_members");

        let ty = ty.normalize();
        match ty {
            Type::ClassDef(c) => {
                let mut members = c
                    .body
                    .iter()
                    .filter(|&super_member| {
                        if let Some(super_key) = super_member.key() {
                            for excluded in excluded {
                                if let Some(exc_key) = excluded.key() {
                                    if self.key_matches(super_key.span(), &super_key, &exc_key, false) {
                                        return false;
                                    }
                                }
                            }
                        }

                        true
                    })
                    .cloned()
                    .collect_vec();

                match &c.super_class {
                    Some(sc) => {
                        let mut excluded = excluded.to_vec();
                        excluded.extend(members.iter());
                        // TODO(kdy1): Override

                        if let Some(super_members) = self.collect_class_members(&excluded, sc)? {
                            members.extend(super_members)
                        }

                        Ok(Some(members))
                    }
                    None => Ok(Some(members)),
                }
            }
            Type::Class(c) => self.collect_class_members(excluded, &Type::ClassDef(c.def.clone())),
            _ => {
                error!("unimplemented: collect_class_members: {:?}", ty);
                Ok(None)
            }
        }
    }

    /// Note: `span` is only used while expanding type (to prevent
    /// panic) in the case of [Type::Ref].
    pub(crate) fn convert_type_to_type_lit<'a>(&mut self, span: Span, ty: Cow<'a, Type>) -> VResult<Option<Cow<'a, TypeLit>>> {
        let span = span.with_ctxt(SyntaxContext::empty());

        debug_assert!(!span.is_dummy(), "type_to_type_lit: `span` should not be dummy");

        let ty = self.normalize(
            Some(span),
            ty,
            NormalizeTypeOpts {
                preserve_union: true,
                ..Default::default()
            },
        )?;

        if ty.is_type_lit() {
            match ty {
                Cow::Owned(ty) => {
                    let t = ty.expect_type_lit();
                    return Ok(Some(Cow::Owned(t)));
                }

                Cow::Borrowed(ty) => match ty.normalize() {
                    Type::TypeLit(t) => return Ok(Some(Cow::Borrowed(t))),
                    _ => {
                        unreachable!()
                    }
                },
            }
        }

        if ty.is_interface() {
            let t = ty.into_owned().expect_interface();
            let mut members = vec![];

            for parent in &t.extends {
                let parent = self.type_of_ts_entity_name(parent.span(), &parent.expr, parent.type_args.as_deref())?;

                let super_els = self.convert_type_to_type_lit(span, Cow::Owned(parent))?;

                members.extend(super_els.into_iter().map(Cow::into_owned).flat_map(|v| v.members))
            }

            members.extend(t.body);
            let members = self.merge_type_elements(span, members)?;
            return Ok(Some(Cow::Owned(TypeLit {
                span: t.span,
                members,
                metadata: TypeLitMetadata {
                    inexact: true,
                    ..Default::default()
                },
                tracker: Default::default(),
            })));
        }

        let ty = ty.normalize();

        Ok(Some(match ty {
            Type::Lit(ty) => {
                let kind = match &ty.lit {
                    RTsLit::Bool(..) => TsKeywordTypeKind::TsBooleanKeyword,
                    RTsLit::Number(..) => TsKeywordTypeKind::TsNumberKeyword,
                    RTsLit::Str(..) => TsKeywordTypeKind::TsStringKeyword,
                    RTsLit::Tpl(..) => unreachable!(),
                    RTsLit::BigInt(..) => TsKeywordTypeKind::TsBigIntKeyword,
                };

                let ty = self
                    .convert_type_to_type_lit(
                        span,
                        Cow::Owned(Type::Keyword(KeywordType {
                            span: ty.span,
                            kind,
                            metadata: KeywordTypeMetadata {
                                common: ty.metadata.common,
                                ..Default::default()
                            },
                            tracker: Default::default(),
                        })),
                    )
                    .context("tried to convert a literal to type literal")?
                    .map(Cow::into_owned);
                return Ok(ty.map(Cow::Owned));
            }

            Type::Keyword(ty) => {
                let name = match ty.kind {
                    TsKeywordTypeKind::TsNumberKeyword => js_word!("Number"),
                    TsKeywordTypeKind::TsObjectKeyword => js_word!("Object"),
                    TsKeywordTypeKind::TsBooleanKeyword => js_word!("Boolean"),
                    TsKeywordTypeKind::TsBigIntKeyword => js_word!("BigInt"),
                    TsKeywordTypeKind::TsStringKeyword => js_word!("String"),
                    TsKeywordTypeKind::TsSymbolKeyword => js_word!("Symbol"),
                    _ => return Ok(None),
                };

                return Ok(self
                    .convert_type_to_type_lit(
                        span,
                        Cow::Owned(Type::Ref(Ref {
                            span,
                            type_name: RTsEntityName::Ident(RIdent::new(name, span)),
                            type_args: None,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })),
                    )?
                    .map(Cow::into_owned)
                    .map(Cow::Owned));
            }

            Type::Enum(e) => self.enum_to_type_lit(e).map(Cow::Owned)?,

            Type::Class(c) => {
                let mut members = vec![];
                if let Some(super_class) = &c.def.super_class {
                    let super_class = self.instantiate_class(span, super_class)?;
                    let super_els = self.convert_type_to_type_lit(span, Cow::Owned(super_class))?;
                    members.extend(super_els.map(|ty| ty.into_owned().members).into_iter().flatten());
                }

                // TODO(kdy1): Override

                for member in &c.def.body {
                    members.extend(self.make_type_el_from_class_member(member, false));
                }

                Cow::Owned(TypeLit {
                    span: c.span,
                    members,
                    metadata: TypeLitMetadata { ..Default::default() },
                    tracker: Default::default(),
                })
            }

            Type::ClassDef(c) => {
                let mut members = vec![];
                if let Some(super_class) = &c.super_class {
                    let super_els = self.convert_type_to_type_lit(span, Cow::Borrowed(super_class))?;
                    members.extend(super_els.map(|ty| ty.into_owned().members).into_iter().flatten());
                }

                // TODO(kdy1): Override

                for member in &c.body {
                    members.extend(self.make_type_el_from_class_member(member, true));
                }

                Cow::Owned(TypeLit {
                    span: c.span,
                    members,
                    metadata: TypeLitMetadata { ..Default::default() },
                    tracker: Default::default(),
                })
            }

            Type::Intersection(t) => {
                let mut members = vec![];
                for ty in &t.types {
                    let opt = self.convert_type_to_type_lit(span, Cow::Borrowed(ty))?;
                    members.extend(opt.into_iter().map(Cow::into_owned).flat_map(|v| v.members));
                }

                let members = self.merge_type_elements(span, members)?;
                Cow::Owned(TypeLit {
                    span: t.span,
                    members,
                    metadata: TypeLitMetadata {
                        inexact: true,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
            }

            Type::Conditional(t) => {
                let mut members = vec![];
                {
                    let ty = self.overwrite_conditional(span, t);
                    let opt = self.convert_type_to_type_lit(span, Cow::Borrowed(&ty))?;
                    members.extend(opt.into_iter().map(Cow::into_owned).flat_map(|v| v.members));
                }
                {
                    let opt = self.convert_type_to_type_lit(span, Cow::Borrowed(&t.false_type))?;
                    members.extend(opt.into_iter().map(Cow::into_owned).flat_map(|v| v.members));
                }
                Cow::Owned(TypeLit {
                    span: t.span,
                    members,
                    metadata: TypeLitMetadata {
                        inexact: true,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
            }
            Type::Constructor(ty) => {
                let el = TypeElement::Constructor(ConstructorSignature {
                    span: ty.span.with_ctxt(SyntaxContext::empty()),
                    accessibility: None,
                    params: ty.params.clone(),
                    ret_ty: Some(ty.type_ann.clone()),
                    type_params: ty.type_params.clone(),
                });

                Cow::Owned(TypeLit {
                    span: ty.span,
                    members: vec![el],
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
            }

            Type::Function(ty) => {
                let el = self
                    .fn_to_type_element(ty)
                    .context("tried to convert function to type element to create type literal")?;

                Cow::Owned(TypeLit {
                    span: ty.span,
                    members: vec![el],
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
            }

            Type::Tuple(ty) => {
                let mut members = vec![];

                for (idx, e) in ty.elems.iter().enumerate() {
                    members.push(TypeElement::Property(PropertySignature {
                        span: e.span.with_ctxt(SyntaxContext::empty()),
                        accessibility: None,
                        readonly: false,
                        key: Key::Num(RNumber {
                            span: e.span,
                            value: idx as f64,
                            raw: None,
                        }),
                        optional: false,
                        params: Default::default(),
                        type_ann: Some(e.ty.clone()),
                        type_params: Default::default(),
                        metadata: Default::default(),
                        accessor: Default::default(),
                    }));
                }

                // length
                members.push(TypeElement::Property(PropertySignature {
                    span: ty.span.with_ctxt(SyntaxContext::empty()),
                    accessibility: None,
                    readonly: true,
                    key: Key::Normal {
                        span: ty.span.with_ctxt(SyntaxContext::empty()),
                        sym: "length".into(),
                    },
                    optional: false,
                    params: Default::default(),
                    type_ann: Some(box Type::Keyword(KeywordType {
                        span: ty.span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        metadata: KeywordTypeMetadata {
                            common: ty.metadata.common,
                            ..Default::default()
                        },
                        tracker: Default::default(),
                    })),
                    type_params: Default::default(),
                    metadata: Default::default(),
                    accessor: Accessor {
                        getter: true,
                        setter: false,
                    },
                }));

                Cow::Owned(TypeLit {
                    span: ty.span,
                    members,
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
            }

            _ => {
                error!("unimplemented: type_to_type_lit: {}", force_dump_type_as_string(ty));
                return Ok(None);
            }
        }))
    }

    fn merge_type_elements(&mut self, span: Span, mut els: Vec<TypeElement>) -> VResult<Vec<TypeElement>> {
        run(|| {
            // As merging is not common, we optimize it by creating a new vector only if
            // there's a conflict

            let mut merged = vec![];

            for (ai, a) in els.iter().enumerate() {
                for (bi, b) in els.iter().enumerate() {
                    if ai >= bi {
                        continue;
                    }

                    match (a, b) {
                        (TypeElement::Index(a_index), TypeElement::Index(b_index)) => {
                            if merged.iter().all(|(a, b)| *b != bi) {
                                merged.push((ai, bi));
                            }
                        }

                        (TypeElement::Property(ap), TypeElement::Property(bp)) => {
                            if merged.iter().all(|(a, b)| *b != bi) && self.key_matches(span, &ap.key, &bp.key, false) {
                                merged.push((ai, bi));
                            }
                        }

                        _ => {}
                    }
                }
            }

            if merged.is_empty() {
                return Ok(els);
            }

            els.freeze();
            // For (ai, bi) in `merged`, we can assume ai < bi because we only store in that
            // case
            for (ai, bi) in merged.iter().copied() {
                let b = els[bi].take();
                self.merge_type_element(span, &mut els[ai], b)?;
            }

            let new = els
                .into_iter()
                .enumerate()
                .filter_map(|(i, el)| if merged.iter().any(|(ai, bi)| *bi == i) { None } else { Some(el) })
                .collect::<Vec<_>>();

            Ok(new)
        })
        .with_context(|| "tried to merge type elements".to_string())
    }

    fn merge_type_element(&mut self, span: Span, to: &mut TypeElement, from: TypeElement) -> VResult<()> {
        run(|| match (to, from) {
            (TypeElement::Property(to), TypeElement::Property(from)) => {
                if let Some(to_type) = &to.type_ann {
                    if let Some(from_type) = from.type_ann {
                        to.type_ann = Some(
                            box Type::Intersection(Intersection {
                                span: to_type.span(),
                                types: vec![*to_type.clone(), *from_type],
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                            .fixed()
                            .freezed(),
                        );
                    }
                }

                Ok(())
            }

            (TypeElement::Index(to), TypeElement::Index(from)) => {
                if let Some(to_type) = &to.type_ann {
                    if let Some(from_type) = from.type_ann {
                        to.type_ann = Some(
                            box Type::Intersection(Intersection {
                                span: to_type.span(),
                                types: vec![*to_type.clone(), *from_type],
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                            .fixed()
                            .freezed(),
                        );
                    }
                }

                Ok(())
            }

            (to, from) => {
                todo!("merge_type_element: {:?} and {:?}", to, from)
            }
        })
        .context("tried to merge a type element")
    }

    pub(crate) fn normalize_tuples(&mut self, ty: &mut Type) {
        normalize_tuples(ty);
        ty.fix();
    }

    /// This is used to determine `form` of `els`. Each type has a
    /// value. e.g. `1` for [TypeElement::Call].
    pub(crate) fn kinds_of_type_elements(&mut self, els: &[TypeElement]) -> Vec<u8> {
        let mut v = els
            .iter()
            .map(|v| match v {
                TypeElement::Call(_) => 1,
                TypeElement::Constructor(_) => 2,
                TypeElement::Property(_) => 3,
                TypeElement::Method(_) => 4,
                TypeElement::Index(_) => 5,
            })
            .collect::<Vec<_>>();
        v.sort();
        v
    }

    pub(crate) fn expand_intrinsic_types(&mut self, span: Span, ty: &StringMapping) -> VResult<Type> {
        let arg = &ty.type_args;

        match self.normalize(None, Cow::Borrowed(&arg.params[0]), Default::default())?.normalize() {
            Type::Lit(LitType { lit: RTsLit::Str(s), .. }) => {
                let new_val = apply_string_mapping(&ty.kind, &s.value);

                return Ok(Type::Lit(LitType {
                    span: arg.params[0].span(),
                    lit: RTsLit::Str(RStr {
                        span: arg.params[0].span(),
                        value: JsWord::from(new_val.as_ref()),
                        raw: None,
                    }),
                    metadata: LitTypeMetadata {
                        common: arg.params[0].metadata(),
                        ..Default::default()
                    },
                    tracker: Default::default(),
                }));
            }
            Type::Tpl(TplType {
                span,
                quasis,
                types,
                metadata,
                ..
            }) => {
                let quasis = quasis
                    .iter()
                    .map(|quasis| {
                        let value = apply_string_mapping(&ty.kind, &quasis.value);

                        TplElem { value, ..quasis.clone() }
                    })
                    .collect();

                return Ok(Type::Tpl(TplType {
                    span: *span,
                    quasis,
                    types: types.clone(),
                    metadata: *metadata,
                    tracker: Default::default(),
                }));
            }

            Type::Param(TypeParam {
                span: param_span,
                name,
                constraint: Some(constraint),
                default,
                metadata,
                ..
            }) => {
                let resolved_constraint = match constraint.normalize() {
                    Type::Lit(LitType {
                        span: constraint_span,
                        lit: RTsLit::Str(s),
                        metadata,
                        tracker,
                    }) => self
                        .expand_intrinsic_types(
                            span,
                            &StringMapping {
                                span: ty.span,
                                kind: ty.kind.clone(),
                                type_args: TypeParamInstantiation {
                                    span: *param_span,
                                    params: vec![Type::Lit(LitType {
                                        span: *constraint_span,
                                        lit: RTsLit::Str(s.clone()),
                                        metadata: *metadata,
                                        tracker: *tracker,
                                    })],
                                },
                                metadata: ty.metadata,
                            },
                        )
                        .ok()
                        .map(|value| value.freezed())
                        .map(|value| box value),
                    Type::Union(Union {
                        types,
                        span: union_span,
                        metadata,
                        tracker,
                    }) => Some(
                        box Type::Union(Union {
                            types: types
                                .iter()
                                .map(|inner_ty| {
                                    self.expand_intrinsic_types(
                                        span,
                                        &StringMapping {
                                            span: ty.span(),
                                            kind: ty.kind.clone(),
                                            type_args: TypeParamInstantiation {
                                                span: inner_ty.span(),
                                                params: vec![inner_ty.clone()],
                                            },
                                            metadata: ty.metadata,
                                        },
                                    )
                                })
                                .map(|val| val.ok())
                                .filter_map(|val| val.freezed())
                                .collect(),
                            span: *union_span,
                            metadata: *metadata,
                            tracker: *tracker,
                        })
                        .freezed(),
                    ),
                    _ => None,
                };

                let constraint = resolved_constraint.as_ref().unwrap_or(constraint);

                let constraint = self
                    .normalize(Some(span), Cow::Borrowed(constraint), Default::default())
                    .context("failed to expand intrinsic in type parameters")?
                    .freezed()
                    .into_owned()
                    .freezed();

                let arg = Type::Param(TypeParam {
                    span: *param_span,
                    name: name.clone(),
                    constraint: Some(box constraint),
                    default: default.clone(),
                    metadata: *metadata,
                    tracker: Default::default(),
                });

                return Ok(Type::StringMapping(StringMapping {
                    span,
                    kind: ty.kind.clone(),
                    type_args: TypeParamInstantiation {
                        span: ty.type_args.span,
                        params: vec![arg],
                    },
                    metadata: ty.metadata,
                }));
            }

            _ => {}
        }

        Ok(Type::StringMapping(ty.clone()))
    }

    pub(crate) fn report_error_for_unresolved_type(
        &mut self,
        span: Span,
        type_name: &RExpr,
        type_args: Option<&TypeParamInstantiation>,
    ) -> VResult<()> {
        if self.config.is_builtin {
            return Ok(());
        }

        let _tracing = dev_span!("report_error_for_unresolved_type");

        let l = left_of_expr(type_name);
        let l = match l {
            Some(v) => v,
            _ => return Ok(()),
        };
        let top_id: Id = l.into();

        let is_resolved = self.data.bindings.types.contains(&top_id)
            || self.data.imports_by_id.contains_key(&top_id)
            || self.data.unresolved_imports.contains(&top_id)
            || self.env.get_global_type(l.span, top_id.sym()).is_ok();

        if is_resolved {
            return Ok(());
        }
        let span = l.span.or_else(|| span);
        let name = Name::try_from(type_name);

        let name = match name {
            Ok(v) => v,
            _ => return Ok(()),
        };

        match type_name {
            RExpr::Member(_) => {
                if let Ok(var) = self.type_of_var(l, TypeOfMode::RValue, None) {
                    if var.is_module() {
                        return Ok(());
                    }
                }

                Err(ErrorKind::NamespaceNotFound {
                    span,
                    name: box name,
                    ctxt: self.ctx.module_id,
                    type_args: type_args.cloned().map(Box::new),
                }
                .into())
            }
            RExpr::Ident(i) if &*i.sym == "globalThis" => Ok(()),
            RExpr::Ident(_) => Err(ErrorKind::TypeNotFound {
                span,
                name: box name,
                ctxt: self.ctx.module_id,
                type_args: type_args.cloned().map(Box::new),
            }
            .into()),
            _ => Ok(()),
        }
    }

    /// Utility method to convert a class member to a type element.
    ///
    /// This method is used while inferring types and while assigning
    /// type element to class member or vice versa.
    #[inline]
    pub(super) fn make_type_el_from_class_member(&self, member: &ClassMember, static_mode: bool) -> Option<TypeElement> {
        Some(match member {
            ClassMember::Constructor(c) => TypeElement::Constructor(c.clone()),
            ClassMember::Method(m) => {
                if m.is_static != static_mode {
                    return None;
                }

                TypeElement::Method(MethodSignature {
                    span: m.span.with_ctxt(SyntaxContext::empty()),
                    accessibility: m.accessibility,
                    readonly: false,
                    key: m.key.clone(),
                    optional: m.is_optional,
                    params: m.params.clone(),
                    ret_ty: Some(m.ret_ty.clone()),
                    type_params: m.type_params.clone(),
                    metadata: Default::default(),
                })
            }
            ClassMember::Property(p) => {
                if p.is_static != static_mode {
                    return None;
                }

                TypeElement::Property(PropertySignature {
                    span: p.span.with_ctxt(SyntaxContext::empty()),
                    accessibility: p.accessibility,
                    readonly: p.readonly,
                    key: p.key.clone(),
                    optional: p.is_optional,
                    params: vec![],
                    type_ann: p.value.clone(),
                    type_params: None,
                    metadata: Default::default(),
                    accessor: p.accessor,
                })
            }
            ClassMember::IndexSignature(i) => TypeElement::Index(i.clone()),
        })
    }

    /// Exclude `excluded` from `ty`
    ///
    /// # Subclasses
    ///
    /// ```ts
    /// class B {}
    /// class P {}
    /// class C extends P {}
    ///
    /// declare let a: C | B
    ///
    ///
    /// if (!(a instanceof P)) {
    ///     // At here, we can deduce that `a` is `B`.
    ///     // To use the fact that `a` is not `P`, we check for the parent type of `ty
    /// }
    /// ```
    fn exclude_type(&mut self, span: Span, ty: &mut Type, excluded: &Type) {
        let span = span.with_ctxt(SyntaxContext::empty());

        if ty.type_eq(excluded) {
            *ty = Type::never(
                ty.span(),
                KeywordTypeMetadata {
                    common: ty.metadata(),
                    ..Default::default()
                },
            );
            return;
        }

        let res = self.normalize(Some(span), Cow::Borrowed(excluded), Default::default());
        let excluded = match res {
            Ok(v) => v,
            Err(..) => Cow::Borrowed(excluded),
        };

        if let Type::Union(excluded) = excluded.normalize() {
            //
            for excluded in &excluded.types {
                self.exclude_type(span, ty, excluded)
            }

            return;
        }

        // TODO(kdy1): PERF
        match ty.normalize_mut() {
            Type::Union(ty) => {
                for ty in &mut ty.types {
                    self.exclude_type(span, ty, &excluded);
                }
                ty.types.retain(|element| !element.is_never());
            }
            Type::Intersection(ty) => {
                for ty in &mut ty.types {
                    self.exclude_type(span, ty, &excluded);
                }
                ty.types.retain(|element| !element.is_unknown());
            }
            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => {
                if constraint.is_unknown() {
                    let mut constraint_temp = constraint.clone();
                    self.exclude_type(span, &mut constraint_temp, &excluded);
                    *ty = Type::new_intersection(span, [ty.clone(), *constraint_temp]);
                } else {
                    self.exclude_type(span, constraint, &excluded);
                    if constraint.is_never() {
                        *ty = Type::never(span, Default::default());
                    }
                }
            }

            Type::Class(cls) => {
                //
                if let Some(super_def) = &cls.def.super_class {
                    if let Ok(mut super_instance) = self.instantiate_class(cls.span, super_def) {
                        self.exclude_type(span, &mut super_instance, &excluded);
                        if super_instance.is_never() {
                            *ty = Type::never(
                                cls.span,
                                KeywordTypeMetadata {
                                    common: cls.metadata.common,
                                    ..Default::default()
                                },
                            );
                        }
                    }
                }
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                span,
                ..
            }) => {
                if !self.rule().strict_null_checks {
                    return;
                }

                let mut unknown = vec![
                    Type::Keyword(KeywordType {
                        span: *span,
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                    Type::Keyword(KeywordType {
                        span: *span,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                    Type::TypeLit(TypeLit {
                        span: *span,
                        members: vec![],
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                ];
                unknown.retain(|ty| !ty.type_eq(&excluded));
                if unknown.len() == 3 {
                    return;
                }
                *ty = Type::new_union(*span, unknown)
            }
            _ => {}
        }
    }

    fn exclude_types(&mut self, span: Span, ty: &mut Type, excludes: Option<Vec<Type>>) {
        ty.freeze();

        let mapped_ty = self.normalize(
            Some(span),
            Cow::Borrowed(&*ty),
            NormalizeTypeOpts {
                // `typeof` can  result in a stack overflow, because it calls `type_of_var`.
                preserve_typeof: true,
                ..Default::default()
            },
        );
        let mut mapped_ty = match mapped_ty {
            Ok(v) => v,
            Err(_) => Cow::Borrowed(&*ty),
        };

        let excludes = match excludes {
            Some(v) => v,
            None => return,
        };

        for excluded in excludes {
            self.exclude_type(span, ALLOW_DEEP_CLONE.set(&(), || mapped_ty.to_mut()), &excluded);
        }

        *ty = ALLOW_DEEP_CLONE.set(&(), || mapped_ty.into_owned());
        ty.fix();
    }

    /// We precompute all type declarations in the scope, using this
    /// method.
    pub(crate) fn fill_known_type_names<N>(&mut self, node: &N)
    where
        N: Send + Sync + for<'aa> VisitWith<BindingCollector<'aa>> + VisitWith<KnownTypeVisitor>,
    {
        if self.config.is_builtin {
            return;
        }
        if self.data.bindings.collected {
            return;
        }

        self.data.bindings = collect_bindings(node);
    }
}

pub(crate) fn left_of_expr(t: &RExpr) -> Option<&RIdent> {
    match t {
        RExpr::Member(t) => left_of_expr(&t.obj),
        RExpr::Ident(i) => Some(i),

        _ => None,
    }
}

fn apply_string_mapping<T: AsRef<str>>(intrinsic: &IntrinsicKind, raw: T) -> Atom {
    let raw = raw.as_ref();

    match intrinsic {
        IntrinsicKind::Uppercase => raw.to_ascii_uppercase(),
        IntrinsicKind::Lowercase => raw.to_ascii_lowercase(),
        IntrinsicKind::Capitalize => {
            if raw.is_empty() {
                "".into()
            } else {
                let mut res = String::new();
                let mut chars = raw.chars();

                res.extend(chars.next().into_iter().map(|v| v.to_ascii_uppercase()));
                res.push_str(chars.as_str());

                res
            }
        }
        IntrinsicKind::Uncapitalize => {
            if raw.is_empty() {
                "".into()
            } else {
                let mut res = String::new();
                let mut chars = raw.chars();

                res.extend(chars.next().into_iter().map(|v| v.to_ascii_lowercase()));
                res.push_str(chars.as_str());

                res
            }
        }
    }
    .into()
}
