use crate::{
    analyzer::{expr::TypeOfMode, generic::ExtendsOpts, scope::ExpandOpts, Analyzer, Ctx},
    type_facts::TypeFacts,
    util::unwrap_ref_with_single_arg,
    ValidationResult,
};
use fxhash::FxHashMap;
use itertools::Itertools;
use rnode::{VisitMutWith, VisitWith};
use stc_ts_ast_rnode::{RExpr, RIdent, RInvalid, RNumber, RStr, RTsEntityName, RTsLit};
use stc_ts_base_type_ops::bindings::{collect_bindings, BindingCollector, KnownTypeVisitor};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt, Error};
use stc_ts_generics::ExpandGenericOpts;
use stc_ts_type_ops::{tuple_normalization::TupleNormalizer, Fix};
use stc_ts_types::{
    name::Name, Accessor, Array, Class, ClassDef, ClassMember, ClassMetadata, ComputedKey, Conditional,
    ConditionalMetadata, ConstructorSignature, Id, IdCtx, IndexedAccessType, Instance, InstanceMetadata, Intersection,
    IntersectionMetadata, Intrinsic, IntrinsicKind, Key, KeywordType, KeywordTypeMetadata, LitType, LitTypeMetadata,
    MethodSignature, ModuleId, Operator, PropertySignature, QueryExpr, Ref, ThisType, ThisTypeMetadata, Tuple,
    TupleElement, Type, TypeElement, TypeLit, TypeLitMetadata, TypeParam, TypeParamInstantiation, Union,
};
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    debug_ctx,
    ext::{SpanExt, TypeVecExt},
    stack, TryOpt,
};
use std::{borrow::Cow, collections::HashMap};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::{TsKeywordTypeKind, TsTypeOperatorOp};
use tracing::{debug, error, instrument, span, Level};

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

    //// If `true`, we will not expand generics.
    pub process_only_key: bool,
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
    /// TOOD: Accept [Cow] to optimize performance

    ///
    /// # Span
    ///
    /// If `span` is provided, it will be used for types **created** by the
    /// method. Otherwise the span of the original type is used.
    pub(crate) fn normalize<'a>(
        &mut self,
        span: Option<Span>,
        mut ty: Cow<'a, Type>,
        opts: NormalizeTypeOpts,
    ) -> ValidationResult<Cow<'a, Type>> {
        let _tracing = if cfg!(debug_assertions) {
            let ty_str = dump_type_as_string(&self.cm, &ty);

            Some(span!(Level::ERROR, "normalize", ty = &*ty_str).entered())
        } else {
            None
        };

        let res = (|| {
            ty.assert_valid();

            let actual_span = span.unwrap_or_else(|| ty.span());
            if !self.is_builtin {
                debug_assert!(
                    !actual_span.is_dummy(),
                    "Cannot normalize a type with dummy span\n{:?}",
                    ty
                );
            }

            let _stack = stack::track(actual_span)?;
            let _context = debug_ctx!(format!("Normalize: {}", dump_type_as_string(&self.cm, &ty)));

            match ty.normalize() {
                Type::Lit(..)
                | Type::TypeLit(..)
                | Type::Interface(..)
                | Type::Class(..)
                | Type::ClassDef(..)
                | Type::Tuple(..)
                | Type::Function(..)
                | Type::Constructor(..)
                | Type::EnumVariant(..)
                | Type::Enum(..)
                | Type::Param(_)
                | Type::Module(_)
                | Type::Tpl(..) => return Ok(ty),
                _ => {}
            }

            if ty.is_arc() {
                let ty = self.normalize(span, Cow::Borrowed(ty.normalize()), opts)?.into_owned();

                return Ok(Cow::Owned(ty));
            }

            if matches!(
                ty.normalize(),
                Type::Conditional(..) | Type::Array(..) | Type::IndexedAccessType(..) | Type::Mapped(..)
            ) {
                ty.make_clone_cheap();
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

                        new_ty.make_clone_cheap();

                        return Ok(Cow::Owned(self.normalize(span, new_ty, opts)?.into_owned()));
                    }

                    Type::Keyword(k) => {
                        if opts.normalize_keywords {
                            let name = match k.kind {
                                TsKeywordTypeKind::TsNumberKeyword => Some(js_word!("Number")),
                                TsKeywordTypeKind::TsObjectKeyword => Some(js_word!("Number")),
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
                        return Ok(Cow::Owned(
                            self.normalize(span, Cow::Borrowed(&a.ty), opts)?.into_owned(),
                        ));
                    }

                    Type::Intrinsic(i) => {
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
                        })));
                    }

                    // Not normalizable.
                    Type::Infer(_) | Type::StaticThis(_) | Type::This(_) => {}

                    // Maybe it can be changed in future, but currently noop
                    Type::Union(_) => {}

                    Type::Intersection(ty) => {
                        let is_str = ty.types.iter().any(|ty| ty.is_str());
                        let is_num = ty.types.iter().any(|ty| ty.is_num());
                        let is_bool = ty.types.iter().any(|ty| ty.is_bool());

                        if u32::from(is_str) + u32::from(is_num) + u32::from(is_bool) >= 2 {
                            return Ok(Cow::Owned(Type::Keyword(KeywordType {
                                span: ty.span,
                                kind: TsKeywordTypeKind::TsNeverKeyword,
                                metadata: KeywordTypeMetadata {
                                    common: ty.metadata.common,
                                },
                            })));
                        }
                    }

                    Type::Conditional(c) => {
                        let mut check_type = self
                            .normalize(span, Cow::Borrowed(&c.check_type), Default::default())
                            .context("tried to normalize the `check` type of a conditional type")?
                            .into_owned();
                        check_type.make_clone_cheap();

                        let mut extends_type = self
                            .normalize(span, Cow::Borrowed(&c.extends_type), Default::default())
                            .context("tried to normalize the `extends` type of a conditional type")?;

                        extends_type.make_clone_cheap();

                        if let Some(v) = self.extends(ty.span(), Default::default(), &check_type, &extends_type) {
                            let ty = if v { &c.true_type } else { &c.false_type };
                            // TODO(kdy1): Optimize
                            let ty = self
                                .normalize(span, Cow::Borrowed(&ty), opts)
                                .context("tried to normalize the calculated type of a conditional type")?
                                .into_owned();
                            return Ok(Cow::Owned(ty));
                        }

                        match check_type.normalize() {
                            Type::Param(TypeParam {
                                name,
                                constraint: Some(check_type_constraint),
                                ..
                            }) => {
                                let new_type = self
                                    .reduce_conditional_type(
                                        c.span,
                                        &check_type,
                                        &check_type_constraint,
                                        &extends_type,
                                        &c.true_type,
                                        &c.false_type,
                                        c.metadata,
                                    )
                                    .context("tried to reduce conditional type")?;

                                if let Some(new_type) = new_type {
                                    return self.normalize(span, Cow::Owned(new_type), opts);
                                }
                            }
                            _ => {}
                        }

                        match check_type.normalize() {
                            Type::Union(check_type_union) => {
                                let mut all = true;
                                let mut types = vec![];
                                for check_type in &check_type_union.types {
                                    let res = self.extends(ty.span(), Default::default(), &check_type, &extends_type);
                                    if let Some(v) = res {
                                        if v {
                                            if !c.true_type.is_never() {
                                                types.push(check_type.clone());
                                            }
                                        } else {
                                            if !c.false_type.is_never() {
                                                types.push(check_type.clone());
                                            }
                                        }
                                    } else {
                                        all = false;
                                        break;
                                    }
                                }

                                if all {
                                    let new = Type::Union(Union {
                                        span: actual_span.with_ctxt(SyntaxContext::empty()),
                                        types,
                                        metadata: Default::default(),
                                    })
                                    .fixed();

                                    new.assert_valid();

                                    return Ok(Cow::Owned(new));
                                }
                            }
                            _ => {}
                        }

                        // TOOD: Optimize
                        // If we can calculate type using constraints, do so.

                        // TODO(kdy1): PERF
                        match check_type.normalize_mut() {
                            Type::Param(TypeParam {
                                name,
                                constraint: Some(check_type_constraint),
                                ..
                            }) => {
                                // We removes unmatchable constraints.
                                // It means, for
                                //
                                // T: a type param extends string | undefined
                                // A: T extends null | undefined ? never : T
                                //
                                // We removes `undefined` from parents of T.

                                match check_type_constraint.normalize() {
                                    Type::Union(check_type_union) => {
                                        let mut all = true;
                                        let mut types = vec![];
                                        for check_type in &check_type_union.types {
                                            let res =
                                                self.extends(ty.span(), Default::default(), &check_type, &extends_type);
                                            if let Some(v) = res {
                                                if v {
                                                    if !c.true_type.is_never() {
                                                        types.push(check_type.clone());
                                                    }
                                                } else {
                                                    if !c.false_type.is_never() {
                                                        types.push(check_type.clone());
                                                    }
                                                }
                                            } else {
                                                all = false;
                                                break;
                                            }
                                        }

                                        if all {
                                            types.dedup_type();
                                            let new = Type::Union(Union {
                                                span: actual_span.with_ctxt(SyntaxContext::empty()),
                                                types,
                                                metadata: Default::default(),
                                            });

                                            *check_type_constraint = box new;

                                            let mut params = HashMap::default();
                                            params.insert(
                                                name.clone(),
                                                ALLOW_DEEP_CLONE.set(&(), || check_type.clone().fixed().cheap()),
                                            );
                                            let c = self.expand_type_params(&params, c.clone(), Default::default())?;
                                            let c = Type::Conditional(c);
                                            c.assert_valid();

                                            return Ok(Cow::Owned(c));
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }

                    Type::Query(q) => {
                        if !opts.preserve_typeof {
                            match &*q.expr {
                                QueryExpr::TsEntityName(e) => {
                                    let expanded_ty = self
                                        .resolve_typeof(actual_span, e)
                                        .context("tried to resolve typeof as a part of normalization")?;

                                    if ty.type_eq(&expanded_ty) {
                                        return Ok(Cow::Owned(Type::any(
                                            actual_span.with_ctxt(SyntaxContext::empty()),
                                            Default::default(),
                                        )));
                                    }

                                    if expanded_ty.normalize().is_query() {
                                        panic!(
                                            "normalize: resolve_typeof returned a query type: {}",
                                            dump_type_as_string(&self.cm, &expanded_ty)
                                        )
                                    }

                                    return Ok(self
                                        .normalize(span, Cow::Owned(expanded_ty), opts)
                                        .context("tried to normalize the type returned from typeof")?);
                                }
                                QueryExpr::Import(_) => {}
                            }
                        }
                        // TODO
                    }

                    Type::Instance(ty) => {
                        let ty = self
                            .instantiate_for_normalization(span, &ty.ty)
                            .context("tried to instantiate for normalizations")?;
                        ty.assert_valid();
                        return Ok(Cow::Owned(ty));
                    }

                    Type::Import(_) => {}

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
                            diallow_unknown_object_property: true,
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
                            Default::default(),
                        );

                        if let Ok(prop_ty) = prop_ty {
                            if ty.type_eq(&prop_ty) {
                                return Ok(ty);
                            }

                            let _context =
                                debug_ctx!(format!("Property type: {}", dump_type_as_string(&self.cm, &prop_ty)));

                            match prop_ty.normalize() {
                                Type::IndexedAccessType(prop_ty) => match prop_ty.index_type.normalize() {
                                    Type::Param(..) => {}
                                    _ => {
                                        panic!("{:?}", prop_ty);
                                    }
                                },
                                _ => {}
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
                        })));
                    }

                    Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty,
                        ..
                    }) => {
                        let keys_ty = self
                            .keyof(actual_span, &ty)
                            .context("tried to get keys of a type as a part of normalization")?;
                        keys_ty.assert_valid();
                        return Ok(Cow::Owned(keys_ty));
                    }

                    Type::Operator(_) => {
                        // TODO(kdy1):
                    }

                    _ => {}
                }
            }

            Ok(ty)
        })();

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
    ) -> ValidationResult<Option<Type>> {
        if !check_type.normalize().is_type_param() {
            return Ok(None);
        }
        let span = span.with_ctxt(SyntaxContext::empty());
        let mut worked = false;

        let mut true_type = self.normalize(Some(span), Cow::Borrowed(true_type), Default::default())?;
        let mut false_type = self.normalize(Some(span), Cow::Borrowed(false_type), Default::default())?;

        match true_type.normalize() {
            Type::Conditional(c) => {
                if (*c.check_type).type_eq(check_type) {
                    if let Some(ty) = self.reduce_conditional_type(
                        span,
                        check_type,
                        &extends_type,
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
            _ => {}
        }

        match false_type.normalize() {
            Type::Conditional(c) => {
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
            _ => {}
        }

        match check_type_constraint.normalize() {
            Type::Union(check_type_union) => {
                //
                let can_match = check_type_union.types.iter().any(|check_type_contraint| {
                    self.extends(span, Default::default(), check_type_contraint, extends_type)
                        .unwrap_or(true)
                });

                if !can_match {
                    return Ok(Some(Type::never(span, Default::default())));
                }
            }
            _ => {
                //
                if let Some(extends) = self.extends(
                    span,
                    ExtendsOpts { ..Default::default() },
                    &check_type_constraint,
                    extends_type,
                ) {
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
            })))
        } else {
            Ok(None)
        }
    }

    // This is part of normalization.
    fn instantiate_for_normalization(&mut self, span: Option<Span>, ty: &Type) -> ValidationResult<Type> {
        let mut ty = self.normalize(
            span,
            Cow::Borrowed(ty),
            NormalizeTypeOpts {
                normalize_keywords: false,
                ..Default::default()
            },
        )?;
        ty.make_clone_cheap();
        let metadata = ty.metadata();
        let actual_span = ty.span();

        // TODO(kdy1): PERF
        let ty = ty.into_owned().foldable();

        Ok(match ty {
            // For self-references in classes, we preserve `instanceof` type.
            Type::Ref(..) => Type::Instance(Instance {
                span: actual_span,
                ty: box ty,
                metadata: InstanceMetadata {
                    common: metadata,
                    ..Default::default()
                },
            }),

            Type::ClassDef(def) => Type::Class(Class {
                span: actual_span,
                def: box def,
                metadata: ClassMetadata {
                    common: metadata,
                    ..Default::default()
                },
            }),

            Type::StaticThis(ty) => Type::This(ThisType {
                span: actual_span,
                metadata: ThisTypeMetadata {
                    common: metadata,
                    ..Default::default()
                },
            }),

            Type::Intersection(ty) => {
                let types = ty
                    .types
                    .into_iter()
                    .map(|ty| self.instantiate_for_normalization(span, &ty))
                    .collect::<Result<_, _>>()?;

                Type::Intersection(Intersection { types, ..ty }).fixed()
            }

            Type::Union(ty) => {
                let types = ty
                    .types
                    .into_iter()
                    .map(|ty| self.instantiate_for_normalization(span, &ty))
                    .collect::<Result<_, _>>()?;

                Type::Union(Union { types, ..ty }).fixed()
            }

            Type::Array(ty) => {
                let elem_type = box self.instantiate_for_normalization(span, &ty.elem_type)?;
                Type::Array(Array { elem_type, ..ty })
            }

            Type::Tuple(ty) => {
                let elems = ty
                    .elems
                    .into_iter()
                    .map(|e| -> ValidationResult<_> {
                        let ty = box self.instantiate_for_normalization(span, &e.ty)?;
                        Ok(TupleElement { ty, ..e })
                    })
                    .collect::<Result<_, _>>()?;

                Type::Tuple(Tuple { elems, ..ty })
            }

            _ => ty,
        })
    }

    pub(crate) fn report_possibly_null_or_undefined(&mut self, span: Span, ty: &Type) -> ValidationResult<()> {
        let ty = self
            .normalize(Some(span), Cow::Borrowed(ty), Default::default())
            .context("tried to normalize to see if it can be undefined")?;

        if ty.is_str() || ty.is_bool() || ty.is_num() || ty.is_lit() {
            return Ok(());
        }

        if ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
            return Err(Error::ObjectIsPossiblyUndefined { span });
        }
        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
            return Err(Error::ObjectIsPossiblyNull { span });
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
            | Type::Param(..) => Ok(()),
            Type::Union(ty) => {
                let has_null = ty.types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsNullKeyword));
                let has_undefined = ty.types.iter().any(|ty| {
                    ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
                });

                // tsc is crazy. It uses different error code for these errors.
                if has_null && has_undefined {
                    return Err(Error::ObjectIsPossiblyNullOrUndefined { span });
                }

                if has_null {
                    return Err(Error::ObjectIsPossiblyNull { span });
                }

                if has_undefined {
                    return Err(Error::ObjectIsPossiblyUndefined { span });
                }

                Ok(())
            }
            _ => Err(Error::ObjectIsPossiblyUndefinedWithType {
                span,
                ty: box ty.into_owned(),
            }),
        }
    }

    #[instrument(skip(self, span, ty))]
    pub(crate) fn can_be_undefined(&mut self, span: Span, ty: &Type) -> ValidationResult<bool> {
        let ty = self
            .normalize(Some(span), Cow::Borrowed(ty), Default::default())
            .context("tried to normalize to see if it can be undefined")?;

        if ty.is_str() || ty.is_bool() || ty.is_num() || ty.is_lit() {
            return Ok(false);
        }

        if ty.is_any()
            || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
            || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
        {
            return Ok(true);
        }

        Ok(match &*ty {
            Type::Class(..)
            | Type::ClassDef(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Keyword(..)
            | Type::Lit(..) => false,
            Type::Union(ty) => {
                for ty in &ty.types {
                    if self.can_be_undefined(span, ty)? {
                        return Ok(true);
                    }
                }

                false
            }
            _ => true,
        })
    }

    #[instrument(skip(self, span, ty))]
    pub(crate) fn expand_type_ann<'a>(
        &mut self,
        span: Span,
        ty: Option<&'a Type>,
    ) -> ValidationResult<Option<Cow<'a, Type>>> {
        let ty = match ty {
            Some(v) => v,
            None => return Ok(None),
        };
        let span = span.with_ctxt(SyntaxContext::empty());

        let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default())?;

        Ok(Some(ty))
    }

    #[instrument(skip(self, def))]
    pub(crate) fn create_prototype_of_class_def(&mut self, def: &ClassDef) -> ValidationResult<TypeLit> {
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
                            .map(|ty| {
                                self.expand_type_params(&type_params, *ty, Default::default())
                                    .map(Box::new)
                            })
                            .try_opt()?;
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
        })
    }

    /// Exclude types from `ty` using type facts with key `name`, for the
    /// current scope.
    #[instrument(skip(self, span, name, ty))]
    pub(crate) fn exclude_types_using_fact(&mut self, span: Span, name: &Name, ty: &mut Type) {
        debug_assert!(!span.is_dummy(), "exclude_types should not be called with a dummy span");

        let mut types_to_exclude = vec![];
        let mut s = Some(&self.scope);

        while let Some(scope) = s {
            types_to_exclude.extend(scope.facts.excludes.get(&name).cloned().into_iter().flatten());
            s = scope.parent();
        }

        types_to_exclude.extend(
            self.cur_facts
                .true_facts
                .excludes
                .get(&name)
                .cloned()
                .into_iter()
                .flatten(),
        );

        let before = dump_type_as_string(&self.cm, &ty);
        self.exclude_types(span, ty, Some(types_to_exclude));
        let after = dump_type_as_string(&self.cm, &ty);

        debug!("[types/facts] Excluded types: {} => {}", before, after);
    }

    #[instrument(skip(self, name, ty))]
    pub(crate) fn apply_type_facts(&mut self, name: &Name, ty: Type) -> Type {
        let type_facts = self.scope.get_type_facts(&name)
            | self
                .cur_facts
                .true_facts
                .facts
                .get(&name)
                .copied()
                .unwrap_or(TypeFacts::None);

        debug!("[types/fact] Facts for {:?} is {:?}", name, type_facts);

        self.apply_type_facts_to_type(type_facts, ty)
    }

    /// Collect all class members, including inherited members.
    ///
    /// # Parmeters
    ///
    /// ## excluded
    ///
    /// Memebers of base class.
    #[instrument(skip(self, excluded, ty))]
    pub(crate) fn collect_class_members(
        &mut self,
        excluded: &[&ClassMember],
        ty: &Type,
    ) -> ValidationResult<Option<Vec<ClassMember>>> {
        if self.is_builtin {
            return Ok(None);
        }

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

                        if let Some(super_members) = self.collect_class_members(&excluded, &sc)? {
                            members.extend(super_members)
                        }

                        return Ok(Some(members));
                    }
                    None => {
                        return Ok(Some(members));
                    }
                }
            }
            Type::Class(c) => self.collect_class_members(excluded, &Type::ClassDef(*c.def.clone())),
            _ => {
                error!("unimplemented: collect_class_members: {:?}", ty);
                return Ok(None);
            }
        }
    }

    /// Note: `span` is only used while expanding type (to prevent panic) in the
    /// case of [Type::Ref].
    pub(crate) fn convert_type_to_type_lit<'a>(
        &mut self,
        span: Span,
        ty: Cow<'a, Type>,
    ) -> ValidationResult<Option<Cow<'a, TypeLit>>> {
        let span = span.with_ctxt(SyntaxContext::empty());

        let _ctx = debug_ctx!(format!("type_to_type_lit: {:?}", ty));

        debug_assert!(!span.is_dummy(), "type_to_type_lit: `span` should not be dummy");

        if ty.normalize().is_type_lit() {
            match ty {
                Cow::Owned(ty) => {
                    let t = ty.foldable().expect_type_lit();
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

        if ty.normalize().is_interface() {
            let t = ty.into_owned().foldable().expect_interface();
            let mut members = vec![];

            for parent in &t.extends {
                let parent = self.type_of_ts_entity_name(
                    parent.span(),
                    self.ctx.module_id,
                    &parent.expr,
                    parent.type_args.as_deref(),
                )?;

                let super_els = self.convert_type_to_type_lit(span, Cow::Owned(parent))?;

                members.extend(super_els.into_iter().map(Cow::into_owned).flat_map(|v| v.members))
            }

            // TODO(kdy1): Override
            members.extend(t.body);
            return Ok(Some(Cow::Owned(TypeLit {
                span: t.span,
                members,
                metadata: TypeLitMetadata {
                    inexact: true,
                    ..Default::default()
                },
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
                            ctxt: ModuleId::builtin(),
                            type_name: RTsEntityName::Ident(RIdent::new(name, span)),
                            type_args: None,
                            metadata: Default::default(),
                        })),
                    )?
                    .map(Cow::into_owned)
                    .map(Cow::Owned));
            }

            Type::Ref(..) => {
                let ty = self.expand_top_ref(span, Cow::Borrowed(ty), Default::default())?;
                return self
                    .convert_type_to_type_lit(span, ty)
                    .map(|o| o.map(Cow::into_owned).map(Cow::Owned));
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
                    members.extend(self.make_type_el_from_class_member(member, false)?);
                }

                Cow::Owned(TypeLit {
                    span: c.span,
                    members,
                    metadata: TypeLitMetadata { ..Default::default() },
                })
            }

            Type::ClassDef(c) => {
                let mut members = vec![];
                if let Some(super_class) = &c.super_class {
                    let super_els = self.convert_type_to_type_lit(span, Cow::Borrowed(&super_class))?;
                    members.extend(super_els.map(|ty| ty.into_owned().members).into_iter().flatten());
                }

                // TODO(kdy1): Override

                for member in &c.body {
                    members.extend(self.make_type_el_from_class_member(member, true)?);
                }

                Cow::Owned(TypeLit {
                    span: c.span,
                    members,
                    metadata: TypeLitMetadata { ..Default::default() },
                })
            }

            Type::Intersection(t) => {
                let mut members = vec![];
                for ty in &t.types {
                    let opt = self.convert_type_to_type_lit(span, Cow::Borrowed(ty))?;
                    members.extend(opt.into_iter().map(Cow::into_owned).flat_map(|v| v.members));
                }

                Cow::Owned(TypeLit {
                    span: t.span,
                    members,
                    metadata: Default::default(),
                })
            }

            Type::Alias(ty) => {
                return Ok(self
                    .convert_type_to_type_lit(span, Cow::Borrowed(&ty.ty))?
                    .map(Cow::into_owned)
                    .map(Cow::Owned))
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
                })
            }

            Type::Mapped(m) => {
                let ty = self.expand_mapped(span, m)?;
                if let Some(ty) = ty {
                    let ty = self
                        .convert_type_to_type_lit(span, Cow::Owned(ty))?
                        .map(Cow::into_owned)
                        .map(Cow::Owned);

                    match ty {
                        Some(v) => v,
                        None => return Ok(None),
                    }
                } else {
                    return Ok(None);
                }
            }

            Type::Query(..) => {
                // TODO(kdy1): Optimize
                let ty = self
                    .normalize(None, Cow::Borrowed(ty), Default::default())
                    .context("tried to normalize a type to convert it to type literal")?;
                let ty = self
                    .convert_type_to_type_lit(span, ty)
                    .context("tried to convert a normalized type to type liteal")?
                    .map(Cow::into_owned)
                    .map(Cow::Owned);
                return Ok(ty);
            }

            _ => {
                error!("unimplemented: type_to_type_lit: {:?}", ty);
                return Ok(None);
            }
        }))
    }

    ///
    /// - `Promise<T>` => `T`
    /// - `T | PromiseLike<T>` => `T`
    pub(crate) fn normalize_promise_arg<'a>(&mut self, arg: &'a Type) -> Cow<'a, Type> {
        if let Some(arg) = unwrap_ref_with_single_arg(&arg, "Promise") {
            return self.normalize_promise_arg(&arg);
        }

        match arg.normalize() {
            Type::Union(u) => {
                // Part of `Promise<T | PromiseLike<T>> => Promise<T>`
                if u.types.len() == 2 {
                    let first = u.types[0].normalize();
                    let second = u.types[1].normalize();

                    if let Some(second_arg) = unwrap_ref_with_single_arg(&second, "PromiseLike") {
                        if second_arg.type_eq(first) {
                            return Cow::Borrowed(first);
                        }
                    }
                }
            }
            _ => {}
        }

        Cow::Borrowed(arg)
    }

    pub(crate) fn normalize_tuples(&mut self, ty: &mut Type) {
        let marks = self.marks();

        ty.visit_mut_with(&mut TupleNormalizer);
        ty.fix();
    }

    /// This is used to determine `form` of `els`. Each type has a value. e.g.
    /// `1` for [TypeElement::Call].
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

    pub(crate) fn expand_intrinsic_types(&mut self, span: Span, ty: &Intrinsic) -> ValidationResult {
        let arg = &ty.type_args;

        match ty.kind {
            IntrinsicKind::Uppercase
            | IntrinsicKind::Lowercase
            | IntrinsicKind::Capitalize
            | IntrinsicKind::Uncapitalize => match arg.params[0].normalize() {
                Type::Lit(LitType {
                    lit: RTsLit::Str(s), ..
                }) => {
                    let new_val = match ty.kind {
                        IntrinsicKind::Uppercase => s.value.to_uppercase(),
                        IntrinsicKind::Lowercase => s.value.to_lowercase(),
                        IntrinsicKind::Capitalize => {
                            if s.value.is_empty() {
                                "".into()
                            } else {
                                let mut res = String::new();
                                let mut chars = s.value.chars();

                                res.extend(chars.next().into_iter().flat_map(|v| v.to_uppercase()));
                                res.push_str(chars.as_str());

                                res
                            }
                        }
                        IntrinsicKind::Uncapitalize => {
                            if s.value.is_empty() {
                                "".into()
                            } else {
                                let mut res = String::new();
                                let mut chars = s.value.chars();

                                res.extend(chars.next().into_iter().flat_map(|v| v.to_lowercase()));
                                res.push_str(chars.as_str());

                                res
                            }
                        }
                    };

                    return Ok(Type::Lit(LitType {
                        span: arg.params[0].span(),
                        lit: RTsLit::Str(RStr {
                            span: arg.params[0].span(),
                            value: new_val.into(),
                            has_escape: false,
                            kind: Default::default(),
                        }),
                        metadata: LitTypeMetadata {
                            common: arg.params[0].metadata(),
                            ..Default::default()
                        },
                    }));
                }

                _ => {}
            },
        }

        Ok(Type::Intrinsic(ty.clone()))
    }

    #[instrument(skip(self, span, type_name, type_args))]
    pub(crate) fn report_error_for_unresolve_type(
        &mut self,
        span: Span,
        type_name: &RTsEntityName,
        type_args: Option<&TypeParamInstantiation>,
    ) -> ValidationResult<()> {
        if self.is_builtin {
            return Ok(());
        }

        let l = left(&type_name);
        let top_id: Id = l.into();

        let is_resolved = self.data.bindings.types.contains(&top_id)
            || self.imports_by_id.contains_key(&top_id)
            || self.data.unresolved_imports.contains(&top_id)
            || self.env.get_global_type(l.span, &top_id.sym()).is_ok();

        if is_resolved {
            return Ok(());
        }
        let span = l.span.or_else(|| span);

        match type_name {
            RTsEntityName::TsQualifiedName(_) => {
                if let Ok(var) = self.type_of_var(&l, TypeOfMode::RValue, None) {
                    if var.normalize().is_module() {
                        return Ok(());
                    }
                }

                Err(Error::NamspaceNotFound {
                    span,
                    name: box type_name.clone().into(),
                    ctxt: self.ctx.module_id,
                    type_args: type_args.cloned().map(Box::new),
                })
            }
            RTsEntityName::Ident(i) if &*i.sym == "globalThis" => return Ok(()),
            RTsEntityName::Ident(_) => Err(Error::TypeNotFound {
                span,
                name: box type_name.clone().into(),
                ctxt: self.ctx.module_id,
                type_args: type_args.cloned().map(Box::new),
            }),
        }
    }
    /// Utility method to convert a class member to a type element.
    ///
    /// This method is used while inferring types and while assigning type
    /// element to class member or vice versa.
    #[inline]
    pub(super) fn make_type_el_from_class_member(
        &self,
        member: &ClassMember,
        static_mode: bool,
    ) -> ValidationResult<Option<TypeElement>> {
        Ok(Some(match member {
            ClassMember::Constructor(c) => TypeElement::Constructor(c.clone()),
            ClassMember::Method(m) => {
                if m.is_static != static_mode {
                    return Ok(None);
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
                    return Ok(None);
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
        }))
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

        match ty.normalize() {
            Type::Ref(..) => {
                // We ignore errors.
                if let Ok(mut expanded_ty) = self
                    .expand_top_ref(ty.span(), Cow::Borrowed(&*ty), Default::default())
                    .map(Cow::into_owned)
                {
                    self.exclude_type(span, &mut expanded_ty, &excluded);
                    *ty = expanded_ty;
                    return;
                }
            }
            _ => {}
        }

        match excluded.normalize() {
            Type::Union(excluded) => {
                //
                for excluded in &excluded.types {
                    self.exclude_type(span, ty, &excluded)
                }

                return;
            }
            _ => {}
        }

        // TODO(kdy1): PERF
        match ty.normalize_mut() {
            Type::Union(ty) => {
                for ty in &mut ty.types {
                    self.exclude_type(span, ty, &excluded);
                }
                ty.types.retain(|element| !element.is_never());
            }

            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => {
                self.exclude_type(span, constraint, &excluded);
                if constraint.is_never() {
                    *ty = Type::never(span, Default::default());
                    return;
                }
            }

            Type::Class(cls) => {
                //
                if let Some(super_def) = &cls.def.super_class {
                    if let Ok(mut super_instance) = self.instantiate_class(cls.span, &super_def) {
                        self.exclude_type(span, &mut super_instance, &excluded);
                        if super_instance.is_never() {
                            *ty = Type::never(
                                cls.span,
                                KeywordTypeMetadata {
                                    common: cls.metadata.common,
                                    ..Default::default()
                                },
                            );
                            return;
                        }
                    }
                }
            }

            _ => {}
        }
    }

    fn exclude_types(&mut self, span: Span, ty: &mut Type, excludes: Option<Vec<Type>>) {
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

    /// We precomputes all type declarations in the scope, using this method.
    pub(crate) fn fill_known_type_names<N>(&mut self, node: &N)
    where
        N: Send + Sync + for<'aa> VisitWith<BindingCollector<'aa>> + VisitWith<KnownTypeVisitor>,
    {
        if self.is_builtin {
            return;
        }
        if self.data.bindings.collected {
            return;
        }

        self.data.bindings = collect_bindings(node);
    }
}

pub(crate) fn left(t: &RTsEntityName) -> &RIdent {
    match t {
        RTsEntityName::TsQualifiedName(t) => left(&t.left),
        RTsEntityName::Ident(i) => i,
    }
}
