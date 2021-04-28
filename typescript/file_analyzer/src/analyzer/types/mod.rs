use super::Analyzer;
use crate::type_facts::TypeFacts;
use crate::util::type_ext::TypeVecExt;
use crate::ValidationResult;
use fxhash::FxHashMap;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_types::name::Name;
use stc_ts_types::Array;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassMember;
use stc_ts_types::ConstructorSignature;
use stc_ts_types::Key;
use stc_ts_types::MethodSignature;
use stc_ts_types::Operator;
use stc_ts_types::PropertySignature;
use stc_ts_types::QueryExpr;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeLitMetadata;
use stc_ts_types::TypeParam;
use stc_ts_types::Union;
use stc_ts_utils::MapWithMut;
use stc_utils::error::context;
use stc_utils::stack;
use stc_utils::TryOpt;
use std::borrow::Cow;
use std::collections::HashMap;
use swc_atoms::js_word;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::SyntaxContext;
use swc_common::TypeEq;
use swc_ecma_ast::MethodKind;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::TsTypeOperatorOp;

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
        ty: &'a Type,
        mut opts: NormalizeTypeOpts,
    ) -> ValidationResult<Cow<'a, Type>> {
        let actual_span = span.unwrap_or_else(|| ty.span());
        if !self.is_builtin {
            debug_assert!(
                !actual_span.is_dummy(),
                "Cannot normalize a type with dummy span\n{:?}",
                ty
            );
        }

        let _stack = stack::track(actual_span)?;

        let ty = ty.normalize();

        match ty {
            Type::Ref(_) => {
                let ty = self
                    .expand_top_ref(actual_span, Cow::Borrowed(ty))
                    .context("tried to expand a ref type as a part of normalization")?;
                return Ok(Cow::Owned(self.normalize(span, &ty, opts)?.into_owned()));
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

                        return Ok(Cow::Owned(global));
                    }
                }
            }

            Type::Mapped(m) => {
                if !opts.preserve_mapped {
                    let ty = self.expand_mapped(actual_span, m)?;
                    if let Some(ty) = ty {
                        return Ok(Cow::Owned(
                            self.normalize(span, &ty, opts)
                                .context("tried to expand a mapped type as a part of normalization")?
                                .into_owned(),
                        ));
                    }
                }
            }

            Type::Alias(a) => return Ok(Cow::Owned(self.normalize(None, &a.ty, opts)?.into_owned())),

            // Leaf types.
            Type::Array(arr) => {
                let elem_type = box self
                    .normalize(None, &arr.elem_type, opts)
                    .context("tried to normalize the type of the element of an array type")?
                    .into_owned();
                return Ok(Cow::Owned(Type::Array(Array {
                    span: arr.span,
                    elem_type,
                })));
            }

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
            | Type::Module(_) => return Ok(Cow::Borrowed(ty)),

            // Not normalizable.
            Type::Infer(_) | Type::Instance(_) | Type::StaticThis(_) | Type::This(_) => {}

            // Maybe it can be changed in future, but currently noop
            Type::Union(_) | Type::Intersection(_) => {}

            Type::Conditional(c) => {
                let mut check_type = self
                    .normalize(span, &c.check_type, Default::default())
                    .context("tried to normalize the `check` type of a conditional type")?
                    .into_owned();

                let extends_type = self
                    .normalize(span, &c.extends_type, Default::default())
                    .context("tried to normalize the `extends` type of a conditional type")?;

                if let Some(v) = self.extends(ty.span(), Default::default(), &check_type, &extends_type) {
                    let ty = if v { &c.true_type } else { &c.false_type };
                    return self
                        .normalize(span, &ty, opts)
                        .context("tried to normalize the calculated type of a conditional type");
                }

                // TOOD: Optimize
                // If we can calculate type using constraints, do so.
                match check_type.normalize_mut() {
                    Type::Param(TypeParam {
                        name,
                        constraint: Some(check_type_contraint),
                        ..
                    }) => {
                        // We removes unmatchable constraints.
                        // It means, for
                        //
                        // T: a type param extends string | undefined
                        // A: T extends null | undefined ? never : T
                        //
                        // We removes `undefined` from parents of T.

                        match check_type_contraint.normalize() {
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
                                    types.dedup_type();
                                    let new = Type::Union(Union {
                                        span: actual_span,
                                        types,
                                    });

                                    *check_type_contraint = box new;

                                    let mut params = HashMap::default();
                                    params.insert(name.clone(), check_type);
                                    let c = self.expand_type_params(&params, c.clone())?;

                                    return Ok(Cow::Owned(Type::Conditional(c)));
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
                            let ty = self
                                .resolve_typeof(actual_span, e)
                                .context("tried to resolve typeof as a part of normalization")?;

                            return Ok(Cow::Owned(
                                self.normalize(span, &ty, opts)
                                    .context("tried to normalize the type returned from typeof")?
                                    .into_owned(),
                            ));
                        }
                        QueryExpr::Import(_) => {}
                    }
                }
                // TODO
            }

            Type::Import(_) => {}

            Type::Predicate(_) => {
                // TODO: Add option for this.
            }

            Type::IndexedAccessType(_) => {
                // TODO:
            }

            Type::Operator(Operator {
                op: TsTypeOperatorOp::KeyOf,
                ty,
                ..
            }) => {
                let keys_ty = self
                    .keyof(actual_span, &ty)
                    .context("tried to get keys of a type as a part of normalization")?;
                return Ok(Cow::Owned(keys_ty));
            }

            Type::Operator(_) => {
                // TODO:
            }

            _ => {}
        }

        Ok(Cow::Borrowed(ty.normalize()))
    }

    pub(crate) fn expand_type_ann<'a>(&mut self, ty: Option<&'a Type>) -> ValidationResult<Option<Cow<'a, Type>>> {
        let ty = match ty {
            Some(v) => v,
            None => return Ok(None),
        };

        let ty = self.normalize(None, ty, Default::default())?;

        Ok(Some(ty))
    }

    pub(crate) fn create_prototype_of_class_def(&mut self, def: &ClassDef) -> ValidationResult<TypeLit> {
        let mut members = vec![];

        let type_params = def.type_params.as_ref().map(|decl| {
            let ty = Type::any(decl.span);

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
                            .map(|ty| self.expand_type_params(&type_params, *ty).map(Box::new))
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

    pub(crate) fn exclude_types_using_fact(&mut self, name: &Name, ty: &mut Type) {
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
        self.exclude_types(ty, Some(types_to_exclude));
        let after = dump_type_as_string(&self.cm, &ty);

        slog::debug!(self.logger, "[types/facts] Excluded types: {} => {}", before, after);
    }

    pub(crate) fn apply_type_facts(&mut self, name: &Name, ty: Type) -> Type {
        let type_facts = self.scope.get_type_facts(&name)
            | self
                .cur_facts
                .true_facts
                .facts
                .get(&name)
                .copied()
                .unwrap_or(TypeFacts::None);

        self.apply_type_facts_to_type(type_facts, ty)
    }

    pub(crate) fn collect_class_members(&mut self, ty: &Type) -> ValidationResult<Option<Vec<ClassMember>>> {
        let ty = ty.normalize();
        match ty {
            Type::ClassDef(c) => {
                match &c.super_class {
                    Some(sc) => {
                        let mut members = c.body.clone();
                        // TODO: Override

                        if let Some(super_members) = self.collect_class_members(&sc)? {
                            members.extend(super_members)
                        }

                        return Ok(Some(members));
                    }
                    None => {
                        return Ok(Some(c.body.clone()));
                    }
                }
            }
            Type::Class(c) => self.collect_class_members(&Type::ClassDef(*c.def.clone())),
            _ => {
                slog::error!(self.logger, "unimplemented: collect_class_members: {:?}", ty);
                return Ok(None);
            }
        }
    }

    pub(crate) fn intersection(&mut self, span: Span, types: Vec<Type>) -> Type {
        let mut actual = vec![];

        let all_known = types.iter().all(|ty| ty.normalize().is_union_type())
            && types.iter().flat_map(|ty| ty.iter_union()).all(|ty| match ty {
                Type::Lit(..) | Type::Keyword(..) => true,
                _ => false,
            });

        if !all_known {
            return Type::intersection(span, types);
        }

        for ty in types.iter().flat_map(|ty| ty.iter_union()) {
            let in_all = types
                .iter()
                .all(|candidates| candidates.iter_union().any(|pred| pred.type_eq(ty)));

            if !in_all {
                continue;
            }

            actual.push(ty.clone());
        }
        actual.dedup_type();

        Type::intersection(span, actual)
    }

    /// Note: `span` is only used while expanding type (to prevent panic) in the
    /// case of [Type::Ref].
    pub(crate) fn type_to_type_lit<'a>(
        &mut self,
        span: Span,
        ty: &'a Type,
    ) -> ValidationResult<Option<Cow<'a, TypeLit>>> {
        let _ctx = context(format!("type_to_type_lit: {:?}", ty));

        debug_assert!(!span.is_dummy(), "type_to_type_lit: `span` should not be dummy");

        let ty = ty.normalize();

        Ok(Some(match ty {
            Type::Ref(..) => {
                let ty = self.expand_top_ref(span, Cow::Borrowed(ty))?;
                return self
                    .type_to_type_lit(span, &ty)
                    .map(|o| o.map(Cow::into_owned).map(Cow::Owned));
            }

            Type::TypeLit(t) => Cow::Borrowed(t),

            Type::Interface(t) => {
                let mut members = vec![];

                for parent in &t.extends {
                    let parent = self.type_of_ts_entity_name(
                        parent.span(),
                        self.ctx.module_id,
                        &parent.expr,
                        parent.type_args.as_deref(),
                    )?;

                    let super_els = self.type_to_type_lit(span, &parent)?;

                    members.extend(super_els.into_iter().map(Cow::into_owned).flat_map(|v| v.members))
                }

                // TODO: Override
                members.extend(t.body.clone());

                Cow::Owned(TypeLit {
                    span: t.span,
                    members,
                    metadata: TypeLitMetadata {
                        inexact: true,
                        ..Default::default()
                    },
                })
            }

            Type::Enum(e) => self.enum_to_type_lit(e).map(Cow::Owned)?,

            Type::Class(c) => {
                let mut members = vec![];
                if let Some(super_class) = &c.def.super_class {
                    let super_class = self.instantiate_class(span, super_class)?;
                    let super_els = self.type_to_type_lit(span, &super_class)?;
                    members.extend(super_els.map(|ty| ty.into_owned().members).into_iter().flatten());
                }

                // TODO: Override

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
                    let super_els = self.type_to_type_lit(span, super_class)?;
                    members.extend(super_els.map(|ty| ty.into_owned().members).into_iter().flatten());
                }

                // TODO: Override

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
                    let opt = self.type_to_type_lit(span, ty)?;
                    members.extend(opt.into_iter().map(Cow::into_owned).flat_map(|v| v.members));
                }

                Cow::Owned(TypeLit {
                    span: t.span,
                    members,
                    metadata: Default::default(),
                })
            }

            Type::Alias(ty) => return self.type_to_type_lit(span, &ty.ty),

            Type::Constructor(ty) => {
                let el = TypeElement::Constructor(ConstructorSignature {
                    span: ty.span,
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
                        span: e.span,
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
                    }));
                }

                // length
                members.push(TypeElement::Property(PropertySignature {
                    span: ty.span,
                    accessibility: None,
                    readonly: true,
                    key: Key::Normal {
                        span: ty.span.with_ctxt(SyntaxContext::empty()),
                        sym: "length".into(),
                    },
                    optional: false,
                    params: Default::default(),
                    type_ann: Some(box Type::Keyword(RTsKeywordType {
                        span: ty.span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                    })),
                    type_params: Default::default(),
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
                    let ty = self.type_to_type_lit(span, &ty)?.map(Cow::into_owned).map(Cow::Owned);

                    match ty {
                        Some(v) => v,
                        None => return Ok(None),
                    }
                } else {
                    return Ok(None);
                }
            }

            Type::Query(..) => {
                let ty = self
                    .normalize(None, ty, Default::default())
                    .context("tried to normalize a type to convert it to type literal")?;
                let ty = self
                    .type_to_type_lit(span, &ty)
                    .context("tried to convert a normalized type to type liteal")?
                    .map(Cow::into_owned)
                    .map(Cow::Owned);
                return Ok(ty);
            }

            _ => {
                slog::error!(self.logger, "unimplemented: type_to_type_lit: {:?}", ty);
                return Ok(None);
            }
        }))
    }
    pub(crate) fn normalize_tuples(&mut self, ty: &mut Type) {
        ty.visit_mut_with(&mut TupleNormalizer);
    }

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

                match m.kind {
                    MethodKind::Method => TypeElement::Method(MethodSignature {
                        span: m.span,
                        accessibility: m.accessibility,
                        readonly: false,
                        key: m.key.clone(),
                        optional: m.is_optional,
                        params: m.params.clone(),
                        ret_ty: Some(m.ret_ty.clone()),
                        type_params: m.type_params.clone(),
                    }),
                    MethodKind::Getter => TypeElement::Property(PropertySignature {
                        span: m.span,
                        accessibility: m.accessibility,
                        readonly: false,
                        key: m.key.clone(),
                        optional: m.is_optional,
                        params: vec![],
                        // TODO: Check for setter property with same key.
                        type_ann: Some(m.ret_ty.clone()),
                        type_params: None,
                    }),
                    MethodKind::Setter => return Ok(None),
                }
            }
            ClassMember::Property(p) => {
                if p.is_static != static_mode {
                    return Ok(None);
                }

                TypeElement::Property(PropertySignature {
                    span: p.span,
                    accessibility: p.accessibility,
                    readonly: p.readonly,
                    key: p.key.clone(),
                    optional: p.is_optional,
                    params: vec![],
                    type_ann: p.value.clone(),
                    type_params: None,
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
    fn exclude_type(&mut self, ty: &mut Type, excluded: &Type) {
        if ty.type_eq(excluded) {
            *ty = Type::never(ty.span());
            return;
        }

        match ty.normalize() {
            Type::Ref(..) => {
                // We ignore errors.
                if let Ok(mut expanded_ty) = self.expand_top_ref(ty.span(), Cow::Borrowed(&*ty)).map(Cow::into_owned) {
                    self.exclude_type(&mut expanded_ty, excluded);
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
                    self.exclude_type(ty, &excluded)
                }

                return;
            }
            _ => {}
        }

        match ty.normalize_mut() {
            Type::Union(ty) => {
                for ty in &mut ty.types {
                    self.exclude_type(ty, excluded);
                }
                ty.types.retain(|element| !element.is_never());
            }

            Type::Param(TypeParam {
                span,
                constraint: Some(constraint),
                ..
            }) => {
                self.exclude_type(constraint, excluded);
                if constraint.is_never() {
                    *ty = Type::never(*span);
                    return;
                }
            }

            Type::Class(cls) => {
                //
                if let Some(super_def) = &cls.def.super_class {
                    if let Ok(mut super_instance) = self.instantiate_class(cls.span, &super_def) {
                        self.exclude_type(&mut super_instance, excluded);
                        if super_instance.is_never() {
                            *ty = Type::never(cls.span);
                            return;
                        }
                    }
                }
            }

            _ => {}
        }
    }

    fn exclude_types(&mut self, ty: &mut Type, excludes: Option<Vec<Type>>) {
        let excludes = match excludes {
            Some(v) => v,
            None => return,
        };

        for excluded in excludes {
            self.exclude_type(ty, &excluded);
        }
    }
}

struct TupleNormalizer;

impl VisitMut<Type> for TupleNormalizer {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match ty.normalize() {
            Type::Tuple(..) => {
                let span = ty.span();
                let mut types = ty
                    .take()
                    .foldable()
                    .tuple()
                    .unwrap()
                    .elems
                    .into_iter()
                    .map(|elem| *elem.ty)
                    .collect::<Vec<_>>();
                types.dedup_type();

                let has_other = types.iter().any(|ty| !ty.is_null_or_undefined());
                if has_other {
                    types.retain(|ty| !ty.is_null_or_undefined())
                }

                *ty = Type::Array(Array {
                    span,
                    elem_type: box Type::union(types),
                });
            }
            _ => {}
        }
    }
}
