use std::{borrow::Cow, cmp::min, collections::hash_map::Entry, mem::take, time::Instant};

use fxhash::{FxHashMap, FxHashSet};
use itertools::{EitherOrBoth, Itertools};
use rnode::{Fold, FoldWith, VisitMut, VisitMutWith, VisitWith};
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RNumber, RPat, RTsEntityName};
use stc_ts_errors::{
    debug::{dump_type_as_string, force_dump_type_as_string, print_backtrace, print_type},
    DebugExt,
};
use stc_ts_generics::{
    expander::InferTypeResult,
    type_param::{finder::TypeParamUsageFinder, remover::TypeParamRemover, renamer::TypeParamRenamer},
};
use stc_ts_type_ops::{generalization::prevent_generalize, Fix};
use stc_ts_types::{
    replace::replace_type, Array, ClassMember, FnParam, Function, Id, IdCtx, Index, IndexSignature, IndexedAccessType, Intersection, Key,
    KeywordType, KeywordTypeMetadata, Mapped, OptionalType, PropertySignature, Readonly, Ref, Tuple, TupleElement, TupleMetadata, Type,
    TypeElement, TypeLit, TypeOrSpread, TypeParam, TypeParamDecl, TypeParamInstantiation, TypeParamMetadata, Union, UnionMetadata,
};
use stc_ts_utils::MapWithMut;
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    dev_span, stack,
};
use swc_atoms::js_word;
use swc_common::{EqIgnoreSpan, Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;
use tracing::{debug, error, info, trace, warn};

use self::inference::{InferenceInfo, InferencePriority};
pub(crate) use self::{expander::ExtendsOpts, inference::InferTypeOpts};
use super::{
    assign::get_tuple_subtract_count,
    expr::{AccessPropertyOpts, TypeOfMode},
};
use crate::{
    analyzer::{scope::ExpandOpts, Analyzer, Ctx, NormalizeTypeOpts},
    util::{unwrap_builtin_with_single_arg, RemoveTypes},
    VResult,
};

mod expander;
mod inference;
#[cfg(test)]
mod tests;

#[derive(Debug)]
pub(super) struct InferData {
    /// Inferred type parameters
    type_params: FxHashMap<Id, InferenceInfo>,

    errored: FxHashSet<Id>,

    /// For the code below, we can know that `T` defaults to `unknown` while
    /// inferring type of function parameters. We cannot know the type before
    /// it. So we store the default type while it.
    ///
    /// ```ts
    /// declare function one<T>(handler: (t: T) => void): T
    ///
    /// var empty = one(() => {
    /// });
    /// ```
    defaults: FxHashMap<Id, Type>,

    dejavu: Vec<(Type, Type)>,

    skip_generalization: bool,

    priority: InferencePriority,

    contravariant: bool,
}

impl Default for InferData {
    fn default() -> Self {
        Self {
            type_params: Default::default(),
            defaults: Default::default(),
            errored: Default::default(),
            dejavu: Default::default(),
            skip_generalization: Default::default(),
            priority: InferencePriority::MaxValue,
            contravariant: Default::default(),
        }
    }
}

/// Type inference for arguments.
impl Analyzer<'_, '_> {
    /// This method accepts Option<&[TypeParamInstantiation]> because user may
    /// provide only some of type arguments.
    pub(super) fn infer_arg_types(
        &mut self,
        span: Span,
        base: Option<&TypeParamInstantiation>,
        type_params: &[TypeParam],
        params: &[FnParam],
        args: &[TypeOrSpread],
        default_ty: Option<&Type>,
        ret_ty: Option<&Type>,
        ret_ty_type_ann: Option<&Type>,
        opts: InferTypeOpts,
    ) -> VResult<InferTypeResult> {
        #[cfg(debug_assertions)]
        let _tracing = dev_span!("infer_arg_types");

        warn!(
            "infer_arg_types: {:?}",
            type_params.iter().map(|p| format!("{}, ", p.name)).collect::<String>()
        );

        let start = Instant::now();

        let mut inferred = InferData::default();

        if let Some(base) = base {
            for (param, type_param) in base.params.iter().zip(type_params) {
                info!("User provided `{:?} = {:?}`", type_param.name, param.clone());
                inferred.type_params.insert(
                    type_param.name.clone(),
                    InferenceInfo {
                        type_param: type_param.name.clone(),
                        candidates: Default::default(),
                        contra_candidates: Default::default(),
                        inferred_type: param.clone().freezed(),
                        priority: Default::default(),
                        top_level: Default::default(),
                        is_fixed: true,
                        implied_arity: Default::default(),
                        rest_index: Default::default(),
                    },
                );
            }
        }

        // We allocate a new vector only if required.
        let mut actual_args;
        let args = if args.iter().any(|arg| arg.spread.is_some()) {
            actual_args = vec![];
            for arg in args {
                if arg.spread.is_some() {
                    match arg.ty.normalize_instance() {
                        Type::Tuple(Tuple { elems, .. }) => {
                            actual_args.extend(elems.iter().map(|elem| TypeOrSpread {
                                span: arg.spread.unwrap(),
                                spread: None,
                                ty: elem.ty.clone(),
                            }));
                        }
                        _ => {
                            actual_args.push(arg.clone());
                        }
                    }
                } else {
                    actual_args.push(arg.clone());
                }
            }
            &*actual_args
        } else {
            args
        };

        let skip = if params.is_empty() {
            0
        } else {
            match &params[0].pat {
                RPat::Ident(RBindingIdent {
                    id: RIdent { sym: js_word!("this"), .. },
                    ..
                }) => 1,
                _ => 0,
            }
        };

        for (idx, p) in params.iter().skip(skip).enumerate() {
            let is_rest = matches!(&p.pat, RPat::Rest(_));
            let opts = InferTypeOpts {
                rest_type_index: Some(idx),
                ..opts
            };

            if !is_rest {
                if let Some(arg) = args.get(idx) {
                    self.infer_type(span, &mut inferred, &p.ty, &arg.ty, opts)?;
                }
            } else {
                let stop_idx = args
                    .iter()
                    .skip(idx)
                    .position(|arg| arg.spread.is_some())
                    .map(|v| v + idx)
                    .unwrap_or({
                        // No spread means all arguments are used.
                        args.len()
                    });

                match p.ty.normalize_instance() {
                    Type::Param(param) => {
                        self.infer_type(
                            span,
                            &mut inferred,
                            &p.ty,
                            &Type::Tuple(Tuple {
                                span: p.ty.span(),
                                elems: args[idx..stop_idx]
                                    .iter()
                                    .map(|arg| &arg.ty)
                                    .cloned()
                                    .map(|ty| TupleElement {
                                        span: DUMMY_SP,
                                        label: None,
                                        ty,
                                        tracker: Default::default(),
                                    })
                                    .collect(),
                                metadata: TupleMetadata {
                                    common: p.ty.metadata(),
                                    ..Default::default()
                                },
                                tracker: Default::default(),
                            })
                            .freezed(),
                            opts,
                        )?;
                    }
                    Type::Array(p_ty) => {
                        // Handle varargs. This result in union of all types.
                        for arg in &args[idx..stop_idx] {
                            self.infer_type(span, &mut inferred, &p_ty.elem_type, &arg.ty, opts)?;
                        }
                        if let Some(arg) = args.get(stop_idx) {
                            self.infer_type(span, &mut inferred, &p_ty.elem_type, &arg.ty, opts)?;
                        }
                    }
                    _ => {
                        // Handle varargs
                        for arg in &args[idx..stop_idx] {
                            self.infer_type(span, &mut inferred, &p.ty, &arg.ty, opts)?;
                        }
                    }
                }
            }
        }

        if let Some(ret_ty) = ret_ty {
            if let Some(ret_type_ann) = ret_ty_type_ann {
                let _tracing = dev_span!("infer_arg_types: return type annotation");

                self.infer_type(
                    span,
                    &mut inferred,
                    ret_ty,
                    ret_type_ann,
                    InferTypeOpts {
                        priority: InferencePriority::ReturnType,
                        ..Default::default()
                    },
                )?;
            }
        }

        info!("infer_type is finished:\n{:?}", &inferred.type_params);

        // Defaults
        for type_param in type_params {
            if inferred.type_params.contains_key(&type_param.name) {
                continue;
            }

            if let Some(Type::Param(ref p)) = type_param.constraint.as_deref().map(Type::normalize) {
                // TODO(kdy1): Handle complex inheritance like
                //      function foo<A extends B, B extends C>(){ }

                if let Some(actual) = inferred.type_params.remove(&p.name) {
                    info!(
                        "infer_arg_type: {} => {} => {:?} because of the extends clause",
                        type_param.name, p.name, actual
                    );
                    inferred.type_params.insert(p.name.clone(), actual.clone());
                    inferred.type_params.insert(type_param.name.clone(), actual);
                } else {
                    info!("infer_arg_type: {} => {} because of the extends clause", type_param.name, p.name);
                    self.insert_inferred(span, &mut inferred, type_param, Cow::Owned(Type::Param(p.clone())), opts)?;
                }
                continue;
            }

            if type_param.constraint.is_some() && is_literals(type_param.constraint.as_ref().unwrap()) {
                self.insert_inferred(
                    span,
                    &mut inferred,
                    type_param,
                    Cow::Borrowed(type_param.constraint.as_deref().unwrap()),
                    opts,
                )?;
                continue;
            }

            if matches!(
                type_param.constraint.as_deref().map(Type::normalize),
                Some(Type::Interface(..) | Type::Keyword(..) | Type::Ref(..) | Type::TypeLit(..))
            ) {
                let ty = self
                    .expand(
                        span,
                        *type_param.constraint.clone().unwrap(),
                        ExpandOpts {
                            full: true,
                            expand_union: false,
                            ..Default::default()
                        },
                    )?
                    .freezed();
                if !inferred.type_params.contains_key(&type_param.name) {
                    self.insert_inferred(span, &mut inferred, type_param, Cow::Owned(ty), opts)?;
                }
                continue;
            }
            if !inferred.type_params.contains_key(&type_param.name) {
                if let Some(default_ty) = inferred.defaults.remove(&type_param.name) {
                    self.insert_inferred(span, &mut inferred, type_param, Cow::Owned(default_ty), opts)?;
                } else {
                    if let Some(default) = &type_param.default {
                        self.insert_inferred(span, &mut inferred, type_param, Cow::Borrowed(default), opts)?;
                        continue;
                    }

                    if let Some(default_ty) = default_ty {
                        error!("infer: A type parameter {} defaults to {:?}", type_param.name, default_ty);

                        self.insert_inferred(span, &mut inferred, type_param, Cow::Borrowed(default_ty), opts)?;
                    }
                }
            }
        }

        self.prevent_generalization_of_top_level_types(type_params, ret_ty, &mut inferred, opts.is_type_ann);

        self.prevent_generalization_of_inferred_types(type_params, &mut inferred, opts.is_type_ann);

        let map = self.finalize_inference(span, type_params, inferred);

        let end = Instant::now();

        warn!("infer_arg_types is finished. (time = {:?})", end - start);

        Ok(map)
    }

    /// Handles `infer U`.
    pub(super) fn infer_ts_infer_types(
        &mut self,
        span: Span,
        base: &Type,
        concrete: &Type,
        opts: InferTypeOpts,
    ) -> VResult<FxHashMap<Id, Type>> {
        let _tracing = dev_span!("infer_ts_infer_types");

        let mut inferred = InferData::default();
        self.infer_type(span, &mut inferred, base, concrete, opts)?;
        let mut map = self.finalize_inference(span, &[], inferred);

        for ty in map.types.values_mut() {
            prevent_generalize(ty);
        }

        Ok(map.types)
    }

    /// # Inference rule
    ///
    /// 1. We iterate over parameters and arguments in order.
    ///
    /// 2. If newly inferred type is not compatible with the previous one, we
    /// don't store it. `compatible` here means the previous type is
    /// assignable to the newly inferred type.
    ///
    /// 3. If there's `any` or `unknown`, those are used because all types are
    /// `compatible` with them.
    ///
    /// If `any` and `unknown` co-exist, the last one is selected.
    ///
    /// 4. `{}` and an empty interface work just like `any` or `unknown`. It's
    /// because almost all types are `compatible` with it, so the same rule
    /// applies. But `any` or `unknown` is preferred over `{}`.
    ///
    /// 5. If a parameter of a closure has an explicit type, the `compatibility`
    /// rule applies. But some types like the built-in `Object`  are exceptions
    /// and those are ignored. i.e. The inferred types are not changed to
    /// `Object`.
    ///
    /// 6. The return type of a closure does not have effect on the inference,
    /// iff it's a direct function expression.
    ///
    ///
    /// # Postprocess
    ///
    /// 1. If there was noe error and if there's no constraints like `extends
    /// string` nor `extends number`, the inferred types are generalized.
    ///
    /// ---
    ///
    /// ```ts
    /// function foo<T>(x: { bar: T; baz: T }) {
    ///     return x;
    /// }
    ///
    /// declare function fn1(): void;
    /// declare function f2(): string;
    ///
    /// declare class C1 {
    ///     prop: string
    /// }
    ///
    /// declare class C2 {
    ///     c2prop: number
    /// }
    ///
    /// interface I1 {
    ///     s: string
    /// }
    ///
    /// declare const us: unique symbol
    /// declare var i1: I1
    ///
    /// declare var c1: C1
    /// declare var c2: C2
    ///
    ///
    /// declare var n: number
    ///
    /// foo({ bar: 1, baz: '' }); // Error on baz (number is selected)
    /// foo({ bar: '', baz: 1 }); // Error on baz (string is selected)
    /// foo({ bar: '', baz: n }); // Error on baz (string is selected)
    /// foo({ bar: Symbol.iterator, baz: 5 }) // Error on baz (symbol is selected)
    /// foo({ bar: us, baz: 5 }) // Error on baz (unique symbol is selected)
    ///
    ///
    /// foo({ bar: [], baz: '' }); // Error on bar (string is selected)
    /// foo({ bar: {}, baz: '' }); // Error on bar (string is selected)
    ///
    /// declare var u: string | number
    /// foo({ bar: 1, baz: u }) // Ok
    /// foo({ bar: {}, baz: u }) // Error on bar (string | number is selected)
    ///
    /// foo({ bar: i1, baz: 5 }) // Error on baz (I1 is selected)
    /// foo({ bar: 5, baz: i1 }) // Error on baz (number is selected)
    ///
    ///
    /// foo({ bar: 5, baz: fn1 }) // Error on baz (number is selected)
    /// foo({ bar: 5, baz: i1 }) // Error on baz (number is selected)
    ///
    /// foo({ bar: 1, baz: c2 }) // Error on baz (number is selected)
    /// foo({ bar: c1, baz: 1 }) // Error on baz (C1 is selected)
    /// foo({ bar: c1, baz: c2 }) // Error on baz (C1 is selected)
    /// foo({ bar: i1, baz: c1 }) // Error on baz (I1 is selected)
    /// foo({ bar: c1, baz: i1 }) // Error on baz (C1 is selected)
    ///
    ///
    /// function arr<T>(x: T[]) {
    ///     return x;
    /// }
    ///
    /// arr([1, '']); // Ok
    /// arr(['', 1]); // Ok
    /// arr([Symbol.iterator, 5]) // Ok
    /// arr([us, 5]) // Ok
    ///
    ///
    /// arr([[], '']); // Ok
    /// arr([{}, '']); // Ok
    ///
    /// arr([1, u]) // Ok
    /// arr([{}, u]) // Ok
    /// ```
    fn infer_type(&mut self, span: Span, inferred: &mut InferData, param: &Type, arg: &Type, opts: InferTypeOpts) -> VResult<()> {
        if self.config.is_builtin {
            return Ok(());
        }

        let span = span.with_ctxt(SyntaxContext::empty());

        let _tracing = if cfg!(debug_assertions) {
            let param_str = force_dump_type_as_string(param);
            let arg_str = force_dump_type_as_string(arg);

            Some(dev_span!("infer_type", param = &*param_str, arg = &*arg_str))
        } else {
            None
        };

        debug!("Start");

        let res = self.infer_type_inner(span, inferred, param, arg, opts);

        debug!("End");

        res
    }

    /// Infer types, so that `param` has same type as `arg`.
    ///
    ///
    /// TODO(kdy1): Optimize
    fn infer_type_inner(&mut self, span: Span, inferred: &mut InferData, param: &Type, arg: &Type, mut opts: InferTypeOpts) -> VResult<()> {
        if self.config.is_builtin {
            return Ok(());
        }

        let marks = self.marks();

        let _stack = match stack::track(span) {
            Ok(v) => v,
            Err(_) => return Ok(()),
        };

        if !opts.skip_initial_union_check {
            if inferred
                .dejavu
                .iter()
                .any(|(prev_param, prev_arg)| prev_param.type_eq(param) && prev_arg.type_eq(arg))
            {
                return Ok(());
            }
            inferred.dejavu.push((param.clone(), arg.clone()));
        }

        debug_assert!(!span.is_dummy(), "infer_type: `span` should not be dummy");

        if param.is_keyword() || param.type_eq(arg) {
            return Ok(());
        }

        let param_normalized = param.normalize();
        let arg_normalized = arg.normalize();

        param.assert_clone_cheap();
        arg.assert_clone_cheap();

        /// Returns true if we can unconditionally delegate to `infer_type`.
        fn should_delegate(ty: &Type) -> bool {
            match ty.normalize() {
                Type::Instance(..) => true,
                Type::IndexedAccessType(t) => matches!(t.index_type.normalize(), Type::Lit(..)),
                _ => false,
            }
        }

        if should_delegate(param_normalized) {
            let mut param = self.normalize(Some(span), Cow::Borrowed(param_normalized), Default::default())?;
            param.freeze();
            return self.infer_type(span, inferred, &param, arg, opts);
        }

        if should_delegate(arg_normalized) {
            let mut arg = self.normalize(Some(span), Cow::Borrowed(arg_normalized), Default::default())?;
            arg.freeze();

            return self.infer_type(span, inferred, param, &arg, opts);
        }

        let p;
        let param = match param.normalize() {
            Type::Mapped(..) => {
                // TODO(kdy1): PERF
                p = box param_normalized
                    .clone()
                    .foldable()
                    .fold_with(&mut MappedIndexedSimplifier)
                    .freezed();
                &p
            }
            _ => param,
        };
        let param_normalized = param.normalize();

        {
            // Handle array-like types
            let opts = InferTypeOpts {
                append_type_as_union: true,
                ..opts
            };

            if let (Some(p), Some(a)) = (array_elem_type(param_normalized), array_elem_type(arg_normalized)) {
                return self.infer_type(span, inferred, p, a, opts);
            }
        }

        match (param_normalized.normalize(), arg.normalize()) {
            (Type::Union(p), _) => {
                if !opts.skip_initial_union_check {
                    self.infer_type_using_union(
                        span,
                        inferred,
                        p,
                        arg,
                        InferTypeOpts {
                            append_type_as_union: true,
                            ..opts
                        },
                    )?;

                    return Ok(());
                } else {
                    opts.skip_initial_union_check = false;
                }
            }

            (Type::Intersection(param), _) => {
                for param in &param.types {
                    self.infer_type(span, inferred, param, arg, opts)?;
                }

                return Ok(());
            }

            _ => {}
        }

        let p = param_normalized;
        let a = arg_normalized;

        if let Some(res) = self.infer_builtin(span, inferred, param, arg, opts) {
            return res;
        }

        if self.infer_type_by_converting_to_type_lit(span, inferred, param, arg, opts)? {
            return Ok(());
        }

        if opts.for_fn_assignment {
            if let Type::Param(arg) = arg_normalized.normalize() {
                if !param_normalized.is_type_param() {
                    self.insert_inferred(span, inferred, arg, Cow::Borrowed(param), opts)?;
                    return Ok(());
                }
            }
        }

        match (param.normalize(), arg.normalize()) {
            (_, Type::Enum(..)) => {
                let arg = self
                    .normalize(
                        Some(arg_normalized.span()),
                        Cow::Borrowed(arg),
                        NormalizeTypeOpts {
                            expand_enum_def: true,
                            preserve_global_this: true,
                            ..Default::default()
                        },
                    )
                    .context("tried to normalize enum")?
                    .freezed()
                    .into_owned()
                    .freezed();
                return self.infer_type_inner(span, inferred, param, &arg, opts);
            }

            (Type::Index(Index { ty: param, .. }), Type::Index(Index { ty: arg, .. })) => {
                return self.infer_from_contravariant_types(span, inferred, arg, param, opts)
            }

            (Type::Conditional(target), Type::Conditional(source)) => {
                self.infer_from_types(span, inferred, &source.check_type, &target.check_type, opts)?;
                self.infer_from_types(span, inferred, &source.extends_type, &target.extends_type, opts)?;
                self.infer_from_types(span, inferred, &source.true_type, &target.true_type, opts)?;
                self.infer_from_types(span, inferred, &source.false_type, &target.false_type, opts)?;

                return Ok(());
            }
            (Type::Conditional(target), ..) => {
                return self.infer_to_multiple_types_with_priority(
                    span,
                    inferred,
                    arg,
                    &[*target.true_type.clone(), *target.false_type.clone()],
                    if inferred.contravariant {
                        InferencePriority::ContravariantConditional
                    } else {
                        InferencePriority::None
                    },
                    false,
                    opts,
                )
            }

            (Type::Tpl(target), _) => {
                return self.infer_to_tpl_lit_type(span, inferred, arg, target, opts);
            }

            _ => {}
        }

        match param_normalized.normalize() {
            Type::Param(TypeParam {
                ref name, ref constraint, ..
            }) => {
                let constraint = constraint.as_ref().map(|ty| ty.normalize());
                if !opts.for_fn_assignment && !self.ctx.skip_identical_while_inference {
                    if let Some(prev) = inferred.type_params.get(name).cloned() {
                        let ctx = Ctx {
                            skip_identical_while_inference: true,
                            ..self.ctx
                        };
                        prev.inferred_type.assert_clone_cheap();

                        self.with_ctx(ctx).infer_type(span, inferred, &prev.inferred_type, arg, opts)?;
                        self.with_ctx(ctx).infer_type(span, inferred, arg, &prev.inferred_type, opts)?;
                    }
                }

                trace!("infer_type: type parameter: {} = {:?}", name, constraint);

                if constraint.is_some() && is_literals(constraint.as_ref().unwrap()) {
                    info!("infer from literal constraint: {} = {:?}", name, constraint);
                    if let Some(orig) = inferred.type_params.get(name) {
                        if !orig.inferred_type.eq_ignore_span(constraint.as_ref().unwrap()) {
                            print_backtrace();
                            unreachable!(
                                "Cannot override T in `T extends <literal>`\nOrig: {:?}\nConstraints: {:?}",
                                orig, constraint
                            )
                        }
                    }

                    return Ok(());
                }

                if let Some(constraint) = constraint {
                    if constraint.is_str() || constraint.is_num() {
                        match arg.normalize() {
                            // We use `default`
                            Type::TypeLit(..) | Type::Interface(..) | Type::Class(..) => return Ok(()),
                            _ => {}
                        }
                    }

                    // TODO(kdy1): Infer only if constraints are matched
                    //
                    // if let Some(false) = self.extends(span, ExtendsOpts {
                    // ..Default::default() }, arg, constraint) {
                    //     return Ok(());
                    // }
                }

                if self.ctx.skip_identical_while_inference {
                    if let Type::Param(arg) = arg_normalized {
                        if *name == arg.name {
                            return Ok(());
                        }
                    }
                }

                if arg_normalized.is_any() && self.is_implicitly_typed(arg_normalized) {
                    if inferred.type_params.contains_key(&name.clone()) {
                        return Ok(());
                    }

                    match inferred.defaults.entry(name.clone()) {
                        Entry::Occupied(..) => {}
                        Entry::Vacant(e) => {
                            e.insert(Type::Param(TypeParam {
                                span: arg_normalized.span(),
                                name: name.clone(),
                                constraint: None,
                                default: None,
                                metadata: TypeParamMetadata {
                                    common: arg_normalized.metadata(),
                                    ..Default::default()
                                },
                                tracker: Default::default(),
                            }));
                        }
                    }

                    //
                    return Ok(());
                }

                debug!(
                    "({}): Inferred `{}` as {}",
                    self.scope.depth(),
                    name,
                    dump_type_as_string(arg_normalized)
                );

                self.upsert_inferred(span, inferred, name.clone(), arg, opts)?;

                return Ok(());
            }

            Type::Interface(param) => match arg.normalize() {
                Type::Interface(..) => self.infer_type_using_interface(span, inferred, param, arg, opts)?,
                Type::TypeLit(..) | Type::Tuple(..) => return self.infer_type_using_interface(span, inferred, param, arg, opts),
                _ => {}
            },

            Type::Infer(param) => {
                self.insert_inferred(span, inferred, &param.type_param, Cow::Borrowed(arg), opts)?;
                return Ok(());
            }

            Type::Array(param_arr @ Array { .. }) => {
                let opts = InferTypeOpts {
                    append_type_as_union: true,
                    ..opts
                };

                match arg_normalized {
                    Type::Array(Array {
                        elem_type: arg_elem_type, ..
                    }) => return self.infer_type(span, inferred, &param_arr.elem_type, arg_elem_type, opts),

                    Type::Tuple(arg) => {
                        let arg = Type::new_union(span, arg.elems.iter().map(|element| *element.ty.clone())).freezed();
                        return self.infer_type(span, inferred, &param_arr.elem_type, &arg, opts);
                    }

                    _ => {}
                }
            }

            // // TODO(kdy1): Check if index type extends `keyof obj_type`
            // Type::IndexedAccessType(IndexedAccessType {
            //     obj_type: box Type::Param(param_obj),
            //     index_type:
            //         box Type::Param(TypeParam {
            //             constraint: Some(..),
            //             ..
            //         }),
            //     ..
            // }) => {
            //     match inferred.type_elements.entry(param_obj.name.clone()) {
            //         Entry::Occupied(mut e) => {
            //             let (name, prev_ty) = e.remove_entry();
            //
            //             inferred
            //                 .type_elements
            //                 .insert(name, Type::new_union(span, vec![prev_ty, box arg.clone()]))
            //                 .expect_none("Cannot override");
            //         }
            //         Entry::Vacant(e) => {
            //             e.insert(box arg.clone());
            //         }
            //     }
            //     return Ok(());
            // }
            Type::Function(p) => match arg_normalized {
                Type::Function(a) => {
                    self.infer_type_of_fn_params(
                        span,
                        inferred,
                        &p.params,
                        &a.params,
                        InferTypeOpts {
                            ignore_builtin_object_interface: true,
                            ..opts
                        },
                    )?;
                    self.infer_type(
                        span,
                        inferred,
                        &p.ret_ty,
                        &a.ret_ty,
                        InferTypeOpts {
                            ignore_builtin_object_interface: true,
                            ..opts
                        },
                    )?;

                    if !opts.for_fn_assignment {
                        if let Some(arg_type_params) = &a.type_params {
                            let mut data = InferData {
                                dejavu: inferred.dejavu.clone(),
                                ..Default::default()
                            };
                            self.infer_type_of_fn_params(span, &mut data, &a.params, &p.params, InferTypeOpts { use_error: true, ..opts })?;

                            for name in data.errored {
                                if !inferred.type_params.contains_key(&name) {
                                    inferred.errored.insert(name);
                                }
                            }

                            for (name, ty) in data.type_params {
                                inferred.type_params.entry(name).or_insert(ty);
                            }
                        }
                    }

                    if let Some(arg_type_params) = &a.type_params {
                        self.rename_inferred(span, inferred, arg_type_params)?;
                    }
                    return Ok(());
                }
                _ => {
                    dbg!();
                }
            },

            Type::TypeLit(param) => match arg_normalized {
                Type::TypeLit(arg) => return self.infer_type_using_type_lit_and_type_lit(span, inferred, param, arg, opts),

                Type::IndexedAccessType(arg_iat) => {
                    let arg_obj_ty = self.expand(
                        arg_iat.span,
                        *arg_iat.obj_type.clone(),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            ..Default::default()
                        },
                    )?;

                    if let Some(arg_obj_ty) = arg_obj_ty.mapped() {
                        if let TypeParam {
                            constraint:
                                Some(box Type::Index(Index {
                                    ty: box Type::Param(param_ty),
                                    ..
                                })),
                            ..
                        } = &arg_obj_ty.type_param
                        {
                            let mut new_lit = TypeLit {
                                span: arg_iat.span,
                                members: vec![],
                                metadata: Default::default(),
                                tracker: Default::default(),
                            };
                            for member in &param.members {
                                match member {
                                    TypeElement::Property(p) => {
                                        let p = p.clone();
                                        if let Some(type_ann) = &p.type_ann {
                                            // TODO(kdy1): Change p.ty

                                            self.infer_type(span, inferred, type_ann, arg, opts)?;
                                        }

                                        new_lit.members.push(TypeElement::Property(p));
                                    }
                                    // TODO(kdy1): Handle IndexSignature
                                    _ => {
                                        unimplemented!("calculating IndexAccessType for member other than property: member = {:?}", member)
                                    }
                                }
                            }
                            self.insert_inferred(span, inferred, param_ty, Cow::Owned(Type::TypeLit(new_lit)), opts)?;

                            return Ok(());
                        }
                    }
                }

                Type::Interface(..) | Type::Alias(..) => {
                    if let Some(arg) = self.convert_type_to_type_lit(span, Cow::Borrowed(arg))? {
                        return self.infer_type_using_type_lit_and_type_lit(span, inferred, param, &arg, opts);
                    }
                }

                _ => {}
            },

            Type::Tuple(param) => match arg_normalized {
                Type::Array(arg) => {
                    for elem in &param.elems {
                        match elem.ty.normalize() {
                            Type::Rest(rest) => {
                                self.infer_type(span, inferred, &rest.ty, &arg.elem_type, opts)?;
                            }
                            _ => {
                                self.infer_type(span, inferred, &elem.ty, &arg.elem_type, opts)?;
                            }
                        }
                    }
                }
                Type::Tuple(arg) => return self.infer_type_using_tuple_and_tuple(span, inferred, param, p, arg, a, opts),
                _ => {
                    dbg!();
                }
            },

            Type::Keyword(..) => {
                if let Type::Keyword(..) = arg_normalized {
                    return Ok(());
                }

                dbg!();
            }

            Type::Predicate(..) => {
                dbg!();
            }

            Type::Rest(param_rest) => {
                if let Type::Rest(arg_rest) = arg_normalized {
                    return self.infer_type(span, inferred, &param_rest.ty, &arg_rest.ty, opts);
                }

                return self.infer_type(
                    span,
                    inferred,
                    &param_rest.ty,
                    &Type::Tuple(Tuple {
                        span,
                        elems: vec![TupleElement {
                            span,
                            label: None,
                            ty: box arg.clone(),
                            tracker: Default::default(),
                        }],
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                    .freezed(),
                    InferTypeOpts {
                        is_inferring_rest_type: true,
                        append_type_as_union: true,
                        ..opts
                    },
                );
            }

            Type::Ref(param) => match arg_normalized {
                Type::Ref(arg)
                    if param.type_name.eq_ignore_span(&arg.type_name)
                        && param.type_args.as_ref().map(|v| v.params.len()) == arg.type_args.as_ref().map(|v| v.params.len()) =>
                {
                    if param.type_args.is_none() && arg.type_args.is_none() {
                        return Ok(());
                    }
                    if param.type_args.is_none() || arg.type_args.is_none() {
                        unimplemented!("Comparing `Ref<T>` (with type args) and `Ref` (without type args)");
                    }

                    for pa in param
                        .type_args
                        .as_ref()
                        .unwrap()
                        .params
                        .iter()
                        .zip_longest(arg.type_args.as_ref().unwrap().params.iter())
                    {
                        match pa {
                            EitherOrBoth::Both(param, arg) => {
                                self.infer_type(span, inferred, param, arg, opts)?;
                            }
                            _ => {
                                unreachable!("type inference: Comparison of Ref<Arg1, Arg2> and Ref<Arg1> (different length)");
                            }
                        }
                    }
                    return Ok(());
                }
                // Type::Param(arg) => match &param.type_name {
                //     RTsEntityName::TsQualifiedName(_) => {}
                //     RTsEntityName::Ident(param) => {
                //         inferred
                //             .type_params
                //             .insert(param.clone().into(), box Type::Param(arg.clone()));
                //         return Ok(());
                //     }
                // },
                _ => {
                    // TODO(kdy1): Expand children first or add expansion information to inferred.
                    if cfg!(debug_assertions) {
                        debug!("infer_type: expanding param");
                    }
                    let mut param = self.expand(
                        span,
                        Type::Ref(param.clone()),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ignore_expand_prevention_for_all: false,
                            ..Default::default()
                        },
                    )?;
                    param.freeze();
                    match param.normalize() {
                        Type::Ref(..) => {
                            dbg!();

                            info!("Ref: {:?}", param);
                        }
                        _ => return self.infer_type(span, inferred, &param, arg, opts),
                    }
                }
            },

            Type::Lit(..) => {
                if let Type::Lit(..) = arg_normalized {
                    return Ok(());
                }
            }

            Type::Alias(param) => {
                self.infer_type(span, inferred, &param.ty, arg, opts)?;
                if let Some(type_params) = &param.type_params {
                    self.rename_inferred(span, inferred, type_params)?;
                }
                return Ok(());
            }

            Type::Mapped(param) => {
                if self.infer_type_using_mapped_type(span, inferred, param, arg, opts)? {
                    dbg!();
                    return Ok(());
                }
            }

            Type::IndexedAccessType(param) => {
                if let Type::IndexedAccessType(arg) = arg_normalized {
                    if param.obj_type.eq_ignore_span(&arg.obj_type) {
                        self.infer_type(span, inferred, &param.index_type, &arg.index_type, opts)?;
                        return Ok(());
                    }
                }

                match param {
                    IndexedAccessType {
                        obj_type: box Type::Param(obj_type),
                        ..
                    } if self.mapped_type_param_name.contains(&obj_type.name) => {
                        self.insert_inferred(span, inferred, obj_type, Cow::Borrowed(arg), opts)?;
                        return Ok(());
                    }

                    IndexedAccessType {
                        obj_type: box Type::Intersection(Intersection { types, .. }),
                        ..
                    } if types.iter().all(|ty| match ty.normalize() {
                        Type::Param(obj_type) => self.mapped_type_param_name.contains(&obj_type.name),
                        _ => false,
                    }) =>
                    {
                        for ty in types {
                            if let Type::Param(obj_type) = ty.normalize() {
                                self.insert_inferred(span, inferred, obj_type, Cow::Borrowed(arg), opts)?;
                            }
                        }
                        return Ok(());
                    }
                    _ => {}
                }

                if opts.index_tuple_with_param {
                    if let (
                        Type::Param(obj_param),
                        Type::Param(TypeParam {
                            constraint: Some(index_param_constraint),
                            ..
                        }),
                    ) = (param.obj_type.normalize(), param.index_type.normalize())
                    {
                        // param  = [string, number, ...T][P];
                        // arg = true;
                        //
                        // where P is keyof T
                        //
                        // =>
                        //
                        // T = true

                        if let Type::Index(Index { ty: keyof_ty, .. }) = index_param_constraint.normalize() {
                            return self.infer_type(
                                span,
                                inferred,
                                &param.obj_type,
                                arg,
                                InferTypeOpts {
                                    append_type_as_union: true,
                                    ..Default::default()
                                },
                            );
                        }
                    }
                }
            }

            Type::Constructor(param) => {
                if let Type::Class(arg_class) = arg_normalized {
                    for member in &arg_class.def.body {
                        match member {
                            ClassMember::Constructor(constructor) => {
                                self.infer_type_of_fn_params(span, inferred, &param.params, &constructor.params, opts)?;

                                if let Some(ret_ty) = &constructor.ret_ty {
                                    return self.infer_type(span, inferred, &param.type_ann, ret_ty, opts);
                                }
                            }
                            ClassMember::Method(_) => {}
                            ClassMember::Property(_) => {}
                            ClassMember::IndexSignature(_) => {}
                        }
                    }

                    return self.infer_type(span, inferred, &param.type_ann, arg, opts);
                }
            }

            Type::Class(param) => {
                if let Type::Class(arg) = arg_normalized {
                    return self.infer_types_using_class(span, inferred, param, arg, opts);
                }
            }

            Type::ClassDef(param) => {
                if let Type::ClassDef(arg) = arg_normalized {
                    return self.infer_types_using_class_def(span, inferred, param, arg, opts);
                }
            }

            Type::Readonly(param) => {
                self.infer_type_using_readonly(span, inferred, param, arg, opts)?;

                // We need to check parents
                if let Type::Interface(..) = arg.normalize() {
                } else {
                    return Ok(());
                }
            }

            _ => {}
        }

        match (param.normalize(), arg.normalize()) {
            (Type::Union(Union { types: param_types, .. }), _) => {
                return self.infer_to_multiple_types(span, inferred, arg, param_types, true, opts);
            }
            (Type::Intersection(Intersection { types: param_types, .. }), _) => {
                return self.infer_to_multiple_types(span, inferred, arg, param_types, false, opts);
            }

            (_, Type::Union(arg_union)) => {
                // Source is a union or intersection type, infer from each constituent type
                for source_type in arg_union.types.iter() {
                    self.infer_from_types(span, inferred, source_type, param, opts)?;
                }
                return Ok(());
            }

            _ => {}
        }

        match arg_normalized {
            // Handled by generic expander, so let's return it as-is.
            Type::Mapped(..) => {}

            Type::Readonly(Readonly { ty: arg, .. }) => return self.infer_type(span, inferred, param, arg, opts),

            Type::Array(arr) => {
                debug_assert_eq!(span.ctxt, SyntaxContext::empty());

                let params = vec![*arr.elem_type.clone()];
                return self.infer_type(
                    span,
                    inferred,
                    param,
                    &Type::Ref(Ref {
                        span,
                        type_name: RTsEntityName::Ident(RIdent::new("Array".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation { span, params }),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                    .freezed(),
                    opts,
                );
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),
            Type::Keyword(..) => {}
            Type::Ref(..) => {
                let arg = self
                    .expand(
                        span,
                        arg.clone(),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ignore_expand_prevention_for_all: false,
                            ..Default::default()
                        },
                    )?
                    .freezed();
                match arg.normalize() {
                    Type::Ref(..) => {}
                    _ => {
                        return self.infer_type(span, inferred, param, &arg, opts);
                    }
                }
            }
            Type::Alias(arg) => return self.infer_type(span, inferred, param, &arg.ty, opts),

            Type::Interface(arg) => {
                // Body should be handled by the match expression above.

                for parent in &arg.extends {
                    let parent = self
                        .type_of_ts_entity_name(span, &parent.expr, parent.type_args.as_deref())?
                        .freezed();
                    self.infer_type(span, inferred, param, &parent, opts)?;
                }

                // Check to print unimplemented error message
                match param_normalized {
                    Type::Index(..) | Type::Interface(..) => return Ok(()),
                    _ => {}
                }
            }

            Type::Intersection(arg) => {
                // Infer each type, and then intersect each type parameters.

                let mut data = vec![];

                for ty in &arg.types {
                    let mut inferred = InferData {
                        dejavu: inferred.dejavu.clone(),
                        ..Default::default()
                    };
                    self.infer_type(span, &mut inferred, param, ty, opts)
                        .context("failed to in infer element type of an intersection type")?;
                    data.push(inferred);
                }

                let mut map = FxHashMap::<_, Vec<_>>::default();
                for item in data {
                    for (name, ty) in item.type_params {
                        map.entry(name).or_default().push(ty.inferred_type);
                    }
                }

                for (name, types) in map {
                    self.upsert_inferred(span, inferred, name, &Type::new_intersection(span, types).freezed(), opts)?;
                }

                return Ok(());
            }

            _ => {}
        }

        // Prevent logging
        match arg.normalize() {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => {
                return Ok(());
            }

            _ => {}
        }

        // Prevent logging
        let ignore = |ty: &Type| {
            matches!(
                ty.normalize(),
                Type::Enum(..)
                    | Type::EnumVariant(..)
                    | Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword
                            | TsKeywordTypeKind::TsStringKeyword
                            | TsKeywordTypeKind::TsBigIntKeyword
                            | TsKeywordTypeKind::TsBooleanKeyword,
                        ..
                    })
                    | Type::Lit(..)
            )
        };
        if ignore(param) && ignore(arg) {
            return Ok(());
        }

        // Prevent logging
        match (param.normalize(), arg.normalize()) {
            (Type::Lit(..), _) => return Ok(()),

            (
                Type::Function(..) | Type::Constructor(..) | Type::TypeLit(..) | Type::Array(..) | Type::Tuple(..),
                Type::Lit(..) | Type::Predicate(..) | Type::Keyword(..),
            )
            | (_, Type::Param(..)) => {
                warn!(
                    "Cannot infer type with param = {} and arg = {}",
                    force_dump_type_as_string(param),
                    force_dump_type_as_string(arg),
                );
                return Ok(());
            }

            _ => {}
        }

        if param.is_predicate() && arg.is_bool() {
            return Ok(());
        }

        error!(
            "unimplemented: infer_type\nparam  = {}\narg = {}",
            force_dump_type_as_string(param),
            force_dump_type_as_string(arg),
        );
        Ok(())
    }

    fn infer_type_using_mapped_type(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Mapped,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<bool> {
        let _tracing = dev_span!("infer_type_using_mapped_type");

        match arg.normalize() {
            Type::Ref(arg) => {
                let arg = self
                    .expand(
                        arg.span,
                        Type::Ref(arg.clone()),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ..Default::default()
                        },
                    )?
                    .freezed();

                match arg.normalize() {
                    Type::Ref(..) => return Ok(false),
                    _ => return self.infer_type_using_mapped_type(span, inferred, param, &arg, opts),
                }
            }
            Type::Mapped(arg) => {
                if param.type_param.name == arg.type_param.name {
                    if let Some(param_ty) = &param.ty {
                        if let Some(arg_ty) = &arg.ty {
                            self.infer_type(span, inferred, param_ty, arg_ty, opts)?;
                        }
                    }

                    return Ok(true);
                }
            }

            Type::Enum(..) | Type::Alias(..) | Type::Intersection(..) | Type::Class(..) | Type::Interface(..) => {
                let arg = self
                    .convert_type_to_type_lit(span, Cow::Borrowed(arg))
                    .context("tried to convert a type into a type literal to infer mapped type")?
                    .map(Cow::into_owned)
                    .map(Type::TypeLit);
                if let Some(arg) = arg {
                    return self.infer_type_using_mapped_type(span, inferred, param, &arg, opts);
                }
            }
            _ => {}
        }

        {
            struct Res {
                name: Id,
                key_name: Id,
                readonly: Option<TruePlusMinus>,
                optional: Option<TruePlusMinus>,
            }
            /// Matches with normalized types.
            /// type Boxed<R> = {
            ///     [P in keyof R]: Box<R[P]>;
            /// }
            fn matches(param: &Mapped) -> Option<Res> {
                match param {
                    Mapped {
                        type_param:
                            TypeParam {
                                name: key_name,
                                constraint: Some(constraint),
                                ..
                            },
                        readonly,
                        optional,
                        ..
                    } => match constraint.normalize() {
                        Type::Index(Index { ty: operator_arg, .. }) => match operator_arg.normalize() {
                            Type::Param(TypeParam { name, .. }) => Some(Res {
                                name: name.clone(),
                                key_name: key_name.clone(),
                                optional: *optional,
                                readonly: *readonly,
                            }),
                            _ => None,
                        },

                        Type::Param(TypeParam {
                            constraint: Some(constraint),
                            ..
                        }) => match constraint.normalize() {
                            Type::Param(TypeParam {
                                name: key_name,
                                constraint: Some(constraint),
                                ..
                            }) => match constraint.normalize() {
                                Type::Index(Index { ty, .. }) => match ty.normalize() {
                                    Type::Param(TypeParam { name, .. }) => Some(Res {
                                        name: name.clone(),
                                        key_name: key_name.clone(),
                                        optional: *optional,
                                        readonly: *readonly,
                                    }),
                                    _ => None,
                                },
                                _ => None,
                            },

                            _ => None,
                        },

                        _ => None,
                    },

                    _ => None,
                }
            }

            if let Some(Res {
                name,
                readonly,
                optional,
                key_name,
            }) = matches(param)
            {
                debug!(
                    "[generic/inference] Found form of `P in keyof T` where T = {}, P = {}",
                    name, key_name
                );

                match arg.normalize() {
                    Type::TypeLit(arg) => {
                        // We should make a new type literal, based on the information.
                        let mut key_types = vec![];
                        let mut new_members = Vec::<TypeElement>::with_capacity(arg.members.len());

                        // In the code below, we are given Box<R[P]> and keys of R.
                        // We have to deduce T is { a: Box<string> } from given facts.
                        //
                        // type Boxed<R> = {
                        //     [P in keyof R]: Box<R[P]>;
                        // }
                        //
                        // declare function unbox<T>(obj: Boxed<T>): T;
                        //
                        // declare let b: {
                        //     a: Box<string>,
                        // };
                        // let v = unbox(b);
                        for arg_member in &arg.members {
                            if let Some(key) = arg_member.key() {
                                match key {
                                    Key::Num(..) | Key::Normal { .. } => {
                                        key_types.push(key.ty().into_owned());
                                    }
                                    _ => {
                                        unimplemented!("Inference of keys except ident in mapped type.\nKey: {:?}", key)
                                    }
                                }
                            }

                            match arg_member {
                                TypeElement::Property(arg_prop) => {
                                    let type_ann: Option<_> = if let Some(arg_prop_ty) = &arg_prop.type_ann {
                                        if let Some(param_ty) = ALLOW_DEEP_CLONE.set(&(), || {
                                            let mut ty = param.ty.clone();
                                            ty.freeze();
                                            ty
                                        }) {
                                            let old = take(&mut self.mapped_type_param_name);
                                            self.mapped_type_param_name = vec![name.clone()];

                                            let mut data = InferData {
                                                dejavu: inferred.dejavu.clone(),
                                                ..Default::default()
                                            };
                                            self.infer_type(span, &mut data, &param_ty, arg_prop_ty, opts)?;
                                            let inferred_ty = data.type_params.remove(&name).map(|v| v.inferred_type).freezed();

                                            self.mapped_type_param_name = old;

                                            inferred_ty.or_else(|| data.defaults.remove(&name))
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    };
                                    let type_ann = type_ann
                                        .map(Box::new)
                                        .or_else(|| Some(box Type::any(arg_prop.span, Default::default())));

                                    new_members.push(TypeElement::Property(PropertySignature {
                                        optional: calc_true_plus_minus_in_param(optional, arg_prop.optional),
                                        readonly: calc_true_plus_minus_in_param(readonly, arg_prop.readonly),
                                        type_ann,
                                        ..arg_prop.clone()
                                    }));
                                }

                                TypeElement::Index(i) => {
                                    let type_ann = if let Some(arg_prop_ty) = &i.type_ann {
                                        if let Some(param_ty) = &param.ty {
                                            // TODO(kdy1): PERF

                                            let mut mapped_param_ty = arg_prop_ty.clone().foldable();

                                            replace_type(
                                                &mut mapped_param_ty,
                                                |ty| matches!(ty.normalize(), Type::Param(TypeParam { name: param_name, .. }) if name == *param_name),
                                                |ty| match ty.normalize() {
                                                    Type::Param(TypeParam { name: param_name, .. }) if name == *param_name => {
                                                        Some(*param_ty.clone())
                                                    }

                                                    _ => None,
                                                },
                                            );
                                            mapped_param_ty.freeze();

                                            self.infer_type(span, inferred, &mapped_param_ty, arg_prop_ty, opts)?;
                                        }

                                        // inferred.type_elements.remove(&name)
                                        None
                                    } else {
                                        Some(box Type::any(i.span, Default::default()))
                                    };
                                    new_members.push(TypeElement::Index(IndexSignature { type_ann, ..i.clone() }));
                                }

                                TypeElement::Method(arg_method) => {
                                    let mut arg_prop_ty = Type::Function(Function {
                                        span: arg_method.span,
                                        type_params: arg_method.type_params.clone(),
                                        params: arg_method.params.clone(),
                                        ret_ty: arg_method
                                            .ret_ty
                                            .clone()
                                            .unwrap_or_else(|| box Type::any(arg_method.span, Default::default())),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    });
                                    arg_prop_ty.freeze();
                                    let type_ann = if let Some(param_ty) = ALLOW_DEEP_CLONE.set(&(), || {
                                        let mut ty = param.ty.clone();
                                        ty.freeze();
                                        ty
                                    }) {
                                        let old = take(&mut self.mapped_type_param_name);
                                        self.mapped_type_param_name = vec![name.clone()];

                                        let mut data = InferData {
                                            dejavu: inferred.dejavu.clone(),
                                            ..Default::default()
                                        };
                                        self.infer_type(span, &mut data, &param_ty, &arg_prop_ty, opts)?;
                                        let mut defaults = take(&mut data.defaults);
                                        let mut map = self.finalize_inference(span, &[], data);
                                        let inferred_ty = map.types.remove(&name);

                                        self.mapped_type_param_name = old;

                                        inferred_ty.or_else(|| defaults.remove(&name))
                                    } else {
                                        None
                                    };
                                    let type_ann = type_ann
                                        .map(Box::new)
                                        .or_else(|| Some(box Type::any(arg_method.span, Default::default())));

                                    new_members.push(TypeElement::Property(PropertySignature {
                                        span: arg_method.span,
                                        accessibility: None,
                                        readonly: calc_true_plus_minus_in_param(readonly, arg_method.readonly),
                                        key: arg_method.key.clone(),
                                        optional: calc_true_plus_minus_in_param(optional, arg_method.optional),
                                        params: Default::default(),
                                        type_ann,
                                        type_params: Default::default(),
                                        metadata: Default::default(),
                                        accessor: Default::default(),
                                    }));
                                }

                                _ => {
                                    error!("unimplemented: infer_mapped: Mapped <- Assign: TypeElement({:#?})", arg_member);
                                    return Ok(true);
                                }
                            }
                        }

                        self.insert_inferred_raw(
                            span,
                            inferred,
                            name.clone(),
                            Cow::Owned(
                                Type::TypeLit(TypeLit {
                                    span: arg.span,
                                    members: new_members,
                                    metadata: arg.metadata,
                                    tracker: Default::default(),
                                })
                                .freezed(),
                            ),
                            opts,
                        )?;

                        let mut keys = Type::Union(Union {
                            span: param.span,
                            types: key_types,
                            metadata: UnionMetadata {
                                common: param.metadata.common,
                                ..Default::default()
                            },
                            tracker: Default::default(),
                        })
                        .fixed();
                        prevent_generalize(&mut keys);
                        keys.freeze();

                        self.insert_inferred_raw(span, inferred, key_name, Cow::Owned(keys), opts)?;

                        return Ok(true);
                    }

                    Type::Array(arg) => {
                        let new_ty = if let Some(param_ty) = &param.ty {
                            let old = take(&mut self.mapped_type_param_name);
                            self.mapped_type_param_name = vec![name.clone()];

                            let mut data = InferData {
                                dejavu: inferred.dejavu.clone(),
                                ..Default::default()
                            };
                            self.infer_type(span, &mut data, param_ty, &arg.elem_type, opts)?;
                            let mut map = self.finalize_inference(span, &[], data);
                            let mut inferred_ty = map.types.remove(&name);

                            self.mapped_type_param_name = old;

                            match &mut inferred_ty {
                                Some(ty) => {
                                    handle_optional_for_element(ty, optional);
                                }
                                None => {}
                            }

                            inferred_ty
                        } else {
                            None
                        };

                        self.insert_inferred_raw(
                            span,
                            inferred,
                            name.clone(),
                            Cow::Owned(
                                Type::Array(Array {
                                    span: arg.span,
                                    elem_type: box new_ty.unwrap_or_else(|| {
                                        Type::any(
                                            arg.span,
                                            KeywordTypeMetadata {
                                                common: arg.metadata.common,
                                                ..Default::default()
                                            },
                                        )
                                    }),
                                    metadata: arg.metadata,
                                    tracker: Default::default(),
                                })
                                .freezed(),
                            ),
                            opts,
                        )?;

                        return Ok(true);
                    }

                    Type::Tuple(arg) => {
                        let mut new_elems = vec![];
                        if let Some(param_ty) = &param.ty {
                            for elem in arg.elems.iter() {
                                let old = take(&mut self.mapped_type_param_name);
                                self.mapped_type_param_name = vec![name.clone()];

                                let mut data = InferData {
                                    dejavu: inferred.dejavu.clone(),
                                    ..Default::default()
                                };
                                self.infer_type(
                                    span,
                                    &mut data,
                                    param_ty,
                                    &elem.ty,
                                    InferTypeOpts {
                                        index_tuple_with_param: true,
                                        ..opts
                                    },
                                )?;
                                let mut map = self.finalize_inference(span, &[], data);
                                let mut inferred_ty = map.types.remove(&name);

                                self.mapped_type_param_name = old;

                                match &mut inferred_ty {
                                    Some(ty) => {
                                        handle_optional_for_element(ty, optional);
                                    }
                                    None => {}
                                }

                                new_elems.push(TupleElement {
                                    span: elem.span,
                                    label: elem.label.clone(),
                                    ty: box inferred_ty.unwrap_or_else(|| Type::any(elem.span, Default::default())),
                                    tracker: Default::default(),
                                });
                            }
                        }

                        self.insert_inferred_raw(
                            span,
                            inferred,
                            name.clone(),
                            Cow::Owned(
                                Type::Tuple(Tuple {
                                    span: arg.span,
                                    elems: new_elems,
                                    metadata: arg.metadata,
                                    tracker: Default::default(),
                                })
                                .freezed(),
                            ),
                            opts,
                        )?;

                        return Ok(true);
                    }

                    _ => {
                        dbg!();
                    }
                }
            }
        }

        {
            // Record<Key, Type> expands to
            //
            //
            // type Record<Key extends keyof any, Type> = {
            //     [P in Key]: Type;
            // };
            if let Some(constraint) = &param.type_param.constraint {
                if let Type::Param(type_param) = constraint.normalize() {
                    debug!(
                        "[generic/inference] Found form of `P in T` where T = {}, P = {}",
                        type_param.name, param.type_param.name
                    );

                    if let Type::TypeLit(arg) = arg.normalize() {
                        let key_ty = arg.members.iter().filter_map(|element| match element {
                            TypeElement::Property(p) => match &p.key {
                                Key::Private(..) | Key::Computed(..) => None,
                                _ => Some(p.key.ty().into_owned()),
                            }, // TODO(kdy1): Handle method element
                            _ => None,
                        });
                        let mut key_ty = Type::new_union(span, key_ty);
                        prevent_generalize(&mut key_ty);
                        key_ty.freeze();
                        self.insert_inferred(span, inferred, type_param, Cow::Owned(key_ty), opts)?;
                    }

                    let param_ty = param.ty.as_ref().unwrap();

                    let names = {
                        let mut tp = type_param;

                        loop {
                            match &tp.constraint.as_ref().map(|v| v.normalize()) {
                                Some(Type::Param(p)) => {
                                    tp = p;
                                }
                                Some(Type::Index(Index {
                                    ty: box Type::Param(p), ..
                                })) => {
                                    tp = p;
                                }
                                _ => {
                                    break;
                                }
                            }
                        }

                        match &tp.constraint {
                            Some(box Type::Union(ty))
                                if ty.types.iter().all(|ty| {
                                    matches!(
                                        ty.normalize(),
                                        Type::Index(Index {
                                            ty: box Type::Param(..),
                                            ..
                                        })
                                    )
                                }) =>
                            {
                                ty.types
                                    .iter()
                                    .map(|ty| match ty.normalize() {
                                        Type::Index(Index {
                                            ty: box Type::Param(p), ..
                                        }) => p.name.clone(),
                                        _ => unreachable!(),
                                    })
                                    .collect()
                            }

                            _ => vec![tp.name.clone()],
                        }
                    };

                    let old = take(&mut self.mapped_type_param_name);
                    self.mapped_type_param_name = names.clone();
                    {
                        let mut v = MappedReverser::default();
                        let reversed_param_ty = param_ty.clone().fold_with(&mut v);

                        if v.did_work {
                            self.infer_type(span, inferred, &reversed_param_ty, arg, opts)?;
                            self.mapped_type_param_name = old;

                            return Ok(true);
                        }
                    }

                    if let Type::TypeLit(arg) = arg.normalize() {
                        let mut type_elements = FxHashMap::<_, Vec<_>>::default();

                        if let Some(param_ty) = &param.ty {
                            for m in &arg.members {
                                match m {
                                    TypeElement::Property(p) => {
                                        //
                                        if let Some(ref type_ann) = p.type_ann {
                                            self.infer_type(span, inferred, param_ty, type_ann, opts)?;
                                        }

                                        for name in &names {
                                            if *name == type_param.name {
                                                continue;
                                            }

                                            let ty = inferred.type_params.remove(name).map(|v| box v.inferred_type);

                                            type_elements
                                                .entry(name.clone())
                                                .or_default()
                                                .push(TypeElement::Property(PropertySignature {
                                                    optional: calc_true_plus_minus_in_param(param.optional, p.optional),
                                                    readonly: calc_true_plus_minus_in_param(param.readonly, p.readonly),
                                                    type_ann: ty,
                                                    ..p.clone()
                                                }));
                                        }
                                    }

                                    _ => {
                                        unimplemented!("infer_type: Mapped <- Assign: TypeElement({:?})", m)
                                    }
                                }
                            }

                            for name in names {
                                if name == type_param.name {
                                    continue;
                                }

                                let list_ty = Type::TypeLit(TypeLit {
                                    span: arg.span,
                                    members: type_elements.remove(&name).unwrap_or_default(),
                                    metadata: arg.metadata,
                                    tracker: Default::default(),
                                })
                                .freezed();

                                self.insert_inferred_raw(span, inferred, name.clone(), Cow::Owned(list_ty), opts)?;
                            }
                        }

                        self.mapped_type_param_name = old;
                        return Ok(true);
                    }

                    self.mapped_type_param_name = old;
                }
            }
        }

        match &param.type_param.constraint {
            Some(constraint) => {
                if let Type::Index(operator) = constraint.normalize() {
                    if let Type::IndexedAccessType(
                        iat @ IndexedAccessType {
                            obj_type: box Type::Param(..),
                            index_type: box Type::Param(..),
                            ..
                        },
                    ) = operator.ty.normalize()
                    {
                        if let Type::Param(..) = iat.obj_type.normalize() {
                            if let Type::Param(..) = iat.index_type.normalize() {
                                let param_ty = param.ty.clone().unwrap();
                                let name = param.type_param.name.clone();
                                let (obj_ty, index_ty) = match &**param.type_param.constraint.as_ref().unwrap() {
                                    Type::Index(Index {
                                        ty:
                                            box Type::IndexedAccessType(IndexedAccessType {
                                                obj_type: box Type::Param(obj_ty),
                                                index_type: box Type::Param(index_ty),
                                                ..
                                            }),
                                        ..
                                    }) => (obj_ty, index_ty),
                                    _ => unreachable!(),
                                };
                                if name == index_ty.name {
                                    match arg.normalize() {
                                        Type::TypeLit(arg) => {
                                            let mut members = Vec::with_capacity(arg.members.len());

                                            for m in &arg.members {
                                                match m {
                                                    TypeElement::Property(p) => {
                                                        let optional = calc_true_plus_minus_in_param(param.optional, p.optional);
                                                        //
                                                        if let Some(ref type_ann) = p.type_ann {
                                                            self.infer_type(span, inferred, &param_ty, type_ann, opts)?;
                                                        }
                                                        members.push(TypeElement::Property(PropertySignature {
                                                            optional,
                                                            readonly: calc_true_plus_minus_in_param(param.readonly, p.readonly),
                                                            type_ann: None,
                                                            ..p.clone()
                                                        }));
                                                    }

                                                    _ => unimplemented!("infer_type: Mapped <- Assign: TypeElement({:?})", m),
                                                }
                                            }

                                            let list_ty = Type::TypeLit(TypeLit {
                                                span: arg.span,
                                                members,
                                                metadata: arg.metadata,
                                                tracker: Default::default(),
                                            });

                                            self.insert_inferred_raw(span, inferred, name, Cow::Owned(list_ty), opts)?;
                                            return Ok(true);
                                        }

                                        _ => {
                                            dbg!();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            None => {}
        }

        {
            match &param.type_param.constraint {
                Some(constraint) => {
                    if let Type::Index(
                        operator @ Index {
                            ty: box Type::Mapped(Mapped { ty: Some(..), .. }),
                            ..
                        },
                    ) = constraint.normalize()
                    {
                        if let Type::Mapped(..) = operator.ty.normalize() {
                            let reversed_param_ty = param.ty.as_ref().unwrap().clone().fold_with(&mut MappedReverser::default());

                            self.infer_type(span, inferred, &reversed_param_ty, arg, opts)?;

                            return Ok(true);
                        }
                    }
                }
                None => {}
            }
        }

        {
            // We handle all other mapped types at here.
            //
            //
            // In the code below,
            //
            // declare type Boxed<Pick<P, T>> = {
            //     [BoxedP in keyof Pick<P, K>[BoxedT]]: Box<Pick<P, K>[BoxedP]>;
            // };
            if let Some(constraint) = &param.type_param.constraint {
                if let Type::Index(Index { ty, .. }) = constraint.normalize() {
                    if let Some(param_ty) = &param.ty {
                        if let Type::TypeLit(arg_lit) = arg.normalize() {
                            let reversed_param_ty = param_ty.clone().fold_with(&mut MappedReverser::default()).freezed();
                            print_type("reversed", &reversed_param_ty);

                            self.infer_type(span, inferred, &reversed_param_ty, arg, opts)?;

                            return Ok(true);
                        }
                    }
                }
            }
        }

        Ok(false)
    }

    fn infer_type_using_tuple_and_tuple(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Tuple,
        param_ty: &Type,
        arg: &Tuple,
        arg_ty: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let len = param.elems.len().max(arg.elems.len());

        let l_max = param.elems.len().saturating_sub(get_tuple_subtract_count(&arg.elems));
        let r_max = arg.elems.len().saturating_sub(get_tuple_subtract_count(&param.elems));

        let _tracing = dev_span!("infer_type_using_tuple_and_tuple", l_max = l_max, r_max = r_max);

        for index in 0..len {
            let li = min(index, l_max);
            let ri = min(index, r_max);

            let _tracing = dev_span!("infer_type_using_tuple_and_tuple", li = li, ri = ri);

            let l_elem_type = self.access_property(
                span,
                param_ty,
                &Key::Num(RNumber {
                    span,
                    value: li as _,
                    raw: None,
                }),
                TypeOfMode::RValue,
                IdCtx::Type,
                AccessPropertyOpts {
                    do_not_validate_type_of_computed_prop: true,
                    disallow_indexing_array_with_string: true,
                    disallow_creating_indexed_type_from_ty_els: true,
                    disallow_indexing_class_with_computed: true,
                    use_undefined_for_tuple_index_error: true,
                    return_rest_tuple_element_as_is: true,
                    ..Default::default()
                },
            )?;

            let r_elem_type = self.access_property(
                span,
                arg_ty,
                &Key::Num(RNumber {
                    span,
                    value: ri as _,
                    raw: None,
                }),
                TypeOfMode::RValue,
                IdCtx::Type,
                AccessPropertyOpts {
                    do_not_validate_type_of_computed_prop: true,
                    disallow_indexing_array_with_string: true,
                    disallow_creating_indexed_type_from_ty_els: true,
                    disallow_indexing_class_with_computed: true,
                    use_undefined_for_tuple_index_error: true,
                    return_rest_tuple_element_as_is: true,
                    ..Default::default()
                },
            )?;

            self.infer_type(
                span,
                inferred,
                &l_elem_type,
                &r_elem_type,
                InferTypeOpts {
                    append_type_as_union: true,
                    ..opts
                },
            )?;
        }

        Ok(())
    }

    fn infer_type_of_fn_param(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &FnParam,
        arg: &FnParam,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.infer_type(
            span,
            inferred,
            &param.ty,
            &arg.ty,
            InferTypeOpts {
                append_type_as_union: opts.append_type_as_union || opts.for_fn_assignment,

                ..opts
            },
        )
    }

    fn infer_type_of_fn_params(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        params: &[FnParam],
        args: &[FnParam],
        opts: InferTypeOpts,
    ) -> VResult<()> {
        for (param, arg) in params.iter().zip(args) {
            self.infer_type_of_fn_param(span, inferred, param, arg, opts)?
        }

        if params.len() > args.len() {
            for param in &params[args.len()..] {
                if let Type::Param(param) = &*param.ty {
                    // TODO: Union
                    inferred.defaults.insert(
                        param.name.clone(),
                        Type::unknown(param.span.with_ctxt(SyntaxContext::empty()), Default::default()),
                    );
                }
            }
        }

        Ok(())
    }

    fn rename_inferred(&mut self, span: Span, inferred: &mut InferData, arg_type_params: &TypeParamDecl) -> VResult<()> {
        info!("rename_inferred");

        if arg_type_params.params.iter().any(|v| inferred.errored.contains(&v.name)) {
            return Ok(());
        }

        //
        let mut fixed = FxHashMap::default();

        inferred.type_params.iter().for_each(|(param_name, ty)| {
            // Ignore unrelated type parameters
            if arg_type_params.params.iter().all(|v| *param_name != v.name) {
                return;
            }

            fixed.insert(param_name.clone(), ty.inferred_type.clone());
        });

        inferred.type_params.iter_mut().for_each(|(_, ty)| {
            replace_type(
                &mut ty.inferred_type,
                |node| match node.normalize() {
                    Type::Param(p) => fixed.contains_key(&p.name),
                    _ => false,
                },
                |node| {
                    //
                    match node.normalize() {
                        Type::Param(p) if fixed.contains_key(&p.name) => Some((*fixed.get(&p.name).unwrap()).clone()),
                        _ => None,
                    }
                },
            );

            ty.inferred_type.freeze();
        });

        Ok(())
    }
}

fn array_elem_type(t: &Type) -> Option<&Type> {
    if let Type::Array(a) = t.normalize() {
        return Some(&a.elem_type);
    }

    if let Some(elem) = unwrap_builtin_with_single_arg(t, "Array")
        .or_else(|| unwrap_builtin_with_single_arg(t, "ArrayLike"))
        .or_else(|| unwrap_builtin_with_single_arg(t, "ReadonlyArray"))
    {
        elem.assert_clone_cheap();
        return Some(elem);
    }

    None
}

/// Handles renaming of the type parameters.
impl Analyzer<'_, '_> {
    pub(super) fn rename_type_params(&mut self, span: Span, mut ty: Type, type_ann: Option<&Type>) -> VResult<Type> {
        if self.config.is_builtin {
            return Ok(ty);
        }

        ty.freeze();

        debug!(
            "rename_type_params(has_ann = {:?}, ty = {})",
            type_ann.is_some(),
            dump_type_as_string(&ty)
        );

        if ty.is_intersection() {
            return Ok(ty);
        }

        let mut usage_visitor = TypeParamUsageFinder::default();
        ty.normalize().visit_with(&mut usage_visitor);
        if usage_visitor.params.is_empty() {
            debug!("rename_type_param: No type parameter is used in type");

            if let Some(f) = ty.as_fn_type_mut() {
                f.type_params = None;
            }

            return Ok(ty);
        }

        if let Some(type_ann) = type_ann {
            let mut inferred = InferData::default();

            self.infer_type(span, &mut inferred, &ty, type_ann, Default::default())?;
            info!(
                "renaming type parameters based on type annotation provided by user\ntype_ann = {:?}",
                type_ann
            );

            let map = self.finalize_inference(span, &usage_visitor.params, inferred);

            // TODO(kdy1): PERF
            return Ok(ty
                .foldable()
                .fold_with(&mut TypeParamRenamer {
                    inferred: map.types,
                    declared: Default::default(),
                })
                .fixed());
        }

        let decl = Some(TypeParamDecl {
            span: DUMMY_SP,
            params: usage_visitor.params,
            tracker: Default::default(),
        });

        if let Some(ref mut f) = ty.as_fn_type_mut() {
            f.type_params = decl;
        } else if matches!(ty.normalize(), Type::ClassDef(..) | Type::Class(..)) {
            return Ok(ty);
        }

        Ok(ty.foldable().fold_with(&mut TypeParamRemover::new()).fixed())
    }
}

/// This method returns true for types like `'foo'` and `'foo' | 'bar'`.
pub(super) fn is_literals(ty: &Type) -> bool {
    match ty.normalize() {
        Type::Lit(_) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(is_literals),
        _ => false,
    }
}

struct SingleTypeParamReplacer<'a> {
    name: &'a Id,
    to: &'a Type,
}

impl Fold<Type> for SingleTypeParamReplacer<'_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty = ty.fold_children_with(self);

        match &ty {
            Type::Param(TypeParam { name, .. }) if *self.name == *name => return (*self.to).clone(),

            _ => {}
        }

        ty
    }
}

pub(crate) fn calc_true_plus_minus_in_param(param: Option<TruePlusMinus>, previous: bool) -> bool {
    match param {
        Some(v) => match v {
            TruePlusMinus::True => false,
            TruePlusMinus::Plus => true,
            TruePlusMinus::Minus => true,
        },
        None => previous,
    }
}

/// Replaces type parameters with name `from` to type `to`.
struct MappedKeyReplacer<'a> {
    /// The name of type parameter
    from: &'a Id,
    /// Type of key. This is typically literal.
    to: &'a Type,
}

impl VisitMut<Type> for MappedKeyReplacer<'_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        match ty.normalize() {
            Type::Param(param) if *self.from == param.name => {
                *ty = self.to.clone();
            }
            _ => {
                // TODO(kdy1): PERF
                ty.normalize_mut();
                ty.visit_mut_children_with(self)
            }
        }
    }
}

struct MappedIndexTypeReplacer<'a> {
    obj_ty: &'a Type,
    /// The name of key
    index_param_name: &'a Id,

    to: &'a Type,
}

impl VisitMut<Type> for MappedIndexTypeReplacer<'_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty.visit_mut_children_with(self);

        if let Type::IndexedAccessType(IndexedAccessType { obj_type, index_type, .. }) = &*ty {
            if self.obj_ty.type_eq(&**obj_type) {
                match &**index_type {
                    Type::Param(key) if *self.index_param_name == key.name => {
                        *ty = self.to.clone();
                    }
                    _ => {}
                }
            }
        }
    }
}

/// This struct reverses the mapped type.
///
/// ===== ===== ===== Type (param_ty) ===== ===== =====
/// TYPE as {
//     value: {
///         [P in K]: T[P];
///     };
/// };
///
/// ===== ===== ===== Type (ty) ===== ===== =====
/// TYPE as {
///     [P in K]: T[P];
/// };
/// ===== ===== ===== Type (arg) ===== ===== =====
/// TYPE as {
///     foo: {
///         value: number;
///     };
///     bar: {
///         value: string;
///     };
/// };
#[derive(Default)]
struct MappedReverser {
    did_work: bool,
}

impl Fold<Type> for MappedReverser {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty = ty.fold_children_with(self);

        match ty {
            Type::TypeLit(TypeLit {
                span, members, metadata, ..
            }) if members.len() == 1
                && members.iter().any(|member| match member {
                    TypeElement::Property(p) => {
                        if let Some(ty) = &p.type_ann {
                            ty.is_mapped()
                        } else {
                            false
                        }
                    }
                    TypeElement::Method(_) => unimplemented!(),
                    TypeElement::Index(_) => unimplemented!(),
                    _ => false,
                }) =>
            {
                self.did_work = true;
                let member = members.into_iter().next().unwrap();

                match member {
                    TypeElement::Property(p) => {
                        let mapped: Mapped = p.type_ann.unwrap().mapped().unwrap();
                        let ty = box Type::TypeLit(TypeLit {
                            span,
                            members: vec![TypeElement::Property(PropertySignature { type_ann: mapped.ty, ..p })],
                            metadata,
                            tracker: Default::default(),
                        });

                        return Type::Mapped(Mapped { ty: Some(ty), ..mapped });
                    }
                    TypeElement::Method(_) => unimplemented!(),
                    TypeElement::Index(_) => unimplemented!(),
                    _ => unreachable!(),
                }
            }
            _ => {}
        }

        ty
    }
}

struct MappedIndexedSimplifier;

impl Fold<Type> for MappedIndexedSimplifier {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty = ty.fold_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type,
                index_type:
                    box Type::Param(TypeParam {
                        name: index_name,
                        constraint: Some(box Type::Index(Index { ty: indexed_ty, .. })),
                        ..
                    }),
                ..
            }) if obj_type.type_eq(&indexed_ty) => {
                return *obj_type;
            }
            _ => {}
        }

        ty
    }
}

fn handle_optional_for_element(element_ty: &mut Type, optional: Option<TruePlusMinus>) {
    let v = match optional {
        Some(v) => v,
        None => return,
    };

    match v {
        TruePlusMinus::True => {
            if element_ty.is_optional() {
                match element_ty.normalize_mut() {
                    Type::Optional(ty) => {
                        let ty = ty.ty.take();
                        let ty = ty.remove_falsy();

                        *element_ty = ty;
                    }
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                let new_ty = element_ty.take().remove_falsy();

                *element_ty = new_ty;
            }
        }
        TruePlusMinus::Plus => match element_ty.normalize() {
            Type::Optional(ty) => {}
            _ => {
                let ty = box element_ty.take();
                *element_ty = Type::Optional(OptionalType {
                    span: DUMMY_SP,
                    ty,
                    metadata: Default::default(),
                    tracker: Default::default(),
                });
            }
        },
        TruePlusMinus::Minus => {}
    }
}
