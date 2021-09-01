pub(crate) use self::{expander::ExtendsOpts, inference::InferTypeOpts};
use crate::{
    analyzer::{assign::AssignOpts, scope::ExpandOpts, Analyzer, Ctx},
    ty::TypeExt,
    util::{unwrap_ref_with_single_arg, RemoveTypes},
    ValidationResult,
};
use fxhash::FxHashMap;
use itertools::{EitherOrBoth, Itertools};
use rnode::{Fold, FoldWith, NodeId, VisitMut, VisitMutWith, VisitWith};
use stc_ts_ast_rnode::{RIdent, RPat, RStr, RTsEntityName, RTsKeywordType, RTsLit, RTsLitType};
use stc_ts_errors::{
    debug::{dump_type_as_string, print_backtrace, print_type},
    DebugExt,
};
use stc_ts_generics::type_param::{finder::TypeParamUsageFinder, remover::TypeParamRemover, renamer::TypeParamRenamer};
use stc_ts_type_ops::Fix;
use stc_ts_types::{
    Array, ClassMember, FnParam, Function, Id, IndexSignature, IndexedAccessType, Intersection, Key, Mapped, ModuleId,
    Operator, OptionalType, PropertySignature, Ref, Tuple, TupleElement, Type, TypeElement, TypeLit, TypeOrSpread,
    TypeParam, TypeParamDecl, TypeParamInstantiation, Union,
};
use stc_ts_utils::MapWithMut;
use stc_utils::{error::context, stack};
use std::{borrow::Cow, collections::hash_map::Entry, mem::take, time::Instant};
use swc_common::{EqIgnoreSpan, Span, Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;
use tracing::{debug, error, info, instrument, trace, warn};

mod expander;
mod inference;
mod type_form;

#[derive(Debug, Clone)]
enum InferredType {
    Union(Type),
    Other(Vec<Type>),
}

#[derive(Debug, Default)]
pub(super) struct InferData {
    /// Inferred type parameters
    type_params: FxHashMap<Id, InferredType>,

    /// For the code below, we can know that `T` defaults to `unknown` while
    /// inferring type of funcation parametrs. We cannot know the type before
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
}

/// Type inference for arguments.
impl Analyzer<'_, '_> {
    /// Create [TypeParamInstantiation] from inferred type information.
    pub(super) fn instantiate(
        &mut self,
        span: Span,
        type_params: &[TypeParam],
        mut inferred: FxHashMap<Id, Type>,
    ) -> ValidationResult<TypeParamInstantiation> {
        let mut params = Vec::with_capacity(type_params.len());
        for type_param in type_params {
            if let Some(ty) = inferred.remove(&type_param.name) {
                info!("infer_arg_type: {}", type_param.name);
                params.push(ty);
            } else {
                match type_param.constraint {
                    Some(box Type::Param(ref p)) => {
                        // TODO: Handle complex inheritance like
                        //      function foo<A extends B, B extends C>(){ }

                        if let Some(actual) = inferred.remove(&p.name) {
                            info!(
                                "infer_arg_type: {} => {} => {:?} because of the extends clause",
                                type_param.name, p.name, actual
                            );
                            params.push(actual);
                        } else {
                            info!(
                                "infer_arg_type: {} => {} because of the extends clause",
                                type_param.name, p.name
                            );
                            params.push(Type::Param(p.clone()));
                        }
                        continue;
                    }
                    _ => {}
                }

                if type_param.constraint.is_some() && is_literals(&type_param.constraint.as_ref().unwrap()) {
                    params.push(*type_param.constraint.clone().unwrap());
                    continue;
                }

                if type_param.constraint.is_some()
                    && match **type_param.constraint.as_ref().unwrap() {
                        Type::Interface(..) | Type::Keyword(..) | Type::Ref(..) | Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    let ty = self.expand(
                        span,
                        *type_param.constraint.clone().unwrap(),
                        ExpandOpts {
                            full: true,
                            expand_union: false,
                            ..Default::default()
                        },
                    )?;
                    params.push(ty);
                    continue;
                }

                warn!("instantiate: A type parameter {} defaults to {{}}", type_param.name);

                // Defaults to {}
                params.push(Type::TypeLit(TypeLit {
                    span,
                    members: vec![],
                    metadata: Default::default(),
                }));
            }
        }

        Ok(TypeParamInstantiation { span: DUMMY_SP, params })
    }

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
    ) -> ValidationResult<FxHashMap<Id, Type>> {
        warn!(
            "infer_arg_types: {:?}",
            type_params.iter().map(|p| format!("{}, ", p.name)).collect::<String>()
        );

        let start = Instant::now();

        let opts = InferTypeOpts::default();

        let mut inferred = InferData::default();

        if let Some(base) = base {
            for (param, type_param) in base.params.iter().zip(type_params) {
                info!("User provided `{:?} = {:?}`", type_param.name, param.clone());
                inferred
                    .type_params
                    .insert(type_param.name.clone(), InferredType::Other(vec![param.clone()]));
            }
        }

        // We allocate a new vertor only if required.
        let mut actual_args;
        let args = if args.iter().any(|arg| arg.spread.is_some()) {
            actual_args = vec![];
            for arg in args {
                if arg.spread.is_some() {
                    match arg.ty.normalize() {
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

        for (idx, p) in params.iter().enumerate() {
            let is_rest = match &p.pat {
                RPat::Rest(_) => true,
                _ => false,
            };

            if !is_rest {
                if let Some(arg) = args.get(idx) {
                    self.infer_type(span, &mut inferred, &p.ty, &arg.ty, opts)?;
                }
            } else {
                match p.ty.normalize() {
                    Type::Param(param) => {
                        self.infer_type(
                            span,
                            &mut inferred,
                            &p.ty,
                            &Type::Tuple(Tuple {
                                span: p.ty.span(),
                                elems: args[idx..]
                                    .iter()
                                    .map(|arg| &arg.ty)
                                    .cloned()
                                    .map(|ty| TupleElement {
                                        span: DUMMY_SP,
                                        label: None,
                                        ty,
                                    })
                                    .collect(),
                            }),
                            opts,
                        )?;
                    }
                    Type::Array(p_ty) => {
                        // Handle varargs. This result in union of all types.
                        for arg in &args[idx..] {
                            self.infer_type(span, &mut inferred, &p_ty.elem_type, &arg.ty, opts)?;
                        }
                    }
                    _ => {
                        // Handle varargs
                        for arg in &args[idx..] {
                            self.infer_type(span, &mut inferred, &p.ty, &arg.ty, opts)?;
                        }
                    }
                }
            }
        }

        // Defaults
        for type_param in type_params {
            if inferred.type_params.contains_key(&type_param.name) {
                continue;
            }

            match type_param.constraint {
                Some(box Type::Param(ref p)) => {
                    // TODO: Handle complex inheritance like
                    //      function foo<A extends B, B extends C>(){ }

                    if let Some(actual) = inferred.type_params.remove(&p.name) {
                        info!(
                            "infer_arg_type: {} => {} => {:?} because of the extends clause",
                            type_param.name, p.name, actual
                        );
                        inferred.type_params.insert(p.name.clone(), actual.clone());
                        inferred.type_params.insert(type_param.name.clone(), actual);
                    } else {
                        info!(
                            "infer_arg_type: {} => {} because of the extends clause",
                            type_param.name, p.name
                        );
                        self.insert_inferred(
                            span,
                            &mut inferred,
                            type_param.name.clone(),
                            Cow::Owned(Type::Param(p.clone())),
                            opts,
                        )?;
                    }
                    continue;
                }
                _ => {}
            }

            if type_param.constraint.is_some() && is_literals(&type_param.constraint.as_ref().unwrap()) {
                self.insert_inferred(
                    span,
                    &mut inferred,
                    type_param.name.clone(),
                    Cow::Borrowed(&type_param.constraint.as_deref().unwrap()),
                    opts,
                )?;
                continue;
            }

            if type_param.constraint.is_some()
                && match **type_param.constraint.as_ref().unwrap() {
                    Type::Interface(..) | Type::Keyword(..) | Type::Ref(..) | Type::TypeLit(..) => true,
                    _ => false,
                }
            {
                let ctx = Ctx {
                    preserve_params: true,
                    preserve_ret_ty: true,
                    ..self.ctx
                };
                let ty = self.with_ctx(ctx).expand(
                    span,
                    *type_param.constraint.clone().unwrap(),
                    ExpandOpts {
                        full: true,
                        expand_union: false,
                        ..Default::default()
                    },
                )?;
                if !inferred.type_params.contains_key(&type_param.name) {
                    self.insert_inferred(span, &mut inferred, type_param.name.clone(), Cow::Owned(ty), opts)?;
                }
                continue;
            }
            if !inferred.type_params.contains_key(&type_param.name) {
                if let Some(default_ty) = inferred.defaults.remove(&type_param.name) {
                    self.insert_inferred(
                        span,
                        &mut inferred,
                        type_param.name.clone(),
                        Cow::Owned(default_ty),
                        opts,
                    )?;
                } else {
                    if let Some(default) = &type_param.default {
                        self.insert_inferred(
                            span,
                            &mut inferred,
                            type_param.name.clone(),
                            Cow::Borrowed(&default),
                            opts,
                        )?;
                        continue;
                    }

                    if let Some(default_ty) = default_ty {
                        error!(
                            "infer: A type parameter {} defaults to {:?}",
                            type_param.name, default_ty
                        );

                        self.insert_inferred(
                            span,
                            &mut inferred,
                            type_param.name.clone(),
                            Cow::Borrowed(&default_ty),
                            opts,
                        )?;
                    }
                }
            }
        }

        self.prevent_generalization_of_inferred_types(type_params, &mut inferred);

        let map = self.finalize_inference(inferred);

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
    ) -> ValidationResult<FxHashMap<Id, Type>> {
        let mut inferred = InferData::default();
        self.infer_type(span, &mut inferred, base, concrete, opts)?;
        let map = self.finalize_inference(inferred);

        Ok(map)
    }

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
    /// declare const usymbol: unique symbol
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
    /// foo({ bar: usymbol, baz: 5 }) // Error on baz (unique symbol is selected)
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
    /// arr([usymbol, 5]) // Ok
    ///
    ///
    /// arr([[], '']); // Ok
    /// arr([{}, '']); // Ok
    ///
    /// arr([1, u]) // Ok
    /// arr([{}, u]) // Ok
    /// ```
    #[instrument(name = "infer_type", skip(self, span, inferred, param, arg, opts))]
    fn infer_type(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        if self.is_builtin {
            return Ok(());
        }

        let param_str = dump_type_as_string(&self.cm, &param);
        let arg_str = dump_type_as_string(&self.cm, &arg);

        let start = Instant::now();

        let res = self.infer_type_inner(span, inferred, param, arg, opts);

        let end = Instant::now();

        debug!(
            kind = "perf",
            op = "infer_type",
            "infer_type: `{}` === `{}`. (took {:?})",
            param_str,
            arg_str,
            end - start
        );

        res
    }

    /// Infer types, so that `param` has same type as `arg`.
    ///
    ///
    /// TODO: Optimize
    fn infer_type_inner(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        if self.is_builtin {
            return Ok(());
        }

        let marks = self.marks();

        let _stack = match stack::track(span) {
            Ok(v) => v,
            Err(_) => return Ok(()),
        };

        let _ctx = context(format!(
            "infer_type()\nParam: {}\nArg: {}",
            dump_type_as_string(&self.cm, &param),
            dump_type_as_string(&self.cm, &arg)
        ));

        if inferred
            .dejavu
            .iter()
            .any(|(prev_param, prev_arg)| prev_param.type_eq(param) && prev_arg.type_eq(arg))
        {
            return Ok(());
        }
        inferred.dejavu.push((param.clone(), arg.clone()));

        debug_assert!(!span.is_dummy(), "infer_type: `span` should not be dummy");

        let param = param.normalize();
        let arg = arg.normalize();

        print_type(&"param", &self.cm, &param);
        print_type(&"arg", &self.cm, &arg);

        match param {
            Type::Instance(..) => {
                let param = self.normalize(Some(span), Cow::Borrowed(&param), Default::default())?;
                return self.infer_type(span, inferred, &param, arg, opts);
            }
            _ => {}
        }

        match arg {
            Type::Instance(..) => {
                let arg = self.normalize(Some(span), Cow::Borrowed(&arg), Default::default())?;

                return self.infer_type(span, inferred, param, &arg, opts);
            }
            _ => {}
        }

        if param.type_eq(arg) {
            return Ok(());
        }

        if param.is_keyword() {
            return Ok(());
        }

        let p;
        let param = match param {
            Type::Mapped(..) => {
                p = box param.clone().foldable().fold_with(&mut MappedIndexedSimplifier);
                &p
            }
            _ => param,
        };

        if cfg!(feature = "fastpath") {
            let opts = InferTypeOpts {
                append_type_as_union: true,
                ..opts
            };
            if let Some(param_elem) = unwrap_ref_with_single_arg(param, "Array")
                .or_else(|| unwrap_ref_with_single_arg(&param, "ArrayLike"))
                .or_else(|| unwrap_ref_with_single_arg(&param, "ReadonlyArray"))
            {
                match arg {
                    Type::Array(arg) => {
                        return self.infer_type(span, inferred, &param_elem, &arg.elem_type, opts);
                    }
                    _ => {}
                }

                if let Some(arg_elem) = unwrap_ref_with_single_arg(arg, "Array")
                    .or_else(|| unwrap_ref_with_single_arg(arg, "ArrayLike"))
                    .or_else(|| unwrap_ref_with_single_arg(&arg, "ReadonlyArray"))
                {
                    return self.infer_type(span, inferred, &param_elem, &arg_elem, opts);
                }
            }
        }

        match (param, arg) {
            (Type::Union(p), Type::Union(a)) => {
                self.infer_type_using_union_and_union(
                    span,
                    inferred,
                    p,
                    arg,
                    a,
                    InferTypeOpts {
                        append_type_as_union: true,
                        ..opts
                    },
                )?;

                return Ok(());
            }

            (Type::Union(param), _) => {
                return self.infer_type_using_union(
                    span,
                    inferred,
                    param,
                    arg,
                    InferTypeOpts {
                        append_type_as_union: true,
                        ..opts
                    },
                );
            }

            (Type::Intersection(param), _) => {
                for param in &param.types {
                    self.infer_type(span, inferred, param, arg, opts)?;
                }

                return Ok(());
            }

            (_, Type::Union(arg)) => {
                if opts.append_type_as_union {
                    //
                    for a in &arg.types {
                        self.infer_type(span, inferred, param, a, opts)?;
                    }

                    return Ok(());
                }
            }

            _ => {}
        }

        let p = param;
        let a = arg;

        if let Some(res) = self.infer_builtin(span, inferred, param, arg, opts) {
            return res;
        }

        if self.infer_type_by_converting_to_type_lit(span, inferred, param, arg, opts)? {
            return Ok(());
        }

        match arg {
            Type::Param(arg) => {
                if !param.normalize().is_type_param() {
                    self.insert_inferred(span, inferred, arg.name.clone(), Cow::Borrowed(&param), opts)?;
                    return Ok(());
                }
            }
            _ => {}
        }

        match param {
            Type::Param(TypeParam {
                ref name,
                ref constraint,
                ..
            }) => {
                let constraint = constraint.as_ref().map(|ty| ty.normalize());
                if !self.ctx.skip_identical_while_inferencing {
                    if let Some(prev) = inferred.type_params.get(name).cloned() {
                        let ctx = Ctx {
                            skip_identical_while_inferencing: true,
                            ..self.ctx
                        };
                        match prev {
                            InferredType::Union(prev) => {
                                self.with_ctx(ctx).infer_type(span, inferred, &prev, arg, opts)?;
                            }
                            InferredType::Other(prev) => {
                                let prev = Type::union(prev).cheap();
                                self.with_ctx(ctx).infer_type(span, inferred, &prev, arg, opts)?;
                            }
                        }
                    }
                }

                trace!("infer_type: type parameter: {} = {:?}", name, constraint);

                if constraint.is_some() && is_literals(&constraint.as_ref().unwrap()) {
                    info!("infer from literal constraint: {} = {:?}", name, constraint);
                    if let Some(orig) = inferred.type_params.get(&name) {
                        let orig = match orig.clone() {
                            InferredType::Union(ty) => ty,
                            InferredType::Other(types) => Type::union(types),
                        };

                        if !orig.eq_ignore_span(&constraint.as_ref().unwrap()) {
                            print_backtrace();
                            panic!(
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

                    // TODO: Infer only if constraints are matched
                    //
                    // if let Some(false) = self.extends(span, ExtendsOpts {
                    // ..Default::default() }, arg, constraint) {
                    //     return Ok(());
                    // }
                }

                if self.ctx.skip_identical_while_inferencing {
                    match arg {
                        Type::Param(arg) => {
                            if *name == arg.name {
                                return Ok(());
                            }
                        }
                        _ => {}
                    }
                }

                if arg.is_any() && self.is_implicitly_typed(&arg) {
                    if inferred.type_params.contains_key(&name.clone()) {
                        return Ok(());
                    }

                    match inferred.defaults.entry(name.clone()) {
                        Entry::Occupied(..) => {}
                        Entry::Vacant(e) => {
                            e.insert(Type::Param(TypeParam {
                                span: arg.span(),
                                name: name.clone(),
                                constraint: None,
                                default: None,
                            }));
                        }
                    }

                    //
                    return Ok(());
                }

                info!("({}): infer: {} = {:?}", self.scope.depth(), name, arg);

                match inferred.type_params.entry(name.clone()) {
                    Entry::Occupied(mut e) => {
                        match e.get_mut() {
                            InferredType::Union(e) => return Ok(()),
                            InferredType::Other(e) => {
                                if !e.is_empty() && !opts.append_type_as_union {
                                    return Ok(());
                                }

                                // If we inferred T as `number`, we don't need to add `1`.
                                if e.iter().any(|prev| {
                                    self.assign_with_opts(
                                        &mut Default::default(),
                                        AssignOpts {
                                            span,
                                            ..Default::default()
                                        },
                                        prev,
                                        &arg,
                                    )
                                    .is_ok()
                                }) {
                                    return Ok(());
                                }

                                for prev in e.iter_mut() {
                                    if self
                                        .assign_with_opts(
                                            &mut Default::default(),
                                            AssignOpts {
                                                span,
                                                ..Default::default()
                                            },
                                            &arg,
                                            prev,
                                        )
                                        .is_ok()
                                    {
                                        *prev = arg.clone().generalize_lit(marks);
                                        return Ok(());
                                    }
                                }

                                let param_ty = Type::union(e.clone()).cheap();
                                e.push(arg.clone().generalize_lit(marks));

                                match param_ty.normalize() {
                                    Type::Param(param) => {
                                        self.insert_inferred(
                                            span,
                                            inferred,
                                            param.name.clone(),
                                            Cow::Borrowed(&arg),
                                            opts,
                                        )?;
                                    }
                                    _ => {}
                                }

                                match arg.normalize() {
                                    Type::Param(param) => {
                                        self.insert_inferred(
                                            span,
                                            inferred,
                                            param.name.clone(),
                                            Cow::Owned(param_ty),
                                            opts,
                                        )?;
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    Entry::Vacant(e) => {
                        let arg = arg.clone().generalize_lit(marks);

                        e.insert(InferredType::Other(vec![arg]));
                    }
                }

                return Ok(());
            }

            Type::Interface(param) => match arg {
                Type::Interface(..) => self.infer_type_using_interface(span, inferred, param, arg, opts)?,
                Type::TypeLit(..) | Type::Tuple(..) => {
                    return self.infer_type_using_interface(span, inferred, param, arg, opts)
                }
                _ => {}
            },

            Type::Infer(param) => {
                self.insert_inferred(span, inferred, param.type_param.name.clone(), Cow::Borrowed(&arg), opts)?;
                return Ok(());
            }

            Type::Array(arr @ Array { .. }) => {
                let opts = InferTypeOpts {
                    append_type_as_union: true,
                    ..opts
                };

                match arr.elem_type.normalize() {
                    Type::Param(TypeParam {
                        constraint: Some(constraint),
                        ..
                    }) => match constraint.normalize() {
                        Type::Operator(Operator {
                            op: TsTypeOperatorOp::KeyOf,
                            ..
                        }) => {
                            let mut arg = arg.clone();
                            self.prevent_generalize(&mut arg);
                            return self.infer_type(span, inferred, &arr.elem_type, &arg, opts);
                        }
                        _ => {}
                    },
                    _ => {}
                }

                match arg {
                    Type::Array(Array {
                        elem_type: arg_elem_type,
                        ..
                    }) => return self.infer_type(span, inferred, &arr.elem_type, &arg_elem_type, opts),

                    Type::Tuple(arg) => {
                        let arg = Type::union(arg.elems.iter().map(|element| *element.ty.clone()));
                        return self.infer_type(span, inferred, &arr.elem_type, &arg, opts);
                    }

                    _ => {}
                }
            }

            // // TODO: Check if index type extends `keyof obj_type`
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
            //                 .insert(name, Type::union(vec![prev_ty, box arg.clone()]))
            //                 .expect_none("Cannot override");
            //         }
            //         Entry::Vacant(e) => {
            //             e.insert(box arg.clone());
            //         }
            //     }
            //     return Ok(());
            // }
            Type::Function(p) => match arg {
                Type::Function(a) => {
                    self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;
                    self.infer_type(span, inferred, &p.ret_ty, &a.ret_ty, InferTypeOpts { ..opts })?;

                    if let Some(arg_type_params) = &a.type_params {
                        self.rename_inferred(inferred, arg_type_params)?;
                    }
                    return Ok(());
                }
                _ => {
                    dbg!();
                }
            },

            Type::TypeLit(param) => match arg {
                Type::TypeLit(arg) => {
                    return self.infer_type_using_type_lit_and_type_lit(span, inferred, param, arg, opts)
                }

                Type::IndexedAccessType(arg_iat) => {
                    let arg_obj_ty = self
                        .expand(
                            arg_iat.span,
                            *arg_iat.obj_type.clone(),
                            ExpandOpts {
                                full: true,
                                expand_union: true,
                                ..Default::default()
                            },
                        )?
                        .foldable();
                    match arg_obj_ty {
                        Type::Mapped(arg_obj_ty) => match &arg_obj_ty.type_param {
                            TypeParam {
                                constraint:
                                    Some(box Type::Operator(Operator {
                                        op: TsTypeOperatorOp::KeyOf,
                                        ty: box Type::Param(param_ty),
                                        ..
                                    })),
                                ..
                            } => {
                                let mut new_lit = TypeLit {
                                    span: arg_iat.span,
                                    members: vec![],
                                    metadata: Default::default(),
                                };
                                for member in &param.members {
                                    match member {
                                        TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(type_ann) = &p.type_ann {
                                                // TODO: Change p.ty

                                                self.infer_type(span, inferred, &type_ann, arg, opts)?;
                                            }

                                            new_lit.members.push(TypeElement::Property(p));
                                        }
                                        // TODO: Handle IndexSignature
                                        _ => unimplemented!(
                                            "calculating IndexAccessType for member other than property: member = {:?}",
                                            member
                                        ),
                                    }
                                }
                                self.insert_inferred(
                                    span,
                                    inferred,
                                    param_ty.name.clone(),
                                    Cow::Owned(Type::TypeLit(new_lit)),
                                    opts,
                                )?;

                                return Ok(());
                            }

                            _ => {}
                        },

                        _ => {}
                    }
                }

                Type::Interface(..) | Type::Enum(..) | Type::Alias(..) => {
                    if let Some(arg) = self.convert_type_to_type_lit(span, arg)? {
                        return self.infer_type_using_type_lit_and_type_lit(span, inferred, param, &arg, opts);
                    }
                }

                _ => {
                    dbg!();
                }
            },

            Type::Tuple(param) => match arg {
                Type::Tuple(arg) => return self.infer_type_using_tuple_and_tuple(span, inferred, param, arg, opts),
                _ => {
                    dbg!();
                }
            },

            Type::Keyword(..) => {
                match arg {
                    Type::Keyword(..) => return Ok(()),
                    _ => {}
                }

                dbg!();
            }

            Type::Predicate(..) => {
                dbg!();
            }

            Type::Ref(param) => match arg {
                Type::Ref(arg)
                    if param.type_name.eq_ignore_span(&arg.type_name)
                        && param.type_args.as_ref().map(|v| v.params.len())
                            == arg.type_args.as_ref().map(|v| v.params.len()) =>
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
                                unreachable!(
                                    "type inference: Comparison of Ref<Arg1, Arg2> and Ref<Arg1> (different length)"
                                );
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
                    // TODO: Expand children first or add expansion information to inferred.
                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ignore_expand_prevention_for_all: false,
                        ..self.ctx
                    };
                    debug!("infer_type: expanding param");
                    let param = self.with_ctx(ctx).expand(
                        span,
                        Type::Ref(param.clone()),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            ..Default::default()
                        },
                    )?;
                    match param.normalize() {
                        Type::Ref(..) => {
                            dbg!();

                            info!("Ref: {:?}", param);
                        }
                        _ => return self.infer_type(span, inferred, &param, arg, opts),
                    }
                }
            },

            Type::Lit(..) => match arg {
                Type::Lit(..) => return Ok(()),
                _ => {
                    dbg!();
                }
            },

            Type::Alias(param) => {
                self.infer_type(span, inferred, &param.ty, arg, opts)?;
                if let Some(type_params) = &param.type_params {
                    self.rename_inferred(inferred, type_params)?;
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
                match arg {
                    Type::IndexedAccessType(arg) => {
                        if param.obj_type.eq_ignore_span(&arg.obj_type) {
                            self.infer_type(span, inferred, &param.index_type, &arg.index_type, opts)?;
                            return Ok(());
                        }
                    }
                    _ => {}
                }

                match param {
                    IndexedAccessType {
                        obj_type: box Type::Param(obj_type),
                        ..
                    } if self.mapped_type_param_name.contains(&obj_type.name) => {
                        self.insert_inferred(span, inferred, obj_type.name.clone(), Cow::Borrowed(&arg), opts)?;
                        return Ok(());
                    }

                    IndexedAccessType {
                        obj_type: box Type::Intersection(Intersection { types, .. }),
                        ..
                    } if types.iter().all(|ty| match ty.normalize() {
                        Type::Param(obj_type) => {
                            let current = self.mapped_type_param_name.contains(&obj_type.name);
                            current
                        }
                        _ => false,
                    }) =>
                    {
                        for ty in types {
                            match ty.normalize() {
                                Type::Param(obj_type) => {
                                    self.insert_inferred(
                                        span,
                                        inferred,
                                        obj_type.name.clone(),
                                        Cow::Borrowed(&arg),
                                        opts,
                                    )?;
                                }

                                _ => {}
                            }
                        }
                        return Ok(());
                    }
                    _ => {}
                }
            }

            Type::Constructor(param) => match arg {
                Type::Class(arg_class) => {
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
                _ => {}
            },

            Type::Class(param) => match arg {
                Type::Class(arg) => return self.infer_types_using_class(span, inferred, param, arg, opts),
                _ => {}
            },

            Type::ClassDef(param) => match arg {
                Type::ClassDef(arg) => return self.infer_types_using_class_def(span, inferred, param, arg, opts),
                _ => {}
            },

            Type::Operator(param) => {
                self.infer_type_using_operator(span, inferred, param, arg, opts)?;

                // We need to check parents
                match arg {
                    Type::Interface(..) => {}
                    _ => return Ok(()),
                }
            }

            _ => {}
        }

        match arg {
            // Handled by generic expander, so let's return it as-is.
            Type::Mapped(..) => {}

            Type::Array(arr) => {
                let mut params = vec![];
                params.push(*arr.elem_type.clone());
                return self.infer_type(
                    span,
                    inferred,
                    param,
                    &Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new("Array".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation { span, params }),
                    }),
                    opts,
                );
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),
            Type::Keyword(..) => {}
            Type::Ref(..) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ignore_expand_prevention_for_all: false,
                    preserve_params: true,
                    ..self.ctx
                };
                let arg = self.with_ctx(ctx).expand(
                    span,
                    arg.clone(),
                    ExpandOpts {
                        full: true,
                        expand_union: true,
                        ..Default::default()
                    },
                )?;
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
                    let parent = self.type_of_ts_entity_name(
                        span,
                        self.ctx.module_id,
                        &parent.expr,
                        parent.type_args.as_deref(),
                    )?;
                    self.infer_type(span, inferred, &param, &parent, opts)?;
                }

                // Check to print unimplemented error message
                match param {
                    Type::Operator(..) | Type::Interface(..) => return Ok(()),
                    _ => {}
                }
            }
            _ => {}
        }

        match arg {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            })
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            })
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => {
                // Prevent logging
                return Ok(());
            }
            _ => {}
        }

        if param.is_str_lit() {
            // Prevent logging
            return Ok(());
        }

        if param.is_predicate() && arg.is_bool() {
            // Prevent logging
            return Ok(());
        }

        error!(
            "infer_arg_type: unimplemented\nparam  = {}\narg = {}",
            dump_type_as_string(&self.cm, param),
            dump_type_as_string(&self.cm, arg),
        );
        Ok(())
    }

    #[instrument(skip(self, span, inferred, param, arg, opts))]
    fn infer_type_using_mapped_type(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Mapped,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<bool> {
        match arg.normalize() {
            Type::Ref(arg) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    preserve_params: true,
                    ..self.ctx
                };

                let arg = self.with_ctx(ctx).expand(
                    arg.span,
                    Type::Ref(arg.clone()),
                    ExpandOpts {
                        full: true,
                        expand_union: true,
                        ..Default::default()
                    },
                )?;

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

            Type::Tuple(..)
            | Type::Enum(..)
            | Type::Alias(..)
            | Type::Intersection(..)
            | Type::Class(..)
            | Type::Interface(..) => {
                let arg = self
                    .convert_type_to_type_lit(span, arg)
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
            /// type Boxified<R> = {
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
                        Type::Operator(Operator {
                            op: TsTypeOperatorOp::KeyOf,
                            ty: operator_arg,
                            ..
                        }) => match operator_arg.normalize() {
                            Type::Param(TypeParam { name, .. }) => {
                                return Some(Res {
                                    name: name.clone(),
                                    key_name: key_name.clone(),
                                    optional: *optional,
                                    readonly: *readonly,
                                })
                            }
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
                                Type::Operator(Operator {
                                    op: TsTypeOperatorOp::KeyOf,
                                    ty,
                                    ..
                                }) => match ty.normalize() {
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
                match arg {
                    Type::TypeLit(arg) => {
                        // We should make a new type literal, based on the information.
                        let mut key_types = vec![];
                        let mut new_members = Vec::<TypeElement>::with_capacity(arg.members.len());

                        // In the code below, we are given Box<R[P]> and keys of R.
                        // We have to deduce T is { a: Box<string> } from given facts.
                        //
                        // type Boxified<R> = {
                        //     [P in keyof R]: Box<R[P]>;
                        // }
                        //
                        // declare function unboxify<T>(obj: Boxified<T>): T;
                        //
                        // declare let b: {
                        //     a: Box<string>,
                        // };
                        // let v = unboxify(b);
                        for arg_member in &arg.members {
                            if let Some(key) = arg_member.key() {
                                match key {
                                    Key::Normal { span: i_span, sym } => key_types.push(Type::Lit(RTsLitType {
                                        node_id: NodeId::invalid(),
                                        span: param.span,
                                        lit: RTsLit::Str(RStr {
                                            span: *i_span,
                                            value: sym.clone(),
                                            has_escape: false,
                                            kind: Default::default(),
                                        }),
                                    })),
                                    Key::Num(n) => {
                                        key_types.push(Type::Lit(RTsLitType {
                                            node_id: NodeId::invalid(),
                                            span: param.span,
                                            lit: RTsLit::Number(n.clone()),
                                        }));
                                    }
                                    _ => {
                                        unimplemented!("Inference of keys except ident in mapped type.\nKey: {:?}", key)
                                    }
                                }
                            }

                            match arg_member {
                                TypeElement::Property(arg_prop) => {
                                    let type_ann: Option<_> = if let Some(arg_prop_ty) = &arg_prop.type_ann {
                                        if let Some(param_ty) = &param.ty {
                                            let old = take(&mut self.mapped_type_param_name);
                                            self.mapped_type_param_name = vec![name.clone()];

                                            let mut data = InferData::default();
                                            self.infer_type(span, &mut data, &param_ty, arg_prop_ty, opts)?;
                                            let inferred_ty = data.type_params.remove(&name).map(|ty| match ty {
                                                InferredType::Union(ty) => ty,
                                                InferredType::Other(types) => Type::union(types).cheap(),
                                            });

                                            self.mapped_type_param_name = old;

                                            inferred_ty.or_else(|| data.defaults.remove(&name))
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    };
                                    let type_ann =
                                        type_ann.map(Box::new).or_else(|| Some(box Type::any(arg_prop.span)));

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
                                            let mapped_param_ty = arg_prop_ty.clone().foldable().fold_with(
                                                &mut SingleTypeParamReplacer {
                                                    name: &name,
                                                    to: param_ty,
                                                },
                                            );

                                            self.infer_type(span, inferred, &mapped_param_ty, arg_prop_ty, opts)?;
                                        }

                                        // inferred.type_elements.remove(&name)
                                        None
                                    } else {
                                        Some(box Type::any(i.span))
                                    };
                                    new_members.push(TypeElement::Index(IndexSignature { type_ann, ..i.clone() }));
                                }

                                TypeElement::Method(arg_method) => {
                                    let arg_prop_ty = Type::Function(Function {
                                        span: arg_method.span,
                                        type_params: arg_method.type_params.clone(),
                                        params: arg_method.params.clone(),
                                        ret_ty: arg_method
                                            .ret_ty
                                            .clone()
                                            .unwrap_or_else(|| box Type::any(arg_method.span)),
                                    });
                                    let type_ann = if let Some(param_ty) = param.ty.as_ref() {
                                        let old = take(&mut self.mapped_type_param_name);
                                        self.mapped_type_param_name = vec![name.clone()];

                                        let mut data = InferData::default();
                                        self.infer_type(span, &mut data, &param_ty, &arg_prop_ty, opts)?;
                                        let mut defaults = take(&mut data.defaults);
                                        let mut map = self.finalize_inference(data);
                                        let inferred_ty = map.remove(&name);

                                        self.mapped_type_param_name = old;

                                        inferred_ty.or_else(|| defaults.remove(&name))
                                    } else {
                                        None
                                    };
                                    let type_ann =
                                        type_ann.map(Box::new).or_else(|| Some(box Type::any(arg_method.span)));

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
                                    error!(
                                        "unimplemented: infer_mapped: Mapped <- Assign: TypeElement({:#?})",
                                        arg_member
                                    );
                                    return Ok(true);
                                }
                            }
                        }

                        self.insert_inferred(
                            span,
                            inferred,
                            name.clone(),
                            Cow::Owned(Type::TypeLit(TypeLit {
                                span: arg.span,
                                members: new_members,
                                metadata: arg.metadata,
                            })),
                            opts,
                        )?;

                        let mut keys = Type::Union(Union {
                            span: param.span,
                            types: key_types,
                        });
                        self.prevent_generalize(&mut keys);

                        self.insert_inferred(span, inferred, key_name.clone(), Cow::Owned(keys), opts)?;

                        return Ok(true);
                    }

                    Type::Array(arg) => {
                        let new_ty = if let Some(param_ty) = &param.ty {
                            let old = take(&mut self.mapped_type_param_name);
                            self.mapped_type_param_name = vec![name.clone()];

                            let mut data = InferData::default();
                            self.infer_type(span, &mut data, &param_ty, &arg.elem_type, opts)?;
                            let mut map = self.finalize_inference(data);
                            let mut inferred_ty = map.remove(&name);

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

                        self.insert_inferred(
                            span,
                            inferred,
                            name.clone(),
                            Cow::Owned(Type::Array(Array {
                                span: arg.span,
                                elem_type: box new_ty.unwrap_or_else(|| Type::any(arg.span)),
                            })),
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
            match &param.type_param.constraint {
                Some(constraint) => match constraint.normalize() {
                    Type::Param(type_param) => {
                        debug!(
                            "[generic/inference] Found form of `P in T` where T = {}, P = {}",
                            type_param.name, param.type_param.name
                        );

                        match arg {
                            Type::TypeLit(arg) => {
                                let key_ty = arg.members.iter().filter_map(|element| match element {
                                    TypeElement::Property(p) => match &p.key {
                                        Key::Normal {
                                            span: i_span,
                                            sym: i_sym,
                                        } => Some(Type::Lit(RTsLitType {
                                            node_id: NodeId::invalid(),
                                            span: param.span,
                                            lit: RTsLit::Str(RStr {
                                                span: *i_span,
                                                value: i_sym.clone(),
                                                has_escape: false,
                                                kind: Default::default(),
                                            }),
                                        })),
                                        _ => None,
                                    }, // TODO: Handle method element
                                    _ => None,
                                });
                                let mut key_ty = Type::union(key_ty);
                                self.prevent_generalize(&mut key_ty);
                                self.insert_inferred(
                                    span,
                                    inferred,
                                    type_param.name.clone(),
                                    Cow::Owned(key_ty),
                                    opts,
                                )?;
                            }
                            _ => {}
                        }

                        let param_ty = param.ty.as_ref().unwrap();

                        let names = {
                            let mut tp = type_param;

                            loop {
                                match &tp.constraint.as_ref().map(|v| v.normalize()) {
                                    Some(Type::Param(p)) => {
                                        tp = p;
                                    }
                                    Some(Type::Operator(Operator {
                                        op: TsTypeOperatorOp::KeyOf,
                                        ty: box Type::Param(p),
                                        ..
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
                                    if ty.types.iter().all(|ty| match ty.normalize() {
                                        Type::Operator(Operator {
                                            ty: box Type::Param(..),
                                            ..
                                        }) => true,
                                        _ => false,
                                    }) =>
                                {
                                    ty.types
                                        .iter()
                                        .map(|ty| match ty.normalize() {
                                            Type::Operator(Operator {
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
                            let revesed_param_ty = param_ty.clone().fold_with(&mut v);

                            if v.did_work {
                                self.infer_type(span, inferred, &revesed_param_ty, arg, opts)?;
                                self.mapped_type_param_name = old;

                                return Ok(true);
                            }
                        }

                        let mut type_elements = FxHashMap::<_, Vec<_>>::default();
                        match arg {
                            Type::TypeLit(arg) => {
                                //
                                if let Some(param_ty) = &param.ty {
                                    for m in &arg.members {
                                        match m {
                                            TypeElement::Property(p) => {
                                                //
                                                if let Some(ref type_ann) = p.type_ann {
                                                    self.infer_type(span, inferred, &param_ty, &type_ann, opts)?;
                                                }

                                                for name in &names {
                                                    if *name == type_param.name {
                                                        continue;
                                                    }

                                                    let ty = inferred
                                                        .type_params
                                                        .remove(name)
                                                        .map(|ty| match ty {
                                                            InferredType::Union(v) => v,
                                                            InferredType::Other(v) => Type::union(v).cheap(),
                                                        })
                                                        .map(Box::new);

                                                    type_elements.entry(name.clone()).or_default().push(
                                                        TypeElement::Property(PropertySignature {
                                                            optional: calc_true_plus_minus_in_param(
                                                                param.optional,
                                                                p.optional,
                                                            ),
                                                            readonly: calc_true_plus_minus_in_param(
                                                                param.readonly,
                                                                p.readonly,
                                                            ),
                                                            type_ann: ty,
                                                            ..p.clone()
                                                        }),
                                                    );
                                                }
                                            }

                                            _ => unimplemented!("infer_type: Mapped <- Assign: TypeElement({:?})", m),
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
                                        });

                                        self.insert_inferred(span, inferred, name.clone(), Cow::Owned(list_ty), opts)?;
                                    }
                                }

                                self.mapped_type_param_name = old;
                                return Ok(true);
                            }
                            // Handled by generic expander, so let's return it as-is.
                            _ => {}
                        }

                        self.mapped_type_param_name = old;
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        match &param.type_param.constraint {
            Some(constraint) => {
                match constraint.normalize() {
                    Type::Operator(
                        operator
                        @
                        Operator {
                            op: TsTypeOperatorOp::KeyOf,
                            ..
                        },
                    ) => match operator.ty.normalize() {
                        Type::IndexedAccessType(
                            iat
                            @
                            IndexedAccessType {
                                obj_type: box Type::Param(..),
                                index_type: box Type::Param(..),
                                ..
                            },
                        ) => match iat.obj_type.normalize() {
                            Type::Param(..) => match iat.index_type.normalize() {
                                Type::Param(..) => {
                                    let param_ty = param.ty.clone().unwrap();
                                    let name = param.type_param.name.clone();
                                    let (obj_ty, index_ty) = match &**param.type_param.constraint.as_ref().unwrap() {
                                        Type::Operator(Operator {
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
                                        match arg {
                                            Type::TypeLit(arg) => {
                                                let mut members = Vec::with_capacity(arg.members.len());

                                                for m in &arg.members {
                                                    match m {
                                                        TypeElement::Property(p) => {
                                                            let optional = calc_true_plus_minus_in_param(
                                                                param.optional,
                                                                p.optional,
                                                            );
                                                            //
                                                            if let Some(ref type_ann) = p.type_ann {
                                                                self.infer_type(
                                                                    span, inferred, &param_ty, &type_ann, opts,
                                                                )?;
                                                            }
                                                            members.push(TypeElement::Property(PropertySignature {
                                                                optional,
                                                                readonly: calc_true_plus_minus_in_param(
                                                                    param.readonly,
                                                                    p.readonly,
                                                                ),
                                                                type_ann: None,
                                                                ..p.clone()
                                                            }));
                                                        }

                                                        _ => unimplemented!(
                                                            "infer_type: Mapped <- Assign: TypeElement({:?})",
                                                            m
                                                        ),
                                                    }
                                                }

                                                let list_ty = Type::TypeLit(TypeLit {
                                                    span: arg.span,
                                                    members,
                                                    metadata: arg.metadata,
                                                });

                                                self.insert_inferred(
                                                    span,
                                                    inferred,
                                                    name.clone(),
                                                    Cow::Owned(list_ty),
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
                                _ => {}
                            },

                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                }
            }
            None => {}
        }

        {
            match &param.type_param.constraint {
                Some(constraint) => match constraint.normalize() {
                    Type::Operator(
                        operator
                        @
                        Operator {
                            op: TsTypeOperatorOp::KeyOf,
                            ty: box Type::Mapped(Mapped { ty: Some(..), .. }),
                            ..
                        },
                    ) => match operator.ty.normalize() {
                        Type::Mapped(..) => {
                            let revesed_param_ty = param
                                .ty
                                .as_ref()
                                .unwrap()
                                .clone()
                                .fold_with(&mut MappedReverser::default());

                            self.infer_type(span, inferred, &revesed_param_ty, arg, opts)?;

                            return Ok(true);
                        }
                        _ => {}
                    },
                    _ => {}
                },
                None => {}
            }
        }

        {
            // We handle all other maped types at here.
            //
            //
            // In the code below,
            //
            // declare type Boxified<Pick<P, T>> = {
            //     [BoxifiedP in keyof Pick<P, K>[BoxifiedT]]: Box<Pick<P, K>[BoxifiedP]>;
            // };
            match &param.type_param.constraint {
                Some(constraint) => match constraint.normalize() {
                    Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty,
                        ..
                    }) => match &param.ty {
                        Some(param_ty) => match arg {
                            Type::TypeLit(arg_lit) => {
                                let revesed_param_ty = param_ty.clone().fold_with(&mut MappedReverser::default());
                                print_type(&"reversed", &self.cm, &revesed_param_ty);

                                self.infer_type(span, inferred, &revesed_param_ty, arg, opts)?;

                                return Ok(true);
                            }

                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            }
        }

        print_backtrace();

        Ok(false)
    }

    fn infer_type_using_tuple_and_tuple(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Tuple,
        arg: &Tuple,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        for item in param
            .elems
            .iter()
            .map(|element| &element.ty)
            .zip_longest(arg.elems.iter().map(|element| &element.ty))
        {
            match item {
                EitherOrBoth::Both(param, arg) => self.infer_type(span, inferred, param, arg, opts)?,
                EitherOrBoth::Left(_) => {}
                EitherOrBoth::Right(_) => {}
            }
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
    ) -> ValidationResult<()> {
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
    ) -> ValidationResult<()> {
        for (param, arg) in params.iter().zip(args) {
            self.infer_type_of_fn_param(span, inferred, param, arg, opts)?
        }

        if params.len() > args.len() {
            for param in &params[args.len()..] {
                match &*param.ty {
                    Type::Param(param) => {
                        // TOOD: Union
                        inferred.defaults.insert(param.name.clone(), Type::unknown(param.span));
                    }
                    _ => {
                        // TOOD: Complex inference logic for types like (b:
                        // Boxifiied<T>) => T
                    }
                }
            }
        }

        Ok(())
    }

    fn rename_inferred(&mut self, inferred: &mut InferData, arg_type_params: &TypeParamDecl) -> ValidationResult<()> {
        struct Renamer<'a> {
            fixed: &'a FxHashMap<Id, Type>,
        }

        impl VisitMut<Type> for Renamer<'_> {
            fn visit_mut(&mut self, node: &mut Type) {
                match node.normalize() {
                    Type::Param(p) if self.fixed.contains_key(&p.name) => {
                        *node = (*self.fixed.get(&p.name).unwrap()).clone();
                    }
                    _ => node.visit_mut_children_with(self),
                }
            }
        }

        //
        let mut fixed = FxHashMap::default();

        inferred.type_params.iter().for_each(|(param_name, ty)| {
            // Ignore unrelated type parameters
            if arg_type_params.params.iter().all(|v| *param_name != v.name) {
                return;
            }
            let ty = match ty.clone() {
                InferredType::Union(v) => v,
                InferredType::Other(types) => Type::union(types).cheap(),
            };
            fixed.insert(param_name.clone(), ty);
        });

        let mut v = Renamer { fixed: &fixed };
        inferred.type_params.iter_mut().for_each(|(_, ty)| match ty {
            InferredType::Union(ty) => {
                ty.visit_mut_with(&mut v);
            }
            InferredType::Other(ty) => {
                ty.visit_mut_with(&mut v);
            }
        });

        Ok(())
    }
}

/// Handles renaming of the type parameters.
impl Analyzer<'_, '_> {
    pub(super) fn rename_type_params(&mut self, span: Span, mut ty: Type, type_ann: Option<&Type>) -> ValidationResult {
        if self.is_builtin {
            return Ok(ty);
        }
        debug!(
            "rename_type_params(has_ann = {:?}, ty = {})",
            type_ann.is_some(),
            dump_type_as_string(&self.cm, &ty)
        );

        if ty.normalize().is_intersection_type() {
            return Ok(ty);
        }

        // ty = self.expand(span, ty)?;

        let mut usage_visitor = TypeParamUsageFinder::default();
        ty.normalize().visit_with(&mut usage_visitor);
        if usage_visitor.params.is_empty() {
            debug!("rename_type_param: No type parameter is used in type");

            if matches!(ty.normalize(), Type::Function(..)) {
                match ty.normalize_mut() {
                    Type::Function(ref mut f) => {
                        f.type_params = None;
                    }

                    _ => {}
                }
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

            let map = self.finalize_inference(inferred);

            return Ok(ty
                .foldable()
                .fold_with(&mut TypeParamRenamer {
                    inferred: map,
                    declared: Default::default(),
                })
                .fixed());
        }

        let decl = Some(TypeParamDecl {
            span: DUMMY_SP,
            params: usage_visitor.params,
        });

        match ty.normalize_mut() {
            Type::Function(ref mut f) => {
                f.type_params = decl;
            }

            Type::ClassDef(..) | Type::Class(..) => {
                return Ok(ty);
            }

            _ => {}
        }

        Ok(ty.fold_with(&mut TypeParamRemover::new()).fixed())
    }
}

/// This method returns true for types like `'foo'` and `'foo' | 'bar'`.
pub(super) fn is_literals(ty: &Type) -> bool {
    match ty.normalize() {
        Type::Lit(_) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|v| is_literals(v)),
        _ => false,
    }
}

struct SingleTypeParamReplacer<'a> {
    name: &'a Id,
    to: &'a Type,
}

impl Fold<Type> for SingleTypeParamReplacer<'_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match &ty {
            Type::Param(TypeParam { name, .. }) if *self.name == *name => return (*self.to).clone(),

            _ => {}
        }

        ty
    }
}

struct TypeParamInliner<'a> {
    param: &'a Id,
    value: &'a RStr,
}

impl VisitMut<Type> for TypeParamInliner<'_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match ty {
            Type::Param(p) if p.name == *self.param => {
                *ty = Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    span: p.span,
                    lit: RTsLit::Str(self.value.clone()),
                });
                return;
            }
            _ => {}
        }
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

pub(crate) fn calc_true_plus_minus_in_arg(v: Option<TruePlusMinus>, previous: bool) -> bool {
    match v {
        Some(v) => match v {
            TruePlusMinus::True => true,
            TruePlusMinus::Plus => true,
            TruePlusMinus::Minus => false,
        },
        None => previous,
    }
}

/// Replaceds type parameters with name `from` to type `to`.
struct MappedKeyReplacer<'a> {
    /// The name of type parameter
    from: &'a Id,
    /// Type of key. This is typically literal.
    to: &'a Type,
}

impl VisitMut<Type> for MappedKeyReplacer<'_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        match ty {
            Type::Param(param) if *self.from == param.name => {
                *ty = self.to.clone();
            }
            _ => ty.visit_mut_children_with(self),
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
        ty.visit_mut_children_with(self);

        match &*ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type, index_type, ..
            }) => {
                if self.obj_ty.type_eq(&**obj_type) {
                    match &**index_type {
                        Type::Param(key) if *self.index_param_name == key.name => {
                            *ty = self.to.clone();
                            return;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
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
        ty = ty.fold_children_with(self);

        match ty {
            Type::TypeLit(TypeLit {
                span,
                members,
                metadata,
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
                            members: vec![TypeElement::Property(PropertySignature {
                                type_ann: mapped.ty,
                                ..p
                            })],
                            metadata,
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
        ty = ty.fold_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type,
                index_type:
                    box Type::Param(TypeParam {
                        name: index_name,
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty: indexed_ty,
                                ..
                            })),
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
        TruePlusMinus::True => match element_ty.normalize_mut() {
            Type::Optional(ty) => {
                let ty = ty.ty.take();
                let ty = ty.remove_falsy();

                *element_ty = ty;
            }
            _ => {
                let new_ty = element_ty.take().remove_falsy();

                *element_ty = new_ty;
            }
        },
        TruePlusMinus::Plus => match element_ty.normalize() {
            Type::Optional(ty) => {}
            _ => {
                let ty = box element_ty.take();
                *element_ty = Type::Optional(OptionalType { span: DUMMY_SP, ty })
            }
        },
        TruePlusMinus::Minus => {}
    }
}
