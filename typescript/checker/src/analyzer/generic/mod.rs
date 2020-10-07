use self::remover::TypeParamRemover;
use super::{util::Comparator, Analyzer, Ctx};
use crate::{
    debug::{print_backtrace, print_type},
    ty::{
        self, Array, FnParam, IndexSignature, IndexedAccessType, Mapped, Operator,
        PropertySignature, Tuple, Type, TypeElement, TypeLit, TypeOrSpread, TypeParam,
        TypeParamDecl, TypeParamInstantiation, Union,
    },
    util::{EqIgnoreSpan, TypeEq},
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::{FxHashMap, FxHashSet};
use itertools::{EitherOrBoth, Itertools};
use std::{collections::hash_map::Entry, mem::take};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_types::{Fold, FoldWith, Id, Intersection, VisitMut, VisitMutWith, VisitWith};

mod expander;
mod remover;

/// Lower value means higher priority and it contains lower value if the
/// type parameter and the type argument are simpler.
///
///
///
/// e.g.
/// - `param = TypeParam, arg = Keyword` => 1
type Priority = u16;

#[derive(Debug, Default)]
pub(super) struct InferData {
    /// Inferred type parameters
    type_params: FxHashMap<Id, Box<Type>>,

    priorities: FxHashMap<Id, Priority>,

    /// It's incremented before calling `infer_type`
    cur_priority: Priority,
}

impl Analyzer<'_, '_> {
    fn insert_inferred(
        &mut self,
        inferred: &mut InferData,
        name: Id,
        ty: Box<Type>,
    ) -> ValidationResult<()> {
        if let Some(previous) = inferred.type_params.get(&name) {
            if let Some(prev_priority) = inferred.priorities.get(&name) {
                if inferred.cur_priority >= *prev_priority {
                    // We cannot overfide in this case

                    if ty.type_eq(previous) {
                        return Ok(());
                    }

                    // TODO: Change this to error
                    print_backtrace();
                    panic!(
                        "A type parameter (`{}`) has multiple (or same type with multi usage) \
                         type. This is a type error, but swc currently does not handle \
                         it.\nPrevious: {:?}\nNew: {:?}",
                        name, previous, ty
                    );
                }
            }
        }

        log::info!("Inferred {} as {:?}", name, ty);

        inferred.type_params.insert(name.clone(), ty);
        inferred.priorities.insert(name, inferred.cur_priority);

        Ok(())
    }
}

/// Type inference for arguments.
impl Analyzer<'_, '_> {
    /// Create [TypeParamInstantiation] from inferred type information.
    pub(super) fn instantiate(
        &mut self,
        span: Span,
        type_params: &[TypeParam],
        mut inferred: FxHashMap<Id, Box<Type>>,
    ) -> ValidationResult<TypeParamInstantiation> {
        let mut params = Vec::with_capacity(type_params.len());
        for type_param in type_params {
            if let Some(ty) = inferred.remove(&type_param.name) {
                log::info!("infer_arg_type: {}", type_param.name);
                params.push(ty);
            } else {
                match type_param.constraint {
                    Some(box Type::Param(ref p)) => {
                        // TODO: Handle complex inheritance like
                        //      function foo<A extends B, B extends C>(){ }

                        if let Some(actual) = inferred.remove(&p.name) {
                            log::info!(
                                "infer_arg_type: {} => {} => {:?} because of the extends clause",
                                type_param.name,
                                p.name,
                                actual
                            );
                            params.push(actual);
                        } else {
                            log::info!(
                                "infer_arg_type: {} => {} because of the extends clause",
                                type_param.name,
                                p.name
                            );
                            params.push(box Type::Param(p.clone()));
                        }
                        continue;
                    }
                    _ => {}
                }

                if type_param.constraint.is_some()
                    && is_literals(&type_param.constraint.as_ref().unwrap())
                {
                    params.push(type_param.constraint.clone().unwrap());
                    continue;
                }

                if type_param.constraint.is_some()
                    && match **type_param.constraint.as_ref().unwrap() {
                        Type::Interface(..)
                        | Type::Keyword(..)
                        | Type::Ref(..)
                        | Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    let ty =
                        self.expand_fully(span, type_param.constraint.clone().unwrap(), false)?;
                    params.push(ty);
                    continue;
                }

                log::warn!(
                    "instantiate: A type parameter {} defaults to {{}}",
                    type_param.name
                );

                // Defaults to {}
                params.push(box Type::TypeLit(TypeLit {
                    span,
                    members: vec![],
                }));
            }
        }

        Ok(TypeParamInstantiation {
            span: DUMMY_SP,
            params,
        })
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
        default_ty: &Type,
    ) -> ValidationResult<FxHashMap<Id, Box<Type>>> {
        log::warn!(
            "infer_arg_types: {:?}",
            type_params
                .iter()
                .map(|p| format!("{}, ", p.name))
                .collect::<String>()
        );

        let mut inferred = InferData::default();

        if let Some(base) = base {
            for (param, type_param) in base.params.iter().zip(type_params) {
                log::info!(
                    "User provided `{:?} = {:?}`",
                    type_param.name,
                    param.clone()
                );
                inferred
                    .type_params
                    .insert(type_param.name.clone(), param.clone());
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
                        _ => unimplemented!(
                            "spread argument typed other than tuple.\nType: {:#?}",
                            arg.ty
                        ),
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
                Pat::Rest(_) => true,
                _ => false,
            };

            if !is_rest {
                if let Some(arg) = args.get(idx) {
                    self.infer_type(&mut inferred, &p.ty, &arg.ty)?;
                }
            } else {
                // Handle varargs
                for arg in &args[idx..] {
                    self.infer_type(&mut inferred, &p.ty, &arg.ty)?;
                }
            }
        }

        for type_param in type_params {
            if inferred.type_params.contains_key(&type_param.name) {
                continue;
            }

            match type_param.constraint {
                Some(box Type::Param(ref p)) => {
                    // TODO: Handle complex inheritance like
                    //      function foo<A extends B, B extends C>(){ }

                    if let Some(actual) = inferred.type_params.remove(&p.name) {
                        log::info!(
                            "infer_arg_type: {} => {} => {:?} because of the extends clause",
                            type_param.name,
                            p.name,
                            actual
                        );
                        inferred.type_params.insert(p.name.clone(), actual.clone());
                        inferred.type_params.insert(type_param.name.clone(), actual);
                    } else {
                        log::info!(
                            "infer_arg_type: {} => {} because of the extends clause",
                            type_param.name,
                            p.name
                        );
                        self.insert_inferred(
                            &mut inferred,
                            type_param.name.clone(),
                            box Type::Param(p.clone()),
                        )?;
                    }
                    continue;
                }
                _ => {}
            }

            if type_param.constraint.is_some()
                && is_literals(&type_param.constraint.as_ref().unwrap())
            {
                self.insert_inferred(
                    &mut inferred,
                    type_param.name.clone(),
                    type_param.constraint.clone().unwrap(),
                )?;
                continue;
            }

            if type_param.constraint.is_some()
                && match **type_param.constraint.as_ref().unwrap() {
                    Type::Interface(..) | Type::Keyword(..) | Type::Ref(..) | Type::TypeLit(..) => {
                        true
                    }
                    _ => false,
                }
            {
                let ty = self.expand_fully(span, type_param.constraint.clone().unwrap(), false)?;
                self.insert_inferred(&mut inferred, type_param.name.clone(), ty)?;
                continue;
            }

            log::error!(
                "infer: A type parameter {} defaults to {:?}",
                type_param.name,
                default_ty
            );

            // Defaults to {}
            self.insert_inferred(
                &mut inferred,
                type_param.name.clone(),
                box default_ty.clone(),
            )?;
        }

        Ok(inferred.type_params)
    }

    /// Infer types, so that `param` has same type as `arg`.
    ///
    ///
    /// TODO: Optimize
    fn infer_type(
        &mut self,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
    ) -> ValidationResult<()> {
        if self.is_builtin {
            return Ok(());
        }

        print_type("param", &self.cm, &param);
        print_type("arg", &self.cm, &arg);

        let param = param.normalize();
        let arg = arg.normalize();

        let p;
        let param = match param {
            Type::Mapped(..) => {
                p = box param
                    .clone()
                    .into_owned()
                    .fold_with(&mut MappedIndexedSimplifier);
                &p
            }
            _ => param,
        };

        match arg {
            Type::Union(arg) => {
                //
                for a in &arg.types {
                    self.infer_type(inferred, param, a)?;
                }

                return Ok(());
            }

            _ => {}
        }

        let p = param;
        let a = arg;

        match param {
            Type::Param(TypeParam {
                ref name,
                ref constraint,
                ..
            }) => {
                log::trace!("infer_type: type parameter: {} = {:?}", name, constraint);

                if constraint.is_some() && is_literals(&constraint.as_ref().unwrap()) {
                    log::info!("infer from literal constraint: {} = {:?}", name, constraint);
                    if let Some(orig) = inferred.type_params.get(&name) {
                        if !orig.eq_ignore_span(&constraint.as_ref().unwrap()) {
                            print_backtrace();
                            panic!(
                                "Cannot override T in `T extends <literal>`\nOrig: \
                                 {:?}\nConstraints: {:?}",
                                orig, constraint
                            )
                        }
                    }

                    return Ok(());
                }

                let mut arg = box arg.clone();
                if arg.is_any() && self.is_implicitly_typed(&arg) {
                    if inferred.type_params.contains_key(&name.clone()) {
                        return Ok(());
                    }
                    arg = Type::unknown(arg.span());
                }

                log::info!("({}): infer: {} = {:?}", self.scope.depth(), name, arg);
                match inferred.type_params.entry(name.clone()) {
                    Entry::Occupied(e) => {
                        // Use this for type inference.
                        let (name, param_ty) = e.remove_entry();

                        inferred
                            .type_params
                            .insert(name, Type::union(vec![param_ty.clone(), arg.clone()]));

                        match &*param_ty {
                            Type::Param(param) => {
                                self.insert_inferred(inferred, param.name.clone(), arg.clone())?;
                            }
                            _ => {}
                        }

                        match &*arg {
                            Type::Param(param) => {
                                self.insert_inferred(inferred, param.name.clone(), param_ty)?;
                            }
                            _ => {}
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(arg);
                    }
                }

                return Ok(());
            }

            Type::Array(
                arr
                @
                Array {
                    elem_type:
                        box Type::Param(TypeParam {
                            constraint:
                                Some(box Type::Operator(Operator {
                                    op: TsTypeOperatorOp::KeyOf,
                                    ..
                                })),
                            ..
                        }),
                    ..
                },
            ) => {
                let mut arg = arg.clone();
                self.prevent_generalize(&mut arg);
                return self.infer_type(inferred, &arr.elem_type, &arg);
            }

            Type::Array(Array { elem_type, .. }) => match arg {
                Type::Array(Array {
                    elem_type: arg_elem_type,
                    ..
                }) => return self.infer_type(inferred, &elem_type, &arg_elem_type),

                Type::Tuple(arg) => {
                    let arg = Type::union(arg.elems.iter().map(|element| &element.ty).cloned());
                    return self.infer_type(inferred, &elem_type, &arg);
                }

                _ => {}
            },

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
                    self.infer_type_of_fn_params(inferred, &p.params, &a.params)?;
                    self.infer_type(inferred, &p.ret_ty, &a.ret_ty)?;

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
                Type::TypeLit(arg) => return self.infer_type_lit(inferred, param, arg),

                Type::IndexedAccessType(arg_iat) => {
                    let arg_obj_ty =
                        self.expand_fully(arg_iat.span, arg_iat.obj_type.clone(), true)?;
                    match *arg_obj_ty {
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
                                };
                                for member in &param.members {
                                    match member {
                                        TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(type_ann) = &p.type_ann {
                                                // TODO: Change p.ty

                                                self.infer_type(inferred, &type_ann, arg)?;
                                            }

                                            new_lit.members.push(TypeElement::Property(p));
                                        }
                                        // TODO: Handle IndexSignature
                                        _ => unimplemented!(
                                            "calculating IndexAccessType for member other than \
                                             property: member = {:?}",
                                            member
                                        ),
                                    }
                                }
                                self.insert_inferred(
                                    inferred,
                                    param_ty.name.clone(),
                                    box Type::TypeLit(new_lit),
                                )?;

                                return Ok(());
                            }

                            _ => {}
                        },

                        _ => {}
                    }
                }
                _ => {
                    dbg!();
                }
            },

            Type::Tuple(param) => match arg {
                Type::Tuple(arg) => return self.infer_tuple(inferred, param, arg),
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
                Type::Ref(arg) if param.type_name.eq_ignore_span(&arg.type_name) => {
                    if param.type_args.is_none() && arg.type_args.is_none() {
                        return Ok(());
                    }
                    if param.type_args.is_none() || arg.type_args.is_none() {
                        unimplemented!(
                            "Comparing `Ref<T>` (with type args) and `Ref` (without type args)"
                        );
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
                                self.infer_type(inferred, param, arg)?;
                            }
                            _ => {
                                unimplemented!(
                                    "type inference: Comparison of Ref<Arg1, Arg2> and Ref<Arg1> \
                                     (different length)"
                                );
                            }
                        }
                    }
                    return Ok(());
                }
                Type::Param(arg) => match &param.type_name {
                    TsEntityName::TsQualifiedName(_) => {}
                    TsEntityName::Ident(param) => {
                        inferred
                            .type_params
                            .insert(param.clone().into(), box Type::Param(arg.clone()));
                        return Ok(());
                    }
                },
                _ => {
                    // TODO: Expand children first or add expansion information to inferred.
                    let ctx = Ctx {
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ignore_expand_prevention_for_all: true,
                        ..self.ctx
                    };
                    let param = self.with_ctx(ctx).expand_fully(
                        param.span(),
                        box Type::Ref(param.clone()),
                        true,
                    )?;
                    match *param {
                        Type::Ref(..) => {
                            dbg!();

                            log::info!("Ref: {:?}", param);
                        }
                        _ => return self.infer_type(inferred, &param, arg),
                    }
                }
            },

            Type::Lit(..) => match arg {
                Type::Lit(..) => return Ok(()),
                _ => {
                    dbg!();
                }
            },

            // TODO: implement
            Type::Union(param) => {
                //
                for p in &param.types {
                    self.infer_type(inferred, p, arg)?;
                }

                return Ok(());
            }

            Type::Alias(param) => {
                self.infer_type(inferred, &param.ty, arg)?;
                if let Some(type_params) = &param.type_params {
                    self.rename_inferred(inferred, type_params)?;
                }
                return Ok(());
            }

            Type::Mapped(param) => {
                if self.infer_mapped(inferred, param, arg)? {
                    dbg!();
                    return Ok(());
                }
            }

            Type::IndexedAccessType(param) => {
                match arg {
                    Type::IndexedAccessType(arg) => {
                        if param.obj_type.eq_ignore_span(&arg.obj_type) {
                            self.infer_type(inferred, &param.index_type, &arg.index_type)?;
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
                        self.insert_inferred(inferred, obj_type.name.clone(), box arg.clone())?;
                        return Ok(());
                    }

                    IndexedAccessType {
                        obj_type: box Type::Intersection(Intersection { types, .. }),
                        ..
                    } if types.iter().all(|ty| match &**ty {
                        Type::Param(obj_type) => {
                            let current = self.mapped_type_param_name.contains(&obj_type.name);
                            current
                        }
                        _ => false,
                    }) =>
                    {
                        for ty in types {
                            match &**ty {
                                Type::Param(obj_type) => {
                                    self.insert_inferred(
                                        inferred,
                                        obj_type.name.clone(),
                                        box arg.clone(),
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

            Type::Intersection(param) => {
                if param.types.len() == 1 {
                    return self.infer_type(inferred, &param.types[0], arg);
                }
            }

            _ => {}
        }

        match arg {
            // Handled by generic expander, so let's return it as-is.
            Type::Mapped(..) => {}
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),
            Type::Keyword(..) => {}
            Type::Ref(..) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };
                let arg = self
                    .with_ctx(ctx)
                    .expand_fully(arg.span(), box arg.clone(), true)?;
                match *arg {
                    Type::Ref(..) => {}
                    _ => {
                        return self.infer_type(inferred, param, &arg);
                    }
                }
            }
            Type::Alias(arg) => return self.infer_type(inferred, param, &arg.ty),
            _ => {}
        }

        log::error!(
            "infer_arg_type: unimplemented\nparam  = {:#?}\narg = {:#?}",
            param,
            arg,
        );
        Ok(())
    }

    fn infer_mapped(
        &mut self,
        inferred: &mut InferData,
        param: &Mapped,
        arg: &Type,
    ) -> ValidationResult<bool> {
        match arg.normalize() {
            Type::Ref(arg) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };

                let arg =
                    self.with_ctx(ctx)
                        .expand_fully(arg.span, box Type::Ref(arg.clone()), true)?;

                match arg.normalize() {
                    Type::Ref(..) => return Ok(false),
                    _ => return self.infer_mapped(inferred, param, &arg),
                }
            }
            Type::Mapped(arg) => {
                if param.type_param.name == arg.type_param.name {
                    if let Some(param_ty) = &param.ty {
                        if let Some(arg_ty) = &arg.ty {
                            self.infer_type(inferred, param_ty, arg_ty)?;
                        }
                    }

                    return Ok(true);
                }
            }
            _ => {}
        }

        match param {
            // type Boxified<R> = {
            //     [P in keyof R]: Box<R[P]>;
            // }
            Mapped {
                type_param:
                    TypeParam {
                        name: key_name,
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty: box Type::Param(TypeParam { name, .. }),
                                ..
                            })),
                        ..
                    },
                readonly,
                optional,
                ..
            }
            | Mapped {
                type_param:
                    TypeParam {
                        constraint:
                            Some(box Type::Param(TypeParam {
                                name: key_name,
                                constraint:
                                    Some(box Type::Operator(Operator {
                                        op: TsTypeOperatorOp::KeyOf,
                                        ty: box Type::Param(TypeParam { name, .. }),
                                        ..
                                    })),
                                ..
                            })),
                        ..
                    },
                optional,
                readonly,
                ..
            } => {
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
                                    Expr::Ident(i) => key_types.push(box Type::Lit(TsLitType {
                                        span: param.span,
                                        lit: TsLit::Str(Str {
                                            span: i.span,
                                            value: i.sym.clone(),
                                            has_escape: false,
                                        }),
                                    })),
                                    _ => unimplemented!(
                                        "Inference of keys except ident in mapped type.\nKey: {:?}",
                                        key
                                    ),
                                }
                            }

                            match arg_member {
                                TypeElement::Property(arg_prop) => {
                                    let type_ann: Option<_> =
                                        if let Some(arg_prop_ty) = &arg_prop.type_ann {
                                            if let Some(param_ty) = &param.ty {
                                                let old = take(&mut self.mapped_type_param_name);
                                                self.mapped_type_param_name = vec![name.clone()];

                                                let mut data = InferData::default();
                                                self.infer_type(&mut data, &param_ty, arg_prop_ty)?;
                                                let inferred_ty = data.type_params.remove(&name);

                                                self.mapped_type_param_name = old;

                                                if let Some(inferred_ty) = inferred_ty {
                                                    Some(inferred_ty)
                                                } else {
                                                    Some(arg_prop_ty.clone())
                                                }
                                            } else {
                                                Some(arg_prop_ty.clone())
                                            }
                                        } else {
                                            None
                                        };
                                    let type_ann = type_ann.or_else(|| arg_prop.type_ann.clone());

                                    new_members.push(TypeElement::Property(PropertySignature {
                                        optional: calc_true_plus_minus_in_param(
                                            *optional,
                                            arg_prop.optional,
                                        ),
                                        readonly: calc_true_plus_minus_in_param(
                                            *readonly,
                                            arg_prop.readonly,
                                        ),
                                        type_ann,
                                        ..arg_prop.clone()
                                    }));
                                }
                                TypeElement::Index(i) => {
                                    let type_ann = if let Some(arg_prop_ty) = &i.type_ann {
                                        if let Some(param_ty) = &param.ty {
                                            let mapped_param_ty = arg_prop_ty
                                                .clone()
                                                .into_owned()
                                                .fold_with(&mut TypeParamReplacer {
                                                    name: &name,
                                                    to: param_ty,
                                                });

                                            self.infer_type(
                                                inferred,
                                                &mapped_param_ty,
                                                arg_prop_ty,
                                            )?;
                                        }

                                        // inferred.type_elements.remove(&name)
                                        None
                                    } else {
                                        Some(Type::any(i.span))
                                    };
                                    new_members.push(TypeElement::Index(IndexSignature {
                                        type_ann,
                                        ..i.clone()
                                    }));
                                }

                                _ => {
                                    log::error!(
                                        "not implemented yet: infer_mapped: Mapped <- Assign: \
                                         TypeElement({:#?})",
                                        arg_member
                                    );
                                    return Ok(true);
                                }
                            }
                        }

                        self.insert_inferred(
                            inferred,
                            name.clone(),
                            box Type::TypeLit(TypeLit {
                                span: arg.span,
                                members: new_members,
                            }),
                        )?;

                        let mut keys = box Type::Union(Union {
                            span: param.span,
                            types: key_types,
                        });
                        self.prevent_generalize(&mut keys);

                        self.insert_inferred(inferred, key_name.clone(), keys)?;

                        return Ok(true);
                    }
                    _ => {}
                }
            }

            Mapped {
                type_param:
                    TypeParam {
                        constraint: Some(box Type::Param(type_param)),
                        ..
                    },
                optional,
                readonly,
                ..
            } => {
                let mut type_param = type_param;
                loop {
                    match &type_param.constraint {
                        Some(box Type::Param(p)) => {
                            type_param = p;
                        }
                        Some(box Type::Operator(Operator {
                            ty: box Type::Param(p),
                            ..
                        })) => {
                            type_param = p;
                        }
                        _ => {
                            break;
                        }
                    }
                }

                let names = match &type_param.constraint {
                    Some(box Type::Union(ty))
                        if ty.types.iter().all(|ty| match &**ty {
                            Type::Operator(Operator {
                                ty: box Type::Param(..),
                                ..
                            }) => true,
                            _ => false,
                        }) =>
                    {
                        ty.types
                            .iter()
                            .map(|ty| match &**ty {
                                Type::Operator(Operator {
                                    ty: box Type::Param(p),
                                    ..
                                }) => p.name.clone(),
                                _ => unreachable!(),
                            })
                            .collect()
                    }
                    _ => vec![type_param.name.clone()],
                };

                let old = take(&mut self.mapped_type_param_name);
                self.mapped_type_param_name = names.clone();
                //

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
                                            self.infer_type(inferred, &param_ty, &type_ann)?;
                                        }

                                        for name in &names {
                                            let ty = inferred.type_params.remove(name);

                                            type_elements.entry(name.clone()).or_default().push(
                                                TypeElement::Property(PropertySignature {
                                                    optional: calc_true_plus_minus_in_param(
                                                        *optional, p.optional,
                                                    ),
                                                    readonly: calc_true_plus_minus_in_param(
                                                        *readonly, p.readonly,
                                                    ),
                                                    type_ann: ty,
                                                    ..p.clone()
                                                }),
                                            );
                                        }
                                    }

                                    _ => unimplemented!(
                                        "infer_type: Mapped <- Assign: TypeElement({:?})",
                                        m
                                    ),
                                }
                            }

                            for name in names {
                                let list_ty = Type::TypeLit(TypeLit {
                                    span: arg.span,
                                    members: type_elements.remove(&name).unwrap_or_default(),
                                });

                                self.insert_inferred(inferred, name.clone(), box list_ty)?;
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

            Mapped {
                type_param:
                    TypeParam {
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty:
                                    box Type::IndexedAccessType(IndexedAccessType {
                                        obj_type: box Type::Param(..),
                                        index_type: box Type::Param(..),
                                        ..
                                    }),
                                ..
                            })),
                        ..
                    },
                ty: Some(..),
                optional,
                readonly,
                ..
            } => {
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
                                        //
                                        if let Some(ref type_ann) = p.type_ann {
                                            self.infer_type(inferred, &param_ty, &type_ann)?;
                                        }
                                        members.push(TypeElement::Property(PropertySignature {
                                            optional: calc_true_plus_minus_in_param(
                                                *optional, p.optional,
                                            ),
                                            readonly: calc_true_plus_minus_in_param(
                                                *readonly, p.readonly,
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
                            });

                            self.insert_inferred(inferred, name.clone(), box list_ty)?;
                            return Ok(true);
                        }

                        _ => {}
                    }
                }
            }

            Mapped {
                type_param:
                    TypeParam {
                        name: key_name,
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty: box Type::Mapped(Mapped { ty: Some(..), .. }),
                                ..
                            })),
                        ..
                    },
                ty: Some(param_ty),
                optional,
                readonly,
                ..
            } => {
                let revesed_param_ty = param_ty.clone().fold_with(&mut MappedReverser);

                self.infer_type(inferred, &revesed_param_ty, arg)?;

                return Ok(true);
            }

            // We handle all other maped types at here.
            //
            //
            // In the code below,
            //
            // declare type Boxified<Pick<P, T>> = {
            //     [BoxifiedP in keyof Pick<P, K>[BoxifiedT]]: Box<Pick<P, K>[BoxifiedP]>;
            // };
            Mapped {
                type_param:
                    TypeParam {
                        name: key_name,
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty,
                                ..
                            })),
                        ..
                    },
                ty: Some(param_ty),
                optional,
                readonly,
                ..
            } => match arg {
                Type::TypeLit(arg_lit) => {
                    let revesed_param_ty = param_ty.clone().fold_with(&mut MappedReverser);
                    print_type("reversed", &self.cm, &revesed_param_ty);

                    self.infer_type(inferred, &revesed_param_ty, arg)?;

                    return Ok(true);

                    // let mut members =
                    // Vec::with_capacity(arg_lit.members.len());

                    // for m in &arg_lit.members {
                    //     match m {
                    //         TypeElement::Property(p) => {
                    //             let key_type = if p.computed {
                    //                 p.key.clone().validate_with(self)?
                    //             } else {
                    //                 match &*p.key {
                    //                     Expr::Ident(i) => box
                    // Type::Lit(TsLitType {
                    // span: p.key.span(),
                    // lit: TsLit::Str(Str {
                    // span: i.span,
                    // value: i.sym.clone(),
                    // has_escape: false,
                    // }),                     }),
                    //                     _ => unreachable!(),
                    //                 }
                    //             };

                    //             let mut type_ann = ty.clone();

                    //             print_type("param_ty", &self.cm, &type_ann);
                    //             print_type("ty", &self.cm, ty);
                    //             print_type("arg", &self.cm, arg);

                    //             print_type("param_ty (reversed)", &self.cm,
                    // &revesed_param_ty);

                    //             {
                    //                 let mut data = InferData::default();
                    //                 self.infer_type(&mut data, ty, arg)?;
                    //                 for (id, ty) in data.type_params {
                    //                     print_type(&format!("ty-arg: {}",
                    // id), &self.cm, &ty);
                    // }             }

                    //             print_type("Prop: After ty-arg", &self.cm,
                    // &type_ann);             type_ann =
                    // type_ann.fold_with(&mut Simplifier);
                    //             print_type("Prop: After ty-arg, simplified",
                    // &self.cm, &type_ann);

                    //             {
                    //                 let mut data = InferData::default();
                    //                 self.infer_type(&mut data,
                    // &revesed_param_ty, arg)?;
                    // for (id, ty) in data.type_params {
                    //                     print_type(&format!("ty-param: {}",
                    // id), &self.cm, &ty);
                    // type_ann.visit_mut_with(&mut MappedKeyReplacer {
                    //                         from: &id,
                    //                         to: &ty,
                    //                     });
                    //                 }
                    //             }

                    //             print_type(
                    //                 "Prop: After ty-param, simpilified",
                    //                 &self.cm,
                    //                 &type_ann,
                    //             );
                    //             type_ann = type_ann.fold_with(&mut
                    // Simplifier);
                    // print_type("Prop: After ty-param", &self.cm, &type_ann);

                    //             type_ann.visit_mut_with(&mut
                    // MappedKeyReplacer {
                    // from: key_name,                 to:
                    // &key_type,             });

                    //             print_type("Prop type afeter replacing key",
                    // &self.cm, &type_ann);

                    //             type_ann = type_ann.fold_with(&mut
                    // Simplifier);

                    //             print_type("Prop type", &self.cm, &type_ann);

                    //             //

                    //             let type_ann = if let Some(ref prop_ty) =
                    // p.type_ann {
                    // Some(prop_ty.clone())             }
                    // else {                 None
                    //             };
                    //
                    // members.push(TypeElement::Property(PropertySignature {
                    //                 optional:
                    // calc_true_plus_minus_in_param(*optional, p.optional),
                    //                 readonly:
                    // calc_true_plus_minus_in_param(*readonly, p.readonly),
                    //                 type_ann,
                    //                 ..p.clone()
                    //             }));
                    //         }

                    //         _ => {
                    //             unimplemented!("infer_type: Mapped <- Assign:
                    // TypeElement({:?})", m)         }
                    //     }
                    // }

                    // let list_ty = Type::TypeLit(TypeLit {
                    //     span: arg_lit.span,
                    //     members,
                    // });

                    // print_type("Built literal", &self.cm, &list_ty);

                    // self.infer_type(inferred, &revesed_param_ty, &list_ty)?;

                    // // self.insert_inferred(inferred, key_name.clone(), box
                    // list_ty)?; return Ok(true);
                }

                _ => {}
            },

            _ => {}
        }

        print_backtrace();

        Ok(false)
    }

    /// Compare fields.
    fn infer_type_lit(
        &mut self,
        inferred: &mut InferData,
        param: &TypeLit,
        arg: &TypeLit,
    ) -> ValidationResult<()> {
        for p in &param.members {
            for a in &arg.members {
                //

                match p {
                    TypeElement::Property(p) => match a {
                        TypeElement::Property(a) => {
                            if p.key.type_eq(&a.key) {
                                if let Some(pt) = &p.type_ann {
                                    if let Some(at) = &a.type_ann {
                                        self.infer_type(inferred, pt, at)?;
                                    } else {
                                        dbg!((&p, &a));
                                    }
                                } else {
                                    dbg!((&p, &a));
                                }
                            }
                        }
                        _ => {}
                    },
                    TypeElement::Index(param) => match a {
                        TypeElement::Property(arg) => {
                            if param.params.len() != 1 {
                                unimplemented!(
                                    "handling of IndexSignature with zero / multiple parameters"
                                );
                            }

                            if let Some(p_type_ann) = &param.type_ann {
                                if let Some(a_type_ann) = &arg.type_ann {
                                    self.infer_type(inferred, p_type_ann, a_type_ann)?;
                                }
                            }
                        }
                        TypeElement::Index(arg) => {
                            if param.params.type_eq(&arg.params) {
                                if let Some(pt) = &param.type_ann {
                                    if let Some(at) = &arg.type_ann {
                                        self.infer_type(inferred, pt, at)?;
                                    }
                                } else {
                                    dbg!((&param, &arg));
                                }
                            } else {
                                dbg!((&param, &arg));
                            }
                        }
                        _ => {}
                    },
                    TypeElement::Method(..) => {
                        // TODO
                    }
                    TypeElement::Constructor(..) => {
                        // TODO
                    }
                    _ => unimplemented!("TypeElement({:#?}) in type literal", p),
                }
            }
        }

        Ok(())
    }

    fn infer_tuple(
        &mut self,
        inferred: &mut InferData,
        param: &Tuple,
        arg: &Tuple,
    ) -> ValidationResult<()> {
        for item in param
            .elems
            .iter()
            .map(|element| &element.ty)
            .zip_longest(arg.elems.iter().map(|element| &element.ty))
        {
            match item {
                EitherOrBoth::Both(param, arg) => self.infer_type(inferred, param, arg)?,
                EitherOrBoth::Left(_) => {}
                EitherOrBoth::Right(_) => {}
            }
        }

        Ok(())
    }

    fn infer_type_of_fn_param(
        &mut self,
        inferred: &mut InferData,
        param: &FnParam,
        arg: &FnParam,
    ) -> ValidationResult<()> {
        self.infer_type(inferred, &param.ty, &arg.ty)
    }

    fn infer_type_of_fn_params(
        &mut self,
        inferred: &mut InferData,
        params: &[FnParam],
        args: &[FnParam],
    ) -> ValidationResult<()> {
        for (param, arg) in params.iter().zip(args) {
            self.infer_type_of_fn_param(inferred, param, arg)?
        }

        Ok(())
    }

    fn rename_inferred(
        &mut self,
        inferred: &mut InferData,
        arg_type_params: &TypeParamDecl,
    ) -> ValidationResult<()> {
        struct Renamer<'a> {
            fixed: &'a FxHashMap<Id, Box<Type>>,
        }

        impl ty::VisitMut for Renamer<'_> {
            fn visit_mut_type(&mut self, node: &mut Type) {
                match node {
                    Type::Param(p) if self.fixed.contains_key(&p.name) => {
                        *node = (**self.fixed.get(&p.name).unwrap()).clone();
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
            fixed.insert(param_name.clone(), ty.clone());
        });

        let mut v = Renamer { fixed: &fixed };
        inferred.type_params.iter_mut().for_each(|(_, ty)| {
            ty.visit_mut_with(&mut v);
        });

        Ok(())
    }
}

/// Handles renaming of the type parameters.
impl Analyzer<'_, '_> {
    pub(super) fn rename_type_params(
        &mut self,
        span: Span,
        mut ty: Box<Type>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        if self.is_builtin {
            return Ok(ty);
        }
        log::debug!(
            "rename_type_params(has_ann = {:?}, ty = {:?})",
            type_ann.is_some(),
            ty
        );

        // ty = self.expand(span, ty)?;

        let mut usage_visitor = TypeParamUsageFinder::default();
        ty.normalize().visit_with(&ty, &mut usage_visitor);
        if usage_visitor.params.is_empty() {
            log::debug!("rename_type_param: No type parameter is used in type");
            match *ty {
                Type::Function(ref mut f) => {
                    f.type_params = None;
                }

                _ => {}
            }

            return Ok(ty);
        }

        let mut inferred = InferData::default();

        if let Some(type_ann) = type_ann {
            self.infer_type(&mut inferred, &ty, type_ann)?;
            log::info!(
                "renaming type parameters based on type annotation provided by user\ntype_ann = \
                 {:?}",
                type_ann
            );
            return Ok(box ty.into_owned().fold_with(&mut TypeParamRenamer {
                inferred: inferred.type_params,
            }));
        }

        let decl = Some(TypeParamDecl {
            span: DUMMY_SP,
            params: usage_visitor.params,
        });

        match *ty {
            Type::Function(ref mut f) => {
                f.type_params = decl;
            }

            _ => {}
        }

        Ok(ty.fold_with(&mut TypeParamRemover::new()))
    }
}

#[derive(Debug)]
struct TypeParamRenamer {
    inferred: FxHashMap<Id, Box<Type>>,
}

impl Fold for TypeParamRenamer {
    fn fold_type(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::Param(ref param) => {
                if let Some(ty) = self.inferred.get(&param.name) {
                    return *ty.clone();
                }
            }
            _ => {}
        }

        ty
    }
}

#[derive(Debug, Default)]
struct TypeParamUsageFinder {
    params: Vec<TypeParam>,
}

impl ty::Visit for TypeParamUsageFinder {
    #[inline]
    fn visit_type_param_decl(&mut self, _: &TypeParamDecl, _: &dyn ty::TypeNode) {}

    fn visit_type_param(&mut self, node: &TypeParam, _: &dyn ty::TypeNode) {
        for p in &self.params {
            if node.name == p.name {
                return;
            }
        }

        log::info!("Found type parameter({})", node.name);

        self.params.push(node.clone());
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

struct TypeParamReplacer<'a> {
    name: &'a Id,
    to: &'a Type,
}

impl Fold for TypeParamReplacer<'_> {
    fn fold_type(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match &ty {
            Type::Param(TypeParam { name, .. }) if *self.name == *name => {
                return (*self.to).clone()
            }

            _ => {}
        }

        ty
    }
}

struct TypeParamInliner<'a> {
    param: &'a Id,
    value: &'a Str,
}

impl VisitMut for TypeParamInliner<'_> {
    fn visit_mut_type(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match ty {
            Type::Param(p) if p.name == *self.param => {
                *ty = Type::Lit(TsLitType {
                    span: p.span,
                    lit: TsLit::Str(self.value.clone()),
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

struct MappedKeyReplacer<'a> {
    /// The name of type parameter
    from: &'a Id,
    /// Type of key. This is typically literal.
    to: &'a Type,
}

impl VisitMut for MappedKeyReplacer<'_> {
    fn visit_mut_type(&mut self, ty: &mut Type) {
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

impl VisitMut for MappedIndexTypeReplacer<'_> {
    fn visit_mut_type(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        match &*ty {
            Type::IndexedAccessType(IndexedAccessType {
                obj_type,
                index_type,
                ..
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
struct MappedReverser;

impl Fold for MappedReverser {
    fn fold_type(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::TypeLit(TypeLit { span, members })
                if members.len() == 1
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
                        });

                        return Type::Mapped(Mapped {
                            ty: Some(ty),
                            ..mapped
                        });
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

impl Fold for MappedIndexedSimplifier {
    fn fold_type(&mut self, mut ty: Type) -> Type {
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
