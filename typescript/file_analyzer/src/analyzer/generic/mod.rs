use self::remover::TypeParamRemover;
use super::Analyzer;
use super::Ctx;
use crate::util::RemoveTypes;
use crate::ValidationResult;
use fxhash::FxHashMap;
use itertools::EitherOrBoth;
use itertools::Itertools;
use rnode::Fold;
use rnode::FoldWith;
use rnode::NodeId;
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::debug::print_type;
use stc_ts_types::Array;
use stc_ts_types::FnParam;
use stc_ts_types::Id;
use stc_ts_types::IndexSignature;
use stc_ts_types::IndexedAccessType;
use stc_ts_types::Intersection;
use stc_ts_types::Key;
use stc_ts_types::Mapped;
use stc_ts_types::Operator;
use stc_ts_types::OptionalType;
use stc_ts_types::PropertySignature;
use stc_ts_types::RestType;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeOrSpread;
use stc_ts_types::TypeParam;
use stc_ts_types::TypeParamDecl;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::Union;
use stc_ts_utils::MapWithMut;
use std::collections::hash_map::Entry;
use std::mem::take;
use swc_common::EqIgnoreSpan;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;

mod expander;
mod inference;
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
    defaults: FxHashMap<Id, Box<Type>>,
}

/// TODO: Move to inference.rs
impl Analyzer<'_, '_> {
    fn insert_inferred(&mut self, inferred: &mut InferData, name: Id, ty: Box<Type>) -> ValidationResult<()> {
        slog::info!(self.logger, "Inferred {} as {:?}", name, ty);

        match ty.normalize() {
            Type::Param(ty) => {
                if name == ty.name {
                    return Ok(());
                }
            }
            _ => {}
        }

        match inferred.type_params.entry(name.clone()) {
            Entry::Occupied(e) => {
                // Use this for type inference.
                let (name, param_ty) = e.remove_entry();

                inferred
                    .type_params
                    .insert(name, Type::union(vec![param_ty.clone(), ty]).cheap());
            }
            Entry::Vacant(e) => {
                e.insert(ty.cheap());
            }
        }

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
                slog::info!(self.logger, "infer_arg_type: {}", type_param.name);
                params.push(ty);
            } else {
                match type_param.constraint {
                    Some(box Type::Param(ref p)) => {
                        // TODO: Handle complex inheritance like
                        //      function foo<A extends B, B extends C>(){ }

                        if let Some(actual) = inferred.remove(&p.name) {
                            slog::info!(
                                self.logger,
                                "infer_arg_type: {} => {} => {:?} because of the extends clause",
                                type_param.name,
                                p.name,
                                actual
                            );
                            params.push(actual);
                        } else {
                            slog::info!(
                                self.logger,
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

                if type_param.constraint.is_some() && is_literals(&type_param.constraint.as_ref().unwrap()) {
                    params.push(type_param.constraint.clone().unwrap());
                    continue;
                }

                if type_param.constraint.is_some()
                    && match **type_param.constraint.as_ref().unwrap() {
                        Type::Interface(..) | Type::Keyword(..) | Type::Ref(..) | Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    let ty = self.expand_fully(span, type_param.constraint.clone().unwrap(), false)?;
                    params.push(ty);
                    continue;
                }

                slog::warn!(
                    self.logger,
                    "instantiate: A type parameter {} defaults to {{}}",
                    type_param.name
                );

                // Defaults to {}
                params.push(box Type::TypeLit(TypeLit { span, members: vec![] }));
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
    ) -> ValidationResult<FxHashMap<Id, Box<Type>>> {
        slog::warn!(
            self.logger,
            "infer_arg_types: {:?}",
            type_params.iter().map(|p| format!("{}, ", p.name)).collect::<String>()
        );

        let mut inferred = InferData::default();

        if let Some(base) = base {
            for (param, type_param) in base.params.iter().zip(type_params) {
                slog::info!(
                    self.logger,
                    "User provided `{:?} = {:?}`",
                    type_param.name,
                    param.clone()
                );
                inferred.type_params.insert(type_param.name.clone(), param.clone());
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
                        _ => unimplemented!("spread argument typed other than tuple.\nType: {:#?}", arg.ty),
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
                    self.infer_type(span, &mut inferred, &p.ty, &arg.ty)?;
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
                        )?;
                    }
                    Type::Array(p_ty) => {
                        // Handle varargs. This result in union of all types.
                        for arg in &args[idx..] {
                            self.infer_type(span, &mut inferred, &p_ty.elem_type, &arg.ty)?;
                        }
                    }
                    _ => {
                        // Handle varargs
                        for arg in &args[idx..] {
                            self.infer_type(span, &mut inferred, &p.ty, &arg.ty)?;
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
                        slog::info!(
                            self.logger,
                            "infer_arg_type: {} => {} => {:?} because of the extends clause",
                            type_param.name,
                            p.name,
                            actual
                        );
                        inferred.type_params.insert(p.name.clone(), actual.clone());
                        inferred.type_params.insert(type_param.name.clone(), actual);
                    } else {
                        slog::info!(
                            self.logger,
                            "infer_arg_type: {} => {} because of the extends clause",
                            type_param.name,
                            p.name
                        );
                        self.insert_inferred(&mut inferred, type_param.name.clone(), box Type::Param(p.clone()))?;
                    }
                    continue;
                }
                _ => {}
            }

            if type_param.constraint.is_some() && is_literals(&type_param.constraint.as_ref().unwrap()) {
                self.insert_inferred(
                    &mut inferred,
                    type_param.name.clone(),
                    type_param.constraint.clone().unwrap(),
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
                let ty = self
                    .with_ctx(ctx)
                    .expand_fully(span, type_param.constraint.clone().unwrap(), false)?;
                if !inferred.type_params.contains_key(&type_param.name) {
                    self.insert_inferred(&mut inferred, type_param.name.clone(), ty)?;
                }
                continue;
            }
            if !inferred.type_params.contains_key(&type_param.name) {
                if let Some(default_ty) = inferred.defaults.remove(&type_param.name) {
                    self.insert_inferred(&mut inferred, type_param.name.clone(), default_ty)?;
                } else {
                    if let Some(default) = &type_param.default {
                        self.insert_inferred(&mut inferred, type_param.name.clone(), default.clone())?;
                        continue;
                    }

                    if let Some(default_ty) = default_ty {
                        slog::error!(
                            self.logger,
                            "infer: A type parameter {} defaults to {:?}",
                            type_param.name,
                            default_ty
                        );

                        self.insert_inferred(&mut inferred, type_param.name.clone(), box default_ty.clone())?;
                    }
                }
            }
        }

        slog::warn!(self.logger, "infer_arg_types is finished");

        Ok(inferred.type_params)
    }

    /// Handles `infer U`.
    pub(super) fn infer_ts_infer_types(
        &mut self,
        span: Span,
        base: &Type,
        concrete: &Type,
    ) -> ValidationResult<FxHashMap<Id, Box<Type>>> {
        let mut inferred = InferData::default();
        self.infer_type(span, &mut inferred, base, concrete)?;
        Ok(inferred.type_params)
    }

    /// Infer types, so that `param` has same type as `arg`.
    ///
    ///
    /// TODO: Optimize
    fn infer_type(&mut self, span: Span, inferred: &mut InferData, param: &Type, arg: &Type) -> ValidationResult<()> {
        if self.is_builtin {
            return Ok(());
        }

        print_type(&self.logger, "param", &self.cm, &param);
        print_type(&self.logger, "arg", &self.cm, &arg);

        let param = param.normalize();
        let arg = arg.normalize();

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

        match arg {
            Type::Union(arg) => {
                //
                for a in &arg.types {
                    self.infer_type(span, inferred, param, a)?;
                }

                return Ok(());
            }

            _ => {}
        }

        let p = param;
        let a = arg;

        self.infer_builtin(span, inferred, param, arg)?;

        match param {
            Type::Param(TypeParam {
                ref name,
                ref constraint,
                ..
            }) => {
                slog::trace!(self.logger, "infer_type: type parameter: {} = {:?}", name, constraint);

                if constraint.is_some() && is_literals(&constraint.as_ref().unwrap()) {
                    slog::info!(
                        self.logger,
                        "infer from literal constraint: {} = {:?}",
                        name,
                        constraint
                    );
                    if let Some(orig) = inferred.type_params.get(&name) {
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

                match arg {
                    Type::Param(arg) => {
                        if *name == arg.name {
                            return Ok(());
                        }
                    }
                    _ => {}
                }

                let mut arg = box arg.clone();
                if arg.is_any() && self.is_implicitly_typed(&arg) {
                    if inferred.type_params.contains_key(&name.clone()) {
                        return Ok(());
                    }
                    arg = Type::unknown(arg.span());
                }

                slog::info!(self.logger, "({}): infer: {} = {:?}", self.scope.depth(), name, arg);

                match inferred.type_params.entry(name.clone()) {
                    Entry::Occupied(e) => {
                        // Use this for type inference.
                        let (name, param_ty) = e.remove_entry();

                        inferred
                            .type_params
                            .insert(name, Type::union(vec![param_ty.clone(), arg.clone()]));

                        match param_ty.normalize() {
                            Type::Param(param) => {
                                self.insert_inferred(inferred, param.name.clone(), arg.clone())?;
                            }
                            _ => {}
                        }

                        match arg.normalize() {
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

            Type::Interface(param) => match arg {
                Type::Interface(..) => self.infer_type_using_interface(span, inferred, param, arg)?,
                Type::TypeLit(..) => return self.infer_type_using_interface(span, inferred, param, arg),
                _ => {}
            },

            Type::Infer(param) => {
                self.insert_inferred(inferred, param.type_param.name.clone(), box arg.clone())?;
                return Ok(());
            }

            Type::Array(arr @ Array { .. }) => {
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
                            return self.infer_type(span, inferred, &arr.elem_type, &arg);
                        }
                        _ => {}
                    },
                    _ => {}
                }

                match arg {
                    Type::Array(Array {
                        elem_type: arg_elem_type,
                        ..
                    }) => return self.infer_type(span, inferred, &arr.elem_type, &arg_elem_type),

                    Type::Tuple(arg) => {
                        let arg = Type::union(arg.elems.iter().map(|element| &element.ty).cloned());
                        return self.infer_type(span, inferred, &arr.elem_type, &arg);
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
                    self.infer_type_of_fn_params(span, inferred, &p.params, &a.params)?;
                    self.infer_type(span, inferred, &p.ret_ty, &a.ret_ty)?;

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
                Type::TypeLit(arg) => return self.infer_type_using_type_lit_and_type_lit(span, inferred, param, arg),

                Type::IndexedAccessType(arg_iat) => {
                    let arg_obj_ty = self
                        .expand_fully(arg_iat.span, arg_iat.obj_type.clone(), true)?
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
                                };
                                for member in &param.members {
                                    match member {
                                        TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(type_ann) = &p.type_ann {
                                                // TODO: Change p.ty

                                                self.infer_type(span, inferred, &type_ann, arg)?;
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
                                self.insert_inferred(inferred, param_ty.name.clone(), box Type::TypeLit(new_lit))?;

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
                Type::Tuple(arg) => return self.infer_tuple(span, inferred, param, arg),
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
                                self.infer_type(span, inferred, param, arg)?;
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
                    slog::debug!(self.logger, "infer_type: expanding param");
                    let param = self
                        .with_ctx(ctx)
                        .expand_fully(span, box Type::Ref(param.clone()), true)?;
                    match *param {
                        Type::Ref(..) => {
                            dbg!();

                            slog::info!(self.logger, "Ref: {:?}", param);
                        }
                        _ => return self.infer_type(span, inferred, &param, arg),
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
                    self.infer_type(span, inferred, p, arg)?;
                }

                return Ok(());
            }

            Type::Alias(param) => {
                self.infer_type(span, inferred, &param.ty, arg)?;
                if let Some(type_params) = &param.type_params {
                    self.rename_inferred(inferred, type_params)?;
                }
                return Ok(());
            }

            Type::Mapped(param) => {
                if self.infer_mapped(span, inferred, param, arg)? {
                    dbg!();
                    return Ok(());
                }
            }

            Type::IndexedAccessType(param) => {
                match arg {
                    Type::IndexedAccessType(arg) => {
                        if param.obj_type.eq_ignore_span(&arg.obj_type) {
                            self.infer_type(span, inferred, &param.index_type, &arg.index_type)?;
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
                                    self.insert_inferred(inferred, obj_type.name.clone(), box arg.clone())?;
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
                    return self.infer_type(span, inferred, &param.types[0], arg);
                }
            }

            Type::Constructor(param) => match arg {
                Type::Class(arg_class) => {
                    for member in &arg_class.body {
                        match member {
                            stc_ts_types::ClassMember::Constructor(constructor) => {
                                self.infer_type_of_fn_params(span, inferred, &param.params, &constructor.params)?;

                                if let Some(ret_ty) = &constructor.ret_ty {
                                    return self.infer_type(span, inferred, &param.type_ann, ret_ty);
                                }
                            }
                            stc_ts_types::ClassMember::Method(_) => {}
                            stc_ts_types::ClassMember::Property(_) => {}
                            stc_ts_types::ClassMember::IndexSignature(_) => {}
                        }
                    }

                    return self.infer_type(span, inferred, &param.type_ann, arg);
                }
                _ => {}
            },

            Type::Class(param) => match arg {
                Type::Class(arg) => return self.infer_class(span, inferred, param, arg),
                _ => {}
            },

            Type::Operator(param) => {
                self.infer_type_from_operator(span, inferred, param, arg)?;

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
                    ..self.ctx
                };
                let arg = self.with_ctx(ctx).expand_fully(span, box arg.clone(), true)?;
                match *arg {
                    Type::Ref(..) => {}
                    _ => {
                        return self.infer_type(span, inferred, param, &arg);
                    }
                }
            }
            Type::Alias(arg) => return self.infer_type(span, inferred, param, &arg.ty),

            Type::Interface(arg) => {
                // Body should be handled by the match expression above.

                for parent in &arg.extends {
                    let parent = self.type_of_ts_entity_name(
                        span,
                        self.ctx.module_id,
                        &parent.expr,
                        parent.type_args.as_deref(),
                    )?;
                    self.infer_type(span, inferred, &param, &parent)?;
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

        slog::error!(
            self.logger,
            "infer_arg_type: unimplemented\nparam  = {}\narg = {}",
            dump_type_as_string(&self.cm, param),
            dump_type_as_string(&self.cm, arg),
        );
        Ok(())
    }
    fn infer_mapped(
        &mut self,
        span: Span,
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

                let arg = self
                    .with_ctx(ctx)
                    .expand_fully(arg.span, box Type::Ref(arg.clone()), true)?;

                match arg.normalize() {
                    Type::Ref(..) => return Ok(false),
                    _ => return self.infer_mapped(span, inferred, param, &arg),
                }
            }
            Type::Mapped(arg) => {
                if param.type_param.name == arg.type_param.name {
                    if let Some(param_ty) = &param.ty {
                        if let Some(arg_ty) = &arg.ty {
                            self.infer_type(span, inferred, param_ty, arg_ty)?;
                        }
                    }

                    return Ok(true);
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
                                    Key::Normal { span: i_span, sym } => key_types.push(box Type::Lit(RTsLitType {
                                        node_id: NodeId::invalid(),
                                        span: param.span,
                                        lit: RTsLit::Str(RStr {
                                            span: *i_span,
                                            value: sym.clone(),
                                            has_escape: false,
                                            kind: Default::default(),
                                        }),
                                    })),
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
                                            self.infer_type(span, &mut data, &param_ty, arg_prop_ty)?;
                                            let inferred_ty = data.type_params.remove(&name);

                                            self.mapped_type_param_name = old;

                                            inferred_ty.or_else(|| data.defaults.remove(&name))
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    };
                                    let type_ann = type_ann.or_else(|| Some(Type::any(arg_prop.span)));

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
                                            let mapped_param_ty =
                                                arg_prop_ty.clone().foldable().fold_with(&mut TypeParamReplacer {
                                                    name: &name,
                                                    to: param_ty,
                                                });

                                            self.infer_type(span, inferred, &mapped_param_ty, arg_prop_ty)?;
                                        }

                                        // inferred.type_elements.remove(&name)
                                        None
                                    } else {
                                        Some(Type::any(i.span))
                                    };
                                    new_members.push(TypeElement::Index(IndexSignature { type_ann, ..i.clone() }));
                                }

                                _ => {
                                    slog::error!(
                                        self.logger,
                                        "not implemented yet: infer_mapped: Mapped <- Assign: TypeElement({:#?})",
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

                    Type::Array(arg) => {
                        let new_ty = if let Some(param_ty) = &param.ty {
                            let old = take(&mut self.mapped_type_param_name);
                            self.mapped_type_param_name = vec![name.clone()];

                            let mut data = InferData::default();
                            self.infer_type(span, &mut data, &param_ty, &arg.elem_type)?;
                            let mut inferred_ty = data.type_params.remove(&name);

                            self.mapped_type_param_name = old;

                            match &mut inferred_ty {
                                Some(ty) => {
                                    handle_optional_for_element(&mut **ty, optional);
                                }
                                None => {}
                            }

                            inferred_ty
                        } else {
                            None
                        };

                        self.insert_inferred(
                            inferred,
                            name.clone(),
                            box Type::Array(Array {
                                span: arg.span,
                                elem_type: new_ty.unwrap_or_else(|| Type::any(arg.span)),
                            }),
                        )?;

                        return Ok(true);
                    }

                    // TODO: Handle array
                    Type::Tuple(arg) => {
                        let mut new_elements = vec![];

                        for element in &arg.elems {
                            let new_ty = if let Some(param_ty) = &param.ty {
                                let old = take(&mut self.mapped_type_param_name);
                                self.mapped_type_param_name = vec![name.clone()];

                                let mut data = InferData::default();
                                let rest = match &*element.ty {
                                    Type::Rest(RestType {
                                        span: rest_span,
                                        ty: box Type::Array(arr),
                                    }) => {
                                        self.infer_type(span, &mut data, &param_ty, &arr.elem_type)?;
                                        Some(*rest_span)
                                    }
                                    _ => {
                                        self.infer_type(span, &mut data, &param_ty, &element.ty)?;
                                        None
                                    }
                                };
                                let mut inferred_ty = data.type_params.remove(&name);

                                match &mut inferred_ty {
                                    Some(ty) => {
                                        handle_optional_for_element(&mut **ty, optional);
                                    }
                                    None => {}
                                }

                                self.mapped_type_param_name = old;

                                if let Some(rest) = rest {
                                    if let Some(elem_type) = inferred_ty {
                                        Some(box Type::Rest(RestType {
                                            span: rest,
                                            ty: box Type::Array(Array {
                                                // TODO
                                                span: DUMMY_SP,
                                                elem_type,
                                            }),
                                        }))
                                    } else {
                                        None
                                    }
                                } else {
                                    inferred_ty
                                }
                            } else {
                                None
                            };

                            new_elements.push(TupleElement {
                                ty: new_ty.unwrap_or_else(|| Type::any(arg.span)),
                                ..element.clone()
                            });
                        }

                        self.insert_inferred(
                            inferred,
                            name.clone(),
                            box Type::Tuple(Tuple {
                                span: arg.span,
                                elems: new_elements,
                            }),
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
                        match arg {
                            Type::TypeLit(arg) => {
                                let key_ty = arg.members.iter().filter_map(|element| match element {
                                    TypeElement::Property(p) => match &p.key {
                                        Key::Normal {
                                            span: i_span,
                                            sym: i_sym,
                                        } => Some(box Type::Lit(RTsLitType {
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
                                self.insert_inferred(inferred, type_param.name.clone(), key_ty)?;
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
                                self.infer_type(span, inferred, &revesed_param_ty, arg)?;
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
                                                    self.infer_type(span, inferred, &param_ty, &type_ann)?;
                                                }

                                                for (id, ty) in &inferred.type_params {
                                                    print_type(&self.logger, &format!("{}", id), &self.cm, &ty);
                                                }

                                                for name in &names {
                                                    if *name == type_param.name {
                                                        continue;
                                                    }

                                                    let ty = inferred.type_params.remove(name);

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
                                                                self.infer_type(span, inferred, &param_ty, &type_ann)?;
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
                                                });

                                                self.insert_inferred(inferred, name.clone(), box list_ty)?;
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

                            self.infer_type(span, inferred, &revesed_param_ty, arg)?;

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
                                print_type(&self.logger, "reversed", &self.cm, &revesed_param_ty);

                                self.infer_type(span, inferred, &revesed_param_ty, arg)?;

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

    fn infer_tuple(
        &mut self,
        span: Span,
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
                EitherOrBoth::Both(param, arg) => self.infer_type(span, inferred, param, arg)?,
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
    ) -> ValidationResult<()> {
        self.infer_type(span, inferred, &param.ty, &arg.ty)
    }

    fn infer_type_of_fn_params(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        params: &[FnParam],
        args: &[FnParam],
    ) -> ValidationResult<()> {
        for (param, arg) in params.iter().zip(args) {
            self.infer_type_of_fn_param(span, inferred, param, arg)?
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
            fixed: &'a FxHashMap<Id, Box<Type>>,
        }

        impl VisitMut<Type> for Renamer<'_> {
            fn visit_mut(&mut self, node: &mut Type) {
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
        slog::debug!(
            self.logger,
            "rename_type_params(has_ann = {:?}, ty = {:?})",
            type_ann.is_some(),
            ty
        );

        // ty = self.expand(span, ty)?;

        let mut usage_visitor = TypeParamUsageFinder::default();
        ty.normalize().visit_with(&mut usage_visitor);
        if usage_visitor.params.is_empty() {
            slog::debug!(self.logger, "rename_type_param: No type parameter is used in type");
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
            self.infer_type(span, &mut inferred, &ty, type_ann)?;
            slog::info!(
                self.logger,
                "renaming type parameters based on type annotation provided by user\ntype_ann = {:?}",
                type_ann
            );
            return Ok(box ty.foldable().fold_with(&mut TypeParamRenamer {
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

impl Fold<Type> for TypeParamRenamer {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::Param(ref param) => {
                if let Some(mapped) = self.inferred.get(&param.name) {
                    match mapped.normalize() {
                        Type::Param(..) => return ty,
                        _ => {}
                    }
                    return *mapped.clone();
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

/// Noop as declaration is not usage.
impl Visit<TypeParamDecl> for TypeParamUsageFinder {
    #[inline]
    fn visit(&mut self, _: &TypeParamDecl) {}
}

impl Visit<TypeParam> for TypeParamUsageFinder {
    fn visit(&mut self, node: &TypeParam) {
        for p in &self.params {
            if node.name == p.name {
                return;
            }
        }

        // slog::info!(self.logger, "Found type parameter({})", node.name);

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

impl Fold<Type> for TypeParamReplacer<'_> {
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
