use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
};

use fxhash::FxHashMap;
use itertools::Itertools;
use stc_ts_ast_rnode::{RTsEntityName, RTsLit};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_generics::expander::InferTypeResult;
use stc_ts_type_form::{compare_type_forms, max_path, TypeForm};
use stc_ts_type_ops::generalization::prevent_generalize;
use stc_ts_types::{
    Array, ArrayMetadata, Class, ClassDef, ClassMember, Function, Id, Interface, KeywordType,
    KeywordTypeMetadata, LitType, Operator, Ref, Type, TypeElement, TypeLit, TypeParam,
    TypeParamMetadata, Union,
};
use swc_common::{Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::{TsKeywordTypeKind, TsTypeOperatorOp};
use tracing::{error, info};

use crate::{
    analyzer::{
        assign::AssignOpts,
        generic::{type_form::OldTypeForm, InferData, InferredType},
        Analyzer,
    },
    ty::TypeExt,
    util::unwrap_ref_with_single_arg,
    ValidationResult,
};

/// # Default
///
/// All fields default to `false`.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct InferTypeOpts {
    pub for_fn_assignment: bool,
    /// Defaults to false because
    ///
    /// ```ts
    /// function foo<T>(x: T, y: T) {
    ///     return x;
    /// }
    ///
    /// foo(1, '')
    /// ```
    ///
    /// the code above is error.
    ///
    ///
    /// This is `true` for array
    pub append_type_as_union: bool,

    pub skip_union: bool,
}

impl Analyzer<'_, '_> {
    /// Union-union inference is special, because
    ///
    /// `T | PromiseLike<T>` <= `void | PromiseLike<void>`
    ///
    /// should result in `T = void`, not `T = void | PromiseLike<void>`
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn infer_type_using_union_and_union(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Union,
        arg_ty: &Type,
        arg: &Union,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        // Check 'form's of type.
        //
        // `Promise<T> | T` and `Promise<void> | void` have same 'form'.
        if param.types.len() == arg.types.len() {
            // TODO(kdy1): Sort types so `T | PromiseLike<T>` has same form as
            // `PromiseLike<void> | void`.

            let param_type_form = param.types.iter().map(OldTypeForm::from).collect_vec();
            let arg_type_form = arg.types.iter().map(OldTypeForm::from).collect_vec();

            if param_type_form == arg_type_form {
                for (p, a) in param.types.iter().zip(arg.types.iter()) {
                    self.infer_type(span, inferred, p, a, opts)?;
                }

                return Ok(());
            }
        }

        for p in &param.types {
            self.infer_type(span, inferred, p, arg_ty, opts)?;
        }

        Ok(())
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn infer_type_using_union(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Union,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        let type_forms = param.types.iter().map(TypeForm::from).collect_vec();
        let arg_form = TypeForm::from(arg);

        let matched_paths = type_forms
            .iter()
            .map(|param| compare_type_forms(&param, &arg_form))
            .collect_vec();
        let max = matched_paths.iter().max_by(|a, b| max_path(a, b));

        if let Some(max) = max {
            for (idx, (param, type_path)) in
                param.types.iter().zip(matched_paths.iter()).enumerate()
            {
                if type_path == max {
                    self.infer_type(span, inferred, &param, arg, opts)?;
                }
            }

            return Ok(());
        }

        //
        if !opts.skip_union {
            for p in &param.types {
                self.infer_type(span, inferred, p, arg, opts)?;
            }
        }

        return Ok(());
    }

    pub(super) fn insert_inferred(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        tp: &TypeParam,
        ty: Cow<Type>,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        self.insert_inferred_raw(span, inferred, tp.name.clone(), ty, opts)
    }

    /// # Rules
    ///
    /// ## Type literal
    ///
    /// If one of type literal is `specified` according to the metadata, type
    /// inference is done.
    ///
    /// See:
    ///
    /// ```ts
    /// declare function f<T>(...items: T[]): T;
    /// declare let data: { a: 1, b: "abc", c: true };
    /// declare let data2: { b: "foo", c: true };
    ///
    /// // Not specified
    /// let e1 = f({ a: 1, b: 2 }, { a: "abc" }, {});
    /// let e2 = f({}, { a: "abc" }, { a: 1, b: 2 });
    ///
    /// // Type inference is done if at least one element is specified.
    /// let e3 = f(data, { a: 2 }); // Error
    /// let e4 = f({ a: 2 }, data); // Error
    /// let e5 = f(data, data2); // Error
    /// ```
    pub(super) fn insert_inferred_raw(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        name: Id,
        ty: Cow<Type>,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        let marks = self.marks();

        info!(
            "Inferred {} as {}",
            name,
            dump_type_as_string(&self.cm, &ty)
        );

        match ty.n() {
            Type::Param(ty) => {
                if name == ty.name {
                    return Ok(());
                }
            }
            _ => {}
        }

        if ty.is_any() && self.is_implicitly_typed(&ty) {
            if inferred.type_params.contains_key(&name) {
                return Ok(());
            }

            match inferred.defaults.entry(name.clone()) {
                Entry::Occupied(..) => {}
                Entry::Vacant(e) => {
                    e.insert(Type::Param(TypeParam {
                        span: ty.span(),
                        name: name.clone(),
                        constraint: None,
                        default: None,
                        metadata: TypeParamMetadata {
                            common: ty.metadata(),
                            ..Default::default()
                        },
                    }));
                }
            }

            //
            return Ok(());
        }

        match inferred.type_params.entry(name.clone()) {
            Entry::Occupied(mut e) => {
                match e.get() {
                    InferredType::Union(_) => return Ok(()),
                    _ => {}
                }

                if ty.is_union_type() {
                    *e.get_mut() = InferredType::Union(ty.into_owned().cheap());
                    return Ok(());
                }

                match e.get_mut() {
                    InferredType::Union(e) => {
                        unreachable!()
                    }
                    InferredType::Other(e) => {
                        if e.iter().any(|prev| prev.type_eq(&*ty)) {
                            return Ok(());
                        }

                        if !e.is_empty() && !opts.append_type_as_union {
                            inferred.errored.insert(name.clone());
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
                                    &ty,
                                    prev,
                                )
                                .is_ok()
                            {
                                *prev = ty.into_owned().generalize_lit();
                                return Ok(());
                            }
                        }

                        e.push(ty.into_owned().generalize_lit());
                    }
                }
            }
            Entry::Vacant(e) => {
                e.insert(InferredType::Other(vec![ty.into_owned().generalize_lit()]));
            }
        }

        Ok(())
    }

    /// Infer types, using `param` and `arg`.
    pub(crate) fn infer_type_with_types(
        &mut self,
        span: Span,
        type_params: &[TypeParam],
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<FxHashMap<Id, Type>> {
        if cfg!(debug_assertions) {
            // Assertion for deep clone
            let _ = type_params.clone();
            let _ = param.clone();
            let _ = arg.clone();
        }

        let mut inferred = InferData::default();

        self.infer_type(
            span,
            &mut inferred,
            &param,
            &arg,
            InferTypeOpts {
                skip_union: true,
                ..opts
            },
        )
        .context("tried to infer type using two type")?;

        let map = self.finalize_inference(inferred);

        Ok(map.types)
    }

    /// Handle some special builtin types

    pub(super) fn infer_builtin(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> Option<ValidationResult<()>> {
        let param = param.n();
        let arg = arg.n();

        if let Some(elem_type) =
            unwrap_ref_with_single_arg(param, "ReadonlyArray").or_else(|| match param.n() {
                Type::Interface(Interface { name, body, .. }) => {
                    if name == "ReadonlyArray" {
                        body.iter()
                            .filter_map(|v| match v {
                                TypeElement::Index(i) => i.type_ann.as_deref(),
                                _ => None,
                            })
                            .next()
                    } else {
                        None
                    }
                }
                _ => None,
            })
        {
            return Some(self.infer_type(
                span,
                inferred,
                &Type::Array(Array {
                    span: param.span(),
                    elem_type: box elem_type.clone(),
                    metadata: ArrayMetadata {
                        common: param.metadata(),
                        ..Default::default()
                    },
                }),
                arg,
                InferTypeOpts {
                    append_type_as_union: true,
                    ..opts
                },
            ));
        }

        match param {
            Type::Array(Array { elem_type, .. }) => match arg {
                Type::Ref(Ref {
                    type_name: RTsEntityName::Ident(type_name),
                    type_args,
                    ..
                }) if type_name.sym == *"ReadonlyArray" => match type_args {
                    Some(type_args) => {
                        return Some(self.infer_type(
                            span,
                            inferred,
                            &elem_type,
                            &type_args.params[0],
                            InferTypeOpts {
                                append_type_as_union: true,
                                ..opts
                            },
                        ));
                    }
                    None => {}
                },
                _ => {}
            },
            _ => {}
        }

        None
    }

    pub(super) fn infer_type_using_interface(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        let arg = arg.n();

        match arg {
            Type::Interface(arg) => {
                self.infer_type_using_interface_and_interface(span, inferred, param, arg, opts)?;
            }
            Type::TypeLit(arg) => {
                self.infer_type_using_type_elements_and_type_elements(
                    span,
                    inferred,
                    &param.body,
                    &arg.members,
                    opts,
                )?;
            }
            Type::Tuple(..) => {
                // Convert to a type literal.
                if let Some(arg) = self.convert_type_to_type_lit(span, Cow::Borrowed(arg))? {
                    self.infer_type_using_type_elements_and_type_elements(
                        span,
                        inferred,
                        &param.body,
                        &arg.members,
                        opts,
                    )?;
                }
            }
            _ => {
                unimplemented!()
            }
        }

        for parent in &param.extends {
            let parent = self.type_of_ts_entity_name(
                span,
                self.ctx.module_id,
                &parent.expr,
                parent.type_args.as_deref(),
            )?;
            self.infer_type(span, inferred, &parent, arg, opts)?;
        }

        Ok(())
    }

    fn infer_type_using_interface_and_interface(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Interface,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        self.infer_type_using_type_elements_and_type_elements(
            span,
            inferred,
            &param.body,
            &arg.body,
            opts,
        )?;

        Ok(())
    }

    /// Compare fields.
    pub(super) fn infer_type_using_type_lit_and_type_lit(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &TypeLit,
        arg: &TypeLit,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        self.infer_type_using_type_elements_and_type_elements(
            span,
            inferred,
            &param.members,
            &arg.members,
            opts,
        )
    }

    /// Returns `Ok(true)` if this method know how to infer types.
    pub(super) fn infer_type_by_converting_to_type_lit(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<bool> {
        let p = param.n();
        let a = arg.n();
        match (p, a) {
            (Type::Constructor(..), Type::Class(..)) | (Type::Function(..), Type::Function(..)) => {
                return Ok(false)
            }
            (Type::Constructor(..), _) | (Type::Function(..), _) => {
                let p = self.convert_type_to_type_lit(span, Cow::Borrowed(p))?;
                let a = self.convert_type_to_type_lit(span, Cow::Borrowed(a))?;
                if let Some(p) = p {
                    if let Some(a) = a {
                        self.infer_type_using_type_elements_and_type_elements(
                            span, inferred, &p.members, &a.members, opts,
                        )?;
                        return Ok(true);
                    }
                }
            }
            _ => {}
        }

        Ok(false)
    }

    fn infer_type_using_type_elements_and_type_elements(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &[TypeElement],
        arg: &[TypeElement],
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        for p in param {
            for a in arg {
                let opts = match p {
                    TypeElement::Index(..) => InferTypeOpts {
                        append_type_as_union: true,
                        ..opts
                    },
                    _ => InferTypeOpts {
                        append_type_as_union: if opts.for_fn_assignment {
                            false
                        } else {
                            opts.append_type_as_union
                        },
                        ..opts
                    },
                };
                //

                match (p, a) {
                    (TypeElement::Property(p), TypeElement::Property(a)) => {
                        if self
                            .assign(span, &mut Default::default(), &p.key.ty(), &a.key.ty())
                            .is_ok()
                        {
                            if let Some(pt) = &p.type_ann {
                                if let Some(at) = &a.type_ann {
                                    self.infer_type(span, inferred, pt, at, opts)?;
                                } else {
                                    dbg!((&p, &a));
                                }
                            } else {
                                dbg!((&p, &a));
                            }
                        }
                        continue;
                    }

                    (TypeElement::Property(p), TypeElement::Method(a)) => {
                        if self.key_matches(span, &p.key, &a.key, false) {
                            let span = span.with_ctxt(SyntaxContext::empty());

                            if let Some(pt) = &p.type_ann {
                                self.infer_type(
                                    span,
                                    inferred,
                                    &pt,
                                    &Type::Function(Function {
                                        span,
                                        type_params: a.type_params.clone(),
                                        params: a.params.clone(),
                                        ret_ty: a.ret_ty.clone().unwrap_or_else(|| {
                                            box Type::any(span, Default::default())
                                        }),
                                        metadata: Default::default(),
                                    }),
                                    opts,
                                )?;
                            }
                        }
                        continue;
                    }

                    (TypeElement::Method(p), TypeElement::Property(a)) => {
                        if self.key_matches(span, &a.key, &p.key, false) {
                            let span = span.with_ctxt(SyntaxContext::empty());

                            if let Some(at) = &a.type_ann {
                                self.infer_type(
                                    span,
                                    inferred,
                                    &Type::Function(Function {
                                        span,
                                        type_params: p.type_params.clone(),
                                        params: p.params.clone(),
                                        ret_ty: p.ret_ty.clone().unwrap_or_else(|| {
                                            box Type::any(span, Default::default())
                                        }),
                                        metadata: Default::default(),
                                    }),
                                    &at,
                                    opts,
                                )?;
                            }
                        }
                        continue;
                    }

                    (TypeElement::Index(p), TypeElement::Index(a)) => {
                        if p.params.type_eq(&a.params) {
                            if let Some(pt) = &p.type_ann {
                                if let Some(at) = &a.type_ann {
                                    self.infer_type(
                                        span,
                                        inferred,
                                        pt,
                                        at,
                                        InferTypeOpts {
                                            append_type_as_union: true,
                                            ..opts
                                        },
                                    )?;
                                } else {
                                    dbg!((&p, &a));
                                }
                            } else {
                                dbg!((&p, &a));
                            }
                        }
                        continue;
                    }

                    (TypeElement::Index(p), TypeElement::Property(a)) => {
                        assert_eq!(
                            p.params.len(),
                            1,
                            "Index signature should have exactly one parameter"
                        );

                        if self
                            .assign(span, &mut Default::default(), &p.params[0].ty, &a.key.ty())
                            .is_ok()
                            || p.params[0].ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                        {
                            if let Some(p_ty) = &p.type_ann {
                                if let Some(arg_ty) = &a.type_ann {
                                    self.infer_type(
                                        span,
                                        inferred,
                                        &p_ty,
                                        &arg_ty,
                                        InferTypeOpts {
                                            append_type_as_union: true,
                                            ..opts
                                        },
                                    )?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Method(p), TypeElement::Method(a)) => {
                        if self
                            .assign(span, &mut Default::default(), &p.key.ty(), &a.key.ty())
                            .is_ok()
                        {
                            self.infer_type_of_fn_params(
                                span, inferred, &p.params, &a.params, opts,
                            )?;

                            if let Some(p_ret) = &p.ret_ty {
                                if let Some(a_ret) = &a.ret_ty {
                                    self.infer_type(span, inferred, &p_ret, &a_ret, opts)?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Constructor(p), TypeElement::Constructor(a)) => {
                        self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;

                        if let Some(p_ret) = &p.ret_ty {
                            if let Some(a_ret) = &a.ret_ty {
                                self.infer_type(span, inferred, &p_ret, &a_ret, opts)?;
                            }
                        }

                        continue;
                    }

                    (TypeElement::Call(p), TypeElement::Call(a)) => {
                        self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;

                        if let Some(p_ret) = &p.ret_ty {
                            if let Some(a_ret) = &a.ret_ty {
                                self.infer_type(span, inferred, &p_ret, &a_ret, opts)?;
                            }
                        }

                        continue;
                    }

                    (TypeElement::Call(..), _) | (TypeElement::Constructor(..), _) => {
                        // Prevent log
                        continue;
                    }

                    _ => {}
                }

                error!(
                    "unimplemented: type inference: type element:\nParam = {:#?}\nArg = {:#?}",
                    p, a
                );
            }
        }

        Ok(())
    }

    pub(super) fn infer_type_using_operator(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Operator,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        match param.op {
            TsTypeOperatorOp::KeyOf => {}
            TsTypeOperatorOp::Unique => {}
            TsTypeOperatorOp::ReadOnly => {
                return self.infer_type(span, inferred, &param.ty, arg, opts)
            }
        }

        error!(
            "infer_type_from_operator_and_tuple: unimplemented\nparam  = {:#?}\narg = {:#?}",
            param, arg,
        );
        Ok(())
    }

    pub(super) fn infer_types_using_class(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Class,
        arg: &Class,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        self.infer_types_using_class_def(span, inferred, &param.def, &arg.def, opts)
    }

    pub(super) fn infer_types_using_class_def(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &ClassDef,
        arg: &ClassDef,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        for pm in &param.body {
            for am in &arg.body {
                match (pm, am) {
                    (ClassMember::Property(p), ClassMember::Property(a))
                        if p.is_static == a.is_static =>
                    {
                        if self.key_matches(span, &p.key, &a.key, false) {
                            if let Some(p_ty) = &p.value {
                                if let Some(a_ty) = &a.value {
                                    self.infer_type(span, inferred, p_ty, a_ty, opts)?;
                                }
                            }
                        }
                    }

                    _ => {}
                }
            }
        }

        // TODO(kdy1): Check for parents.
        Ok(())
    }

    pub(super) fn finalize_inference(&self, inferred: InferData) -> InferTypeResult {
        let mut map = HashMap::default();

        for (k, v) in inferred.type_params {
            let mut ty = match v {
                InferredType::Union(ty) => ty,
                InferredType::Other(types) => Type::union(types),
            };

            self.replace_null_or_undefined_while_defaulting_to_any(&mut ty);

            ty.make_cheap();

            map.insert(k, ty);
        }

        InferTypeResult {
            types: map,
            errored: inferred.errored,
        }
    }

    /// TODO(kdy1): Handle union
    fn replace_null_or_undefined_while_defaulting_to_any(&self, ty: &mut Type) {
        if ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
            *ty = Type::any(
                ty.span(),
                KeywordTypeMetadata {
                    common: ty.metadata(),
                    ..Default::default()
                },
            );
            return;
        }

        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
            *ty = Type::any(
                ty.span(),
                KeywordTypeMetadata {
                    common: ty.metadata(),
                    ..Default::default()
                },
            );
            return;
        }

        match ty.n() {
            Type::Tuple(..) => match ty.nm() {
                Type::Tuple(ty) => {
                    for elem in ty.elems.iter_mut() {
                        self.replace_null_or_undefined_while_defaulting_to_any(&mut elem.ty);
                    }
                }
                _ => unreachable!(),
            },
            _ => {}
        }
    }

    /// Prevent generalizations if a type parameter extends literal.
    pub(super) fn prevent_generalization_of_inferred_types(
        &mut self,
        type_params: &[TypeParam],
        inferred: &mut InferData,
    ) {
        for type_param in type_params {
            match type_param.constraint.as_deref() {
                Some(Type::Lit(..)) => {}

                Some(ty) => {
                    if !should_prevent_generalization(ty) {
                        continue;
                    }
                }
                _ => continue,
            }

            if let Some(ty) = inferred.type_params.get_mut(&type_param.name) {
                match ty {
                    InferredType::Union(ty) => {
                        prevent_generalize(ty);
                    }
                    InferredType::Other(types) => {
                        for ty in types {
                            prevent_generalize(ty);
                        }
                    }
                }
            }
        }
    }
}

fn should_prevent_generalization(constraint: &Type) -> bool {
    match constraint.n() {
        Type::Lit(LitType {
            lit: RTsLit::Str(..) | RTsLit::Number(..) | RTsLit::Bool(..),
            ..
        })
        | Type::Keyword(KeywordType {
            kind:
                TsKeywordTypeKind::TsStringKeyword
                | TsKeywordTypeKind::TsNumberKeyword
                | TsKeywordTypeKind::TsBooleanKeyword,
            ..
        }) => true,
        Type::Union(Union { ref types, .. }) => {
            types.iter().all(|ty| should_prevent_generalization(&ty))
        }
        _ => false,
    }
}
