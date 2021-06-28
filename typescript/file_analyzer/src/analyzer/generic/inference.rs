use crate::{
    analyzer::{
        generic::{type_form::OldTypeForm, InferData, InferredType},
        Analyzer, Ctx,
    },
    ValidationResult,
};
use fxhash::FxHashMap;
use itertools::Itertools;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_type_form::{compare_type_forms, max_path, TypeForm};
use stc_ts_type_ops::is_str_lit_or_union;
use stc_ts_types::{
    Array, Class, ClassDef, ClassMember, Id, Interface, Operator, Ref, Type, TypeElement, TypeLit, TypeParam, Union,
};
use std::collections::hash_map::Entry;
use swc_common::{Span, Spanned, TypeEq};
use swc_ecma_ast::{TsKeywordTypeKind, TsTypeOperatorOp};

/// # Default
///
/// All fields default to `false`.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct InferTypeOpts {}

impl Analyzer<'_, '_> {
    /// Union-union inference is special, because
    ///
    /// `T | PromiseLike<T>` <= `void | PromiseLike<void>`
    ///
    /// should result in `T = void`, not `T = void | PromiseLike<void>`
    pub(super) fn infer_type_using_union_and_union(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Union,
        arg_ty: &Type,
        arg: &Union,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        // If there's a top-level type parameters, check for `form`s of type.
        if param.types.len() == arg.types.len() && param.types.iter().map(Type::normalize).any(|ty| ty.is_type_param())
        {
            // TODO: Sort types so `T | PromiseLike<T>` has same form as
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
            for (idx, (param, type_path)) in param.types.iter().zip(matched_paths.iter()).enumerate() {
                if type_path == max {
                    self.infer_type(span, inferred, &param, arg, opts)?;
                }
            }

            return Ok(());
        }

        //
        if !self.ctx.skip_union_while_inferencing {
            for p in &param.types {
                self.infer_type(span, inferred, p, arg, opts)?;
            }
        }

        return Ok(());
    }

    /// # Rules
    ///
    /// ## Type literal
    ///
    /// If one of type literal is `specified` accoarding to the metadata, type
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
    pub(super) fn insert_inferred(&mut self, inferred: &mut InferData, name: Id, ty: Type) -> ValidationResult<()> {
        slog::info!(
            self.logger,
            "Inferred {} as {}",
            name,
            dump_type_as_string(&self.cm, &ty)
        );

        match ty.normalize() {
            Type::Param(ty) => {
                if name == ty.name {
                    return Ok(());
                }
            }
            _ => {}
        }

        if ty.is_any() && self.is_implicitly_typed(&ty) {
            if inferred.type_params.contains_key(&name.clone()) {
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
                    }));
                }
            }

            //
            return Ok(());
        }

        match inferred.type_params.entry(name.clone()) {
            Entry::Occupied(e) => match e.get_mut() {
                InferredType::Union(e) => {
                    unreachable!("InferredType::Union should not be stored in hashmap")
                }
                InferredType::Other(e) => {
                    if e.iter().any(|prev| prev.type_eq(&ty)) {
                        return Ok(());
                    }

                    e.push(ty);
                }
            },
            Entry::Vacant(e) => {
                e.insert(InferredType::Other(vec![ty]));
            }
        }

        Ok(())
    }

    pub(crate) fn infer_type_with_types(
        &mut self,
        span: Span,
        type_params: &[TypeParam],
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<FxHashMap<Id, Type>> {
        let mut inferred = InferData::default();

        let ctx = Ctx {
            skip_union_while_inferencing: true,
            ..self.ctx
        };

        self.with_ctx(ctx)
            .infer_type(span, &mut inferred, &param, &arg, opts)
            .context("tried to infer type using two type")?;

        self.finalize_inference(&mut inferred);

        Ok(inferred.type_params)
    }

    /// Handle some special builtin types

    pub(super) fn infer_builtin(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        let param = param.normalize();
        let arg = arg.normalize();

        match param {
            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(type_name),
                type_args,
                ..
            }) if type_name.sym == *"ReadonlyArray" => match type_args {
                Some(type_args) => match arg {
                    Type::Array(Array { elem_type, .. }) => {
                        return self.infer_type(span, inferred, &type_args.params[0], elem_type, opts);
                    }
                    _ => {}
                },
                None => {}
            },

            Type::Array(Array { elem_type, .. }) => match arg {
                Type::Ref(Ref {
                    type_name: RTsEntityName::Ident(type_name),
                    type_args,
                    ..
                }) if type_name.sym == *"ReadonlyArray" => match type_args {
                    Some(type_args) => {
                        return self.infer_type(span, inferred, &elem_type, &type_args.params[0], opts);
                    }
                    None => {}
                },
                _ => {}
            },
            _ => {}
        }

        Ok(())
    }

    pub(super) fn infer_type_using_interface(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> ValidationResult<()> {
        let arg = arg.normalize();

        match arg {
            Type::Interface(arg) => {
                self.infer_type_using_interface_and_interface(span, inferred, param, arg, opts)?;
            }
            Type::TypeLit(arg) => {
                self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.members, opts)?;
            }
            Type::Tuple(..) => {
                // Convert to a type literal.
                if let Some(arg) = self.type_to_type_lit(span, arg)? {
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
            let parent =
                self.type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, parent.type_args.as_deref())?;
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
        self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.body, opts)?;

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
        self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.members, &arg.members, opts)
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
        let p = param.normalize();
        let a = arg.normalize();
        match (p, a) {
            (Type::Constructor(..), Type::Class(..)) | (Type::Function(..), Type::Function(..)) => return Ok(false),
            (Type::Constructor(..), _) | (Type::Function(..), _) => {
                let p = self.type_to_type_lit(span, p)?;
                let a = self.type_to_type_lit(span, a)?;
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
                //

                match (p, a) {
                    (TypeElement::Property(p), TypeElement::Property(a)) => {
                        if self
                            .assign(&mut Default::default(), &p.key.ty(), &a.key.ty(), span)
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

                    (TypeElement::Index(p), TypeElement::Index(a)) => {
                        if p.params.type_eq(&a.params) {
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

                    (TypeElement::Index(p), TypeElement::Property(a)) => {
                        assert_eq!(p.params.len(), 1, "Index signature should have exactly one parameter");

                        if let Ok(()) = self.assign(&mut Default::default(), &p.params[0].ty, &a.key.ty(), span) {
                            if let Some(p_ty) = &p.type_ann {
                                if let Some(arg_ty) = &a.type_ann {
                                    self.infer_type(span, inferred, &p_ty, &arg_ty, opts)?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Method(p), TypeElement::Method(a)) => {
                        if self
                            .assign(&mut Default::default(), &p.key.ty(), &a.key.ty(), span)
                            .is_ok()
                        {
                            self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;

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

                slog::error!(
                    self.logger,
                    "unimplemented: type infernce: type element:\nParam = {:#?}\nArg = {:#?}",
                    p,
                    a
                );
            }
        }

        Ok(())
    }

    pub(super) fn infer_type_from_operator(
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
            TsTypeOperatorOp::ReadOnly => return self.infer_type(span, inferred, &param.ty, arg, opts),
        }

        slog::error!(
            self.logger,
            "infer_type_from_operator_and_tuple: unimplemented\nparam  = {:#?}\narg = {:#?}",
            param,
            arg,
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
                    (ClassMember::Property(p), ClassMember::Property(a)) if p.is_static == a.is_static => {
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

        // TODO: Check for parents.
        Ok(())
    }

    pub(super) fn finalize_inference(&self, inferred: &mut InferData) {
        for (k, v) in inferred.type_params.iter_mut() {
            self.replace_null_or_undefined_while_defaulting_to_any(v);

            v.make_cheap();
        }
    }

    /// TODO: Handle union
    fn replace_null_or_undefined_while_defaulting_to_any(&self, ty: &mut Type) {
        if ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
            *ty = Type::any(ty.span());
            return;
        }

        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
            *ty = Type::any(ty.span());
            return;
        }

        match ty.normalize() {
            Type::Tuple(..) => match ty.normalize_mut() {
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
                    if !is_str_lit_or_union(ty) {
                        continue;
                    }
                }
                _ => continue,
            }

            if let Some(ty) = inferred.type_params.get_mut(&type_param.name) {
                self.prevent_generalize(ty);
            }
        }
    }
}
