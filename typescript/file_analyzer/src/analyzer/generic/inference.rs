use super::InferData;
use crate::analyzer::Analyzer;
use crate::util::is_str_lit_or_union;
use crate::ValidationResult;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_types::Array;
use stc_ts_types::Class;
use stc_ts_types::ClassMember;
use stc_ts_types::Interface;
use stc_ts_types::Operator;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeParam;
use swc_common::Span;
use swc_common::TypeEq;
use swc_ecma_ast::TsTypeOperatorOp;

impl Analyzer<'_, '_> {
    /// Handle some special builtin types

    pub(super) fn infer_builtin(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
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
                        return self.infer_type(span, inferred, &type_args.params[0], elem_type);
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
                        return self.infer_type(span, inferred, &elem_type, &type_args.params[0]);
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
    ) -> ValidationResult<()> {
        let arg = arg.normalize();

        match arg {
            Type::Interface(arg) => {
                self.infer_type_using_interface_and_interface(span, inferred, param, arg)?;
            }
            Type::TypeLit(arg) => {
                self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.members)?;
            }
            _ => {
                todo!()
            }
        }

        for parent in &param.extends {
            let parent =
                self.type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, parent.type_args.as_deref())?;
            self.infer_type(span, inferred, &parent, arg)?;
        }

        Ok(())
    }

    fn infer_type_using_interface_and_interface(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Interface,
    ) -> ValidationResult<()> {
        self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.body)?;

        Ok(())
    }

    /// Compare fields.
    pub(super) fn infer_type_using_type_lit_and_type_lit(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &TypeLit,
        arg: &TypeLit,
    ) -> ValidationResult<()> {
        self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.members, &arg.members)
    }

    /// Returns `Ok(true)` if this method know how to infer types.
    pub(super) fn infer_type_by_converting_to_type_lit(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
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
                        self.infer_type_using_type_elements_and_type_elements(span, inferred, &p.members, &a.members)?;
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
    ) -> ValidationResult<()> {
        for p in param {
            for a in arg {
                //

                match (p, a) {
                    (TypeElement::Property(p), TypeElement::Property(a)) => {
                        if self.assign(&p.key.ty(), &a.key.ty(), span).is_ok() {
                            if let Some(pt) = &p.type_ann {
                                if let Some(at) = &a.type_ann {
                                    self.infer_type(span, inferred, pt, at)?;
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
                                    self.infer_type(span, inferred, pt, at)?;
                                }
                            } else {
                                dbg!((&p, &a));
                            }
                        } else {
                            dbg!((&p, &a));
                        }
                        continue;
                    }

                    (TypeElement::Index(p), TypeElement::Property(a)) => {
                        assert_eq!(p.params.len(), 1, "Index signature should have exactly one parameter");

                        if let Ok(()) = self.assign(&p.params[0].ty, &a.key.ty(), span) {
                            if let Some(p_ty) = &p.type_ann {
                                if let Some(arg_ty) = &a.type_ann {
                                    self.infer_type(span, inferred, &p_ty, &arg_ty)?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Method(p), TypeElement::Method(a)) => {
                        if self.assign(&p.key.ty(), &a.key.ty(), span).is_ok() {
                            self.infer_type_of_fn_params(span, inferred, &p.params, &a.params)?;

                            if let Some(p_ret) = &p.ret_ty {
                                if let Some(a_ret) = &a.ret_ty {
                                    self.infer_type(span, inferred, &p_ret, &a_ret)?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Constructor(p), TypeElement::Constructor(a)) => {
                        self.infer_type_of_fn_params(span, inferred, &p.params, &a.params)?;

                        if let Some(p_ret) = &p.ret_ty {
                            if let Some(a_ret) = &a.ret_ty {
                                self.infer_type(span, inferred, &p_ret, &a_ret)?;
                            }
                        }

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
    ) -> ValidationResult<()> {
        match param.op {
            TsTypeOperatorOp::KeyOf => {}
            TsTypeOperatorOp::Unique => {}
            TsTypeOperatorOp::ReadOnly => return self.infer_type(span, inferred, &param.ty, arg),
        }

        slog::error!(
            self.logger,
            "infer_type_from_operator_and_tuple: unimplemented\nparam  = {:#?}\narg = {:#?}",
            param,
            arg,
        );
        Ok(())
    }

    pub(super) fn infer_class(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Class,
        arg: &Class,
    ) -> ValidationResult<()> {
        for pm in &param.body {
            for am in &arg.body {
                match (pm, am) {
                    (ClassMember::Property(p), ClassMember::Property(a)) if p.is_static == a.is_static => {
                        if self.assign(&p.key.ty(), &a.key.ty(), span).is_ok() {
                            if let Some(p_ty) = &p.value {
                                if let Some(a_ty) = &a.value {
                                    self.infer_type(span, inferred, p_ty, a_ty)?;
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
