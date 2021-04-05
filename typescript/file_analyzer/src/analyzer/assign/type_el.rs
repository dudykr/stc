use super::AssignOpts;
use crate::analyzer::util::ResultExt;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_types::Array;
use stc_ts_types::Class;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassMember;
use stc_ts_types::MethodSignature;
use stc_ts_types::ModuleId;
use stc_ts_types::Operator;
use stc_ts_types::PropertySignature;
use stc_ts_types::Ref;
use stc_ts_types::Tuple;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeLitMetadata;
use stc_ts_types::TypeParamInstantiation;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;
use swc_ecma_ast::Accessibility;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::TsTypeOperatorOp;

impl Analyzer<'_, '_> {
    /// This method is called when lhs of assignment is interface or type
    /// literal.
    ///
    /// ```js
    /// interface A {}
    /// let a: A = foo;
    /// let b: { key: string } = foo;
    /// ```
    pub(super) fn assign_to_type_elements(
        &mut self,
        opts: AssignOpts,
        lhs_span: Span,
        lhs: &[TypeElement],
        rhs: &Type,
        lhs_metadata: TypeLitMetadata,
    ) -> ValidationResult<()> {
        let span = opts.span;
        // debug_assert!(!span.is_dummy());

        let mut errors = vec![];
        let mut missing_fields = vec![];

        let numeric_keyed_ty = lhs
            .iter()
            .filter_map(|e| match e {
                TypeElement::Index(ref i)
                    if i.params.len() == 1 && i.params[0].ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword) =>
                {
                    Some(i.type_ann.as_ref())
                }

                _ => None,
            })
            .next();

        if let Some(numeric_keyed_ty) = numeric_keyed_ty {
            let any = box Type::any(span);
            let numeric_keyed_ty = numeric_keyed_ty.unwrap_or(&any);

            match *rhs.normalize() {
                Type::Array(Array { ref elem_type, .. }) => {
                    return self.assign_inner(numeric_keyed_ty, elem_type, opts)
                }

                Type::Tuple(Tuple { ref elems, .. }) => {
                    let mut errors = Errors::default();
                    for el in elems {
                        self.assign_inner(
                            numeric_keyed_ty,
                            &el.ty,
                            AssignOpts {
                                span: if el.span().is_dummy() { span } else { el.span() },
                                ..opts
                            },
                        )
                        .store(&mut errors);
                    }
                    return if errors.is_empty() {
                        Ok(())
                    } else {
                        Err(Error::Errors {
                            span,
                            errors: errors.into(),
                        })
                    };
                }

                _ => {}
            }
        }

        {
            let mut unhandled_rhs = vec![];

            match rhs.normalize() {
                Type::Ref(Ref {
                    type_name:
                        RTsEntityName::Ident(RIdent {
                            sym: js_word!("Function"),
                            ..
                        }),
                    ..
                }) => {
                    if lhs.iter().any(|el| match el {
                        TypeElement::Call(..) | TypeElement::Constructor(..) => true,
                        _ => false,
                    }) {
                        return Ok(());
                    }
                }

                Type::TypeLit(TypeLit {
                    members: rhs_members,
                    metadata: rhs_metadata,
                    ..
                }) => {
                    let allow_unknown_rhs = opts.allow_unknown_rhs || rhs_metadata.inexact;
                    for r in rhs_members {
                        if !allow_unknown_rhs {
                            unhandled_rhs.push(r.span());
                        }
                    }

                    self.handle_assignment_of_type_elements_to_type_elements(
                        AssignOpts {
                            allow_unknown_rhs,
                            ..opts
                        },
                        &mut missing_fields,
                        &mut unhandled_rhs,
                        lhs,
                        rhs_members,
                    )
                    .context("tried assignment of a type literal to a type literals")
                    .store(&mut errors);
                }

                Type::Interface(..) => {
                    if let Some(rty) = self
                        .type_to_type_lit(span, &rhs)?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                    {
                        return self.assign_to_type_elements(opts, lhs_span, lhs, &rty, lhs_metadata);
                    }

                    return Err(Error::SimpleAssignFailed { span });
                }

                Type::Tuple(..) if lhs.is_empty() => return Ok(()),

                Type::Array(..) if lhs.is_empty() => return Ok(()),

                Type::Array(r_arr) => {
                    if lhs.iter().any(|member| match member {
                        TypeElement::Property(_) | TypeElement::Method(_) => true,
                        _ => false,
                    }) {
                        return Err(Error::SimpleAssignFailed { span });
                    }

                    //
                    let r_arr = Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new("Array".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation {
                            span: DUMMY_SP,
                            params: vec![*r_arr.elem_type.clone()],
                        }),
                    });

                    let rhs = self.normalize(&r_arr, Default::default())?;

                    return self
                        .assign_to_type_elements(
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            lhs_span,
                            lhs,
                            &rhs,
                            lhs_metadata,
                        )
                        .context("tried to assign an array as interface to type elements");
                }

                Type::Tuple(rhs) => {
                    // Handle { 0: nubmer } = [1]
                    let rhs_len = rhs.elems.len();

                    // TODO: Check for literal properties

                    // for el in lhs {
                    //     match el {
                    //         TypeElement::Property(l_el) => {
                    //             match l
                    //         }
                    //         _ => {}
                    //     }
                    // }

                    return Ok(());
                }

                Type::ClassDef(rhs_cls) => {
                    let rhs = self
                        .type_to_type_lit(span, &rhs)
                        .context("tried to convert a class definition into a type literal for assignment")?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                        .unwrap();

                    return self
                        .assign_to_type_elements(
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            lhs_span,
                            lhs,
                            &rhs,
                            lhs_metadata,
                        )
                        .convert_err(|err| match err {
                            Error::Errors { span, .. } => Error::SimpleAssignFailed { span },
                            Error::MissingFields { span, .. } => Error::SimpleAssignFailed { span },
                            _ => err,
                        })
                        .context("tried to assign a class definition to type elements");
                }

                Type::Class(rhs_cls) => {
                    // TODO: Check if constructor exists.
                    if rhs_cls.def.is_abstract {
                        return Err(Error::CannotAssignAbstractConstructorToNonAbstractConstructor { span });
                    }

                    // TODO: Optimize
                    // for el in lhs {
                    //     self.assign_class_members_to_type_element(opts, el, &rhs.body)?;
                    // }

                    let rhs = self
                        .type_to_type_lit(span, &rhs)
                        .context("tried to convert a class into type literal for assignment")?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                        .unwrap();

                    return self
                        .assign_to_type_elements(
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            lhs_span,
                            lhs,
                            &rhs,
                            lhs_metadata,
                        )
                        .context("tried to assign a class instance to type elements");
                }

                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Number(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Str(..), ..
                })
                | Type::Mapped(..)
                    if lhs.is_empty() =>
                {
                    return Ok(())
                }

                Type::Enum(r) => {
                    let rhs = self.enum_to_type_lit(r).map(Type::TypeLit)?;
                    return self
                        .assign_to_type_elements(
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            lhs_span,
                            lhs,
                            &rhs,
                            lhs_metadata,
                        )
                        .context("tried to assign an enum to type elements");
                }

                Type::Function(..) | Type::Constructor(..) => {
                    let rhs = self
                        .type_to_type_lit(span, &rhs)
                        .context("tried to convert a function to a type literal for asssignment")?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                        .unwrap();

                    return self
                        .assign_to_type_elements(opts, lhs_span, lhs, &rhs, lhs_metadata)
                        .context("tried to assign the converted type to type elements");
                }

                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Number(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Str(..), ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Bool(..), ..
                }) => return Err(Error::SimpleAssignFailed { span }),

                // TODO: Strict mode
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => return Ok(()),

                // TODO: Strict mode
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => return Ok(()),

                Type::Mapped(r_mapped) => {
                    for l_el in lhs {
                        match l_el {
                            TypeElement::Call(_) => {}
                            TypeElement::Constructor(_) => {}
                            TypeElement::Property(_) => {}
                            TypeElement::Method(_) => {}
                            TypeElement::Index(l_index) => {
                                if let Some(Type::Operator(Operator {
                                    op: TsTypeOperatorOp::KeyOf,
                                    ty: r_constraint,
                                    ..
                                })) = r_mapped.type_param.constraint.as_deref().map(|ty| ty.normalize())
                                {
                                    if let Ok(()) = self.assign_with_opts(opts, &l_index.params[0].ty, &&r_constraint) {
                                        if let Some(l_type_ann) = &l_index.type_ann {
                                            if let Some(r_ty) = &r_mapped.ty {
                                                self.assign_with_opts(opts, &l_type_ann, &r_ty)
                                                    .context("tried to assign a mapped type to an index signature")?;
                                            }
                                        }

                                        return Ok(());
                                    }
                                }
                            }
                        }
                    }
                }

                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsObjectKeyword,
                    ..
                }) => return Ok(()),

                _ => {
                    return Err(Error::Unimplemented {
                        span,
                        msg: format!("assign_to_type_elements - {:#?}", rhs),
                    })
                }
            }

            if !errors.is_empty() {
                return Err(Error::ObjectAssignFailed { span, errors })?;
            }

            if !unhandled_rhs.is_empty() {
                // The code below is invalid as c is not defined in type.
                //
                //      var c { [n: number]: { a: string; b: number; }; } = [{ a:
                // '', b: 0, c: '' }];

                return Err(Error::Errors {
                    span,
                    errors: unhandled_rhs
                        .into_iter()
                        .map(|span| Error::UnknownPropertyInObjectLiteralAssignment { span })
                        .collect(),
                });
            }
        }

        'l: for m in lhs {
            // Handle `toString()`
            match m {
                TypeElement::Method(ref m) => {
                    if m.key == js_word!("toString") {
                        continue;
                    }
                }
                _ => {}
            }

            // Handle optional
            match m {
                TypeElement::Method(ref m) if m.optional => continue,
                TypeElement::Property(ref m) if m.optional => continue,
                _ => {}
            }

            match *rhs.normalize() {
                // Check class members
                Type::Class(Class {
                    def: box ClassDef { ref body, .. },
                    ..
                }) => {
                    match m {
                        TypeElement::Call(_) => {
                            unimplemented!("assign: interface {{ () => ret; }} = new Foo()")
                        }
                        TypeElement::Constructor(_) => {
                            unimplemented!("assign: interface {{ new () => ret; }} = new Foo()")
                        }
                        TypeElement::Property(ref lp) => {
                            for rm in body {
                                match rm {
                                    ClassMember::Property(ref rp) => {
                                        match rp.accessibility {
                                            Some(Accessibility::Private) | Some(Accessibility::Protected) => {
                                                errors.push(Error::AccessibilityDiffers { span });
                                            }
                                            _ => {}
                                        }

                                        if lp.key.type_eq(&rp.key) {
                                            continue 'l;
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            unimplemented!("assign: interface {{ prop: string; }} = new Foo()")
                        }
                        TypeElement::Method(_) => {
                            unimplemented!("assign: interface {{ method() => ret; }} = new Foo()")
                        }
                        TypeElement::Index(_) => {
                            unimplemented!("assign: interface {{ [key: string]: Type; }} = new Foo()")
                        }
                    }
                    // TOOD: missing fields
                }

                Type::Tuple(..)
                | Type::Array(..)
                | Type::Lit(..)
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                }) => return Ok(()),

                _ => {}
            }
        }

        if !missing_fields.is_empty() {
            errors.push(Error::MissingFields {
                span,
                fields: missing_fields,
            });
        }

        if !errors.is_empty() {
            return Err(Error::Errors {
                span,
                errors: errors.into(),
            });
        }

        Ok(())
    }
    fn handle_assignment_of_type_elements_to_type_elements(
        &mut self,
        opts: AssignOpts,
        missing_fields: &mut Vec<TypeElement>,
        unhandled_rhs: &mut Vec<Span>,
        lhs: &[TypeElement],
        rhs: &[TypeElement],
    ) -> ValidationResult<()> {
        let span = opts.span;

        for (i, m) in lhs.into_iter().enumerate().filter(|(_, m)| m.key().is_some()) {
            let res = self
                .assign_type_elements_to_type_element(opts, missing_fields, m, rhs)
                .with_context(|| format!("tried to assign to {}th element: {:?}", i, m.key()));

            let success = match res {
                Ok(()) => true,
                Err(Error::Errors { ref errors, .. }) if errors.is_empty() => true,
                Err(err) => return Err(err),
            };
            if success && rhs.len() > i {
                if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rhs[i].span()) {
                    unhandled_rhs.remove(pos);
                } else {
                    // panic!("it should be removable")
                }
            }
        }

        let mut errors = vec![];

        // Index signature can eat multiple rhs.
        for m in lhs.iter().filter(|m| m.key().is_none()) {
            for r in rhs {
                let res = self
                    .assign_type_elements_to_type_element(opts, missing_fields, m, &[r.clone()])
                    .with_context(|| format!("tried to assign to an element (not a key-based)"));

                let success = match res {
                    Ok(()) => true,
                    Err(Error::Errors { ref errors, .. }) if errors.is_empty() => true,
                    Err(err) => {
                        errors.push(err);
                        false
                    }
                };

                if success {
                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == r.span()) {
                        unhandled_rhs.remove(pos);
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(Error::Errors { span, errors });
        }

        Ok(())
    }

    /// This method assigns each property to corresponding property.
    pub(super) fn assign_type_elements_to_type_element(
        &mut self,
        opts: AssignOpts,
        missing_fields: &mut Vec<TypeElement>,
        lm: &TypeElement,
        rhs_members: &[TypeElement],
    ) -> ValidationResult<()> {
        let span = opts.span;
        // We need this to show error if not all of rhs_member is matched

        if let Some(l_key) = lm.key() {
            for rm in rhs_members {
                if let Some(r_key) = rm.key() {
                    if l_key.type_eq(&*r_key) {
                        match lm {
                            TypeElement::Property(ref lp) => match rm {
                                TypeElement::Property(ref r_el) => {
                                    if lp.accessibility != r_el.accessibility {
                                        if lp.accessibility == Some(Accessibility::Private)
                                            || r_el.accessibility == Some(Accessibility::Private)
                                        {
                                            return Err(Error::AssignFailedDueToAccessibility { span });
                                        }
                                    }

                                    self.assign_inner(
                                        lp.type_ann.as_deref().unwrap_or(&Type::any(span)),
                                        r_el.type_ann.as_deref().unwrap_or(&Type::any(span)),
                                        opts,
                                    )?;
                                    return Ok(());
                                }
                                TypeElement::Method(rm) => {
                                    if let Some(lp_ty) = &lp.type_ann {
                                        if let Type::Function(lp_ty) = lp_ty.normalize() {
                                            self.assign_params(opts, &lp_ty.params, &rm.params, true).context(
                                                "tried to assign parameters of a method property to the parameters of \
                                                 a property with callable type",
                                            )?;

                                            // TODO: Return type
                                        }
                                    }
                                    return Ok(());
                                }
                                _ => {}
                            },

                            // `foo(a: string) is assignable to foo(a: any)`
                            TypeElement::Method(ref lm) => match rm {
                                TypeElement::Method(ref rm) => {
                                    //

                                    self.assign_params(opts, &lm.params, &rm.params, true)?;

                                    // TODO: Return type

                                    return Ok(());
                                }

                                TypeElement::Property(rp) => {
                                    // Allow assigning property with callable type to methods.
                                    if let Some(rp_ty) = &rp.type_ann {
                                        if let Type::Function(rp_ty) = rp_ty.normalize() {
                                            self.assign_params(opts, &lm.params, &rp_ty.params, true).context(
                                                "tried to assign parameters of a property with callable type to a \
                                                 method parameters",
                                            )?;

                                            // TODO: Return type
                                        }
                                    }
                                    return Ok(());
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                }
            }

            match lm {
                TypeElement::Property(PropertySignature { optional: true, .. })
                | TypeElement::Method(MethodSignature { optional: true, .. }) => {}
                _ => {
                    // No property with `key` found.
                    missing_fields.push(lm.clone());
                }
            }
        } else {
            match lm {
                // TODO: Check type of the index.
                TypeElement::Index(l_index) => {
                    // TODO: Verify
                    for rm in rhs_members {
                        match rm {
                            TypeElement::Call(_) | TypeElement::Constructor(_) => continue,

                            TypeElement::Property(r_prop) => {
                                if let Ok(()) = self.assign(&l_index.params[0].ty, &r_prop.key.ty(), span) {
                                    if let Some(l_index_ret_ty) = &l_index.type_ann {
                                        if let Some(r_prop_ty) = &r_prop.type_ann {
                                            self.assign_with_opts(opts, &l_index_ret_ty, &&r_prop_ty).context(
                                                "tried to assign a type of property to thr type of an index signature",
                                            )?;
                                        }
                                    }
                                }
                            }

                            TypeElement::Method(_) => {
                                slog::error!(self.logger, "unimplemented: Index = Method");
                            }
                            TypeElement::Index(_) => {
                                slog::error!(self.logger, "unimplemented: Index = Index");
                            }
                        }
                    }
                }
                TypeElement::Call(..) => {
                    //
                    for rm in rhs_members {
                        match rm {
                            // TODO: Check type of parameters
                            // TODO: Check return type
                            TypeElement::Call(..) => return Ok(()),
                            _ => {}
                        }
                    }

                    missing_fields.push(lm.clone());
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub(super) fn assign_class_members_to_type_element(
        &mut self,
        opts: AssignOpts,
        el: &TypeElement,
        rhs_members: &[ClassMember],
    ) -> ValidationResult<()> {
        match el {
            TypeElement::Property(lp) => {
                for rhs_member in rhs_members {
                    match rhs_member {
                        ClassMember::Method(_) => {}
                        ClassMember::Property(rp) => {
                            // Check for property
                            if self.assign(&lp.key.ty(), &rp.key.ty(), opts.span).is_ok() {
                                if let Some(lt) = &lp.type_ann {
                                    if let Some(rt) = &rp.value {
                                        self.assign(&lt, &rt, opts.span)?;
                                        return Ok(());
                                    }
                                }
                            }
                        }
                        _ => continue,
                    }
                }

                if lp.optional {
                    return Ok(());
                }

                // TODO: Report error.
            }

            // TODO: Optimize
            TypeElement::Call(_) | TypeElement::Constructor(_) | TypeElement::Method(_) | TypeElement::Index(_) => {}
        }

        Ok(())
    }
}
