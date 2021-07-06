use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        types::NormalizeTypeOpts,
        util::ResultExt,
        Analyzer,
    },
    ValidationResult,
};
use rnode::NodeId;
use stc_ts_ast_rnode::{RIdent, RTsEntityName, RTsKeywordType, RTsLit, RTsLitType};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt, Error, Errors};
use stc_ts_type_ops::Fix;
use stc_ts_types::{
    Array, Class, ClassDef, ClassMember, MethodSignature, ModuleId, Operator, PropertySignature, Ref, Tuple, Type,
    TypeElement, TypeLit, TypeLitMetadata, TypeParamInstantiation, Union,
};
use stc_utils::ext::SpanExt;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::{Span, Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::{Accessibility, TsKeywordTypeKind, TsTypeOperatorOp};

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
        data: &mut AssignData,
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
                    return self.assign_inner(data, numeric_keyed_ty, elem_type, opts)
                }

                Type::Tuple(Tuple { ref elems, .. }) => {
                    let mut errors = Errors::default();
                    for el in elems {
                        self.assign_inner(
                            data,
                            numeric_keyed_ty,
                            &el.ty,
                            AssignOpts {
                                span: el.span().or_else(|| span),
                                ..opts
                            },
                        )
                        .context("tried to assign an element of tuple to numerically keyed type")
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
                            // optional members do not have effect.
                            match r {
                                TypeElement::Property(PropertySignature { optional: true, .. })
                                | TypeElement::Method(MethodSignature { optional: true, .. }) => continue,
                                _ => {}
                            }
                            unhandled_rhs.push(r.span());
                        }
                    }

                    self.handle_assignment_of_type_elements_to_type_elements(
                        data,
                        AssignOpts {
                            allow_unknown_rhs,
                            ..opts
                        },
                        &mut missing_fields,
                        &mut unhandled_rhs,
                        lhs,
                        rhs_members,
                    )
                    .with_context(|| {
                        format!(
                            "tried assignment of a type literal to a type literals\nLHS={}\nRHS={}",
                            dump_type_as_string(
                                &self.cm,
                                &Type::TypeLit(TypeLit {
                                    span: DUMMY_SP,
                                    members: lhs.to_vec(),
                                    metadata: Default::default()
                                })
                            ),
                            dump_type_as_string(
                                &self.cm,
                                &Type::TypeLit(TypeLit {
                                    span: DUMMY_SP,
                                    members: rhs_members.to_vec(),
                                    metadata: Default::default()
                                })
                            ),
                        )
                    })
                    .store(&mut errors);
                }

                Type::Interface(..) | Type::Intersection(..) => {
                    if let Some(rty) = self
                        .type_to_type_lit(span, &rhs)?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                    {
                        return self.assign_to_type_elements(data, opts, lhs_span, lhs, &rty, lhs_metadata);
                    }

                    return Err(Error::SimpleAssignFailed { span });
                }

                Type::Tuple(..) | Type::Array(..) | Type::EnumVariant(..) if lhs.is_empty() => return Ok(()),

                Type::Array(..) | Type::Tuple(..) => {
                    if opts.allow_assignment_of_array_to_optional_type_lit {
                        if lhs.iter().all(|el| match el {
                            TypeElement::Property(PropertySignature { optional: true, .. })
                            | TypeElement::Method(MethodSignature { optional: true, .. }) => true,
                            _ => false,
                        }) {
                            return Ok(());
                        }
                    }
                    if lhs.iter().any(|member| match member {
                        TypeElement::Property(PropertySignature { optional: true, .. })
                        | TypeElement::Method(MethodSignature { optional: true, .. }) => true,
                        _ => false,
                    }) {
                        return Err(Error::SimpleAssignFailed { span });
                    }

                    match rhs.normalize() {
                        Type::Array(r_arr) => {
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

                            let rhs = self.normalize(None, Cow::Owned(r_arr), Default::default())?;

                            return self
                                .assign_to_type_elements(
                                    data,
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

                        Type::Tuple(r_tuple) => {
                            {
                                // Try assigning as an array.

                                let r_elem_type = Type::Union(Union {
                                    span: r_tuple.span,
                                    types: r_tuple.elems.iter().map(|el| *el.ty.clone()).collect(),
                                })
                                .fixed();

                                //
                                let r_arr = Type::Ref(Ref {
                                    span,
                                    ctxt: ModuleId::builtin(),
                                    type_name: RTsEntityName::Ident(RIdent::new("Array".into(), DUMMY_SP)),
                                    type_args: Some(box TypeParamInstantiation {
                                        span: DUMMY_SP,
                                        params: vec![r_elem_type],
                                    }),
                                });

                                let rhs = self.normalize(None, Cow::Owned(r_arr), Default::default())?;

                                if let Ok(()) = self.assign_to_type_elements(
                                    data,
                                    AssignOpts {
                                        allow_unknown_rhs: true,
                                        ..opts
                                    },
                                    lhs_span,
                                    lhs,
                                    &rhs,
                                    lhs_metadata,
                                ) {
                                    return Ok(());
                                }
                            }

                            if let Some(rhs) = self
                                .type_to_type_lit(span, rhs)?
                                .map(Cow::into_owned)
                                .map(Type::TypeLit)
                            {
                                return self
                                    .assign_to_type_elements(
                                        data,
                                        AssignOpts {
                                            allow_unknown_rhs: true,
                                            ..opts
                                        },
                                        lhs_span,
                                        lhs,
                                        &rhs,
                                        lhs_metadata,
                                    )
                                    .context("tried to assign a tuple as type literal to type elements");
                            }
                        }

                        _ => unreachable!(),
                    }

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
                            data,
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
                        .with_context(|| {
                            format!(
                                "tried to assign a class definition to type elements\nRHS = {}",
                                dump_type_as_string(&self.cm, &rhs),
                            )
                        });
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
                            data,
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
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
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
                    lit: RTsLit::BigInt(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Str(..), ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Bool(..), ..
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
                            data,
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
                        .assign_to_type_elements(data, opts, lhs_span, lhs, &rhs, lhs_metadata)
                        .with_context(|| {
                            format!(
                                "tried to assign the converted type to type elements:\nRHS={}",
                                dump_type_as_string(&self.cm, &rhs)
                            )
                        });
                }

                Type::Keyword(RTsKeywordType {
                    kind: kind @ TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: kind @ TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: kind @ TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: kind @ TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                }) => {
                    let rhs = Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent {
                            span,
                            sym: match kind {
                                TsKeywordTypeKind::TsNumberKeyword => "Number".into(),
                                TsKeywordTypeKind::TsBooleanKeyword => "Boolean".into(),
                                TsKeywordTypeKind::TsBigIntKeyword => "BigInt".into(),
                                TsKeywordTypeKind::TsStringKeyword => "String".into(),
                                _ => {
                                    unreachable!()
                                }
                            },
                            node_id: NodeId::invalid(),
                            optional: false,
                        }),
                        type_args: None,
                    });

                    let rhs = self.normalize(None, Cow::Owned(rhs), Default::default())?;

                    // Try builtin assignment
                    return self
                        .assign_to_type_elements(
                            data,
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            lhs_span,
                            lhs,
                            &rhs,
                            lhs_metadata,
                        )
                        .with_context(|| {
                            format!(
                                "tried to assign a keyword as builtin to type elements\nRHS = {}",
                                dump_type_as_string(&self.cm, &rhs)
                            )
                        });
                }

                Type::Lit(RTsLitType {
                    lit: lit @ RTsLit::Number(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: lit @ RTsLit::Str(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: lit @ RTsLit::Bool(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: lit @ RTsLit::BigInt(..),
                    ..
                }) => {
                    // Try keyword assignment

                    return self
                        .assign_to_type_elements(
                            data,
                            opts,
                            lhs_span,
                            lhs,
                            &Type::Keyword(RTsKeywordType {
                                span,
                                kind: match lit {
                                    RTsLit::BigInt(_) => TsKeywordTypeKind::TsBigIntKeyword,
                                    RTsLit::Number(_) => TsKeywordTypeKind::TsNumberKeyword,
                                    RTsLit::Str(_) => TsKeywordTypeKind::TsStringKeyword,
                                    RTsLit::Bool(_) => TsKeywordTypeKind::TsBooleanKeyword,
                                    _ => {
                                        unreachable!()
                                    }
                                },
                            }),
                            lhs_metadata,
                        )
                        .context("tried to assign a literal as keyword to type elements");
                }

                Type::Param(..)
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
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
                                    if let Ok(()) =
                                        self.assign_with_opts(data, opts, &l_index.params[0].ty, &&r_constraint)
                                    {
                                        if let Some(l_type_ann) = &l_index.type_ann {
                                            if let Some(r_ty) = &r_mapped.ty {
                                                self.assign_with_opts(data, opts, &l_type_ann, &r_ty)
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

                Type::EnumVariant(..) => return Err(Error::SimpleAssignFailed { span }),

                Type::Keyword(..) => {
                    let rhs = self
                        .normalize(
                            Some(span),
                            Cow::Borrowed(&rhs),
                            NormalizeTypeOpts {
                                normalize_keywords: true,
                                ..Default::default()
                            },
                        )
                        .convert_err(|err| Error::SimpleAssignFailed { span: err.span() })
                        .context("failed to normalize")?;

                    if rhs.normalize().is_keyword() {
                        return Err(
                            Error::SimpleAssignFailed { span }.context("failed to assign builtin type of a keyword")
                        );
                    }

                    return self
                        .assign_to_type_elements(data, opts, lhs_span, lhs, &rhs, lhs_metadata)
                        .context("tried to assign using expanded builtin type");
                }

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
        data: &mut AssignData,
        opts: AssignOpts,
        missing_fields: &mut Vec<TypeElement>,
        unhandled_rhs: &mut Vec<Span>,
        lhs: &[TypeElement],
        rhs: &[TypeElement],
    ) -> ValidationResult<()> {
        let span = opts.span;

        let mut errors = vec![];

        for (i, m) in lhs.into_iter().enumerate().filter(|(_, m)| m.key().is_some()) {
            let res = self
                .assign_type_elements_to_type_element(data, opts, missing_fields, unhandled_rhs, m, rhs)
                .with_context(|| format!("tried to assign to {}th element: {:?}", i, m.key()));

            match res {
                Ok(()) => {}
                Err(Error::Errors { ref errors, .. }) if errors.is_empty() => {}
                Err(err) => errors.push(err),
            }
        }

        if !errors.is_empty() {
            return Err(Error::Errors { span, errors });
        }

        // Index signature can eat multiple rhs.
        for m in lhs.iter().filter(|m| m.key().is_none()) {
            for r in rhs {
                let res = self
                    .assign_type_elements_to_type_element(data, opts, missing_fields, unhandled_rhs, m, &[r.clone()])
                    .with_context(|| format!("tried to assign to an element (not a key-based)"));

                errors.extend(res.err());
            }
        }

        if !errors.is_empty() {
            return Err(Error::Errors { span, errors });
        }

        Ok(())
    }

    /// This method assigns each property to corresponding property.

    ///
    ///
    ///
    /// # Implementation notes
    ///
    ///
    /// ## Methods
    ///
    /// ### Type parameters
    ///
    /// ```ts
    /// interface T {
    ///     f(x: number): void;
    /// }
    /// var t: T;
    /// t = { f: <T>(x:T) => 1 };
    /// ```
    /// This is valid.
    ///
    ///
    /// ## Call signatures
    ///
    /// ```ts
    // declare var a: {
    ///     (s: string): void
    ///     (s: number): void
    /// }
    /// declare var b: {
    ///     (s: string): void
    /// }
    ///
    ///
    /// a = b // error
    /// b = a // ok
    /// ```
    fn assign_type_elements_to_type_element(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        missing_fields: &mut Vec<TypeElement>,
        unhandled_rhs: &mut Vec<Span>,
        lm: &TypeElement,
        rhs_members: &[TypeElement],
    ) -> ValidationResult<()> {
        let span = opts.span;
        // We need this to show error if not all of rhs_member is matched

        let mut errors = vec![];
        let mut done = false;

        if let Some(l_key) = lm.key() {
            for rm in rhs_members {
                if let Some(r_key) = rm.key() {
                    let opts = AssignOpts {
                        right_ident_span: Some(r_key.span()),
                        ..opts
                    };
                    if l_key.type_eq(&*r_key) {
                        match lm {
                            TypeElement::Property(ref lp) => match rm {
                                TypeElement::Property(ref rp) => {
                                    if lp.accessibility != rp.accessibility {
                                        if lp.accessibility == Some(Accessibility::Private)
                                            || rp.accessibility == Some(Accessibility::Private)
                                        {
                                            return Err(Error::AssignFailedDueToAccessibility { span });
                                        }
                                    }

                                    // Allow assigning undefined to optional properties.
                                    (|| {
                                        if opts.for_castablity {
                                            if lp.optional {
                                                if let Some(r_ty) = &rp.type_ann {
                                                    if r_ty.is_undefined() {
                                                        return Ok(());
                                                    }
                                                }
                                            }

                                            if rp.optional {
                                                if let Some(lt) = &lp.type_ann {
                                                    if lt.is_undefined() {
                                                        return Ok(());
                                                    }
                                                }
                                            }
                                        }

                                        self.assign_inner(
                                            data,
                                            lp.type_ann.as_deref().unwrap_or(&Type::any(span)),
                                            rp.type_ann.as_deref().unwrap_or(&Type::any(span)),
                                            opts,
                                        )
                                    })()?;

                                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                        unhandled_rhs.remove(pos);
                                    }
                                    return Ok(());
                                }
                                TypeElement::Method(rm) => {
                                    if let Some(lp_ty) = &lp.type_ann {
                                        if let Type::Function(lp_ty) = lp_ty.normalize() {
                                            self.assign_params(data, opts, &lp_ty.params, &rm.params).context(
                                                "tried to assign parameters of a method property to the parameters of \
                                                 a property with callable type",
                                            )?;

                                            if let Some(r_ret_ty) = &rm.ret_ty {
                                                self.assign_with_opts(data, opts, &lp_ty.ret_ty, &r_ret_ty).context(
                                                    "tried to assign return type of a method property to the return \
                                                     type of a property with callable type",
                                                )?;
                                            }
                                        }
                                    }

                                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                        unhandled_rhs.remove(pos);
                                    }
                                    return Ok(());
                                }
                                _ => {}
                            },

                            // `foo(a: string) is assignable to foo(a: any)`
                            TypeElement::Method(ref lm) => match rm {
                                TypeElement::Method(ref rm) => {
                                    //

                                    self.assign_to_fn_like(
                                        data,
                                        opts,
                                        lm.type_params.as_ref(),
                                        &lm.params,
                                        lm.ret_ty.as_deref(),
                                        rm.type_params.as_ref(),
                                        &rm.params,
                                        rm.ret_ty.as_deref(),
                                    )
                                    .context("tried to assign to callable type element")?;
                                    // TODO: Return type

                                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                        unhandled_rhs.remove(pos);
                                    }

                                    return Ok(());
                                }

                                TypeElement::Property(rp) => {
                                    // Allow assigning property with callable type to methods.
                                    if let Some(rp_ty) = &rp.type_ann {
                                        if let Type::Function(rf) = rp_ty.normalize() {
                                            self.assign_to_fn_like(
                                                data,
                                                opts,
                                                lm.type_params.as_ref(),
                                                &lm.params,
                                                lm.ret_ty.as_deref(),
                                                rf.type_params.as_ref(),
                                                &rf.params,
                                                Some(&rf.ret_ty),
                                            )
                                            .context(
                                                "tried to assign a property with callable type to a method property",
                                            )?;
                                        }
                                    }
                                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                        unhandled_rhs.remove(pos);
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
                TypeElement::Index(li) => {
                    // TODO: Verify
                    for rm in rhs_members {
                        match rm {
                            TypeElement::Call(_) | TypeElement::Constructor(_) => continue,

                            TypeElement::Property(r_prop) => {
                                if let Ok(()) = self.assign(data, &li.params[0].ty, &r_prop.key.ty(), span) {
                                    if let Some(l_index_ret_ty) = &li.type_ann {
                                        if let Some(r_prop_ty) = &r_prop.type_ann {
                                            self.assign_with_opts(data, opts, &l_index_ret_ty, &&r_prop_ty)
                                                .context(
                                                    "tried to assign a type of property to thr type of an index \
                                                     signature",
                                                )?;
                                        }
                                    }
                                }
                            }

                            TypeElement::Method(_) => {
                                slog::error!(self.logger, "unimplemented: Index = Method");
                            }
                            TypeElement::Index(ri) => {
                                if li.params.type_eq(&ri.params) {
                                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == ri.span()) {
                                        unhandled_rhs.remove(pos);
                                    }

                                    if let Some(lt) = &li.type_ann {
                                        if let Some(rt) = &ri.type_ann {
                                            return self.assign_with_opts(data, opts, &lt, &rt);
                                        }
                                    }
                                }

                                slog::error!(self.logger, "unimplemented: error reporting for Index = Index");
                            }
                        }
                    }
                }
                TypeElement::Call(lc) => {
                    //
                    for rm in rhs_members {
                        match rm {
                            // TODO: Check type of parameters
                            // TODO: Check return type
                            TypeElement::Call(rc) => {
                                if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                    unhandled_rhs.remove(pos);
                                }
                                done = true;

                                let res = self.assign_to_fn_like(
                                    data,
                                    AssignOpts {
                                        infer_type_params_of_left: true,
                                        ..opts
                                    },
                                    lc.type_params.as_ref(),
                                    &lc.params,
                                    lc.ret_ty.as_deref(),
                                    rc.type_params.as_ref(),
                                    &rc.params,
                                    rc.ret_ty.as_deref(),
                                );

                                match res {
                                    Ok(()) => return Ok(()),
                                    Err(err) => {
                                        errors.push(err);
                                    }
                                }

                                continue;
                            }
                            _ => {}
                        }
                    }

                    missing_fields.push(lm.clone());
                }
                _ => {}
            }
        }

        if done {
            if errors.is_empty() {
                return Ok(());
            } else {
                return Err(Error::ObjectAssignFailed { span, errors });
            }
        }

        unhandled_rhs.clear();

        Ok(())
    }

    pub(super) fn assign_class_members_to_type_element(
        &mut self,
        data: &mut AssignData,
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
                            if self.assign(data, &lp.key.ty(), &rp.key.ty(), opts.span).is_ok() {
                                if let Some(lt) = &lp.type_ann {
                                    if let Some(rt) = &rp.value {
                                        self.assign(data, &lt, &rt, opts.span)?;
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

            TypeElement::Index(li) => {
                for rhs_member in rhs_members {
                    match rhs_member {
                        ClassMember::IndexSignature(ri) => {
                            if ri.params.type_eq(&li.params) {
                                if let Some(lt) = &li.type_ann {
                                    if let Some(rt) = &ri.type_ann {
                                        return self.assign_with_opts(data, opts, &lt, &rt);
                                    }
                                }
                            }
                        }

                        _ => continue,
                    }
                }
            }

            TypeElement::Call(_) | TypeElement::Constructor(_) | TypeElement::Method(_) => {}
        }

        Ok(())
    }
}
