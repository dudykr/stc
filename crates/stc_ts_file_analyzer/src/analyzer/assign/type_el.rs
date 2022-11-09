use std::borrow::Cow;

use itertools::Itertools;
use rnode::NodeId;
use stc_ts_ast_rnode::{RIdent, RTsEntityName, RTsLit};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt, Error, Errors};
use stc_ts_type_ops::Fix;
use stc_ts_types::{
    Array, Class, ClassDef, ClassMember, Function, Key, KeywordType, LitType, MethodSignature, ModuleId, Operator, PropertySignature, Ref,
    TplType, Tuple, Type, TypeElement, TypeLit, TypeLitMetadata, TypeParamInstantiation, Union, UnionMetadata,
};
use stc_utils::{cache::Freeze, ext::SpanExt};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::{Accessibility, TsKeywordTypeKind, TsTypeOperatorOp};

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        types::NormalizeTypeOpts,
        util::ResultExt,
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    /// This method is called when lhs of assignment is interface or type
    /// literal.
    ///
    /// ```js
    /// interface A {}
    /// let a: A = foo;
    /// let b: { key: string } = foo;
    /// ```
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn assign_to_type_elements(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        lhs_span: Span,
        lhs: &[TypeElement],
        rhs: &Type,
        lhs_metadata: TypeLitMetadata,
    ) -> VResult<()> {
        let span = opts.span.with_ctxt(SyntaxContext::empty());
        // debug_assert!(!span.is_dummy());

        let mut errors = vec![];
        let mut missing_fields = vec![];

        let numeric_keyed_ty = lhs
            .iter()
            .filter_map(|e| match e {
                TypeElement::Index(ref i) if i.params.len() == 1 && i.params[0].ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword) => {
                    Some(i.type_ann.as_ref())
                }

                _ => None,
            })
            .next();

        if let Some(numeric_keyed_ty) = numeric_keyed_ty {
            let any = box Type::any(span, Default::default());
            let numeric_keyed_ty = numeric_keyed_ty.unwrap_or(&any);

            match *rhs.normalize() {
                Type::Array(Array { ref elem_type, .. }) => return self.assign_inner(data, numeric_keyed_ty, elem_type, opts),

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
                    type_name: RTsEntityName::Ident(RIdent {
                        sym: js_word!("Function"), ..
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

                    // Exclude duplicate properties on rhs
                    let valid_rhs_indexes = {
                        let mut v = vec![];
                        let mut used_keys: Vec<Key> = vec![];

                        for (index, r) in rhs_members.iter().enumerate().rev() {
                            match r {
                                TypeElement::Property(p @ PropertySignature { optional: false, .. }) => {
                                    if used_keys.iter().any(|prev| prev.type_eq(&p.key)) {
                                        continue;
                                    }

                                    used_keys.push(p.key.clone());
                                }
                                TypeElement::Property(PropertySignature { optional: true, .. })
                                | TypeElement::Method(MethodSignature { optional: true, .. }) => {
                                    // TODO: Skip this while not creaitng
                                    // `MissingProperties`
                                }
                                _ => {}
                            }

                            v.push(index);
                        }

                        v
                    };

                    let rhs_members = rhs_members
                        .into_iter()
                        .enumerate()
                        .filter(|(index, _)| valid_rhs_indexes.contains(index))
                        .map(|(_, v)| v.clone())
                        .collect::<Vec<_>>();

                    if !allow_unknown_rhs {
                        let mut done = vec![];

                        for r in &rhs_members {
                            // optional members do not have effect.
                            match r {
                                TypeElement::Property(PropertySignature { optional: true, .. })
                                | TypeElement::Method(MethodSignature { optional: true, .. }) => continue,
                                _ => {}
                            }

                            if let Some(key) = r.key() {
                                if done.iter().any(|prev: &Key| prev.type_eq(key)) {
                                    continue;
                                }

                                done.push(key.clone());
                            }

                            unhandled_rhs.push(r.span());
                        }
                    }

                    self.handle_assignment_of_type_elements_to_type_elements(
                        data,
                        AssignOpts { allow_unknown_rhs, ..opts },
                        &mut missing_fields,
                        &mut unhandled_rhs,
                        lhs,
                        lhs_metadata,
                        &rhs_members,
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
                        .convert_type_to_type_lit(span, Cow::Borrowed(rhs))?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                    {
                        return self
                            .assign_to_type_elements(data, opts, lhs_span, lhs, &rty, lhs_metadata)
                            .context("tried to assign to type elements by converting rhs to a type literal");
                    }

                    return Err(Error::SimpleAssignFailed { span, cause: None });
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
                        return Err(Error::SimpleAssignFailed { span, cause: None });
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
                                metadata: Default::default(),
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
                                    metadata: UnionMetadata {
                                        common: r_tuple.metadata.common,
                                        ..Default::default()
                                    },
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
                                    metadata: Default::default(),
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
                                .convert_type_to_type_lit(span, Cow::Borrowed(rhs))?
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

                    // TODO(kdy1): Check for literal properties

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
                        .convert_type_to_type_lit(span, Cow::Borrowed(rhs))
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
                            Error::Errors { span, .. } => Error::SimpleAssignFailed {
                                span,
                                cause: Some(box err),
                            },
                            Error::MissingFields { span, .. } => Error::SimpleAssignFailed {
                                span,
                                cause: Some(box err),
                            },
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
                    // TODO(kdy1): Check if constructor exists.
                    if rhs_cls.def.is_abstract {
                        return Err(Error::CannotAssignAbstractConstructorToNonAbstractConstructor { span });
                    }

                    // TODO(kdy1): Optimize
                    // for el in lhs {
                    //     self.assign_class_members_to_type_element(opts, el, &rhs.body)?;
                    // }

                    let rhs = self
                        .convert_type_to_type_lit(span, Cow::Borrowed(rhs))
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

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Lit(LitType {
                    lit: RTsLit::Number(..), ..
                })
                | Type::Lit(LitType {
                    lit: RTsLit::BigInt(..), ..
                })
                | Type::Lit(LitType { lit: RTsLit::Str(..), .. })
                | Type::Lit(LitType { lit: RTsLit::Bool(..), .. })
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
                    let mut rhs = self
                        .convert_type_to_type_lit(span, Cow::Borrowed(rhs))
                        .context("tried to convert a function to a type literal for asssignment")?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                        .unwrap();
                    rhs.make_clone_cheap();

                    return self
                        .assign_to_type_elements(data, opts, lhs_span, lhs, &rhs, lhs_metadata)
                        .with_context(|| {
                            format!(
                                "tried to assign the converted type to type elements:\nRHS={}",
                                dump_type_as_string(&self.cm, &rhs)
                            )
                        });
                }

                Type::Keyword(KeywordType {
                    kind: kind @ TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: kind @ TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: kind @ TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
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
                        metadata: Default::default(),
                    });

                    let rhs = self.normalize(Some(span), Cow::Owned(rhs), Default::default())?;

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
                        .map_err(|err| {
                            err.convert_all(|err| match err {
                                Error::MissingFields { .. } => Error::SimpleAssignFailed {
                                    span: err.span(),
                                    cause: Some(box err),
                                },
                                _ => err,
                            })
                        })
                        .with_context(|| {
                            format!(
                                "tried to assign a keyword as builtin to type elements\nRHS = {}",
                                dump_type_as_string(&self.cm, &rhs)
                            )
                        });
                }

                Type::Lit(LitType {
                    lit: lit @ RTsLit::Number(..),
                    ..
                })
                | Type::Lit(LitType {
                    lit: lit @ RTsLit::Str(..),
                    ..
                })
                | Type::Lit(LitType {
                    lit: lit @ RTsLit::Bool(..),
                    ..
                })
                | Type::Lit(LitType {
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
                            &Type::Keyword(KeywordType {
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
                                metadata: Default::default(),
                            }),
                            lhs_metadata,
                        )
                        .context("tried to assign a literal as keyword to type elements");
                }

                Type::Param(..)
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                }) => return Err(Error::SimpleAssignFailed { span, cause: None }),

                // TODO(kdy1): Strict mode
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => return Ok(()),

                // TODO(kdy1): Strict mode
                Type::Keyword(KeywordType {
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
                                    if let Ok(()) = self.assign_with_opts(data, opts, &l_index.params[0].ty, &&r_constraint) {
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

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsObjectKeyword,
                    ..
                }) => {
                    if lhs.is_empty() {
                        return Ok(());
                    } else {
                        let err = Error::MissingFields {
                            span,
                            fields: lhs.to_vec(),
                        }
                        .context("keyword `object` is not assignable to a non-empty type literal");
                        return Err(Error::Errors { span, errors: vec![err] });
                    }
                }

                Type::EnumVariant(..) => return Err(Error::SimpleAssignFailed { span, cause: None }),

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
                        .convert_err(|err| Error::SimpleAssignFailed {
                            span: err.span(),
                            cause: Some(box err),
                        })
                        .context("failed to normalize")?;

                    if rhs.is_keyword() {
                        return Err(Error::SimpleAssignFailed { span, cause: None }.context("failed to assign builtin type of a keyword"));
                    }

                    return self
                        .assign_to_type_elements(data, opts, lhs_span, lhs, &rhs, lhs_metadata)
                        .context("tried to assign using expanded builtin type");
                }

                Type::Tpl(TplType { span: rhs_span, .. }) => {
                    if lhs.len() == 0 {
                        return Ok(());
                    }

                    return self.assign_to_type_elements(
                        data,
                        opts,
                        lhs_span,
                        lhs,
                        &Type::Keyword(KeywordType {
                            span: *rhs_span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            metadata: Default::default(),
                        }),
                        lhs_metadata,
                    );
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
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
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

    pub(super) fn try_assign_using_parent(&mut self, data: &mut AssignData, l: &Type, r: &Type, opts: AssignOpts) -> Option<VResult<()>> {
        let span = opts.span;

        match r.normalize() {
            Type::Interface(ri) => {
                let res: VResult<_> = try {
                    for parent in &ri.extends {
                        let parent = self.type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, parent.type_args.as_deref())?;

                        // An interface can extend a class.
                        let parent = self.instantiate_class(span, &parent)?;

                        let res = self.assign_with_opts(
                            data,
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            &l,
                            &parent,
                        );
                        if res.is_ok() {
                            return Some(Ok(()));
                        }
                    }

                    return None;
                };

                Some(res)
            }
            _ => None,
        }
    }

    fn handle_assignment_of_type_elements_to_type_elements(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        missing_fields: &mut Vec<TypeElement>,
        unhandled_rhs: &mut Vec<Span>,
        lhs: &[TypeElement],
        lhs_metadata: TypeLitMetadata,
        rhs: &[TypeElement],
    ) -> VResult<()> {
        let span = opts.span;

        let mut errors = vec![];

        for (i, m) in lhs.into_iter().enumerate().filter(|(_, m)| m.key().is_some()) {
            let res = self
                .assign_type_elements_to_type_element(data, opts, missing_fields, unhandled_rhs, &[m], lhs_metadata, rhs)
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

        let lhs_index = lhs.iter().filter(|m| matches!(m, TypeElement::Index(_))).collect_vec();
        let lhs_call = lhs.iter().filter(|m| matches!(m, TypeElement::Call(_))).collect_vec();
        let lhs_constructor = lhs.iter().filter(|m| matches!(m, TypeElement::Constructor(_))).collect_vec();

        if !lhs_index.is_empty() {
            let res = self
                .assign_type_elements_to_type_element(data, opts, missing_fields, unhandled_rhs, &lhs_index, lhs_metadata, rhs)
                .with_context(|| format!("tried to assign to an element (not a key-based)"));

            errors.extend(res.err());
        }

        if !lhs_call.is_empty() {
            let res = self
                .assign_type_elements_to_type_element(data, opts, missing_fields, unhandled_rhs, &lhs_call, lhs_metadata, rhs)
                .with_context(|| format!("tried to assign to an element (not a key-based)"));

            errors.extend(res.err());
        }

        if !lhs_constructor.is_empty() {
            let res = self
                .assign_type_elements_to_type_element(data, opts, missing_fields, unhandled_rhs, &lhs_constructor, lhs_metadata, rhs)
                .with_context(|| format!("tried to assign to an element (not a key-based)"));

            errors.extend(res.err());
        }

        if !errors.is_empty() {
            return Err(Error::Errors { span, errors });
        }

        Ok(())
    }

    /// This method assigns each property to corresponding property.
    ///
    /// Because of overloads, this methods takes `&[TypeElement]` instead of
    /// [TypeElement] for lhs.
    ///
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
        lms: &[&TypeElement],
        lhs_metadata: TypeLitMetadata,
        rhs_members: &[TypeElement],
    ) -> VResult<()> {
        debug_assert!(!lms.is_empty());

        let span = opts.span.with_ctxt(SyntaxContext::empty());
        // We need this to show error if not all of rhs_member is matched

        let missing_field_start_idx = missing_fields.len();

        let mut errors = vec![];
        let mut done = false;

        for lm in lms.iter().copied() {
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

                                        if !opts.for_castablity {
                                            if !lp.optional && rp.optional {
                                                return Err(Error::AssignFailedDueToOptionalityDifference { span });
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
                                                lp.type_ann.as_deref().unwrap_or(&Type::any(span, Default::default())),
                                                rp.type_ann.as_deref().unwrap_or(&Type::any(span, Default::default())),
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
                                                self.assign_to_fn_like(
                                                    data,
                                                    opts,
                                                    true,
                                                    lp_ty.type_params.as_ref(),
                                                    &lp_ty.params,
                                                    Some(&lp_ty.ret_ty),
                                                    rm.type_params.as_ref(),
                                                    &rm.params,
                                                    rm.ret_ty.as_deref(),
                                                )
                                                .context("tried to assign a method signature to a property signature with function type")?;
                                            } else {
                                                self.assign_with_opts(
                                                    data,
                                                    opts,
                                                    &lp_ty,
                                                    &Type::Function(Function {
                                                        span,
                                                        type_params: rm.type_params.clone(),
                                                        params: rm.params.clone(),
                                                        ret_ty: rm.ret_ty.clone().unwrap_or_else(|| {
                                                            box Type::any(span.with_ctxt(SyntaxContext::empty()), Default::default())
                                                        }),
                                                        metadata: Default::default(),
                                                    }),
                                                )
                                                .context(
                                                    "failed to assign a method signature to a property signature because the property was \
                                                     not a function",
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

                                // `foo(a: string) is assignable to foo(a: any)`
                                TypeElement::Method(ref lm) => match rm {
                                    TypeElement::Method(ref rm) => {
                                        let res = self
                                            .assign_to_fn_like(
                                                data,
                                                AssignOpts {
                                                    is_params_of_method_definition: true,
                                                    ..opts
                                                },
                                                true,
                                                lm.type_params.as_ref(),
                                                &lm.params,
                                                lm.ret_ty.as_deref(),
                                                rm.type_params.as_ref(),
                                                &rm.params,
                                                rm.ret_ty.as_deref(),
                                            )
                                            .context("tried to assign to callable type element");
                                        // TODO(kdy1): Return type

                                        match res {
                                            Ok(()) => {
                                                if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                                    unhandled_rhs.remove(pos);
                                                }

                                                return Ok(());
                                            }
                                            Err(err) => {
                                                errors.push(err);
                                                done = true
                                            }
                                        }
                                    }

                                    TypeElement::Property(rp) => {
                                        // Allow assigning property with callable type to methods.
                                        if let Some(rp_ty) = &rp.type_ann {
                                            if let Type::Function(rf) = rp_ty.normalize() {
                                                self.assign_to_fn_like(
                                                    data,
                                                    opts,
                                                    true,
                                                    lm.type_params.as_ref(),
                                                    &lm.params,
                                                    lm.ret_ty.as_deref(),
                                                    rf.type_params.as_ref(),
                                                    &rf.params,
                                                    Some(&rf.ret_ty),
                                                )
                                                .context("tried to assign a property with callable type to a method property")?;
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

                    TypeElement::Method(MethodSignature {
                        key: Key::Normal { sym, .. },
                        ..
                    }) if &**sym == "toString" => {}

                    _ => {
                        if !opts.allow_missing_fields {
                            // No property with `key` found.
                            missing_fields.push(lm.clone());
                        }
                    }
                }
            } else if !opts.skip_call_and_constructor_elem {
                match lm {
                    // TODO(kdy1): Check type of the index.
                    TypeElement::Index(li) => {
                        unhandled_rhs.clear();
                        // TODO(kdy1): Verify
                        for rm in rhs_members {
                            match rm {
                                TypeElement::Call(_) | TypeElement::Constructor(_) => continue,

                                TypeElement::Property(r_prop) => {
                                    done = true;

                                    if self
                                        .assign(span, &mut Default::default(), &li.params[0].ty, &r_prop.key.ty())
                                        .is_ok()
                                        || li.params[0].ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                                    {
                                        if let Some(l_index_ret_ty) = &li.type_ann {
                                            if let Some(r_prop_ty) = &r_prop.type_ann {
                                                self.assign_with_opts(data, opts, &l_index_ret_ty, &&r_prop_ty)
                                                    .context("tried to assign a type of property to thr type of an index signature")?;
                                            }
                                        }

                                        if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                            unhandled_rhs.remove(pos);
                                        }
                                    }
                                }

                                TypeElement::Method(rm) => {
                                    done = true;

                                    if self.assign(span, &mut Default::default(), &li.params[0].ty, &rm.key.ty()).is_ok()
                                        || li.params[0].ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                                    {
                                        if let Some(li_ret) = &li.type_ann {
                                            self.assign_with_opts(
                                                data,
                                                AssignOpts {
                                                    allow_assignment_to_param: false,
                                                    ..opts
                                                },
                                                &li_ret,
                                                &Type::Function(Function {
                                                    span: rm.span,
                                                    type_params: rm.type_params.clone(),
                                                    params: rm.params.clone(),
                                                    ret_ty: rm.ret_ty.clone().unwrap_or_else(|| {
                                                        box Type::any(rm.span.with_ctxt(SyntaxContext::empty()), Default::default())
                                                    }),
                                                    metadata: Default::default(),
                                                }),
                                            )
                                            .context("tried to assign a method to an index signature")?;
                                        }
                                    }

                                    if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                        unhandled_rhs.remove(pos);
                                    }
                                }

                                TypeElement::Index(ri) => {
                                    done = true;

                                    if li.params.type_eq(&ri.params) || ri.params[0].ty.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
                                        if let Some(pos) = unhandled_rhs.iter().position(|span| *span == ri.span()) {
                                            unhandled_rhs.remove(pos);
                                        }

                                        if let Some(lt) = &li.type_ann {
                                            if let Some(rt) = &ri.type_ann {
                                                self.assign_with_opts(data, opts, &lt, &rt)?;
                                            }
                                        }

                                        return Ok(());
                                    }

                                    errors.push(
                                        Error::SimpleAssignFailed { span, cause: None }.context("failed to assign to an index signature"),
                                    );
                                }
                            }
                        }
                    }
                    TypeElement::Call(lc) => {
                        //
                        for (ri, rm) in rhs_members.iter().enumerate() {
                            match rm {
                                TypeElement::Call(rc) => {
                                    for rm in rhs_members.iter().filter(|rm| matches!(rm, TypeElement::Call(_))) {
                                        if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                            unhandled_rhs.remove(pos);
                                            continue;
                                        }
                                    }

                                    done = true;

                                    let res = self
                                        .assign_to_fn_like(
                                            data,
                                            AssignOpts {
                                                infer_type_params_of_left: true,
                                                ..opts
                                            },
                                            true,
                                            lc.type_params.as_ref(),
                                            &lc.params,
                                            lc.ret_ty.as_deref(),
                                            rc.type_params.as_ref(),
                                            &rc.params,
                                            rc.ret_ty.as_deref(),
                                        )
                                        .with_context(|| format!("tried to assign {}th element to a call signature", ri));

                                    match res {
                                        Ok(()) => {
                                            missing_fields.drain(missing_field_start_idx..);
                                            return Ok(());
                                        }
                                        Err(err) => {
                                            errors.push(err);
                                        }
                                    }

                                    continue;
                                }
                                _ => {}
                            }
                        }

                        if !opts.allow_missing_fields {
                            missing_fields.push(lm.clone());
                        }
                    }

                    TypeElement::Constructor(lc) => {
                        //
                        for rm in rhs_members {
                            match rm {
                                TypeElement::Constructor(rc) => {
                                    for rm in rhs_members.iter().filter(|rm| matches!(rm, TypeElement::Constructor(_))) {
                                        if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                            unhandled_rhs.remove(pos);
                                            continue;
                                        }
                                    }

                                    done = true;

                                    let res = self.assign_to_fn_like(
                                        data,
                                        AssignOpts {
                                            infer_type_params_of_left: true,
                                            ..opts
                                        },
                                        false,
                                        lc.type_params.as_ref(),
                                        &lc.params,
                                        lc.ret_ty.as_deref(),
                                        rc.type_params.as_ref(),
                                        &rc.params,
                                        rc.ret_ty.as_deref(),
                                    );

                                    match res {
                                        Ok(()) => {
                                            missing_fields.drain(missing_field_start_idx..);

                                            for rm in rhs_members {
                                                match rm {
                                                    TypeElement::Constructor(..) => {
                                                        if let Some(pos) = unhandled_rhs.iter().position(|span| *span == rm.span()) {
                                                            unhandled_rhs.remove(pos);
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }

                                            return Ok(());
                                        }
                                        Err(err) => {
                                            errors.push(err);
                                        }
                                    }

                                    continue;
                                }
                                _ => {}
                            }
                        }

                        if !opts.is_assigning_to_class_members {
                            return Err(Error::SimpleAssignFailed { span, cause: None }.context("failed to assign to a constructor"));
                        }
                    }

                    _ => {}
                }
            }
        }

        if done {
            if errors.is_empty() {
                return Ok(());
            }
        }

        if !errors.is_empty() {
            return Err(Error::ObjectAssignFailed { span, errors });
        }

        unhandled_rhs.clear();

        Ok(())
    }
}
