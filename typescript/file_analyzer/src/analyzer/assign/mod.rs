use super::Analyzer;
use crate::{
    analyzer::util::ResultExt,
    ty::{self, TypeExt},
    ValidationResult,
};
use rnode::NodeId;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_types::Key;
use stc_ts_types::Mapped;
use stc_ts_types::MethodSignature;
use stc_ts_types::PropertySignature;
use stc_ts_types::Ref;
use stc_ts_types::{
    Array, ClassInstance, EnumVariant, FnParam, Interface, Intersection, Tuple, Type, TypeElement, TypeLit, TypeParam,
    Union,
};
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

mod class;
mod query;
mod type_el;

/// Context used for `=` assignments.
#[derive(Clone, Copy)]
pub(crate) struct AssignOpts {
    pub span: Span,
    pub allow_unknown_rhs: bool,
}

impl Analyzer<'_, '_> {
    pub(crate) fn assign_with_op(&mut self, span: Span, op: AssignOp, lhs: &Type, rhs: &Type) -> ValidationResult<()> {
        debug_assert_ne!(op, op!("="));

        let l = self.expand_top_ref(span, Cow::Borrowed(lhs))?;
        let r = self.expand_top_ref(span, Cow::Borrowed(rhs))?;

        let lhs = l.normalize();
        let rhs = r.normalize();

        // Trivial
        if lhs.is_any() || rhs.is_any() {
            return Ok(());
        }

        // Addition to a string converts rhs into stirng.
        if lhs.is_str() {
            return Ok(());
        }

        if lhs.is_num() || lhs.is_enum_variant() || lhs.is_enum_type() {
            // TODO: Check if actual value is number.

            if rhs.is_num() {
                return Ok(());
            }

            if rhs.is_enum_variant() {
                // TODO: Check if actual value is numberx.
                return Ok(());
            }

            if rhs.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                || rhs.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                || rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
            {
                return Err(Error::AssignOpCannotBeApplied { span, op });
            }
        }

        match lhs {
            Type::TypeLit(lhs) => {
                if lhs.members.is_empty() {
                    if rhs.is_str() {
                        return Ok(());
                    }
                }
            }
            _ => {}
        }

        match op {
            op!("+=") => {}
            _ => {}
        }

        Err(Error::InvalidOpAssign {
            span,
            op,
            lhs: l.into_owned().clone(),
            rhs: r.into_owned().clone(),
        })
    }

    /// TODO: Change argument order. (Span should come first).
    pub(crate) fn assign(&mut self, left: &Type, right: &Type, span: Span) -> ValidationResult<()> {
        self.assign_with_opts(
            AssignOpts {
                span,
                allow_unknown_rhs: false,
            },
            left,
            right,
        )
    }

    pub(crate) fn assign_with_opts(&mut self, opts: AssignOpts, left: &Type, right: &Type) -> ValidationResult<()> {
        if self.is_builtin {
            return Ok(());
        }

        // if cfg!(debug_assertions) && span.is_dummy() {
        //     print_backtrace();
        //     debug_assert!(!span.is_dummy());
        // }

        // self.verify_before_assign("lhs", left);
        // self.verify_before_assign("rhs", right);

        let res = self.assign_inner(left, right, opts);
        match res {
            Err(Error::Errors { errors, .. }) if errors.is_empty() => return Ok(()),
            _ => {}
        }

        res.map_err(|err| match err {
            Error::AssignFailed { .. }
            | Error::DebugContext { .. }
            | Error::Errors { .. }
            | Error::Unimplemented { .. } => err,
            _ => Error::AssignFailed {
                span: opts.span,
                left: box left.clone(),
                right: box right.clone(),
                cause: vec![err],
            },
        })
    }

    /// Verifies that `ty` is
    ///
    /// - Not a reference
    /// - Not a type parameter declared on child scope.
    fn verify_before_assign(&self, ctx: &'static str, ty: &Type) {
        match ty.normalize() {
            Type::Ref(ref r) => {
                print_backtrace();
                panic!(
                    "A reference type (in {}) should be expanded before calling .assign()\n{:?}",
                    ctx, r,
                )
            }

            _ => {}
        }
    }

    fn normalize_for_assign<'a>(&mut self, ty: &'a Type) -> Cow<'a, Type> {
        let ty = ty.normalize();

        match ty {
            Type::Ref(Ref {
                span,
                type_name: RTsEntityName::Ident(type_name),
                type_args: None,
                ..
            }) => {
                // TODO: Check if ref points global.
                return Cow::Owned(Type::Keyword(RTsKeywordType {
                    span: *span,
                    kind: match type_name.sym {
                        js_word!("Boolean") => TsKeywordTypeKind::TsBooleanKeyword,
                        js_word!("Number") => TsKeywordTypeKind::TsNumberKeyword,
                        js_word!("String") => TsKeywordTypeKind::TsStringKeyword,
                        _ => return Cow::Borrowed(ty),
                    },
                }));
            }
            _ => {}
        }

        Cow::Borrowed(ty)
    }

    fn assign_inner(&mut self, to: &Type, rhs: &Type, opts: AssignOpts) -> Result<(), Error> {
        self.assign_without_wrapping(to, rhs, opts).with_context(|| {
            //
            let lhs = dump_type_as_string(&self.cm, &to);
            let rhs = dump_type_as_string(&self.cm, &rhs);

            format!("lhs = {}rhs = {}", lhs, rhs)
        })
    }

    /// Assigns, but does not wrap error with [Error::AssignFailed].
    fn assign_without_wrapping(&mut self, to: &Type, rhs: &Type, opts: AssignOpts) -> Result<(), Error> {
        let span = opts.span;

        if !self.is_builtin && span.is_dummy() {
            panic!("cannot assign with dummy span")
        }

        // debug_assert!(!span.is_dummy(), "\n\t{:?}\n<-\n\t{:?}", to, rhs);
        let to = self.normalize_for_assign(to);
        let rhs = self.normalize_for_assign(rhs);

        let to = to.normalize();
        let rhs = rhs.normalize();

        macro_rules! fail {
            () => {{
                return Err(Error::AssignFailed {
                    span,
                    left: box to.clone(),
                    right: box rhs.clone(),
                    cause: vec![],
                });
            }};
        }

        macro_rules! handle_enum_in_rhs {
            ($e:expr) => {{
                let e = $e;

                // Allow
                //      let e: E = E.a
                //
                // and
                //
                //      let e1: E = E.a
                //      let e2: E = e1
                match *to {
                    Type::Enum(ref left_enum) => {
                        if left_enum.id.sym == *e.id.sym {
                            return Ok(());
                        }
                        fail!()
                    }
                    _ => {}
                }

                if !e.has_str && !e.has_num {
                    return self.assign_inner(
                        to,
                        &Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        }),
                        opts,
                    );
                }

                if !e.has_num {
                    return self.assign_inner(
                        to,
                        &Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }),
                        opts,
                    );
                }

                if !e.has_str {
                    return self.assign_inner(
                        to,
                        &Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        }),
                        opts,
                    );
                }

                return self.assign_inner(
                    to,
                    &Type::union(vec![
                        box Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        }),
                        box Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }),
                    ]),
                    opts,
                );
            }};
        }

        if to.type_eq(rhs) {
            return Ok(());
        }

        match to {
            Type::Ref(Ref {
                type_name:
                    RTsEntityName::Ident(RIdent {
                        sym: js_word!("Symbol"),
                        ..
                    }),
                ..
            }) => {
                if rhs.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                    return Ok(());
                }
            }

            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(type_name),
                type_args,
                ..
            }) if type_name.sym == *"ReadonlyArray" => match type_args {
                Some(type_args) => {
                    if type_args.params.len() == 1 {
                        match rhs {
                            Type::Array(Array { elem_type, .. }) => {
                                return self
                                    .assign_inner(&type_args.params[0], elem_type, opts)
                                    .context("tried to assign an array to a readonly array (builtin)");
                            }
                            _ => {}
                        }
                    }
                }
                None => {}
            },

            // Str contains `kind`, and it's not handled properly by type_eq.
            Type::Lit(RTsLitType {
                lit: RTsLit::Str(to), ..
            }) => match rhs {
                Type::Lit(RTsLitType {
                    lit: RTsLit::Str(rhs), ..
                }) => {
                    if to.value == rhs.value {
                        return Ok(());
                    } else {
                        fail!()
                    }
                }
                _ => {}
            },

            Type::Ref(left) => {
                match rhs {
                    Type::Ref(right) => {
                        // We need this as type may recurse, and thus cannot be handled by expander.
                        if left.type_name.type_eq(&right.type_name) && left.type_args.type_eq(&right.type_args) {
                            return Ok(());
                        }
                    }
                    _ => {}
                }

                let to = self.expand_top_ref(span, Cow::Borrowed(to))?;
                return self.assign_inner(&to, rhs, opts);
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            })
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => return Ok(()),

            _ => {}
        }

        // Allow v = null and v = undefined if strict null check is false
        if !self.rule().strict_null_checks {
            match rhs {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => return Ok(()),
                _ => {}
            }
        }

        {
            // Handle special cases.
            // Assigning boolean to Boolean is ok, but assigning Boolean to boolean is an
            // error.
            let special_cases = &[
                (TsKeywordTypeKind::TsBooleanKeyword, "Boolean"),
                (TsKeywordTypeKind::TsStringKeyword, "String"),
                (TsKeywordTypeKind::TsNumberKeyword, "Number"),
            ];

            for (kwd, interface) in special_cases {
                let rhs = rhs.clone().generalize_lit();
                match to {
                    Type::Keyword(k) if k.kind == *kwd => match *rhs {
                        Type::Interface(ref i) => {
                            if i.name.as_str() == *interface {
                                return Err(Error::AssignedWrapperToPrimitive { span });
                            }
                        }
                        _ => {}
                    },
                    Type::Interface(ref i) if i.name.as_str() == *interface => match *rhs {
                        Type::Keyword(ref k) if k.kind == *kwd => return Ok(()),
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        match *to {
            // let a: any = 'foo'
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => fail!(),

            // Anything is assignable to unknown
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Ok(()),

            // Everything is assignable to Object
            Type::Interface(ref i) if i.name.as_str() == "Object" => return Ok(()),

            Type::Module(..) => {
                dbg!();
                return Err(Error::InvalidLValue { span: to.span() });
            }
            Type::Enum(ref e) => {
                match rhs.normalize() {
                    Type::Lit(RTsLitType {
                        lit: RTsLit::Number(..),
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => {
                        // validEnumAssignments.ts insists that this is valid.
                        return Ok(());
                    }
                    Type::EnumVariant(rhs) => {
                        if rhs.enum_name == e.id {
                            return Ok(());
                        }
                        fail!()
                    }
                    _ => {}
                }
            }
            Type::EnumVariant(ref e) => {
                dbg!();
                return Err(Error::InvalidLValue { span: e.span });
            }

            Type::Intersection(ref i) => {
                let mut errors = Errors::default();

                for ty in &i.types {
                    match self.assign_inner(&ty, rhs, opts) {
                        Ok(..) => {}
                        Err(err) => errors.push(err),
                    }
                }

                if errors.is_empty() {
                    return Ok(());
                }

                return Err(Error::Errors {
                    span,
                    errors: errors.into(),
                });
            }

            Type::Class(ref l) => match rhs.normalize() {
                Type::ClassInstance(r) => return self.assign_to_class(opts, l, &r.ty),
                Type::Interface(..) | Type::TypeLit(..) | Type::Lit(..) | Type::Class(..) => {
                    return self.assign_to_class(opts, l, rhs.normalize())
                }
                _ => {}
            },

            Type::Lit(ref lhs) => match rhs.normalize() {
                Type::Lit(rhs) if lhs.eq_ignore_span(&rhs) => return Ok(()),
                _ => fail!(),
            },

            _ => {}
        }

        match rhs {
            Type::Ref(..) => {
                let rhs = self.expand_top_ref(span, Cow::Borrowed(rhs))?;
                return self.assign_inner(to, &rhs, opts);
            }

            Type::Query(rhs) => return self.assign_from_query_type(opts, to, &rhs),

            Type::Infer(..) => fail!(),

            // When strict null check is disabled, we can assign null / undefined to many things.
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            })
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            }) => {
                // Deny assigning null to class. (not instance)

                match *to.normalize() {
                    Type::Class(..) | Type::Function(..) => fail!(),
                    _ => {}
                }

                if !self.rule().strict_null_checks {
                    return Ok(());
                }
            }
            Type::Union(Union { ref types, .. }) => {
                let errors = types
                    .iter()
                    .filter_map(|rhs| match self.assign_inner(to, rhs, opts) {
                        Ok(()) => None,
                        Err(err) => Some(err),
                    })
                    .collect::<Vec<_>>();
                if errors.is_empty() {
                    return Ok(());
                }
                return Err(Error::Errors { span, errors });
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),

            // Handle unknown on rhs
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                if to.is_kwd(TsKeywordTypeKind::TsAnyKeyword) || to.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                    return Ok(());
                }

                fail!();
            }

            Type::Param(TypeParam {
                ref name,
                ref constraint,
                ..
            }) => {
                //
                match to {
                    Type::Param(TypeParam { name: ref l_name, .. }) => {
                        if name == l_name {
                            return Ok(());
                        }
                    }

                    _ => {}
                }

                match *constraint {
                    Some(ref c) => {
                        return self.assign_inner(
                            to,
                            c,
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                        );
                    }
                    None => match to.normalize() {
                        Type::TypeLit(TypeLit { ref members, .. }) if members.is_empty() => return Ok(()),
                        _ => {}
                    },
                }

                fail!()
            }

            Type::Enum(ref e) => handle_enum_in_rhs!(e),

            Type::EnumVariant(EnumVariant {
                ref ctxt,
                ref enum_name,
                ..
            }) => {
                if let Some(types) = self.find_type(*ctxt, enum_name)? {
                    for ty in types {
                        if let Type::Enum(ref e) = ty.normalize() {
                            handle_enum_in_rhs!(e);
                        }
                    }
                }

                fail!()
            }

            Type::Predicate(..) => match rhs {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }) => return Ok(()),
                _ => {}
            },

            _ => {}
        }

        match to {
            Type::Mapped(to) => return self.assign_to_mapped(opts, to, rhs),
            Type::Param(TypeParam {
                constraint: Some(ref c),
                ..
            }) => return self.assign_inner(c, rhs, opts),

            Type::Array(Array { ref elem_type, .. }) => match rhs {
                Type::Array(Array {
                    elem_type: ref rhs_elem_type,
                    ..
                }) => {
                    return self.assign_inner(&elem_type, &rhs_elem_type, opts);
                }

                Type::Tuple(Tuple { ref elems, .. }) => {
                    let mut errors = Errors::default();
                    for el in elems {
                        errors.extend(self.assign_inner(elem_type, &el.ty, opts).err());
                    }
                    if !errors.is_empty() {
                        Err(errors)?;
                    }

                    return Ok(());
                }
                _ => fail!(),
            },

            // let a: string | number = 'string';
            Type::Union(Union { ref types, .. }) => {
                let results = types
                    .iter()
                    .map(|to| self.assign_inner(&to, rhs, opts))
                    .collect::<Vec<_>>();
                if results.iter().any(Result::is_ok) {
                    return Ok(());
                }
                return Err(Error::UnionError {
                    span,
                    errors: results.into_iter().map(Result::unwrap_err).collect(),
                });
            }

            Type::Intersection(Intersection { ref types, .. }) => {
                let vs = types
                    .iter()
                    .map(|to| self.assign_inner(&to, rhs, opts))
                    .collect::<Vec<_>>();

                // TODO: Multiple error
                for v in vs {
                    if let Err(error) = v {
                        return Err(Error::IntersectionError { span, error: box error });
                    }
                }

                return Ok(());
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsObjectKeyword,
                ..
            }) => {
                // let a: object = {};
                match *rhs {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Function(..)
                    | Type::Constructor(..)
                    | Type::Enum(..)
                    | Type::Class(..)
                    | Type::TypeLit(..) => return Ok(()),

                    _ => {}
                }
            }

            // Handle same keyword type.
            Type::Keyword(RTsKeywordType { kind, .. }) => {
                match *rhs {
                    Type::Keyword(RTsKeywordType { kind: rhs_kind, .. }) => {
                        if rhs_kind == *kind {
                            return Ok(());
                        }

                        if rhs_kind == TsKeywordTypeKind::TsUndefinedKeyword
                            && *kind == TsKeywordTypeKind::TsVoidKeyword
                        {
                            return Ok(());
                        }

                        if *kind == TsKeywordTypeKind::TsUndefinedKeyword
                            && rhs_kind == TsKeywordTypeKind::TsVoidKeyword
                        {
                            return Ok(());
                        }

                        fail!()
                    }

                    Type::Array(..) => fail!(),

                    _ => {}
                }

                match kind {
                    TsKeywordTypeKind::TsStringKeyword => match *rhs {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Str(..), ..
                        }) => return Ok(()),
                        Type::Lit(..) => fail!(),
                        _ => {}
                    },

                    TsKeywordTypeKind::TsNumberKeyword => match *rhs {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Number(..),
                            ..
                        }) => return Ok(()),
                        Type::Lit(..) => fail!(),

                        Type::EnumVariant(ref v) => {
                            // Allow assigning enum with numeric values to
                            // number.
                            if let Some(types) = self.find_type(v.ctxt, &v.enum_name)? {
                                for ty in types {
                                    match *ty.normalize() {
                                        Type::Enum(ref e) => {
                                            let is_num = !e.has_str;
                                            if is_num {
                                                return Ok(());
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }

                            fail!()
                        }
                        _ => {}
                    },

                    TsKeywordTypeKind::TsBooleanKeyword => match *rhs {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Bool(..), ..
                        }) => return Ok(()),
                        Type::Lit(..) => fail!(),
                        _ => return Ok(()),
                    },

                    TsKeywordTypeKind::TsVoidKeyword | TsKeywordTypeKind::TsUndefinedKeyword => {
                        //

                        match *rhs.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            }) => return Ok(()),
                            Type::Lit(..)
                            | Type::Keyword(..)
                            | Type::TypeLit(..)
                            | Type::Class(..)
                            | Type::ClassInstance(..)
                            | Type::Interface(..)
                            | Type::Module(..)
                            | Type::EnumVariant(..) => fail!(),
                            Type::Function(..) => return Err(Error::CannotAssignToNonVariable { span: rhs.span() }),
                            _ => {}
                        }
                    }

                    TsKeywordTypeKind::TsSymbolKeyword => {
                        //

                        match *rhs.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsSymbolKeyword,
                                ..
                            }) => return Ok(()),
                            _ => fail!(),
                        }
                    }

                    _ => {}
                }
            }

            Type::Enum(ref e) => {
                //
                match *rhs {
                    Type::EnumVariant(ref r) => {
                        if r.enum_name == e.id {
                            return Ok(());
                        }
                    }
                    _ => {}
                }

                return Err(Error::AssignFailed {
                    span,
                    left: box Type::Enum(e.clone()),
                    right: box rhs.clone(),
                    cause: vec![],
                });
            }

            Type::EnumVariant(ref l) => match *rhs {
                Type::EnumVariant(ref r) => {
                    if l.enum_name == r.enum_name && l.name == r.name {
                        return Ok(());
                    }

                    fail!()
                }
                _ => {
                    dbg!();
                    return Err(Error::InvalidLValue { span });
                }
            },

            Type::This(RTsThisType { span }) => return Err(Error::CannotAssingToThis { span: *span }),

            Type::Interface(Interface {
                ref body, ref extends, ..
            }) => {
                for parent in extends {
                    let parent =
                        self.type_of_ts_entity_name(span, self.ctx.module_id, &parent.expr, parent.type_args.as_ref())?;

                    if self.assign_with_opts(opts, &parent, &rhs).is_ok() {
                        return Ok(());
                    }
                }

                // TODO: Prevent recursion and uncomment the code below.
                //
                // // We try assigning as builtin interfaces.
                // match rhs {
                //     Type::Keyword(RTsKeywordType {
                //         kind: TsKeywordTypeKind::TsStringKeyword,
                //         ..
                //     })
                //     | Type::Lit(RTsLitType {
                //         lit: RTsLit::Str(..), ..
                //     }) => {
                //         return self
                //             .assign_inner(
                //                 to,
                //                 &Type::Ref(Ref {
                //                     span,
                //                     ctxt: ModuleId::builtin(),
                //                     type_name:
                // RTsEntityName::Ident(RIdent::new("String".into(), span)),
                //                     type_args: None,
                //                 }),
                //                 opts,
                //             )
                //             .context("tried to assign by converting rhs to builtin inferface
                // 'String'")     }
                //     _ => {}
                // }
                self.assign_to_type_elements(opts, span, &body, rhs)
                    .context("tried to assign an interfafce to an interface")?;

                // Assignment failed. This check is required to distinguish an empty interface
                // from an interface with parents.
                //
                // TODO: Use errors returned from parent assignment.
                if body.is_empty() && !extends.is_empty() {
                    fail!()
                }

                return Ok(());
            }

            Type::TypeLit(TypeLit { ref members, .. }) => {
                self.assign_to_type_elements(opts, span, &members, rhs)?;

                return Ok(());
            }

            Type::Lit(RTsLitType { ref lit, .. }) => match *rhs {
                Type::Lit(RTsLitType { lit: ref r_lit, .. }) => {
                    if lit.eq_ignore_span(r_lit) {
                        return Ok(());
                    }

                    // Extra check to handle "has_escape"
                    match (lit, r_lit) {
                        (&RTsLit::Str(ref l), &RTsLit::Str(ref r)) if l.value == r.value => return Ok(()),
                        _ => {}
                    }

                    fail!()
                }
                // TODO: allow
                // let a: true | false = bool
                _ => fail!(),
            },

            Type::Function(ty::Function {
                ret_ty: ref left_ret_ty,
                ..
            }) => {
                // var fnr2: () => any = fnReturn2();
                match *rhs {
                    Type::Function(ty::Function {
                        ret_ty: ref right_ret_ty,
                        ..
                    }) => {
                        // TODO: Verify type parameters.
                        self.assign_inner(right_ret_ty, left_ret_ty, opts)?;
                        // TODO: Verify parameter counts

                        return Ok(());
                    }

                    Type::Lit(..) => return Err(Error::CannotAssignToNonVariable { span }),
                    _ => fail!(),
                }
            }

            Type::Tuple(Tuple { ref elems, .. }) => {
                //
                match *rhs.normalize() {
                    Type::Tuple(Tuple {
                        elems: ref rhs_elems, ..
                    }) => {
                        if elems.len() < rhs_elems.len() {
                            fail!();
                        }
                        if !elems.is_empty() && rhs_elems.is_empty() {
                            fail!();
                        }

                        let mut errors = Errors::default();
                        for (l, r) in elems.into_iter().zip(rhs_elems) {
                            for el in elems {
                                let err = match *r.ty.normalize() {
                                    Type::Keyword(RTsKeywordType {
                                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                        ..
                                    }) => continue,
                                    _ => {}
                                };

                                errors.extend(self.assign_inner(&l.ty, &r.ty, opts).err());
                            }
                        }

                        if !errors.is_empty() {
                            Err(errors)?;
                        }

                        return Ok(());
                    }
                    Type::Array(..) => fail!(),
                    _ => {}
                }
            }

            //Type::Simple(ref s) => match **s {
            //    RTsType::TsTypePredicate(..) => match *rhs.normalize() {
            //        Type::Keyword(RTsKeywordType {
            //            kind: TsKeywordTypeKind::TsBooleanKeyword,
            //            ..
            //        })
            //        | Type::Lit(RTsLitType {
            //            lit: RTsLit::Bool(..),
            //            ..
            //        }) => return Ok(()),
            //        _ => {}
            //    },
            //
            //    _ => {}
            //},

            // TODO: Check type arguments
            Type::ClassInstance(ClassInstance { ty: ref l_ty, .. }) => match *rhs.normalize() {
                Type::Keyword(..) | Type::TypeLit(..) | Type::Lit(..) => fail!(),

                Type::ClassInstance(ClassInstance { ty: ref r_ty, .. }) => return self.assign_inner(l_ty, &r_ty, opts),
                _ => {}
            },

            Type::Constructor(ref lc) => match *rhs.normalize() {
                Type::Lit(..) | Type::Class(ty::Class { is_abstract: true, .. }) => fail!(),
                _ => {}
            },

            _ => {}
        }

        // TODO: Implement full type checker
        slog::error!(self.logger, "unimplemented: assign: \nLeft: {:?}\nRight: {:?}", to, rhs);
        Ok(())
    }

    /// This method is called when lhs of assignment is interface or type
    /// literal.
    ///
    /// ```js
    /// interface A {}
    /// let a: A = foo;
    /// let b: { key: string } = foo;
    /// ```
    fn assign_to_type_elements(
        &mut self,
        opts: AssignOpts,
        lhs_span: Span,
        lhs: &[TypeElement],
        rhs: &Type,
    ) -> ValidationResult<()> {
        let span = opts.span;
        // debug_assert!(!span.is_dummy());

        let mut errors = Errors::default();
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

            macro_rules! handle_type_elements {
                ($rhs:expr) => {{
                    for r in $rhs {
                        if !opts.allow_unknown_rhs {
                            unhandled_rhs.push(r.span());
                        }
                    }

                    for (i, m) in lhs.into_iter().enumerate() {
                        let res = self
                            .assign_type_elements_to_type_element(opts, &mut missing_fields, m, $rhs)
                            .with_context(|| format!("tried to assign to {}th element: {:?}", i, m.key()));

                        let success = match res {
                            Ok(()) => true,
                            Err(Error::Errors { ref errors, .. }) if errors.is_empty() => true,
                            Err(err) => {
                                errors.push(err);
                                false
                            }
                        };
                        if success && $rhs.len() > i {
                            if let Some(pos) = unhandled_rhs.iter().position(|span| *span == $rhs[i].span()) {
                                unhandled_rhs.remove(pos);
                            } else {
                                // panic!("it should be removable")
                            }
                        }
                    }
                }};
            }

            match rhs.normalize() {
                Type::TypeLit(TypeLit {
                    members: rhs_members, ..
                }) => {
                    handle_type_elements!(&*rhs_members);
                }

                Type::Interface(Interface { body, .. }) => {
                    handle_type_elements!(&*body);
                    // TODO: Check parent interface
                }

                Type::Tuple(..) if lhs.is_empty() => return Ok(()),

                Type::Array(..) if lhs.is_empty() => return Ok(()),

                Type::Array(..) => return Err(Error::InvalidAssignmentOfArray { span }),

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

                Type::Class(rhs) => {
                    //
                    for el in lhs {
                        self.assign_class_members_to_type_element(opts, el, &rhs.body)?;
                    }

                    return Ok(());
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
                }) if lhs.is_empty() => return Ok(()),

                _ => {
                    return Err(Error::Unimplemented {
                        span,
                        msg: format!("assign_to_type_elements - ??"),
                    })
                }
            }

            if !errors.is_empty() {
                return Err(errors)?;
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
                // Check class itself
                Type::Class(ty::Class { ref body, .. }) => {
                    match m {
                        TypeElement::Call(_) => {
                            unimplemented!("ssign: interface {{ () => ret; }} = class Foo {{}}",)
                        }
                        TypeElement::Constructor(_) => {
                            // TODO: Check # of parameters
                            for rm in body {
                                match rm {
                                    ty::ClassMember::Constructor(..) => continue 'l,
                                    _ => {}
                                }
                            }

                            errors.push(Error::ConstructorRequired {
                                span,
                                lhs: lhs_span,
                                rhs: rhs.span(),
                            });
                        }
                        TypeElement::Property(p) => {
                            //

                            for rm in body {
                                match rm {
                                    ty::ClassMember::Constructor(..) => continue 'l,
                                    _ => {}
                                }
                            }
                        }
                        TypeElement::Method(_) => {
                            unimplemented!("assign: interface {{ method() => ret; }} = class Foo {{}}")
                        }
                        TypeElement::Index(_) => {
                            unimplemented!("assign: interface {{ [key: string]: Type; }} = class Foo {{}}")
                        }
                    }

                    // TODO: missing fields
                }

                // Check class members
                Type::ClassInstance(ClassInstance {
                    ty: box Type::Class(ty::Class { ref body, .. }),
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
                                    ty::ClassMember::Property(ref rp) => {
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
                }) => return Err(vec![])?,

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

    /// This method assigns each property to corresponding property.
    fn assign_type_elements_to_type_element(
        &mut self,
        opts: AssignOpts,
        missing_fields: &mut Vec<TypeElement>,
        m: &TypeElement,
        rhs_members: &[TypeElement],
    ) -> ValidationResult<()> {
        let span = opts.span;
        // We need this to show error if not all of rhs_member is matched

        if let Some(l_key) = m.key() {
            for rm in rhs_members {
                if let Some(r_key) = rm.key() {
                    if l_key.type_eq(&*r_key) {
                        match m {
                            TypeElement::Property(ref el) => match rm {
                                TypeElement::Property(ref r_el) => {
                                    self.assign_inner(
                                        el.type_ann.as_ref().unwrap_or(&Type::any(span)),
                                        r_el.type_ann.as_ref().unwrap_or(&Type::any(span)),
                                        opts,
                                    )?;
                                    return Ok(());
                                }
                                _ => {}
                            },

                            // `foo(a: string) is assignable to foo(a: any)`
                            TypeElement::Method(ref lm) => match rm {
                                TypeElement::Method(ref rm) => {
                                    //

                                    if lm.params.len() != rm.params.len() {
                                        return Err(Error::Unimplemented {
                                            span,
                                            msg: format!(
                                                "lhs.method.params.len() = {}; rhs.method.params.len() = {};",
                                                lm.params.len(),
                                                rm.params.len()
                                            ),
                                        });
                                    }

                                    for (lp, rp) in lm.params.iter().zip(rm.params.iter()) {
                                        self.assign_inner(&lp.ty, &rp.ty, opts)
                                            .context("tried to assign a method parameter to a method parameter")?;
                                    }

                                    return Ok(());
                                }

                                TypeElement::Property(rp) => {
                                    // Allow assigning property with callable type to methods.
                                    if let Some(rp_ty) = &rp.type_ann {
                                        if let Type::Function(rp_ty) = rp_ty.normalize() {
                                            if lm.params.len() != rp_ty.params.len() {
                                                return Err(Error::Unimplemented {
                                                    span,
                                                    msg: format!(
                                                        "lhs.method.params.len() = {}; rhs.property.params.len() = {};",
                                                        lm.params.len(),
                                                        rp_ty.params.len()
                                                    ),
                                                });
                                            }

                                            for (lp, rp) in lm.params.iter().zip(rp_ty.params.iter()) {
                                                self.assign_inner(&lp.ty, &rp.ty, opts).context(
                                                    "tried to assign a parameter of a property with callable type to \
                                                     a method parameter",
                                                )?;
                                            }

                                            return Ok(());
                                        }
                                    }
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                }
            }

            match m {
                TypeElement::Property(PropertySignature { optional: true, .. })
                | TypeElement::Method(MethodSignature { optional: true, .. }) => {}
                _ => {
                    // No property with `key` found.
                    missing_fields.push(m.clone());
                }
            }
        } else {
            match m {
                // TODO: Check type of the index.
                TypeElement::Index(..) => {
                    // TODO: Verify
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

                    missing_fields.push(m.clone());
                }
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn extract_keys(&mut self, span: Span, ty: &Type) -> ValidationResult {
        let ty = ty.normalize();

        match ty {
            Type::TypeLit(ty) => {
                //
                let mut keys = vec![];
                for member in &ty.members {
                    match member {
                        TypeElement::Property(PropertySignature {
                            span,
                            key: Key::Normal { sym: key, .. },
                            ..
                        }) => {
                            keys.push(Box::new(Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span: *span,
                                lit: RTsLit::Str(RStr {
                                    span: *span,
                                    has_escape: false,
                                    kind: Default::default(),
                                    value: key.clone(),
                                }),
                            })));
                        }
                        _ => {}
                    }
                }

                return Ok(Type::union(keys));
            }
            _ => {}
        }

        Err(Error::Unimplemented {
            span,
            msg: format!("Extract keys"),
        })
    }

    /// Handles `P in 'foo' | 'bar'`. Note that `'foo' | 'bar'` part should be
    /// passed as `keys`.
    ///
    ///
    /// Currently only literals and unions are supported for `keys`.
    fn assign_keys(&mut self, opts: AssignOpts, keys: &Type, rhs: &Type) -> ValidationResult<()> {
        let keys = keys.normalize();
        let rhs = rhs.normalize();

        let rhs_keys = self.extract_keys(opts.span, &rhs)?;

        self.assign_with_opts(opts, &keys, &rhs_keys)
    }

    /// Returns `Ok(true)` if assignment was successfult and returns `Ok(false)`
    /// if the method doesn't know the way to handle assignment.
    fn assign_to_mapped(&mut self, opts: AssignOpts, to: &Mapped, rhs: &Type) -> ValidationResult<()> {
        let rhs = rhs.normalize();

        // Validate keys
        match &to.type_param.constraint {
            Some(constraint) => self.assign_keys(opts, &constraint, rhs)?,
            None => {}
        }

        let ty = match &to.ty {
            Some(v) => v.normalize(),
            None => return Ok(()),
        };

        match rhs {
            Type::TypeLit(rhs) => {
                //
                for member in &rhs.members {
                    match member {
                        TypeElement::Property(prop) => {
                            if let Some(prop_ty) = &prop.type_ann {
                                self.assign_with_opts(opts, &ty, &prop_ty)?;
                            }
                        }
                        _ => {
                            return Err(Error::Unimplemented {
                                span: opts.span,
                                msg: format!("Assignment to mapped type: type element - {:?}", member),
                            })
                        }
                    }
                }

                return Ok(());
            }
            _ => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("Assignment to mapped type"),
        })
    }
}

/// Returns true if l and r are lieteral and equal to each other.
fn is_key_eq(l: &RExpr, r: &RExpr) -> bool {
    (match (l, r) {
        (&RExpr::Ident(..), &RExpr::Ident(..)) => true,
        (&RExpr::Member(..), &RExpr::Member(..)) => true,
        _ => false,
    }) && l.eq_ignore_span(r)
}

fn count_required_params(v: &[FnParam]) -> usize {
    v.iter().filter(|v| v.required).count()
}

//fn type_of_ts_fn_param<'a>(p: &TsFnParam) -> Type {
//    match p {
//        RTsFnParam::Ident(Ident { type_ann, .. })
//        | TsFnParam::Array(ArrayPat { type_ann, .. })
//        | TsFnParam::Object(ObjectPat { type_ann, .. })
//        | TsFnParam::Rest(RRestPat { type_ann, .. }) => type_ann
//            .clone()
//            .map(|ty| Type::from(ty))
//            .unwrap_or(Type::any(p.span())),
//    }
//}
