use fxhash::FxHashMap;
use rnode::FoldWith;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_generics::{
    expander::{GenericExpander, GENERIC_CACHE},
    ExpandGenericOpts,
};
use stc_ts_type_ops::Fix;
use stc_ts_types::{Id, Interface, KeywordType, TypeElement, TypeParam, TypeParamDecl, TypeParamInstantiation};
use stc_utils::{cache::Freeze, dev_span, ext::SpanExt};
use swc_common::{Span, Spanned, TypeEq};
use swc_ecma_ast::*;
use tracing::debug;

use crate::{
    analyzer::{assign::AssignOpts, scope::ExpandOpts, Analyzer},
    ty::Type,
    VResult,
};

/// All fields default to false.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub(crate) struct ExtendsOpts {
    /// If true, different classes are treated as not extending each other even
    /// though those are empty.
    ///
    /// `false` by default because the type `Foo` in code below is 1.
    ///
    /// ```ts
    /// class C {}
    /// class D {}
    ///
    /// type Foo = C extends D ? 1 : 0
    /// ```
    pub disallow_different_classes: bool,

    /// `strictSubtype` of `tsc`.
    pub strict: bool,

    pub allow_missing_fields: bool,
}

/// Generic expander.
impl Analyzer<'_, '_> {
    pub(in super::super) fn instantiate_type_params_using_args(
        &mut self,
        span: Span,
        type_params: &TypeParamDecl,
        type_args: &TypeParamInstantiation,
    ) -> VResult<FxHashMap<Id, Type>> {
        let _tracing = dev_span!("instantiate_type_params_using_args");

        let mut params = FxHashMap::default();

        for (idx, param) in type_params.params.iter().enumerate() {
            if let Some(arg) = type_args.params.get(idx) {
                // TODO(kdy1): Change this to assert.
                let arg = arg.clone().freezed();
                params.insert(param.name.clone(), arg);
            } else {
                if let Some(default) = &param.default {
                    let default = default.clone().freezed();
                    params.insert(param.name.clone(), *default.clone());
                } else {
                    unimplemented!(
                        "Reporting errors when type parameter count and type argument count differs\nParams={:#?}\nArgs: {:#?}",
                        type_params,
                        type_args
                    )
                }
            }
        }

        Ok(params)
    }

    ///
    ///
    ///  This methods handle special types like mapped type.
    ///
    ///  e.g.
    ///      type BadNested<T> = {
    ///          x: T extends number ? T : string;
    ///      };
    ///      T extends {
    ///          [K in keyof BadNested<infer P>]: BadNested<infer P>[K];
    ///      } ? P : never;
    ///
    ///
    ///z     T extends {
    ///          x: infer P extends number ? infer P : string;
    ///      } ? P : never
    pub(in super::super) fn expand_type_params<T>(&mut self, params: &FxHashMap<Id, Type>, ty: T, opts: ExpandGenericOpts) -> VResult<T>
    where
        T: for<'aa> FoldWith<GenericExpander<'aa>> + Fix,
    {
        let _tracing = dev_span!("expand_type_params");

        for param in params.values() {
            param.assert_valid();
            debug_assert!(param.is_clone_cheap());
        }

        GENERIC_CACHE.configure(|| {
            let ty = ty
                .fold_with(&mut GenericExpander {
                    cm: self.cm.clone(),
                    params,
                    fully: false,
                    dejavu: Default::default(),
                    opts,
                })
                .fixed();

            Ok(ty)
        })
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    pub(crate) fn extends(&mut self, span: Span, child: &Type, parent: &Type, opts: ExtendsOpts) -> Option<bool> {
        let _tracing = if cfg!(debug_assertions) {
            let child = dump_type_as_string(child);
            let parent = dump_type_as_string(parent);
            Some(dev_span!(
                "extends",
                child = tracing::field::display(&child),
                parent = tracing::field::display(&parent)
            ))
        } else {
            None
        };

        let child = child.normalize();
        let parent = parent.normalize();

        if child.is_any() {
            return Some(true);
        }

        if child.type_eq(parent) {
            return Some(true);
        }

        debug!(
            "[generic/extends] Checking if {} extends {}",
            dump_type_as_string(child),
            dump_type_as_string(parent),
        );

        if let Type::Param(TypeParam {
            constraint: Some(child), ..
        }) = child
        {
            if let Some(v) = self.extends(span, child, parent, opts) {
                return Some(v);
            }
        }

        match child {
            Type::Param(..) | Type::Infer(..) | Type::IndexedAccessType(..) | Type::Conditional(..) => return None,
            Type::Ref(..) => {
                let child = self
                    .expand(
                        child.span(),
                        child.clone(),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ignore_expand_prevention_for_all: false,
                            ..Default::default()
                        },
                    )
                    .unwrap()
                    .freezed();
                if let Type::Ref(..) = child.normalize() {
                    return None;
                }

                return self.extends(span, &child, parent, opts);
            }

            Type::Union(child) => {
                let mut prev = None;

                for child in &child.types {
                    let res = self.extends(span, child, parent, opts)?;

                    match prev {
                        Some(v) => {
                            if v != res {
                                return None;
                            }
                        }
                        None => {
                            prev = Some(res);
                        }
                    }
                }

                return prev;
            }
            // `never` extends all types because it's bottom type in TypeScript
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNeverKeyword,
                ..
            }) => return Some(true),
            _ => {}
        }

        match parent {
            Type::Param(..) | Type::Infer(..) | Type::IndexedAccessType(..) => return None,
            Type::Ref(..) => {
                let mut parent = self
                    .expand(
                        parent.span().or_else(|| span),
                        parent.clone(),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ignore_expand_prevention_for_all: false,
                            ..Default::default()
                        },
                    )
                    .unwrap();
                if let Type::Ref(..) = parent.normalize() {
                    return None;
                }
                parent.freeze();

                return self.extends(span, child, &parent, opts);
            }
            _ => {}
        }

        match parent {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => return Some(false),
            Type::Union(parent) => {
                let mut has_false = false;

                for parent in &parent.types {
                    let res = self.extends(span, child, parent, opts);
                    if let Some(true) = res {
                        return Some(true);
                    }
                    match res {
                        Some(true) => return Some(true),
                        Some(false) => {
                            has_false = true;
                        }
                        None => {}
                    }
                }

                if has_false {
                    return Some(false);
                } else {
                    return None;
                }
            }

            Type::Interface(Interface { name, .. }) if *name.sym() == *"Function" => match child {
                Type::Function(..) => {
                    return Some(true);
                }
                Type::TypeLit(child) => {
                    if child.members.iter().any(|m| matches!(m, TypeElement::Call(..))) {
                        return Some(true);
                    }
                }
                _ => {}
            },

            Type::Interface(Interface { name, .. }) if *name.sym() == *"ObjectConstructor" => match child {
                Type::Class(..) | Type::ClassDef(..) | Type::Interface(..) | Type::TypeLit(..) => {
                    return Some(true);
                }
                _ => {}
            },

            Type::Interface(Interface { name, .. }) if *name.sym() == *"ArrayConstructor" => match child {
                Type::Array(..) | Type::Tuple(..) => {
                    return Some(true);
                }
                _ => {}
            },

            _ => {}
        }

        match child {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => {
                if self.rule().strict_null_checks {
                    return Some(true);
                }
                return Some(false);
            }
            Type::Function(..) => match parent {
                Type::Class(..) | Type::Enum(..) => return Some(false),
                _ => {}
            },
            Type::Interface(..) => {
                if let Type::TypeLit(..) = parent {
                    if opts.strict {
                        return Some(false);
                    }
                }
            }
            Type::TypeLit(..) => match parent {
                Type::Class(..) | Type::ClassDef(..) => return Some(false),
                _ => {}
            },
            Type::ClassDef(child_class) => match parent {
                Type::Function(..) | Type::Lit(..) => return Some(false),
                Type::Constructor(..) => return Some(true),
                Type::TypeLit(parent) => {
                    // //
                    // // TODO
                    // for element in &parent.members {
                    //     match element {
                    //         TypeElement::Call(_) => todo!("Class extends
                    // TypeLit => Call"),
                    //         TypeElement::Constructor(_) => {
                    //             todo!("Class extends TypeLit => Constructor")
                    //         }
                    //         TypeElement::Property(_) => todo!("Class extends
                    // TypeLit => Property"),
                    //         TypeElement::Method(_) => todo!("Class extends
                    // TypeLit => Method"),
                    //         TypeElement::Index(_) => {
                    //             todo!("Class extends TypeLit =>
                    // IndexSignature")         }
                    //     }
                    // }

                    // return Some(true);
                }
                _ => {
                    if let Some(super_class) = &child_class.super_class {
                        if (**super_class).type_eq(parent) {
                            return Some(true);
                        }
                    }

                    if let Type::ClassDef(parent) = parent {
                        // Check for grand parent
                        if let Some(grand_parent) = &parent.super_class {
                            if let Some(false) = self.extends(span, child, grand_parent, opts) {
                                return Some(false);
                            }
                        }
                    }
                }
            },
            Type::Tuple(child_tuple) => {
                if let Type::Array(parent_array) = parent {
                    if child_tuple
                        .elems
                        .iter()
                        .all(|child_element| self.extends(span, &child_element.ty, &parent_array.elem_type, opts) == Some(true))
                    {
                        return Some(true);
                    }
                }
            }
            Type::Array(child_array) => {
                if let Type::Tuple(parent_tuple) = parent {
                    return Some(false);
                }
            }
            Type::Intersection(child_intersection) => {
                for child_ty in child_intersection.types.iter() {
                    match self.extends(span, child_ty, parent, opts) {
                        Some(true) => return Some(true),
                        None => return None,
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        if child.is_null_or_undefined() && (parent.is_fn_type() || parent.is_constructor()) {
            return Some(false);
        }

        let res = self.assign_with_opts(
            &mut Default::default(),
            parent,
            child,
            AssignOpts {
                span,
                disallow_special_assignment_to_empty_class: true,
                disallow_different_classes: opts.disallow_different_classes,
                allow_assignment_to_param_constraint: true,
                allow_unknown_rhs: Some(!opts.strict),
                allow_unknown_rhs_if_expanded: !opts.strict,
                allow_missing_fields: opts.allow_missing_fields,
                ..Default::default()
            },
        );

        match res {
            Ok(()) => Some(true),
            _ => Some(false),
        }
    }
}
