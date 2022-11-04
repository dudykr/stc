use fxhash::FxHashMap;
use rnode::FoldWith;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_generics::{expander::GenericExpander, ExpandGenericOpts};
use stc_ts_type_ops::Fix;
use stc_ts_types::{Id, Interface, KeywordType, TypeParam, TypeParamDecl, TypeParamInstantiation};
use stc_utils::cache::Freeze;
use swc_common::{Span, Spanned, TypeEq};
use swc_ecma_ast::*;
use tracing::debug;

use crate::{
    analyzer::{assign::AssignOpts, scope::ExpandOpts, Analyzer, Ctx},
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
}

/// Generic expander.
impl Analyzer<'_, '_> {
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(in super::super) fn instantiate_type_params_using_args(
        &mut self,
        span: Span,
        type_params: &TypeParamDecl,
        type_args: &TypeParamInstantiation,
    ) -> VResult<FxHashMap<Id, Type>> {
        let mut params = FxHashMap::default();

        for (idx, param) in type_params.params.iter().enumerate() {
            if let Some(arg) = type_args.params.get(idx) {
                // TODO(kdy1): Change this to assert.
                let arg = arg.clone().cheap();
                params.insert(param.name.clone(), arg);
            } else {
                if let Some(default) = &param.default {
                    let default = default.clone().cheap();
                    params.insert(param.name.clone(), default.clone());
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
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(in super::super) fn expand_type_params<T>(&mut self, params: &FxHashMap<Id, Type>, ty: T, opts: ExpandGenericOpts) -> VResult<T>
    where
        T: for<'aa> FoldWith<GenericExpander<'aa>> + Fix,
    {
        for (_, param) in params {
            debug_assert!(param.is_clone_cheap());
        }

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
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn extends(&mut self, span: Span, child: &Type, parent: &Type, opts: ExtendsOpts) -> Option<bool> {
        let child = child.normalize();
        let parent = parent.normalize();

        if child.is_any() {
            return Some(true);
        }

        if child.type_eq(&parent) {
            return Some(true);
        }

        debug!(
            "[generic/extends] Checking if {} extends {}",
            dump_type_as_string(&self.cm, &child),
            dump_type_as_string(&self.cm, &parent),
        );

        match child {
            Type::Param(TypeParam {
                constraint: Some(child), ..
            }) => {
                if let Some(v) = self.extends(span, child, parent, opts) {
                    return Some(v);
                }
            }
            _ => {}
        }

        match child {
            Type::Param(..) | Type::Infer(..) => return None,
            Type::Ref(..) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ignore_expand_prevention_for_all: false,
                    preserve_params: true,
                    preserve_ret_ty: true,
                    ..self.ctx
                };
                let child = self
                    .with_ctx(ctx)
                    .expand(
                        child.span(),
                        child.clone(),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            ..Default::default()
                        },
                    )
                    .unwrap()
                    .freezed();
                match child.normalize() {
                    Type::Ref(..) => return None,
                    _ => {}
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

            _ => {}
        }

        match parent {
            Type::Param(..) | Type::Infer(..) => return None,
            Type::Ref(..) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ignore_expand_prevention_for_all: false,
                    preserve_params: true,
                    preserve_ret_ty: true,
                    ..self.ctx
                };
                let mut parent = self
                    .with_ctx(ctx)
                    .expand(
                        parent.span(),
                        parent.clone(),
                        ExpandOpts {
                            full: true,
                            expand_union: true,
                            ..Default::default()
                        },
                    )
                    .unwrap();
                match parent.normalize() {
                    Type::Ref(..) => return None,
                    _ => {}
                }
                parent.make_clone_cheap();

                return self.extends(span, child, &parent, opts);
            }
            _ => {}
        }

        match parent {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
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
            Type::Function(..) => match parent {
                Type::Class(..) | Type::Enum(..) => return Some(false),
                _ => {}
            },
            Type::Interface(..) => match parent {
                Type::TypeLit(..) => return Some(false),
                _ => {}
            },
            Type::TypeLit(..) => match parent {
                Type::Class(..) | Type::ClassDef(..) => return Some(false),
                _ => {}
            },
            Type::ClassDef(child_class) => match parent {
                Type::Function(..) | Type::Lit(..) => return Some(false),
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
                        if (&**super_class).type_eq(parent) {
                            return Some(true);
                        }
                    }

                    match parent {
                        Type::ClassDef(parent) => {
                            // Check for grand parent
                            if let Some(grand_parent) = &parent.super_class {
                                if let Some(false) = self.extends(span, child, grand_parent, opts) {
                                    return Some(false);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            },
            Type::Tuple(child_tuple) => match parent {
                Type::Array(parent_array) => {
                    if child_tuple
                        .elems
                        .iter()
                        .all(|child_element| self.extends(span, &child_element.ty, &parent_array.elem_type, opts) == Some(true))
                    {
                        return Some(true);
                    }
                }
                _ => {}
            },
            Type::Array(child_array) => match parent {
                Type::Tuple(parent_tuple) => return Some(false),
                _ => {}
            },
            _ => {}
        }
        // dbg!(child, parent);

        let res = self.assign_with_opts(
            &mut Default::default(),
            AssignOpts {
                span,
                disallow_special_assignment_to_empty_class: true,
                disallow_different_classes: opts.disallow_different_classes,
                allow_assignment_to_param_constraint: true,
                ..Default::default()
            },
            parent,
            child,
        );

        match res {
            Ok(()) => Some(true),
            _ => Some(false),
        }
    }
}
