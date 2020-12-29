use crate::{
    analyzer::{Analyzer, Ctx},
    ty::{
        Array, IndexedAccessType, Mapped, Operator, PropertySignature, Ref, Type, TypeElement,
        TypeLit,
    },
    ValidationResult,
};
use fxhash::{FxHashMap, FxHashSet};
use rnode::Fold;
use rnode::FoldWith;
use slog::Logger;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_types::{Id, TypeParam};
use swc_atoms::js_word;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_ecma_ast::*;

/// Generic expander.
impl Analyzer<'_, '_> {
    pub(in super::super) fn expand_type_params(
        &mut self,
        params: &FxHashMap<Id, Box<Type>>,
        ty: Box<Type>,
    ) -> ValidationResult {
        let ty = self.expand_type_params_inner(params, ty, false)?;
        Ok(ty)
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
    fn expand_type_params_inner(
        &mut self,
        params: &FxHashMap<Id, Box<Type>>,
        ty: Box<Type>,
        fully: bool,
    ) -> ValidationResult {
        let ty = ty.fold_with(&mut GenericExpander {
            logger: self.logger.clone(),
            analyzer: self,
            params,
            fully,
            dejavu: Default::default(),
        });

        Ok(ty)
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    pub(in crate::analyzer) fn extends(&mut self, child: &Type, parent: &Type) -> Option<bool> {
        let child = child.normalize();
        let parent = parent.normalize();

        if child.type_eq(&parent) {
            return Some(true);
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
                    .expand_fully(child.span(), box child.clone(), true)
                    .unwrap();
                match &*child {
                    Type::Ref(..) => return None,
                    _ => {}
                }

                return self.extends(&child, parent);
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
                let parent = self
                    .with_ctx(ctx)
                    .expand_fully(parent.span(), box parent.clone(), true)
                    .unwrap();
                match &*parent {
                    Type::Ref(..) => return None,
                    _ => {}
                }

                return self.extends(child, &parent);
            }
            _ => {}
        }

        match parent {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            }) => return Some(false),
            Type::Union(parent) => {
                for res in parent
                    .types
                    .iter()
                    .map(|parent| self.extends(child, &parent))
                {
                    if res != Some(true) {
                        return Some(false);
                    }
                }

                return Some(true);
            }
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
                Type::Class(..) => return Some(false),
                _ => {}
            },
            Type::Class(child_class) => match parent {
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
                        Type::Class(parent) => {
                            // Check for grand parent
                            if let Some(grand_parent) = &parent.super_class {
                                if let Some(false) = self.extends(child, grand_parent) {
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
                    if child_tuple.elems.iter().all(|child_element| {
                        self.extends(&child_element.ty, &parent_array.elem_type) == Some(true)
                    }) {
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
        let span = child.span();
        // dbg!(child, parent);

        {
            let ctx = Ctx {
                fail_on_extra_fields: true,
                ..self.ctx
            };
            match self.with_ctx(ctx).assign(parent, child, span) {
                Ok(()) => Some(true),
                _ => Some(false),
            }
        }
    }
}

/// This struct does not expands ref to other thpe. See Analyzer.expand to do
/// such operation.
struct GenericExpander<'a, 'b, 'c, 'd> {
    logger: Logger,
    analyzer: &'a mut Analyzer<'b, 'c>,
    params: &'d FxHashMap<Id, Box<Type>>,
    /// Expand fully?
    fully: bool,
    dejavu: FxHashSet<Id>,
}

impl Fold<Type> for GenericExpander<'_, '_, '_, '_> {
    fn fold(&mut self, ty: Type) -> Type {
        let old_fully = self.fully;
        self.fully |= match ty.normalize() {
            Type::Mapped(..) => true,
            _ => false,
        };
        let span = ty.span();

        slog::debug!(self.logger, "generic_expand: {:?}", &ty);
        let ty = ty.foldable();

        match ty {
            Type::StaticThis(..) | Type::Symbol(..) => return ty,
            Type::Ref(Ref {
                span,
                type_name: RTsEntityName::Ident(ref i),
                ref type_args,
                ..
            }) => {
                if i.sym == js_word!("Array") {
                    return Type::Array(Array {
                        span,
                        elem_type: type_args
                            .as_ref()
                            .and_then(|args| args.params.iter().next().cloned())
                            .unwrap_or_else(|| Type::any(span)),
                    });
                }

                if self.dejavu.contains(&i.into()) {
                    slog::debug!(self.logger, "Dejavu: {}", i.sym);
                    return ty;
                }

                slog::info!(self.logger, "Ref: {}", Id::from(i));

                if let Some(ty) = self.params.get(&i.into()) {
                    return *ty.clone();
                }

                return ty.fold_children_with(self);
            }

            Type::Ref(..) => return ty.fold_children_with(self),

            // Type::IndexedAccessType(IndexedAccessType {
            //     span,
            //     obj_type:
            //         box Type::Param(TypeParam {
            //             name: obj_param_name,
            //             ..
            //         }),
            //     index_type:
            //         box Type::Param(TypeParam {
            //             name: index_param_name,
            //             constraint:
            //                 Some(box Type::Operator(Operator {
            //                     op: TsTypeOperatorOp::KeyOf,
            //                     ty: box Type::Param(constraint_param),
            //                     ..
            //                 })),
            //             ..
            //         }),
            //     ..
            // }) if obj_param_name == constraint_param.name
            //     && self.params.contains_key(&obj_param_name)
            //     && self.params.get(&obj_param_name).unwrap().is_type_lit() =>
            // {
            //     dbg!(&index_param_name);
            //     if let Some(box Type::TypeLit(ref lit)) = self.params.get(&obj_param_name) {
            //         let mut new_members = vec![];

            //         for member in &lit.members {}

            //         return Type::TypeLit(TypeLit {
            //             span: lit.span,
            //             members: new_members,
            //         });
            //     }

            //     unreachable!()
            // }
            Type::Param(mut param) => {
                param = param.fold_with(self);

                if self.dejavu.contains(&param.name) {
                    return Type::Param(param);
                }

                if let Some(ty) = self.params.get(&param.name) {
                    slog::info!(
                        self.logger,
                        "generic_expand: Type parameter: {} => {:?}",
                        param.name,
                        ty
                    );

                    // If it's not self-referential, we fold it again.

                    self.dejavu.insert(param.name.clone());
                    let ty = *ty.clone().fold_with(self);
                    self.dejavu.remove(&param.name);
                    return ty;
                }

                slog::warn!(
                    self.logger,
                    "generic_expand: Failed to found type parameter instantiation: {}",
                    param.name,
                );

                return Type::Param(param);
            }

            // Alias returns other than self.
            Type::Alias(mut alias) => {
                alias = alias.fold_with(self);
                //
                if let Some(..) = &alias.type_params {
                    // TODO: Handle unresolved type parameter
                    slog::warn!(
                        self.logger,
                        "An type alias has type parameters. It may not be fully expanded."
                    );
                }
                return *alias.ty.fold_with(self);
            }

            Type::Interface(mut i) if self.fully => {
                i = i.fold_with(self);

                if let Some(..) = &i.type_params {
                    slog::error!(
                        self.logger,
                        "An interface has type parameters. It may not be fully expanded."
                    );
                }

                // TODO: Handle super
                if !i.extends.is_empty() {
                    slog::error!(
                        self.logger,
                        "not yet implemented: expanding interface which has a parent"
                    );
                    return Type::Interface(i);
                }

                return Type::TypeLit(TypeLit {
                    span: i.span,
                    members: i.body,
                });
            }
            Type::Class(mut c) => {
                c = c.fold_with(self);

                if let Some(..) = &c.type_params {
                    slog::error!(
                        self.logger,
                        "A class has type parameters. It may not be fully expanded."
                    );
                }

                return Type::Class(c);
            }

            Type::Conditional(mut c) => {
                c = c.fold_with(self);

                // if let Some(v) = self.analyzer.extends(&c.check_type, &c.extends_type) {
                //     return if v { *c.true_type } else { *c.false_type };
                // }

                return Type::Conditional(c);
            }

            Type::Mapped(mut m @ Mapped { ty: Some(..), .. }) => {
                match &m.type_param.constraint {
                    Some(constraint) => match constraint.normalize() {
                        Type::Operator(
                            operator
                            @
                            Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ..
                            },
                        ) => match operator.ty.normalize() {
                            Type::Param(param) if self.params.contains_key(&param.name) => {
                                let ty = self.params.get(&param.name).unwrap();
                                match ty.normalize() {
                                    Type::TypeLit(ty)
                                        if ty.members.iter().all(|element| match element {
                                            TypeElement::Property(..) => true,
                                            _ => false,
                                        }) =>
                                    {
                                        let mut members = vec![];

                                        for member in &ty.members {
                                            match member {
                                                TypeElement::Property(p) => members.push(
                                                    TypeElement::Property(PropertySignature {
                                                        type_ann: m.ty.clone().fold_with(
                                                            &mut MappedHandler {
                                                                analyzer: self.analyzer,
                                                                key: &p.key,
                                                                param_name: &param.name,
                                                                prop_ty: &*p
                                                                    .type_ann
                                                                    .clone()
                                                                    .unwrap_or_else(|| {
                                                                        Type::any(p.span)
                                                                    }),
                                                            },
                                                        ),
                                                        ..p.clone()
                                                    }),
                                                ),
                                                _ => {}
                                            }
                                        }

                                        return Type::TypeLit(TypeLit {
                                            span: ty.span,
                                            members,
                                        });
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                }

                // let m_ty = m.clone();

                // m.ty = m.ty.fold_with(&mut MappedHandler {
                //     analyzer: self.analyzer,
                //     ty: &m_ty,
                //     params: self.params,
                // });

                m = m.fold_with(self);

                match m.type_param.constraint {
                    Some(box Type::TypeLit(lit)) => {
                        let ty = m.ty.clone();

                        let members = lit
                            .members
                            .into_iter()
                            .map(|mut v| match v {
                                TypeElement::Property(ref mut p) => {
                                    p.type_ann = ty.clone();

                                    v
                                }
                                _ => todo!("type element other than property in a mapped type"),
                            })
                            .collect();
                        return Type::TypeLit(TypeLit { span, members });
                    }

                    Some(box Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: box Type::Union(ref u),
                        ..
                    })) => {
                        slog::error!(self.logger, "Union!");
                    }
                    _ => {}
                }

                m.ty = m.ty.map(|v| box v.foldable());
                m.ty = match m.ty {
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        obj_type,
                        index_type,
                    })) => {
                        let obj_type = box obj_type.foldable();
                        match *obj_type {
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for m in members {
                                    match m {
                                        TypeElement::Property(p) => {
                                            //
                                            new_members.push(TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span,
                                    members: new_members,
                                });
                            }

                            _ => Some(box Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly,
                                obj_type,
                                index_type,
                            })),
                        }
                    }
                    _ => m.ty,
                };

                if let Some(constraint) = &m.type_param.constraint {
                    match &**constraint {
                        Type::Operator(Operator {
                            span,
                            op: TsTypeOperatorOp::KeyOf,
                            ty,
                        }) => match &**ty {
                            Type::Keyword(..) => return *ty.clone(),
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    TypeElement::Method(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for member in members {
                                    match member {
                                        TypeElement::Method(method) => {
                                            new_members.push(TypeElement::Property(
                                                PropertySignature {
                                                    span: method.span,
                                                    readonly: method.readonly,
                                                    key: method.key.clone(),
                                                    computed: method.computed,
                                                    optional: method.optional,
                                                    params: vec![],
                                                    type_ann: m.ty.clone().map(|v| v),
                                                    type_params: None,
                                                },
                                            ));
                                        }
                                        TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(ty) = &m.ty {
                                                p.type_ann = Some(ty.clone());
                                            }
                                            //
                                            new_members.push(TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span: *span,
                                    members: new_members,
                                });
                            }
                            _ => {}
                        },

                        _ => {}
                    }
                }

                return Type::Mapped(m);
            }

            Type::This(..) | Type::Keyword(..) | Type::TypeLit(..) | Type::Lit(..) => {
                return ty.fold_children_with(self)
            }

            Type::Query(..)
            | Type::Operator(..)
            | Type::Tuple(..)
            | Type::Infer(..)
            | Type::Import(..)
            | Type::Predicate(..)
            | Type::Array(..)
            | Type::Union(..)
            | Type::Intersection(..)
            | Type::Function(..)
            | Type::Constructor(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Interface(..)
            | Type::Namespace(..)
            | Type::Module(..)
            | Type::ClassInstance(..)
            | Type::Optional(..)
            | Type::Rest(..)
            | Type::IndexedAccessType(..)
            | Type::Mapped(..) => return ty.fold_children_with(self),

            Type::Arc(a) => return (*a.ty).clone().fold_with(self),
        }
    }
}

struct MappedHandler<'a, 'b, 'c, 'd> {
    analyzer: &'a mut Analyzer<'b, 'c>,
    param_name: &'d Id,
    prop_ty: &'d Type,

    key: &'d RExpr,
}

impl Fold<Type> for MappedHandler<'_, '_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        match ty.normalize() {
            Type::IndexedAccessType(ty) => match ty.obj_type.normalize() {
                Type::Param(TypeParam {
                    name: obj_param_name,
                    ..
                }) => match ty.index_type.normalize() {
                    Type::Param(TypeParam {
                        name: index_param_name,
                        constraint: Some(index_type_constraint),
                        ..
                    }) => match index_type_constraint.normalize() {
                        Type::Operator(
                            operator
                            @
                            Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ..
                            },
                        ) => match operator.ty.normalize() {
                            Type::Param(constraint_param) => {
                                if *obj_param_name == constraint_param.name
                                    && *self.param_name == *obj_param_name
                                {
                                    return self.prop_ty.clone();
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }

        ty = ty.foldable();
        ty = ty.fold_children_with(self);

        ty
    }
}
