use crate::analyzer::assign::AssignOpts;
use crate::analyzer::expr::TypeOfMode;
use crate::{
    analyzer::{Analyzer, Ctx},
    ty::{Array, IndexedAccessType, Mapped, Operator, PropertySignature, Ref, Type, TypeElement, TypeLit},
    ValidationResult,
};
use fxhash::{FxHashMap, FxHashSet};
use rnode::Fold;
use rnode::FoldWith;
use rnode::VisitWith;
use slog::Logger;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_generics::type_param::finder::TypeParamUsageFinder;
use stc_ts_type_ops::Fix;
use stc_ts_types::Function;
use stc_ts_types::IdCtx;
use stc_ts_types::Interface;
use stc_ts_types::Key;
use stc_ts_types::TypeParamDecl;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::Union;
use stc_ts_types::{Id, TypeParam};
use stc_utils::error::context;
use stc_utils::ext::SpanExt;
use stc_utils::stack;
use swc_atoms::js_word;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_ecma_ast::*;

/// All fields default to false.
#[derive(Debug, Clone, Copy, Default)]
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
    pub(in super::super) fn instantiate_type_params_using_args(
        &mut self,
        span: Span,
        type_params: &TypeParamDecl,
        type_args: &TypeParamInstantiation,
    ) -> ValidationResult<FxHashMap<Id, Type>> {
        let mut params = FxHashMap::default();

        for (idx, param) in type_params.params.iter().enumerate() {
            if let Some(arg) = type_args.params.get(idx) {
                // TODO: Change this to assert.
                let arg = arg.clone().cheap();
                params.insert(param.name.clone(), arg);
            } else {
                if let Some(default) = &param.default {
                    let default = default.clone().cheap();
                    params.insert(param.name.clone(), default.clone());
                } else {
                    unimplemented!(
                        "Reporting errors when type parameter count and type argument count \
                         difffers\nParams={:#?}\nArgs: {:#?}",
                        type_params,
                        type_args
                    )
                }
            }
        }

        Ok(params)
    }

    pub(in super::super) fn expand_type_params<T>(&mut self, params: &FxHashMap<Id, Type>, ty: T) -> ValidationResult<T>
    where
        T: for<'aa, 'bb, 'cc, 'dd> FoldWith<GenericExpander<'aa, 'bb, 'cc, 'dd>> + Fix,
    {
        let ty = self.expand_type_params_inner(params, ty, false)?.fixed();
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
    fn expand_type_params_inner<T>(&mut self, params: &FxHashMap<Id, Type>, ty: T, fully: bool) -> ValidationResult<T>
    where
        T: for<'aa, 'bb, 'cc, 'dd> FoldWith<GenericExpander<'aa, 'bb, 'cc, 'dd>>,
    {
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
    pub(crate) fn extends(&mut self, span: Span, opts: ExtendsOpts, child: &Type, parent: &Type) -> Option<bool> {
        let child = child.normalize();
        let parent = parent.normalize();

        if child.type_eq(&parent) {
            return Some(true);
        }

        slog::debug!(
            self.logger,
            "[generic/extends] Checking if {} extends {}",
            dump_type_as_string(&self.cm, &child),
            dump_type_as_string(&self.cm, &parent),
        );

        match child {
            Type::Param(TypeParam {
                constraint: Some(child),
                ..
            }) => {
                if let Some(v) = self.extends(span, opts, child, parent) {
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
                    .expand_fully(child.span(), child.clone(), true)
                    .unwrap();
                match child.normalize() {
                    Type::Ref(..) => return None,
                    _ => {}
                }

                return self.extends(span, opts, &child, parent);
            }

            Type::Union(child) => {
                let mut prev = None;

                for child in &child.types {
                    let res = self.extends(span, opts, child, parent)?;

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
                let parent = self
                    .with_ctx(ctx)
                    .expand_fully(parent.span(), parent.clone(), true)
                    .unwrap();
                match parent.normalize() {
                    Type::Ref(..) => return None,
                    _ => {}
                }

                return self.extends(span, opts, child, &parent);
            }
            _ => {}
        }

        match parent {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            }) => return Some(false),
            Type::Union(parent) => {
                let mut has_false = false;

                for parent in &parent.types {
                    let res = self.extends(span, opts, child, parent);
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
                Type::Class(..) => return Some(false),
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
                                if let Some(false) = self.extends(span, opts, child, grand_parent) {
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
                        self.extends(span, opts, &child_element.ty, &parent_array.elem_type) == Some(true)
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
        // dbg!(child, parent);

        match self.assign_with_opts(
            &mut Default::default(),
            AssignOpts {
                span,
                disallow_different_classes: opts.disallow_different_classes,
                ..Default::default()
            },
            parent,
            child,
        ) {
            Ok(()) => Some(true),
            _ => Some(false),
        }
    }
}

/// This struct does not expands ref to other thpe. See Analyzer.expand to do
/// such operation.
pub(crate) struct GenericExpander<'a, 'b, 'c, 'd> {
    logger: Logger,
    analyzer: &'a mut Analyzer<'b, 'c>,
    params: &'d FxHashMap<Id, Type>,
    /// Expand fully?
    fully: bool,
    dejavu: FxHashSet<Id>,
}

impl Fold<Type> for GenericExpander<'_, '_, '_, '_> {
    fn fold(&mut self, ty: Type) -> Type {
        let _stack = match stack::track(ty.span()) {
            Ok(v) => v,
            _ => {
                slog::error!(
                    self.logger,
                    "[generic/expander] Stack overflow: {}",
                    dump_type_as_string(&self.analyzer.cm, &ty)
                );
                return ty;
            }
        };
        let _context = context(format!(
            "Expanding generics of {}",
            dump_type_as_string(&self.analyzer.cm, &ty)
        ));

        let old_fully = self.fully;
        self.fully |= match ty.normalize() {
            Type::Mapped(..) => true,
            _ => false,
        };
        let span = ty.span();

        {
            let mut v = TypeParamUsageFinder::default();
            ty.visit_with(&mut v);
            let will_expand = v.params.iter().any(|param| self.params.contains_key(&param.name));
            if !will_expand {
                return ty;
            }
        }

        slog::debug!(
            self.logger,
            "[generic/expander]: Expanding {}",
            dump_type_as_string(&self.analyzer.cm, &ty)
        );
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
                        elem_type: box type_args
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

                return ty.fold_children_with(self);
            }

            Type::Instance(..) | Type::Ref(..) => return ty.fold_children_with(self),

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
                        "generic_expand: Expanding type parameter `{}` => {}",
                        param.name,
                        dump_type_as_string(&self.analyzer.cm, &ty)
                    );

                    // If it's not self-referential, we fold it again.

                    self.dejavu.insert(param.name.clone());
                    let ty = ty.clone().fold_with(self);
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

                return *alias.ty;
            }

            Type::Interface(mut i) => {
                i = i.fold_with(self);

                return Type::Interface(i);
            }
            Type::Class(mut c) => {
                c = c.fold_with(self);

                if let Some(..) = &c.def.type_params {
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
                                            TypeElement::Property(..) | TypeElement::Method(..) => true,
                                            _ => false,
                                        }) =>
                                    {
                                        let mut members = vec![];

                                        for member in &ty.members {
                                            match member {
                                                TypeElement::Property(p) => {
                                                    members.push(TypeElement::Property(PropertySignature {
                                                        type_ann: m.ty.clone().fold_with(&mut MappedHandler {
                                                            analyzer: self.analyzer,
                                                            key: &p.key,
                                                            param_name: &param.name,
                                                            prop_ty: &*p
                                                                .type_ann
                                                                .clone()
                                                                .unwrap_or_else(|| box Type::any(p.span)),
                                                        }),
                                                        ..p.clone()
                                                    }))
                                                }
                                                TypeElement::Method(method) => {
                                                    members.push(TypeElement::Property(PropertySignature {
                                                        span: method.span,
                                                        accessibility: None,
                                                        readonly: method.readonly,
                                                        key: method.key.clone(),
                                                        optional: method.optional,
                                                        params: Default::default(),
                                                        type_ann: m.ty.clone().fold_with(&mut MappedHandler {
                                                            analyzer: self.analyzer,
                                                            key: &method.key,
                                                            param_name: &param.name,
                                                            prop_ty: &Type::Function(Function {
                                                                span: method.span,
                                                                type_params: method.type_params.clone(),
                                                                params: method.params.clone(),
                                                                ret_ty: method
                                                                    .ret_ty
                                                                    .clone()
                                                                    .unwrap_or_else(|| box Type::any(method.span)),
                                                            }),
                                                        }),
                                                        type_params: Default::default(),
                                                    }))
                                                }
                                                _ => {}
                                            }
                                        }

                                        for member in &mut members {
                                            self.analyzer.apply_mapped_flags(member, m.optional, m.readonly);
                                        }

                                        return Type::TypeLit(TypeLit {
                                            span: ty.span,
                                            members,
                                            metadata: ty.metadata,
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

                        let mut members = lit
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

                        for member in &mut members {
                            self.analyzer.apply_mapped_flags(member, m.optional, m.readonly);
                        }

                        return Type::TypeLit(TypeLit {
                            span,
                            members,
                            metadata: lit.metadata,
                        });
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
                            Type::TypeLit(TypeLit {
                                span,
                                members,
                                metadata,
                                ..
                            }) if members.iter().all(|m| match m {
                                TypeElement::Property(_) => true,
                                TypeElement::Method(_) => true,
                                _ => false,
                            }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for m in members {
                                    match m {
                                        TypeElement::Property(..) | TypeElement::Method(..) => {
                                            new_members.push(m);
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                for member in &mut new_members {
                                    self.analyzer.apply_mapped_flags(member, m.optional, m.readonly);
                                }

                                return Type::TypeLit(TypeLit {
                                    span,
                                    members: new_members,
                                    metadata,
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
                        }) => match ty.normalize() {
                            Type::Keyword(..) if m.optional == None && m.readonly == None => return *ty.clone(),
                            Type::TypeLit(TypeLit {
                                span,
                                members,
                                metadata: ty_metadata,
                                ..
                            }) if members.iter().all(|m| match m {
                                TypeElement::Property(_) => true,
                                TypeElement::Method(_) => true,
                                _ => false,
                            }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for member in members {
                                    match member {
                                        TypeElement::Method(method) => {
                                            new_members.push(TypeElement::Property(PropertySignature {
                                                span: method.span,
                                                accessibility: None,
                                                readonly: method.readonly,
                                                key: method.key.clone(),
                                                optional: method.optional,
                                                params: vec![],
                                                type_ann: m.ty.clone().map(|v| v),
                                                type_params: None,
                                            }));
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

                                for member in &mut new_members {
                                    self.analyzer.apply_mapped_flags(member, m.optional, m.readonly);
                                }

                                return Type::TypeLit(TypeLit {
                                    span: *span,
                                    members: new_members,
                                    metadata: *ty_metadata,
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

            Type::IndexedAccessType(ty) => {
                let mut ty = ty.fold_with(self);
                ty.obj_type.fix();

                let key = match ty.index_type.normalize() {
                    Type::Lit(RTsLitType {
                        lit: RTsLit::Str(s), ..
                    }) => Some(Key::Normal {
                        span: s.span,
                        sym: s.value.clone(),
                    }),
                    Type::Lit(RTsLitType {
                        lit: RTsLit::Number(v), ..
                    }) => Some(Key::Num(v.clone())),
                    _ => None,
                };

                let key = match key {
                    Some(v) => v,
                    None => return Type::IndexedAccessType(ty),
                };

                let span = key.span().or_else(|| ty.obj_type.span()).or_else(|| ty.span());

                if let Ok(prop_ty) =
                    self.analyzer
                        .access_property(span, &ty.obj_type, &key, TypeOfMode::RValue, IdCtx::Var)
                {
                    return prop_ty;
                }

                Type::IndexedAccessType(ty)
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
            | Type::Namespace(..)
            | Type::Module(..)
            | Type::ClassDef(..)
            | Type::Optional(..)
            | Type::Rest(..)
            | Type::Mapped(..)
            | Type::Tpl(..) => return ty.fold_children_with(self),

            Type::Arc(a) => return (*a.ty).clone().fold_with(self),
        }
    }
}

/// Override to handle some edge cases.
///
/// For inputs like `T | PromiseLike<T>` where `T` = `T | PromiseLike<T>`, we
/// should expand it to `T | PromiseLike<T>`.
impl Fold<Union> for GenericExpander<'_, '_, '_, '_> {
    fn fold(&mut self, u: Union) -> Union {
        let u = u.fold_children_with(self);

        {
            // TODO: Handle recursive types.
        }

        u
    }
}

struct MappedHandler<'a, 'b, 'c, 'd> {
    analyzer: &'a mut Analyzer<'b, 'c>,
    param_name: &'d Id,
    prop_ty: &'d Type,

    key: &'d Key,
}

impl Fold<Type> for MappedHandler<'_, '_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        match ty.normalize() {
            Type::IndexedAccessType(ty) => match ty.obj_type.normalize() {
                Type::Param(TypeParam {
                    name: obj_param_name, ..
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
                                if *obj_param_name == constraint_param.name && *self.param_name == *obj_param_name {
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
