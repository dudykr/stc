use super::Analyzer;
use super::Ctx;
use crate::ValidationResult;
use crate::{analyzer::generic::is_literals, ty, ty::Type, util::is_str_lit_or_union};
use rnode::Fold;
use rnode::FoldWith;
use rnode::Visit;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RStr;
use stc_ts_errors::Error;
use stc_ts_storage::Storage;
use stc_ts_types::TypeElement;
use stc_ts_types::{ClassInstance, Id, IndexedAccessType, Intersection, ModuleId, QueryExpr, QueryType, Ref, Tuple};
use std::iter::once;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;
use ty::TypeExt;

impl Analyzer<'_, '_> {
    /// `span` and `callee` is used only for error reporting.
    fn make_instance_from_type_elements(
        &mut self,
        span: Span,
        callee: &Type,
        elements: &[TypeElement],
    ) -> ValidationResult<Box<Type>> {
        for member in elements {
            match member {
                TypeElement::Constructor(c) => {
                    if let Some(ty) = &c.ret_ty {
                        return Ok(ty.clone());
                    }
                }
                _ => continue,
            }
        }

        Err(box Error::NoNewSignature {
            span,
            callee: box callee.clone(),
        })
    }

    /// Make instance of `ty`. In case of error, error will be reported to user
    /// and `ty` will be returned.
    ///
    ///
    /// TODO: Use Cow
    pub(super) fn make_instance_or_report(&mut self, span: Span, ty: &Type) -> Box<Type> {
        if span.is_dummy() {
            panic!("Cannot make an instance with dummy span")
        }

        let res = self.make_instance(span, ty);
        match res {
            Ok(ty) => ty,
            Err(err) => {
                self.storage.report(err);
                box ty.clone()
            }
        }
    }

    /// TODO: Use Cow
    pub(super) fn make_instance(&mut self, span: Span, ty: &Type) -> ValidationResult {
        let ty = ty.normalize();

        if ty.is_any() {
            return Ok(box ty.clone());
        }

        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
            return Ok(box ty.clone());
        }

        match ty {
            Type::Ref(..) => {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };
                let ty = self
                    .with_ctx(ctx)
                    .expand_fully(span, box ty.normalize().clone(), false)?;

                match ty.normalize() {
                    Type::Ref(..) => return Ok(ty.clone()),
                    _ => return self.make_instance(span, &ty),
                }
            }

            Type::TypeLit(type_lit) => {
                return self.make_instance_from_type_elements(span, ty, &type_lit.members);
            }

            Type::Interface(interface) => {
                let res = self.make_instance_from_type_elements(span, ty, &interface.body);
                let err = match res {
                    Ok(v) => return Ok(v),
                    Err(err) => err,
                };

                for parent in &interface.extends {
                    let ctxt = self.ctx.module_id;
                    let parent_ty = self.type_of_ts_entity_name(span, ctxt, &parent.expr, None)?;
                    if let Ok(ty) = self.make_instance(span, &parent_ty) {
                        return Ok(ty);
                    }
                }

                return Err(err);
            }

            Type::Class(..) => {
                return Ok(box Type::ClassInstance(ClassInstance {
                    span,
                    ty: box ty.clone(),
                    type_args: None,
                }))
            }

            _ => {}
        }

        Err(box Error::NoNewSignature {
            span,
            callee: box ty.clone(),
        })
    }
}

pub(crate) fn instantiate_class(module_id: ModuleId, ty: Box<Type>) -> Box<Type> {
    let span = ty.span();

    match *ty.normalize() {
        Type::Tuple(Tuple { ref elems, span }) => box Type::Tuple(Tuple {
            span,
            elems: elems
                .iter()
                .cloned()
                .map(|mut element| {
                    // TODO: Remove clone
                    element.ty = instantiate_class(module_id, element.ty);
                    element
                })
                .collect(),
        }),
        Type::Class(ref cls) => box Type::ClassInstance(ClassInstance {
            // TODO
            span,

            // TODO; Remove clone
            ty: box Type::Class(cls.clone()),

            // TODO
            type_args: None,
        }),

        Type::Intersection(ref i) => {
            let types = i
                .types
                .iter()
                .map(|ty| instantiate_class(module_id, ty.clone()))
                .collect();

            box Type::Intersection(Intersection { span: i.span, types })
        }

        Type::Query(QueryType {
            span,
            expr: box QueryExpr::TsEntityName(ref type_name),
        }) => box Type::Ref(Ref {
            span,
            ctxt: module_id,
            type_name: type_name.clone(),
            type_args: Default::default(),
        }),

        _ => return ty,
    }
}

#[derive(Debug, Default)]
pub(super) struct Generalizer {
    pub force: bool,
}

impl Fold<stc_ts_types::Function> for Generalizer {
    #[inline]
    fn fold(&mut self, node: ty::Function) -> ty::Function {
        node
    }
}

impl Fold<Type> for Generalizer {
    fn fold(&mut self, mut ty: Type) -> Type {
        match ty.normalize() {
            Type::IndexedAccessType(IndexedAccessType { index_type, .. }) if is_str_lit_or_union(&index_type) => {
                return ty
            }
            _ => {}
        }
        if !self.force {
            if is_literals(&ty) {
                return ty;
            }
        }

        let force = match ty {
            Type::TypeLit(..) => true,
            _ => false,
        };

        let old = self.force;
        self.force = force;
        ty = ty.fold_children_with(self);
        self.force = old;

        *ty.generalize_lit()
    }
}

impl Analyzer<'_, '_> {
    //    /// Validates and store errors if required.
    //    pub fn check<T, O>(&mut self, node: &T) -> Option<O>
    //    where
    //        Self: Validate<T, Output = Result<O, Error>>,
    //    {
    //        let res: Result<O, _> = self.validate_with(node);
    //        match res {
    //            Ok(v) => Some(v),
    //            Err(err) => {
    //                self.storage.report(err);
    //                None
    //            }
    //        }
    //    }
}

pub trait ResultExt<T>: Into<Result<T, Box<Error>>> {
    fn store<V>(self, to: &mut V) -> Option<T>
    where
        V: Extend<Box<Error>>,
    {
        match self.into() {
            Ok(val) => Some(val),
            Err(e) => {
                to.extend(once(e));
                None
            }
        }
    }

    fn report(self, storage: &mut Storage) -> Option<T> {
        match self.into() {
            Ok(v) => Some(v),
            Err(err) => {
                storage.report(err);
                None
            }
        }
    }
}

impl<T> ResultExt<T> for Result<T, Box<Error>> {}

/// Simple utility to check (l, r) and (r, l) with same code.
#[derive(Debug, Clone, Copy)]
pub(super) struct Comparator<T>
where
    T: Copy,
{
    pub left: T,
    pub right: T,
}

impl<T> Comparator<T>
where
    T: Copy,
{
    pub fn take_if_any_matches<F, R>(&self, mut op: F) -> Option<R>
    where
        F: FnMut(T, T) -> Option<R>,
    {
        op(self.left, self.right).or_else(|| op(self.right, self.left))
    }

    pub fn both<F>(&self, mut op: F) -> bool
    where
        F: FnMut(T) -> bool,
    {
        op(self.left) && op(self.right)
    }

    pub fn any<F>(&self, mut op: F) -> bool
    where
        F: FnMut(T) -> bool,
    {
        op(self.left) || op(self.right)
    }
}

pub(super) fn is_prop_name_eq(l: &RPropName, r: &RPropName) -> bool {
    macro_rules! check {
        ($l:expr, $r:expr) => {{
            let l = $l;
            let r = $r;

            match l {
                RPropName::Ident(RIdent { ref sym, .. }) | RPropName::Str(RStr { value: ref sym, .. }) => match &*r {
                    RPropName::Ident(RIdent { sym: ref r_sym, .. }) | RPropName::Str(RStr { value: ref r_sym, .. }) => {
                        return sym == r_sym
                    }
                    RPropName::Num(n) => return sym == &*n.value.to_string(),
                    _ => return false,
                },
                RPropName::Computed(..) => return false,
                _ => {}
            }
        }};
    }

    check!(l, r);
    check!(r, l);

    false
}

pub(super) struct VarVisitor<'a> {
    pub names: &'a mut Vec<Id>,
}

impl Visit<RExpr> for VarVisitor<'_> {
    fn visit(&mut self, _: &RExpr) {}
}

impl Visit<RIdent> for VarVisitor<'_> {
    fn visit(&mut self, i: &RIdent) {
        self.names.push(i.into())
    }
}
