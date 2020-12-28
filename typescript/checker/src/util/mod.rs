use crate::{
    ty,
    ty::{Class, FnParam, Intersection, Type, TypeElement, TypeParamInstantiation, Union},
};
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ast_rnode::RArrayPat;
use stc_ast_rnode::RAssignPat;
use stc_ast_rnode::RBlockStmt;
use stc_ast_rnode::RBool;
use stc_ast_rnode::RExpr;
use stc_ast_rnode::RIdent;
use stc_ast_rnode::RModuleDecl;
use stc_ast_rnode::RModuleItem;
use stc_ast_rnode::RObjectPat;
use stc_ast_rnode::RPat;
use stc_ast_rnode::RPropName;
use stc_ast_rnode::RRestPat;
use stc_ast_rnode::RStmt;
use stc_ast_rnode::RTsKeywordType;
use stc_ast_rnode::RTsLit;
use stc_ast_rnode::RTsLitType;
use stc_ast_rnode::RTsType;
use stc_ast_rnode::RTsTypeAnn;
use stc_types::{Id, InferType, TypeParam};
use swc_common::{Mark, Span, Spanned, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::{drop_span, ModuleItemLike, StmtLike};

pub(crate) mod dashmap;
pub(crate) mod graph;
pub(crate) mod map_with_mut;
pub(crate) mod named;
pub(crate) mod property_map;
pub(crate) mod type_ext;
pub(crate) trait ModuleItemOrStmt {
    fn try_into(self) -> Result<RModuleDecl, RStmt>;
}

impl ModuleItemOrStmt for RModuleItem {
    #[inline]
    fn try_into(self) -> Result<RModuleDecl, RStmt> {
        match self {
            RModuleItem::ModuleDecl(decl) => Ok(decl),
            RModuleItem::Stmt(stmt) => Err(stmt),
        }
    }
}

impl ModuleItemOrStmt for RStmt {
    #[inline]
    fn try_into(self) -> Result<RModuleDecl, RStmt> {
        Err(self)
    }
}

pub(crate) struct MarkFinder {
    found: bool,
    mark: Mark,
}

impl Visit<Type> for MarkFinder {
    fn visit(&mut self, ty: &Type) {
        if self.found {
            return;
        }
        ty.visit_children_with(self);

        let mut ctxt: SyntaxContext = ty.span().ctxt;

        loop {
            let mark = ctxt.remove_mark();
            if mark == Mark::root() {
                return;
            }

            if mark == self.mark {
                self.found = true;
                return;
            }
        }
    }
}

pub(crate) fn contains_mark<T>(n: &T, mark: Mark) -> bool
where
    T: VisitWith<MarkFinder>,
{
    let mut v = MarkFinder { found: false, mark };
    n.visit_with(&mut v);
    v.found
}

struct InferTypeFinder {
    found: bool,
}

impl Visit<InferType> for InferTypeFinder {
    fn visit(&mut self, _: &InferType) {
        self.found = true;
    }
}

pub(crate) fn contains_infer_type(n: &Type) -> bool {
    let mut v = InferTypeFinder { found: false };
    n.visit_with(&mut v);
    v.found
}

/// Applies `mark` to **all** types.
pub(crate) struct Marker {
    pub mark: Mark,
}

impl VisitMut<Span> for Marker {
    fn visit_mut(&mut self, span: &mut Span) {
        span.ctxt = span.ctxt.apply_mark(self.mark);
    }
}

impl VisitMut<Type> for Marker {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.normalize_mut();
        ty.visit_mut_children_with(self);
    }
}

/// TODO: Rename: `is_all_str_lit`
pub(crate) fn is_str_lit_or_union(t: &Type) -> bool {
    match t {
        Type::Lit(RTsLitType {
            lit: RTsLit::Str(..),
            ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_lit_or_union(&ty)),
        _ => false,
    }
}

pub(crate) fn is_str_or_union(t: &Type) -> bool {
    match t {
        Type::Lit(RTsLitType {
            lit: RTsLit::Str(..),
            ..
        }) => true,
        Type::Keyword(RTsKeywordType {
            kind: TsKeywordTypeKind::TsStringKeyword,
            ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_or_union(&ty)),
        _ => false,
    }
}

pub(crate) trait AsModuleDecl {
    const IS_MODULE_ITEM: bool;
    fn as_module_decl(&self) -> Result<&RModuleDecl, &RStmt>;
}

impl AsModuleDecl for RStmt {
    const IS_MODULE_ITEM: bool = false;

    fn as_module_decl(&self) -> Result<&RModuleDecl, &RStmt> {
        Err(self)
    }
}

impl AsModuleDecl for RModuleItem {
    const IS_MODULE_ITEM: bool = true;

    fn as_module_decl(&self) -> Result<&RModuleDecl, &RStmt> {
        match self {
            RModuleItem::ModuleDecl(decl) => Ok(decl),
            RModuleItem::Stmt(stmt) => Err(stmt),
        }
    }
}

/// TODO: Change it to return Box<Type>
pub(crate) trait RemoveTypes {
    /// Removes falsy values from `self`.
    fn remove_falsy(self) -> Type;

    /// Removes truthy values from `self`.
    fn remove_truthy(self) -> Type;
}

impl RemoveTypes for Type {
    fn remove_falsy(self) -> Type {
        match self {
            Type::Keyword(RTsKeywordType { kind, span }) => match kind {
                TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword => {
                    return *Type::never(span);
                }
                _ => {}
            },
            Type::Lit(RTsLitType {
                lit:
                    RTsLit::Bool(RBool {
                        value: false, span, ..
                    }),
                ..
            }) => return *Type::never(span),

            Type::Union(u) => return u.remove_falsy(),
            Type::Intersection(i) => return i.remove_falsy(),
            _ => {}
        }

        self
    }

    fn remove_truthy(self) -> Type {
        match self {
            Type::Lit(RTsLitType {
                lit:
                    RTsLit::Bool(RBool {
                        value: true, span, ..
                    }),
                ..
            }) => return *Type::never(span),

            Type::Union(u) => u.remove_truthy(),
            Type::Intersection(i) => i.remove_truthy(),
            _ => self,
        }
    }
}

impl RemoveTypes for Intersection {
    fn remove_falsy(self) -> Type {
        let types = self
            .types
            .into_iter()
            .map(|ty| box ty.remove_falsy())
            .collect::<Vec<_>>();

        if types.iter().any(|ty| ty.is_never()) {
            return *Type::never(self.span);
        }

        if types.len() == 1 {
            return *types.into_iter().next().unwrap();
        }

        Intersection {
            span: self.span,
            types,
        }
        .into()
    }

    fn remove_truthy(self) -> Type {
        let types = self
            .types
            .into_iter()
            .map(|ty| box ty.remove_truthy())
            .collect::<Vec<_>>();
        if types.iter().any(|ty| ty.is_never()) {
            return *Type::never(self.span);
        }

        if types.len() == 1 {
            return *types.into_iter().next().unwrap();
        }

        Intersection {
            span: self.span,
            types,
        }
        .into()
    }
}

impl RemoveTypes for Union {
    fn remove_falsy(self) -> Type {
        let types: Vec<_> = self
            .types
            .into_iter()
            .map(|ty| box ty.remove_falsy())
            .filter(|ty| !ty.is_never())
            .collect();

        if types.is_empty() {
            return *Type::never(self.span);
        }

        if types.len() == 1 {
            return *types.into_iter().next().unwrap();
        }

        Union {
            span: self.span,
            types,
        }
        .into()
    }

    fn remove_truthy(self) -> Type {
        let types: Vec<_> = self
            .types
            .into_iter()
            .map(|ty| box ty.remove_truthy())
            .filter(|ty| !ty.is_never())
            .collect();

        if types.is_empty() {
            return *Type::never(self.span);
        }

        if types.len() == 1 {
            return *types.into_iter().next().unwrap();
        }

        Union {
            span: self.span,
            types,
        }
        .into()
    }
}

impl<'a, T> RemoveTypes for Box<T>
where
    T: RemoveTypes,
{
    fn remove_falsy(self) -> Type {
        (*self).remove_falsy()
    }

    fn remove_truthy(self) -> Type {
        (*self).remove_truthy()
    }
}

pub(crate) trait EndsWithRet {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool;
}

impl EndsWithRet for RStmt {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        match *self {
            RStmt::Return(..) | RStmt::Break(..) | RStmt::Continue(..) | RStmt::Throw(..) => true,
            RStmt::Block(ref stmt) => stmt.ends_with_ret(),
            _ => false,
        }
    }
}

impl EndsWithRet for RBlockStmt {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        self.stmts.ends_with_ret()
    }
}

impl<T> EndsWithRet for Vec<T>
where
    T: EndsWithRet,
{
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        match self.last() {
            Some(ref stmt) => stmt.ends_with_ret(),
            _ => false,
        }
    }
}

pub trait PatExt {
    fn get_ty(&self) -> Option<&RTsType>;
    fn get_mut_ty(&mut self) -> Option<&mut RTsType>;
    fn set_ty(&mut self, ty: Option<Box<RTsType>>);
}

impl PatExt for RPat {
    fn get_ty(&self) -> Option<&RTsType> {
        match *self {
            RPat::Array(RArrayPat { ref type_ann, .. })
            | RPat::Assign(RAssignPat { ref type_ann, .. })
            | RPat::Ident(RIdent { ref type_ann, .. })
            | RPat::Object(RObjectPat { ref type_ann, .. })
            | RPat::Rest(RRestPat { ref type_ann, .. }) => {
                type_ann.as_ref().map(|ty| &*ty.type_ann)
            }

            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => {
                //Some(RTsType::TsKeywordType(RTsKeywordType {
                //    span: self.span(),
                //    kind: TsKeywordTypeKind::TsAnyKeyword,
                //}))
                None
            }

            _ => None,
        }
    }

    fn get_mut_ty(&mut self) -> Option<&mut RTsType> {
        match *self {
            RPat::Array(RArrayPat {
                ref mut type_ann, ..
            })
            | RPat::Assign(RAssignPat {
                ref mut type_ann, ..
            })
            | RPat::Ident(RIdent {
                ref mut type_ann, ..
            })
            | RPat::Object(RObjectPat {
                ref mut type_ann, ..
            })
            | RPat::Rest(RRestPat {
                ref mut type_ann, ..
            }) => type_ann.as_mut().map(|ty| &mut *ty.type_ann),

            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => None,

            _ => None,
        }
    }

    fn set_ty(&mut self, ty: Option<Box<RTsType>>) {
        match *self {
            RPat::Array(RArrayPat {
                ref mut type_ann, ..
            })
            | RPat::Assign(RAssignPat {
                ref mut type_ann, ..
            })
            | RPat::Ident(RIdent {
                ref mut type_ann, ..
            })
            | RPat::Object(RObjectPat {
                ref mut type_ann, ..
            })
            | RPat::Rest(RRestPat {
                ref mut type_ann, ..
            }) => {
                *type_ann = ty.map(|type_ann| RTsTypeAnn {
                    span: type_ann.span(),
                    type_ann,
                })
            }

            _ => {}
        }
    }
}

pub(crate) struct TypeParamFinder<'a> {
    name: &'a Id,
    found: bool,
}

pub(crate) fn contains_type_param<T>(node: &T, name: &Id) -> bool
where
    T: for<'a> rnode::VisitWith<TypeParamFinder<'a>>,
{
    let mut v = TypeParamFinder { name, found: false };

    name.visit_with(&mut v);

    v.found
}

impl rnode::Visit<TypeParam> for TypeParamFinder<'_> {
    fn visit(&mut self, p: &TypeParam) {
        if p.name == *self.name {
            self.found = true
        } else {
            p.visit_children_with(self)
        }
    }
}

/// Finds all idents of variable
pub struct DestructuringFinder<'a, I: From<RIdent>> {
    pub found: &'a mut Vec<I>,
}

pub fn find_ids_in_pat<T, I: From<RIdent>>(node: &T) -> Vec<I>
where
    T: for<'any> VisitWith<DestructuringFinder<'any, I>>,
{
    let mut found = vec![];

    {
        let mut v = DestructuringFinder { found: &mut found };
        node.visit_with(&mut v);
    }

    found
}

/// No-op (we don't care about expressions)
impl<I: From<RIdent>> Visit<RExpr> for DestructuringFinder<'_, I> {
    fn visit(&mut self, _: &RExpr) {}
}

/// No-op (we don't care about expressions)
impl<I: From<RIdent>> Visit<RPropName> for DestructuringFinder<'_, I> {
    fn visit(&mut self, _: &RPropName) {}
}

impl<'a, I: From<RIdent>> Visit<RIdent> for DestructuringFinder<'a, I> {
    fn visit(&mut self, i: &RIdent) {
        self.found.push(i.clone().into());
    }
}
