use crate::{
    ty,
    ty::{Class, FnParam, Intersection, Type, TypeElement, TypeParamInstantiation, Union},
};
use stc_types::VisitMutWith;
use stc_types::{Id, InferType, TypeNode, TypeParam, Visit, VisitMut, VisitWith};
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
    fn try_into(self) -> Result<ModuleDecl, Stmt>;
}

impl ModuleItemOrStmt for ModuleItem {
    #[inline]
    fn try_into(self) -> Result<ModuleDecl, Stmt> {
        match self {
            ModuleItem::ModuleDecl(decl) => Ok(decl),
            ModuleItem::Stmt(stmt) => Err(stmt),
        }
    }
}

impl ModuleItemOrStmt for Stmt {
    #[inline]
    fn try_into(self) -> Result<ModuleDecl, Stmt> {
        Err(self)
    }
}

pub(crate) struct MarkFinder {
    found: bool,
    mark: Mark,
}

impl Visit for MarkFinder {
    fn visit_type(&mut self, ty: &Type, _: &dyn TypeNode) {
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
    n.visit_with(n, &mut v);
    v.found
}

struct InferTypeFinder {
    found: bool,
}

impl Visit for InferTypeFinder {
    fn visit_infer_type(&mut self, _: &InferType, _: &dyn TypeNode) {
        self.found = true;
    }
}

pub(crate) fn contains_infer_type(n: &Type) -> bool {
    let mut v = InferTypeFinder { found: false };
    n.visit_with(n, &mut v);
    v.found
}

/// Applies `mark` to **all** types.
pub(crate) struct Marker {
    pub mark: Mark,
}

impl VisitMut for Marker {
    fn visit_mut_span(&mut self, span: &mut Span) {
        span.ctxt = span.ctxt.apply_mark(self.mark);
    }

    fn visit_mut_type(&mut self, ty: &mut Type) {
        ty.normalize_mut();
        ty.visit_mut_children_with(self);
    }
}

/// TODO: Rename: `is_all_str_lit`
pub(crate) fn is_str_lit_or_union(t: &Type) -> bool {
    match t {
        Type::Lit(TsLitType {
            lit: TsLit::Str(..),
            ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_lit_or_union(&ty)),
        _ => false,
    }
}

pub(crate) fn is_str_or_union(t: &Type) -> bool {
    match t {
        Type::Lit(TsLitType {
            lit: TsLit::Str(..),
            ..
        }) => true,
        Type::Keyword(TsKeywordType {
            kind: TsKeywordTypeKind::TsStringKeyword,
            ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_or_union(&ty)),
        _ => false,
    }
}

pub(crate) trait AsModuleDecl: StmtLike + ModuleItemLike {
    const IS_MODULE_ITEM: bool;
    fn as_module_decl(&self) -> Result<&ModuleDecl, &Stmt>;
}

impl AsModuleDecl for Stmt {
    const IS_MODULE_ITEM: bool = false;

    fn as_module_decl(&self) -> Result<&ModuleDecl, &Stmt> {
        Err(self)
    }
}

impl AsModuleDecl for ModuleItem {
    const IS_MODULE_ITEM: bool = true;

    fn as_module_decl(&self) -> Result<&ModuleDecl, &Stmt> {
        match self {
            ModuleItem::ModuleDecl(decl) => Ok(decl),
            ModuleItem::Stmt(stmt) => Err(stmt),
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
            Type::Keyword(TsKeywordType { kind, span }) => match kind {
                TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword => {
                    return *Type::never(span);
                }
                _ => {}
            },
            Type::Lit(TsLitType {
                lit:
                    TsLit::Bool(Bool {
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
            Type::Lit(TsLitType {
                lit: TsLit::Bool(Bool {
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

impl EndsWithRet for Stmt {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        match *self {
            Stmt::Return(..) | Stmt::Break(..) | Stmt::Continue(..) | Stmt::Throw(..) => true,
            Stmt::Block(ref stmt) => stmt.ends_with_ret(),
            _ => false,
        }
    }
}

impl EndsWithRet for BlockStmt {
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
    fn get_ty(&self) -> Option<&TsType>;
    fn get_mut_ty(&mut self) -> Option<&mut TsType>;
    fn set_ty(&mut self, ty: Option<Box<TsType>>);
}

impl PatExt for Pat {
    fn get_ty(&self) -> Option<&TsType> {
        match *self {
            Pat::Array(ArrayPat { ref type_ann, .. })
            | Pat::Assign(AssignPat { ref type_ann, .. })
            | Pat::Ident(Ident { ref type_ann, .. })
            | Pat::Object(ObjectPat { ref type_ann, .. })
            | Pat::Rest(RestPat { ref type_ann, .. }) => type_ann.as_ref().map(|ty| &*ty.type_ann),

            Pat::Invalid(..) | Pat::Expr(box Expr::Invalid(..)) => {
                //Some(TsType::TsKeywordType(TsKeywordType {
                //    span: self.span(),
                //    kind: TsKeywordTypeKind::TsAnyKeyword,
                //}))
                None
            }

            _ => None,
        }
    }

    fn get_mut_ty(&mut self) -> Option<&mut TsType> {
        match *self {
            Pat::Array(ArrayPat {
                ref mut type_ann, ..
            })
            | Pat::Assign(AssignPat {
                ref mut type_ann, ..
            })
            | Pat::Ident(Ident {
                ref mut type_ann, ..
            })
            | Pat::Object(ObjectPat {
                ref mut type_ann, ..
            })
            | Pat::Rest(RestPat {
                ref mut type_ann, ..
            }) => type_ann.as_mut().map(|ty| &mut *ty.type_ann),

            Pat::Invalid(..) | Pat::Expr(box Expr::Invalid(..)) => None,

            _ => None,
        }
    }

    fn set_ty(&mut self, ty: Option<Box<TsType>>) {
        match *self {
            Pat::Array(ArrayPat {
                ref mut type_ann, ..
            })
            | Pat::Assign(AssignPat {
                ref mut type_ann, ..
            })
            | Pat::Ident(Ident {
                ref mut type_ann, ..
            })
            | Pat::Object(ObjectPat {
                ref mut type_ann, ..
            })
            | Pat::Rest(RestPat {
                ref mut type_ann, ..
            }) => {
                *type_ann = ty.map(|type_ann| TsTypeAnn {
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
    T: for<'a> stc_types::VisitWith<TypeParamFinder<'a>>,
{
    let mut v = TypeParamFinder { name, found: false };

    name.visit_with(&node, &mut v);

    v.found
}

impl stc_types::Visit for TypeParamFinder<'_> {
    fn visit_type_param(&mut self, p: &TypeParam, _: &dyn TypeNode) {
        if p.name == *self.name {
            self.found = true
        } else {
            p.visit_children_with(self)
        }
    }
}
