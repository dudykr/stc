use std::fmt::Debug;

use crate::ty::{Intersection, Type, Union};
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ts_ast_rnode::RBlockStmt;
use stc_ts_ast_rnode::RBool;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_types::{Id, InferType, TypeParam};
use swc_common::{Mark, Span, Spanned, SyntaxContext};
use swc_ecma_ast::*;

pub(crate) mod dashmap;
pub(crate) mod graph;
pub(crate) mod named;
pub(crate) mod property_map;
pub(crate) mod type_ext;

pub(crate) struct TypeParamAssertFinder {
    found: bool,
}

impl Visit<TypeParam> for TypeParamAssertFinder {
    fn visit(&mut self, value: &TypeParam) {
        self.found = true;
    }
}

pub(crate) fn assert_no_type_param<N>(n: &N)
where
    N: Debug + VisitWith<TypeParamAssertFinder>,
{
    let mut v = TypeParamAssertFinder { found: false };
    n.visit_with(&mut v);
    if v.found {
        panic!("{:#?} should not contain type parameter", n)
    }
}

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

/// Prevent interop with hygiene.
impl VisitMut<RIdent> for Marker {
    fn visit_mut(&mut self, _: &mut RIdent) {}
}

impl VisitMut<Type> for Marker {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.normalize_mut();
        ty.visit_mut_children_with(self);
    }
}

pub(crate) fn is_str_or_union(t: &Type) -> bool {
    match t.normalize() {
        Type::Lit(RTsLitType {
            lit: RTsLit::Str(..), ..
        }) => true,
        Type::Keyword(RTsKeywordType {
            kind: TsKeywordTypeKind::TsStringKeyword,
            ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_or_union(&ty)),
        _ => false,
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
                    return Type::never(span);
                }
                _ => {}
            },
            Type::Lit(RTsLitType {
                lit: RTsLit::Bool(RBool { value: false, span, .. }),
                ..
            }) => return Type::never(span),

            Type::Union(u) => return u.remove_falsy(),
            Type::Intersection(i) => return i.remove_falsy(),
            _ => {}
        }

        self
    }

    fn remove_truthy(self) -> Type {
        match self {
            Type::Lit(RTsLitType {
                lit: RTsLit::Bool(RBool { value: true, span, .. }),
                ..
            }) => return Type::never(span),

            Type::Union(u) => u.remove_truthy(),
            Type::Intersection(i) => i.remove_truthy(),
            _ => self,
        }
    }
}

impl RemoveTypes for Intersection {
    fn remove_falsy(self) -> Type {
        let types = self.types.into_iter().map(|ty| ty.remove_falsy()).collect::<Vec<_>>();

        if types.iter().any(|ty| ty.is_never()) {
            return Type::never(self.span);
        }

        if types.len() == 1 {
            return types.into_iter().next().unwrap();
        }

        Intersection { span: self.span, types }.into()
    }

    fn remove_truthy(self) -> Type {
        let types = self.types.into_iter().map(|ty| ty.remove_truthy()).collect::<Vec<_>>();
        if types.iter().any(|ty| ty.is_never()) {
            return Type::never(self.span);
        }

        if types.len() == 1 {
            return types.into_iter().next().unwrap();
        }

        Intersection { span: self.span, types }.into()
    }
}

impl RemoveTypes for Union {
    fn remove_falsy(self) -> Type {
        let types: Vec<_> = self
            .types
            .into_iter()
            .map(|ty| ty.remove_falsy())
            .filter(|ty| !ty.is_never())
            .collect();

        if types.is_empty() {
            return Type::never(self.span);
        }

        if types.len() == 1 {
            return types.into_iter().next().unwrap();
        }

        Union { span: self.span, types }.into()
    }

    fn remove_truthy(self) -> Type {
        let types: Vec<_> = self
            .types
            .into_iter()
            .map(|ty| ty.remove_truthy())
            .filter(|ty| !ty.is_never())
            .collect();

        if types.is_empty() {
            return Type::never(self.span);
        }

        if types.len() == 1 {
            return types.into_iter().next().unwrap();
        }

        Union { span: self.span, types }.into()
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
