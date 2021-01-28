#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(box_syntax)]
#![feature(box_patterns)]

pub use self::comments::StcComments;
pub use self::map_with_mut::MapWithMut;
use rnode::NodeId;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RAssignPat;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeAnn;
use swc_common::Spanned;

mod comments;
mod map_with_mut;

pub trait HasNodeId {
    fn node_id(&self) -> Option<NodeId>;
}

impl HasNodeId for RStmt {
    fn node_id(&self) -> Option<NodeId> {
        Some(match self {
            RStmt::Block(s) => s.node_id,
            RStmt::Empty(..) => return None,
            RStmt::Debugger(s) => s.node_id,
            RStmt::With(s) => s.node_id,
            RStmt::Return(s) => s.node_id,
            RStmt::Labeled(s) => s.node_id,
            RStmt::Break(s) => s.node_id,
            RStmt::Continue(s) => s.node_id,
            RStmt::If(s) => s.node_id,
            RStmt::Switch(s) => s.node_id,
            RStmt::Throw(s) => s.node_id,
            RStmt::Try(s) => s.node_id,
            RStmt::While(s) => s.node_id,
            RStmt::DoWhile(s) => s.node_id,
            RStmt::For(s) => s.node_id,
            RStmt::ForIn(s) => s.node_id,
            RStmt::ForOf(s) => s.node_id,
            RStmt::Decl(s) => return s.node_id(),
            RStmt::Expr(s) => s.node_id,
        })
    }
}

impl HasNodeId for RDecl {
    fn node_id(&self) -> Option<NodeId> {
        Some(match self {
            RDecl::Class(d) => d.node_id,
            RDecl::Fn(d) => d.node_id,
            RDecl::Var(d) => d.node_id,
            RDecl::TsInterface(d) => d.node_id,
            RDecl::TsTypeAlias(d) => d.node_id,
            RDecl::TsEnum(d) => d.node_id,
            RDecl::TsModule(d) => d.node_id,
        })
    }
}

impl HasNodeId for RModuleItem {
    fn node_id(&self) -> Option<NodeId> {
        match self {
            RModuleItem::ModuleDecl(d) => d.node_id(),
            RModuleItem::Stmt(s) => s.node_id(),
        }
    }
}

impl HasNodeId for RModuleDecl {
    fn node_id(&self) -> Option<NodeId> {
        Some(match self {
            RModuleDecl::Import(d) => d.node_id,
            RModuleDecl::ExportDecl(d) => d.node_id,
            RModuleDecl::ExportNamed(d) => d.node_id,
            RModuleDecl::ExportDefaultDecl(d) => d.node_id,
            RModuleDecl::ExportDefaultExpr(d) => d.node_id,
            RModuleDecl::ExportAll(d) => d.node_id,
            RModuleDecl::TsImportEquals(d) => d.node_id,
            RModuleDecl::TsExportAssignment(d) => d.node_id,
            RModuleDecl::TsNamespaceExport(d) => d.node_id,
        })
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

pub trait PatExt {
    fn get_ty(&self) -> Option<&RTsType>;
    fn get_mut_ty(&mut self) -> Option<&mut RTsType>;
    fn set_ty(&mut self, ty: Option<Box<RTsType>>);
    fn node_id(&self) -> Option<NodeId>;
}

impl PatExt for RPat {
    fn get_ty(&self) -> Option<&RTsType> {
        match *self {
            RPat::Array(RArrayPat { ref type_ann, .. })
            | RPat::Assign(RAssignPat { ref type_ann, .. })
            | RPat::Ident(RIdent { ref type_ann, .. })
            | RPat::Object(RObjectPat { ref type_ann, .. })
            | RPat::Rest(RRestPat { ref type_ann, .. }) => type_ann.as_ref().map(|ty| &*ty.type_ann),

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
            RPat::Array(RArrayPat { ref mut type_ann, .. })
            | RPat::Assign(RAssignPat { ref mut type_ann, .. })
            | RPat::Ident(RIdent { ref mut type_ann, .. })
            | RPat::Object(RObjectPat { ref mut type_ann, .. })
            | RPat::Rest(RRestPat { ref mut type_ann, .. }) => type_ann.as_mut().map(|ty| &mut *ty.type_ann),

            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => None,

            _ => None,
        }
    }

    fn set_ty(&mut self, ty: Option<Box<RTsType>>) {
        match *self {
            RPat::Array(RArrayPat { ref mut type_ann, .. })
            | RPat::Assign(RAssignPat { ref mut type_ann, .. })
            | RPat::Ident(RIdent { ref mut type_ann, .. })
            | RPat::Object(RObjectPat { ref mut type_ann, .. })
            | RPat::Rest(RRestPat { ref mut type_ann, .. }) => {
                *type_ann = ty.map(|type_ann| RTsTypeAnn {
                    node_id: NodeId::invalid(),
                    span: type_ann.span(),
                    type_ann,
                })
            }

            _ => {}
        }
    }

    fn node_id(&self) -> Option<NodeId> {
        Some(match self {
            RPat::Ident(i) => i.node_id,
            RPat::Array(a) => a.node_id,
            RPat::Rest(r) => r.node_id,
            RPat::Object(o) => o.node_id,
            RPat::Assign(a) => a.node_id,
            RPat::Invalid(_) => return None,
            RPat::Expr(_) => return None,
        })
    }
}

pub trait OptionExt<T>: Sized {
    fn as_mut_to_opt(&mut self) -> &mut Option<T>;

    fn fill_with<F>(&mut self, op: F)
    where
        F: FnOnce() -> T,
    {
        let opt = self.as_mut_to_opt();
        match opt {
            Some(..) => {}
            None => *opt = Some(op()),
        }
    }
}

impl<T> OptionExt<T> for Option<T> {
    fn as_mut_to_opt(&mut self) -> &mut Option<T> {
        self
    }
}
