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
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeAnn;
use swc_common::Spanned;

mod comments;
mod map_with_mut;

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
                    node_id: NodeId::invalid(),
                    span: type_ann.span(),
                    type_ann,
                })
            }

            _ => {}
        }
    }
}
