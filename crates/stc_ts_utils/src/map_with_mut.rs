use std::mem::replace;

use rnode::NodeId;
use stc_ts_ast_rnode::{
    RBool, RClassMember, RDecl, REmptyStmt, RExpr, RIdent, RInvalid, RModuleItem, RPat, RPropName,
    RStmt, RTsKeywordType, RTsLit, RTsType, RVarDecl,
};
use stc_ts_types::{LitType, Type};
use swc_atoms::js_word;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;

/// Helper for migration from [Fold] to [VisitMut]
pub trait MapWithMut: Sized {
    fn dummy() -> Self;

    fn take(&mut self) -> Self {
        replace(self, Self::dummy())
    }

    #[inline]
    fn map_with_mut<F>(&mut self, op: F)
    where
        F: FnOnce(Self) -> Self,
    {
        let dummy = Self::dummy();
        let v = replace(self, dummy);
        let v = op(v);
        let _dummy = replace(self, v);
    }
}

impl MapWithMut for Type {
    fn dummy() -> Self {
        Type::Lit(LitType {
            span: DUMMY_SP,
            lit: RTsLit::Bool(RBool {
                span: DUMMY_SP,
                value: false,
            }),
            metadata: Default::default(),
        })
    }
}

impl MapWithMut for RPat {
    fn dummy() -> Self {
        RPat::Invalid(RInvalid { span: DUMMY_SP })
    }
}

impl MapWithMut for RTsType {
    fn dummy() -> Self {
        RTsType::TsKeywordType(RTsKeywordType {
            span: DUMMY_SP,
            kind: TsKeywordTypeKind::TsNeverKeyword,
        })
    }
}

impl MapWithMut for RClassMember {
    fn dummy() -> Self {
        RClassMember::Empty(REmptyStmt { span: DUMMY_SP })
    }
}

impl MapWithMut for RExpr {
    fn dummy() -> Self {
        RExpr::Invalid(RInvalid { span: DUMMY_SP })
    }
}

impl MapWithMut for RIdent {
    fn dummy() -> Self {
        RIdent::new(js_word!(""), DUMMY_SP)
    }
}

impl MapWithMut for RPropName {
    fn dummy() -> Self {
        RPropName::Ident(RIdent::dummy())
    }
}

impl MapWithMut for RDecl {
    fn dummy() -> Self {
        RDecl::Var(box RVarDecl {
            node_id: NodeId::invalid(),
            span: DUMMY_SP,
            kind: VarDeclKind::Var,
            declare: false,
            decls: vec![],
        })
    }
}

impl<T> MapWithMut for Vec<T> {
    fn dummy() -> Self {
        Vec::new()
    }
}

impl MapWithMut for RStmt {
    fn dummy() -> Self {
        RStmt::Empty(REmptyStmt { span: DUMMY_SP })
    }
}

impl MapWithMut for RModuleItem {
    fn dummy() -> Self {
        RStmt::dummy().into()
    }
}
