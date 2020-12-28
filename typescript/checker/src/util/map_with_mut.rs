use stc_ast_rnode::RBool;
use stc_ast_rnode::RClassMember;
use stc_ast_rnode::RDecl;
use stc_ast_rnode::REmptyStmt;
use stc_ast_rnode::RExpr;
use stc_ast_rnode::RIdent;
use stc_ast_rnode::RInvalid;
use stc_ast_rnode::RModuleItem;
use stc_ast_rnode::RPat;
use stc_ast_rnode::RPropName;
use stc_ast_rnode::RStmt;
use stc_ast_rnode::RTsKeywordType;
use stc_ast_rnode::RTsLit;
use stc_ast_rnode::RTsLitType;
use stc_ast_rnode::RTsType;
use stc_ast_rnode::RVarDecl;
use stc_types::Type;
use std::mem::replace;
use swc_atoms::js_word;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;

/// Helper for migration from [Fold] to [VisitMut]
pub(crate) trait MapWithMut: Sized {
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
        Type::Lit(RTsLitType {
            span: DUMMY_SP,
            lit: RTsLit::Bool(RBool {
                span: DUMMY_SP,
                value: false,
            }),
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
        RDecl::Var(RVarDecl {
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
