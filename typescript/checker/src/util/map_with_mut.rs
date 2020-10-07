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
        Type::Lit(TsLitType {
            span: DUMMY_SP,
            lit: TsLit::Bool(Bool {
                span: DUMMY_SP,
                value: false,
            }),
        })
    }
}

impl MapWithMut for Pat {
    fn dummy() -> Self {
        Pat::Invalid(Invalid { span: DUMMY_SP })
    }
}

impl MapWithMut for TsType {
    fn dummy() -> Self {
        TsType::TsKeywordType(TsKeywordType {
            span: DUMMY_SP,
            kind: TsKeywordTypeKind::TsNeverKeyword,
        })
    }
}

impl MapWithMut for ClassMember {
    fn dummy() -> Self {
        ClassMember::Empty(EmptyStmt { span: DUMMY_SP })
    }
}

impl MapWithMut for Expr {
    fn dummy() -> Self {
        Expr::Invalid(Invalid { span: DUMMY_SP })
    }
}

impl MapWithMut for Ident {
    fn dummy() -> Self {
        Ident::new(js_word!(""), DUMMY_SP)
    }
}

impl MapWithMut for PropName {
    fn dummy() -> Self {
        PropName::Ident(Ident::dummy())
    }
}

impl MapWithMut for Decl {
    fn dummy() -> Self {
        Decl::Var(VarDecl {
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

impl MapWithMut for Stmt {
    fn dummy() -> Self {
        Stmt::Empty(EmptyStmt { span: DUMMY_SP })
    }
}

impl MapWithMut for ModuleItem {
    fn dummy() -> Self {
        Stmt::dummy().into()
    }
}
