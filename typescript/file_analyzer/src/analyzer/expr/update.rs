use super::TypeOfMode;
use crate::analyzer::util::ResultExt;
use crate::analyzer::Analyzer;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RUpdateExpr;
use stc_ts_errors::Error;
use stc_ts_types::Type;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RUpdateExpr) -> ValidationResult {
        let span = e.span;

        let ty = e
            .arg
            .validate_with_args(self, (TypeOfMode::LValue, None, None))
            .and_then(|ty| match *ty.normalize() {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Str(..), ..
                })
                | Type::Array(..) => Err(box Error::TS2356 { span: e.arg.span() }),

                _ => Ok(ty),
            })
            .report(&mut self.storage);

        if let Some(ty) = ty {
            if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                self.storage.report(box Error::UpdateOpToSymbol {
                    span: e.arg.span(),
                    op: e.op,
                })
            }
        }

        Ok(box Type::Keyword(RTsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        }))
    }
}
