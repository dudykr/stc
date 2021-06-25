use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt, Analyzer},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use stc_ts_ast_rnode::{RExpr, RLit, RTsKeywordType, RTsLit, RTsLitType, RUpdateExpr};
use stc_ts_errors::Error;
use stc_ts_types::Type;
use std::borrow::Cow;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RUpdateExpr) -> ValidationResult {
        let span = e.span;

        match &*e.arg {
            RExpr::New(..) => self.storage.report(Error::ExprInvalidForUpdateArg { span }),
            _ => {}
        }

        let ty = e
            .arg
            .validate_with_args(self, (TypeOfMode::LValue, None, None))
            .and_then(|ty| match ty.normalize() {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Str(..), ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Bool(..), ..
                })
                | Type::TypeLit(..)
                | Type::Array(..)
                | Type::Tuple(..)
                | Type::This(..)
                | Type::Function(..) => Err(Error::TypeInvalidForUpdateArg {
                    span: e.arg.span(),
                    ty: box ty.clone(),
                }),

                _ if ty.is_global_this() => Err(Error::TypeInvalidForUpdateArg {
                    span: e.arg.span(),
                    ty: box ty.clone(),
                }),

                Type::Enum(..) => Err(Error::CannotAssignToNonVariable { span: e.arg.span() }),

                Type::Lit(RTsLitType {
                    lit: RTsLit::Number(..),
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {
                    match &*e.arg {
                        RExpr::Lit(RLit::Num(..)) | RExpr::Call(..) | RExpr::Paren(..) | RExpr::Bin(..) => {
                            self.storage.report(Error::ExprInvalidForUpdateArg { span });
                        }
                        _ => {}
                    }
                    return Ok(ty);
                }

                _ => Ok(ty),
            })
            .report(&mut self.storage);

        if let Some(ty) = ty {
            if let Some(false) = self.is_update_operand_valid(&ty).report(&mut self.storage) {
                self.storage.report(Error::InvalidNumericOperand { span: e.arg.span() })
            }
        }

        Ok(Type::Keyword(RTsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        }))
    }
}

impl Analyzer<'_, '_> {
    fn is_update_operand_valid(&mut self, arg: &Type) -> ValidationResult<bool> {
        let ty = self.normalize(Some(arg.span()), Cow::Borrowed(arg), Default::default())?;

        if ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword)
            || ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword)
            || ty.is_str()
            || ty.is_bool()
        {
            return Ok(false);
        }

        match ty.normalize() {
            Type::Union(ty) => {
                for ty in &ty.types {
                    if !self.is_update_operand_valid(ty)? {
                        return Ok(false);
                    }
                }
            }
            _ => {}
        }

        Ok(true)
    }
}
