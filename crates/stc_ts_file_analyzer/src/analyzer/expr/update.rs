use std::borrow::Cow;

use stc_ts_ast_rnode::{RExpr, RLit, RParenExpr, RTsLit, RUpdateExpr};
use stc_ts_errors::Error;
use stc_ts_types::{KeywordType, LitType, Type};
use stc_utils::cache::Freeze;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt, Analyzer},
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RUpdateExpr) -> VResult {
        let span = e.span;

        match &*e.arg {
            RExpr::New(..) => self.storage.report(Error::ExprInvalidForUpdateArg { span }),
            _ => {}
        }

        let res = e
            .arg
            .validate_with_args(self, (TypeOfMode::LValue, None, None))
            .map(Freeze::freezed);
        let mut errored = false;

        let ty = res
            .and_then(|ty| match ty.normalize() {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                })
                | Type::Lit(LitType {
                    lit: RTsLit::Str(..),
                    ..
                })
                | Type::Lit(LitType {
                    lit: RTsLit::Bool(..),
                    ..
                })
                | Type::TypeLit(..)
                | Type::Array(..)
                | Type::Tuple(..)
                | Type::This(..)
                | Type::Function(..) => {
                    errored = true;
                    Err(Error::TypeInvalidForUpdateArg {
                        span: e.arg.span(),
                        ty: box ty.clone(),
                    })
                }

                _ if ty.is_global_this() => {
                    errored = true;

                    Err(Error::TypeInvalidForUpdateArg {
                        span: e.arg.span(),
                        ty: box ty.clone(),
                    })
                }

                Type::Enum(..) => {
                    errored = true;

                    Err(Error::CannotAssignToNonVariable { span: e.arg.span() })
                }

                Type::Lit(LitType {
                    lit: RTsLit::Number(..),
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {
                    match &*e.arg {
                        RExpr::Lit(RLit::Num(..)) | RExpr::Call(..) | RExpr::Paren(..) => {
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
                self.storage
                    .report(Error::InvalidNumericOperand { span: e.arg.span() })
            }
        } else {
            if !errored
                && match &*e.arg {
                    RExpr::Paren(RParenExpr {
                        expr: box RExpr::Bin(..),
                        ..
                    })
                    | RExpr::Bin(..) => true,
                    _ => false,
                }
            {
                self.storage
                    .report(Error::UpdateArgMustBeVariableOrPropertyAccess { span });
            }
        }

        Ok(Type::Keyword(KeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
            metadata: Default::default(),
        }))
    }
}

impl Analyzer<'_, '_> {
    fn is_update_operand_valid(&mut self, arg: &Type) -> VResult<bool> {
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
