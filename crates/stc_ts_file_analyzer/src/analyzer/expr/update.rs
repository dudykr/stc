use std::borrow::Cow;

use stc_ts_ast_rnode::{RExpr, RLit, RParenExpr, RTsLit, RUpdateExpr};
use stc_ts_errors::ErrorKind;
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
    fn validate(&mut self, e: &RUpdateExpr) -> VResult<Type> {
        let span = e.span;

        if let RExpr::New(..) = &*e.arg {
            self.storage.report(ErrorKind::ExprInvalidForUpdateArg { span }.into())
        }

        if let RExpr::OptChain(..) = &*e.arg {
            self.storage.report(ErrorKind::InvalidOperandOfIncDecOptionalProp { span }.into())
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
                | Type::Lit(LitType { lit: RTsLit::Str(..), .. })
                | Type::Lit(LitType { lit: RTsLit::Bool(..), .. })
                | Type::TypeLit(..)
                | Type::Array(..)
                | Type::Tuple(..)
                | Type::This(..)
                | Type::Function(..) => {
                    errored = true;
                    Err(ErrorKind::TypeInvalidForUpdateArg {
                        span: e.arg.span(),
                        ty: box ty.clone(),
                    }
                    .into())
                }

                _ if ty.is_global_this() => {
                    errored = true;

                    Err(ErrorKind::TypeInvalidForUpdateArg {
                        span: e.arg.span(),
                        ty: box ty.clone(),
                    }
                    .into())
                }

                Type::Enum(..) => {
                    errored = true;

                    Err(ErrorKind::CannotAssignToEnum { span: e.arg.span() }.into())
                }

                Type::Lit(LitType {
                    lit: RTsLit::Number(..), ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {
                    fn is_valid_expr(e: &RExpr) -> bool {
                        match e {
                            RExpr::Lit(RLit::Num(..)) | RExpr::Call(..) | RExpr::Bin(..) => false,
                            RExpr::Paren(r) => is_valid_expr(&r.expr),
                            _ => true,
                        }
                    }
                    if !is_valid_expr(&e.arg) {
                        self.storage.report(ErrorKind::ExprInvalidForUpdateArg { span }.into());
                    }
                    Ok(ty)
                }

                _ => Ok(ty),
            })
            .or_else(|err| match &*err {
                ErrorKind::NoSuchEnumVariant { .. } => Ok(Type::any(span, Default::default())),
                _ => Err(err),
            })
            .report(&mut self.storage);

        if let Some(ty) = ty {
            if let Some(false) = self.is_update_operand_valid(&ty).report(&mut self.storage) {
                self.storage.report(ErrorKind::InvalidNumericOperand { span: e.arg.span() }.into())
            }
        } else {
            if !errored
                && matches!(
                    &*e.arg,
                    RExpr::Paren(RParenExpr {
                        expr: box RExpr::Bin(..),
                        ..
                    }) | RExpr::Bin(..)
                )
            {
                self.storage
                    .report(ErrorKind::UpdateArgMustBeVariableOrPropertyAccess { span }.into());
            }
        }

        Ok(Type::Keyword(KeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
            metadata: Default::default(),
            tracker: Default::default(),
        }))
    }
}

impl Analyzer<'_, '_> {
    fn is_update_operand_valid(&mut self, arg: &Type) -> VResult<bool> {
        let ty = self.normalize(Some(arg.span()), Cow::Borrowed(arg), Default::default())?;

        if ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) || ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) || ty.is_str() || ty.is_bool() {
            return Ok(false);
        }

        if let Type::Union(ty) = ty.normalize() {
            for ty in &ty.types {
                if !self.is_update_operand_valid(ty)? {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }
}
