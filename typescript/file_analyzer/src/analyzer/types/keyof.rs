use super::NormalizeTypeOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::DebugExt;
use stc_ts_types::Type;
use stc_ts_types::Union;
use swc_common::Span;
use swc_ecma_ast::TsKeywordTypeKind;

impl Analyzer<'_, '_> {
    /// Evaluates `keyof` operator.
    ///
    /// # Parameters
    ///
    /// ## `ty`
    /// Should be operand of `keyof`.
    pub(super) fn eval_keyof(&mut self, span: Span, ty: &Type) -> ValidationResult<Type> {
        let ty = self
            .normalize(ty, NormalizeTypeOpts { ..Default::default() })
            .context("tried to normalize")?;

        match ty.normalize() {
            Type::Keyword(RTsKeywordType { kind, .. }) => {
                let interface_name = match kind {
                    TsKeywordTypeKind::TsAnyKeyword => {
                        return Ok(Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }
                    TsKeywordTypeKind::TsVoidKeyword
                    | TsKeywordTypeKind::TsUndefinedKeyword
                    | TsKeywordTypeKind::TsNullKeyword
                    | TsKeywordTypeKind::TsUnknownKeyword => {
                        return Ok(Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNeverKeyword,
                        }));
                    }
                    TsKeywordTypeKind::TsNumberKeyword => {}
                    TsKeywordTypeKind::TsObjectKeyword => {}
                    TsKeywordTypeKind::TsBooleanKeyword => {}
                    TsKeywordTypeKind::TsBigIntKeyword => {}
                    TsKeywordTypeKind::TsStringKeyword => {}
                    TsKeywordTypeKind::TsSymbolKeyword => {}
                    TsKeywordTypeKind::TsNeverKeyword => {
                        return Ok(Type::Union(Union {
                            span,
                            types: vec![
                                Type::Keyword(RTsKeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                }),
                                Type::Keyword(RTsKeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                }),
                                Type::Keyword(RTsKeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                                }),
                            ],
                        }))
                    }
                    TsKeywordTypeKind::TsIntrinsicKeyword => {}
                };
            }
            _ => {}
        }

        unimplemented!("eval_keyof: {:#?}", ty);
    }
}
