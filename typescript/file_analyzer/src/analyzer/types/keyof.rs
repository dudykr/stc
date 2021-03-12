use super::NormalizeTypeOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_types::ClassMember;
use stc_ts_types::ClassProperty;
use stc_ts_types::Method;
use stc_ts_types::ModuleId;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use stc_ts_types::Union;
use swc_atoms::js_word;
use swc_common::Span;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;

impl Analyzer<'_, '_> {
    /// Evaluates `keyof` operator.
    ///
    /// # Parameters
    ///
    /// ## `ty`
    /// Should be operand of `keyof`.
    pub(super) fn keyof(&mut self, span: Span, ty: &Type) -> ValidationResult<Type> {
        let ty = self
            .normalize(ty, NormalizeTypeOpts { ..Default::default() })
            .context("tried to normalize")?;

        match ty.normalize() {
            Type::Keyword(RTsKeywordType { kind, .. }) => match kind {
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
                TsKeywordTypeKind::TsNumberKeyword
                | TsKeywordTypeKind::TsBooleanKeyword
                | TsKeywordTypeKind::TsStringKeyword => {
                    let name = match kind {
                        TsKeywordTypeKind::TsNumberKeyword => {
                            js_word!("Number")
                        }
                        TsKeywordTypeKind::TsBooleanKeyword => {
                            js_word!("Boolean")
                        }
                        TsKeywordTypeKind::TsStringKeyword => {
                            js_word!("String")
                        }
                        _ => unreachable!(),
                    };
                    return self
                        .keyof(
                            span,
                            &Type::Ref(Ref {
                                span,
                                ctxt: ModuleId::builtin(),
                                type_name: RTsEntityName::Ident(RIdent::new(name, DUMMY_SP)),
                                type_args: None,
                            }),
                        )
                        .context("tried to get keys of builitin interface types");
                }

                TsKeywordTypeKind::TsObjectKeyword => {}
                TsKeywordTypeKind::TsBigIntKeyword => {}
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
            },

            Type::ClassDef(cls) => {
                let mut key_types = vec![];
                for member in &cls.body {
                    match member {
                        ClassMember::Property(ClassProperty { key, .. }) | ClassMember::Method(Method { key, .. }) => {
                            if !key.is_computed() {
                                key_types.push(key.ty().into_owned());
                            }
                        }
                        ClassMember::Constructor(_) => {}
                        ClassMember::IndexSignature(_) => {}
                    }
                }

                return Ok(Type::Union(Union { span, types: key_types }));
            }

            _ => {}
        }

        unimplemented!("keyof: {}", dump_type_as_string(&self.cm, &ty));
    }
}
