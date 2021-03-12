use std::borrow::Cow;

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
use stc_ts_types::MethodSignature;
use stc_ts_types::ModuleId;
use stc_ts_types::PropertySignature;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
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

            Type::TypeLit(l) => {
                let mut types = vec![];
                for member in &l.members {
                    match member {
                        TypeElement::Property(PropertySignature { key, .. })
                        | TypeElement::Method(MethodSignature { key, .. }) => {
                            if !key.is_computed() {
                                types.push(key.ty().into_owned());
                            }
                        }

                        TypeElement::Index(i) => {
                            // TODO: Check if this is correct.
                            if let Some(p) = i.params.first() {
                                types.push(*p.ty.clone());
                            }
                        }

                        TypeElement::Call(_) | TypeElement::Constructor(_) => {}
                    }
                }

                if types.is_empty() {
                    return Ok(Type::never(span));
                }

                return Ok(Type::Union(Union { span, types }));
            }

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

            Type::Array(arr) => {
                return self
                    .keyof(
                        span,
                        &Type::Ref(Ref {
                            span,
                            ctxt: ModuleId::builtin(),
                            type_name: RTsEntityName::Ident(RIdent::new(js_word!("Array"), DUMMY_SP)),
                            type_args: None,
                        }),
                    )
                    .context("tried to get keys of Array (builtin)");
            }

            Type::Interface(..) | Type::Enum(..) => {
                //
                if let Some(ty) = self
                    .type_to_type_lit(span, &ty)?
                    .map(Cow::into_owned)
                    .map(Type::TypeLit)
                {
                    return self
                        .keyof(span, &ty)
                        .context("tried to evaluate `keyof` for type literal created with type_to_type_lit");
                }
            }

            Type::Intersection(i) => {
                // We return union of keys.
                let types = i
                    .types
                    .iter()
                    .map(|ty| self.keyof(span, ty))
                    .collect::<Result<_, _>>()?;

                return Ok(Type::Union(Union { span, types }));
            }

            Type::Union(u) => {
                // We return intersection of keys.
            }

            _ => {}
        }

        unimplemented!("keyof: {}", dump_type_as_string(&self.cm, &ty));
    }
}
