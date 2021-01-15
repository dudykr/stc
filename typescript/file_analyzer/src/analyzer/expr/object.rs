use std::mem::take;

use crate::analyzer::Analyzer;
use crate::analyzer::ScopeKind;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RObjectLit;
use stc_ts_ast_rnode::RPropOrSpread;
use stc_ts_ast_rnode::RSpreadElement;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use swc_common::EqIgnoreSpan;
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit) -> ValidationResult {
        self.with_child(
            ScopeKind::ObjectLit,
            Default::default(),
            |a: &mut Analyzer| {
                let mut special_type = None;

                // TODO: Change order

                for prop in node.props.iter() {
                    match *prop {
                        RPropOrSpread::Prop(ref prop) => {
                            let p: TypeElement = prop.validate_with(a)?;
                            if let Some(key) = p.key() {
                                if a.scope.this_object_members.iter_mut().any(|element| {
                                    match element {
                                        TypeElement::Property(prop)
                                            if (*prop.key).eq_ignore_span(&*key) =>
                                        {
                                            prop.readonly = false;
                                            true
                                        }
                                        _ => false,
                                    }
                                }) {
                                    continue;
                                }
                            }

                            a.scope.this_object_members.push(p);
                        }
                        RPropOrSpread::Spread(RSpreadElement { ref expr, .. }) => {
                            match *expr.validate_with_default(a)? {
                                Type::TypeLit(TypeLit {
                                    members: spread_members,
                                    ..
                                }) => {
                                    a.scope.this_object_members.extend(spread_members);
                                }

                                // Use last type on ...any or ...unknown
                                ty
                                @
                                Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })
                                | ty
                                @
                                Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) => special_type = Some(ty),

                                ty => unimplemented!("spread with non-type-lit: {:#?}", ty),
                            }
                        }
                    }
                }

                if let Some(ty) = special_type {
                    return Ok(box ty);
                }

                Ok(box Type::TypeLit(TypeLit {
                    span: node.span,
                    members: take(&mut a.scope.this_object_members),
                }))
            },
        )
    }
}
