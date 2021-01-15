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
use stc_ts_types::Union;
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

impl Analyzer<'_, '_> {
    fn add_prop_or_spread_to_type(
        &mut self,
        to: Box<Type>,
        prop: &RPropOrSpread,
    ) -> ValidationResult {
        match prop {
            RPropOrSpread::Spread(spread) => {}
            RPropOrSpread::Prop(prop) => {}
        }
    }

    /// If rhs is an union type, return type will be union.
    ///
    /// `{ a: number } + ( {b: number} | { c: number } )` => `{ a: number, b:
    /// number } | { a: number, c: number }`
    fn append_type(&mut self, to: Box<Type>, rhs: Box<Type>) -> ValidationResult<Box<Type>> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        if rhs.is_any() || rhs.is_unknown() {
            return Ok(to);
        }

        match *to {
            Type::Union(to) => Ok(box Type::Union(Union {
                span: to.span,
                types: to
                    .types
                    .into_iter()
                    .map(|to| self.append_type(to, rhs))
                    .collect::<Result<_, _>>()?,
            })),
            _ => {
                unimplemented!("append type: {:?} <= {:?}", to, rhs)
            }
        }
    }
}
