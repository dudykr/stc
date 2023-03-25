use std::borrow::Cow;

use stc_ts_ast_rnode::{
    RBool, RJSXAttrName, RJSXAttrOrSpread, RJSXAttrValue, RJSXElement, RJSXElementChild, RJSXElementName, RJSXExpr, RJSXExprContainer,
    RJSXFragment, RJSXMemberExpr, RJSXNamespacedName, RJSXObject, RJSXSpreadChild, RJSXText, RTsLit,
};
use stc_ts_env::JsxMode;
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{
    CommonTypeMetadata, Id, IdCtx, Key, KeywordType, KeywordTypeMetadata, LitType, PropertySignature, Type, TypeElement, TypeLit,
};
use stc_utils::cache::Freeze;
use swc_atoms::JsWord;
use swc_common::{Span, Spanned};
use swc_ecma_ast::TsKeywordTypeKind;

use super::{call_new::ExtractKind, AccessPropertyOpts, TypeOfMode};
use crate::{
    analyzer::{assign::AssignOpts, util::ResultExt, Analyzer},
    validator::ValidateWith,
    VResult,
};

#[derive(Debug)]
pub enum ResolvedJsxName {
    /// `(props)`
    Intrinsic(Type),

    /// `(type, props)`
    Value(Type, Type),
}

impl Analyzer<'_, '_> {
    fn get_jsx_prop_name(&mut self, span: Span) -> Option<JsWord> {
        if let Some(jsx_cache) = self.data.jsx_prop_name.clone() {
            return jsx_cache;
        }
        let jsx_cache = self.get_jsx_prop_name_no_cache(span);
        self.data.jsx_prop_name = Some(jsx_cache.clone());
        jsx_cache
    }

    fn get_jsx_prop_name_no_cache(&mut self, span: Span) -> Option<JsWord> {
        let jsx = self.get_jsx_namespace()?;

        let ty = self
            .access_property(
                span,
                &jsx,
                &Key::Normal {
                    span,
                    sym: "ElementAttributesProperty".into(),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
                AccessPropertyOpts {
                    disallow_creating_indexed_type_from_ty_els: true,
                    ..Default::default()
                },
            )
            .ok()?;

        let ty = self.convert_type_to_type_lit(span, Cow::Owned(ty)).ok()??;

        if ty.members.len() == 1 {
            match &ty.members[0] {
                TypeElement::Property(PropertySignature {
                    key: Key::Normal { sym, .. },
                    ..
                }) => Some(sym.clone()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn get_jsx_intrinsic_element(&mut self, span: Span, sym: &JsWord) -> VResult<Type> {
        if let Some(jsx) = self.get_jsx_intrinsic_element_list(span)? {
            self.access_property(
                span,
                &jsx,
                &Key::Normal { span, sym: sym.clone() },
                TypeOfMode::RValue,
                IdCtx::Var,
                AccessPropertyOpts {
                    disallow_creating_indexed_type_from_ty_els: true,
                    ..Default::default()
                },
            )
            .context("tried to get type of an intrinsic jsx element")
        } else {
            if !self.ctx.in_declare && !matches!(self.rule().jsx, JsxMode::Preserve) {
                self.storage
                    .report(ErrorKind::ImplicitAny { span }.context("jsx namespace not found"))
            }

            Ok(Type::any(
                span,
                KeywordTypeMetadata {
                    common: CommonTypeMetadata {
                        implicit: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            ))
        }
    }

    fn get_jsx_intrinsic_element_list(&mut self, span: Span) -> VResult<Option<Type>> {
        let jsx = self.get_jsx_namespace();
        let jsx = match jsx {
            Some(v) => v,
            None => return Ok(None),
        };

        Ok(Some(
            self.access_property(
                span,
                &jsx,
                &Key::Normal {
                    span,
                    sym: "IntrinsicElements".into(),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
                AccessPropertyOpts {
                    disallow_creating_indexed_type_from_ty_els: true,
                    ..Default::default()
                },
            )
            .convert_err(|err| ErrorKind::ImplicitAnyBecauseThereIsNoJsxInterface { span: err.span() })
            .context("tried to get JSX.IntrinsicElements")?,
        ))
    }

    fn get_jsx_namespace(&mut self) -> Option<Type> {
        let top_level_ctxt = self.storage.top_level_ctxt(self.ctx.module_id);

        let types = self.find_type(&Id::new("JSX".into(), top_level_ctxt)).ok().flatten()?;

        for ty in types {
            if ty.is_module() {
                return Some(ty.into_owned());
            }
        }

        None
    }

    fn validate_jsx_attrs(&mut self, jsx_element_span: Span, name: &ResolvedJsxName, attrs: &[RJSXAttrOrSpread]) -> VResult<()> {
        let mut object = Type::TypeLit(TypeLit {
            span: jsx_element_span,
            members: vec![],
            metadata: Default::default(),
            tracker: Default::default(),
        });
        for attr in attrs {
            match attr {
                RJSXAttrOrSpread::JSXAttr(attr) => match &attr.name {
                    RJSXAttrName::Ident(attr_name) => {
                        let value = match &attr.value {
                            Some(v) => {
                                // TODO(kdy1): Pass down type annotation

                                let props_object = match name {
                                    ResolvedJsxName::Intrinsic(v) => v,
                                    ResolvedJsxName::Value(_, props) => props,
                                };

                                let res = self.access_property(
                                    attr_name.span,
                                    props_object,
                                    &Key::Normal {
                                        span: attr_name.span,
                                        sym: attr_name.sym.clone(),
                                    },
                                    TypeOfMode::RValue,
                                    IdCtx::Var,
                                    Default::default(),
                                );

                                let type_ann = res.ok().freezed();

                                v.validate_with_args(self, type_ann.as_ref())?
                            }
                            None => Some(Type::Lit(LitType {
                                span: attr_name.span,
                                lit: RTsLit::Bool(RBool {
                                    span: attr_name.span,
                                    value: true,
                                }),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                        };

                        if let Some(value) = value {
                            object = self.append_type_element(
                                object,
                                TypeElement::Property(PropertySignature {
                                    span: attr.span,
                                    accessibility: None,
                                    readonly: false,
                                    key: Key::Normal {
                                        span: attr_name.span,
                                        sym: attr_name.sym.clone(),
                                    },
                                    optional: false,
                                    params: Default::default(),
                                    type_ann: Some(box value),
                                    type_params: None,
                                    metadata: Default::default(),
                                    accessor: Default::default(),
                                }),
                            )?;
                        }
                    }
                    RJSXAttrName::JSXNamespacedName(attr_name) => {
                        return Err(ErrorKind::Unimplemented {
                            span: attr.span,
                            msg: "namespaced name for an attribute".to_string(),
                        }
                        .into())
                    }
                },
                RJSXAttrOrSpread::SpreadElement(el) => {
                    let attr = el.expr.validate_with_default(self)?;
                    object = self.append_type(el.dot3_token, object, attr, Default::default())?;
                }
            }
        }

        object.freeze();

        match name {
            ResolvedJsxName::Intrinsic(name) => {
                self.assign_with_opts(
                    &mut Default::default(),
                    name,
                    &object,
                    AssignOpts {
                        span: jsx_element_span,
                        allow_missing_fields: true,
                        ..Default::default()
                    },
                )
                .context("tried to assign attributes to intrinsic jsx element")
                .report(&mut self.storage);
            }
            ResolvedJsxName::Value(name, props) => {
                self.assign_with_opts(
                    &mut Default::default(),
                    props,
                    &object,
                    AssignOpts {
                        span: jsx_element_span,
                        allow_missing_fields: true,
                        // TODO: Remove the line below after fixing inference issues
                        allow_unknown_rhs: Some(true),
                        ..Default::default()
                    },
                )
                .context("tried to assign attributes to custom jsx element")
                .report(&mut self.storage);
            }
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXElement, type_ann: Option<&Type>) -> VResult<Type> {
        let name = e.opening.name.validate_with(self)?;

        let type_ann_for_children = {
            let obj = match &name {
                ResolvedJsxName::Intrinsic(name) => name,
                ResolvedJsxName::Value(_, props) => props,
            };

            let type_ann_res = self.access_property(
                e.span,
                obj,
                &Key::Normal {
                    span: e.span,
                    sym: "children".into(),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
                Default::default(),
            );

            type_ann_res.ok()
        };

        let children = e.children.validate_with_args(self, type_ann_for_children.as_ref())?;

        self.validate_jsx_attrs(e.span, &name, &e.opening.attrs).report(&mut self.storage);

        match name {
            ResolvedJsxName::Intrinsic(name) => Ok(name),
            ResolvedJsxName::Value(name, props) => Ok(name),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXFragment, type_ann: Option<&Type>) -> VResult<Type> {
        let children = e.children.validate_with_default(self)?;

        self.get_jsx_intrinsic_element(e.span, &"Fragment".into())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXElementChild, type_ann: Option<&Type>) -> VResult<Option<Type>> {
        match e {
            RJSXElementChild::JSXText(e) => e.validate_with(self).map(Some),
            RJSXElementChild::JSXExprContainer(e) => e.validate_with_args(self, type_ann),
            RJSXElementChild::JSXSpreadChild(e) => e.validate_with(self).map(Some),
            RJSXElementChild::JSXElement(e) => e.validate_with_default(self).map(Some),
            RJSXElementChild::JSXFragment(e) => e.validate_with_default(self).map(Some),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXText) -> VResult<Type> {
        Ok(Type::Keyword(KeywordType {
            span: e.span,
            kind: TsKeywordTypeKind::TsStringKeyword,
            metadata: Default::default(),
            tracker: Default::default(),
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXExprContainer, type_ann: Option<&Type>) -> VResult<Option<Type>> {
        e.expr.validate_with_args(self, type_ann)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXSpreadChild) -> VResult<Type> {
        e.expr.validate_with_default(self)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXExpr, type_ann: Option<&Type>) -> VResult<Option<Type>> {
        match e {
            RJSXExpr::Expr(e) => e.validate_with_args(self, (TypeOfMode::RValue, None, type_ann)).map(Some),
            RJSXExpr::JSXEmptyExpr(..) => Ok(None),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXElementName) -> VResult<ResolvedJsxName> {
        let ty = match e {
            RJSXElementName::Ident(ident) => {
                if ident.sym.starts_with(|c: char| c.is_ascii_uppercase()) {
                    ident.validate_with_default(self)?
                } else {
                    return self
                        .get_jsx_intrinsic_element(ident.span, &ident.sym)
                        .map(ResolvedJsxName::Intrinsic);
                }
            }
            RJSXElementName::JSXMemberExpr(e) => e.validate_with(self)?,
            RJSXElementName::JSXNamespacedName(e) => e.validate_with(self)?,
        }
        .freezed();

        let span = e.span();
        let jsx_prop_name = self.get_jsx_prop_name(span);

        let props = match jsx_prop_name {
            Some(jsx_prop_name) => self
                .access_property(
                    span,
                    &ty,
                    &Key::Normal { span, sym: jsx_prop_name },
                    TypeOfMode::RValue,
                    IdCtx::Var,
                    Default::default(),
                )
                .ok(),
            None => {
                let constructors = self.extract_callee_candidates(span, ExtractKind::New, &ty)?;

                if constructors.len() == 1 && !constructors[0].params.is_empty() {
                    Some(*constructors[0].params[0].ty.clone())
                } else {
                    None
                }
            }
        };

        if let Some(props) = props {
            Ok(ResolvedJsxName::Value(ty, props))
        } else {
            Ok(ResolvedJsxName::Value(ty, Type::any(span, Default::default())))
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXMemberExpr) -> VResult<Type> {
        let obj = e.obj.validate_with(self)?;

        self.access_property(
            e.span(),
            &obj,
            &Key::Normal {
                span: e.prop.span,
                sym: e.prop.sym.clone(),
            },
            TypeOfMode::RValue,
            IdCtx::Var,
            AccessPropertyOpts {
                disallow_creating_indexed_type_from_ty_els: true,
                ..Default::default()
            },
        )
        .context("tried to get type of a jsx member expr")
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXObject) -> VResult<Type> {
        match e {
            RJSXObject::Ident(e) => e.validate_with_default(self),
            RJSXObject::JSXMemberExpr(e) => e.validate_with(self),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXNamespacedName) -> VResult<Type> {
        Err(ErrorKind::Unimplemented {
            span: e.span(),
            msg: "jsx namespaced name".to_string(),
        }
        .into())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXAttrValue, type_ann: Option<&Type>) -> VResult<Option<Type>> {
        match e {
            RJSXAttrValue::Lit(v) => v.validate_with(self).map(Some),
            RJSXAttrValue::JSXElement(v) => v.validate_with_args(self, type_ann).map(Some),
            RJSXAttrValue::JSXFragment(v) => v.validate_with_args(self, type_ann).map(Some),
            RJSXAttrValue::JSXExprContainer(v) => v.validate_with_args(self, type_ann),
        }
    }
}
