use stc_ts_ast_rnode::{
    RJSXElement, RJSXElementChild, RJSXElementName, RJSXExpr, RJSXExprContainer, RJSXFragment, RJSXMemberExpr, RJSXNamespacedName,
    RJSXObject, RJSXSpreadChild, RJSXText,
};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{CommonTypeMetadata, Id, IdCtx, Key, KeywordType, KeywordTypeMetadata, Type, TypeParamInstantiation};
use swc_atoms::JsWord;
use swc_common::{Span, Spanned};
use swc_ecma_ast::TsKeywordTypeKind;

use super::{AccessPropertyOpts, TypeOfMode};
use crate::{analyzer::Analyzer, validator::ValidateWith, VResult};

impl Analyzer<'_, '_> {
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
            if !self.ctx.in_declare && self.rule().no_implicit_any {
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
            .context("tried to get JSX.IntrinsicElements")?,
        ))
    }

    fn get_jsx_namespace(&mut self) -> Option<Type> {
        let top_level_ctxt = self.storage.top_level_ctxt(self.ctx.module_id);

        let types = self.find_type(&Id::new("JSX".into(), top_level_ctxt)).ok().flatten()?;

        for ty in types {
            if ty.is_namespace() {
                return Some(ty.into_owned());
            }
        }

        None
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RJSXElement,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let name = e.opening.name.validate_with(self)?;
        let children = e.children.validate_with(self)?;

        Ok(name)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RJSXFragment,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let children = e.children.validate_with(self)?;

        self.get_jsx_intrinsic_element(e.span, &"Fragment".into())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXElementChild) -> VResult<Option<Type>> {
        match e {
            RJSXElementChild::JSXText(e) => e.validate_with(self).map(Some),
            RJSXElementChild::JSXExprContainer(e) => e.validate_with(self),
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
    fn validate(&mut self, e: &RJSXExprContainer) -> VResult<Option<Type>> {
        e.expr.validate_with(self)
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
    fn validate(&mut self, e: &RJSXExpr) -> VResult<Option<Type>> {
        match e {
            RJSXExpr::Expr(e) => e.validate_with_args(self, (TypeOfMode::RValue, None, None)).map(Some),
            RJSXExpr::JSXEmptyExpr(..) => Ok(None),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXElementName) -> VResult<Type> {
        match e {
            RJSXElementName::Ident(ident) => {
                if ident.sym.starts_with(|c: char| c.is_ascii_uppercase()) {
                    ident.validate_with_default(self)
                } else {
                    self.get_jsx_intrinsic_element(ident.span, &ident.sym)
                }
            }
            RJSXElementName::JSXMemberExpr(e) => e.validate_with(self),
            RJSXElementName::JSXNamespacedName(e) => e.validate_with(self),
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
