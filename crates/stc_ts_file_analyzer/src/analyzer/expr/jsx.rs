use stc_ts_ast_rnode::{RJSXElement, RJSXElementChild, RJSXElementName, RJSXFragment, RJSXMemberExpr, RJSXNamespacedName, RJSXObject};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{CommonTypeMetadata, Id, IdCtx, Key, KeywordTypeMetadata, Type, TypeParamInstantiation};
use swc_atoms::JsWord;
use swc_common::{Span, Spanned};

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
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXElementChild) -> VResult<Type> {
        match e {}
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
