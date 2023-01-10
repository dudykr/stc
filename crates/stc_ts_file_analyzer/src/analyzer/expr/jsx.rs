use stc_ts_ast_rnode::{RJSXElement, RJSXElementChild, RJSXElementName, RJSXFragment, RJSXMemberExpr, RJSXNamespacedName, RJSXObject};
use stc_ts_errors::ErrorKind;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{CommonTypeMetadata, Id, KeywordTypeMetadata, Type, TypeParamInstantiation};

use super::TypeOfMode;
use crate::{analyzer::Analyzer, validator::ValidateWith, VResult};

impl Analyzer<'_, '_> {
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
                    if let Some(jsx) = self.get_jsx_namespace() {
                    } else {
                        if !self.ctx.in_declare && self.rule().no_implicit_any {
                            self.storage
                                .report(ErrorKind::ImplicitAny { span: ident.span }.context("jsx namespace not found"))
                        }

                        Ok(Type::any(
                            ident.span,
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
            }
            RJSXElementName::JSXMemberExpr(e) => e.validate_with(self),
            RJSXElementName::JSXNamespacedName(e) => e.validate_with(self),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RJSXMemberExpr) -> VResult<Type> {}
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
    fn validate(&mut self, e: &RJSXNamespacedName) -> VResult<Type> {}
}
