//! This module implements VisitMut for Analyzer

use crate::{analyzer::Analyzer, validator, validator::ValidateWith};
use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{
    RArrowExpr, RBlockStmt, RCatchClause, RClass, RClassDecl, RClassExpr, RClassMember, RClassMethod,
    RComputedPropName, RConstructor, RDoWhileStmt, RExportAll, RExportDecl, RExportDefaultDecl, RExportDefaultExpr,
    RExportNamedSpecifier, RExpr, RFnDecl, RFnExpr, RForInStmt, RForOfStmt, RForStmt, RFunction, RIfStmt, RImportDecl,
    RModule, RModuleItem, RNamedExport, RObjectLit, RParam, RParamOrTsParamProp, RPrivateMethod, RPrivateProp,
    RPropName, RReturnStmt, RSeqExpr, RStmt, RSwitchStmt, RTpl, RTsEnumDecl, RTsExportAssignment, RTsFnParam,
    RTsFnType, RTsImportEqualsDecl, RTsInterfaceBody, RTsInterfaceDecl, RTsModuleDecl, RTsNamespaceDecl, RTsParamProp,
    RTsTplLitType, RTsType, RTsTypeAliasDecl, RTsTypeElement, RVarDecl, RVarDeclarator, RWhileStmt, RWithStmt,
    RYieldExpr,
};

macro_rules! forward {
    ($name:ident,$T:ty) => {
        /// Delegates to `Validate<T>`
        impl Visit<$T> for Analyzer<'_, '_> {
            fn visit(&mut self, n: &$T) {
                let res = n.validate_with_default(self);
                match res {
                    // ignored
                    Ok(..) => {}
                    Err(err) => {
                        self.storage.report(err);
                    }
                }

                self.cur_facts.assert_valid();
            }
        }
    };
}

macro_rules! use_visit_mut {
    ($T:ty) => {
        #[validator]
        impl Analyzer<'_, '_> {
            fn validate(&mut self, node: &$T) {
                node.visit_children_with(self);
                Ok(())
            }
        }
    };
}

use_visit_mut!(RModule);

forward!(visit_mut_export_named_specifier, RExportNamedSpecifier);
forward!(visit_mut_expr, RExpr);
forward!(visit_mut_seq_expr, RSeqExpr);
forward!(visit_mut_block_stmt, RBlockStmt);
forward!(visit_mut_if_stmt, RIfStmt);
forward!(visit_mut_param, RParam);
forward!(visit_mut_function, RFunction);
forward!(visit_mut_fn_decl, RFnDecl);
forward!(visit_mut_fn_expr, RFnExpr);
forward!(visit_mut_var_decl, RVarDecl);
forward!(visit_mut_var_declarator, RVarDeclarator);
forward!(visit_mut_ts_interface_decl, RTsInterfaceDecl);
forward!(visit_mut_ts_type_element, RTsTypeElement);
forward!(visit_mut_prop_name, RPropName);
forward!(visit_mut_computed_prop_name, RComputedPropName);
forward!(visit_mut_class_method, RClassMethod);
forward!(visit_mut_ts_type_alias_decl, RTsTypeAliasDecl);
forward!(visit_mut_ts_module_decl, RTsModuleDecl);
forward!(visit_mut_class_member, RClassMember);
forward!(visit_mut_stmts, Vec<RStmt>);
forward!(visit_mut_module_item, RModuleItem);
forward!(visit_mut_module_items, Vec<RModuleItem>);
forward!(visit_mut_class, RClass);
forward!(visit_mut_class_decl, RClassDecl);
forward!(visit_mut_class_expr, RClassExpr);
forward!(visit_mut_ts_enum_decl, RTsEnumDecl);
forward!(visit_mut_ts_fn_param, RTsFnParam);
forward!(visit_mut_ts_fn_type, RTsFnType);
forward!(visit_mut_ts_type, RTsType);
forward!(visit_mut_arrow_expr, RArrowExpr);
forward!(visit_mut_ts_interface_body, RTsInterfaceBody);
forward!(visit_mut_object_lit, RObjectLit);
forward!(visit_mut_stmt, RStmt);
forward!(visit_mut_switch_stmt, RSwitchStmt);
forward!(visit_mut_with_stmt, RWithStmt);
forward!(visit_mut_return_stmt, RReturnStmt);
forward!(visit_mut_yield_expr, RYieldExpr);
forward!(visit_mut_export_default_expr, RExportDefaultExpr);
forward!(visit_mut_ts_export_assignment, RTsExportAssignment);
forward!(visit_mut_export_default_decl, RExportDefaultDecl);
forward!(visit_mut_export_decl, RExportDecl);
forward!(visit_mut_private_method, RPrivateMethod);
forward!(visit_mut_private_prop, RPrivateProp);
forward!(visit_mut_import_decl, RImportDecl);
forward!(visit_mut_export_all, RExportAll);
forward!(visit_mut_named_export, RNamedExport);
forward!(visit_mut_catch_clause, RCatchClause);
forward!(visit_mut_ts_namespace_decl, RTsNamespaceDecl);
forward!(visit_do_while_stmt, RDoWhileStmt);
forward!(visit_while_stmt, RWhileStmt);
forward!(visit_for_of_stmt, RForOfStmt);
forward!(visit_for_in_stmt, RForInStmt);
forward!(visit_for_stmt, RForStmt);
forward!(visit_constructor, RConstructor);
forward!(visit_ts_param_prop, RTsParamProp);
forward!(visit_tpl, RTpl);
forward!(visit_mut_ts_import_equals_decl, RTsImportEqualsDecl);
forward!(visit, RParamOrTsParamProp);
forward!(visit, RTsTplLitType);
